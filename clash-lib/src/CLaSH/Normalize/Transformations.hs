{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Transformations of the Normalization process
module CLaSH.Normalize.Transformations
  ( appProp
  , bindNonRep
  , liftNonRep
  , caseLet
  , caseCon
  , caseCase
  , inlineNonRep
  , typeSpec
  , nonRepSpec
  , etaExpansionTL
  , nonRepANF
  , bindConstantVar
  , constantSpec
  , makeANF
  , deadCode
  , topLet
  , recToLetRec
  , inlineClosed
  , inlineHO
  , inlineSmall
  , simpleCSE
  , reduceConst
  , reduceNonRepPrim
  )
where

import qualified Control.Lens                as Lens
import qualified Control.Monad               as Monad
import           Control.Monad.Writer        (WriterT (..), lift, tell)
import           Data.Bits                   ((.&.), complement)
import qualified Data.Either                 as Either
import qualified Data.HashMap.Lazy           as HashMap
import qualified Data.List                   as List
import qualified Data.Maybe                  as Maybe
import           Unbound.Generics.LocallyNameless     (Bind, Embed (..), bind, embed,
                                              rec, unbind, unembed, unrebind,
                                              unrec, name2String, string2Name,
                                              rebind)
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           CLaSH.Core.DataCon          (DataCon (..), dataConInstArgTys)
import           CLaSH.Core.FreeVars         (termFreeIds, termFreeTyVars,
                                              typeFreeVars)
import           CLaSH.Core.Pretty           (showDoc)
import           CLaSH.Core.Subst            (substTm, substTms, substTyInTm,
                                              substTysinTm)
import           CLaSH.Core.Term             (LetBinding, Pat (..), Term (..))
import           CLaSH.Core.Type             (TypeView (..), Type (..),
                                              LitTy (..), applyFunTy,
                                              applyTy, isPolyFunCoreTy,
                                              splitFunTy, typeKind,
                                              tyView, mkTyConApp, mkFunTy)
import           CLaSH.Core.TyCon            (TyConName, tyConDataCons)
import           CLaSH.Core.Util             (collectArgs, extractElems,
                                              idToVar, isCon,
                                              isFun, isLet, isPolyFun, isPrim,
                                              isSignalType, isVar, mkApps,
                                              mkLams, mkTmApps, mkVec,
                                              termSize, termType)
import           CLaSH.Core.Var              (Id, Var (..))
import           CLaSH.Netlist.Util          (representableType,
                                              splitNormalized)
import           CLaSH.Normalize.Types
import           CLaSH.Normalize.Util
import           CLaSH.Rewrite.Combinators
import           CLaSH.Rewrite.Types
import           CLaSH.Rewrite.Util
import           CLaSH.Util

-- | Inline non-recursive, non-representable, non-join-point, let-bindings
bindNonRep :: NormRewrite
bindNonRep = inlineBinders nonRepTest
  where
    nonRepTest :: Term -> (Var Term, Embed Term) -> RewriteMonad extra Bool
    nonRepTest e (id_@(Id idName tyE), exprE)
      = (&&) <$> (not <$> (representableType <$> Lens.view typeTranslator <*> Lens.view tcCache <*> pure (unembed tyE)))
             <*> ((&&) <$> (notElem idName <$> (Lens.toListOf <$> localFreeIds <*> pure (unembed exprE)))
                       <*> (pure (not $ isJoinPointIn id_ e)))

    nonRepTest _ _ = return False

-- | Lift non-representable let-bindings
liftNonRep :: NormRewrite
liftNonRep = liftBinders nonRepTest
  where
    nonRepTest :: Term -> (Var Term, Embed Term) -> RewriteMonad extra Bool
    nonRepTest _ ((Id _ tyE), _)
      -- We used to also check whether the binder we are lifting is either
      -- recursive or a join-point. This is no longer needed because we apply
      -- bindNonRep exhaustively before we apply liftNonRep. See also:
      -- [Note] bindNonRep before liftNonRep
      = not <$> (representableType <$> Lens.view typeTranslator
                                   <*> Lens.view tcCache
                                   <*> pure (unembed tyE))

    nonRepTest _ _ = return False

-- | Specialize functions on their type
typeSpec :: NormRewrite
typeSpec ctx e@(TyApp e1 ty)
  | (Var _ _,  args) <- collectArgs e1
  , null $ Lens.toListOf typeFreeVars ty
  , (_, []) <- Either.partitionEithers args
  = specializeNorm ctx e

typeSpec _ e = return e

-- | Specialize functions on their non-representable argument
nonRepSpec :: NormRewrite
nonRepSpec ctx e@(App e1 e2)
  | (Var _ _, args) <- collectArgs e1
  , (_, [])     <- Either.partitionEithers args
  , null $ Lens.toListOf termFreeTyVars e2
  = do tcm <- Lens.view tcCache
       e2Ty <- termType tcm e2
       localVar <- isLocalVar e2
       nonRepE2 <- not <$> (representableType <$> Lens.view typeTranslator <*> Lens.view tcCache <*> pure e2Ty)
       if nonRepE2 && not localVar
         then specializeNorm ctx e
         else return e

nonRepSpec _ e = return e

-- | Lift the let-bindings out of the subject of a Case-decomposition
caseLet :: NormRewrite
caseLet _ (Case (Letrec b) ty alts) = do
  (xes,e) <- unbind b
  changed (Letrec (bind xes (Case e ty alts)))

caseLet _ e = return e

-- | Move a Case-decomposition from the subject of a Case-decomposition to the alternatives
caseCase :: NormRewrite
caseCase _ e@(Case (Case scrut alts1Ty alts1) alts2Ty alts2)
  = do
    ty1Rep  <- representableType <$> Lens.view typeTranslator <*> Lens.view tcCache <*> pure alts1Ty
    if not ty1Rep
      then do newAlts <- mapM ( return
                                  . uncurry bind
                                  . second (\altE -> Case altE alts2Ty alts2)
                                  <=< unbind
                                  ) alts1
              changed $ Case scrut alts2Ty newAlts
      else return e

caseCase _ e = return e

-- | Inline function with a non-representable result if it's the subject
-- of a Case-decomposition
inlineNonRep :: NormRewrite
inlineNonRep _ e@(Case scrut altsTy alts)
  | (Var _ f, args) <- collectArgs scrut
  = do
    cf        <- Lens.use curFun
    isInlined <- zoomExtra (alreadyInlined f cf)
    limit     <- Lens.use (extra.inlineLimit)
    tcm       <- Lens.view tcCache
    scrutTy   <- termType tcm scrut
    let noException = not (exception tcm scrutTy)
    if noException && (Maybe.fromMaybe 0 isInlined) > limit
      then do
        ty <- termType tcm scrut
        traceIf True (concat [$(curLoc) ++ "InlineNonRep: " ++ show f
                             ," already inlined " ++ show limit ++ " times in:"
                             , show cf
                             , "\nType of the subject is: " ++ showDoc ty
                             , "\nFunction " ++ show cf
                             , "will not reach a normal form, and compilation"
                             , "might fail."
                             , "\nRun with '-clash-inline-limit=N' to increase"
                             , "the inlining limit to N."
                             ])
                     (return e)
      else do
        bodyMaybe   <- fmap (HashMap.lookup f) $ Lens.use bindings
        nonRepScrut <- not <$> (representableType <$> Lens.view typeTranslator <*> Lens.view tcCache <*> pure scrutTy)
        case (nonRepScrut, bodyMaybe) of
          (True,Just (_, scrutBody)) -> do
            Monad.when noException (zoomExtra (addNewInline f cf))
            changed $ Case (mkApps scrutBody args) altsTy alts
          _ -> return e
  where
    exception tcm ((tyView . typeKind tcm) -> TyConApp (name2String -> "GHC.Prim.Constraint") _) = True
    exception _ _ = False

inlineNonRep _ e = return e

-- | Specialize a Case-decomposition (replace by the RHS of an alternative) if
-- the subject is (an application of) a DataCon; or if there is only a single
-- alternative that doesn't reference variables bound by the pattern.
caseCon :: NormRewrite
caseCon _ c@(Case scrut _ alts)
  | (Data dc, args) <- collectArgs scrut
  = do
    alts' <- mapM unbind alts
    let dcAltM = List.find (equalCon dc . fst) alts'
    case dcAltM of
      Just (DataPat _ pxs, e) ->
        let (tvs,xs) = unrebind pxs
            fvs = Lens.toListOf termFreeIds e
            (binds,_) = List.partition ((`elem` fvs) . varName . fst)
                      $ zip xs (Either.lefts args)
            e' = case binds of
                  [] -> e
                  _  -> Letrec $ bind (rec $ map (second embed) binds) e
            substTyMap = zip (map varName tvs) (drop (length $ dcUnivTyVars dc) (Either.rights args))
        in  changed (substTysinTm substTyMap e')
      _ -> case alts' of
             ((DefaultPat,e):_) -> changed e
             _ -> error $ $(curLoc) ++ "Report as bug: caseCon error: " ++ showDoc c
  where
    equalCon dc (DataPat dc' _) = dcTag dc == dcTag (unembed dc')
    equalCon _  _               = False

caseCon _ c@(Case (Literal l) _ alts) = do
  alts' <- mapM unbind alts
  let ltAltsM = List.find (equalLit . fst) alts'
  case ltAltsM of
    Just (LitPat _,e) -> changed e
    _ -> case alts' of
           ((DefaultPat,e):_) -> changed e
           _ -> error $ $(curLoc) ++ "Report as bug: caseCon error: " ++ showDoc c
  where
    equalLit (LitPat l')     = l == (unembed l')
    equalLit _               = False

caseCon ctx e@(Case subj ty alts)
  | isConstant subj = do
    tcm <- Lens.view tcCache
    lvl <- Lens.view dbgLevel
    reduceConstant <- Lens.view evaluator
    case reduceConstant tcm True subj of
      Literal l -> caseCon ctx (Case (Literal l) ty alts)
      subj'@(collectArgs -> (Data _,_)) -> caseCon ctx (Case subj' ty alts)
      subj' -> traceIf (lvl > DebugNone) ("Irreducible constant as case subject: " ++ showDoc subj ++ "\nCan be reduced to: " ++ showDoc subj') (caseOneAlt e)

caseCon _ e = caseOneAlt e

caseOneAlt :: Term -> RewriteMonad extra Term
caseOneAlt e@(Case _ _ [alt]) = do
  (pat,altE) <- unbind alt
  case pat of
    DefaultPat    -> changed altE
    LitPat _      -> changed altE
    DataPat _ pxs -> let (tvs,xs)   = unrebind pxs
                         ftvs       = Lens.toListOf termFreeTyVars altE
                         fvs        = Lens.toListOf termFreeIds altE
                         usedTvs    = filter ((`elem` ftvs) . varName) tvs
                         usedXs     = filter ((`elem` fvs) . varName) xs
                     in  case (usedTvs,usedXs) of
                           ([],[]) -> changed altE
                           _       -> return e

caseOneAlt e = return e

-- | Bring an application of a DataCon or Primitive in ANF, when the argument is
-- is considered non-representable
nonRepANF :: NormRewrite
nonRepANF ctx e@(App appConPrim arg)
  | (conPrim, _) <- collectArgs e
  , isCon conPrim || isPrim conPrim
  = do
    untranslatable <- isUntranslatable arg
    case (untranslatable,arg) of
      (True,Letrec b) -> do (binds,body) <- unbind b
                            changed (Letrec (bind binds (App appConPrim body)))
      (True,Case {})  -> specializeNorm ctx e
      (True,Lam _)    -> specializeNorm ctx e
      _               -> return e

nonRepANF _ e = return e

-- | Ensure that top-level lambda's eventually bind a let-expression of which
-- the body is a variable-reference.
topLet :: NormRewrite
topLet ctx e
  | all isLambdaBodyCtx ctx && not (isLet e)
  = do
  untranslatable <- isUntranslatable e
  if untranslatable
    then return e
    else do tcm <- Lens.view tcCache
            (argId,argVar) <- mkTmBinderFor tcm "topLet" e
            changed . Letrec $ bind (rec [(argId,embed e)]) argVar

topLet ctx e@(Letrec b)
  | all isLambdaBodyCtx ctx
  = do
    (binds,body)   <- unbind b
    localVar       <- isLocalVar body
    untranslatable <- isUntranslatable body
    if localVar || untranslatable
      then return e
      else do tcm <- Lens.view tcCache
              (argId,argVar) <- mkTmBinderFor tcm "topLet" body
              changed . Letrec $ bind (rec $ unrec binds ++ [(argId,embed body)]) argVar

topLet _ e = return e

-- Misc rewrites

-- | Remove unused let-bindings
deadCode :: NormRewrite
deadCode _ e@(Letrec binds) = do
    (xes, body) <- fmap (first unrec) $ unbind binds
    let bodyFVs = Lens.toListOf termFreeIds body
        (xesUsed,xesOther) = List.partition
                               ( (`elem` bodyFVs )
                               . varName
                               . fst
                               ) xes
        xesUsed' = findUsedBndrs [] xesUsed xesOther
    if length xesUsed' /= length xes
      then changed . Letrec $ bind (rec xesUsed') body
      else return e
  where
    findUsedBndrs :: [(Var Term, Embed Term)] -> [(Var Term, Embed Term)]
                  -> [(Var Term, Embed Term)] -> [(Var Term, Embed Term)]
    findUsedBndrs used []      _     = used
    findUsedBndrs used explore other =
      let fvsUsed = concatMap (Lens.toListOf termFreeIds . unembed . snd) explore
          (explore',other') = List.partition
                                ( (`elem` fvsUsed)
                                . varName
                                . fst
                                ) other
      in findUsedBndrs (used ++ explore) explore' other'

deadCode _ e = return e

-- | Inline let-bindings when the RHS is either a local variable reference or
-- is constant
bindConstantVar :: NormRewrite
bindConstantVar = inlineBinders test
  where
    test _ (_,Embed e) = (||) <$> isLocalVar e <*> pure (isConstant e)

-- | Inline nullary/closed functions
inlineClosed :: NormRewrite
inlineClosed _ e@(collectArgs -> (Var _ f,args))
  | all (either isConstant (const True)) args
  = do
    tcm <- Lens.view tcCache
    eTy <- termType tcm e
    untranslatable <- isUntranslatableType eTy
    let isSignal = isSignalType tcm eTy
    if untranslatable || isSignal
      then return e
      else do
        bndrs <- Lens.use bindings
        case HashMap.lookup f bndrs of
          -- Don't inline recursive expressions
          Just (_,body) -> let cg = callGraph [] bndrs f
                           in if null (recursiveComponents cg)
                              then changed (mkApps body args)
                              else return e
          _ -> return e

inlineClosed _ e@(Var fTy f) = do
  tcm <- Lens.view tcCache
  let closed   = not (isPolyFunCoreTy tcm fTy)
      isSignal = isSignalType tcm fTy
  untranslatable <- isUntranslatableType fTy
  if closed && not untranslatable && not isSignal
    then do
      bndrs <- Lens.use bindings
      case HashMap.lookup f bndrs of
        -- Don't inline recursive expressions
        Just (_,body) -> let cg = callGraph [] bndrs f
                         in if null (recursiveComponents cg)
                            then changed body
                            else return e
        _ -> return e
    else return e

inlineClosed _ e = return e

-- | Inline small functions
inlineSmall :: NormRewrite
inlineSmall _ e@(collectArgs -> (Var _ f,args)) = do
  untranslatable <- isUntranslatable e
  if untranslatable
    then return e
    else do
      bndrs <- Lens.use bindings
      sizeLimit <- Lens.use (extra.inlineBelow)
      case HashMap.lookup f bndrs of
        -- Don't inline recursive expressions
        Just (_,body) -> let cg = callGraph [] bndrs f
                         in if null (recursiveComponents cg) &&
                               termSize body < sizeLimit
                            then changed (mkApps body args)
                            else return e
        _ -> return e

inlineSmall _ e = return e

-- | Specialise functions on arguments which are constant
constantSpec :: NormRewrite
constantSpec ctx e@(App e1 e2)
  | (Var _ _, args) <- collectArgs e1
  , (_, [])     <- Either.partitionEithers args
  , null $ Lens.toListOf termFreeTyVars e2
  , isConstant e2
  = specializeNorm ctx e

constantSpec _ e = return e


-- Experimental

-- | Propagate arguments of application inwards; except for 'Lam' where the
-- argument becomes let-bound.
appProp :: NormRewrite
appProp _ (App (Lam b) arg) = do
  (v,e) <- unbind b
  if isConstant arg || isVar arg
    then changed $ substTm (varName v) arg e
    else changed . Letrec $ bind (rec [(v,embed arg)]) e

appProp _ (App (Letrec b) arg) = do
  (v,e) <- unbind b
  changed . Letrec $ bind v (App e arg)

appProp _ (App (Case scrut ty alts) arg) = do
  tcm <- Lens.view tcCache
  argTy <- termType tcm arg
  let ty' = applyFunTy tcm ty argTy
  if isConstant arg || isVar arg
    then do
      alts' <- mapM ( return
                    . uncurry bind
                    . second (`App` arg)
                    <=< unbind
                    ) alts
      changed $ Case scrut ty' alts'
    else do
      (boundArg,argVar) <- mkTmBinderFor tcm "caseApp" arg
      alts' <- mapM ( return
                    . uncurry bind
                    . second (`App` argVar)
                    <=< unbind
                    ) alts
      changed . Letrec $ bind (rec [(boundArg,embed arg)]) (Case scrut ty' alts')

appProp _ (TyApp (TyLam b) t) = do
  (tv,e) <- unbind b
  changed $ substTyInTm (varName tv) t e

appProp _ (TyApp (Letrec b) t) = do
  (v,e) <- unbind b
  changed . Letrec $ bind v (TyApp e t)

appProp _ (TyApp (Case scrut altsTy alts) ty) = do
  alts' <- mapM ( return
                . uncurry bind
                . second (`TyApp` ty)
                <=< unbind
                ) alts
  tcm <- Lens.view tcCache
  ty' <- applyTy tcm altsTy ty
  changed $ Case scrut ty' alts'

appProp _ e = return e

type NormRewriteW = Transform (WriterT [LetBinding] (RewriteMonad NormalizeState))

-- NOTE [unsafeUnbind]: Use unsafeUnbind (which doesn't freshen pattern
-- variables). Reason: previously collected expression still reference
-- the 'old' variable names created by the traversal!

-- | Turn an expression into a modified ANF-form. As opposed to standard ANF,
-- constants do not become let-bound.
makeANF :: NormRewrite
makeANF ctx (Lam b) = do
  -- See NOTE [unsafeUnbind]
  let (bndr,e) = unsafeUnbind b
  e' <- makeANF (LamBody bndr:ctx) e
  return $ Lam (bind bndr e')

makeANF _ (TyLam b) = return (TyLam b)

makeANF ctx e
  = do
    (e',bndrs) <- runWriterT $ bottomupR collectANF ctx e
    case bndrs of
      [] -> return e
      _  -> changed . Letrec $ bind (rec bndrs) e'

collectANF :: NormRewriteW
collectANF _ e@(App appf arg)
  | (conVarPrim, _) <- collectArgs e
  , isCon conVarPrim || isPrim conVarPrim || isVar conVarPrim
  = do
    untranslatable <- lift (isUntranslatable arg)
    localVar       <- lift (isLocalVar arg)
    case (untranslatable,localVar || isConstant arg,arg) of
      (False,False,_) -> do tcm <- Lens.view tcCache
                            (argId,argVar) <- lift (mkTmBinderFor tcm "repANF" arg)
                            tell [(argId,embed arg)]
                            return (App appf argVar)
      (True,False,Letrec b) -> do (binds,body) <- unbind b
                                  tell (unrec binds)
                                  return (App appf body)
      _ -> return e

collectANF _ (Letrec b) = do
  -- See NOTE [unsafeUnbind]
  let (binds,body) = unsafeUnbind b
  tell (unrec binds)
  untranslatable <- lift (isUntranslatable body)
  localVar       <- lift (isLocalVar body)
  if localVar || untranslatable
    then return body
    else do
      tcm <- Lens.view tcCache
      (argId,argVar) <- lift (mkTmBinderFor tcm "bodyVar" body)
      tell [(argId,embed body)]
      return argVar

-- TODO: The code below special-cases ANF for the ':-' constructor for the
-- 'Signal' type. The 'Signal' type is essentially treated as a "transparent"
-- type by the CLaSH compiler, so observing its constructor leads to all kinds
-- of problems. In this case that "CLaSH.Rewrite.Util.mkSelectorCase" will
-- try to project the LHS and RHS of the ':-' constructor, however,
-- 'mkSelectorCase' uses 'coreView' to find the "real" data-constructor.
-- 'coreView' however looks through the 'Signal' type, and hence 'mkSelector'
-- finds the data constructors for the element type of Signal. This resulted in
-- error #24 (https://github.com/christiaanb/clash2/issues/24), where we
-- try to get the first field out of the 'Vec's 'Nil' constructor.
--
-- Ultimately we should stop treating Signal as a "transparent" type and deal
-- handling of the Signal type, and the involved co-recursive functions,
-- properly. At the moment, CLaSH cannot deal with this recursive type and the
-- recursive functions involved, hence the need for special-casing code. After
-- everything is done properly, we should remove the two lines below.
collectANF _ e@(Case _ _ [unsafeUnbind -> (DataPat dc _,_)])
  | name2String (dcName $ unembed dc) == "CLaSH.Signal.Internal.:-" = return e

collectANF ctx (Case subj ty alts) = do
    localVar     <- lift (isLocalVar subj)
    (bndr,subj') <- if localVar || isConstant subj
      then return ([],subj)
      else do tcm <- Lens.view tcCache
              (argId,argVar) <- lift (mkTmBinderFor tcm "subjLet" subj)
              return ([(argId,embed subj)],argVar)

    (binds,alts') <- fmap (first concat . unzip) $ mapM (lift . doAlt subj') alts

    tell (bndr ++ binds)
    return (Case subj' ty alts')
  where
    doAlt :: Term -> Bind Pat Term -> RewriteMonad NormalizeState ([LetBinding],Bind Pat Term)
    -- See NOTE [unsafeUnbind]
    doAlt subj' = fmap (second (uncurry bind)) . doAlt' subj' . unsafeUnbind

    doAlt' :: Term -> (Pat,Term) -> RewriteMonad NormalizeState ([LetBinding],(Pat,Term))
    doAlt' subj' alt@(DataPat dc pxs@(unrebind -> ([],xs)),altExpr) = do
      lv      <- isLocalVar altExpr
      patSels <- Monad.zipWithM (doPatBndr subj' (unembed dc)) xs [0..]
      let usesXs (Var _ n) = any ((== n) . varName) xs
          usesXs _         = False
      if (lv && not (usesXs altExpr)) || isConstant altExpr
        then return (patSels,alt)
        else do tcm <- Lens.view tcCache
                (altId,altVar) <- mkTmBinderFor tcm "altLet" altExpr
                return ((altId,embed altExpr):patSels,(DataPat dc pxs,altVar))
    doAlt' _ alt@(DataPat _ _, _) = return ([],alt)
    doAlt' _ alt@(pat,altExpr) = do
      lv <- isLocalVar altExpr
      if lv || isConstant altExpr
        then return ([],alt)
        else do tcm <- Lens.view tcCache
                (altId,altVar) <- mkTmBinderFor tcm "altLet" altExpr
                return ([(altId,embed altExpr)],(pat,altVar))

    doPatBndr :: Term -> DataCon -> Id -> Int -> RewriteMonad NormalizeState LetBinding
    doPatBndr subj' dc pId i
      = do tcm <- Lens.view tcCache
           patExpr <- mkSelectorCase ($(curLoc) ++ "doPatBndr") tcm ctx subj' (dcTag dc) i
           return (pId,embed patExpr)

collectANF _ e = return e

-- | Eta-expand top-level lambda's (DON'T use in a traversal!)
etaExpansionTL :: NormRewrite
etaExpansionTL ctx (Lam b) = do
  (bndr,e) <- unbind b
  e' <- etaExpansionTL (LamBody bndr:ctx) e
  return $ Lam (bind bndr e')

etaExpansionTL ctx e
  = do
    tcm <- Lens.view tcCache
    isF <- isFun tcm e
    if isF
      then do
        argTy <- ( return
                 . fst
                 . Maybe.fromMaybe (error $ $(curLoc) ++ "etaExpansion splitFunTy")
                 . splitFunTy tcm
                 <=< termType tcm
                 ) e
        (newIdB,newIdV) <- mkInternalVar "eta" argTy
        e' <- etaExpansionTL (LamBody newIdB:ctx) (App e newIdV)
        changed . Lam $ bind newIdB e'
      else return e

-- | Turn a  normalized recursive function, where the recursive calls only pass
-- along the unchanged original arguments, into let-recursive function. This
-- means that all recursive calls are replaced by the same variable reference as
-- found in the body of the top-level let-expression.
recToLetRec :: NormRewrite
recToLetRec [] e = do
  fn          <- Lens.use curFun
  bodyM       <- fmap (HashMap.lookup fn) $ Lens.use bindings
  tcm         <- Lens.view tcCache
  normalizedE <- splitNormalized tcm e
  case (normalizedE,bodyM) of
    (Right (args,bndrs,res), Just (bodyTy,_)) -> do
      let appF              = mkTmApps (Var bodyTy fn) (map idToVar args)
          (toInline,others) = List.partition ((==) appF . unembed . snd) bndrs
          resV              = idToVar res
      case (toInline,others) of
        (_:_,_:_) -> do
          let substsInline = map (\(id_,_) -> (varName id_,resV)) toInline
              others'      = map (second (embed . substTms substsInline . unembed)) others
          changed $ mkLams (Letrec $ bind (rec others') resV) args
        _ -> return e
    _ -> return e

recToLetRec _ e = return e

-- | Inline a function with functional arguments
inlineHO :: NormRewrite
inlineHO _ e@(App _ _)
  | (Var _ f, args) <- collectArgs e
  = do
    tcm <- Lens.view tcCache
    hasPolyFunArgs <- or <$> mapM (either (isPolyFun tcm) (const (return False))) args
    if hasPolyFunArgs
      then do cf        <- Lens.use curFun
              isInlined <- zoomExtra (alreadyInlined f cf)
              limit     <- Lens.use (extra.inlineLimit)
              if (Maybe.fromMaybe 0 isInlined) > limit
                then do
                  lvl <- Lens.view dbgLevel
                  traceIf (lvl > DebugNone) ($(curLoc) ++ "InlineHO: " ++ show f ++ " already inlined " ++ show limit ++ " times in:" ++ show cf) (return e)
                else do
                  bodyMaybe <- fmap (HashMap.lookup f) $ Lens.use bindings
                  case bodyMaybe of
                    Just (_, body) -> do
                      zoomExtra (addNewInline f cf)
                      changed (mkApps body args)
                    _ -> return e
      else return e

inlineHO _ e = return e

-- | Simplified CSE, only works on let-bindings, works from top to bottom
simpleCSE :: NormRewrite
simpleCSE _ e@(Letrec b) = do
  (binders,body) <- first unrec <$> unbind b
  let (reducedBindings,body') = reduceBindersFix binders body
  if length binders /= length reducedBindings
     then changed (Letrec (bind (rec reducedBindings) body'))
     else return e

simpleCSE _ e = return e

reduceBindersFix :: [LetBinding]
                 -> Term
                 -> ([LetBinding],Term)
reduceBindersFix binders body = if length binders /= length reduced
                                   then reduceBindersFix reduced body'
                                   else (binders,body)
  where
    (reduced,body') = reduceBinders [] body binders

reduceBinders :: [LetBinding]
              -> Term
              -> [LetBinding]
              -> ([LetBinding],Term)
reduceBinders processed body [] = (processed,body)
reduceBinders processed body ((id_,expr):binders) = case List.find ((== expr) . snd) processed of
    Just (id2,_) ->
      let var        = Var (unembed (varType id2)) (varName id2)
          idName     = varName id_
          processed' = map (second (Embed . (substTm idName var) . unembed)) processed
          binders'   = map (second (Embed . (substTm idName var) . unembed)) binders
          body'      = substTm idName var body
      in  reduceBinders processed' body' binders'
    Nothing -> reduceBinders ((id_,expr):processed) body binders

reduceConst :: NormRewrite
reduceConst _ e@(App _ _)
  | isConstant e
  , (conPrim, _) <- collectArgs e
  , isPrim conPrim
  = do
    tcm <- Lens.view tcCache
    reduceConstant <- Lens.view evaluator
    case reduceConstant tcm False e of
      e'@(Data _)    -> changed e'
      e'@(Literal _) -> changed e'
      _              -> return e

reduceConst _ e = return e

-- | Replace primitives by their "definition" if they would lead to let-bindings
-- with a non-representable type when a function is in ANF. This happens for
-- example when CLaSH.Size.Vector.map consumes or produces a vector of
-- non-representable elements.
--
-- Basically what this transformation does is replace a primitive the completely
-- unrolled recursive definition that it represents. e.g.
--
-- > zipWith ($) (xs :: Vec 2 (Int -> Int)) (ys :: Vec 2 Int)
--
-- is replaced by:
--
-- > let (x0  :: (Int -> Int))       = case xs  of (:>) _ x xr -> x
-- >     (xr0 :: Vec 1 (Int -> Int)) = case xs  of (:>) _ x xr -> xr
-- >     (x1  :: (Int -> Int)(       = case xr0 of (:>) _ x xr -> x
-- >     (y0  :: Int)                = case ys  of (:>) _ y yr -> y
-- >     (yr0 :: Vec 1 Int)          = case ys  of (:>) _ y yr -> xr
-- >     (y1  :: Int                 = case yr0 of (:>) _ y yr -> y
-- > in  (($) x0 y0 :> ($) x1 y1 :> Nil)
--
-- Currently, it only handles the following functions:
--
-- * CLaSH.Sized.Vector.map
-- * CLaSH.Sized.Vector.zipWith
-- * CLaSH.Sized.Vector.traverse#
reduceNonRepPrim :: NormRewrite
reduceNonRepPrim _ e@(App _ _)
  | (Prim f _, args) <- collectArgs e
  = case f of
      "CLaSH.Sized.Vector.zipWith" | length args == 7 -> do
        let [lhsElTy,rhsElty,resElTy,nTy] = Either.rights args
        case nTy of
          (LitTy (NumTy n)) -> do
            untranslatableTys <- mapM isUntranslatableType [lhsElTy,rhsElty,resElTy]
            if or untranslatableTys
               then let [fun,lhsArg,rhsArg] = Either.lefts args
                    in  reduceZipWith n lhsElTy rhsElty resElTy fun lhsArg rhsArg
               else return e
          _ -> return e
      "CLaSH.Sized.Vector.map" | length args == 5 -> do
        let [argElTy,resElTy,nTy] = Either.rights args
        case nTy of
          (LitTy (NumTy n)) -> do
            untranslatableTys <- mapM isUntranslatableType [argElTy,resElTy]
            if or untranslatableTys
               then let [fun,arg] = Either.lefts args
                    in  reduceMap n argElTy resElTy fun arg
               else return e
          _ -> return e
      "CLaSH.Sized.Vector.traverse#" | length args == 7 ->
        let [aTy,fTy,bTy,nTy] = Either.rights args
        in  case nTy of
          (LitTy (NumTy n)) ->
            let [dict,fun,arg] = Either.lefts args
            in  reduceTraverse n aTy fTy bTy dict fun arg
          _ -> return e
      "CLaSH.Sized.Vector.fold" | length args == 4 -> do
        let [aTy,nTy] = Either.rights args
            isPow2 x  = x /= 0 && (x .&. (complement x + 1)) == x
        untranslatableTy <- isUntranslatableType aTy
        case nTy of
          (LitTy (NumTy n)) | not (isPow2 (n + 1)) || untranslatableTy ->
            let [fun,arg] = Either.lefts args
            in  reduceFold (n + 1) aTy fun arg
          _ -> return e
      _ -> return e

reduceNonRepPrim _ e = return e

-- | Replace an application of @CLaSH.Sized.Vector.zipWith@ primitive on vectors
-- of a known length @n@, by the fully unrolled recursive "definition" of of
-- @CLaSH.Sized.Vector.zipWith@
reduceZipWith :: Int  -- ^ Length of the vector(s)
              -> Type -- ^ Type of the lhs of the function
              -> Type -- ^ Type of the rhs of the function
              -> Type -- ^ Type of the result of the function
              -> Term -- ^ The zipWith'd functions
              -> Term -- ^ The 1st vector argument
              -> Term -- ^ The 2nd vector argument
              -> NormalizeSession Term
reduceZipWith n lhsElTy rhsElTy resElTy fun lhsArg rhsArg = do
  tcm <- Lens.view tcCache
  (TyConApp vecTcNm _) <- tyView <$> termType tcm lhsArg
  let (Just vecTc)     = HashMap.lookup vecTcNm tcm
      [nilCon,consCon] = tyConDataCons vecTc
      (varsL,elemsL)   = second concat . unzip $ extractElems consCon lhsElTy 'L' n lhsArg
      (varsR,elemsR)   = second concat . unzip $ extractElems consCon rhsElTy 'R' n rhsArg
      funApps          = zipWith (\l r -> mkApps fun [Left l,Left r]) varsL varsR
      lbody            = mkVec nilCon consCon resElTy n funApps
      lb               = Letrec (bind (rec (init elemsL ++ init elemsR)) lbody)
  changed lb

-- | Replace an application of @CLaSH.Sized.Vector.map@ primitive on vectors
-- of a known length @n@, by the fully unrolled recursive "definition" of of
-- @CLaSH.Sized.Vector.map@
reduceMap :: Int  -- ^ Length of the vector
          -> Type -- ^ Argument type of the function
          -> Type -- ^ Result type of the function
          -> Term -- ^ The map'd function
          -> Term -- ^ The map'd over vector
          -> NormalizeSession Term
reduceMap n argElTy resElTy fun arg = do
  tcm <- Lens.view tcCache
  (TyConApp vecTcNm _) <- tyView <$> termType tcm arg
  let (Just vecTc)     = HashMap.lookup vecTcNm tcm
      [nilCon,consCon] = tyConDataCons vecTc
      (vars,elems)     = second concat . unzip $ extractElems consCon argElTy 'A' n arg
      funApps          = map (fun `App`) vars
      lbody            = mkVec nilCon consCon resElTy n funApps
      lb               = Letrec (bind (rec (init elems)) lbody)
  changed lb

-- | Replace an application of @CLaSH.Sized.Vector.traverse#@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @CLaSH.Sized.Vector.map@
reduceTraverse :: Int  -- ^ Length of the vector
               -> Type -- ^ Element type of the argument vector
               -> Type -- ^ The type of the applicative
               -> Type -- ^ Element type of the result vector
               -> Term -- ^ The @Applicative@ dictionary
               -> Term -- ^ The function to traverse with
               -> Term -- ^ The argument vector
               -> NormalizeSession Term
reduceTraverse n aTy fTy bTy dict fun arg = do
  tcm <- Lens.view tcCache
  (TyConApp vecTcNm    _) <- tyView <$> termType tcm arg
  (TyConApp apDictTcNm _) <- tyView <$> termType tcm dict
  let (Just apDictTc)    = HashMap.lookup apDictTcNm tcm
      [apDictCon]        = tyConDataCons apDictTc
      (Just apDictIdTys) = dataConInstArgTys apDictCon [fTy]
      apDictIds          = zipWith Id (map string2Name ["functorDict"
                                                       ,"pure"
                                                       ,"ap"
                                                       ,"apConstL"
                                                       ,"apConstR"])
                                      (map embed apDictIdTys)

      (TyConApp funcDictTcNm _) = tyView (head apDictIdTys)
      (Just funcDictTc) = HashMap.lookup funcDictTcNm tcm
      [funcDictCon] = tyConDataCons funcDictTc
      (Just funcDictIdTys) = dataConInstArgTys funcDictCon [fTy]
      funcDicIds    = zipWith Id (map string2Name ["fmap","fmapConst"])
                                 (map embed funcDictIdTys)

      apPat    = DataPat (embed apDictCon) (rebind [] apDictIds)
      fnPat    = DataPat (embed funcDictCon) (rebind [] funcDicIds)

      -- Extract the 'pure' function from the Applicative dictionary
      pureTy = apDictIdTys!!1
      pureTm = Case dict pureTy [bind apPat (Var pureTy (string2Name "pure"))]

      -- Extract the '<*>' function from the Applicative dictionary
      apTy   = apDictIdTys!!2
      apTm   = Case dict apTy [bind apPat (Var apTy (string2Name "ap"))]

      -- Extract the Functor dictionary from the Applicative dictionary
      funcTy = (head apDictIdTys)
      funcTm = Case dict funcTy
                         [bind apPat (Var funcTy (string2Name "functorDict"))]

      -- Extract the 'fmap' function from the Functor dictionary
      fmapTy = (head funcDictIdTys)
      fmapTm = Case (Var funcTy (string2Name "functorDict")) fmapTy
                    [bind fnPat (Var fmapTy (string2Name "fmap"))]

      (Just vecTc)     = HashMap.lookup vecTcNm tcm
      [nilCon,consCon] = tyConDataCons vecTc
      (vars,elems)     = second concat . unzip
                                       $ extractElems consCon aTy 'T' n arg

      funApps = map (fun `App`) vars

      lbody   = mkTravVec vecTcNm nilCon consCon (idToVar (apDictIds!!1))
                                                 (idToVar (apDictIds!!2))
                                                 (idToVar (funcDicIds!!0))
                                                 bTy n funApps

      lb      = Letrec (bind (rec ([((apDictIds!!0),embed funcTm)
                                   ,((apDictIds!!1),embed pureTm)
                                   ,((apDictIds!!2),embed apTm)
                                   ,((funcDicIds!!0),embed fmapTm)
                                   ] ++ init elems)) lbody)
  changed lb

-- | Create the traversable vector
--
-- e.g. for a length '2' input vector, we get
--
-- > (:>) <$> x0 <*> ((:>) <$> x1 <*> pure Nil)
mkTravVec :: TyConName -- ^ Vec tcon
          -> DataCon   -- ^ Nil con
          -> DataCon   -- ^ Cons con
          -> Term      -- ^ 'pure' term
          -> Term      -- ^ '<*>' term
          -> Term      -- ^ 'fmap' term
          -> Type      -- ^ 'b' ty
          -> Int       -- ^ Length of the vector
          -> [Term]    -- ^ Elements of the vector
          -> Term
mkTravVec vecTc nilCon consCon pureTm apTm fmapTm bTy = go
  where
    go :: Int -> [Term] -> Term
    go _ [] = mkApps pureTm [Right (mkTyConApp vecTc [LitTy (NumTy 0),bTy])
                            ,Left  (mkApps (Data nilCon)
                                           [Right (LitTy (NumTy 0))
                                           ,Right bTy
                                           ,Left  (Prim "_CO_" nilCoTy)])]

    go n (x:xs) = mkApps apTm
      [Right (mkTyConApp vecTc [LitTy (NumTy (n-1)),bTy])
      ,Right (mkTyConApp vecTc [LitTy (NumTy n),bTy])
      ,Left (mkApps fmapTm [Right bTy
                           ,Right (mkFunTy (mkTyConApp vecTc [LitTy (NumTy (n-1)),bTy])
                                           (mkTyConApp vecTc [LitTy (NumTy n),bTy]))
                           ,Left  (mkApps (Data consCon)
                                          [Right (LitTy (NumTy n))
                                          ,Right bTy
                                          ,Right (LitTy (NumTy (n-1)))
                                          ,Left  (Prim "_CO_" (consCoTy n))
                                          ])
                           ,Left  x])
      ,Left (go (n-1) xs)]

    nilCoTy = head (Maybe.fromJust (dataConInstArgTys nilCon [(LitTy (NumTy 0))
                                                             ,bTy]))

    consCoTy n = head (Maybe.fromJust (dataConInstArgTys consCon
                                                         [(LitTy (NumTy n))
                                                         ,bTy
                                                         ,(LitTy (NumTy (n-1)))]))

reduceFold :: Int  -- ^ Length of the vector
           -> Type -- ^ Element type of the argument vector
           -> Term -- ^ The function to fold with
           -> Term -- ^ The argument vector
           -> NormalizeSession Term
reduceFold n aTy fun arg = do
    tcm <- Lens.view tcCache
    (TyConApp vecTcNm _) <- tyView <$> termType tcm arg
    let (Just vecTc)     = HashMap.lookup vecTcNm tcm
        [_,consCon]      = tyConDataCons vecTc
        (vars,elems)     = second concat . unzip
                         $ extractElems consCon aTy 'F' n arg
        lbody            = foldV vars
        lb               = Letrec (bind (rec (init elems)) lbody)
    changed lb
  where
    foldV [a] = a
    foldV as  = let (l,r) = splitAt (length as `div` 2) as
                    lF    = foldV l
                    rF    = foldV r
                in  mkApps fun [Left lF, Left rF]
