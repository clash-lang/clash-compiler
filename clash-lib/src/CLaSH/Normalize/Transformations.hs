{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ViewPatterns    #-}

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
  )
where

import qualified Control.Lens                as Lens
import qualified Control.Monad               as Monad
import           Control.Monad.Writer        (WriterT (..), lift, tell)
import qualified Data.Either                 as Either
import qualified Data.HashMap.Lazy           as HashMap
import qualified Data.List                   as List
import qualified Data.Maybe                  as Maybe
import           Unbound.Generics.LocallyNameless     (Bind, Embed (..), bind, embed,
                                              rec, unbind, unembed, unrebind,
                                              unrec, name2String)
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           CLaSH.Core.DataCon          (DataCon, dcName, dcTag,
                                              dcUnivTyVars)
import           CLaSH.Core.FreeVars         (termFreeIds, termFreeTyVars,
                                              typeFreeVars)
import           CLaSH.Core.Pretty           (showDoc)
import           CLaSH.Core.Subst            (substTm, substTms, substTyInTm,
                                              substTysinTm)
import           CLaSH.Core.Term             (LetBinding, Pat (..), Term (..))
import           CLaSH.Core.Type             (TypeView (..), applyFunTy,
                                              applyTy, splitFunTy, typeKind, tyView)
import           CLaSH.Core.Util             (collectArgs, idToVar, isCon,
                                              isFun, isLet, isPolyFun, isPrim,
                                              isVar, mkApps, mkLams, mkTmApps,
                                              termSize,termType)
import           CLaSH.Core.Var              (Id, Var (..))
import           CLaSH.Netlist.Util          (representableType,
                                              splitNormalized)
import           CLaSH.Normalize.Types
import           CLaSH.Normalize.Util
import           CLaSH.Rewrite.Combinators
import           CLaSH.Rewrite.Types
import           CLaSH.Rewrite.Util
import           CLaSH.Util

-- | Inline non-recursive, non-representable let-bindings
bindNonRep :: NormRewrite
bindNonRep = inlineBinders nonRepTest
  where
    nonRepTest (Id idName tyE, exprE)
      = (&&) <$> (not <$> (representableType <$> Lens.use typeTranslator <*> Lens.use tcCache <*> pure (unembed tyE)))
             <*> (notElem idName <$> (Lens.toListOf <$> localFreeIds <*> pure (unembed exprE)))

    nonRepTest _ = return False

-- | Lift non-representable let-bindings
liftNonRep :: NormRewrite
liftNonRep = liftBinders nonRepTest
  where
    nonRepTest (Id idName tyE, exprE)
      = (&&) <$> (not <$> (representableType <$> Lens.use typeTranslator <*> Lens.use tcCache <*> pure (unembed tyE)))
             <*> (elem idName <$> (Lens.toListOf <$> localFreeIds <*> pure (unembed exprE)))

    nonRepTest _ = return False

-- | Specialize functions on their type
typeSpec :: NormRewrite
typeSpec ctx e@(TyApp e1 ty)
  | (Var _ _,  args) <- collectArgs e1
  , null $ Lens.toListOf typeFreeVars ty
  , (_, []) <- Either.partitionEithers args
  = specializeNorm False ctx e

typeSpec _ e = return e

-- | Specialize functions on their non-representable argument
nonRepSpec :: NormRewrite
nonRepSpec ctx e@(App e1 e2)
  | (Var _ _, args) <- collectArgs e1
  , (_, [])     <- Either.partitionEithers args
  , null $ Lens.toListOf termFreeTyVars e2
  = R $ do tcm <- Lens.use tcCache
           e2Ty <- termType tcm e2
           localVar <- isLocalVar e2
           nonRepE2 <- not <$> (representableType <$> Lens.use typeTranslator <*> Lens.use tcCache <*> pure e2Ty)
           if nonRepE2 && not localVar
             then runR $ specializeNorm True ctx e
             else return e

nonRepSpec _ e = return e

-- | Lift the let-bindings out of the subject of a Case-decomposition
caseLet :: NormRewrite
caseLet _ (Case (Letrec b) ty alts) = R $ do
  (xes,e) <- unbind b
  changed . Letrec $ bind xes (Case e ty alts)

caseLet _ e = return e

-- | Move a Case-decomposition from the subject of a Case-decomposition to the alternatives
caseCase :: NormRewrite
caseCase _ e@(Case (Case scrut alts1Ty alts1) alts2Ty alts2)
  = R $ do
    ty1Rep  <- representableType <$> Lens.use typeTranslator <*> Lens.use tcCache <*> pure alts1Ty
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
  = R $ do
    isInlined <- liftR $ alreadyInlined f
    limit     <- liftR $ Lens.use inlineLimit
    tcm       <- Lens.use tcCache
    scrutTy   <- termType tcm scrut
    let noException = not (exception tcm scrutTy)
    if noException && (Maybe.fromMaybe 0 isInlined) > limit
      then do
        cf <- liftR $ Lens.use curFun
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
        nonRepScrut <- not <$> (representableType <$> Lens.use typeTranslator <*> Lens.use tcCache <*> pure scrutTy)
        case (nonRepScrut, bodyMaybe) of
          (True,Just (_, scrutBody)) -> do
            Monad.when noException (liftR $ addNewInline f)
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
  = R $ do
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

caseCon _ c@(Case (Literal l) _ alts) = R $ do
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
    tcm <- Lens.use tcCache
    lvl <- Lens.view dbgLevel
    reduceConstant <- Lens.use evaluator
    case reduceConstant tcm subj of
      Literal l -> caseCon ctx (Case (Literal l) ty alts)
      subj'@(collectArgs -> (Data _,_)) -> caseCon ctx (Case subj' ty alts)
      subj' -> traceIf (lvl > DebugNone) ("Irreducible constant as case subject: " ++ showDoc subj ++ "\nCan be reduced to: " ++ showDoc subj') (caseOneAlt e)

caseCon _ e = caseOneAlt e

caseOneAlt :: Monad m => Term -> R m Term
caseOneAlt e@(Case _ _ [alt]) = R $ do
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
  = R $ do
    untranslatable <- isUntranslatable arg
    case (untranslatable,arg) of
      (True,Letrec b) -> do (binds,body) <- unbind b
                            changed . Letrec $ bind binds (App appConPrim body)
      (True,Case {})  -> runR $ specializeNorm True ctx e
      (True,Lam _)    -> runR $ specializeNorm True ctx e
      _               -> return e

nonRepANF _ e = return e

-- | Ensure that top-level lambda's eventually bind a let-expression of which
-- the body is a variable-reference.
topLet :: NormRewrite
topLet ctx e
  | all isLambdaBodyCtx ctx && not (isLet e)
  = R $ do
  untranslatable <- isUntranslatable e
  if untranslatable
    then return e
    else do tcm <- Lens.use tcCache
            (argId,argVar) <- mkTmBinderFor tcm "topLet" e
            changed . Letrec $ bind (rec [(argId,embed e)]) argVar

topLet ctx e@(Letrec b)
  | all isLambdaBodyCtx ctx
  = R $ do
    (binds,body)   <- unbind b
    localVar       <- isLocalVar body
    untranslatable <- isUntranslatable body
    if localVar || untranslatable
      then return e
      else do tcm <- Lens.use tcCache
              (argId,argVar) <- mkTmBinderFor tcm "topLet" body
              changed . Letrec $ bind (rec $ unrec binds ++ [(argId,embed body)]) argVar

topLet _ e = return e

-- Misc rewrites

-- | Remove unused let-bindings
deadCode :: NormRewrite
deadCode _ e@(Letrec binds) = R $ do
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
    test (_,Embed e) = (||) <$> isLocalVar e <*> pure (isConstant e)

-- | Inline nullary/closed functions
inlineClosed :: NormRewrite
inlineClosed _ e@(collectArgs -> (Var _ f,args))
  | all (either isConstant (const True)) args
  = R $ do
    untranslatable <- isUntranslatable e
    if untranslatable
      then return e
      else do
        bodyMaybe <- fmap (HashMap.lookup f) $ Lens.use bindings
        case bodyMaybe of
          -- Don't inline recursive expressions
          Just (_,body) -> if f `elem` (Lens.toListOf termFreeIds body)
                              then return e
                              else changed (mkApps body args)
          _ -> return e

inlineClosed _ e@(Var _ f) = R $ do
  tcm <- Lens.use tcCache
  closed <- isClosed tcm e
  untranslatable <- isUntranslatable e
  if closed && not untranslatable
    then do
      bodyMaybe <- fmap (HashMap.lookup f) $ Lens.use bindings
      case bodyMaybe of
        -- Don't inline recursive expressions
        Just (_,body) -> if f `elem` (Lens.toListOf termFreeIds body)
                            then return e
                            else changed body
        _ -> return e
    else return e

inlineClosed _ e = return e

-- | Inline small functions
inlineSmall :: NormRewrite
inlineSmall _ e@(collectArgs -> (Var _ f,args)) = R $ do
  untranslatable <- isUntranslatable e
  if untranslatable
    then return e
    else do
      isInlined <- liftR $ alreadyInlined f
      limit     <- liftR $ Lens.use inlineLimit
      if (Maybe.fromMaybe 0 isInlined) > limit
        then do
          cf <- liftR $ Lens.use curFun
          lvl <- Lens.view dbgLevel
          traceIf (lvl > DebugNone) ($(curLoc) ++ "InlineSmall: " ++ show f ++ " already inlined " ++ show limit ++ " times in:" ++ show cf) (return e)
        else do
          bodyMaybe <- HashMap.lookup f <$> Lens.use bindings
          case bodyMaybe of
            (Just (_,body))
              | termSize body < 5 -> do liftR $ addNewInline f
                                        changed (mkApps body args)
            _ -> return e

inlineSmall _ e = return e

-- | Specialise functions on arguments which are constant
constantSpec :: NormRewrite
constantSpec ctx e@(App e1 e2)
  | (Var _ _, args) <- collectArgs e1
  , (_, [])     <- Either.partitionEithers args
  , null $ Lens.toListOf termFreeTyVars e2
  , isConstant e2
  = specializeNorm False ctx e

constantSpec _ e = return e


-- Experimental

-- | Propagate arguments of application inwards; except for 'Lam' where the
-- argument becomes let-bound.
appProp :: NormRewrite
appProp _ (App (Lam b) arg) = R $ do
  (v,e) <- unbind b
  if isConstant arg || isVar arg
    then changed $ substTm (varName v) arg e
    else changed . Letrec $ bind (rec [(v,embed arg)]) e

appProp _ (App (Letrec b) arg) = R $ do
  (v,e) <- unbind b
  changed . Letrec $ bind v (App e arg)

appProp _ (App (Case scrut ty alts) arg) = R $ do
  tcm <- Lens.use tcCache
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

appProp _ (TyApp (TyLam b) t) = R $ do
  (tv,e) <- unbind b
  changed $ substTyInTm (varName tv) t e

appProp _ (TyApp (Letrec b) t) = R $ do
  (v,e) <- unbind b
  changed . Letrec $ bind v (TyApp e t)

appProp _ (TyApp (Case scrut altsTy alts) ty) = R $ do
  alts' <- mapM ( return
                . uncurry bind
                . second (`TyApp` ty)
                <=< unbind
                ) alts
  tcm <- Lens.use tcCache
  ty' <- applyTy tcm altsTy ty
  changed $ Case scrut ty' alts'

appProp _ e = return e

type NormRewriteW = Transform (WriterT [LetBinding] (R NormalizeMonad))

liftNormR :: RewriteMonad NormalizeMonad a
          -> WriterT [LetBinding] (R NormalizeMonad) a
liftNormR = lift . R

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
  = R $ do
    (e',bndrs) <- runR $ runWriterT $ bottomupR collectANF ctx e
    case bndrs of
      [] -> return e
      _  -> changed . Letrec $ bind (rec bndrs) e'

collectANF :: NormRewriteW
collectANF _ e@(App appf arg)
  | (conVarPrim, _) <- collectArgs e
  , isCon conVarPrim || isPrim conVarPrim || isVar conVarPrim
  = do
    untranslatable <- liftNormR $ isUntranslatable arg
    localVar       <- liftNormR $ isLocalVar arg
    case (untranslatable,localVar || isConstant arg,arg) of
      (False,False,_) -> do tcm <- Lens.use tcCache
                            (argId,argVar) <- liftNormR $ mkTmBinderFor tcm "repANF" arg
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
  untranslatable <- liftNormR $ isUntranslatable body
  localVar       <- liftNormR $ isLocalVar body
  if localVar || untranslatable
    then return body
    else do
      tcm <- Lens.use tcCache
      (argId,argVar) <- liftNormR $ mkTmBinderFor tcm "bodyVar" body
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
    localVar     <- liftNormR $ isLocalVar subj
    (bndr,subj') <- if localVar || isConstant subj
      then return ([],subj)
      else do tcm <- Lens.use tcCache
              (argId,argVar) <- liftNormR $ mkTmBinderFor tcm "subjLet" subj
              return ([(argId,embed subj)],argVar)

    (binds,alts') <- fmap (first concat . unzip) $ liftNormR $ mapM (doAlt subj') alts

    tell (bndr ++ binds)
    return (Case subj' ty alts')
  where
    doAlt :: Term -> Bind Pat Term -> RewriteMonad NormalizeMonad ([LetBinding],Bind Pat Term)
    -- See NOTE [unsafeUnbind]
    doAlt subj' = fmap (second (uncurry bind)) . doAlt' subj' . unsafeUnbind

    doAlt' :: Term -> (Pat,Term) -> RewriteMonad NormalizeMonad ([LetBinding],(Pat,Term))
    doAlt' subj' alt@(DataPat dc pxs@(unrebind -> ([],xs)),altExpr) = do
      lv      <- isLocalVar altExpr
      patSels <- Monad.zipWithM (doPatBndr subj' (unembed dc)) xs [0..]
      let usesXs (Var _ n) = any ((== n) . varName) xs
          usesXs _         = False
      if (lv && not (usesXs altExpr)) || isConstant altExpr
        then return (patSels,alt)
        else do tcm <- Lens.use tcCache
                (altId,altVar) <- mkTmBinderFor tcm "altLet" altExpr
                return ((altId,embed altExpr):patSels,(DataPat dc pxs,altVar))
    doAlt' _ alt@(DataPat _ _, _) = return ([],alt)
    doAlt' _ alt@(pat,altExpr) = do
      lv <- isLocalVar altExpr
      if lv || isConstant altExpr
        then return ([],alt)
        else do tcm <- Lens.use tcCache
                (altId,altVar) <- mkTmBinderFor tcm "altLet" altExpr
                return ([(altId,embed altExpr)],(pat,altVar))

    doPatBndr :: Term -> DataCon -> Id -> Int -> RewriteMonad NormalizeMonad LetBinding
    doPatBndr subj' dc pId i
      = do tcm <- Lens.use tcCache
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
  = R $ do
    tcm <- Lens.use tcCache
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
        e' <- runR $ etaExpansionTL (LamBody newIdB:ctx) (App e newIdV)
        changed . Lam $ bind newIdB e'
      else return e

-- | Turn a  normalized recursive function, where the recursive calls only pass
-- along the unchanged original arguments, into let-recursive function. This
-- means that all recursive calls are replaced by the same variable reference as
-- found in the body of the top-level let-expression.
recToLetRec :: NormRewrite
recToLetRec [] e = R $ do
  fn          <- liftR $ Lens.use curFun
  bodyM       <- fmap (HashMap.lookup fn) $ Lens.use bindings
  tcm         <- Lens.use tcCache
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
  = R $ do
    tcm <- Lens.use tcCache
    hasPolyFunArgs <- or <$> mapM (either (isPolyFun tcm) (const (return False))) args
    if hasPolyFunArgs
      then do isInlined <- liftR $ alreadyInlined f
              limit     <- liftR $ Lens.use inlineLimit
              if (Maybe.fromMaybe 0 isInlined) > limit
                then do
                  cf  <- liftR $ Lens.use curFun
                  lvl <- Lens.view dbgLevel
                  traceIf (lvl > DebugNone) ($(curLoc) ++ "InlineHO: " ++ show f ++ " already inlined " ++ show limit ++ " times in:" ++ show cf) (return e)
                else do
                  bodyMaybe <- fmap (HashMap.lookup f) $ Lens.use bindings
                  case bodyMaybe of
                    Just (_, body) -> do
                      liftR $ addNewInline f
                      changed $ mkApps body args
                    _ -> return e
      else return e

inlineHO _ e = return e

-- | Simplified CSE, only works on let-bindings, works from top to bottom
simpleCSE :: NormRewrite
simpleCSE _ e@(Letrec b) = R $ do
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
  = R $ do
    tcm <- Lens.use tcCache
    reduceConstant <- Lens.use evaluator
    case reduceConstant tcm e of
      e'@(Data _)    -> changed e'
      e'@(Literal _) -> changed e'
      _              -> return e

reduceConst _ e = return e
