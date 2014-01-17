{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

{-# OPTIONS_GHC -fcontext-stack=21 #-}

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
  , inlineClosedTerm
  , nonRepANF
  , bindConstantVar
  , constantSpec
  , makeANF
  , deadCode
  , topLet
  , inlineTLWrapper
  , inlineWrapper
  , recToLetRec
  , inlineClosed
  )
where

import           Control.Error               (headMay)
import           Control.Lens                ((.=),(%=))
import qualified Control.Lens                as Lens
import qualified Control.Monad               as Monad
import           Control.Monad.Writer        (WriterT (..), lift, tell)
import qualified Data.Either                 as Either
import qualified Data.HashMap.Lazy           as HashMap
import qualified Data.List                   as List
import qualified Data.Maybe                  as Maybe
import           Unbound.LocallyNameless     (Bind, Embed (..), bind, embed,
                                              rec, unbind, unembed, unrebind,
                                              unrec)
import           Unbound.LocallyNameless.Ops (unsafeUnbind)

import           CLaSH.Core.DataCon          (DataCon, dcTag, dcUnivTyVars)
import           CLaSH.Core.FreeVars         (termFreeIds, termFreeTyVars,
                                              termFreeVars, typeFreeVars)
import           CLaSH.Core.Pretty           (showDoc)
import           CLaSH.Core.Subst            (substTm, substTms, substTyInTm,
                                              substTysinTm)
import           CLaSH.Core.Term             (LetBinding, Pat (..), Term (..))
import           CLaSH.Core.Type             (splitFunTy)
import           CLaSH.Core.Util             (collectArgs, idToVar, isCon,
                                              isFun, isLet, isPrim, isVar,
                                              mkApps, mkLams, mkTmApps,
                                              termType)
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
      = (&&) <$> (not <$> (representableType <$> Lens.use typeTranslator <*> pure (unembed tyE)))
             <*> ((notElem idName . snd) <$> localFreeVars (unembed exprE))

    nonRepTest _ = return False

-- | Lift non-representable let-bindings
liftNonRep :: NormRewrite
liftNonRep = liftBinders nonRepTest
  where
    nonRepTest (Id _ tyE, _) =
      not <$> (representableType <$> Lens.use typeTranslator <*> pure (unembed tyE))
--     nonRepTest (Id idName tyE, exprE)
--       = (&&) <$> (not <$> (representableType <$> Lens.use typeTranslator <*> pure (unembed tyE)))
--              <*> ((elem idName . snd) <$> localFreeVars (unembed exprE))

    nonRepTest _ = return False

-- | Specialize functions on their type
typeSpec :: NormRewrite
typeSpec ctx e@(TyApp e1 ty)
  | (Var _ _,  args) <- collectArgs e1
  , null $ typeFreeVars ty
  , (_, []) <- Either.partitionEithers args
  = specializeNorm ctx e

typeSpec _ e = return e

-- | Specialize functions on their non-representable argument
nonRepSpec :: NormRewrite
nonRepSpec ctx e@(App e1 e2)
  | (Var _ _, args) <- collectArgs e1
  , (_, [])     <- Either.partitionEithers args
  , null $ termFreeTyVars e2
  = R $ do e2Ty <- termType e2
           localVar <- isLocalVar e2
           nonRepE2 <- not <$> (representableType <$> Lens.use typeTranslator <*> pure e2Ty)
           if nonRepE2 && not localVar
             then runR $ specializeNorm ctx e
             else return e

nonRepSpec _ e = return e

-- | Lift the let-bindings out of the subject of a Case-decomposition
caseLet :: NormRewrite
caseLet _ (Case (Letrec b) alts) = R $ do
  (xes,e) <- unbind b
  changed . Letrec $ bind xes (Case e alts)

caseLet _ e = return e

-- | Move a Case-decomposition from the subject of a Case-decomposition to the alternatives
caseCase :: NormRewrite
caseCase _ e@(Case (Case scrut alts1) alts2)
  = R $ do
    alt1E   <- snd <$> unbind (head alts1)
    alts1Ty <- termType alt1E
    ty1Rep  <- representableType <$> Lens.use typeTranslator <*> pure alts1Ty
    if not ty1Rep
      then do newAlts <- mapM ( return
                                  . uncurry bind
                                  . second (\altE -> Case altE alts2)
                                  <=< unbind
                                  ) alts1
              changed $ Case scrut newAlts
      else return e

caseCase _ e = return e

-- | Inline function with a non-representable result if it's the subject
-- of a Case-decomposition
inlineNonRep :: NormRewrite
inlineNonRep _ e@(Case scrut alts)
  | (Var _ f, args) <- collectArgs scrut
  = R $ do
    isInlined <- liftR $ alreadyInlined f
    case isInlined of
      Just n -> do
        cf <- liftR $ Lens.use curFun
        error $ $(curLoc) ++ "InlineNonRep: " ++ show f ++ " already inlined " ++ show n ++ " times in:" ++ show cf
      Nothing -> do
        scrutTy     <- termType scrut
        bodyMaybe   <- fmap (HashMap.lookup f) $ Lens.use bindings
        nonRepScrut <- not <$> (representableType <$> Lens.use typeTranslator <*> pure scrutTy)
        case (nonRepScrut, bodyMaybe) of
          (True,Just (_, scrutBody)) -> do
            liftR $ addNewInline f
            changed $ Case (mkApps scrutBody args) alts
          _ -> return e

inlineNonRep _ e = return e

-- | Specialize a Case-decomposition (replace by the RHS of an alternative) if
-- the subject is (an application of) a DataCon; or if there is only a single
-- alternative that doesn't reference variables bound by the pattern.
caseCon :: NormRewrite
caseCon _ c@(Case scrut alts)
  | (Data dc, args) <- collectArgs scrut
  = R $ do
    alts' <- mapM unbind alts
    let dcAltM = List.find (equalCon dc . fst) alts'
    case dcAltM of
      Just (DataPat _ pxs, e) ->
        let (tvs,xs) = unrebind pxs
            fvs = termFreeIds e
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

caseCon _ c@(Case (Literal l) alts) = R $ do
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

caseCon _ e@(Case _ [alt]) = R $ do
  (pat,altE) <- unbind alt
  case pat of
    DefaultPat    -> changed altE
    LitPat _      -> changed altE
    DataPat _ pxs -> let (tvs,xs)   = unrebind pxs
                         (ftvs,fvs) = termFreeVars altE
                         usedTvs    = filter ((`elem` ftvs) . varName) tvs
                         usedXs     = filter ((`elem` fvs) . varName) xs
                     in  case (usedTvs,usedXs) of
                           ([],[]) -> changed altE
                           _       -> return e

caseCon _ e = return e

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
      (True,Case {})  -> runR $ specializeNorm ctx e
      (True,Lam _)    -> runR $ specializeNorm ctx e
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
    else do (argId,argVar) <- mkTmBinderFor "topLet" e
            changed . Letrec $ bind (rec [(argId,embed e)]) argVar

topLet ctx e@(Letrec b)
  | all isLambdaBodyCtx ctx
  = R $ do
    (binds,body)   <- unbind b
    localVar       <- isLocalVar body
    untranslatable <- isUntranslatable body
    if localVar || untranslatable
      then return e
      else do (argId,argVar) <- mkTmBinderFor "topLet" body
              changed . Letrec $ bind (rec $ unrec binds ++ [(argId,embed body)]) argVar

topLet _ e = return e

-- Misc rewrites

-- | Remove unused let-bindings
deadCode :: NormRewrite
deadCode _ e@(Letrec binds) = R $ do
    (xes, body) <- fmap (first unrec) $ unbind binds
    let bodyFVs = termFreeIds body
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
      let fvsUsed = concatMap (termFreeIds . unembed . snd) explore
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

inlineClosed :: NormRewrite
inlineClosed _ e@(Var _ f) = R $ do
  bodyMaybe <- fmap (HashMap.lookup f) $ Lens.use bindings
  case bodyMaybe of
    Just (_,body) -> do
      closed <- isClosed body
      untranslatable <- isUntranslatable e
      if closed && not untranslatable
        then changed body
        else return e
    _ -> return e

inlineClosed _ e = return e

-- | Inline nullary/closed functions
inlineClosedTerm :: String -> NormRewrite -> NormRewrite
inlineClosedTerm rwS rw _ e@(Var _ f) = R $ do
  bodyMaybe <- fmap (HashMap.lookup f) $ Lens.use bindings
  normMaybe <- fmap (HashMap.lookup f) $ liftR $ Lens.use normalized
  case bodyMaybe of
    Just (_,body) -> do
      closed <- isClosed body
      untranslatable <- isUntranslatable body
      if closed && not untranslatable
        then case normMaybe of
               Just norm -> changed norm
               Nothing   -> do cf <- liftR $ Lens.use curFun
                               liftR $ curFun .= f
                               newNorm <- lift $ runRewrite rwS rw body
                               liftR $ curFun .= cf
                               liftR $ normalized %= HashMap.insert f newNorm
                               changed newNorm
        else return e
    _ -> return e

inlineClosedTerm _ _ _ e = return e

-- | Specialise functions on arguments which are constant
constantSpec :: NormRewrite
constantSpec ctx e@(App e1 e2)
  | (Var _ _, args) <- collectArgs e1
  , (_, [])     <- Either.partitionEithers args
  , null $ termFreeTyVars e2
  , isConstant e2
  = specializeNorm ctx e

constantSpec _ e = return e

-- | Inline functions which simply \"wrap\" another function
inlineTLWrapper :: NormRewrite
inlineTLWrapper [] e = R $ do
  normalizedM <- splitNormalized e
  case normalizedM of
    Right (_,[(_,bExpr)],_) -> case collectArgs (unembed bExpr) of
      (Var _ fn,args) -> do
        allLocal <- fmap and $ mapM (either isLocalVar (\_ -> return True)) args
        bodyMaybe <- fmap (HashMap.lookup fn) $ Lens.use bindings
        case (bodyMaybe,allLocal) of
          (Just (bodyTy,body),True) -> do
            eTy <- termType e
            if eTy == bodyTy
              then changed body
              else return e
          _ -> return e
      _ -> return e
    _ -> return e

inlineTLWrapper _ e = return e

-- | Inline functions which simply \"wrap\" another function
inlineWrapper :: NormRewrite
inlineWrapper ctx e@(Var _ f)
  | maybe True (/= AppArg) (headMay ctx)
  = R $ do
    bodyMaybe <- fmap (HashMap.lookup f) $ Lens.use bindings
    case bodyMaybe of
      Just (_,body) -> do
        normalizedBodyM <- splitNormalized body
        case normalizedBodyM of
          Right (_,[_],_) -> changed body
          _ -> return e
      _ -> return e

inlineWrapper _ e = return e

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

appProp _ (App (Case scrut alts) arg) = R $ do
  if isConstant arg || isVar arg
    then do
      alts' <- mapM ( return
                    . uncurry bind
                    . second (`App` arg)
                    <=< unbind
                    ) alts
      changed $ Case scrut alts'
    else do
      (boundArg,argVar) <- mkTmBinderFor "caseApp" arg
      alts' <- mapM ( return
                    . uncurry bind
                    . second (`App` argVar)
                    <=< unbind
                    ) alts
      changed . Letrec $ bind (rec [(boundArg,embed arg)]) (Case scrut alts')

appProp _ (TyApp (TyLam b) t) = R $ do
  (tv,e) <- unbind b
  changed $ substTyInTm (varName tv) t e

appProp _ (TyApp (Letrec b) t) = R $ do
  (v,e) <- unbind b
  changed . Letrec $ bind v (TyApp e t)

appProp _ (TyApp (Case scrut alts) ty) = R $ do
  alts' <- mapM ( return
                . uncurry bind
                . second (`TyApp` ty)
                <=< unbind
                ) alts
  changed $ Case scrut alts'

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
      (False,False,_) -> do (argId,argVar) <- liftNormR $ mkTmBinderFor "repANF" arg
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
      (argId,argVar) <- liftNormR $ mkTmBinderFor "bodyVar" body
      tell [(argId,embed body)]
      return argVar

collectANF ctx e@(Case subj alts) = do
    untranslatableSubj <- liftNormR $ isUntranslatable subj
    localVar           <- liftNormR $ isLocalVar subj
    (bndr,subj') <- if localVar || untranslatableSubj || isConstant subj
      then return ([],subj)
      else do (argId,argVar) <- liftNormR $ mkTmBinderFor "subjLet" subj
              return ([(argId,embed subj)],argVar)

    untranslatableE <- liftNormR $ isUntranslatable e
    (binds,alts') <- if untranslatableE
      then return ([],alts)
      else fmap (first concat . unzip) $ liftNormR $ mapM (doAlt subj') alts

    tell (bndr ++ binds)
    return (Case subj' alts')
  where
    doAlt :: Term -> Bind Pat Term -> RewriteMonad NormalizeMonad ([LetBinding],Bind Pat Term)
    -- See NOTE [unsafeUnbind]
    doAlt subj' = fmap (second (uncurry bind)) . doAlt' subj' . unsafeUnbind

    doAlt' :: Term -> (Pat,Term) -> RewriteMonad NormalizeMonad ([LetBinding],(Pat,Term))
    doAlt' subj' alt@(DataPat dc pxs@(unrebind -> ([],xs)),altExpr) = do
      lv      <- isLocalVar altExpr
      patSels <- Monad.zipWithM (doPatBndr subj' (unembed dc)) xs [0..]
      if lv || isConstant altExpr
        then return (patSels,alt)
        else do (altId,altVar) <- mkTmBinderFor "altLet" altExpr
                return ((altId,embed altExpr):patSels,(DataPat dc pxs,altVar))
    doAlt' _ alt@(DataPat _ _, _) = return ([],alt)
    doAlt' _ alt@(pat,altExpr) = do
      lv <- isLocalVar altExpr
      if lv || isConstant altExpr
        then return ([],alt)
        else do (altId,altVar) <- mkTmBinderFor "altLet" altExpr
                return ([(altId,embed altExpr)],(pat,altVar))

    doPatBndr :: Term -> DataCon -> Id -> Int -> RewriteMonad NormalizeMonad LetBinding
    doPatBndr subj' dc pId i
      = do patExpr <- mkSelectorCase ($(curLoc) ++ "doPatBndr") ctx subj' (dcTag dc) i
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
    isF <- isFun e
    if isF
      then do
        argTy <- ( return
                 . fst
                 . Maybe.fromMaybe (error "etaExpansion splitFunTy")
                 . splitFunTy
                 <=< termType
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
  normalizedE <- splitNormalized e
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
