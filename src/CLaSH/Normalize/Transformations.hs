{-# LANGUAGE PatternGuards #-}
module CLaSH.Normalize.Transformations where

import qualified Data.Either       as Either
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Label.PureM  as LabelM
import qualified Data.List         as List
import qualified Data.Maybe        as Maybe
import Unbound.LocallyNameless        (Embed(..),bind,embed,rec,unbind,unembed,unrebind,unrec)

import CLaSH.Core.DataCon    (dcTag,dcUnivTyVars)
import CLaSH.Core.FreeVars   (typeFreeVars,termFreeIds,termFreeTyVars)
import CLaSH.Core.Pretty     (showDoc)
import CLaSH.Core.Prim       (Prim(..))
import CLaSH.Core.Subst      (substTyInTm,substTysinTm)
import CLaSH.Core.Term       (Term(..),LetBinding,Pat(..))
import CLaSH.Core.Type       (isPolyTy,splitFunTy,applyFunTy,isFunTy,applyTy)
import CLaSH.Core.Util       (collectArgs,mkApps,isFun,isLam,termType,isVar,isCon,isPrimCon,isPrimFun,isLet)
import CLaSH.Core.Var        (Var(..))
import CLaSH.Normalize.Types
import CLaSH.Normalize.Util
import CLaSH.Rewrite.Types
import CLaSH.Rewrite.Util
import CLaSH.Util

----------------------------------------------------------
-- = Definitions =
--
-- FUN (a -> b) = True
-- FUN _        = False
--
-- BOX (T ts) = any (\t -> BOX t || FUN t) (CONARGS T ts)
-- BOX _      = False
--
-- POLY (∀a.t)   = True
-- POLY (a -> b) = POLY b
-- POLY _        = False
--
-- POLYFUN (∀a.t)   = POLYFUN t
-- POLYFUN (a -> b) = True
-- POLYFUN _        = False
--
-- = Inline, Lift, or Specialize =
--
-- Function Application:
-- Has FUN arguments?   => Inline
-- Has BOXed arguments? => Specialize
-- Has Type arguments?  => Specialize
-- Others               => Do Nothing
--
-- Let-binding:
-- Has POLYFUNction type?              => Lift
-- Takes Type arguments, non-recursive => Inline
-- Takes Type arguments, recursive     => Lift
-- Has BOX type, non-recursive?        => Inline
-- Has BOX type, recursive?            => Lift
-- Others                              => Do Nothing
--
-----------------------------------------------------------
tauReduction :: NormRewrite
tauReduction _ (TyApp (TyLam b) t) = R $ do
  (tv,e) <- unbind b
  changed $ substTyInTm (varName tv) t e

tauReduction _ e = return e

letTyApp :: NormRewrite
letTyApp _ (TyApp (Letrec b) t) = R $ do
  (v,e) <- unbind b
  changed . Letrec $ bind v (TyApp e t)

letTyApp _ e = return e

caseTyApp :: NormRewrite
caseTyApp _ (TyApp (Case scrut ty' alts) ty) = R $ do
  alts' <- mapM ( return
                . uncurry bind
                . second (`TyApp` ty)
                <=< unbind
                ) alts
  ty'' <- applyTy ty' ty
  changed $ Case scrut ty'' alts'

caseTyApp _ e = return e

lamApp :: NormRewrite
lamApp _ (App (Lam b) arg) = R $ do
  (v,e) <- unbind b
  changed . Letrec $ bind (rec [(v,embed arg)]) e

lamApp _ e = return e

letApp :: NormRewrite
letApp _ (App (Letrec b) arg) = R $ do
  (v,e) <- unbind b
  changed . Letrec $ bind v (App e arg)

letApp _ e = return e

caseApp :: NormRewrite
caseApp _ (App (Case scrut ty alts) arg) = R $ do
  (boundArg,argVar) <- mkTmBinderFor "caseApp" arg
  alts' <- mapM ( return
                . uncurry bind
                . second (`App` argVar)
                <=< unbind
                ) alts
  argTy <- termType arg
  let ty' = applyFunTy ty argTy
  changed . Letrec $ bind (rec [(boundArg,embed arg)]) (Case scrut ty' alts')

caseApp _ e = return e

bindNonRep :: NormRewrite
bindNonRep = inlineBinders nonRepTest
  where
    nonRepTest (Id idName tyE, exprE)
      | nonRep (unembed tyE)
      = fmap (notElem idName . snd) $ localFreeVars (unembed exprE)

    nonRepTest _ = return False

liftNonRep :: NormRewrite
liftNonRep = liftBinders nonRepTest
  where
    nonRepTest (Id idName tyE, exprE)
      | nonRep (unembed tyE)
      = fmap (elem idName . snd) $ localFreeVars (unembed exprE)

    nonRepTest _ = return False


typeSpec :: NormRewrite
typeSpec ctx e@(TyApp e1 ty)
  | (Var _ _,  args) <- collectArgs e1
  , null $ typeFreeVars ty
  , (_, []) <- Either.partitionEithers args
  = specialise specialisations ctx e

typeSpec _ e = return e

nonRepSpec :: NormRewrite
nonRepSpec ctx e@(App e1 e2)
  | (Var _ _, args) <- collectArgs e1
  , (_, [])     <- Either.partitionEithers args
  , null $ termFreeTyVars e2
  = do e2Ty <- termType e2
       if nonRep e2Ty
         then specialise specialisations ctx e
         else return e

nonRepSpec _ e = return e

caseLet :: NormRewrite
caseLet _ (Case (Letrec b) ty alts) = R $ do
  (xes,e) <- unbind b
  changed . Letrec $ bind xes (Case e ty alts)

caseLet _ e = return e

caseCase :: NormRewrite
caseCase _ (Case (Case scrut ty1 alts1) ty2 alts2)
  | nonRep ty1
  = R $ do
    newAlts  <- mapM ( return
                     . uncurry bind
                     . second (\altE -> Case altE ty2 alts2)
                     <=< unbind
                     ) alts1
    changed $ Case scrut ty2 newAlts

caseCase _ e = return e

inlineNonRep :: NormRewrite
inlineNonRep ctx e@(Case scrut ty alts)
  | (Var _ f, args) <- collectArgs scrut
  = R $ do
    isInlined <- liftR $ alreadyInlined f
    case isInlined of
      True -> do
        cf <- liftR $ LabelM.gets curFun
        traceIf True ($(curLoc) ++ "InlineBox: " ++ show f ++ " already inlined in: " ++ show cf) $ return e
      False -> do
        scrutTy   <- termType scrut
        bodyMaybe <- fmap (HashMap.lookup f) $ LabelM.gets bindings
        case (nonRep scrutTy, bodyMaybe) of
          (True,Just (_, scrutBody)) -> do
            liftR $ LabelM.modify newInlined (f:)
            changed $ Case (mkApps scrutBody args) ty alts
          _ -> return e

inlineNonRep _ e = return e

caseCon :: NormRewrite
caseCon _ (Case scrut ty alts)
  | (Data dc, args) <- collectArgs scrut
  = R $ do
    alts' <- mapM unbind alts
    let dcAltM = List.find (equalCon dc . fst) alts'
    case dcAltM of
      Just (DataPat _ pxs, e) -> do
        let (tvs,xs) = unrebind pxs
        let fvs = termFreeIds e
        let (binds,_) = List.partition ((`elem` fvs) . varName . fst)
                      $ zip xs (Either.lefts args)
        let e' = case binds of
                  [] -> e
                  _  -> Letrec $ bind (rec $ map (second embed) binds) e
        let substTyMap = zip (map varName tvs) (drop (length $ dcUnivTyVars dc) (Either.rights args))

        traceIf True ("substTyMap:" ++ show substTyMap) $ changed (substTysinTm substTyMap e')
      Nothing -> do
        let defAltM = List.find (isDefPat . fst) alts'
        case defAltM of
          Just (DefaultPat, e) -> do
            changed e
          Nothing -> error $ $(curLoc) ++ "Non-exhaustive case-statement"
          Just _ -> error $ $(curLoc) ++ "Report as bug: caseCon error"
      Just _ -> error $ $(curLoc) ++ "Report as bug: caseCon error"
  where
    equalCon dc (DataPat dc' _) = dcTag dc == dcTag (unembed dc')
    equalCon _  _               = False

    isDefPat DefaultPat = True
    isDefPat _          = False

caseCon _ e = return e

-- = Inline, Lift, or Specialize =
--
-- Let-binding:
-- Has POLYFUNction type?              => Lift
-- Takes Type arguments, non-recursive => Inline
-- Takes Type arguments, recursive     => Lift
-- Others                              => Do Nothing
--
bindPoly :: NormRewrite
bindPoly = inlineBinders bindPolyTest
  where
    bindPolyTest (Id idName tyE, exprE)
      | isPolyTy (unembed tyE) && null (typeFreeVars (unembed tyE)) = do
          (_,localFVs) <- localFreeVars (unembed exprE)
          polyFunTy <- isPolyFunTy (unembed tyE)
          return $ (not polyFunTy) && (idName `notElem` localFVs)
    bindPolyTest _ = return False

liftPoly :: NormRewrite
liftPoly = liftBinders liftPolyTest
  where
    liftPolyTest (Id idName tyE, exprE)
      | isPolyTy (unembed tyE) && null (typeFreeVars (unembed tyE)) = do
          (_,localFVs) <- localFreeVars (unembed exprE)
          polyFunTy <- isPolyFunTy (unembed tyE)
          return $ polyFunTy || (idName `elem` localFVs)
    liftPolyTest _ = return False

-- = Inline, Lift, or Specialize =
--
-- Function Application:
-- Has Type arguments?  => Specialize
-- Others               => Do Nothing
--


-- Defunctionalization Rewrite rules
-- inlineBox :: NormRewrite
-- inlineBox ctx e@(Case scrut ty alts)
--   | (Var _ f, args) <- collectArgs scrut
--   = R $ do
--     isInlined <- liftR $ alreadyInlined f
--     case isInlined of
--       True -> do
--         cf <- liftR $ LabelM.gets curFun
--         traceIf True ($(curLoc) ++ "InlineBox: " ++ show f ++ " already inlined in: " ++ show cf) $ return e
--       False -> do
--         scrutTy <- termType scrut
--         bodyMaybe <- fmap (HashMap.lookup f) $ LabelM.gets bindings
--         case (isBoxTy scrutTy, bodyMaybe) of
--           (True,Just (_, scrutBody)) -> do
--             scrutBody' <- lift $ runRewrite "monomorphization" monomorphization scrutBody
--             liftR $ LabelM.modify newInlined (f:)
--             changed $ Case (mkApps scrutBody' args) ty alts
--           _ -> return e
--
-- inlineBox _ e = return e

-- = Inline, Lift, or Specialize =
--
-- Let-binding:
-- Has POLYFUNction type?              => Lift
-- Has BOX type, non-recursive?        => Inline
-- Has BOX type, recursive?            => Lift
-- Others                              => Do Nothing
--
liftFun :: NormRewrite
liftFun = liftBinders liftFunTest
  where
    liftFunTest (Id idName tyE, exprE)
      | null (typeFreeVars (unembed tyE)) = do
          let ty = unembed tyE
          (_,localFVs) <-  localFreeVars (unembed exprE)
          return $ isFunTy ty || isBoxTy ty && idName `elem` localFVs
    liftFunTest _ = return False

bindBox :: NormRewrite
bindBox = inlineBinders bindBoxTest
  where
    bindBoxTest (Id idName tyE, exprE)
      | null (typeFreeVars (unembed tyE)) = do
          let ty = unembed tyE
          (_,localFVs) <-  localFreeVars (unembed exprE)
          return $ isBoxTy ty && idName `notElem` localFVs
    bindBoxTest _ = return False

-- = Inline, Lift, or Specialize =
--
-- Function Application:
-- Has FUN arguments?   => Inline
-- Has BOXed arguments? => Specialize
-- Others               => Do Nothing
--
-- inlineHO :: NormRewrite
-- inlineHO ctx e@(App e1 e2)
--   | (Var _ f, args) <- collectArgs e1
--   = R $ do
--     gamma <- mkGamma ctx
--     e2Ty <- termType e2
--     case isFunTy e2Ty of
--       True -> do
--         isInlined <- liftR $ alreadyInlined f
--         case isInlined of
--           True -> do
--             cf <- liftR $ LabelM.gets curFun
--             traceIf True ($(curLoc) ++ "InlineBox: " ++ show f ++ " already inlined in: " ++ show cf) $ return e
--           False -> do
--             bodyMaybe <- fmap (HashMap.lookup f) $ LabelM.gets bindings
--             case bodyMaybe of
--               Just (_,bodyTm) -> do
--                 bodyTm' <- lift $ runRewrite "monomorphization" monomorphization bodyTm
--                 let newE = mkApps bodyTm' args
--                 liftR $ LabelM.modify newInlined (f:)
--                 changed (App newE e2)
--               Nothing -> return e
--       False -> return e
--
-- inlineHO _ e = return e




-- Simplification Rewrite Rules
deadCode :: NormRewrite
deadCode _ e@(Letrec binds) = R $ do
    (xes, body) <- fmap (first unrec) $ unbind binds
    let bodyFVs = termFreeIds body
    let (xesUsed,xesOther) = List.partition
                               ( (`elem` bodyFVs )
                               . varName
                               . fst
                               ) xes
    let xesUsed' = findUsedBndrs [] xesUsed xesOther
    case (length xesUsed' /= length xes) of
      True  -> changed . Letrec $ bind (rec xesUsed') body
      False -> return e
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

etaExpand :: NormRewrite
etaExpand (AppFirst:_)  e = return e
etaExpand (AppSecond:_) e = return e
etaExpand _ e
  | not (isLam e)
  = R $ do
    isF <- isFun e
    case isF of
      True -> do
        argTy <- ( return
                 . fst
                 . Maybe.fromMaybe (error "etaExpand splitFunTy")
                 . splitFunTy
                 <=< termType
                 ) e
        (newIdB,newIdV) <- mkInternalVar "eta" argTy
        changed . Lam $ bind newIdB (App e newIdV)
      False -> return e

etaExpand _ e = return e

appSimpl :: NormRewrite
appSimpl _ e@(App appf arg)
  | (f, _) <- collectArgs e
  , isVar f || isCon f || isPrimCon f || isPrimFun f
  = R $ do
    localVar       <- isLocalVar arg
    untranslatable <- isUntranslatable arg
    let simple     = isSimple arg
    case localVar || untranslatable || simple of
      True  -> return e
      False -> do
        (argId,argVar) <- mkTmBinderFor "appSimpl" arg
        changed . Letrec $ bind (rec [(argId,embed arg)]) (App appf argVar)

appSimpl _ e = return e

letFlat :: NormRewrite
letFlat _ e@(Letrec binds) = R $ do
    (xes, body) <- fmap (first unrec) $ unbind binds
    (binds',updated) <- fmap unzip $ mapM flatBind xes
    case (or updated) of
      True  -> changed . Letrec $ bind (rec (concat binds')) body
      False -> return e
  where
    flatBind :: Monad m => LetBinding -> RewriteMonad m ([LetBinding],Bool)
    flatBind (bndr, Embed (Letrec binds')) = do
      (r,body) <- unbind binds'
      let r' = unrec r
      return ((bndr, Embed body):r', True)
    flatBind b = return ([b],False)

letFlat _ e = return e

inlineVar :: NormRewrite
inlineVar = inlineBinders (isLocalVar . unembed . snd)

retLet :: NormRewrite
retLet ctx expr@(Letrec b) | all isLambdaBodyCtx ctx = R $ do
  (xes,body) <- fmap (first unrec) $ unbind b
  lv <- isLocalVar body
  unTran <- isUntranslatable body
  case lv || unTran of
    False -> do
      (resId,resVar) <- mkTmBinderFor "retLet" body
      changed . Letrec $ bind (rec $ (resId,embed body):xes) resVar
    True -> return expr

retLet _ e = return e

retLam :: NormRewrite
retLam ctx e
  | all isLambdaBodyCtx ctx && not (isLam e) && not (isLet e)
  = R $ do
    lv     <- isLocalVar e
    unTran <- isUntranslatable e
    case lv || unTran of
      False -> do
        (resId,resVar) <- mkTmBinderFor "retLam" e
        changed . Letrec $ bind (rec $ [(resId,embed e)]) resVar
      True -> return e

retLam _ e = return e

retVar :: NormRewrite
retVar ctx e@(Var t v)
  | all isLambdaBodyCtx ctx
  = R $ case (HashMap.lookup v . fst $ contextEnv ctx) of
    Just ty -> do
      (boundArg,argVar) <- mkInternalVar "retVar" ty
      changed . Letrec $ bind (rec [(boundArg, Embed $ Var t v)]) argVar
    Nothing -> return e

retVar _ e = return e

bindSimple :: NormRewrite
bindSimple = inlineBinders bindSimpleTest
  where
    bindSimpleTest (Id idName tyE, exprE)
      | null (typeFreeVars (unembed tyE)) = do
          (_,localFVs) <- localFreeVars (unembed exprE)
          return $ isSimple (unembed exprE) && idName `notElem` localFVs
    bindSimpleTest _ = return False

inlineSimple :: NormRewrite
inlineSimple _ e@(Var _ f) = R $ do
  bodyMaybe <- fmap (HashMap.lookup f) $ LabelM.gets bindings
  case bodyMaybe of
    Just (_,body) | isSimple body -> changed body
    _ -> return e

inlineSimple _ e = return e

inlineWrapper :: NormRewrite
inlineWrapper ctx e
  | (Var _ f, args) <- collectArgs e
  , all (either isVar (const True)) args
  , all isLambdaBodyCtx ctx
  = R $ do
  bodyMaybe <- fmap (HashMap.lookup f) $ LabelM.gets bindings
  case bodyMaybe of
    Just (_,body) -> changed (mkApps body args)
    Nothing -> return e

inlineWrapper _ e = return e

simpleSpec :: NormRewrite
simpleSpec ctx e@(App e1 e2)
  | (Var _ _, args) <- collectArgs e1
  , (_, []) <- Either.partitionEithers args
  , isSimple e2
  = specialise specialisations ctx e

simpleSpec _ e = return e

-- Class Operator Resolution
classOpResolution :: NormRewrite
classOpResolution ctx e@(App (TyApp (Prim (PrimFun sel _)) _) dfunE)
  | (Var _ dfun, dfunArgs) <- collectArgs dfunE = R $ do
    classSelM <- fmap (fmap snd . HashMap.lookup sel)  $ LabelM.gets classOps
    dfunOpsM  <- fmap (fmap snd . HashMap.lookup dfun) $ LabelM.gets dictFuns
    bindingsM <- fmap (fmap snd . HashMap.lookup dfun) $ LabelM.gets bindings
    case (classSelM,dfunOpsM,bindingsM) of
      (Just classSel,Just dfunOps,Nothing)
        | classSel < length dfunOps -> do
          let clsExpr = dfunOps !! classSel
          changed (mkApps clsExpr dfunArgs)
      (Just classSel,Nothing, Just binding) -> do
        clsExpr <- chaseDfun classSel ctx binding
        changed (mkApps clsExpr dfunArgs)
      (Just classSel,Nothing,Nothing) -> do
        traceIf True ($(curLoc) ++ "No binding or dfun for classOP?: " ++ showDoc e) $ return e
      _ -> return e

classOpResolution _ e = return e

chaseDfun ::
  (Monad m, Functor m)
  => Int
  -> [CoreContext]
  -> Term
  -> RewriteMonad m Term
chaseDfun classSel ctx e
  | (Var _ f, args) <- collectArgs e
  = do
    dfunOpsM  <- fmap (fmap snd . HashMap.lookup f) $ LabelM.gets dictFuns
    case dfunOpsM of
      Just dfunOps | classSel < length dfunOps -> do
        let dfunOp = dfunOps !! classSel
        return $ mkApps dfunOp args
      _ -> error $ $(curLoc) ++ "No binding or dfun for classOP?: " ++ show e
  | otherwise
  = do
    selCase <- mkSelectorCase ctx e 0 classSel
    return selCase
