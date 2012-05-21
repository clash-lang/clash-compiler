module CLaSH.Normalize.Transformations where

import Control.Applicative            ((<$>),(<*>),pure)
import qualified Data.Either       as Either
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Label.PureM  as LabelM
import qualified Data.List         as List
import qualified Data.Map          as Map
import qualified Data.Maybe        as Maybe
import Unbound.LocallyNameless        (Embed(..),bind,embed,rec,runFreshM,unbind,unembed,unrec)

import CLaSH.Core.DataCon    (dcTag)
import CLaSH.Core.FreeVars   (typeFreeVars,termFreeIds)
import CLaSH.Core.Subst      (substTyInTm)
import CLaSH.Core.Term       (Term(..),LetBinding,Pat(..))
import CLaSH.Core.Type       (isPolyTy,splitFunTy,applyFunTy,applyTy)
import CLaSH.Core.Util       (collectArgs,mkLams,mkApps,isFun,isLam,termType,isVar,isCon,isPrimCon,isPrimFun)
import CLaSH.Core.Var        (Var(..))
import CLaSH.Normalize.Types
import CLaSH.Rewrite.Types
import CLaSH.Rewrite.Util
import CLaSH.Util

-- Shared Rewrite Rules
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
caseApp ctx (App (Case scrut ty alts) arg) = R $ do
  (boundArg,argVar) <- mkBinderFor ctx "caseApp" arg
  let alts' = map ( uncurry bind
                  . second (`App` argVar)
                  . runFreshM
                  . unbind
                  ) alts
  argTy <- termType <$> mkGamma ctx <*> pure arg
  let ty' = applyFunTy ty argTy
  changed . Letrec $ bind (rec [(boundArg,embed arg)]) (Case scrut ty' alts')

caseApp _ e = return e

-- Monomorphization Rewrite Rules
iotaReduce :: NormRewrite
iotaReduce _ (TyApp (TyLam b) t) = R $ do
  (tv,e) <- unbind b
  changed $ substTyInTm (varName tv) t e

iotaReduce _ e = return e

letTyApp :: NormRewrite
letTyApp _ (TyApp (Letrec b) t) = R $ do
  (v,e) <- unbind b
  changed . Letrec $ bind v (TyApp e t)

letTyApp _ e = return e

caseTyApp :: NormRewrite
caseTyApp _ (TyApp (Case scrut ty' alts) ty) = R $ do
  let alts' = map ( uncurry bind
                  . second (`TyApp` ty)
                  . runFreshM
                  . unbind
                  ) alts
  let ty'' = applyTy ty' ty
  changed $ Case scrut ty'' alts'

caseTyApp _ e = return e

bindPoly :: NormRewrite
bindPoly = inlineBinders bindPolyTest
  where
    bindPolyTest (Id idName tyE, exprE)
      | isPolyTy (unembed tyE) && null (typeFreeVars (unembed tyE)) = do
          (_,localFVs) <- localFreeVars (unembed exprE)
          return $ (idName `notElem` localFVs)
      | otherwise = return False
    bindPolyTest _ = return False

liftPoly :: NormRewrite
liftPoly = liftBinders liftPolyTest
  where
    liftPolyTest (Id idName tyE, exprE)
      | isPolyTy (unembed tyE) && null (typeFreeVars (unembed tyE)) = do
          (_,localFVs) <- localFreeVars (unembed exprE)
          return $ (idName `elem` localFVs)
      | otherwise = return False
    liftPolyTest _ = return False

typeSpec :: NormRewrite
typeSpec ctx e@(TyApp e1 ty)
  | (Var f, args) <- collectArgs e1
  , null $ typeFreeVars ty
  , (eArgs, []) <- Either.partitionEithers args
  = R $ do
    let argLen = length eArgs
    -- Determine if 'f' has already been specialized on 'ty'
    specM <- liftR $ fmap (Map.lookup (f,argLen,ty)) $
               LabelM.gets typeSpecializations
    case specM of
      -- Use previously specialized function
      Just fname -> changed $ mkApps (Var fname) eArgs
      -- Create new specialized function
      Nothing -> do
        bodyMaybe <- fmap (HashMap.lookup f) $ LabelM.gets bindings
        case bodyMaybe of
          Just (_,bodyTm) -> do
            -- Make new binders for existing arguments
            (boundArgs,argVars) <- fmap unzip $
                                     mapM (mkBinderFor ctx "pTS") eArgs
            -- Create specialized functions
            let newBody = mkLams (TyApp (mkApps bodyTm argVars) ty)
                            boundArgs
            newf <- mkFunction ctx f newBody
            -- Remember specialization
            liftR $ LabelM.modify typeSpecializations
                      (Map.insert (f,argLen,ty) newf)
            -- Use specialized function
            let newExpr = mkApps (Var newf) eArgs
            changed newExpr
          Nothing -> return e

typeSpec _ e = return e

-- Defunctionalization Rewrite rules
caseLet :: NormRewrite
caseLet _ (Case (Letrec b) ty alts) = R $ do
  (xes,e) <- unbind b
  changed . Letrec $ bind xes (Case e ty alts)

caseLet _ e = return e

caseCon :: NormRewrite
caseCon _ (Case scrut ty alts)
  | (Data dc, args) <- collectArgs scrut
  = R $ do
    alts' <- mapM unbind alts
    let dcAltM = List.find (equalCon dc . fst) alts'
    case dcAltM of
      Just (DataPat _ xs, e) -> do
        let fvs = termFreeIds e
        let (binds,_) = List.partition ((`elem` fvs) . varName . fst)
                      $ zip xs (Either.lefts args)
        case binds of
          [] -> changed e
          _  -> changed . Letrec $ bind (rec $ map (second embed) binds) e
      Nothing -> do
        let defAltM = List.find (isDefPat . fst) alts'
        case defAltM of
          Just (DefaultPat, e) -> do
            changed e
          Nothing -> error $ $(curLoc) ++ "Non-exhaustive case-statement"
          Just _ -> error $ $(curLoc) ++ "Report as bug: caseCon error"
      Just _ -> error $ $(curLoc) ++ "Report as bug: caseCon error"
  where
    equalCon dc (DataPat dc' _) = dcTag dc == dcTag dc'
    equalCon _  _               = False

    isDefPat DefaultPat = True
    isDefPat _          = False

caseCon _ e = return e

--caseCase :: NormRewrite
--caseCase _ (Case (Case scrut alts1) alts2)
--  | isBox (hea)

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
etaExpand ctx e
  | not (isLam e)
  = R $ do
    gamma <- mkGamma ctx
    case (isFun gamma e) of
      True -> do
        let argTy = ( fst
                    . Maybe.fromMaybe (error "etaExpand splitFunTy")
                    . splitFunTy
                    . termType gamma
                    ) e
        (newIdB,newIdV) <- mkInternalVar "eta" argTy
        changed . Lam $ bind newIdB (App e newIdV)
      False -> return e

etaExpand _ e = return e

appSimpl :: NormRewrite
appSimpl ctx e@(App appf arg)
  | (f, _) <- collectArgs e
  , isVar f || isCon f || isPrimCon f || isPrimFun f
  = R $ do
    localVar <- isLocalVar arg
    untranslatable <- isUntranslatable arg
    case localVar || untranslatable of
      True -> return e
      False -> do
        (argId,argVar) <- mkBinderFor ctx "appSimpl" arg
        changed . Letrec $ bind (rec [(argId,embed arg)]) (App appf argVar)

appSimpl _ e = return e

letFlat :: NormRewrite
letFlat _ e@(Letrec binds) = R $ do
    (xes, body) <- fmap (first unrec) $ unbind binds
    (binds',updated) <- fmap unzip $ mapM flatBind xes
    case (or updated) of
      True  -> return . Letrec $ bind (rec (concat binds')) body
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
      (resId,resVar) <- mkBinderFor ctx "retLet" body
      changed . Letrec $ bind (rec $ (resId,embed body):xes) resVar
    True -> return expr

retLet _ e = return e
