module CLaSH.Normalize.Transformations where

import qualified Data.Either       as Either
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Label.PureM  as LabelM
import qualified Data.Map          as Map
import Unbound.LocallyNameless        (bind,embed,rec,runFreshM,unbind,unembed)

import CLaSH.Core.FreeVars   (typeFreeVars)
import CLaSH.Core.Subst      (substTyInTm)
import CLaSH.Core.Term       (Term(..))
import CLaSH.Core.Type       (isPoly)
import CLaSH.Core.Util       (collectArgs,mkLams,mkApps)
import CLaSH.Core.Var        (Var(..))
import CLaSH.Normalize.Types
import CLaSH.Rewrite.Types
import CLaSH.Rewrite.Util
import CLaSH.Util

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
caseApp ctx (App (Case scrut alts) arg) = R $ do
  (argName,boundArg) <- mkBinderFor ctx "caseApp" arg
  let alts' = map ( uncurry bind
                  . second (`App` (Var argName))
                  . runFreshM
                  . unbind
                  ) alts
  changed . Letrec $ bind (rec [(boundArg,embed arg)]) (Case scrut alts')

caseApp _ e = return e

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
caseTyApp _ (TyApp (Case scrut alts) ty) = R $ do
  let alts' = map ( uncurry bind
                  . second (`TyApp` ty)
                  . runFreshM
                  . unbind
                  ) alts
  changed $ Case scrut alts'

caseTyApp _ e = return e

bindPoly :: NormRewrite
bindPoly ctx = inlineBinders bindPolyTest ctx
  where
    bindPolyTest (Id idName tyE, exprE)
      | isPoly (unembed tyE) && null (typeFreeVars (unembed tyE)) = do
          (_,localFVs) <- localFreeVars (unembed exprE)
          return $ (idName `notElem` localFVs)
      | otherwise = return False
    bindPolyTest _ = return False

liftPoly :: NormRewrite
liftPoly ctx = liftBinders liftPolyTest ctx
  where
    liftPolyTest (Id idName tyE, exprE)
      | isPoly (unembed tyE) && null (typeFreeVars (unembed tyE)) = do
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
    specM <- liftR $ fmap (Map.lookup (f,ty)) $
             LabelM.gets typeSpecializations
    case specM of
      Just fname -> changed $ mkApps (Var fname) eArgs
      Nothing -> do
        bodyMaybe <- fmap (HashMap.lookup f) $ LabelM.gets bindings
        case bodyMaybe of
          Just (_,bodyTm) -> do
            (freeArgs,boundArgs) <- fmap (first (map Var) . unzip) $
                                    mapM (mkBinderFor ctx "pTS") eArgs
            let newBody = mkLams (TyApp (mkApps bodyTm freeArgs) ty) boundArgs
            newf        <- mkFunction ctx f newBody
            liftR $ LabelM.modify typeSpecializations (Map.insert (f,ty) newf)
            let newExpr = mkApps (Var newf) eArgs
            changed newExpr
          Nothing -> return e

typeSpec _ e = return e
