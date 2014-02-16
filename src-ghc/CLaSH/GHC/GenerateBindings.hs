module CLaSH.GHC.GenerateBindings
  (generateBindings)
where

import           Control.Monad.State     (State)
import qualified Control.Monad.State     as State
import           Data.Either             (lefts, rights)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Unbound.LocallyNameless (runFreshM, unembed)

import qualified CoreSyn                 as GHC

import           CLaSH.Core.Term         (Term, TmName)
import           CLaSH.Core.Type         (Type, splitFunForallTy)
import           CLaSH.Core.TyCon        (TyCon, TyConName)
import           CLaSH.Core.TysPrim      (tysPrimMap)
import           CLaSH.Core.Util         (mkLams, mkTyLams)
import           CLaSH.Core.Var          (Var (..))
import           CLaSH.Driver.Types      (BindingMap)
import           CLaSH.GHC.GHC2Core      (GHC2CoreState, coreToId, coreToTerm,
                                          makeAllTyCons, emptyGHC2CoreState)
import           CLaSH.GHC.LoadModules   (loadModules)
import           CLaSH.Primitives.Types  (PrimMap)
import           CLaSH.Rewrite.Util      (mkInternalVar, mkSelectorCase)
import           CLaSH.Util              ((***),first)

generateBindings ::
  PrimMap
  -> String
  -> IO (BindingMap,HashMap TyConName TyCon)
generateBindings primMap modName = do
  (bindings,clsOps,unlocatable) <- loadModules modName
  let ((bindingsMap,clsVMap),tcMap) = State.runState (mkBindings primMap bindings clsOps unlocatable) emptyGHC2CoreState
      tcCache                       = makeAllTyCons tcMap
      allTcCache                    = tysPrimMap `HashMap.union` tcCache
      clsMap                        = HashMap.map (\(ty,i) -> (ty,mkClassSelector allTcCache ty i)) clsVMap
      allBindings                   = bindingsMap `HashMap.union` clsMap
  return (allBindings,allTcCache)

mkBindings :: PrimMap
           -> [(GHC.CoreBndr, GHC.CoreExpr)] -- Binders
           -> [(GHC.CoreBndr,Int)]           -- Class operations
           -> [GHC.CoreBndr]                 -- Unlocatable Expressions
           -> State GHC2CoreState
                    ( BindingMap
                    , HashMap TmName (Type,Int)
                    )
mkBindings primMap bindings clsOps unlocatable = do
  bindingsList <- mapM (\(v,e) -> do
                          tm <- coreToTerm primMap unlocatable e
                          v' <- coreToId v
                          return (varName v', (unembed (varType v'), tm))
                       ) bindings
  clsOpList    <- mapM (\(v,i) -> do
                          v' <- coreToId v
                          let ty = unembed $ varType v'
                          return (varName v', (ty,i))
                       ) clsOps

  return (HashMap.fromList bindingsList, HashMap.fromList clsOpList)

mkClassSelector :: HashMap TyConName TyCon
                -> Type
                -> Int
                -> Term
mkClassSelector tcm ty sel = newExpr
  where
    ((tvs,dictTy:_),_) = first (lefts *** rights)
                       $ first (span (\l -> case l of Left _ -> True
                                                      _      -> False))
                       $ splitFunForallTy ty
    newExpr = runFreshM $ flip State.evalStateT (0 :: Int) $ do
                (dcId,dcVar) <- mkInternalVar "dict" dictTy
                selE         <- mkSelectorCase "mkClassSelector" tcm [] dcVar 1 sel
                return (mkTyLams (mkLams selE [dcId]) tvs)
