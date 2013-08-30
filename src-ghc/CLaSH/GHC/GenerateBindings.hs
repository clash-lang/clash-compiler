module CLaSH.GHC.GenerateBindings
  (generateBindings)
where

import qualified Control.Monad.State     as State
import           Data.Either             (lefts, rights)
import qualified Data.HashMap.Lazy       as HashMap
import           Unbound.LocallyNameless (runFreshM, unembed)

import           CLaSH.Core.Term         (Term)
import           CLaSH.Core.Type         (Type, splitFunForallTy)
import           CLaSH.Core.Util         (mkLams, mkTyLams)
import           CLaSH.Core.Var          (Var (..))
import           CLaSH.Driver.Types      (BindingMap)
import           CLaSH.GHC.GHC2Core      (coreToBndr, coreToTerm,
                                          makeAllTyDataCons)
import           CLaSH.GHC.LoadModules   (loadModules)
import           CLaSH.Primitives.Types  (PrimMap)
import           CLaSH.Rewrite.Util      (mkInternalVar, mkSelectorCase)
import           CLaSH.Util              ((><),first)

generateBindings ::
  PrimMap
  -> String
  -> IO BindingMap
generateBindings primMap modName = do
  (bindings,clsOps,unlocatable,tcs) <- loadModules modName
  let tcsMap = makeAllTyDataCons tcs

  let bindingsMap = HashMap.fromList
                  $ map (\(v,e) ->
                          let v' = coreToBndr tcsMap v
                          in ( varName v'
                             , ( unembed $ varType v'
                               , coreToTerm primMap unlocatable tcsMap e
                               )

                             )
                        ) bindings

  let clsOpMap = HashMap.fromList
               $ map (\(v,i) ->
                       let v' = coreToBndr tcsMap v
                           ty = unembed $ varType v'
                       in ( varName v'
                          , ( unembed $ varType v'
                            , mkClassSelector ty i
                            )
                          )
                     ) clsOps

  return (bindingsMap `HashMap.union` clsOpMap)

mkClassSelector :: Type
                -> Int
                -> Term
mkClassSelector ty sel = newExpr
  where
    ((tvs,dictTy:_),_) = first (lefts >< rights)
                       $ first (span (\l -> case l of Left _ -> True
                                                      _      -> False))
                       $ splitFunForallTy ty
    newExpr = runFreshM $ flip State.evalStateT (0 :: Int) $ do
                (dcId,dcVar) <- mkInternalVar "dict" dictTy
                selE         <- mkSelectorCase "mkClassSelector" [] dcVar 1 sel
                return (mkTyLams (mkLams selE [dcId]) tvs)
