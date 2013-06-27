module CLaSH.Driver.PrepareBinding where

import qualified Data.HashMap.Lazy       as HashMap
import           Data.Maybe              (fromMaybe)
import           Unbound.LocallyNameless (unembed)

import qualified Module
import qualified Name
import qualified Var

import           CLaSH.Core.Var          (Var(..))
import           CLaSH.Driver.Types
import           CLaSH.GHC.GHC2Core      (makeAllTyDataCons,coreToBndr,coreToPrimBndr,coreToTerm,coreToVar)
import           CLaSH.GHC.LoadModules   (loadModules)
import           CLaSH.Primitives.Types

prepareBinding ::
  PrimMap
  -> String
  -> IO (BindingMap,DFunMap,ClassOpMap)
prepareBinding primMap modName = do
  (bindings,dfuns,clsOps,unlocs,tcs) <- loadModules modName
  let unlocatable = unlocs ++ (map fst clsOps)
  let dfunvars = map fst dfuns
  let tcsMap = makeAllTyDataCons tcs

  let bindingsMap = HashMap.fromList
                  $ map (\(v,e) ->
                          let v' = coreToBndr tcsMap v
                          in ( varName v'
                             , ( moduleName $ Var.varName v
                               , ( unembed $ varType v'
                                 , coreToTerm primMap unlocatable dfunvars tcsMap e
                                 )
                               )
                             )
                        ) bindings

  let dfunMap = HashMap.fromList
              $ map (\(v,((tyVs,tmVs),es)) ->
                      let v' = coreToBndr tcsMap v
                      in ( varName v'
                         , ( unembed $ varType v'
                             , ( ( map coreToVar tyVs
                                 , map coreToVar tmVs
                                 )
                               , map (coreToTerm primMap unlocatable dfunvars tcsMap) es
                               )
                             )
                         )
                    ) dfuns

  let clsOpMap = HashMap.fromList
               $ map (\(v,i) ->
                       let v' = coreToPrimBndr tcsMap v
                       in ( varName v'
                          , ( unembed $ varType v',i)
                          )
                     ) clsOps

  return (bindingsMap,dfunMap,clsOpMap)

moduleName ::
  Name.Name
  -> String
moduleName n = fromMaybe "_INTERNAL_" modName
  where
    modName = do
      module_ <- Name.nameModule_maybe n
      let moduleNm = Module.moduleName module_
      return (Module.moduleNameString moduleNm)
