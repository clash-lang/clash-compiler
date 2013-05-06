module CLaSH.Driver.PrepareBinding where

import qualified Data.HashMap.Lazy       as HashMap
import           Data.HashMap.Lazy       (HashMap)
import           Data.Maybe              (fromMaybe)
import           Unbound.LocallyNameless (unembed)

import qualified Module
import qualified Name
import qualified Var

import           CLaSH.Core.Term         (TmName,Term)
import           CLaSH.Core.Type         (Type)
import           CLaSH.Core.Var          (Var(..))
import           CLaSH.GHC.GHC2Core      (makeAllTyDataCons,coreToBndr,coreToPrimBndr,coreToTerm)
import           CLaSH.GHC.LoadModules   (loadModules)
import           CLaSH.Primitives.Types

type DFunMap    = HashMap TmName (Type,[Term])
type ClassOpMap = HashMap TmName (Type,Int)
type BindingMap = HashMap TmName (String,(Type,Term))

prepareBinding ::
  PrimMap
  -> String
  -> IO (BindingMap,DFunMap,ClassOpMap)
prepareBinding primMap modName = do
  (bindings,dfuns,clsOps,unlocs,tcs) <- loadModules modName
  -- let tcsMap = set unlocatable (unlocs++(map fst clsOps)) (makeAllTyDataCons tcs)
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
              $ map (\(v,es) ->
                      let v' = coreToBndr tcsMap v
                      in ( varName v'
                         , ( unembed $ varType v'
                             , map (coreToTerm primMap unlocatable dfunvars tcsMap) es
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
