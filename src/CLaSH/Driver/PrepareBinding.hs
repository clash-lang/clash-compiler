module CLaSH.Driver.PrepareBinding where

import qualified Data.HashMap.Lazy       as HashMap
import           Data.HashMap.Lazy       (HashMap)
import qualified Data.Label              as Label
import           Unbound.LocallyNameless (unembed)

import           CLaSH.Core.Term         (TmName,Term)
import           CLaSH.Core.Type         (Type)
import           CLaSH.Core.Var          (Var(..))
import           CLaSH.GHC.GHC2Core      (makeAllTyDataCons,coreToBndr,coreToTerm,unlocatable)
import           CLaSH.GHC.LoadModules   (loadModules)

type DFunMap    = HashMap TmName (Type,[Term])
type ClassOpMap = HashMap TmName (Type,Int)
type BindingMap = HashMap TmName (Type,Term)

prepareBinding ::
  String
  -> IO (BindingMap,DFunMap,ClassOpMap)
prepareBinding modName = do
  (bindings,dfuns,clsOps,unlocs,tcs) <- loadModules modName
  let tcsMap = Label.set unlocatable unlocs (makeAllTyDataCons tcs)

  let bindingsMap = HashMap.fromList
                  $ map (\(v,e) ->
                          let v' = coreToBndr tcsMap v
                          in ( varName v'
                             , ( unembed $ varType v'
                               , coreToTerm tcsMap e
                               )
                             )
                        ) bindings

  let dfunMap = HashMap.fromList
              $ map (\(v,es) ->
                      let v' = coreToBndr tcsMap v
                      in ( varName v'
                         , ( unembed $ varType v'
                           , map (coreToTerm tcsMap) es
                           )
                         )
                    ) dfuns

  let clsOpMap = HashMap.fromList
               $ map (\(v,i) ->
                       let v' = coreToBndr tcsMap v
                       in ( varName v'
                          , ( unembed $ varType v',i)
                          )
                     ) clsOps

  return (bindingsMap,dfunMap,clsOpMap)
