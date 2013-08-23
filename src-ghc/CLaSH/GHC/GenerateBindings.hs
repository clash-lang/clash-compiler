module CLaSH.GHC.GenerateBindings
  (generateBindings)
where


import qualified Data.HashMap.Lazy       as HashMap
import           Unbound.LocallyNameless (unembed)

import           CLaSH.Core.Var          (Var(..))
import           CLaSH.Driver.Types
import           CLaSH.GHC.GHC2Core      (makeAllTyDataCons,coreToBndr,coreToPrimBndr,coreToTerm,coreToVar)
import           CLaSH.GHC.LoadModules   (loadModules)
import           CLaSH.Primitives.Types

generateBindings ::
  PrimMap
  -> String
  -> IO (BindingMap,DFunMap,ClassOpMap)
generateBindings primMap modName = do
  (bindings,dfuns,clsOps,unlocs,tcs) <- loadModules modName
  let unlocatable = unlocs ++ (map fst clsOps)
  let dfunvars = map fst dfuns
  let tcsMap = makeAllTyDataCons tcs

  let bindingsMap = HashMap.fromList
                  $ map (\(v,e) ->
                          let v' = coreToBndr tcsMap v
                          in ( varName v'
                             , ( unembed $ varType v'
                               , coreToTerm primMap unlocatable dfunvars tcsMap e
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
