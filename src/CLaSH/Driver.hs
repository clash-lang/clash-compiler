{-# LANGUAGE FlexibleInstances #-}
module CLaSH.Driver where

import qualified Control.Concurrent.Supply as Supply
import qualified Data.HashMap.Lazy         as HashMap
import Unbound.LocallyNameless (embed,name2String,unembed)

import CLaSH.Core.Pretty (showDoc)
import CLaSH.Core.Var (Id,Var(..),varName,varType)
import CLaSH.GHC.GHC2Core (makeAllTyDataCons,coreToBndr,coreToTerm)
import CLaSH.GHC.LoadModules (loadModules)
import CLaSH.Normalize (runNormalization, normalize)
import CLaSH.Rewrite.Types (DebugLevel(..))
import CLaSH.Util

import Outputable (showPpr)

generateVHDL ::
  String
  -> IO ()
generateVHDL modName = do
  (allBindings,dfuns,tcs) <- loadModules modName
  let tcsMap = makeAllTyDataCons tcs

  let convertedBindings = map (\(x,e) -> (coreToBndr tcsMap x
                                         ,coreToTerm tcsMap e)
                              ) allBindings

  let convertedDfuns = map (\(x,es) -> (coreToBndr tcsMap x
                                      ,map (coreToTerm tcsMap) es)
                          ) dfuns

  let topEntities = filter (isTopEntity . fst) convertedBindings
  case topEntities of
    [topEntity] -> do
      let bindingsMap = HashMap.fromList $ map
                          (\(v,e) -> ( varName v
                                     , (unembed $ varType v,e)
                                     )
                          ) convertedBindings

      let dfunMap = HashMap.fromList $ map
                      (\(v,es) -> ( varName v
                                  , (unembed $ varType v, es)
                                  )
                      ) convertedDfuns

      supply <- Supply.newSupply
      let transformedBindings
            = map (\(v,(t,e)) -> (Id v (embed t),e))
            $ runNormalization DebugApplied supply bindingsMap dfunMap
            $ normalize [varName $ fst topEntity]

      let printedBindings = showDoc HashMap.empty transformedBindings
      putStrLn printedBindings

    [] -> error $ $(curLoc) ++ "No 'topEntity' found"
    _  -> error $ $(curLoc) ++ "Multiple 'topEntity's found"

isTopEntity ::
  Id
  -> Bool
isTopEntity = ("topEntity" ==) . name2String . varName
