{-# LANGUAGE FlexibleInstances #-}
module CLaSH.Driver where

import qualified Control.Concurrent.Supply as Supply
import qualified Data.HashMap.Lazy         as HashMap
import           Unbound.LocallyNameless      (embed,name2String)

import           CLaSH.Core.Pretty            (showDoc)
import           CLaSH.Core.Term              (TmName)
import           CLaSH.Core.Var               (Var(..))
import           CLaSH.Driver.PrepareBinding
import           CLaSH.Normalize              (runNormalization, normalize)
import           CLaSH.Rewrite.Types          (DebugLevel(..))
import           CLaSH.Util

generateVHDL ::
  String
  -> IO ()
generateVHDL modName = do
  (bindingsMap,dfunMap,clsOpMap) <- prepareBinding modName

  let topEntities = HashMap.toList
                  $ HashMap.filterWithKey isTopEntity bindingsMap

  case topEntities of
    [topEntity] -> do
      supply <- Supply.newSupply
      let transformedBindings
            = map (\(v,(t,e)) -> (Id v (embed t),e))
            $ runNormalization DebugApplied supply bindingsMap dfunMap clsOpMap
            $ normalize [fst topEntity]

      let printedBindings = showDoc HashMap.empty transformedBindings
      putStrLn printedBindings

    [] -> error $ $(curLoc) ++ "No 'topEntity' found"
    _  -> error $ $(curLoc) ++ "Multiple 'topEntity's found"

isTopEntity ::
  TmName
  -> a
  -> Bool
isTopEntity var _ = name2String var == "topEntity"
