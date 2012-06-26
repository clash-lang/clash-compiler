{-# LANGUAGE FlexibleInstances #-}
module CLaSH.Driver where

import qualified Control.Concurrent.Supply as Supply
import qualified Data.HashMap.Lazy         as HashMap
import           Unbound.LocallyNameless      (embed,name2String)
import           Text.PrettyPrint.Leijen.Text (putDoc)

import           CLaSH.Core.Pretty            (showDoc)
import           CLaSH.Core.Term              (TmName)
import           CLaSH.Core.Var               (Var(..))
import           CLaSH.Driver.PrepareBinding
import           CLaSH.Netlist                (runNetlistMonad,genComponent)
import           CLaSH.Netlist.VHDL           (genVHDL)
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
            = runNormalization DebugFinal supply bindingsMap dfunMap clsOpMap
            $ normalize [fst topEntity]

      let transformedTopEntity = head transformedBindings
      let netlist = runNetlistMonad (HashMap.fromList $ transformedBindings)
                  $ genComponent (fst transformedTopEntity)
                                 (snd $ snd transformedTopEntity)

      putDoc $ snd (genVHDL netlist)
      -- let printedBindings = showDoc HashMap.empty
      --                     $ map (\(v,(t,e)) -> (Id v (embed t),e))
      --                         transformedBindings
      -- putStrLn printedBindings

    [] -> error $ $(curLoc) ++ "No 'topEntity' found"
    _  -> error $ $(curLoc) ++ "Multiple 'topEntity's found"

isTopEntity ::
  TmName
  -> a
  -> Bool
isTopEntity var _ = name2String var == "topEntity"
