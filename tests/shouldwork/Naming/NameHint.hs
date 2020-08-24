{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module NameHint where

import Clash.Prelude
import Clash.Netlist.Types

import Prelude as P
import Data.Text (isInfixOf)

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

import Debug.Trace

topEntity
  :: Signal System Bool
  -> Signal System Bool
topEntity = liftA $ nameHint (SSymbol @"someSignalName")

testPath :: FilePath
testPath = "tests/shouldwork/Naming/NameHint.hs"

-- | Assert that a signal named "someSignalName", optionally expanded, is
-- declared once and used in an assignment once.
assertOneDecl :: Component -> IO ()
assertOneDecl (Component _ _ _ ds) =
  case P.concatMap isSigDecl ds of
    [i] ->
      case P.length (filter (isSigAssignment i) ds) of
        1 ->
          pure ()
        n ->
          error $ "Expected one assignment of a signal named "
                  <> "\"someSignalName\", got " <> show n
    is ->
      error $ "Expected one declaration of a signal named "
           <> "\"someSignalName\", got " <> show (P.length is)
 where
  isSigDecl (NetDecl' _ _ i@(isInfixOf "someSignalName" -> True) _ _) = [i]
  isSigDecl _ = []

  isSigAssignment i (Assignment _ (Identifier i' _)) = i == i'
  isSigAssignment _ _ = False

getComponent :: (a, b, c, d) -> d
getComponent (_, _, _, x) = x

mainVHDL :: IO ()
mainVHDL = do
  netlist <- runToNetlistStage SVHDL id testPath
  mapM_ (assertOneDecl . getComponent) netlist

mainVerilog :: IO ()
mainVerilog = do
  netlist <- runToNetlistStage SVerilog id testPath
  mapM_ (assertOneDecl . getComponent) netlist

