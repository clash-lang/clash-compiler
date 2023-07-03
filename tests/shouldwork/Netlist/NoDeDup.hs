{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module NoDeDup where

import Prelude as P

import Clash.Magic
import Clash.Prelude
import Clash.Netlist.Types
import qualified Clash.Netlist.Id as Id

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

data ABCD = A | B | C | D

twice :: Int -> Int
twice a = 2 * a
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE twice #-}

-- | 'f' should have one call to twice in the netlist
f :: Int -> ABCD -> Int
f n abcd =
  case abcd of
    A -> twice (6 + n)
    B -> 5 + n
    C -> n - 3
    D -> twice (3 + n)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE f #-}

-- | 'g' should have two calls to twice in the netlist
g :: Int -> ABCD -> Int
g n abcd =
  case abcd of
    A -> noDeDup (twice (6 + n))
    B -> 5 + n
    C -> n - 3
    D -> noDeDup (twice (3 + n))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE g #-}

topEntity :: Int -> ABCD -> Int
topEntity n abcd = (f n abcd) - (g n abcd)

testPath :: FilePath
testPath = "tests/shouldwork/Netlist/NoDeDup.hs"

isTwiceInst (InstDecl Entity Nothing [] (Id.toText -> "twice") _ _ _) = True
isTwiceInst _ = False

assertNumTwiceInsts :: Component -> IO ()
assertNumTwiceInsts (Component nm inps outs ds) =
  case Id.toText nm of
    "f" | nTwiceInsts == 1 -> pure ()
        | otherwise ->
            error ( "Found " <> show nTwiceInsts <> " instances of twice in f. "
                 <> "Expected 1.")
    "g" | nTwiceInsts == 2 -> pure ()
        | otherwise ->
            error ( "Found " <> show nTwiceInsts <> " instances of twice in g. "
                 <> "Expected 2.")
    "twice" -> pure ()
    "topEntity" -> pure ()
    _ -> error ("Unexpected component: " <> show nm)
 where
  twiceInsts = filter isTwiceInst ds
  nTwiceInsts = P.length twiceInsts

mainVHDL :: IO ()
mainVHDL = do
  netlist <- runToNetlistStage SVHDL id testPath
  mapM_ (assertNumTwiceInsts . snd) netlist
