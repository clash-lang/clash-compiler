{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module NoDeDup where

import Prelude as P

import Clash.Magic
import Clash.Prelude
import Clash.Netlist.Types

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

data ABCD = A | B | C | D

twice :: Int -> Int
twice a = 2 * a
{-# NOINLINE twice #-}

-- | 'f' should have one call to twice in the netlist
f :: Int -> ABCD -> Int
f n abcd =
  case abcd of
    A -> twice (6 + n)
    B -> 5 + n
    C -> n - 3
    D -> twice (3 + n)
{-# NOINLINE f #-}

-- | 'g' should have two calls to twice in the netlist
g :: Int -> ABCD -> Int
g n abcd =
  case abcd of
    A -> noDeDup (twice (6 + n))
    B -> 5 + n
    C -> n - 3
    D -> noDeDup (twice (3 + n))
{-# NOINLINE g #-}

topEntity :: Int -> ABCD -> Int
topEntity n abcd = (f n abcd) - (g n abcd)

testPath :: FilePath
testPath = "tests/shouldwork/Netlist/NoDeDup.hs"

isTwiceInst (InstDecl Entity Nothing [] "twice" _ _ _) = True
isTwiceInst _ = False

assertNumTwiceInsts :: Component -> IO ()
assertNumTwiceInsts (Component nm inps outs ds) =
  case nm of
    "f" | nTwiceInsts == 1 -> pure ()
        | otherwise ->
            error ( "Found " <> show nTwiceInsts <> " instances of twice in f. "
                 <> "Expected 1.")
    "g" | nTwiceInsts == 2 -> pure ()
        | otherwise ->
            error ( "Found " <> show nTwiceInsts <> " instances of twice in g. "
                 <> "Expected 2.")
    "twice" -> pure ()
    "topentity" -> pure ()
    _ -> error ("Unexpected component: " <> show nm)
 where
  twiceInsts = filter isTwiceInst ds
  nTwiceInsts = P.length twiceInsts

getComponent :: (a, b, c, d) -> d
getComponent (_, _, _, x) = x

mainVHDL :: IO ()
mainVHDL = do
  netlist <- runToNetlistStage SVHDL id testPath
  mapM_ (assertNumTwiceInsts . getComponent) netlist
