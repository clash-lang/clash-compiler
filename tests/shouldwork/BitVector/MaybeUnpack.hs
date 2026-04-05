{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module MaybeUnpack where

import qualified Prelude as P
import           GHC.Generics           (Generic)
import           System.Environment     (getArgs)

import           Clash.Explicit.Testbench
import           Clash.Prelude

data Wrapped = Wrapped (Index 3)
  deriving (Generic, NFDataX, BitPack, Eq, P.Show, ShowX)

data Small
  = Small0
  | Small1
  | Small2
  deriving (Generic, NFDataX, BitPack, Eq, P.Show, ShowX)

wasJust :: Maybe a -> Bool
wasJust Nothing = False
wasJust (Just _) = True

wasNothing :: Maybe a -> Bool
wasNothing = Clash.Prelude.not . wasJust

smallMaybeUnpackWorks :: BitVector 2 -> Bool
smallMaybeUnpackWorks x =
  if x == 3
    then wasNothing (maybeUnpack @Small x)
    else wasJust (maybeUnpack @Small x)

wrappedMaybeUnpackWorks :: BitVector 2 -> Bool
wrappedMaybeUnpackWorks x =
  if x == 3
    then wasNothing (maybeUnpack @Wrapped x)
    else wasJust (maybeUnpack @Wrapped x)
topEntity :: BitVector 2 -> (Bool, Bool)
topEntity x = (smallMaybeUnpackWorks x, wrappedMaybeUnpackWorks x)
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  testInput = stimuliGenerator clk rst (0 :> 1 :> 2 :> 3 :> Nil)
  expectedOutput =
    outputVerifier'
      clk
      rst
      ((True, True) :> (True, True) :> (True, True) :> (True, True) :> Nil)
  done = expectedOutput (topEntity <$> testInput)
  clk = tbSystemClockGen (Clash.Prelude.not <$> done)
  rst = systemResetGen

assertEqual :: (Eq a, P.Show a) => P.String -> a -> a -> IO ()
assertEqual label expected actual
  | actual == expected = pure ()
  | otherwise =
      P.error
        ( label
        P.<> ": expected "
        P.<> P.show expected
        P.<> ", but got "
        P.<> P.show actual
        )

assertNothing :: P.String -> Maybe a -> IO ()
assertNothing _ Nothing = pure ()
assertNothing label (Just _) =
  P.error (label P.<> ": expected Nothing, but got Just _")

mainCommon :: IO ()
mainCommon = do
  _ <- getArgs

  assertEqual "Generic sum valid 0" (Just Small0) (maybeUnpack @Small 0)
  assertEqual "Generic sum valid 2" (Just Small2) (maybeUnpack @Small 2)
  assertNothing "Generic sum invalid 3" (maybeUnpack @Small 3)

  assertEqual "Index valid 0" (Just 0) (maybeUnpack @(Index 3) 0)
  assertEqual "Index valid 2" (Just 2) (maybeUnpack @(Index 3) 2)
  assertNothing "Index invalid 3" (maybeUnpack @(Index 3) 3)

  let wrapped = Wrapped 2
  assertEqual "Generic roundtrip" (Just wrapped) (maybeUnpack (pack wrapped))
  assertNothing "Generic invalid field" (maybeUnpack @Wrapped 3)

  let pair = (1 :: Index 3, 2 :: Unsigned 2)
  assertEqual "Tuple roundtrip" (Just pair) (maybeUnpack (pack pair))
  assertNothing
    "Tuple invalid left field"
    (maybeUnpack @(Index 3, Unsigned 2) 0b1101)

  let vec = (0 :: Index 3) :> 2 :> Nil
  assertEqual "Vector roundtrip" (Just vec) (maybeUnpack (pack vec))
  assertNothing
    "Vector invalid element"
    (maybeUnpack @(Vec 2 (Index 3)) 0b1100)

mainVHDL :: IO ()
mainVHDL = mainCommon

mainVerilog :: IO ()
mainVerilog = mainCommon

mainSystemVerilog :: IO ()
mainSystemVerilog = mainCommon
