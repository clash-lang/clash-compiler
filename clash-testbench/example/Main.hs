{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Bool (bool)

import Clash.Prelude (Signal, Clock, Reset, Enable, Signed, System, exposeClockResetEnable, register, bundle, unsafeFromReset, hasReset, fromEnable, hasEnable)

import Clash.Testbench

import Calculator (OPC(..))
--import qualified Calculator (topEntity)
import qualified Register (topEntity)
import qualified RegisterFail (topEntity)

import Control.Monad (void)
import Control.Monad.IO.Class
import Clash.Hedgehog.Sized.Signed
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

{-
genIO :: Gen [(OPC (Signed 4), Maybe (Signed 4))]
genIO = do
  -- generate 7 constants
  cs  <- Gen.list (Range.singleton 7) (genSigned Range.constantBounded)
  -- generate 6 operations
  ops <- map (bool (ADD, (+)) (MUL, (*))) <$> Gen.list (Range.singleton 6) Gen.bool

  let
    -- push the constants to the stack
    in1 = concatMap ((: [Push])    . Imm)  cs -- inputs
    eo1 = concatMap ((: [Nothing]) . Just) cs -- expected outputs

    -- calculate the results of the applied operations
    x : xr = reverse cs
    rs = [ foldl (\a (op, b) -> op a b) x $ zip (map snd ops) $ take n xr
         | n <- [1,2..length xr]
         ]

    -- apply the operations
    in2 = concatMap ((replicate 3 Pop     <>) . pure . fst)  ops -- inputs
    eo2 = concatMap ((replicate 3 Nothing <>) . pure . Just) rs  -- expected outputs

  return $ zip (in1 <> in2) (eo1 <> eo2)

myTestbench
  :: TB ()
myTestbench = mdo
  input <- fromList Pop [Imm 1, Push, Imm 2, Push, Pop, Pop, Pop, ADD]
--  input <- matchIOGenN output genIO
  output <- ("topEntity" @@ Calculator.topEntity) auto auto auto input
  watch input
  watch output
-}

rstenb
  :: Clock  System
  -> Reset  System
  -> Enable System
  -> Signal System (Bool, Bool)
rstenb = exposeClockResetEnable
  $ bundle (unsafeFromReset hasReset, fromEnable hasEnable)

myTestbench
  :: TB ()
myTestbench = mdo
  input <- matchIOGenN output $ do
    cs  <- Gen.list (Range.singleton 7) (genSigned Range.constantBounded)
    return $ ((0,0) :) $ zip cs $ 0 : cs
  output <- ("topEntity" @@ Register.topEntity) auto auto auto input
--  x <- ("rstenb" @@ rstenb) auto auto auto
--  watch x
  watch input
  watch output

myTestbenchFail
  :: TB ()
myTestbenchFail = mdo
  input <- matchIOGenN output $ do
    cs  <- Gen.list (Range.singleton 7) (genSigned Range.constantBounded)
    return $ ((0,0) :) $ zip cs $ 0 : cs
  output <- ("topEntity" @@ RegisterFail.topEntity) auto auto auto input
--  x <- ("rstenb" @@ rstenb) auto auto auto
--  watch x
  watch input
  watch output


main :: IO ()
main =
--  simulate 10 myTestbench
  void $ checkParallel $ Group "Default"
    [ ("'successful test'", withTests 1 $ tbProperty myTestbench)
    , ("'failing test'", withTests 1 $ tbProperty myTestbenchFail)
    ]

foreign export ccall "clash_ffi_main"
  ffiMain :: IO ()

ffiMain :: IO ()
ffiMain = do
--  simulateFFI (SimSettings False False) myTestbench
  sync <- ffiHedgehog
  ffiCheckGroup sync $ Group "Default"
    [ ("'successful test'", withTests 1 $ (tbPropertyFFI sync) myTestbench)
--    [ ("'failing test'", withTests 1 $ (tbPropertyFFI sync) myTestbenchFail)
    ]
