{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module MaybeUnpack where

import           GHC.Generics           (Generic)

import           Clash.Explicit.Testbench
import           Clash.Prelude

data Wrapped = Wrapped (Index 3)
  deriving (Generic, NFDataX, BitPack, Eq, Show, ShowX)

data Small
  = Small0
  | Small1
  | Small2
  deriving (Generic, NFDataX, BitPack, Eq, Show, ShowX)

topEntity
  :: BitVector 2
  -> ( Maybe Small
     , Maybe Wrapped
     , Maybe (Index 3, Unsigned 2)
     , Maybe (Vec 2 (Index 3))
     , Maybe (Index 3, Index 3, Index 3, Index 3)
     )
topEntity x =
  ( maybeUnpack @Small x
  , maybeUnpack @Wrapped x
  , maybeUnpack @(Index 3, Unsigned 2) wide
  , maybeUnpack @(Vec 2 (Index 3)) wide
  , maybeUnpack @(Index 3, Index 3, Index 3, Index 3) (wide ++# wide)
  )
 where
  wide = x ++# x
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  testInput = stimuliGenerator clk rst (0 :> 1 :> 2 :> 3 :> Nil)
  expectedOutput =
    outputVerifier'
      clk
      rst
      ( ( Just Small0
        , Just (Wrapped 0)
        , Just (0, 0)
        , Just (0 :> 0 :> Nil)
        , Just (0, 0, 0, 0)
        )
      :> ( Just Small1
         , Just (Wrapped 1)
         , Just (1, 1)
         , Just (1 :> 1 :> Nil)
         , Just (1, 1, 1, 1)
         )
      :> ( Just Small2
         , Just (Wrapped 2)
         , Just (2, 2)
         , Just (2 :> 2 :> Nil)
         , Just (2, 2, 2, 2)
         )
      :> (Nothing, Nothing, Nothing, Nothing, Nothing)
      :> Nil
      )
  done = expectedOutput (topEntity <$> testInput)
  clk = tbSystemClockGen (Clash.Prelude.not <$> done)
  rst = systemResetGen
