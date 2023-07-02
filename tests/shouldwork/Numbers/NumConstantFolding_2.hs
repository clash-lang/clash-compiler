{-# LANGUAGE CPP                   #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module NumConstantFolding_2 (module NumConstantFolding_2, module ConstantFoldingUtil) where
import ConstantFoldingUtil

#ifndef OUTPUTTEST
import NumConstantFolding_Common
import Clash.Prelude

import Data.Int
import Data.Word
import GHC.Natural

------------------------
-- Test individual types
------------------------

bvSpecific = (r1,r2,r3,r4)
  where
    r1 = (lit 22001 :: BitVector 16) ++# (lit 22002 :: BitVector 16)
    r2 = lit 22003 - size# (lit 22004::BitVector 16)
    r3 = lit 22005 - maxIndex# (lit 22006::BitVector 16)
    r4 = (lit 22007 :: BitVector 16) ! 0

fromIntegralConversions
  = ( ( convertTo @Integer
      , convertTo @Int
      , convertTo @Int8
      , convertTo @Int16
      , convertTo @Int32
      , convertTo @Int64
      )
    , ( convertTo @Word
      , convertTo @Word8
      , convertTo @Word16
      , convertTo @Word32
      , convertTo @Word64
      )
    , ( convertTo @(Signed 16)
      , convertTo @(Unsigned 16)
      , convertTo @(BitVector 16)
      , convertTo @(Index 30000)
      )
    )
    where
      convertTo :: forall b. Num b => _
      convertTo = ((r00,r01,r02,r03,r04,r05,r06,r07,r08,r09,r10),r11,r12,r13,r14)
        where
          r00 = lit 22010 - fromIntegral @Integer        @b (lit 100)
          r01 = lit 22011 - fromIntegral @Int            @b (lit 100)
          r02 = lit 22012 - fromIntegral @Int8           @b (lit 100)
          r03 = lit 22013 - fromIntegral @Int16          @b (lit 100)
          r04 = lit 22014 - fromIntegral @Int32          @b (lit 100)
          r05 = lit 22015 - fromIntegral @Int64          @b (lit 100)
          r06 = lit 22016 - fromIntegral @Word           @b (lit 100)
          r07 = lit 22017 - fromIntegral @Word8          @b (lit 100)
          r08 = lit 22018 - fromIntegral @Word16         @b (lit 100)
          r09 = lit 22019 - fromIntegral @Word32         @b (lit 100)
          r10 = lit 22020 - fromIntegral @Word64         @b (lit 100)
          r11 = lit 22021 - fromIntegral @(Signed 17)    @b (lit 100)
          r12 = lit 22022 - fromIntegral @(Unsigned 17)  @b (lit 100)
          r13 = lit 22023 - fromIntegral @(BitVector 17) @b (lit 100)
          r14 = lit 22024 - fromIntegral @(Index 30000)  @b (lit 100)

tInteger
  = ( cNum            @Integer
    , cEq             @Integer
    , cOrd            @Integer
    , cIntegral       @Integer
    , cBitsNoPopCount @Integer -- popCount @Integer is just crazy
    -- no FiniteBits
    )

tNatural
  = ( cNum        @Natural
    , cEq         @Natural
    , cOrd        @Natural
    , cIntegral   @Natural
    -- , cBits       @Natural -- broken
    -- no FiniteBits
    )

tUFixed
  = ( cNum            @(UFixed 16 0)
    , cEq             @(UFixed 16 0)
    , cOrd            @(UFixed 16 0)
    -- no Integral
    , cBits           @(UFixed 16 0)
    -- , cFiniteBits     @(UFixed 16 0) -- broken
    , csClashSpecific @(UFixed 16 0)
    )

-- TODO Types
-- Bit?

tInt         = csGenericHaskell @Int
tInt16       = csGenericHaskell @Int16
tWord16      = csGenericHaskell @Word16

topEntity
 = ( bvSpecific
   , fromIntegralConversions
   , tInteger
   , tNatural
   , tUFixed
   , tInt
   , tInt16
   , tWord16
   )
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}
#endif
