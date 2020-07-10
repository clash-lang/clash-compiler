{-# LANGUAGE CPP #-}
module NumConstantFolding_1 (module NumConstantFolding_1, module ConstantFoldingUtil) where
import ConstantFoldingUtil

#ifndef OUTPUTTEST
import Clash.Prelude
import NumConstantFolding_Common

------------------------
-- Test individual types
------------------------
tUnsigned16
  = ( cNum            @(Unsigned 16)
    , cEq             @(Unsigned 16)
    , cOrd            @(Unsigned 16)
    , cIntegral       @(Unsigned 16)
    , cParity         @(Unsigned 16)
    , cParityBS0      @(Unsigned 0 )
    , cBits           @(Unsigned 16)
    -- , cFiniteBits  @(Unsigned 16) -- broken
    , csClashSpecific @(Unsigned 16)
    , cResize         @(Unsigned 16)
    )

tSigned16
  = ( cNum            @(Signed 16)
    , cEq             @(Signed 16)
    , cOrd            @(Signed 16)
    , cIntegral       @(Signed 16)
    , cParity         @(Signed 16)
    , cParityBS0      @(Signed 0 )
    , cBits           @(Signed 16)
    -- , cFiniteBits  @(Signed 16) -- broken
    , csClashSpecific @(Signed 16)
    , cResize         @(Signed 16)
    )

tBitVector16
  = ( cNum            @(BitVector 16)
    , cEq             @(BitVector 16)
    , cOrd            @(BitVector 16)
    , cIntegral       @(BitVector 16)
    , cParity         @(BitVector 16)
    , cParityBS0      @(BitVector 0 )
    , cBits           @(BitVector 16)
    -- , cFiniteBits  @(BitVector 16) -- broken
    , csClashSpecific @(BitVector 16)
    , cResize         @(BitVector 16)
    )

tIndex
  = ( cNum            @(SatIndex 'SatError 50000)
    , cEq             @(SatIndex 'SatError 50000)
    , cOrd            @(SatIndex 'SatError 50000)
    , cIntegral       @(SatIndex 'SatError 50000)
    , cParity         @(SatIndex 'SatError 50000)
    , cParityBS0      @(SatIndex 'SatError 1    )
    , cBits           @(SatIndex 'SatError 65536)
    -- , cFiniteBits     @(Index 50000) -- broken
    , cIndex1 -- ensure special case for index 1 is verified
    , csClashSpecific @(SatIndex 'SatError 50000)
    , cResize         @(SatIndex 'SatError 50000)
    )

tSFixed
  = ( cNum            @(SFixed 16 0)
    , cEq             @(SFixed 16 0)
    , cOrd            @(SFixed 16 0)
    -- no Integral
    , cBits           @(SFixed 16 0)
    -- , cFiniteBits     @(SFixed 16 0) -- broken
    , csClashSpecific @(SFixed 16 0)
    )


topEntity
 = ( tUnsigned16
   , tSigned16
   , tBitVector16
   , tIndex
   , tSFixed
   )
{-# NOINLINE topEntity #-}
#endif
