{-|
  Copyright   :  (C) 2020, Foamspace corp
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  hcab14@gmail.com

  LATTICE ECP5 IO primitives. Implementations are documented in the
  <http://www.latticesemi.com/-/media/LatticeSemi/Documents/ApplicationNotes/EH/FPGA-TN-02032-1-2-ECP5-ECP5G-sysIO-Usage-Guide.ashx?document_id=50464>.
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Clash.Cores.LatticeSemi.ECP5.IO
  ( bidirectionalBuffer
  ) where

import           Clash.Annotations.Primitive  (Primitive(..), HDL(..), hasBlackBox)
import           Clash.Prelude
import           Clash.Signal.BiSignal
import           Data.String.Interpolate      (i)
import           Data.String.Interpolate.Util (unindent)
import           GHC.Stack                    (HasCallStack())

-- | BB primitive
bidirectionalBuffer
  :: forall ds dom
   . ( HasCallStack
     , HasBiSignalDefault ds
     , KnownDomain dom
     )
  => Enable dom
  -- ^ output enable
  -> BiSignalIn ds dom 1
  -- ^ PKG_PIN output BiSignal
  -> Signal dom Bit
  -- ^ output bit
  -> ( BiSignalOut ds dom 1 -- PKG_PIN input BiSignal
     , Signal dom Bit       -- input bit
     )
bidirectionalBuffer en pkgPinOut output = (pkgPinIn, dIn)
  where
    (pkgPinIn,dIn) = -- the BB primitve has an active low enable signal
      bbECP5 intrinsicName pkgPinOut output invertedEnable
    invertedEnable = not <$> fromEnable en
    intrinsicName = case (pullUpMode pkgPinOut) of
                      SFloating -> "BB"
                      SPullUp   -> "BBPU"
                      SPullDown -> "BBPD"
-- {-# NOINLINE bidirectionalBuffer #-}

bbECP5
  :: forall ds dom
   . ( HasCallStack
     , HasBiSignalDefault ds
     , KnownDomain dom
     )
  => String
  -> BiSignalIn ds dom 1
  -> Signal dom Bit
  -> Signal dom Bool
  -> ( BiSignalOut ds dom 1
     , Signal dom Bit
     )
bbECP5 _intrinsicName pkgPinIn output notOutputEnable
  = (pkgPinOut, dIn)
   where
     dIn :: Signal dom Bit
     dIn = readFromBiSignal pkgPinIn
     pkgPinOut = writeToBiSignal pkgPinIn (toMaybe . not <$> notOutputEnable <*> output)

     toMaybe :: Bool -> a -> Maybe a
     toMaybe True a  = Just a
     toMaybe False _ = Nothing
{-# NOINLINE bbECP5 #-}
{-# ANN bbECP5 hasBlackBox #-}
{-# ANN bbECP5 (InlinePrimitive [VHDL,Verilog,SystemVerilog] $ unindent [i|
   [ { "BlackBox" :
        { "name" : "Clash.Cores.LatticeSemi.ECP5.IO.bbECP5",
          "kind" : "Declaration",
          "format": "Haskell",
          "templateFunction": "Clash.Cores.LatticeSemi.ECP5.Blackboxes.IO.bbTF"
        }
     }
   ]
   |]) #-}
