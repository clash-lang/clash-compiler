{-|
Copyright  :  (C) 2022, Google Inc,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [FIFO Generator v13.2](https://docs.xilinx.com/v/u/en-US/pg057-fifo-generator).


-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Clash.Cores.Xilinx.DcFifo.Explicit
  ( -- * Instantiating IP
    dcFifo

    -- * Customizing IP
  , DcConfig(..)
  , DcImplementation(..)
  , ReadMode(..)
  , defConfig

    -- * Helper type aliases
  , ResetBusy
  , Full
  , Empty
  , DataCount
  ) where

import Clash.Explicit.Prelude
import Clash.Annotations.Primitive (Primitive(InlineYamlPrimitive))

import Data.String.Interpolate (i)

-- We want constructor names to correspond 1:1 to Xilinx options it ease
-- blackbox implementation.
{-# HLINT ignore "Use camelCase" #-}

type ResetBusy = Bit
type Full = Bit
type Empty = Bit
type DataCount n = Unsigned n

data ReadMode
  = Standard_FIFO
  | First_Word_Fall_Through
  deriving (Show, Generic)

data DcImplementation
  = Independent_Clocks_Block_RAM
  | Independent_Clocks_Distributed_RAM
  | Independent_Clocks_Builtin_FIFO
  deriving (Show, Generic)

data DcConfig depth = DcConfig
  { dcDepth          :: !depth
  , dcImplementation :: !DcImplementation
  , dcReadMode       :: !ReadMode
  , dcReadDataCount  :: !Bool
  , dcWriteDataCount :: !Bool
  }
  deriving (Show, Generic)

defConfig :: KnownNat depth => DcConfig (SNat depth)
defConfig = DcConfig
  { dcDepth = SNat
  , dcImplementation = Independent_Clocks_Block_RAM
  , dcReadMode = Standard_FIFO
  , dcReadDataCount = True
  , dcWriteDataCount = True
  }

-- |
dcFifo ::
  forall depth n write read .
  ( KnownNat n
  , KnownDomain write
  , KnownDomain read

  -- Number of elements should be between [2**4, 2**17] ~ [16, 131072].
  , 4 <= depth
  , depth <= 17
  ) =>
  DcConfig (SNat depth) ->

  Clock write -> Clock read -> Reset read ->

  -- | Write data
  Signal write (BitVector n) ->
  -- | Write enable
  Signal write Bool ->
  -- | Read enable
  Signal read Bool ->
  -- |
  ( Signal write ResetBusy
  , Signal write Full
  , Signal write (DataCount depth)

  , Signal read ResetBusy
  , Signal read Empty
  , Signal read (DataCount depth)
  , Signal read (BitVector n)
  )
dcFifo DcConfig{..} !_wClk !_rClk !_rst !_wData !_wEnable !_rEnable =
  ( deepErrorX "NYI"
  , deepErrorX "NYI"
  , if dcWriteDataCount then writeDataCount else deepErrorX "Write data count disabled"
  , deepErrorX "NYI"
  , deepErrorX "NYI"
  , if dcReadDataCount then readDataCount else deepErrorX "Read data count disabled"
  , deepErrorX "NYI"
  )
 where
  readDataCount = deepErrorX "NYI"
  writeDataCount = deepErrorX "NYI"
{-# NOINLINE dcFifo #-}
{-# ANN dcFifo (InlineYamlPrimitive [minBound..maxBound] [i|
BlackBoxHaskell:
    name: Clash.Cores.Xilinx.DcFifo.Explicit.dcFifo
    templateFunction: Clash.Cores.Xilinx.DcFifo.BlackBoxes.dcFifoBBF
    workInfo: Always
    #    multiResult: true
    includes:
    - name: fifo
      extension: tcl
      format: Haskell
      templateFunction: Clash.Cores.Xilinx.DcFifo.BlackBoxes.dcFifoTclTF
|]) #-}
