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
import Clash.Signal.Internal (Signal (..))

import Data.String.Interpolate (i)
import qualified Data.Sequence as Seq

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

data FifoState n = FifoState
  { hsQueue      :: Seq.Seq (BitVector n)
  , relativeTime :: Int
  }

-- |
dcFifo ::
  forall depth n write read .
  ( KnownNat n
  , KnownDomain write
  , KnownDomain read

  , KnownNat depth
  -- Number of elements should be between [2**4, 2**17] ~ [16, 131072].
  , 4 <= depth
  , depth <= 17
  ) =>
  DcConfig (SNat depth) ->

  Clock write -> Clock read -> Reset read -> -- asynchronous reset

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
dcFifo DcConfig{..} !_wClk !_rClk !_rst writeData wEnable rEnable =
  let
    (wFull, wCnt, rEmpty, rCnt, rData) = go start wEnable rEnable writeData
  in
    ( pure 0
    , wFull
    , if dcWriteDataCount then wCnt else deepErrorX "Write data count disabled"
    , pure 0
    , rEmpty
    , if dcReadDataCount then rCnt else deepErrorX "Read data count disabled"
    , rData
    )
 where

  -- reified depth
  rD = snatToNum @Int (powSNat (SNat @2) (SNat @depth))

  -- TODO: how do we ensure it doesn't "space leak" in Haskell?
  --
  -- force slower first?
  -- https://github.com/clash-lang/clash-compiler/blob/ea114d8edd6a110f72d148203b9db2454cae8f37/clash-prelude/src/Clash/Explicit/BlockRam.hs#L1276-L1280

  go ::
    FifoState n ->
    Signal write Bool ->
    Signal read Bool ->
    Signal write (BitVector n) ->
    ( Signal write Full
    , Signal write (DataCount depth)
    , Signal read Empty
    , Signal read (DataCount depth)
    , Signal read (BitVector n)
    )
  go st@(FifoState _ rt) wEna rEna =
    if rt < tWr
      then goRead st wEna rEna
      else goWrite st wEna rEna

  goWrite (FifoState q rt) wEna rEna wData =
    (full, wCnt, fifoEmpty, rCnt, rData)
    where
      (preFull, preWCnt, fifoEmpty, rCnt, rData) = go (FifoState q' (rt-tWr)) wEna' rEna wData'
      wCnt = sDepth q :- preWCnt
      -- TODO: is this one cycle late?
      full = (if Seq.length q == rD then high else low) :- preFull
      (en :- wEna') = wEna
      (wDatum :- wData') = wData
      q' =
        if Seq.length q + 1 <= rD && en
          then wDatum Seq.<| q
          else q

  sDepth = fromIntegral . Seq.length

  goRead (FifoState q rt) wEna rEna wData =
    (full, wCnt, fifoEmpty, rCnt, rData)
    where
      -- TODO: is this one cycle late?
      fifoEmpty = (if Seq.length q == 0 then high else low) :- preEmpty
      rCnt = sDepth q :- preRCnt
      (full, wCnt, preEmpty, preRCnt, preRData) = go (FifoState q' (rt+tR)) wEna rEna' wData
      (en :- rEna') = rEna
      (q', rData) =
        if en
          then
            case Seq.viewl q of
              Seq.EmptyL -> (q, deepErrorX "FIFO empty" :- preRData)
              qDatum Seq.:< qData -> (qData, qDatum :- preRData)
          else (q, deepErrorX "Enable off" :- preRData)

  start :: FifoState n
  start = FifoState Seq.empty tR -- TODO: I think we want T_fast (period in fast clock domain)

  tWr = snatToNum @Int (clockPeriod @write)
  tR = snatToNum @Int (clockPeriod @read)

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
