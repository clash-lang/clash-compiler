{-|
Copyright  :  (C) 2022, Google Inc,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [FIFO Generator v13.2](https://docs.xilinx.com/v/u/en-US/pg057-fifo-generator).

It is necessary to read the product guide linked above in order to effectively
use the FIFO. The product guide also documents how to interface the FIFO
correctly. To aid comprehension, the signals in this module also mention the
name the product guide uses for that signal like @this@.

Note that the behavior of the FIFO in Haskell simulation does not correspond
exactly to the behavior in HDL simulation, and that neither behaviors correspond
exactly to hardware. All the different behaviors are functionally correct and
equivalent when the usage guidelines in the product guide are observed.

Furthermore, the FIFO is configured as follows:

* /Native/ interface type
* /Independent clocks block RAM/ implementation
* 2 synchronization stages
* /Standard FIFO/ read mode
* /No/ output registers
* Reset pin /enabled/, reset synchronization /disabled/, safety circuit
/disabled/

    (Note: the GUI will still say /Asynchronous Reset/, grayed out. This might be
    confusing: the resets are actually synchronous.)

* Full flag reset value: /@0 (False)@/

    (Note: this is actually not configurable for synchronous resets, it is
    always @False@.)

* No /Dout reset value/
* Optional overflow flag: 'dcOverflow'
* Optional underflow flag: 'dcUnderflow'
* No other configurable status flags (no almost full, almost empty, write
acknowledge, valid, or programmable full\/empty flags)
* Optional write data count: 'dcWriteDataCount' (only full width is supported)
* Optional read data count: 'dcReadDataCount' (only full width is supported)

(The order of these items corresponds to Vivado's /Customize IP/ GUI dialog in
Vivado 2022.1.)
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Cores.Xilinx.DcFifo
  ( -- ** Reset sequencing
    --
    -- $resetSeq

    -- * Instantiating IP
    dcFifo
  , FifoOut(..)

    -- * Customizing IP
  , DcConfig(..)
  , defConfig

    -- * Helper type aliases
  , Full
  , Empty
  , DataCount
  ) where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Signal (..), ClockAB (..), clockTicks)
import Data.Maybe (isJust)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.String.Interpolate (__i)
import GHC.Stack (HasCallStack)

import Clash.Annotations.Primitive (Primitive (InlineYamlPrimitive))

import Clash.Cores.Xilinx.DcFifo.Internal.BlackBoxes
import Clash.Cores.Xilinx.DcFifo.Internal.Instances ()
import Clash.Cores.Xilinx.DcFifo.Internal.Types

{- $setup
>>> import Clash.Explicit.Prelude
-}

{- $resetSeq

  The FIFO only supports synchronous resets ('Clash.Signal.Synchronous'). If the
  read or write domain has an asynchronous reset signal, simulating the FIFO or
  generating HDL for it will throw an error.

  The FIFO does not need to be reset; it is possible to use the FIFO with its
  reset inputs permanently deasserted. However, resetting the FIFO will flush
  it, so resets do have a use. It is necessary to generate a proper reset
  sequence; see the product guide for details. Read and write side need to be
  reset together, both for at least one clock cycle. During the time where
  either or both of the resets are asserted, do not perform write or read
  operations to avoid unexpected behavior.
-}

-- | Default config. Read\/write data counts are enabled but over-\/underflow
-- are not.
--
-- Examples using a type-level argument to specify the depth:
--
-- >>> fifo = dcFifo (defConfig @5)
-- >>> fifo = dcFifo @5 defConfig
-- >>> fifo = dcFifo (defConfig @5){dcReadDataCount=False}
-- >>> fifo = dcFifo @5 defConfig{dcReadDataCount=False}
defConfig :: KnownNat depth => DcConfig depth
defConfig = DcConfig
  { dcDepth = SNat
  , dcReadDataCount = True
  , dcWriteDataCount = True
  , dcOverflow = False
  , dcUnderflow = False
  }

-- | Xilinx dual clock FIFO
--
-- For an explanation about the type-level parameter @depth@, see 'DcConfig'.
--
-- If 'dcReadDataCount', 'dcUnderflow', 'dcOverflow', or 'dcWriteDataCount' are
-- disabled, the relevant signals will return 'XException'.
dcFifo ::
  forall depth a write read .
  ( KnownDomain write
  , KnownDomain read

  , NFDataX a

  , KnownNat depth
  -- Number of elements should be between [2**4, 2**17] ~ [16, 131072].
  , 4 <= depth
  , depth <= 17
  , HasCallStack
  ) =>
  DcConfig depth ->

  -- | Write clock, @wr_clk@
  Clock write ->
  -- | Synchronous write-side reset, @wr_rst@
  Reset write ->
  -- | Read clock, @rd_clk@
  Clock read ->
  -- | Synchronous read-side reset, @rd_rst@
  Reset read ->

  -- | Write data, @din@ and @wr_en@
  Signal write (Maybe a) ->
  -- | Read enable @rd_en@
  Signal read Bool ->
  FifoOut read write depth a
dcFifo DcConfig{..} wClk wRst rClk rRst writeData rEnable =
  case (resetKind @write, resetKind @read) of
    (SSynchronous, SSynchronous) ->
      let
        (wFull, wOver, wCnt, rEmpty, rUnder, rCnt, rData) =
          go (clockTicks wClk rClk) mempty rstSignalR rEnable rstSignalW writeData
      in FifoOut
          wFull
          (if dcOverflow
             then register wClk wRst enableGen False wOver
             else errorX "Overflow disabled")
          (if dcWriteDataCount then wCnt else errorX "Write data count disabled")
          rEmpty
          (if dcUnderflow
             then register rClk rRst enableGen False rUnder
             else errorX "Underflow disabled")
          (if dcReadDataCount then rCnt else errorX "Read data count disabled")
          (register rClk rRst enableGen (deepErrorX "No initial value") rData)
    _ -> error $ show 'dcFifo <> " only supports synchronous resets"

 where
  rstSignalR = unsafeToHighPolarity rRst
  rstSignalW = unsafeToHighPolarity wRst

  -- reified depth
  maxDepth = natToNum @(2 ^ depth - 1) @Int

  go ::
    [ClockAB] ->
    Seq a ->
    Signal read Bool -> -- reset
    Signal read Bool -> -- read enabled
    Signal write Bool -> -- reset
    Signal write (Maybe a) -> -- write data
    ( Signal write Full
    , Signal write Bool
    , Signal write (DataCount depth)

    , Signal read Empty
    , Signal read Bool
    , Signal read (DataCount depth)
    , Signal read a
    )
  go (ClockA:ticks)  = goWrite ticks
  go (ClockB:ticks)  = goRead ticks
  go (ClockAB:ticks) = go (ClockB:ClockA:ticks)
  go [] = error "dcFifo.go: `ticks` should have been an infinite list"

  goWrite ticks _q rstR rEna (True :- rstWNext) (_ :- wData) =
      -- The register will discard the @wOver@ sample
      (False :- preFull, undefined :- preOver, 0 :- preWCnt, fifoEmpty, under, rCnt, rData)
    where
      (preFull, preOver, preWCnt, fifoEmpty, under, rCnt, rData) =
        go ticks mempty rstR rEna rstWNext wData

  goWrite ticks q rstR rEna (_ :- rstW) (wDat :- wDats1) =
    (full, over, wCnt, fifoEmpty, under, rCnt, rData)
    where
      (preFull, preOver, preWCnt, fifoEmpty, under, rCnt, rData) =
        go ticks q' rstR rEna rstW wDats1

      wCnt = sDepth q :- preWCnt
      full = (Seq.length q == maxDepth) :- preFull
      (q', over) =
        if Seq.length q < maxDepth
          then (case wDat of { Just x -> x Seq.<| q ; _ -> q }, False :- preOver)
          else (q, isJust wDat :- preOver)

  sDepth = fromIntegral . Seq.length

  goRead ticks _q (True :- rstRNext) (_ :- rEnas1) rstW wData =
    (full, over, wCnt, fifoEmpty, under, rCnt, rData)
    where
      -- The register will discard the sample
      rData = undefined :- preRData
      fifoEmpty = True :- preEmpty
      rCnt = 0 :- preRCnt
      -- The register will discard the sample
      under = undefined :- preUnder

      (full, over, wCnt, preEmpty, preUnder, preRCnt, preRData) =
        go ticks mempty rstRNext rEnas1 rstW wData

  goRead ticks q (_ :- rstRNext) (rEna :- rEnas1) rstW wData =
    (full, over, wCnt, fifoEmpty, under, rCnt, rData)
    where
      rCnt = sDepth q :- preRCnt
      fifoEmpty = (Seq.length q == 0) :- preEmpty
      rData = nextData :- preRData

      (full, over, wCnt, preEmpty, preUnder, preRCnt, preRData) =
        go ticks q' rstRNext rEnas1 rstW wData

      (q', nextData, under) =
        if rEna
          then
            case Seq.viewr q of
              Seq.EmptyR -> (q, deepErrorX "FIFO empty", True :- preUnder)
              qData Seq.:> qDatum -> (qData, qDatum, False :- preUnder)
          else (q, deepErrorX "Enable off", False :- preUnder)
{-# NOINLINE dcFifo #-}
{-# ANN dcFifo (
   let primName = 'dcFifo
       tfName = 'dcFifoBBF
   in InlineYamlPrimitive [minBound..] [__i|
        BlackBoxHaskell:
            name: #{primName}
            templateFunction: #{tfName}
            workInfo: Always
        |]) #-}
