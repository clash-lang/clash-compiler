{-|
Copyright  :  (C) 2022, Google Inc,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Support for the [FIFO Generator v13.2](https://docs.xilinx.com/v/u/en-US/pg057-fifo-generator).


-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Clash.Cores.Xilinx.DcFifo.Explicit
  ( -- * Instantiating IP
    dcFifo
  , XilinxFifo(..)

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

import           Clash.Annotations.Primitive (Primitive (InlineYamlPrimitive))
import           Clash.Explicit.Prelude
import           Clash.Signal.Internal       (Signal (..))


import qualified Data.Sequence               as Seq
import           Data.String.Interpolate     (i)

-- We want constructor names to correspond 1:1 to Xilinx options it ease
-- blackbox implementation.
{-# HLINT ignore "Use camelCase" #-}

type ResetBusy = Bool
type Full = Bool
type Empty = Bool
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
  , dcOverflow       :: !Bool
  , dcUnderflow      :: !Bool
  }
  deriving (Show, Generic)

defConfig :: KnownNat depth => DcConfig (SNat depth)
defConfig = DcConfig
  { dcDepth = SNat
  , dcImplementation = Independent_Clocks_Block_RAM
  , dcReadMode = Standard_FIFO
  , dcReadDataCount = True
  , dcWriteDataCount = True
  , dcOverflow = False
  , dcUnderflow = False
  }

data FifoState n = FifoState
  { hsQueue      :: Seq.Seq (BitVector n)
  , relativeTime :: Int -- ^ In order to model a FIFO in two clock domains,
                        -- we track relative offset in order to know which
                        -- signals to peel off.
  } deriving Show

data XilinxFifo read write depth n =
  XilinxFifo
    { writeReset :: Signal write ResetBusy -- ^ @wr_rst@
    , isFull :: Signal write Full -- ^ @full@
    , isOverflow :: Signal write Bool -- ^ @overflow@
    , writeCount :: Signal write (DataCount depth) -- ^ @wr_data_count@
    , readReset :: Signal read ResetBusy -- ^ @rd_rst@
    , isEmpty :: Signal read Empty -- ^ @empty@
    , isUnderflow :: Signal read Bool -- ^ @underflow@
    , readCount :: Signal read (DataCount depth) -- ^ @rd_data_count@
    , fifoData :: Signal read (BitVector n) -- ^ @dout@
    }

-- | Xilinx FIFO, see [documentation](https://docs.xilinx.com/v/u/en-US/pg057-fifo-generator)
-- for expected behavior. Note that the behavior does not correspond exactly to
-- the behavior of the RTL model (which doesn't correspond exactly to hardware).
--
-- If 'dcReadDataCount', 'dcUnderflow', 'dcOverflow', or 'dcWriteDataCount' are disabled, the relevant signals
-- will be 'deepErrorX'
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

  -- | @wr_clk@
  Clock write ->
  -- | @rd_clk@
  Clock read ->
  -- | Asynchronous reset @rst@
  Reset read ->

  -- | Write data, @din@ and @wr_en@
  Signal write (Maybe (BitVector n)) ->
  -- | Read enable @rd_en@
  Signal read Bool ->
  XilinxFifo read write depth n
dcFifo DcConfig{..} wClk rClk rst writeData rEnable =
  let
    (wRstBusy, wFull, wOver, wCnt, rRstBusy, rEmpty, rUnder, rCnt, rData) =
      go initState rstSignalR rEnable rstSignalW writeData
  in XilinxFifo
      wRstBusy
      wFull
      (if dcOverflow then wOver else deepErrorX "Overflow disabled")
      (if dcWriteDataCount then wCnt else deepErrorX "Write data count disabled")
      rRstBusy
      rEmpty
      (if dcUnderflow then rUnder else deepErrorX "Underflow disabled")
      (if dcReadDataCount then rCnt else deepErrorX "Read data count disabled")
      (deepErrorX "No sample" :- rData)

 where
  rstSignalR = unsafeToHighPolarity rst
  rstSignalW = unsafeSynchronizer rClk wClk $ unsafeToHighPolarity rst

  -- reified depth
  maxDepth = snatToNum @Int (powSNat (SNat @2) (SNat @depth))

  go ::
    FifoState n ->
    Signal read Bool -> -- reset
    Signal read Bool -> -- read enabled
    Signal write Bool -> -- reset
    Signal write (Maybe (BitVector n)) -> -- write data
    ( Signal write ResetBusy
    , Signal write Full
    , Signal write Bool
    , Signal write (DataCount depth)

    , Signal read ResetBusy
    , Signal read Empty
    , Signal read Bool
    , Signal read (DataCount depth)
    , Signal read (BitVector n)
    )
  go st@(FifoState _ rt) rstR rEna rstW =
    if rt < tWr
      then goRead st rstR rEna rstW
      else goWrite st rstR rEna rstW

  goWrite (FifoState _ rt) rstR rEna (True :- rstWNext) (_ :- wData) =
      (True :- wRstBusy, False :- preFull, False :- preOver, 0 :- preWCnt, rRstBusy, fifoEmpty, under, rCnt, rData)
    where
      (wRstBusy, preFull, preOver, preWCnt, rRstBusy, fifoEmpty, under, rCnt, rData) =
        go (FifoState mempty (rt-tWr)) rstR rEna rstWNext wData

  goWrite (FifoState q rt) rstR rEna (_ :- rstW) (wDat :- wDats1) =
    (False :- wRstBusy, full, over, wCnt, rRstBusy, fifoEmpty, under, rCnt, rData)
    where
      (wRstBusy, preFull, preOver, preWCnt, rRstBusy, fifoEmpty, under, rCnt, rData) =
        go (FifoState q' (rt-tWr)) rstR rEna rstW wDats1

      wCnt = sDepth q :- preWCnt
      full = (Seq.length q == maxDepth) :- preFull
      (q', over) =
        if Seq.length q < maxDepth
          then (case wDat of { Just x -> x Seq.<| q ; _ -> q }, False :- preOver)
          else (q, True :- preOver)

  sDepth = fromIntegral . Seq.length

  goRead (FifoState _ rt) (True :- rstRNext) (_ :- rEnas1) rstW wData =
    (wRstBusy, full, over, wCnt, True :- rRstBusy, fifoEmpty, under, rCnt, rData)
    where
      rData = deepErrorX "Reset" :- preRData
      fifoEmpty = True :- preEmpty
      rCnt = 0 :- preRCnt
      under = False :- preUnder

      (wRstBusy, full, over, wCnt, rRstBusy, preEmpty, preUnder, preRCnt, preRData) =
        go (FifoState mempty (rt+tR)) rstRNext rEnas1 rstW wData

  goRead (FifoState q rt) (_ :- rstRNext) (rEna :- rEnas1) rstW wData =
    (wRstBusy, full, over, wCnt, False :- rRstBusy, fifoEmpty, under, rCnt, rData)
    where
      rCnt = sDepth q :- preRCnt
      fifoEmpty = (Seq.length q == 0) :- preEmpty
      rData = nextData :- preRData

      (wRstBusy, full, over, wCnt, rRstBusy, preEmpty, preUnder, preRCnt, preRData) =
        go (FifoState q' (rt+tR)) rstRNext rEnas1 rstW wData

      (q', nextData, under) =
        if rEna
          then
            case Seq.viewr q of
              Seq.EmptyR -> (q, deepErrorX "FIFO empty", True :- preUnder)
              qData Seq.:> qDatum -> (qData, qDatum, False :- preUnder)
          else (q, deepErrorX "Enable off", deepErrorX "Enable off" :- preUnder)

  initState :: FifoState n
  initState = FifoState Seq.empty 0

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
