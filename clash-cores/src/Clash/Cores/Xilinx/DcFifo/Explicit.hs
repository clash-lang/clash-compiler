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

type ResetBusy = Bit
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
  } deriving Show

data XilinxFifo read write depth n =
  XilinxFifo
    { writeReset :: Signal write ResetBusy
    , isFull :: Signal write Full
    , writeCount :: Signal write (DataCount depth)
    , readReset :: Signal read ResetBusy
    , isEmpty :: Signal read Empty
    , readCount :: Signal read (DataCount depth)
    , fifoData :: Signal read (BitVector n)
    }

-- | Mock Xilinx FIFO, see [documentation](https://docs.xilinx.com/v/u/en-US/pg057-fifo-generator)
-- for expected behavior.
--
-- If 'dcReadDataCount' or 'dcWriteDataCount' are disabled, the relevant signals
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

  Clock write -> Clock read ->
  -- | Asynchronous reset
  Reset read ->

  -- | Write data
  Signal write (BitVector n) ->
  -- | Write enable
  Signal write Bool ->
  -- | Read enable
  Signal read Bool ->
  XilinxFifo read write depth n
dcFifo dcCfg wClk rClk rst writeData wEnable rEnable =
  let (wRst, f, wCnt, rRst, e, rCnt, rData) = dcFifo# dcCfg wClk rClk rst writeData wEnable rEnable
    in XilinxFifo wRst f wCnt rRst e rCnt rData

dcFifo# ::
  forall depth n write read .
  ( KnownNat n
  , KnownDomain write
  , KnownDomain read

  , KnownNat depth
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
  ( Signal write ResetBusy
  , Signal write Full
  , Signal write (DataCount depth)

  , Signal read ResetBusy
  , Signal read Empty
  , Signal read (DataCount depth)
  , Signal read (BitVector n)
  )
dcFifo# DcConfig{..} wClk rClk rst writeData wEnable rEnable =
  let
    (wRstBusy, wFull, wCnt, rRstBusy, rEmpty, rCnt, rData) =
      go initState rstSignalR rEnable rstSignalW wEnable writeData
  in
    ( wRstBusy
    , wFull
    , if dcWriteDataCount then wCnt else deepErrorX "Write data count disabled"
    , rRstBusy
    , rEmpty
    , if dcReadDataCount then rCnt else deepErrorX "Read data count disabled"
    , deepErrorX "No sample" :- rData
    )
 where
  rstSignalR = unsafeToHighPolarity rst
  rstSignalW = unsafeSynchronizer rClk wClk $ unsafeToHighPolarity rst

  -- reified depth
  rD = snatToNum @Int (powSNat (SNat @2) (SNat @depth))

  -- TODO: how do we ensure it doesn't "space leak" in Haskell?
  --
  -- force slower first?
  -- https://github.com/clash-lang/clash-compiler/blob/ea114d8edd6a110f72d148203b9db2454cae8f37/clash-prelude/src/Clash/Explicit/BlockRam.hs#L1276-L1280

  go ::
    FifoState n ->
    Signal read Bool -> -- reset
    Signal read Bool -> -- read enabled
    Signal write Bool -> -- reset
    Signal write Bool -> -- write enable
    Signal write (BitVector n) -> -- write data
    ( Signal write ResetBusy
    , Signal write Full
    , Signal write (DataCount depth)

    , Signal read ResetBusy
    , Signal read Empty
    , Signal read (DataCount depth)
    , Signal read (BitVector n)
    )
  go st@(FifoState _ rt) rstR rEna rstW =
    if rt < tWr
      then goRead st rstR rEna rstW
      else goWrite st rstR rEna rstW
    -- TODO: goBoth case?

  goWrite (FifoState _ rt) rstR rEna (True :- rstWNext) (_ :- wEna) (_ :- wData) =
      (1 :- wRstBusy, False :- preFull, 0 :- preWCnt, rRstBusy, fifoEmpty, rCnt, rData)
    where
      (wRstBusy, preFull, preWCnt, rRstBusy, fifoEmpty, rCnt, rData) =
        go (FifoState mempty (rt-tWr)) rstR rEna rstWNext wEna wData

  goWrite (FifoState q rt) rstR rEna (_ :- rstW) wEnas0 wDats0 =
    (0 :- wRstBusy, full, wCnt, rRstBusy, fifoEmpty, rCnt, rData)
    where
      (wRstBusy, preFull, preWCnt, rRstBusy, fifoEmpty, rCnt, rData) =
        go (FifoState q' (rt-tWr)) rstR rEna rstW wEnas1 wDats1

      wCnt = sDepth q :- preWCnt
      full = (Seq.length q == rD) :- preFull
      (wDat :- wDats1) = wDats0
      (wEna :- wEnas1) = wEnas0
      q' =
        if Seq.length q + 1 <= rD && wEna
          then wDat Seq.<| q
          else q

  sDepth = fromIntegral . Seq.length

  goRead (FifoState q rt) (rstR :- rstRNext) rEnas0 rstW wEna wData =
    (wRstBusy, full, wCnt, (if rstR then 1 else 0) :- rRstBusy, fifoEmpty, rCnt, rData)
    where
      rCnt = sDepth q :- preRCnt
      fifoEmpty = (Seq.length q == 0) :- preEmpty
      rData = nextData :- preRData

      (wRstBusy, full, wCnt, rRstBusy, preEmpty, preRCnt, preRData) =
        go (FifoState q' (rt+tR)) rstRNext rEnas1 rstW wEna wData

      (rEna :- rEnas1) = rEnas0
      (q', nextData) =
        if rEna && not rstR
          then
            case Seq.viewr q of
              Seq.EmptyR -> (q, deepErrorX "FIFO empty")
              qData Seq.:> qDatum -> (qData, qDatum)
          else (q, deepErrorX "Enable off or resetting")

  initState :: FifoState n
  initState = FifoState Seq.empty 0

  tWr = snatToNum @Int (clockPeriod @write)
  tR = snatToNum @Int (clockPeriod @read)

{-# NOINLINE dcFifo# #-}
{-# ANN dcFifo# (InlineYamlPrimitive [minBound..maxBound] [i|
BlackBoxHaskell:
    name: Clash.Cores.Xilinx.DcFifo.Explicit.dcFifo#
    templateFunction: Clash.Cores.Xilinx.DcFifo.BlackBoxes.dcFifoBBF
    workInfo: Always
    #    multiResult: true
    includes:
    - name: fifo
      extension: tcl
      format: Haskell
      templateFunction: Clash.Cores.Xilinx.DcFifo.BlackBoxes.dcFifoTclTF
|]) #-}
