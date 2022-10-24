module Basic where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Sized.Internal.BitVector (undefined#)
import Clash.Cores.Xilinx.DcFifo

-- Configurables
type Overfill = 4
type DepthParam = 4
type ActualDepth = 15
-- End of configurables

type TotalElems = ActualDepth + Overfill
type Elem = Index TotalElems

data FSM
  = Push (Index TotalElems)
  | StartRead
  | Pop (Index TotalElems)
  | Done
  deriving (Show, Generic, NFDataX)

topEntity ::
  Clock XilinxSystem ->
  Reset XilinxSystem ->
  Signal XilinxSystem (Maybe Elem) ->
  Signal XilinxSystem Bool ->
  ( FifoOut XilinxSystem XilinxSystem DepthParam Elem
  , FifoOut XilinxSystem XilinxSystem DepthParam Elem
  )
topEntity clk rst writeData rEnable =
  ( dcFifo minOpt clk rst clk rst writeData rEnable
  , dcFifo maxOpt clk rst clk rst writeData rEnable
  )
 where
  minOpt = DcConfig
    { dcDepth=SNat
    , dcReadDataCount=False
    , dcWriteDataCount=False
    , dcOverflow=False
    , dcUnderflow=False
    }
  maxOpt = DcConfig
    { dcDepth=SNat
    , dcReadDataCount=True
    , dcWriteDataCount=True
    , dcOverflow=True
    , dcUnderflow=True
    }
{-# NOINLINE topEntity #-}

testBench ::
  Signal XilinxSystem Bool
testBench = done
 where
  fsmOut = let (s', o) = unbundle $ fsm <$> register clk noRst en (Push 0) s'
           in o
  (minOut, maxOut) =
    topEntity clk noRst (fWriteData <$> fsmOut) (fREnable <$> fsmOut)
  done =
      register clk noRst en False
    $ assertBitVector clk noRst "FIFO min full"
        (pack <$> isFull minOut) (fExpectedFull <$> fsmOut)
    $ assertBitVector clk noRst "FIFO max full"
        (pack <$> isFull maxOut) (fExpectedFull <$> fsmOut)
    $ assertBitVector clk noRst "FIFO max overflow"
        (pack <$> isOverflow maxOut) (fExpectedOverflow <$> fsmOut)
    $ assertBitVector clk noRst "FIFO min empty"
        (pack <$> isEmpty minOut) (fExpectedEmpty <$> fsmOut)
    $ assertBitVector clk noRst "FIFO max empty"
        (pack <$> isEmpty maxOut) (fExpectedEmpty <$> fsmOut)
    $ assertBitVector clk noRst "FIFO max underflow"
        (pack <$> isUnderflow maxOut) (fExpectedUnderflow <$> fsmOut)
    $ assertBitVector clk noRst "FIFO min data out"
        (pack <$> fifoData minOut) (fExpectedData <$> fsmOut)
    $ assertBitVector clk noRst "FIFO max data out"
        (pack <$> fifoData maxOut) (fExpectedData <$> fsmOut)
        (fDone <$> fsmOut)
  clk = tbClockGen (not <$> done)
  noRst = unsafeFromHighPolarity $ pure False
  en = enableGen
{-# NOINLINE testBench #-}

data FsmOut = FsmOut
  { fDone :: Bool
  , fWriteData :: Maybe Elem
  , fREnable :: Bool
  , fExpectedFull :: BitVector 1
  , fExpectedOverflow :: BitVector 1
  , fExpectedEmpty :: BitVector 1
  , fExpectedUnderflow :: BitVector 1
  , fExpectedData :: BitVector (BitSize Elem)
  }

defFsmOut :: FsmOut
defFsmOut =
  FsmOut{ fDone=False
        , fWriteData=Nothing
        , fREnable=False
        , fExpectedFull=undefined#
        , -- Assert overflow false by default
          fExpectedOverflow=pack False
        , fExpectedEmpty=undefined#
        , -- Assert underflow false by default
          fExpectedUnderflow=pack False
        , fExpectedData=undefined#
        }

fsm ::
  FSM ->
  (FSM, FsmOut)
fsm (Push i) =
  let s' = if (i == maxBound) then StartRead else Push (i + 1)
      o = defFsmOut{ fWriteData=Just i
                   , fExpectedFull=pack (i >= actualDepth)
                   , fExpectedOverflow=pack (i > actualDepth)
                   }
  in (s', o)
fsm StartRead = (Pop 0, defFsmOut{ fREnable=True
                                 , fExpectedOverflow=pack True
                                 })
fsm (Pop i) =
  let isLast = i == maxBound
      s' = if isLast then Done else Pop (i + 1)
      underflow = i >= actualDepth
      o = defFsmOut{ fREnable=not isLast
                   , fExpectedEmpty=pack (i >= actualDepth - 1)
                   , fExpectedUnderflow=pack underflow
                   , fExpectedData=if underflow then undefined#
                                   else pack (resize i)
                   }
  in (s', o)
fsm Done = (Done, defFsmOut{fDone=True, fExpectedEmpty=pack True})

actualDepth :: Index TotalElems
actualDepth = natToNum @ActualDepth
