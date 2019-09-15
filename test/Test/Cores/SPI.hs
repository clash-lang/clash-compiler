module Test.Cores.SPI where

import Data.Maybe
import qualified Prelude as P

import Clash.Prelude
import qualified Clash.Explicit.Prelude as E
import Clash.Explicit.Testbench
import Clash.Sized.Internal.BitVector (undefined#)

import Clash.Cores.SPI
import Clash.Cores.LatticeSemi.IO

misoCapture
  :: forall n dom
   . (HiddenClockResetEnable dom, 1 <= n, KnownNat n)
  => SPIMode
  -> Signal dom (Bool,Bit,Bool)
  -- ^ Slave select, MISO, SCK
  -> Signal dom (Maybe (BitVector n))
misoCapture mode =
  moore go snd ((0 :: Index n,undefined,unpack undefined#),Nothing)
 where
  go ((cntQ,oldSckQ,datQ),_resQ) (ss,miso,sck) = ((cntD,sck,datD),resD)
   where
    cntD | ss = 0
         | captureSck = if cntQ == maxBound then 0 else cntQ + 1
         | otherwise  = cntQ

    datD | ss = unpack undefined#
         | captureSck = tail @(n-1) datQ :< miso
         | otherwise  = datQ

    resD | not ss && captureSck && cntQ == maxBound
         = Just (pack (tail datQ :< miso))
         | otherwise
         = Nothing

    risingSck  = not oldSckQ && sck
    fallingSck = oldSckQ && not sck
    captureSck = if mode == SPIMode0 || mode == SPIMode3
                 then risingSck else fallingSck

testMode :: SPIMode
testMode = SPIMode3

spiSlaveLattice
  :: forall dom n
   . (HiddenClockResetEnable dom, 1 <= n, KnownNat n)
  => BiSignalIn 'Floating dom 1
  -> Signal dom Bool
  -> Signal dom Bit
  -> Signal dom Bool
  -> Signal dom (BitVector n)
  -> (BiSignalOut 'Floating dom 1, Signal dom (Maybe (BitVector n)))
spiSlaveLattice =
  spiSlave (SPISlaveConfig testMode sbioX)
 where
  sbioX bin en dout = bout
   where
    (bout,_,_) = sbio 0b101001 bin (pure 0) dout (pure undefined) en

testSlave :: Signal System Bool
testSlave = done
 where
  testInput = stimuliGenerator clk rst mode1
  (ss,mosi,sck) = unbundle testInput
  din = pure (0b01100111 :: BitVector 8)
  (miso,dout) = exposeClockResetEnable spiSlaveLattice
                  clk rst enableGen
                  (veryUnsafeToBiSignalIn miso) ss mosi sck din

  misoC = exposeClockResetEnable (misoCapture @8 SPIMode0) clk rst enableGen
            (bundle (E.delay clk enableGen False ss
                    ,readFromBiSignal (veryUnsafeToBiSignalIn miso)
                    ,E.delay clk enableGen undefined sck))

  done = outputVerifier' clk rst mode1Exp
            (bundle (dout,misoC))
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen


masterInBP
  :: KnownDomain dom
  => BitVector 8
  -> Clock dom
  -> Reset dom
  -> Signal dom Bool
  -> Signal dom (Maybe (BitVector 8))
masterInBP val clk rst =
  E.moore clk rst enableGen
          (\_ i -> i)
          (\b -> if b then Nothing else Just val)
          True

testMasterSlave :: Signal System (Maybe (BitVector 8), Maybe (BitVector 8))
testMasterSlave = bundle (slaveOut,masterOut)
 where
  slaveIn = pure (0b01100101 :: BitVector 8)
  (misoZ,slaveOut) =
    exposeClockResetEnable spiSlaveLattice
      clk rst enableGen miso ss mosi sck slaveIn
  miso = veryUnsafeToBiSignalIn misoZ

  masterIn = masterInBP (0b01110111 :: BitVector 8) clk rst bp

  (masterOut,bp,ss,mosi,sck) =
    exposeClockResetEnable spiMaster
      clk rst enableGen testMode (readFromBiSignal miso) masterIn

  clk = systemClockGen
  rst = systemResetGen

mode0 :: Vec 21 (Bool, Bit, Bool)
mode0 = $(listToVecTH
            -- SS  , MOSI  , SCK
            [(True , 0::Bit, False)
            ,(True , 0     , False)
            ,(False, 1     , False)
            -- 1
            ,(False, 1     , False)
            ,(False, 1     , True )
            -- 0
            ,(False, 0     , False)
            ,(False, 0     , True )
            -- 0
            ,(False, 0     , False)
            ,(False, 0     , True )
            -- 0
            ,(False, 0     , False)
            ,(False, 0     , True )
            -- 0
            ,(False, 0     , False)
            ,(False, 0     , True )
            -- 1
            ,(False, 1     , False)
            ,(False, 1     , True )
            -- 1
            ,(False, 1     , False)
            ,(False, 1     , True )
            -- 0
            ,(False, 0     , False)
            ,(False, 0     , True )
            -- disable
            ,(False, 0     , False)
            ,(True , 0     , False)
            ]
         )

mode0Exp :: Vec 24 (Maybe (BitVector 8), Maybe (BitVector 8))
mode0Exp = $(listToVecTH (
                -- DOUT, MISOC
                [(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                -- 0
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                -- 0
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                -- 1
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                -- 1
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                -- 0
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                -- 0
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                -- 1
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                -- 1
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                -- 1
                ,(Nothing        , Nothing)
                ,(Just 0b10000110, Nothing)
                -- Finish
                ,(Nothing        , Just 0b01100111)
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                ] :: [(Maybe (BitVector 8), Maybe (BitVector 8))])
             )

mode1 :: Vec 21 (Bool, Bit, Bool)
mode1 = $(listToVecTH
            -- SS  , MOSI  , SCK
            [(True , 0::Bit, False)
            ,(True , 0     , False)
            ,(False, 1     , False)
            -- 1
            ,(False, 1     , True)
            ,(False, 1     , False )
            -- 0
            ,(False, 0     , True)
            ,(False, 0     , False )
            -- 0
            ,(False, 0     , True)
            ,(False, 0     , False )
            -- 0
            ,(False, 0     , True)
            ,(False, 0     , False )
            -- 0
            ,(False, 0     , True)
            ,(False, 0     , False )
            -- 1
            ,(False, 1     , True)
            ,(False, 1     , False )
            -- 1
            ,(False, 1     , True)
            ,(False, 1     , False )
            -- 0
            ,(False, 0     , True)
            ,(False, 0     , False )
            -- disable
            ,(False, 0     , False)
            ,(True , 0     , False)
            ]
         )

mode1Exp :: Vec 22 (Maybe (BitVector 8), Maybe (BitVector 8))
mode1Exp = $(listToVecTH (
                -- DOUT, MISOC
                [(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                -- 0
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                -- 0
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                -- 1
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                -- 1
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                -- 0
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                -- 0
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                -- 1
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                -- 1
                ,(Nothing        , Nothing)
                ,(Nothing        , Nothing)
                -- 1
                ,(Nothing        , Just 0b01100111)
                ,(Just 0b10000110, Nothing)
                -- Finish
                ,(Nothing        , Nothing)
                ] :: [(Maybe (BitVector 8), Maybe (BitVector 8))])
             )
