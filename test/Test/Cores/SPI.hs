module Test.Cores.SPI where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as E
import Clash.Explicit.Testbench
import Clash.Sized.Internal.BitVector (undefined#)

import Clash.Cores.SPI

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
  go ((cntQ,oldSckQ,datQ),resQ) (ss,miso,sck) = ((cntD,sck,datD),resD)
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

test = done
 where
  testInput = stimuliGenerator clk rst mode0
  (ss,mosi,sck) = unbundle testInput
  din = pure (0b01100111 :: BitVector 8)
  dutOutput = exposeClockResetEnable (spiSlave @8 (SPISlaveConfig SPIMode0))
                clk rst enableGen
                ss mosi sck din

  (miso,dout) = unbundle dutOutput
  misoC = exposeClockResetEnable (misoCapture @8 SPIMode1) clk rst enableGen
            (bundle (E.delay clk enableGen False ss
                    ,miso
                    ,E.delay clk enableGen undefined sck))

  done = outputVerifier' clk rst mode0Exp
            (bundle (dout,misoC))
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen

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
                ] :: [(Maybe (BitVector 8), Maybe (BitVector 8))])
             )

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
