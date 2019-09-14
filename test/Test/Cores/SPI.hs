module Test.Cores.SPI where

import Clash.Prelude
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
  testInput = stimuliGenerator clk rst
                $(listToVecTH
                    -- SS  , MOSI  , SCK
                    [(True , 0::Bit, False)
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
  (ss,mosi,sck) = unbundle testInput
  din = pure (0b01100111 :: BitVector 8)
  dutOutput = exposeClockResetEnable (spiSlave @8 (SPISlaveConfig SPIMode0))
                clk rst enableGen
                ss mosi sck din

  (miso,dout) = unbundle dutOutput
  misoC = exposeClockResetEnable (misoCapture @8 SPIMode1) clk rst enableGen
            (bundle (ss,miso,sck))

  done = outputVerifier' clk rst
            $(listToVecTH (
                -- DOUT, MISOC
                [(Nothing        , Nothing)
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
                ,(Just 0b10000110, Just 0b01100111)
                -- Finish
                ,(Nothing        , Nothing)
                ] :: [(Maybe (BitVector 8), Maybe (BitVector 8))])
             )
            (bundle (dout,misoC))
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
