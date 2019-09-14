module Test.Cores.SPI where

import Clash.Prelude
import Clash.Explicit.Testbench
import Clash.Sized.Internal.BitVector (undefined#)

import Clash.Cores.SPI

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
  dutOutput = exposeClockResetEnable (spiSlave @8) clk rst enableGen
                ss mosi sck din
  done = outputVerifier' clk rst
            $(listToVecTH
                -- MISO , DONE , DOUT
                [(1::Bit, False, 0 :: BitVector 8)
                -- 0
                ,(unpack undefined#, False, 0)
                ,(0     , False, 0)
                -- 0
                ,(0     , False, 0)
                ,(0     , False, 0)
                -- 1
                ,(1     , False, 0)
                ,(1     , False, 0)
                -- 1
                ,(1     , False, 0)
                ,(1     , False, 0)
                -- 0
                ,(0     , False, 0)
                ,(0     , False, 0)
                -- 0
                ,(0     , False, 0)
                ,(0     , False, 0)
                -- 1
                ,(1     , False, 0)
                ,(1     , False, 0)
                -- 1
                ,(1     , False, 0)
                ,(1     , False, 0)
                -- 1
                ,(1     , False, 0)
                ,(1     , True , 0b10000110)
                -- Finish
                ,(0     , False, 0b10000110)
                ]
             )
            dutOutput
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
