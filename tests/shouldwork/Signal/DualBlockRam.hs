{-# OPTIONS_GHC -Wno-orphans #-}

module DualBlockRam where

import qualified Prelude as P

import Clash.Explicit.Prelude
import Clash.Explicit.BlockRam
import Clash.Explicit.Testbench
import Data.Bifunctor (bimap)

import DualBlockRamDefinitions

test1a :: Signal A Bool
test1a = done
  where
    -- NOTE: replace 'id' with 'fst' or 'snd' in this definitions to test either
    --       the left or right stream.

    writesA = $(listToVecTH (P.map (Just . This) [1..7]))
    addrsA = $(listToVecTH (P.take 5 (P.repeat (1 :: Addr))))

    writesB = $(listToVecTH (P.map (Just . That) [8..12]))
    addrsB = $(listToVecTH [10,9,8,7,6,5,4,3,2 :: Addr])

    inputWritesA = stimuliGenerator clkA rstA writesA
    inputAddrsA = stimuliGenerator clkA rstA addrsA

    inputWritesB = stimuliGenerator clkB rstB writesB
    inputAddrsB = stimuliGenerator clkB rstB addrsB

    expectedOutput =
      $(listToVecTH
          (P.map
            (id . bimap This That)
            [ (0, 0)
            , (0, 0) -- 1 on the left side gets swallowed by 'ignoreFor'
            , (2, 8)
            , (3, 8)
            , (4, 9)
            , (5, 9)
            , (6, 10) ]))

    (as, bs) = simple clkA inputWritesA inputAddrsA clkB inputWritesB inputAddrsB
    actualOutput0 = fmap id (bundle (as, unsafeSynchronizer clkB clkA bs))
    actualOutput1 = ignoreFor clkA rstA enableGen d2 (id (This 0, That 0)) actualOutput0

    done  = outputVerifier clkA rstA expectedOutput actualOutput1
    done' = not <$> done

    clkA :: Clock A
    clkA = tbClockGen @A done'
    clkB :: Clock B
    clkB = tbClockGen @B (unsafeSynchronizer clkA clkB done')

    rstA :: Reset A
    rstA = resetGen @A
    rstB :: Reset B
    rstB = resetGen @B
