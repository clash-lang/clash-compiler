-- source: https://github.com/clash-lang/clash-compiler/issues/251

module PipelinesViaFolds where

import Clash.Prelude
import Data.Word

topEntity :: SystemClockResetEnable => Signal System Word32 -> Signal System Word32
topEntity = pipeline
  where
    pipeline :: Signal System Word32 -> Signal System Word32
    -- slow normalisation, 32s
    -- pipeline = foldr (\x acc -> stage x . acc) id $ iterate d32 (+1) 1

    -- slow normalisation, killed after 60s
    pipeline = foldr (.) id $ map stage $ iterate d32 (+1) 1

    -- fast normalisation, 1s
    -- pipeline =
    --     stage 1 .  stage 2 .  stage 3 .  stage 4 .  stage 5 .  stage 6 .
    --     stage 7 .  stage 8 .  stage 9 .  stage 10 .  stage 11 .  stage 12 .
    --     stage 13 .  stage 14 .  stage 15 .  stage 16 .  stage 17 .  stage 18 .
    --     stage 19 .  stage 20 .  stage 21 .  stage 22 .  stage 23 .  stage 24 .
    --     stage 25 .  stage 26 .  stage 27 .  stage 28 .  stage 29 .  stage 30 .
    --     stage 31 .  stage 32

    -- Christiaan's workaround
    -- pipeline i = last stages
    --   where
    --     stages = zipWith stage (iterate d32 (subtract 1) 32) (i :> init stages)

    -- Simple example where `stage` is configured by its order in the pipeline
    stage :: Word32 -> Signal System Word32 -> Signal System Word32
    stage i s = let delay = register i $ xor <$> delay <*> s
                in delay
