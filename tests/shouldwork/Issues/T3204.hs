module T3204 where

import Clash.Prelude

topEntity :: Clock System -> Reset System -> Signal System Bool
topEntity clk rst = withClockResetEnable clk rst enableGen (maybe False id <$> snd foo)

foo :: HiddenClockResetEnable dom => ((), Signal dom (Maybe Bool))
foo = ((), Just <$> register False (pure False))
