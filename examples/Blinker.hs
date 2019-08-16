module Blinker where

import Clash.Prelude
import Clash.Intel.ClockGen

data LedMode
  = Rotate
  -- ^ After some period, rotate active led to the left
  | Complement
  -- ^ After some period, turn on all disable LEDs, and vice versa
  deriving (Generic, NFDataX)

-- Define a synthesis domain with a clock with a period of 20000 /ps/.
createDomain vSystem{vName="Input", vPeriod=20000}

-- Define a synthesis domain with a clock with a period of 50000 /ps/.
createDomain vSystem{vName="Dom50", vPeriod=50000}

{-# ANN topEntity
  (Synthesize
    { t_name   = "blinker"
    , t_inputs = [ PortName "CLOCK_50"
                 , PortName "KEY0"
                 , PortName "KEY1"
                 ]
    , t_output = PortName "LED"
    }) #-}
topEntity
  :: Clock Input
  -- ^ Incoming clock
  -> Signal Input Bool
  -- ^ Reset signal, straight from KEY0
  -> Signal Dom50 Bit
  -- ^ Mode choice, straight from KEY1. See 'LedMode'.
  -> Signal Dom50 (BitVector 8)
  -- ^ Output containing 8 bits, corresponding to 8 LEDs
topEntity clk20 rstBtn modeBtn =
  exposeClockResetEnable
    (mealy blinkerT initialStateBlinkerT . isRising 1)
    clk50
    rstSync
    en
    modeBtn
 where
  -- | Enable line for subcomponents: we'll keep it always running
  en = enableGen

  -- Start with the first LED turned on, in rotate mode, with the counter on zero
  initialStateBlinkerT = (1, Rotate, 0)

  -- Signal coming from the reset button is low when pressed, and high when
  -- not pressed. We convert this signal to the polarity of our domain with
  -- 'unsafeFromActiveLow'.
  rst = unsafeFromLowPolarity rstBtn

  -- Instantiate a PLL: this stabilizes the incoming clock signal and indicates
  -- when the signal is stable. We're also using it to transform an incoming
  -- clock signal running at 20 MHz to a clock signal running at 50 MHz.
  (clk50, pllStable) =
    altpll
      @Dom50
      (SSymbol @"altpll50")
      clk20
      rst

  -- Synchronize reset to clock signal coming from PLL. We want the reset to
  -- remain active while the PLL is NOT stable, hence the conversion with
  -- 'unsafeFromActiveLow'
  rstSync =
    resetSynchronizer
      clk50
      (unsafeFromLowPolarity pllStable)
      en

flipMode :: LedMode -> LedMode
flipMode Rotate = Complement
flipMode Complement = Rotate

blinkerT
  :: (BitVector 8, LedMode, SatIndex 'SatError 16650001)
  -> Bool
  -> ((BitVector 8, LedMode, SatIndex 'SatError 16650001), BitVector 8)
blinkerT (leds, mode, cntr) key1R = ((leds', mode', cntr'), leds)
  where
    -- clock frequency = 50e6  (50 MHz)
    -- led update rate = 333e-3 (every 333ms)
    cnt_max = 16650000 :: SatIndex 'SatError 16650001 -- 50e6 * 333e-3

    cntr' | cntr == cnt_max = 0
          | otherwise       = cntr + 1

    mode' | key1R     = flipMode mode
          | otherwise = mode

    leds' | cntr == 0 =
              case mode of
                Rotate -> rotateL leds 1
                Complement -> complement leds
          | otherwise = leds
