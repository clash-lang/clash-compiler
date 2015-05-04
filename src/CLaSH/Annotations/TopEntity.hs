{-# LANGUAGE DeriveDataTypeable #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

@
{\-\# ANN topEntity
  (defTop
    { t_name     = "blinker"
    , t_inputs   = [\"KEY1\"]
    , t_outputs  = [\"LED\"]
    , t_extraIn  = [ (\"CLOCK_50\", 1)
                   , (\"KEY0\"    , 1)
                   ]
    , t_clocks   = [ defClkAltera "altpll50" "CLOCK_50(0)" "not KEY0(0)" ]
    }) \#-\}
topEntity :: Signal Bit -> Signal (BitVector 8)
topEntity key1 = leds
  where
    key1R = isRising 1 key1
    leds  = mealy blinkerT (1,False,0) key1R

blinkerT (leds,mode,cntr) key1R = ((leds',mode',cntr'),leds)
  where
    -- clock frequency = 50e6   (50 MHz)
    -- led update rate = 333e-3 (every 333ms)
    cnt_max = 16650000 -- 50e6 * 333e-3

    cntr' | cntr == cnt_max = 0
          | otherwise       = cntr + 1

    mode' | key1R     = not mode
          | otherwise = mode

    leds' | cntr == 0 = if mode then complement leds
                                else rotateL leds 1
          | otherwise = leds
@

-}
module CLaSH.Annotations.TopEntity where

import Data.Data
import CLaSH.Signal.Explicit (systemClock)

-- | TopEntity specifications, fields are self-explanatory
data TopEntity
  = TopEntity
  { t_name     :: String
  , t_inputs   :: [String]       -- optional
  , t_outputs  :: [String]       -- optional
  , t_extraIn  :: [(String,Int)] -- optional
  , t_extraOut :: [(String,Int)] -- optional
  , t_clocks   :: [ClockSource]  -- optional
  }
  deriving (Data,Show)

-- | A clock source
data ClockSource
  = ClockSource
  { c_name  :: String                -- ^ Component name
  , c_inp   :: Maybe (String,String) -- ^ optional: (Input port, clock pin)
  , c_outp  :: [(String,String)]     -- ^ [(output port,clock signal)]
  , c_reset :: Maybe (String,String) -- ^ optional: Asynchronous reset input
  , c_lock  :: String                -- ^ Port name that indicates clock is stable
  , c_sync  :: Bool                  -- ^ optional: devices connected this clock
                                     -- source should be pulled out of reset in-sync
  }
  deriving (Data,Show)

-- | Default 'TopEntity' which has no clocks, and no specified names for the
-- input and output ports. Also has no clock sources.
defTop :: TopEntity
defTop = TopEntity
  { t_name     = "topentity"
  , t_inputs   = []
  , t_outputs  = []
  , t_extraIn  = []
  , t_extraOut = []
  , t_clocks   = []
  }

-- | A clock source that corresponds to the Altera PLL component with default
-- settings connected to the the system clock.
defClkAltera :: String -- ^ Name of the component
             -> String -- ^ Clock Pin/Expression
             -> String -- ^ Reset Pin/Expression
             -> ClockSource
defClkAltera pllName clkExpr resExpr = ClockSource
  { c_name  = pllName
  , c_inp   = Just ("inclk0",clkExpr)
  , c_outp  = [("c0",show systemClock)]
  , c_reset = Just ("areset",resExpr)
  , c_lock  = "locked"
  , c_sync  = False
  }

-- | A clock source that corresponds to the Xilinx PLL/MMCM component with
-- default settings connected to the the system clock.
defClkXilinx :: String -- ^ Name of the component
             -> String -- ^ Clock Pin/Expression
             -> String -- ^ Reset Pin/Expression
             -> ClockSource
defClkXilinx pllName clkExpr resExpr = ClockSource
  { c_name  = pllName
  , c_inp   = Just ("CLK_IN1",clkExpr)
  , c_outp  = [("CLK_OUT1",show systemClock)]
  , c_reset = Just ("RESET",resExpr)
  , c_lock  = "LOCKED"
  , c_sync  = False
  }
