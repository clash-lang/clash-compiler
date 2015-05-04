{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}

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
    , t_clocks   = [ defClkAltera "CLOCK_50(0)" "not KEY0(0)"
                   ]
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
import CLaSH.Signal.Explicit (SClock, systemClock)

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
  , c_outp  :: [(String,AClk)]       -- ^ [(output port,clock signal)]
  , c_reset :: Maybe (String,String) -- ^ optional: Asynchronous reset input
  , c_lock  :: String                -- ^ Port name that indicates clock is stable
  , c_sync  :: Bool                  -- ^ optional: devices connected this clock
                                     -- source should be pulled out of reset in-sync
  }
  deriving (Data,Show)

data AClk = forall clk . AClk (SClock clk)

instance Eq AClk where
  (AClk sclk1) == (AClk sclk2) = show sclk1 == show sclk2

instance Show AClk where
  show (AClk sclk) = show sclk

instance Data AClk where
  gfoldl _ z (AClk clk) = z (AClk clk)
  toConstr (AClk _)     = aclkConstr
  gunfold _ _ _         = error "Data.Data.gunfold(AClk)"
  dataTypeOf _          = aclkDataType

aclkConstr :: Constr
aclkConstr = mkConstr aclkDataType "AClk" [] Prefix

aclkDataType :: DataType
aclkDataType = mkDataType "CLaSH.Annotations.TopEntity.AClk" [aclkConstr]

defTop :: TopEntity
defTop = TopEntity
  { t_name     = "topentity"
  , t_inputs   = []
  , t_outputs  = []
  , t_extraIn  = []
  , t_extraOut = []
  , t_clocks   = []
  }

defClkAltera :: String -> String -> ClockSource
defClkAltera clkExpr resExpr = ClockSource
  { c_name  = "systempll"
  , c_inp   = Just ("inclk0",clkExpr)
  , c_outp  = [("c0",AClk systemClock)]
  , c_reset = Just ("areset",resExpr)
  , c_lock  = "locked"
  , c_sync  = False
  }

defClkXilinx :: String -> String -> ClockSource
defClkXilinx clkExpr resExpr = ClockSource
  { c_name  = "systempll"
  , c_inp   = Just ("CLK_IN1",clkExpr)
  , c_outp  = [("CLK_OUT1",AClk systemClock)]
  , c_reset = Just ("RESET",resExpr)
  , c_lock  = "LOCKED"
  , c_sync  = False
  }
