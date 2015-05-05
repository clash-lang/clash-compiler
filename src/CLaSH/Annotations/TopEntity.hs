{-# LANGUAGE DeriveDataTypeable #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

@
{\-\# ANN topEntity
  ('defTop'
    { t_name     = "blinker"
    , t_inputs   = [\"KEY1\"]
    , t_outputs  = [\"LED\"]
    , t_extraIn  = [ (\"CLOCK_50\", 1)
                   , (\"KEY0\"    , 1)
                   ]
    , t_clocks   = [ 'defClkAltera' "altpll50" "CLOCK_50(0)" "not KEY0(0)" ]
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

Generates the following @blinker.vhdl@ file:

@
-- Automatically generated VHDL
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use work.all;
use work.types.all;

entity blinker is
  port(KEY1     : in std_logic_vector(0 downto 0);
       CLOCK_50 : in std_logic_vector(0 downto 0);
       KEY0     : in std_logic_vector(0 downto 0);
       LED      : out std_logic_vector(7 downto 0));
end;

architecture structural of blinker is
  signal system1000      : std_logic;
  signal system1000_rstn : std_logic;
  signal altpll50_locked : std_logic;
begin
  altpll50_inst : entity altpll50
    port map
      (inclk0 => CLOCK_50(0)
      ,c0     => system1000
      ,areset => not KEY0(0)
      ,locked => altpll50_locked);

  -- reset system1000_rstn is asynchronously asserted, but synchronously de-asserted
  resetSync_n_0 : block
    signal n_1 : std_logic;
    signal n_2 : std_logic;
  begin
    process(system1000,altpll50_locked)
    begin
      if altpll50_locked = '0' then
        n_1 <= '0';
        n_2 <= '0';
      elsif rising_edge(system1000) then
        n_1 <= '1';
        n_2 <= n_1;
      end if;
    end process;

    system1000_rstn <= n_2;
  end block;

  topEntity_0_inst : entity topEntity_0
    port map
      (key1_i1         => KEY1
      ,system1000      => system1000
      ,system1000_rstn => system1000_rstn
      ,topLet_o        => LED);
end;
@

-}
module CLaSH.Annotations.TopEntity
  ( -- * Data types
    TopEntity (..)
  , ClockSource (..)
    -- * Convenience functions
  , defTop
  , defClkAltera
  , defClkXilinx
  )
where

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
  , c_outp  :: [(String,String)]     -- ^ \[(output port,clock signal)\]
  , c_reset :: Maybe (String,String) -- ^ optional: Asynchronous reset input
  , c_lock  :: String                -- ^ Port name that indicates clock is stable
  , c_sync  :: Bool                  -- ^ optional: devices connected this clock
                                     -- source should be pulled out of reset in-sync
  }
  deriving (Data,Show)

-- | Default 'TopEntity' which has no clocks, and no specified names for the
-- input and output ports. Also has no clock sources:
--
-- >>> defTop
-- TopEntity {t_name = "topentity", t_inputs = [], t_outputs = [], t_extraIn = [], t_extraOut = [], t_clocks = []}
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
--
-- >>> defClkAltera "altpll50" "CLOCK(0)" "not KEY(0)"
-- ClockSource {c_name = "altpll50", c_inp = Just ("inclk0","CLOCK(0)"), c_outp = [("c0","system1000")], c_reset = Just ("areset","not KEY(0)"), c_lock = "locked", c_sync = False}
--
-- Will generate the following VHDL:
--
-- > altpll50_inst : entity altpll50
-- >   port map
-- >     (inclk0 => CLOCK_50(0)
-- >     ,c0     => system1000
-- >     ,areset => not KEY0(0)
-- >     ,locked => altpll50_locked);
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
--
-- >>> defClkXilinx "clkwiz50" "CLOCK(0)" "not KEY(0)"
-- ClockSource {c_name = "clkwiz50", c_inp = Just ("CLK_IN1","CLOCK(0)"), c_outp = [("CLK_OUT1","system1000")], c_reset = Just ("RESET","not KEY(0)"), c_lock = "LOCKED", c_sync = False}
--
-- Will generate the following VHDL:
--
-- > clkwiz50_inst : entity clkwiz50
-- >   port map
-- >     (CLK_IN1  => CLOCK_50(0)
-- >     ,CLK_OUT1 => system1000
-- >     ,RESET    => not KEY0(0)
-- >     ,LOCKED   => altpll50_locked);
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
