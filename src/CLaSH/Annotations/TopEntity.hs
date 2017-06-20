{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

The 'TopEntity' annotations described in this module make it easier to put your
design on an FPGA.

We can exert some control how the top level function is created by the CλaSH
compiler by annotating the @topEntity@ function with a 'TopEntity' annotation.
You apply these annotations using the @ANN@ pragma like so:

@
{\-\# ANN topEntity (TopEntity {t_name = ..., ...  }) \#-\}
topEntity x = ...
@

For example, given the following specification:

@
module Blinker where

import CLaSH.Prelude
import CLaSH.Intel.ClockGen

type Dom50 = Dom \"System\" 20000

topEntity
  :: Clock Dom50 Source
  -> Reset Dom50 Asynchronous
  -> Signal Dom50 Bit
  -> Signal Dom50 (BitVector 8)
topEntity clk rst key1 =
    let  (pllOut,pllStable) = 'CLaSH.Intel.ClockGen.altpll' (SSymbol @ "altpll50") clk rst
         rstSync            = 'CLaSH.Signal.resetSynchroniser' pllOut ('CLaSH.Signal.unsafeToAsyncReset' pllStable)
    in   'CLaSH.Signal.withClockReset' pllOut rstSync leds
  where
    key1R  = 'CLaSH.Prelude.isRising' 1 key1
    leds   = 'CLaSH.Prelude.mealy' blinkerT (1,False,0) key1R

blinkerT (leds,mode,cntr) key1R = ((leds',mode',cntr'),leds)
  where
    -- clock frequency = 50e6  (50 MHz)
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

The CλaSH compiler will normally generate the following @topEntity.vhdl@ file:

@
-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.blinker_types.all;

entity blinker_topentity is
  port(-- clock
       input_0  : in std_logic;
       -- asynchronous reset: active high
       input_1  : in std_logic;
       input_2  : in std_logic_vector(0 downto 0);
       output_0 : out std_logic_vector(7 downto 0));
end;

architecture structural of blinker_topentity is
begin
  blinker_topentity_0_inst : entity blinker_topentity_0
    port map
      (clk    => input_0
      ,rst    => input_1
      ,key1   => input_2
      ,result => output_0);
end;
@

However, if we add the following 'TopEntity' annotation in the file:

@
{\-\# ANN topEntity
  ('defTop'
    { t_name     = "blinker"
    , t_inputs   = [\"CLOCK_50\",\"KEY0\",\"KEY1\"]
    , t_outputs  = [\"LED\"]
    }) \#-\}
@

The CλaSH compiler will generate the following @blinker.vhdl@ file instead:

@
-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.blinker_types.all;

entity blinker is
  port(-- clock
       CLOCK_50 : in std_logic;
       -- asynchronous reset: active high
       KEY0     : in std_logic;
       KEY1     : in std_logic_vector(0 downto 0);
       LED      : out std_logic_vector(7 downto 0));
end;

architecture structural of blinker is
begin
  blinker_topentity_inst : entity blinker_topentity
    port map
      (clk    => CLOCK_50
      ,rst    => KEY0
      ,key1   => KEY1
      ,result => LED);
end;
@

Where we now have:

* A top-level component that is called @blinker@.
* Inputs and outputs that have a /user/-chosen name: @CLOCK_50@, @KEY0@, @KEY1@, @LED@, etc.

See the documentation of 'TopEntity' for the meaning of all its fields.
-}

{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module CLaSH.Annotations.TopEntity
  ( -- * Data types
    TopEntity (..)
  , PortName (..)
  , TestBench (..)
    -- * Convenience functions
  , defTop
  )
where

import qualified Language.Haskell.TH as TH
import           Data.Data

-- | TopEntity annotation
data TopEntity
  = TopEntity
  { t_name     :: String
  -- ^ The name the top-level component should have, put in a correspondingly
  -- named file.
  , t_inputs   :: [PortName]
  -- ^ List of names that are assigned in-order to the inputs of the component.
  , t_outputs  :: [PortName]
  -- ^ List of names that are assigned in-order to the outputs of the component.
  }
  deriving (Data,Show,Read)

data PortName
  = PortName String
  -- ^ You want a port, with the given name, for the entire argument\/type
  --
  -- The name can be left empty.
  | PortField String [PortName]
  -- ^ You want to assign ports to fields of an argument\/type
  --
  -- The first argument of 'Sub' is the name of:
  --
  --   * The signal/wire to which the individual ports are aggregated
  --   * The prefix for any unnamed ports below the 'Sub'
  --
  -- The name can be left empty.
  deriving (Data,Show,Read)

-- | Tell what binder is the 'TestBench' for a 'TopEntity' binder.
--
-- So in the following example, /f/ is the 'TopEntity', and /g/ is the
-- 'TestBench'
--
-- @
-- f :: Bool -> Bool
-- f = ...
-- {\-\# ANN f (defTop {t_name = "f"}) \#-\}
-- {\-\# ANN f (TestBench \'g) \#-\}
--
-- g :: Signal Bool
-- g = ...
-- @
data TestBench
  = TestBench TH.Name
  deriving (Data,Show)

-- | Default 'TopEntity' which has no clocks, and no specified names for the
-- input and output ports. Also has no clock sources:
--
-- >>> defTop
-- TopEntity {t_name = "topentity", t_inputs = [], t_outputs = []}
defTop :: TopEntity
defTop = TopEntity
  { t_name     = "topentity"
  , t_inputs   = []
  , t_outputs  = []
  }
