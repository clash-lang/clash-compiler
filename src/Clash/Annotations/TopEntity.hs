{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

'TopEntity' annotations allow us to control hierarchy and naming aspects of the
CλaSH compiler, specifically, they allow us to:

    * Assign names to entities (VHDL) \/ modules ((System)Verilog), and their
      ports.
    * Put generated HDL files of a logical (sub)entity in their own directory.
    * Use cached versions of generated HDL, i.e., prevent recompilation of
      (sub)entities that have not changed since the last run. Caching is based
      on a @.manifest@ which is generated alongside the HDL; deleting this file
      means deleting the cache; changing this file will result in /undefined/
      behaviour.

Functions with a 'TopEntity' annotation do must adhere to the following
restrictions:

    * Although functions with a 'TopEntity' annotation can of course depend
      on functions with another 'TopEntity' annotation, they must not be
      mutually recursive.
    * Functions with a 'TopEntity' annotation must be completely /monomorphic/
      and /first-order/, and cannot have any /non-representable/ arguments or
      result.

Also take the following into account when using 'TopEntity' annotations.

    * The CλaSH compiler is based on the GHC Haskell compiler, and the GHC
      machinery does not understand 'TopEntity' annotations and it might
      subsequently decide to inline those functions. You should therefor also
      add a @{\-\# NOINLINE f \#-\}@ pragma to the functions which you give
      a 'TopEntity' functions.
    * Functions with a 'TopEntity' annotation will not be specialised
      on constants.

Finally, the root module, the module which you pass as an argument to the
CλaSH compiler must either have:

    * A function with a 'TopEntity' annotation.
    * A function called /topEntity/.

You apply 'TopEntity' annotations to functions using an @ANN@ pragma:

@
{\-\# ANN f (TopEntity {t_name = ..., ...  }) \#-\}
f x = ...
@

For example, given the following specification:

@
module Blinker where

import Clash.Prelude
import Clash.Intel.ClockGen

type Dom50 = Dom \"System\" 20000

topEntity
  :: Clock Dom50 Source
  -> Reset Dom50 Asynchronous
  -> Signal Dom50 Bit
  -> Signal Dom50 (BitVector 8)
topEntity clk rst key1 =
    let  (pllOut,pllStable) = 'Clash.Intel.ClockGen.altpll' (SSymbol @ "altpll50") clk rst
         rstSync            = 'Clash.Signal.resetSynchronizer' pllOut ('Clash.Signal.unsafeToAsyncReset' pllStable)
    in   'Clash.Signal.withClockReset' pllOut rstSync leds
  where
    key1R  = 'Clash.Prelude.isRising' 1 key1
    leds   = 'Clash.Prelude.mealy' blinkerT (1,False,0) key1R

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

The CλaSH compiler would normally generate the following
@blinker_topentity.vhdl@ file:

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
    { t_name   = "blinker"
    , t_inputs = [ PortName \"CLOCK_50\"
                 , PortName \"KEY0\"
                 , PortName \"KEY1\" ]
    , t_output = PortName \"LED\"
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
{-# LANGUAGE DeriveGeneric      #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Annotations.TopEntity
  ( -- * Data types
    TopEntity (..)
  , PortName (..)
  , TestBench (..)
    -- * Convenience functions
  , defTop
  )
where

import           GHC.Generics
import qualified Language.Haskell.TH as TH
import           Data.Data

-- | TopEntity annotation
data TopEntity
  = TopEntity
  { t_name    :: String
  -- ^ The name the top-level component should have, put in a correspondingly
  -- named file.
  , t_inputs  :: [PortName]
  -- ^ List of names that are assigned in-order to the inputs of the component.
  , t_output  :: PortName
  -- ^ Name assigned in-order to the outputs of the component. As a Haskell
  -- function can only truly return a single value -- with multiple values
  -- \"wrapped\" by a tuple -- this field is not a list, but a single
  -- @'PortName'@. Use @'PortField'@ to give names to the individual components
  -- of the output tuple.
  } deriving (Data,Show,Read,Generic)

-- | Give port names for arguments/results.
--
-- Give a data type and function:
--
-- @
-- data T = MkT Int Bool
--
-- {\-\# ANN topEntity (defTop {t_name = \"f\",}) \#-\}
-- f :: Int -> T -> (T,Bool)
-- f a b = ...
-- @
--
-- Clash would normally generate the following VHDL entity:
--
-- @
-- entity f is
--   port(input_0      : in signed(63 downto 0);
--        input_1_0    : in signed(63 downto 0);
--        input_1_1    : in boolean;
--        output_0_0_0 : out signed(63 downto 0);
--        output_0_0_1 : out boolean;
--        output_0_1   : out boolean);
-- end;
-- @
--
-- However, we can change this by using 'PortName's. So by:
--
-- @
-- {\-\# ANN topEntity
--    (defTop
--       { t_name = \"f\"
--       , t_inputs = [ PortName \"a\"
--                    , PortName \"b\" ]
--       , t_output = PortName \"res\" }) \#-\}
-- f :: Int -> T -> (T,Bool)
-- f a b = ...
-- @
--
-- we get:
--
-- @
-- entity f is
--   port(a   : in signed(63 downto 0);
--        b   : in f_types.t;
--        res : out f_types.tup2);
-- end;
-- @
--
-- If we want to name fields for tuples/records we have to use 'PortField'
--
-- @
-- {\-\# ANN topEntity
--    (defTop
--       { t_name = \"f\"
--       , t_inputs = [ PortName \"a\"
--                    , PortField \"\" [ PortName \"b\", PortName \"c\" ] ]
--       , t_output = PortField \"res\" [PortName \"q\"] }) \#-\}
-- f :: Int -> T -> (T,Bool)
-- f a b = ...
-- @
--
-- So that we get:
--
-- @
-- entity f is
--   port(a     : in signed(63 downto 0);
--        b     : in signed(63 downto 0);
--        c     : in boolean;
--        q     : out f_types.t;
--        res_1 : out boolean);
-- end;
-- @
--
-- Notice how we didn't name the second field of the result, and the second
-- output port got 'PortField' name, \"res\", as a prefix for its name.
data PortName
  = PortName String
  -- ^ You want a port, with the given name, for the entire argument\/type
  --
  -- You can use an empty String ,\"\" , in case you want an auto-generated name.
  | PortField String [PortName]
  -- ^ You want to assign ports to fields of an argument\/type
  --
  -- The first argument of 'PortField' is the name of:
  --
  -- 1. The signal/wire to which the individual ports are aggregated.
  --
  -- 2. The prefix for any unnamed ports below the 'PortField'
  --
  -- You can use an empty String ,\"\" , in case you want an auto-generated name.
  deriving (Data,Show,Read,Generic)

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

-- | Default 'TopEntity' which has no specified names for the input and output
-- ports.
--
-- >>> defTop
-- TopEntity {t_name = "topentity", t_inputs = [], t_output = PortName ""}
defTop :: TopEntity
defTop = TopEntity
  { t_name   = "topentity"
  , t_inputs = []
  , t_output = PortName ""
  }
