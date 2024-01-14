{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.,
                  2021-2023, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

'TopEntity' annotations allow us to control hierarchy and naming aspects of the
Clash compiler. We have the 'Synthesize' and 'TestBench' annotation.

=== 'Synthesize' annotation

The 'Synthesize' annotation allows us to:

    * Assign names to entities (VHDL) \/ modules ((System)Verilog), and their
      ports.
    * Put generated HDL files of a logical (sub)entity in their own directory.
    * Use cached versions of generated HDL, i.e., prevent recompilation of
      (sub)entities that have not changed since the last run. Caching is based
      on a @.manifest@ which is generated alongside the HDL; deleting this file
      means deleting the cache; changing this file will result in /undefined/
      behavior.

Functions with a 'Synthesize' annotation must adhere to the following
restrictions:

    * Although functions with a 'Synthesize' annotation can of course depend
      on functions with another 'Synthesize' annotation, they must not be
      mutually recursive.
    * Functions with a 'Synthesize' annotation must be completely /monomorphic/
      and /first-order/, and cannot have any /non-representable/ arguments or
      result.

Also take the following into account when using 'Synthesize' annotations.

    * The Clash compiler is based on the GHC Haskell compiler, and the GHC
      machinery does not understand 'Synthesize' annotations and it might
      subsequently decide to inline those functions. You should therefor also
      add a @{\-\# NOINLINE f \#-\}@ pragma to the functions which you give
      a 'Synthesize' functions.
    * Functions with a 'Synthesize' annotation will not be specialized
      on constants.

Finally, the root module, the module which you pass as an argument to the
Clash compiler must either have:

    * A function with a 'Synthesize' annotation.
    * A function called /topEntity/.

You apply 'Synthesize' annotations to functions using an @ANN@ pragma:

@
{\-\# ANN f (Synthesize {t_name = ..., ...  }) \#-\}
f x = ...
@

For example, given the following specification:

@
module Blinker where

import Clash.Prelude
import Clash.Intel.ClockGen

-- Define a synthesis domain with a clock with a period of 20000 \/ps\/. Signal
-- coming from the reset button is low when pressed, and high when not pressed.
'Clash.Explicit.Signal.createDomain'
  vSystem{vName=\"DomInput\", vPeriod=20000, vResetPolarity=ActiveLow}
-- Define a synthesis domain with a clock with a period of 50000 \/ps\/.
'Clash.Explicit.Signal.createDomain' vSystem{vName=\"Dom50\", vPeriod=50000}

topEntity
  :: Clock DomInput
  -> Reset DomInput
  -> Enable Dom50
  -> Signal Dom50 Bit
  -> Signal Dom50 (BitVector 8)
topEntity clk20 rstBtn enaBtn modeBtn =
  'Clash.Signal.exposeClockResetEnable'
    ('Clash.Prelude.mealy' blinkerT initialStateBlinkerT . 'Clash.Prelude.isRising' 1)
    clk50
    rst50
    enaBtn
    modeBtn
 where
  -- Start with the first LED turned on, in rotate mode, with the counter on zero
  initialStateBlinkerT = (1, False, 0)

  -- Instantiate a PLL: this stabilizes the incoming clock signal and releases
  -- the reset output when the signal is stable. We're also using it to
  -- transform an incoming clock signal running at 20 MHz to a clock signal
  -- running at 50 MHz. Since the signature of topEntity already specifies the
  -- Dom50 domain, we don't need any type signatures to specify the domain here.
  (clk50, rst50) = 'Clash.Intel.ClockGen.altpllSync' clk20 rstBtn

blinkerT
  :: (BitVector 8, Bool, Index 16650001)
  -> Bool
  -> ((BitVector 8, Bool, Index 16650001), BitVector 8)
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

The Clash compiler would normally generate the following
@topEntity.vhdl@ file:

> -- Automatically generated VHDL-93
> library IEEE;
> use IEEE.STD_LOGIC_1164.ALL;
> use IEEE.NUMERIC_STD.ALL;
> use IEEE.MATH_REAL.ALL;
> use std.textio.all;
> use work.all;
> use work.Blinker_topEntity_types.all;
>
> entity topEntity is
>   port(-- clock
>        clk20   : in Blinker_topEntity_types.clk_DomInput;
>        -- reset
>        rstBtn  : in Blinker_topEntity_types.rst_DomInput;
>        -- enable
>        enaBtn  : in Blinker_topEntity_types.en_Dom50;
>        modeBtn : in std_logic;
>        result  : out std_logic_vector(7 downto 0));
> end;
>
> architecture structural of topEntity is
>   ...
> end;

However, if we add the following 'Synthesize' annotation in the file:

@
{\-\# ANN topEntity
  ('Synthesize'
    { t_name   = "blinker"
    , t_inputs = [ PortName \"CLOCK_50\"
                 , PortName \"KEY0\"
                 , PortName \"KEY1\"
                 , PortName \"KEY2\" ]
    , t_output = PortName \"LED\"
    }) \#-\}
@

The Clash compiler will generate the following @blinker.vhdl@ file instead:

> -- Automatically generated VHDL-93
> library IEEE;
> use IEEE.STD_LOGIC_1164.ALL;
> use IEEE.NUMERIC_STD.ALL;
> use IEEE.MATH_REAL.ALL;
> use std.textio.all;
> use work.all;
> use work.blinker_types.all;
>
> entity blinker is
>   port(-- clock
>        CLOCK_50 : in blinker_types.clk_DomInput;
>        -- reset
>        KEY0     : in blinker_types.rst_DomInput;
>        -- enable
>        KEY1     : in blinker_types.en_Dom50;
>        KEY2     : in std_logic;
>        LED      : out std_logic_vector(7 downto 0));
> end;
>
> architecture structural of blinker is
>   ...
> end;

Where we now have:

* A top-level component that is called @blinker@.
* Inputs and outputs that have a /user/-chosen name: @CLOCK_50@, @KEY0@, @KEY1@, @KEY2@, @LED@, etc.

See the documentation of 'Synthesize' for the meaning of all its fields.

=== 'TestBench' annotation

Tell what binder is the test bench for a 'Synthesize'-annotated binder.

@
entityBeingTested :: ...
entityBeingTested = ...
{\-\# NOINLINE entityBeingTested \#-\}
{\-\# ANN entityBeingTested (defSyn "entityBeingTested") \#-\}


myTestBench :: Signal System Bool
myTestBench = ... entityBeingTested ...
{\-\# NOINLINE myTestBench \#-\}
{\-\# ANN myTestBench (TestBench \'entityBeingTested) \#-\}
@

The 'TestBench' annotation actually already implies a 'Synthesize' annotation on
the device under test, so the 'defSyn' in the example could have been omitted.
We recommend you supply 'defSyn' explicitly nonetheless. In any case, it will
still need the @NOINLINE@ annotation.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Annotations.TopEntity
  ( -- * Data types
    TopEntity (..)
  , PortName (..)
    -- * Convenience functions
  , defSyn
  )
where

import           GHC.Generics
import qualified Language.Haskell.TH        as TH
import           Language.Haskell.TH.Syntax (Lift(..))
#if MIN_VERSION_template_haskell(2,16,0)
import           Language.Haskell.TH.Compat
#endif
import           Data.Data

-- | TopEntity annotation
data TopEntity
  -- | Instruct the Clash compiler to use this top-level function as a separately
  -- synthesizable component.
  = Synthesize
  { t_name    :: String
  -- ^ The name the top-level component should have, put in a correspondingly
  -- named file.
  , t_inputs  :: [PortName]
  -- ^ List of names that are assigned in-order to the inputs of the component.
  , t_output  :: PortName
  -- ^ Name assigned in-order to the outputs of the component. As a Haskell
  -- function can only truly return a single value -- with multiple values
  -- \"wrapped\" by a tuple -- this field is not a list, but a single
  -- @'PortName'@. Use @'PortProduct'@ to give names to the individual components
  -- of the output tuple.
  }
  -- | Tell what binder is the 'TestBench' for a 'Synthesize'-annotated binder.
  --
  -- @
  -- {\-\# NOINLINE myTestBench \#-\}
  -- {\-\# ANN myTestBench (TestBench \'entityBeingTested) \#-\}
  -- @
  | TestBench TH.Name
  deriving (Eq,Data,Show,Generic)

instance Lift TopEntity where
  lift (Synthesize name inputs output) =
    TH.appsE
      [ TH.conE 'Synthesize
      , lift name
      , lift inputs
      , lift output
      ]
  lift (TestBench _) = error "Cannot lift a TestBench"
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedFromUntyped
#endif

-- | Give port names for arguments/results.
--
-- Give a data type and function:
--
-- @
-- data T = MkT Int Bool
--
-- {\-\# ANN f (defSyn "f") \#-\}
-- f :: Int -> T -> (T,Bool)
-- f a b = ...
-- @
--
-- Clash would normally generate the following VHDL entity:
--
-- > entity f is
-- >   port(a      : in signed(63 downto 0);
-- >        b_0    : in signed(63 downto 0);
-- >        b_1    : in boolean;
-- >        result : out std_logic_vector(65 downto 0));
-- > end;
--
-- However, we can change this by using 'PortName's. So by:
--
-- @
-- {\-\# ANN f
--    (Synthesize
--       { t_name   = "f"
--       , t_inputs = [ PortName \"a\"
--                    , PortName \"b\" ]
--       , t_output = PortName \"res\" }) \#-\}
-- f :: Int -> T -> (T,Bool)
-- f a b = ...
-- @
--
-- we get:
--
-- > entity f is
-- >   port(a   : in signed(63 downto 0);
-- >        b   : in std_logic_vector(64 downto 0);
-- >        res : out std_logic_vector(65 downto 0));
-- > end;
--
-- If we want to name fields for tuples/records we have to use 'PortProduct'
--
-- @
-- {\-\# ANN f
--    (Synthesize
--       { t_name   = "f"
--       , t_inputs = [ PortName \"a\"
--                    , PortProduct \"\" [ PortName \"b\", PortName \"c\" ] ]
--       , t_output = PortProduct \"res\" [PortName \"q\"] }) \#-\}
-- f :: Int -> T -> (T,Bool)
-- f a b = ...
-- @
--
-- So that we get:
--
-- > entity f is
-- >   port(a     : in signed(63 downto 0);
-- >        b     : in signed(63 downto 0);
-- >        c     : in boolean;
-- >        res_q : out std_logic_vector(64 downto 0);
-- >        res_1 : out boolean);
-- > end;
--
-- Notice how we didn't name the second field of the result, and the second
-- output port got 'PortProduct' name, \"res\", as a prefix for its name.
data PortName
  = PortName String
  -- ^ You want a port, with the given name, for the entire argument\/type
  --
  -- You can use an empty String ,@""@ , in case you want an auto-generated name.
  | PortProduct String [PortName]
  -- ^ You want to assign ports to fields of a product argument\/type
  --
  -- The first argument of 'PortProduct' is the name of:
  --
  -- 1. The signal/wire to which the individual ports are aggregated.
  --
  -- 2. The prefix for any unnamed ports below the 'PortProduct'
  --
  -- You can use an empty String ,@""@ , in case you want an auto-generated name.
  deriving (Eq,Data,Show,Generic,Lift)

-- | Default 'Synthesize' annotation which has no specified names for the input
-- and output ports.
--
-- >>> defSyn "foo"
-- Synthesize {t_name = "foo", t_inputs = [], t_output = PortName ""}
defSyn :: String -> TopEntity
defSyn name = Synthesize
  { t_name   = name
  , t_inputs = []
  , t_output = PortName ""
  }
