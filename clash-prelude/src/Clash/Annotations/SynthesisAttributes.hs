{-|
  Copyright   :  (C) 2018,      Google Inc.,
                     2021-2023, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  API for synthesis attributes (sometimes referred to as "synthesis directives",
  "pragmas", or "logic synthesis directives"). This is an experimental feature,
  please report any unexpected or broken behavior to Clash's GitHub page
  (<https://github.com/clash-lang/clash-compiler/issues>).
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Annotations.SynthesisAttributes
  ( Attr(..)
  , Annotate
  , annotate
  , annotateReg
  , markDebug
  ) where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.String.Interpolate (__i)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)

import Clash.Annotations.Primitive (Primitive(InlineYamlPrimitive), hasBlackBox)
import Clash.Signal.Internal (Signal)
import Clash.Sized.Vector (Vec(..))

type Annotate (a :: Type) (attrs :: k) = a

-- | Synthesis attributes are directives passed to synthesis tools, such as
-- Quartus. An example of such an attribute in VHDL:
--
-- > attribute chip_pin : string;
-- > attribute chip_pin of sel : signal is "C4";
-- > attribute chip_pin of data : signal is "D1, D2, D3, D4";
--
-- This would instruct the synthesis tool to map the wire /sel/ to pin /C4/, and
-- wire /data/ to pins /D1/, /D2/, /D3/, and /D4/. To achieve this in Clash, /Attr/s
-- are used. An example of the same annotation:
--
-- @
-- import Clash.Annotations.SynthesisAttributes (Attr (..), Annotate )
--
-- myFunc
--     :: (Signal System Bool \`Annotate\` 'StringAttr "chip_pin" \"C4\")
--     -> (Signal System Int4 \`Annotate\` 'StringAttr "chip_pin" "D1, D2, D3, D4")
--     -> ...
-- myFunc sel data = ...
-- {\-\# NOINLINE myFunc \#-\}
-- @
--
-- To ensure this function will be rendered as its own module, do not forget a
-- NOINLINE pragma.
--
-- Multiple attributes for the /same/ argument can be specified by using a list.
-- For example:
--
-- @
-- Signal System Bool \`Annotate\`
--   [ 'StringAttr "chip_pin" \"C4\"
--   , 'BoolAttr "direct_enable" 'True
--   , 'IntegerAttr "max_depth" 512
--   , 'Attr "keep"
--   ]
-- @
--
-- For Verilog see:
--     <https://www.intel.com/content/www/us/en/programmable/quartushelp/current/index.htm#hdl/vlog/vlog_file_dir.htm>
--
-- For VHDL, see:
--     <https://www.intel.com/content/www/us/en/programmable/quartushelp/current/index.htm#hdl/vhdl/vhdl_file_dir.htm>
--
-- = Warnings
--
-- When using annotations, it is important that annotated arguments are not
-- eta-reduced, as this may result in the annotation being stripped by GHC. For
-- example
--
-- @
-- f :: Signal System Bool \`Annotate\` 'StringAttr \"chip_pin\" \"C4\"
--   -> Signal System Bool
-- f x = id x -- Using a lambda, i.e. f = \\x -> id x also works
-- @
--
-- will reliably show the annotation in the generated HDL, but
--
-- @
-- g :: Signal System Bool \`Annotate\` 'StringAttr \"chip_pin\" \"C4\"
--   -> Signal System Bool
-- g = id
-- @
--
-- will not work.
--
-- This is an experimental feature, please report any unexpected or broken
-- behavior to Clash's GitHub page (<https://github.com/clash-lang/clash-compiler/issues>).
--
-- Use 'annotate' if you wish to annotate an intermediate signal. Its use is
-- preferred over type level annotations.
data Attr a
  = BoolAttr a Bool
  -- ^ Attribute which argument is rendered as a bool. Example:
  -- <https://www.intel.com/content/www/us/en/programmable/quartushelp/current/index.htm#hdl/vlog/vlog_file_dir_direct_enable.htm>
  | IntegerAttr a Integer
  -- ^ Attribute which argument is rendered as a integer. Example:
  -- <https://www.intel.com/content/www/us/en/programmable/quartushelp/current/index.htm#hdl/vlog/vlog_file_dir_max_depth.htm>
  | StringAttr a a
  -- ^ Attribute which argument is rendered as a string. Example:
  -- <https://www.intel.com/content/www/us/en/programmable/quartushelp/current/index.htm#hdl/vlog/vlog_file_dir_chip.htm>
  | Attr a
  -- ^ Attribute rendered as constant. Example:
  -- <https://www.intel.com/content/www/us/en/programmable/quartushelp/current/index.htm#hdl/vlog/vlog_file_dir_keep.htm>
  deriving (Show, Generic, NFData, Binary, Lift, Eq, Ord, Hashable, Functor)

-- | Create a new identifier in HDL and inserts given synthesis attributes. The
-- name of the intermediate signal can be influenced using naming functions in
-- "Clash.Magic".
annotate :: forall n dom a . Vec n (Attr String) -> Signal dom a -> Signal dom a
annotate !_attrs !a = a
{-# CLASH_OPAQUE annotate #-}
{-# ANN annotate hasBlackBox #-}
{-# ANN annotate
  let primName = show 'annotate
  in InlineYamlPrimitive [minBound..] [__i|
    BlackBoxHaskell:
        name: #{primName}
        templateFunction: "Clash.Primitives.Annotations.SynthesisAttributes.annotateBBF"
        workInfo: Always
  |] #-}

-- | Insert attributes such that signals are preserved in major synthesis tools.
-- Also inserts "mark_debug", a way of signalling Vivado a signal should show up
-- in a list of signals desired for ILA/VIO insertion.
--
-- Attributes inserted: @keep@, @mark_debug@, @noprune@, and @preserve@.
markDebug :: Signal dom a -> Signal dom a
markDebug = annotate $
     BoolAttr "keep" True

  -- Vivado:
  :> BoolAttr "mark_debug" True

  -- Quartus:
  :> Attr "noprune"
  :> Attr "preserve"
  :> Nil

{- | Add synthesis attributes to register declarations in the HDL generated by the Clash
compiler.

For example, given the following code:

> import Clash.Explicit.Prelude
> import Clash.Annotations.SynthesisAttributes
>
> topEntity ::
>   Clock System ->
>   Reset System ->
>   Signal System Bool ->
>   Signal System Bool
> topEntity clk rst x =
>   let r1 = setName @"r1" (annotateReg attrs (dflipflop clk x))
>       r2 = setName @"r2" (annotateReg attrs (register clk rst enableGen False r1))
>    in r2
>  where
>   attrs = Attr "ASYNC_REG" :> Nil

The clash compiler would generate the following Verilog:

> `default_nettype none
> `timescale 100fs/100fs
> module topEntity
>     ( // Inputs
>       input wire  clk // clock
>     , input wire  rst // reset
>     , input wire  x
>
>       // Outputs
>     , output wire  result
>     );
>   (* ASYNC_REG *) reg  r1 = ({1 {1'bx}});
>   (* ASYNC_REG *) reg  r2 = 1'b0;
>
>   // delay begin
>   always @(posedge clk) begin : r1_delay
>     r1 <= x;
>   end
>   // delay end
>
>   // register begin
>   always @(posedge clk or  posedge  rst) begin : r2_register
>     if ( rst) begin
>       r2 <= 1'b0;
>     end else begin
>       r2 <= r1;
>     end
>   end
>   // register end
>
>   assign result = r2;
>
>
> endmodule
> `default_nettype wire

Where we can see that the /r1/ and /r2/ registers are given the specified
/ASYNC_REG/ attribute.

The above pattern can also be abstracted over, e.g. you could write:

> topEntity ::
>   Clock System ->
>   Reset System ->
>   Signal System Bool ->
>   Signal System Bool
> topEntity clk rst =
>   markAsync @"r1" (dflipflop clk) .
>   markAsync @"r2" (register clk rst enableGen False)
>
> markAsync ::
>   forall (name :: Symbol) dom a .
>   (Signal dom a -> Signal dom a) ->
>   Signal dom a ->
>   Signal dom a
> markAsync f = \x -> setName @name (annotateReg attrs (f x))
>  where
>   attrs = Attr "ASYNC_REG" :> Nil
> {-# INLINE markAsync #-}

And it will generate exactly the same Verilog as above.
-}
annotateReg :: forall n dom a . Vec n (Attr String) -> Signal dom a -> Signal dom a
annotateReg !_ x = x
{-# CLASH_OPAQUE annotateReg #-}
{-# ANN annotateReg hasBlackBox #-}
