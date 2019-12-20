{-|
  Copyright   :  (C) 2018, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  API for synthesis attributes (sometimes refered to as "synthesis directives",
  "pragmas", or "logic synthesis directives"). This is an experimental feature,
  please report any unexpected or broken behavior to Clash's GitHub page
  (<https://github.com/clash-lang/clash-compiler/issues>).
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}

module Clash.Annotations.SynthesisAttributes
  ( Attr(..)
  , Annotate
  ) where

import GHC.TypeLits (Symbol)
import Data.Kind (Type)

type Annotate (a :: Type) (attrs :: k) = a

-- | Synthesis attributes are directives passed to sythesis tools, such as
-- Quartus. An example of such an attribute in VHDL:
--
-- @
--   attribute chip_pin : string;
--   attribute chip_pin of sel : signal is \"C4\";
--   attribute chip_pin of data : signal is "D1, D2, D3, D4";
-- @
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
-- {-# NOINLINE myFunc #-}
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
-- Warning: This is an experimental feature, please report any unexpected or broken
-- behavior to Clash's GitHub page (<https://github.com/clash-lang/clash-compiler/issues>).
data Attr
  = BoolAttr Symbol Bool
  -- ^ Attribute which argument is rendered as a bool. Example:
  -- <https://www.intel.com/content/www/us/en/programmable/quartushelp/current/index.htm#hdl/vlog/vlog_file_dir_direct_enable.htm>
  | IntegerAttr Symbol Integer
  -- ^ Attribute which argument is rendered as a integer. Example:
  -- <https://www.intel.com/content/www/us/en/programmable/quartushelp/current/index.htm#hdl/vlog/vlog_file_dir_max_depth.htm>
  | StringAttr Symbol Symbol
  -- ^ Attribute which argument is rendered as a string. Example:
  -- <https://www.intel.com/content/www/us/en/programmable/quartushelp/current/index.htm#hdl/vlog/vlog_file_dir_chip.htm>
  | Attr Symbol
  -- ^ Attribute rendered as constant. Example:
  -- <https://www.intel.com/content/www/us/en/programmable/quartushelp/current/index.htm#hdl/vlog/vlog_file_dir_keep.htm>
