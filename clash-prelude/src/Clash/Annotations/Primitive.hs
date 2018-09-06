{-|
Copyright  :  (C) 2017, Myrtle Software, QBayLogic
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Instruct the clash compiler to look for primitive HDL templates in the
indicated directory. For distribution of new packages with primitive HDL
templates.
-}

{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Annotations.Primitive where

import Data.Data

data HDL
  = SystemVerilog
  | Verilog
  | VHDL
  deriving (Eq, Show, Read, Data)

-- | The 'Primitive' constructor instructs the clash compiler to look for primitive
-- HDL templates in the indicated directory. 'InlinePrimitive' is equivalent but
-- provides the HDL template inline. They are intended for the distribution of
-- new packages with primitive HDL templates.
--
-- === Example of 'Primitive'
--
-- You have some existing IP written in one of HDLs supported by Clash, and
-- you want to distribute some bindings so that the IP can be easily instantiated
-- from Clash.
--
-- You create a package which has a @myfancyip.cabal@ file with the following stanza:
--
-- @
-- data-files: path\/to\/MyFancyIP.json
-- cpp-options: -DCABAL
-- @
--
-- and a @MyFancyIP.hs@ module with the simulation definition and primitive.
--
-- @
-- module MyFancyIP where
--
-- import Clash.Prelude
--
-- myFancyIP :: ...
-- myFancyIP = ...
-- {\-\# NOINLINE myFancyIP \#-\}
-- @
--
-- The @NOINLINE@ pragma is needed so that GHC will never inline the definition.
--
-- Now you need to add the following imports and @ANN@ pragma:
--
-- @
-- \#ifdef CABAL
-- import           Clash.Annotations.Primitive
-- import           System.FilePath
-- import qualified Paths_myfancyip
-- import           System.IO.Unsafe
--
-- {\-\# ANN module (Primitive VHDL (unsafePerformIO Paths_myfancyip.getDataDir \<\/\> "path" \<\/\> "to")) \#-\}
-- \#endif
-- @
--
-- Add more files to the @data-files@ stanza in your @.cabal@ files and more
-- @ANN@ pragma's if you want to add more primitive templates for other HDLs
--
-- === Example of 'InlinePrimitive'
--
-- The following example shows off an inline HDL primitive template. It uses the
-- [interpolate](https://hackage.haskell.org/package/interpolate) package for
-- nicer multiline strings.
--
-- @
-- {\-\# LANGUAGE DataKinds   \#-\}
-- {\-\# LANGUAGE QuasiQuotes \#-\}
--
-- module InlinePrimitive where
--
-- import           Clash.Annotations.Primitive
-- import           Clash.Prelude
-- import           Data.String.Interpolate      (i)
-- import           Data.String.Interpolate.Util (unindent)
--
-- {\-\# ANN example (InlinePrimitive VHDL $ unindent [i|
--   [ { \"BlackBox\" :
--       { "name" : "InlinePrimitive.example"
--       , "templateD" :
--   "-- begin InlinePrimitive example:
--   ~GENSYM[example][0] : block
--   ~RESULT <= 1 + ~ARG[0];
--   end block;
--   -- end InlinePrimitive example"
--       }
--     }
--   ]
--   |]) \#-\}
-- {\-\# NOINLINE example \#-\}
-- example :: Signal System (BitVector 2) -> Signal System (BitVector 2)
-- example = fmap succ
-- @
data Primitive
  = Primitive HDL FilePath
  -- ^ Description of a primitive for a given 'HDL' in a file at 'FilePath'
  | InlinePrimitive HDL String
  -- ^ Description of a primitive for a given 'HDL' as an inline 'String'
  deriving (Show, Read, Data)
