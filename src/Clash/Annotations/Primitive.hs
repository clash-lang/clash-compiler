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

-- | Instruct the clash compiler to look for primitive HDL templates in the
-- indicated directory. For distribution of new packages with primitive HDL
-- templates.
--
-- === Example
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
data Primitive
  = Primitive HDL FilePath
  -- ^ Description of a primitive for a given 'HDL' in a file at 'FilePath'
  | InlinePrimitive HDL String
  -- ^ Description of a primitive for a given 'HDL' as an inline 'String'
  -- (intended to be used within @ANN@ pragmas)
  deriving (Show, Read, Data)
