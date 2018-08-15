{-|
  Copyright  :  (C) 2013-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017     , QBayLogic, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Type definitions used by the Driver module
-}

{-# LANGUAGE CPP #-}

module Clash.Driver.Types where

-- For Int/Word size
#include "MachDeps.h"

import Data.Text         (Text)

import BasicTypes        (InlineSpec)
import SrcLoc            (SrcSpan)

import Clash.Core.Term   (Term)
import Clash.Core.Var    (Id)
import Clash.Core.VarEnv (VarEnv)

import Clash.Netlist.BlackBox.Types (HdlSyn (..))

-- | Global function binders
--
-- Global functions cannot be mutually recursive, only self-recursive
type BindingMap = VarEnv (Id,SrcSpan,InlineSpec,Term)

-- | Debug Message Verbosity
data DebugLevel
  = DebugNone    -- ^ Don't show debug messages
  | DebugFinal   -- ^ Show completely normalized expressions
  | DebugName    -- ^ Names of applied transformations
  | DebugApplied -- ^ Show sub-expressions after a successful rewrite
  | DebugAll     -- ^ Show all sub-expressions on which a rewrite is attempted
  deriving (Eq,Ord,Read)

data ClashOpts = ClashOpts { opt_inlineLimit :: Int
                           , opt_specLimit   :: Int
                           , opt_inlineFunctionLimit :: Word
                           , opt_inlineConstantLimit :: Word
                           , opt_dbgLevel    :: DebugLevel
                           , opt_cachehdl    :: Bool
                           , opt_cleanhdl    :: Bool
                           , opt_intWidth    :: Int
                           , opt_hdlDir      :: Maybe String
                           , opt_hdlSyn      :: HdlSyn
                           , opt_errorExtra  :: Bool
                           , opt_floatSupport :: Bool
                           , opt_importPaths :: [FilePath]
                           , opt_componentPrefix :: Maybe String
                           }


defClashOpts
  :: ClashOpts
defClashOpts
  = ClashOpts
  { opt_dbgLevel            = DebugNone
  , opt_inlineLimit         = 20
  , opt_specLimit           = 20
  , opt_inlineFunctionLimit = 15
  , opt_inlineConstantLimit = 0
  , opt_cachehdl            = True
  , opt_cleanhdl            = True
  , opt_intWidth            = WORD_SIZE_IN_BITS
  , opt_hdlDir              = Nothing
  , opt_hdlSyn              = Other
  , opt_errorExtra          = False
  , opt_floatSupport        = False
  , opt_importPaths         = []
  , opt_componentPrefix     = Nothing
  }

-- | Information about the generated HDL between (sub)runs of the compiler
data Manifest
  = Manifest
  { manifestHash :: (Int,Maybe Int)
    -- ^ Hash of the TopEntity and all its dependencies
    --   + (maybe) Hash of the TestBench and all its dependencies
  , portInNames  :: [Text]
  , portInTypes  :: [Text]
    -- ^ The rendered versions of the types of the input ports of the TopEntity
    --
    -- Used when dealing with multiple @TopEntity@s who have different names
    -- for types which are structurally equal
  , portOutNames :: [Text]
  , portOutTypes :: [Text]
    -- ^ The rendered versions of the types of the output ports of the TopEntity
    --
    -- Used when dealing with multiple @TopEntity@s who have different names
    -- for types which are structurally equal
  , componentNames :: [Text]
    -- ^ Names of all the generated components for the @TopEntity@ (does not
    -- include the names of the components of the @TestBench@ accompanying
    -- the @TopEntity@).
  }
  deriving (Show,Read)
