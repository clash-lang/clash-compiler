{-|
  Copyright  :  (C) 2013-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017     , QBayLogic, Google Inc.
                    2020     , QBayLogic
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Type definitions used by the Driver module
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Driver.Types where

-- For Int/Word size
#include "MachDeps.h"

import           Control.DeepSeq                (NFData)
import           Data.Binary                    (Binary)
import           Data.Fixed
import           Data.Hashable
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import           Data.Text.Prettyprint.Doc
import           GHC.Generics                   (Generic)

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Types.Basic                (InlineSpec)
import           GHC.Types.SrcLoc               (SrcSpan)
import           GHC.Utils.Misc                 (OverridingBool(..))
#else
import           BasicTypes                     (InlineSpec)
import           SrcLoc                         (SrcSpan)
import           Util                           (OverridingBool(..))
#endif

import           Clash.Signal.Internal

import           Clash.Core.Term                (Term)
import           Clash.Core.Var                 (Id)
import           Clash.Core.VarEnv              (VarEnv)
import           Clash.Netlist.BlackBox.Types   (HdlSyn (..))
import {-# SOURCE #-} Clash.Netlist.Types       (PreserveCase(..))

data IsPrim
  = IsPrim
    -- ^ The binding is the unfolding for a primitive.
  | IsFun
    -- ^ The binding is an ordinary function.
  deriving (Binary, Eq, Generic, NFData, Show)

-- A function binder in the global environment.
--
data Binding a = Binding
  { bindingId :: Id
    -- ^ The core identifier for this binding.
  , bindingLoc :: SrcSpan
    -- ^ The source location of this binding in the original source code.
  , bindingSpec :: InlineSpec
    -- ^ the inline specification for this binding, in the original source code.
  , bindingIsPrim :: IsPrim
    -- ^ Is the binding a core term corresponding to a primitive with a known
    -- implementation? If so, it can potentially be inlined despite being
    -- marked as NOINLINE in source.
  , bindingTerm :: a
    -- ^ The term representation for this binding. This is polymorphic so
    -- alternate representations can be used if more appropriate (i.e. in the
    -- evaluator this can be Value for evaluated bindings).
  } deriving (Binary, Functor, Generic, NFData, Show)

-- | Global function binders
--
-- Global functions cannot be mutually recursive, only self-recursive.
type BindingMap = VarEnv (Binding Term)

-- | Debug Message Verbosity
data DebugLevel
  = DebugNone
  -- ^ Don't show debug messages
  | DebugSilent
  -- ^ Run invariant checks and err if violated (enabled by any debug flag)
  | DebugFinal
  -- ^ Show completely normalized expressions
  | DebugName
  -- ^ Show names of applied transformations
  | DebugTry
  -- ^ Show names of tried AND applied transformations
  | DebugApplied
  -- ^ Show sub-expressions after a successful rewrite
  | DebugAll
  -- ^ Show all sub-expressions on which a rewrite is attempted
  deriving (Eq,Ord,Read,Enum,Generic,Hashable)

-- | Options passed to Clash compiler
data ClashOpts = ClashOpts
  { opt_inlineLimit :: Int
  -- ^ Change the number of times a function f can undergo inlining inside
  -- some other function g. This prevents the size of g growing dramatically.
  --
  -- Command line flag: -fclash-inline-limit
  , opt_specLimit :: Int
  -- ^ Change the number of times a function can undergo specialization.
  --
  -- Command line flag: -fclash-spec-limit
  , opt_inlineFunctionLimit :: Word
  -- ^ Set the threshold for function size. Below this threshold functions are
  -- always inlined (if it is not recursive).
  --
  -- Command line flag: -fclash-inline-function-limit
  , opt_inlineConstantLimit :: Word
  -- ^ Set the threshold for constant size. Below this threshold constants are
  -- always inlined. A value of 0 inlines all constants.
  --
  -- Command line flag: -fclash-inline-constant-limit
  , opt_evaluatorFuelLimit :: Word
  -- ^ Set the threshold for maximum unfolding depth in the evaluator. A value
  -- of zero means no potentially non-terminating binding is unfolded.
  --
  -- Command line flag: -fclash-evaluator-fuel-limit
  , opt_dbgLevel :: DebugLevel
  -- ^ Set the debugging mode for the compiler, exposing additional output. See
  -- "DebugLevel" for the available options.
  --
  -- Command line flag: -fclash-debug
  , opt_dbgTransformations :: Set.Set String
  -- ^ List the transformations that are to be debugged.
  --
  -- Command line flag: -fclash-debug-transformations
  , opt_dbgTransformationsFrom :: Int
  -- ^ Only output debug information from (applied) transformation n
  --
  -- Command line flag: -fclash-debug-transformations-from
  , opt_dbgTransformationsLimit :: Int
  -- ^ Only output debug information for n (applied) transformations. If this
  -- limit is exceeded, Clash will stop normalizing.
  --
  -- Command line flag: -fclash-debug-transformations-limit

  , opt_dbgRewriteHistoryFile :: Maybe FilePath
  -- ^ Save all applied rewrites to a file
  --
  -- Command line flag: -fclash-debug-history

  , opt_cachehdl :: Bool
  -- ^ Reuse previously generated output from Clash. Only caches topentities.
  --
  -- Command line flag: -fclash-no-cache
  , opt_clear :: Bool
  -- ^ Remove HDL directories before writing to them. By default, Clash will
  -- only write to non-empty directories if it can prove all files in it are
  -- generated by a previous run. This option applies to directories of the
  -- various top entities, i.e., the subdirectories made in the directory passed
  -- in with @-fclash-hdldir@. Note that Clash will still use a cache if it can.
  --
  -- Command line flag: @-fclash-clear@
  , opt_primWarn :: Bool
  -- ^ Disable warnings for primitives
  --
  -- Command line flag: -fclash-no-prim-warn
  , opt_color :: OverridingBool
  -- ^ Show colors in debug output
  --
  -- Command line flag: -fclash-no-prim-warn
  , opt_intWidth :: Int
  -- ^ Set the bit width for the Int/Word/Integer types. The only allowed values
  -- are 32 or 64.
  , opt_hdlDir :: Maybe String
  -- ^ Directory to save HDL files to
  , opt_hdlSyn :: HdlSyn
  -- ^ Synthesis target. See "HdlSyn" for available options.
  , opt_errorExtra :: Bool
  -- ^ Show additional information in error messages
  , opt_floatSupport :: Bool
  -- ^ Treat floats as a BitVector. Note that operations on floating are still
  -- not supported, use vendor primitives instead.
  , opt_importPaths :: [FilePath]
  -- ^ Paths where Clash should look for modules
  , opt_componentPrefix :: Maybe Text
  -- ^ Prefix components with given string
  , opt_newInlineStrat :: Bool
  -- ^ Use new inline strategy. Functions marked NOINLINE will get their own
  -- HDL module.
  , opt_escapedIds :: Bool
  -- ^ Use escaped identifiers in HDL. See:
  --
  --  * http://vhdl.renerta.com/mobile/source/vhd00037.htm
  --  * http://verilog.renerta.com/source/vrg00018.htm
  , opt_lowerCaseBasicIds :: PreserveCase
  -- ^ Force all generated basic identifiers to lowercase. Among others, this
  -- affects module and file names.
  , opt_ultra :: Bool
  -- ^ Perform a high-effort compile, trading improved performance for
  -- potentially much longer compile times.
  --
  -- Name inspired by Design Compiler's /compile_ultra/ flag.
  , opt_forceUndefined :: Maybe (Maybe Int)
  -- ^
  -- * /Nothing/: generate undefined's in the HDL
  --
  -- * /Just Nothing/: replace undefined's by a constant in the HDL; the
  -- compiler decides what's best
  --
  -- * /Just (Just x)/: replace undefined's by /x/ in the HDL
  , opt_checkIDir :: Bool
  -- ^ Check whether paths specified in 'opt_importPaths' exists on the
  -- filesystem.
  , opt_aggressiveXOpt :: Bool
  -- ^ Enable aggressive X optimization, which may remove undefineds from
  -- generated HDL by replaced with defined alternatives.
  , opt_aggressiveXOptBB :: Bool
  -- ^ Enable aggressive X optimization, which may remove undefineds from
  -- HDL generated by blackboxes. This enables the ~ISUNDEFINED template tag.
  , opt_inlineWFCacheLimit :: Word
  -- ^ At what size do we cache normalized work-free top-level binders.
  , opt_edalize :: Bool
  -- ^ Generate an EDAM file for use with Edalize.
  }

instance Hashable ClashOpts where
  hashWithSalt s ClashOpts {..} =
    s `hashWithSalt`
    opt_inlineLimit `hashWithSalt`
    opt_specLimit `hashWithSalt`
    opt_inlineFunctionLimit `hashWithSalt`
    opt_inlineConstantLimit `hashWithSalt`
    opt_evaluatorFuelLimit `hashWithSalt`
    opt_dbgLevel `hashSet`
    opt_dbgTransformations `hashWithSalt`
    opt_dbgTransformationsFrom `hashWithSalt`
    opt_dbgTransformationsLimit `hashWithSalt`
    opt_dbgRewriteHistoryFile `hashWithSalt`
    opt_cachehdl `hashWithSalt`
    opt_clear `hashWithSalt`
    opt_primWarn `hashOverridingBool`
    opt_color `hashWithSalt`
    opt_intWidth `hashWithSalt`
    opt_hdlDir `hashWithSalt`
    opt_hdlSyn `hashWithSalt`
    opt_errorExtra `hashWithSalt`
    opt_floatSupport `hashWithSalt`
    opt_importPaths `hashWithSalt`
    opt_componentPrefix `hashWithSalt`
    opt_newInlineStrat `hashWithSalt`
    opt_escapedIds `hashWithSalt`
    opt_lowerCaseBasicIds `hashWithSalt`
    opt_ultra `hashWithSalt`
    opt_forceUndefined `hashWithSalt`
    opt_checkIDir `hashWithSalt`
    opt_aggressiveXOpt `hashWithSalt`
    opt_aggressiveXOptBB `hashWithSalt`
    opt_inlineWFCacheLimit `hashWithSalt`
    opt_edalize
   where
    hashOverridingBool :: Int -> OverridingBool -> Int
    hashOverridingBool s1 Auto = hashWithSalt s1 (0 :: Int)
    hashOverridingBool s1 Always = hashWithSalt s1 (1 :: Int)
    hashOverridingBool s1 Never = hashWithSalt s1 (2 :: Int)
    infixl 0 `hashOverridingBool`

    hashSet :: Hashable a => Int -> Set.Set a -> Int
    hashSet = Set.foldl' hashWithSalt
    infixl 0 `hashSet`

defClashOpts :: ClashOpts
defClashOpts
  = ClashOpts
  { opt_dbgLevel            = DebugNone
  , opt_dbgRewriteHistoryFile = Nothing
  , opt_dbgTransformations  = Set.empty
  , opt_dbgTransformationsFrom = 0
  , opt_dbgTransformationsLimit = maxBound
  , opt_inlineLimit         = 20
  , opt_specLimit           = 20
  , opt_inlineFunctionLimit = 15
  , opt_inlineConstantLimit = 0
  , opt_evaluatorFuelLimit  = 20
  , opt_cachehdl            = True
  , opt_clear               = False
  , opt_primWarn            = True
  , opt_color               = Auto
  , opt_intWidth            = WORD_SIZE_IN_BITS
  , opt_hdlDir              = Nothing
  , opt_hdlSyn              = Other
  , opt_errorExtra          = False
  , opt_floatSupport        = False
  , opt_importPaths         = []
  , opt_componentPrefix     = Nothing
  , opt_newInlineStrat      = True
  , opt_escapedIds          = True
  , opt_lowerCaseBasicIds   = PreserveCase
  , opt_ultra               = False
  , opt_forceUndefined      = Nothing
  , opt_checkIDir           = True
  , opt_aggressiveXOpt      = False
  , opt_aggressiveXOptBB    = False
  , opt_inlineWFCacheLimit  = 10 -- TODO: find "optimal" value
  , opt_edalize             = False
  }

-- | Synopsys Design Constraint (SDC) information for a component.
-- Currently this limited to the names and periods of clocks for create_clock.
--
newtype SdcInfo = SdcInfo
  { sdcClock :: [(Text, VDomainConfiguration)]
  }

-- | Render an SDC file from an SdcInfo.
-- The clock periods, waveforms, and targets are all hardcoded.
--
pprSDC :: SdcInfo -> Doc ()
pprSDC = vcat . fmap go . sdcClock
 where
  go (i, dom) =
        -- VDomainConfiguration stores period in ps, SDC expects it in ns.
    let p        = MkFixed (toInteger $ vPeriod dom) :: Fixed E3
        name     = braces (pretty i)
        period   = viaShow p
        waveform = braces ("0.000" <+> viaShow (p / 2))
        targets  = brackets ("get_ports" <+> name)
     in hsep
          [ "create_clock"
          , "-name" <+> name
          , "-period" <+> period
          , "-waveform" <+> waveform
          , targets
          ]
