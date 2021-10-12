{-|
  Copyright  :  (C) 2013-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017     , QBayLogic, Google Inc.
                    2020-2021, QBayLogic
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
import           Data.Maybe                     (isJust)
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)

#if MIN_VERSION_prettyprinter(1,7,0)
import           Prettyprinter
#else
import           Data.Text.Prettyprint.Doc
#endif

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

-- | Information to show about transformations during compilation.
--
-- __NB__: The @Ord@ instance compares by amount of information.
data TransformationInfo
  = None
  -- ^ Show no information about transformations.
  | FinalTerm
  -- ^ Show the final term after all applied transformations.
  | AppliedName
  -- ^ Show the name of every transformation that is applied.
  | AppliedTerm
  -- ^ Show the name and result of every transformation that is applied.
  | TryName
  -- ^ Show the name of every transformation that is attempted, and the result
  -- of every transformation that is applied.
  | TryTerm
  -- ^ Show the name and input to every transformation that is applied, and
  -- the result of every transformation that is applied.
  deriving (Eq, Generic, Hashable, Ord, Read, Show)

-- | Options related to debugging. See 'ClashOpts'
data DebugOpts = DebugOpts
  { dbg_invariants :: Bool
  -- ^ Check that the results of applied transformations do not violate the
  -- invariants for rewriting (e.g. no accidental shadowing, or type changes).
  --
  -- Command line flag: -fclash-debug-invariants
  , dbg_transformationInfo :: TransformationInfo
  -- ^ The information to show when debugging a transformation. See the
  -- 'TransformationInfo' type for different configurations.
  --
  -- Command line flag: -fclash-debug-info (None|FinalTerm|AppliedName|AppliedTerm|TryName|TryTerm)
  , dbg_transformations :: Set String
  -- ^ List the transformations that are being debugged. When the set is empty,
  -- all transformations are debugged.
  --
  -- Command line flag: -fclash-debug-transformations t1[,t2...]
  , dbg_countTransformations :: Bool
  -- ^ Count how many times transformations are applied and provide a summary
  -- at the end of normalization. This includes all transformations, not just
  -- those in 'dbg_transformations'.
  --
  -- Command line flag: -fclash-debug-count-transformations
  , dbg_transformationsFrom :: Maybe Word
  -- ^ Debug transformations applied after the nth transformation applied. This
  -- includes all transformations, not just those in 'dbg_transformations'.
  --
  -- Command line flag: -fclash-debug-transformations-from=N
  , dbg_transformationsLimit :: Maybe Word
  -- ^ Debug up to the nth applied transformation. If this limit is exceeded
  -- then Clash will error. This includes all transformations, not just those
  -- in 'dbg_transformations'.
  --
  -- Command line flag: -fclash-debug-transformations-limit=N
  , dbg_historyFile :: Maybe FilePath
  -- ^ Save information about all applied transformations to a history file
  -- for use with @clash-term@.
  --
  -- Command line flag: -fclash-debug-history[=FILE]
  } deriving (Show)

instance Hashable DebugOpts where
  hashWithSalt s DebugOpts{..} =
    s `hashWithSalt`
    dbg_invariants `hashWithSalt`
    dbg_transformationInfo `hashSet`
    dbg_transformations `hashWithSalt`
    dbg_countTransformations `hashWithSalt`
    dbg_transformationsFrom `hashWithSalt`
    dbg_transformationsLimit `hashWithSalt`
    dbg_historyFile
   where
    hashSet = Set.foldl' hashWithSalt
    infixl 0 `hashSet`

-- | Check whether the debugging options mean the compiler is debugging. This
-- is true only if at least one debugging feature is enabled, namely one of
--
--   * checking for invariants
--   * showing info for transformations
--   * counting applied transformations
--   * limiting the number of transformations
--
-- Other flags, such as writing to a history file or offsetting which applied
-- transformation to show information from do not affect the result, as it is
-- possible to enable these but still not perform any debugging checks in
-- functions like 'applyDebug'. If this is no longer the case, this function
-- will need to be changed.
isDebugging :: DebugOpts -> Bool
isDebugging opts = or
  [ dbg_invariants opts
  , dbg_transformationInfo opts > None
  , dbg_countTransformations opts
  , isJust (dbg_transformationsLimit opts)
  ]

-- | Check whether the requested information is available to the specified
-- transformation according to the options. e.g.
--
-- @
-- traceIf (hasDebugInfo AppliedName name opts) ("Trace something using: " <> show name)
-- @
--
-- This accounts for the set of transformations which are being debugged. For a
-- check which is agnostic to the a transformation, see 'hasTransformationInfo'.
hasDebugInfo :: TransformationInfo -> String -> DebugOpts -> Bool
hasDebugInfo info name opts =
  isDebugged name && hasTransformationInfo info opts
 where
  isDebugged n =
    let set = dbg_transformations opts
     in Set.null set || Set.member n set

-- | Check that the transformation info shown supports the requested info.
-- If the call-site is in the context of a particular transformation,
-- 'hasDebugInfo' should be used instead.
hasTransformationInfo :: TransformationInfo -> DebugOpts -> Bool
hasTransformationInfo info opts =
  info <= dbg_transformationInfo opts

-- NOTE [debugging options]
--
-- The preset debugging options here provide backwards compatibility with the
-- old style DebugLevel enum. However it is also possible to have finer-grained
-- control over debugging by using individual flags which did not previously
-- exist, e.g. -fclash-debug-invariants.

-- | -fclash-debug DebugNone
debugNone :: DebugOpts
debugNone = DebugOpts
  { dbg_invariants = False
  , dbg_transformationInfo = None
  , dbg_transformations = Set.empty
  , dbg_countTransformations = False
  , dbg_transformationsFrom = Nothing
  , dbg_transformationsLimit = Nothing
  , dbg_historyFile = Nothing
  }

-- | -fclash-debug DebugSilent
debugSilent :: DebugOpts
debugSilent = debugNone { dbg_invariants = True }

-- | -fclash-debug DebugFinal
debugFinal :: DebugOpts
debugFinal = debugSilent { dbg_transformationInfo = FinalTerm }

-- | -fclash-debug DebugCount
debugCount :: DebugOpts
debugCount = debugFinal { dbg_countTransformations = True }

-- | -fclash-debug DebugName
debugName :: DebugOpts
debugName = debugCount { dbg_transformationInfo = AppliedName }

-- | -fclash-debug DebugTry
debugTry :: DebugOpts
debugTry = debugName { dbg_transformationInfo = TryName }

-- | -fclash-debug DebugApplied
debugApplied :: DebugOpts
debugApplied = debugTry { dbg_transformationInfo = AppliedTerm }

-- | -fclash-debug DebugAll
debugAll :: DebugOpts
debugAll = debugApplied { dbg_transformationInfo = TryTerm }

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
  , opt_debug :: DebugOpts
  -- ^ Options which control debugging. See 'DebugOpts'.
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
  , opt_renderEnums :: Bool
  -- ^ Render sum types with all zero-width fields as enums where supported, as
  -- opposed to rendering them as bitvectors.
  }

instance Hashable ClashOpts where
  hashWithSalt s ClashOpts {..} =
    s `hashWithSalt`
    opt_inlineLimit `hashWithSalt`
    opt_specLimit `hashWithSalt`
    opt_inlineFunctionLimit `hashWithSalt`
    opt_inlineConstantLimit `hashWithSalt`
    opt_evaluatorFuelLimit `hashWithSalt`
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
    opt_edalize `hashWithSalt`
    opt_renderEnums
   where
    hashOverridingBool :: Int -> OverridingBool -> Int
    hashOverridingBool s1 Auto = hashWithSalt s1 (0 :: Int)
    hashOverridingBool s1 Always = hashWithSalt s1 (1 :: Int)
    hashOverridingBool s1 Never = hashWithSalt s1 (2 :: Int)
    infixl 0 `hashOverridingBool`

defClashOpts :: ClashOpts
defClashOpts
  = ClashOpts
  { opt_inlineLimit         = 20
  , opt_specLimit           = 20
  , opt_inlineFunctionLimit = 15
  , opt_inlineConstantLimit = 0
  , opt_evaluatorFuelLimit  = 20
  , opt_debug               = debugNone
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
  , opt_renderEnums         = True
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
