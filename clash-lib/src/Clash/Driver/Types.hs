{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
  Copyright  :  (C) 2013-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017     , QBayLogic, Google Inc.,
                    2022     , Google Inc.,
                    2020-2026, QBayLogic,
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Type definitions used by the Driver module
-}
module Clash.Driver.Types where

-- For Int/Word size
#include "MachDeps.h"

import Control.DeepSeq (NFData (rnf), deepseq)
import Data.Binary (Binary)
import Data.Fixed
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.IntMap.Strict (IntMap)
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text (dropAround)
#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
#else
import Data.Text.Prettyprint.Doc
#endif
import Clash.Annotations.BitRepresentation.Internal (CustomReprs)
import Clash.Backend.Verilog.Time (Period (..), Unit (Fs))
import Clash.Core.Pretty (unsafeLookupEnvBool)
import Clash.Core.Term (Term)
import Clash.Core.TyCon (TyConMap, TyConName)
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (VarEnv)
import Clash.Driver.Bool (OverridingBool (..))
import Clash.Netlist.BlackBox.Types (HdlSyn (..))
import {-# SOURCE #-} Clash.Netlist.Types (PreserveCase (..), TopEntityT)
import Clash.Primitives.Types (CompiledPrimMap)
import Clash.Signal.Internal
import Clash.Warning (WarningOpts, defWarningOpts)
import GHC.Generics (Generic)
import GHC.Types.Basic (InlineSpec)
import GHC.Types.SrcLoc (SrcSpan)

data ClashEnv = ClashEnv
  { envOpts :: ClashOpts,
    envTyConMap :: TyConMap,
    envTupleTyCons :: IntMap TyConName,
    envPrimitives :: CompiledPrimMap,
    envCustomReprs :: CustomReprs,
    envDomains :: DomainMap
  }
  deriving (Generic, NFData)

data ClashDesign = ClashDesign
  { designEntities :: [TopEntityT],
    designBindings :: BindingMap
  }

instance NFData ClashDesign where
  rnf design =
    designEntities design
      `seq` designBindings design
      `deepseq` ()

data IsPrim
  = -- | The binding is the unfolding for a primitive.
    IsPrim
  | -- | The binding is an ordinary function.
    IsFun
  deriving (Binary, Eq, Generic, NFData, Show)

-- A function binder in the global environment.
--
data Binding a = Binding
  { -- | The core identifier for this binding.
    bindingId :: Id,
    -- | The source location of this binding in the original source code.
    bindingLoc :: SrcSpan,
    -- | the inline specification for this binding, in the original source code.
    bindingSpec :: InlineSpec,
    -- | Is the binding a core term corresponding to a primitive with a known
    -- implementation? If so, it can potentially be inlined despite being
    -- marked as NOINLINE in source.
    bindingIsPrim :: IsPrim,
    -- | The term representation for this binding. This is polymorphic so
    -- alternate representations can be used if more appropriate (i.e. in the
    -- evaluator this can be Value for evaluated bindings).
    bindingTerm :: a,
    -- | Whether the binding is recursive.
    --
    -- TODO Ideally the BindingMap would store recursive and non-recursive
    -- bindings in a way similar to Let / Letrec. GHC also does this.
    bindingRecursive :: Bool
  }
  deriving (Binary, Functor, Generic, NFData, Show)

-- | Global function binders
--
-- Global functions cannot be mutually recursive, only self-recursive.
type BindingMap = VarEnv (Binding Term)

type DomainMap = HashMap Text VDomainConfiguration

-- | Information to show about transformations during compilation.
--
-- __NB__: The @Ord@ instance compares by amount of information.
data TransformationInfo
  = -- | Show no information about transformations.
    None
  | -- | Show the final term after all applied transformations.
    FinalTerm
  | -- | Show the name of every transformation that is applied.
    AppliedName
  | -- | Show the name and result of every transformation that is applied.
    AppliedTerm
  | -- | Show the name of every transformation that is attempted, and the result
    -- of every transformation that is applied.
    TryName
  | -- | Show the name and input to every transformation that is applied, and
    -- the result of every transformation that is applied.
    TryTerm
  deriving (Eq, Generic, Hashable, Ord, Read, Show, NFData)

-- | Options related to debugging. See 'ClashOpts'
data DebugOpts = DebugOpts
  { -- | Check that the results of applied transformations do not violate the
    -- invariants for rewriting (e.g. no accidental shadowing, or type changes).
    --
    -- Command line flag: -fclash-debug-invariants
    dbg_invariants :: Bool,
    -- | The information to show when debugging a transformation. See the
    -- 'TransformationInfo' type for different configurations.
    --
    -- Command line flag: -fclash-debug-info (None|FinalTerm|AppliedName|AppliedTerm|TryName|TryTerm)
    dbg_transformationInfo :: TransformationInfo,
    -- | List the transformations that are being debugged. When the set is empty,
    -- all transformations are debugged.
    --
    -- Command line flag: -fclash-debug-transformations t1[,t2...]
    dbg_transformations :: Set String,
    -- | Count how many times transformations are applied and provide a summary
    -- at the end of normalization. This includes all transformations, not just
    -- those in 'dbg_transformations'.
    --
    -- Command line flag: -fclash-debug-count-transformations
    dbg_countTransformations :: Bool,
    -- | Debug transformations applied after the nth transformation applied. This
    -- includes all transformations, not just those in 'dbg_transformations'.
    --
    -- Command line flag: -fclash-debug-transformations-from=N
    dbg_transformationsFrom :: Maybe Word,
    -- | Debug up to the nth applied transformation. If this limit is exceeded
    -- then Clash will error. This includes all transformations, not just those
    -- in 'dbg_transformations'.
    --
    -- Command line flag: -fclash-debug-transformations-limit=N
    dbg_transformationsLimit :: Maybe Word,
    -- | Save information about all applied transformations to a history file
    -- for use with @clash-term@.
    --
    -- Command line flag: -fclash-debug-history[=FILE]
    dbg_historyFile :: Maybe FilePath
  }
  deriving (Generic, NFData, Show, Eq)

instance Hashable DebugOpts where
  hashWithSalt s DebugOpts {..} =
    s
      `hashWithSalt` dbg_invariants
      `hashWithSalt` dbg_transformationInfo
        `hashSet` dbg_transformations
      `hashWithSalt` dbg_countTransformations
      `hashWithSalt` dbg_transformationsFrom
      `hashWithSalt` dbg_transformationsLimit
      `hashWithSalt` dbg_historyFile
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
isDebugging opts =
  or
    [ dbg_invariants opts,
      dbg_transformationInfo opts > None,
      dbg_countTransformations opts,
      isJust (dbg_transformationsLimit opts)
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
debugNone =
  DebugOpts
    { dbg_invariants = False,
      dbg_transformationInfo = None,
      dbg_transformations = Set.empty,
      dbg_countTransformations = False,
      dbg_transformationsFrom = Nothing,
      dbg_transformationsLimit = Nothing,
      dbg_historyFile = Nothing
    }

-- | -fclash-debug DebugSilent
debugSilent :: DebugOpts
debugSilent = debugNone {dbg_invariants = True}

-- | -fclash-debug DebugFinal
debugFinal :: DebugOpts
debugFinal = debugSilent {dbg_transformationInfo = FinalTerm}

-- | -fclash-debug DebugCount
debugCount :: DebugOpts
debugCount = debugFinal {dbg_countTransformations = True}

-- | -fclash-debug DebugName
debugName :: DebugOpts
debugName = debugCount {dbg_transformationInfo = AppliedName}

-- | -fclash-debug DebugTry
debugTry :: DebugOpts
debugTry = debugName {dbg_transformationInfo = TryName}

-- | -fclash-debug DebugApplied
debugApplied :: DebugOpts
debugApplied = debugTry {dbg_transformationInfo = AppliedTerm}

-- | -fclash-debug DebugAll
debugAll :: DebugOpts
debugAll = debugApplied {dbg_transformationInfo = TryTerm}

-- | Options passed to Clash compiler
data ClashOpts = ClashOpts
  { -- | Are warnings treated as errors.
    --
    -- Command line flag: -Werror
    opt_werror :: Bool,
    -- | Change the number of times a function f can undergo inlining inside
    -- some other function g. This prevents the size of g growing dramatically.
    --
    -- Command line flag: -fclash-inline-limit
    opt_inlineLimit :: Int,
    -- | Change the number of times a function can undergo specialization.
    --
    -- Command line flag: -fclash-spec-limit
    opt_specLimit :: Int,
    -- | Set the threshold for function size. Below this threshold functions are
    -- always inlined (if it is not recursive).
    --
    -- Command line flag: -fclash-inline-function-limit
    opt_inlineFunctionLimit :: Word,
    -- | Set the threshold for constant size. Below this threshold constants are
    -- always inlined. A value of 0 inlines all constants.
    --
    -- Command line flag: -fclash-inline-constant-limit
    opt_inlineConstantLimit :: Word,
    -- | Set the threshold for maximum unfolding depth in the evaluator. A value
    -- of zero means no potentially non-terminating binding is unfolded.
    --
    -- Command line flag: -fclash-evaluator-fuel-limit
    opt_evaluatorFuelLimit :: Word,
    -- | Options which control debugging. See 'DebugOpts'.
    opt_debug :: DebugOpts,
    -- | What the GHC debug level is (i.e. @-g<N>@).
    --
    -- Clash uses this to decide whether to emit source-location information in
    -- generated HDL.
    opt_ghcDebugLevel :: Int,
    -- | Reuse previously generated output from Clash. Only caches topentities.
    --
    -- Command line flag: -fclash-no-cache
    opt_cachehdl :: Bool,
    -- | Remove HDL directories before writing to them. By default, Clash will
    -- only write to non-empty directories if it can prove all files in it are
    -- generated by a previous run. This option applies to directories of the
    -- various top entities, i.e., the subdirectories made in the directory passed
    -- in with @-fclash-hdldir@. Note that Clash will still use a cache if it can.
    --
    -- Command line flag: @-fclash-clear@
    opt_clear :: Bool,
    -- | Which warnings are enabled, and which of them are treated as errors.
    --
    -- Command line flags: -W\<name\>, -Wno-\<name\>, -Werror=\<name\>,
    -- -Wwarn=\<name\>, -Wno-error=\<name\>. See "Clash.Warning".
    opt_warnings :: WarningOpts,
    -- | Show colors in debug output
    --
    -- Command line flag: -fdiagnostics-color
    opt_color :: OverridingBool,
    -- | Set the bit width for the Int\/Word\/Integer types. The only allowed values
    -- are 32 or 64.
    opt_intWidth :: Int,
    -- | Directory to save HDL files to
    opt_hdlDir :: Maybe String,
    -- | Synthesis target. See "HdlSyn" for available options.
    opt_hdlSyn :: HdlSyn,
    -- | Show additional information in error messages
    opt_errorExtra :: Bool,
    -- | Paths where Clash should look for modules
    opt_importPaths :: [FilePath],
    -- | Prefix components with given string
    opt_componentPrefix :: Maybe Text,
    -- | Use new inline strategy. Functions marked NOINLINE will get their own
    -- HDL module.
    opt_newInlineStrat :: Bool,
    -- | Use escaped identifiers in HDL. See:
    --
    --  * https://peterfab.com/ref/vhdl/vhdl_renerta/source/vhd00037.htm
    --  * https://peterfab.com/ref/verilog/verilog_renerta/source/vrg00018.htm
    opt_escapedIds :: Bool,
    -- | Force all generated basic identifiers to lowercase. Among others, this
    -- affects module and file names.
    opt_lowerCaseBasicIds :: PreserveCase,
    -- | Perform a high-effort compile, trading improved performance for
    -- potentially much longer compile times.
    --
    -- Name inspired by Design Compiler's /compile_ultra/ flag.
    opt_ultra :: Bool,
    -- |
    -- * /Nothing/: generate undefined's in the HDL
    --
    -- * /Just Nothing/: replace undefined's by a constant in the HDL; the
    -- compiler decides what's best
    --
    -- * /Just (Just x)/: replace undefined's by /x/ in the HDL
    opt_forceUndefined :: Maybe (Maybe Int),
    -- | Check whether paths specified in 'opt_importPaths' exists on the
    -- filesystem.
    opt_checkIDir :: Bool,
    -- | Enable aggressive X optimization, which may remove undefineds from
    -- generated HDL by replaced with defined alternatives.
    opt_aggressiveXOpt :: Bool,
    -- | Enable aggressive X optimization, which may remove undefineds from
    -- HDL generated by blackboxes. This enables the ~ISUNDEFINED template tag.
    opt_aggressiveXOptBB :: Bool,
    -- | At what size do we cache normalized work-free top-level binders.
    opt_inlineWFCacheLimit :: Word,
    -- | Generate an EDAM file for use with Edalize.
    opt_edalize :: Bool,
    -- | Render sum types with all zero-width fields as enums where supported, as
    -- opposed to rendering them as bitvectors.
    opt_renderEnums :: Bool,
    -- | Timescale precision set in Verilog files. E.g., setting this would sets
    -- the second part of @`timescale 100fs/100fs@.
    opt_timescalePrecision :: Period,
    -- | Don't error if we see a (potentially) broken GHC / platform combination.
    -- See the project's @README.md@ for more information.
    opt_ignoreBrokenGhcs :: Bool,
    -- | Compile top entities concurrently. Disabling this is useful when
    -- investigating bugs, because it will make log output deterministic.
    --
    -- Command line flag: -fclash-no-concurrent-topentity-compilation
    opt_concurrentTopEntities :: Bool,
    -- | Emit a @__debug_hash@ field in @clash-manifest.json@ containing the
    -- SHA256 of each input that feeds into the top-level @hash@. This is a
    -- debugging aid for diagnosing cache misses across builds that produce
    -- identical HDL: diffing two manifests' @__debug_hash@ fields reveals
    -- /which/ input changed.
    --
    -- Command line flag: -fclash-debug-manifest-hash
    opt_debugManifestHash :: Bool
  }
  deriving (Show, Eq, NFData, Generic, Hashable)

defClashOpts :: ClashOpts
defClashOpts =
  ClashOpts
    { opt_werror = False,
      opt_inlineLimit = 20,
      opt_specLimit = 20,
      opt_inlineFunctionLimit = 15,
      opt_inlineConstantLimit = 0,
      opt_evaluatorFuelLimit = 20,
      opt_debug = debugNone,
      opt_ghcDebugLevel = 0,
      opt_cachehdl = True,
      opt_clear = False,
      opt_warnings = defWarningOpts,
      opt_color = Auto,
      opt_intWidth = WORD_SIZE_IN_BITS,
      opt_hdlDir = Nothing,
      opt_hdlSyn = Other,
      opt_errorExtra = False,
      opt_importPaths = [],
      opt_componentPrefix = Nothing,
      opt_newInlineStrat = True,
      opt_escapedIds = True,
      opt_lowerCaseBasicIds = PreserveCase,
      opt_ultra = False,
      opt_forceUndefined = Nothing,
      opt_checkIDir = True,
      opt_aggressiveXOpt = False,
      opt_aggressiveXOptBB = False,
      opt_inlineWFCacheLimit = 10, -- TODO: find "optimal" value
      opt_edalize = False,
      opt_renderEnums = True,
      opt_timescalePrecision = Period 100 Fs,
      -- XXX: We probe environment variables until we've found a proper solution to
      --      https://github.com/clash-lang/clash-compiler/issues/2762.
      opt_ignoreBrokenGhcs = unsafeLookupEnvBool "CLASH_IGNORE_BROKEN_GHCS" False,
      opt_concurrentTopEntities = True,
      opt_debugManifestHash = False
    }

-- | Monads that can get at the options Clash was invoked with.
class (Monad m) => HasClashOpts m where
  askClashOpts :: m ClashOpts

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
      let p = MkFixed (toInteger $ vPeriod dom) :: Fixed E3
          name = braces (pretty (Text.dropAround (== '\\') i))
          period = viaShow p
          waveform = braces ("0.000" <+> viaShow (p / 2))
          targets = brackets ("get_ports" <+> name)
       in hsep
            [ "create_clock",
              "-name" <+> name,
              "-period" <+> period,
              "-waveform" <+> waveform,
              targets
            ]
