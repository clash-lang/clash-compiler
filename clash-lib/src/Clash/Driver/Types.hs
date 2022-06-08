{-|
  Copyright  :  (C) 2013-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017     , QBayLogic, Google Inc.
                    2020-2022, QBayLogic
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Type definitions used by the Driver module
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Driver.Types where

-- For Int/Word size
#include "MachDeps.h"

import           Control.DeepSeq                (NFData(rnf), deepseq)
import           Control.Lens                   (makeLenses)
import           Data.Binary                    (Binary)
import           Data.Fixed
import           Data.Hashable
import           Data.HashMap.Strict            (HashMap)
import           Data.IntMap.Strict             (IntMap)
import           Data.Maybe                     (isJust)
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text as Text              (dropAround)

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

import           Clash.Annotations.BitRepresentation.Internal (CustomReprs)
import           Clash.Signal.Internal

import           Clash.Core.Term                (Term)
import           Clash.Core.TyCon               (TyConMap, TyConName)
import           Clash.Core.Var                 (Id)
import           Clash.Core.VarEnv              (VarEnv)
import           Clash.Netlist.BlackBox.Types   (HdlSyn (..))
import {-# SOURCE #-} Clash.Netlist.Types       (PreserveCase(..), TopEntityT)
import           Clash.Primitives.Types         (CompiledPrimMap)

data ClashDesign = ClashDesign
  { designEntities :: [TopEntityT]
  , designDomains  :: DomainMap
  , designBindings :: BindingMap
  }

instance NFData ClashDesign where
  rnf design =
    designEntities design `seq`
    designDomains design `deepseq`
    designBindings design `deepseq`
    ()

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
  , bindingRecursive :: Bool
    -- ^ Whether the binding is recursive.
    --
    -- TODO Ideally the BindingMap would store recursive and non-recursive
    -- bindings in a way similar to Let / Letrec. GHC also does this.
  } deriving (Binary, Functor, Generic, NFData, Show)

-- | Global function binders
--
-- Global functions cannot be mutually recursive, only self-recursive.
type BindingMap = VarEnv (Binding Term)
type DomainMap = HashMap Text VDomainConfiguration

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
  deriving (Eq, Generic, Hashable, Ord, Read, Show, NFData)

-- | Options related to debugging. See 'ClashOpts'
data DebugOpts = DebugOpts
  { _dbg_invariants :: Bool
  -- ^ Check that the results of applied transformations do not violate the
  -- invariants for rewriting (e.g. no accidental shadowing, or type changes).
  --
  -- Command line flag: -fclash-debug-invariants
  , _dbg_transformationInfo :: TransformationInfo
  -- ^ The information to show when debugging a transformation. See the
  -- 'TransformationInfo' type for different configurations.
  --
  -- Command line flag: -fclash-debug-info (None|FinalTerm|AppliedName|AppliedTerm|TryName|TryTerm)
  , _dbg_transformations :: Set String
  -- ^ List the transformations that are being debugged. When the set is empty,
  -- all transformations are debugged.
  --
  -- Command line flag: -fclash-debug-transformations t1[,t2...]
  , _dbg_countTransformations :: Bool
  -- ^ Count how many times transformations are applied and provide a summary
  -- at the end of normalization. This includes all transformations, not just
  -- those in '_dbg_transformations'.
  --
  -- Command line flag: -fclash-debug-count-transformations
  , _dbg_transformationsFrom :: Maybe Word
  -- ^ Debug transformations applied after the nth transformation applied. This
  -- includes all transformations, not just those in '_dbg_transformations'.
  --
  -- Command line flag: -fclash-debug-transformations-from=N
  , _dbg_transformationsLimit :: Maybe Word
  -- ^ Debug up to the nth applied transformation. If this limit is exceeded
  -- then Clash will error. This includes all transformations, not just those
  -- in '_dbg_transformations'.
  --
  -- Command line flag: -fclash-debug-transformations-limit=N
  , _dbg_historyFile :: Maybe FilePath
  -- ^ Save information about all applied transformations to a history file
  -- for use with @clash-term@.
  --
  -- Command line flag: -fclash-debug-history[=FILE]
  } deriving (Generic, NFData, Show, Eq)
makeLenses ''DebugOpts

instance Hashable DebugOpts where
  hashWithSalt s DebugOpts{..} =
    s `hashWithSalt`
    _dbg_invariants `hashWithSalt`
    _dbg_transformationInfo `hashSet`
    _dbg_transformations `hashWithSalt`
    _dbg_countTransformations `hashWithSalt`
    _dbg_transformationsFrom `hashWithSalt`
    _dbg_transformationsLimit `hashWithSalt`
    _dbg_historyFile
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
  [ _dbg_invariants opts
  , _dbg_transformationInfo opts > None
  , _dbg_countTransformations opts
  , isJust (_dbg_transformationsLimit opts)
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
    let set = _dbg_transformations opts
     in Set.null set || Set.member n set

-- | Check that the transformation info shown supports the requested info.
-- If the call-site is in the context of a particular transformation,
-- 'hasDebugInfo' should be used instead.
hasTransformationInfo :: TransformationInfo -> DebugOpts -> Bool
hasTransformationInfo info opts =
  info <= _dbg_transformationInfo opts

-- NOTE [debugging options]
--
-- The preset debugging options here provide backwards compatibility with the
-- old style DebugLevel enum. However it is also possible to have finer-grained
-- control over debugging by using individual flags which did not previously
-- exist, e.g. -fclash-debug-invariants.

-- | -fclash-debug DebugNone
debugNone :: DebugOpts
debugNone = DebugOpts
  { _dbg_invariants = False
  , _dbg_transformationInfo = None
  , _dbg_transformations = Set.empty
  , _dbg_countTransformations = False
  , _dbg_transformationsFrom = Nothing
  , _dbg_transformationsLimit = Nothing
  , _dbg_historyFile = Nothing
  }

-- | -fclash-debug DebugSilent
debugSilent :: DebugOpts
debugSilent = debugNone { _dbg_invariants = True }

-- | -fclash-debug DebugFinal
debugFinal :: DebugOpts
debugFinal = debugSilent { _dbg_transformationInfo = FinalTerm }

-- | -fclash-debug DebugCount
debugCount :: DebugOpts
debugCount = debugFinal { _dbg_countTransformations = True }

-- | -fclash-debug DebugName
debugName :: DebugOpts
debugName = debugCount { _dbg_transformationInfo = AppliedName }

-- | -fclash-debug DebugTry
debugTry :: DebugOpts
debugTry = debugName { _dbg_transformationInfo = TryName }

-- | -fclash-debug DebugApplied
debugApplied :: DebugOpts
debugApplied = debugTry { _dbg_transformationInfo = AppliedTerm }

-- | -fclash-debug DebugAll
debugAll :: DebugOpts
debugAll = debugApplied { _dbg_transformationInfo = TryTerm }

-- | Options passed to Clash compiler
data ClashOpts = ClashOpts
  { _opt_werror :: Bool
  -- ^ Are warnings treated as errors.
  --
  -- Command line flag: -Werror
  , _opt_inlineLimit :: Int
  -- ^ Change the number of times a function f can undergo inlining inside
  -- some other function g. This prevents the size of g growing dramatically.
  --
  -- Command line flag: -fclash-inline-limit
  , _opt_specLimit :: Int
  -- ^ Change the number of times a function can undergo specialization.
  --
  -- Command line flag: -fclash-spec-limit
  , _opt_inlineFunctionLimit :: Word
  -- ^ Set the threshold for function size. Below this threshold functions are
  -- always inlined (if it is not recursive).
  --
  -- Command line flag: -fclash-inline-function-limit
  , _opt_inlineConstantLimit :: Word
  -- ^ Set the threshold for constant size. Below this threshold constants are
  -- always inlined. A value of 0 inlines all constants.
  --
  -- Command line flag: -fclash-inline-constant-limit
  , _opt_evaluatorFuelLimit :: Word
  -- ^ Set the threshold for maximum unfolding depth in the evaluator. A value
  -- of zero means no potentially non-terminating binding is unfolded.
  --
  -- Command line flag: -fclash-evaluator-fuel-limit
  , _opt_debug :: DebugOpts
  -- ^ Options which control debugging. See 'DebugOpts'.
  , _opt_cachehdl :: Bool
  -- ^ Reuse previously generated output from Clash. Only caches topentities.
  --
  -- Command line flag: -fclash-no-cache
  , _opt_clear :: Bool
  -- ^ Remove HDL directories before writing to them. By default, Clash will
  -- only write to non-empty directories if it can prove all files in it are
  -- generated by a previous run. This option applies to directories of the
  -- various top entities, i.e., the subdirectories made in the directory passed
  -- in with @-fclash-hdldir@. Note that Clash will still use a cache if it can.
  --
  -- Command line flag: @-fclash-clear@
  , _opt_primWarn :: Bool
  -- ^ Disable warnings for primitives
  --
  -- Command line flag: -fclash-no-prim-warn
  , _opt_color :: OverridingBool
  -- ^ Show colors in debug output
  --
  -- Command line flag: -fdiagnostics-color
  , _opt_intWidth :: Int
  -- ^ Set the bit width for the Int/Word/Integer types. The only allowed values
  -- are 32 or 64.
  , _opt_hdlDir :: Maybe String
  -- ^ Directory to save HDL files to
  , _opt_hdlSyn :: HdlSyn
  -- ^ Synthesis target. See "HdlSyn" for available options.
  , _opt_errorExtra :: Bool
  -- ^ Show additional information in error messages
  , _opt_importPaths :: [FilePath]
  -- ^ Paths where Clash should look for modules
  , _opt_componentPrefix :: Maybe Text
  -- ^ Prefix components with given string
  , _opt_newInlineStrat :: Bool
  -- ^ Use new inline strategy. Functions marked NOINLINE will get their own
  -- HDL module.
  , _opt_escapedIds :: Bool
  -- ^ Use escaped identifiers in HDL. See:
  --
  --  * http://vhdl.renerta.com/mobile/source/vhd00037.htm
  --  * http://verilog.renerta.com/source/vrg00018.htm
  , _opt_lowerCaseBasicIds :: PreserveCase
  -- ^ Force all generated basic identifiers to lowercase. Among others, this
  -- affects module and file names.
  , _opt_ultra :: Bool
  -- ^ Perform a high-effort compile, trading improved performance for
  -- potentially much longer compile times.
  --
  -- Name inspired by Design Compiler's /compile_ultra/ flag.
  , _opt_forceUndefined :: Maybe (Maybe Int)
  -- ^
  -- * /Nothing/: generate undefined's in the HDL
  --
  -- * /Just Nothing/: replace undefined's by a constant in the HDL; the
  -- compiler decides what's best
  --
  -- * /Just (Just x)/: replace undefined's by /x/ in the HDL
  , _opt_checkIDir :: Bool
  -- ^ Check whether paths specified in '_opt_importPaths' exists on the
  -- filesystem.
  , _opt_aggressiveXOpt :: Bool
  -- ^ Enable aggressive X optimization, which may remove undefineds from
  -- generated HDL by replaced with defined alternatives.
  , _opt_aggressiveXOptBB :: Bool
  -- ^ Enable aggressive X optimization, which may remove undefineds from
  -- HDL generated by blackboxes. This enables the ~ISUNDEFINED template tag.
  , _opt_inlineWFCacheLimit :: Word
  -- ^ At what size do we cache normalized work-free top-level binders.
  , _opt_edalize :: Bool
  -- ^ Generate an EDAM file for use with Edalize.
  , _opt_renderEnums :: Bool
  -- ^ Render sum types with all zero-width fields as enums where supported, as
  -- opposed to rendering them as bitvectors.
  }
  deriving (Show)
makeLenses ''ClashOpts

instance NFData ClashOpts where
  rnf o =
    _opt_werror o `deepseq`
    _opt_inlineLimit o `deepseq`
    _opt_specLimit o `deepseq`
    _opt_inlineFunctionLimit o `deepseq`
    _opt_inlineConstantLimit o `deepseq`
    _opt_evaluatorFuelLimit o `deepseq`
    _opt_cachehdl o `deepseq`
    _opt_clear o `deepseq`
    _opt_primWarn o `deepseq`
    _opt_color o `seq`
    _opt_intWidth o `deepseq`
    _opt_hdlDir o `deepseq`
    _opt_hdlSyn o `deepseq`
    _opt_errorExtra o `deepseq`
    _opt_importPaths o `deepseq`
    _opt_componentPrefix o `deepseq`
    _opt_newInlineStrat o `deepseq`
    _opt_escapedIds o `deepseq`
    _opt_lowerCaseBasicIds o `deepseq`
    _opt_ultra o `deepseq`
    _opt_forceUndefined o `deepseq`
    _opt_checkIDir o `deepseq`
    _opt_aggressiveXOpt o `deepseq`
    _opt_aggressiveXOptBB o `deepseq`
    _opt_inlineWFCacheLimit o `deepseq`
    _opt_edalize o `deepseq`
    _opt_renderEnums o `deepseq`
    ()

instance Eq ClashOpts where
  s0 == s1 =
    _opt_werror s0 == _opt_werror s1 &&
    _opt_inlineLimit s0 == _opt_inlineLimit s1 &&
    _opt_specLimit s0 == _opt_specLimit s1 &&
    _opt_inlineFunctionLimit s0 == _opt_inlineFunctionLimit s1 &&
    _opt_inlineConstantLimit s0 == _opt_inlineConstantLimit s1 &&
    _opt_evaluatorFuelLimit s0 == _opt_evaluatorFuelLimit s1 &&
    _opt_cachehdl s0 == _opt_cachehdl s1 &&
    _opt_clear s0 == _opt_clear s1 &&
    _opt_primWarn s0 == _opt_primWarn s1 &&
    (_opt_color s0 `eqOverridingBool` _opt_color s1) &&
    _opt_intWidth s0 == _opt_intWidth s1 &&
    _opt_hdlDir s0 == _opt_hdlDir s1 &&
    _opt_hdlSyn s0 == _opt_hdlSyn s1 &&
    _opt_errorExtra s0 == _opt_errorExtra s1 &&
    _opt_importPaths s0 == _opt_importPaths s1 &&
    _opt_componentPrefix s0 == _opt_componentPrefix s1 &&
    _opt_newInlineStrat s0 == _opt_newInlineStrat s1 &&
    _opt_escapedIds s0 == _opt_escapedIds s1 &&
    _opt_lowerCaseBasicIds s0 == _opt_lowerCaseBasicIds s1 &&
    _opt_ultra s0 == _opt_ultra s1 &&
    _opt_forceUndefined s0 == _opt_forceUndefined s1 &&
    _opt_checkIDir s0 == _opt_checkIDir s1 &&
    _opt_aggressiveXOpt s0 == _opt_aggressiveXOpt s1 &&
    _opt_aggressiveXOptBB s0 == _opt_aggressiveXOptBB s1 &&
    _opt_inlineWFCacheLimit s0 == _opt_inlineWFCacheLimit s1 &&
    _opt_edalize s0 == _opt_edalize s1 &&
    _opt_renderEnums s0 == _opt_renderEnums s1

   where
    eqOverridingBool :: OverridingBool -> OverridingBool -> Bool
    eqOverridingBool Auto Auto = True
    eqOverridingBool Always Always = True
    eqOverridingBool Never Never = True
    eqOverridingBool _ _ = False

instance Hashable ClashOpts where
  hashWithSalt s ClashOpts {..} =
    s `hashWithSalt`
    _opt_werror `hashWithSalt`
    _opt_inlineLimit `hashWithSalt`
    _opt_specLimit `hashWithSalt`
    _opt_inlineFunctionLimit `hashWithSalt`
    _opt_inlineConstantLimit `hashWithSalt`
    _opt_evaluatorFuelLimit `hashWithSalt`
    _opt_cachehdl `hashWithSalt`
    _opt_clear `hashWithSalt`
    _opt_primWarn `hashOverridingBool`
    _opt_color `hashWithSalt`
    _opt_intWidth `hashWithSalt`
    _opt_hdlDir `hashWithSalt`
    _opt_hdlSyn `hashWithSalt`
    _opt_errorExtra `hashWithSalt`
    _opt_importPaths `hashWithSalt`
    _opt_componentPrefix `hashWithSalt`
    _opt_newInlineStrat `hashWithSalt`
    _opt_escapedIds `hashWithSalt`
    _opt_lowerCaseBasicIds `hashWithSalt`
    _opt_ultra `hashWithSalt`
    _opt_forceUndefined `hashWithSalt`
    _opt_checkIDir `hashWithSalt`
    _opt_aggressiveXOpt `hashWithSalt`
    _opt_aggressiveXOptBB `hashWithSalt`
    _opt_inlineWFCacheLimit `hashWithSalt`
    _opt_edalize `hashWithSalt`
    _opt_renderEnums
   where
    hashOverridingBool :: Int -> OverridingBool -> Int
    hashOverridingBool s1 Auto = hashWithSalt s1 (0 :: Int)
    hashOverridingBool s1 Always = hashWithSalt s1 (1 :: Int)
    hashOverridingBool s1 Never = hashWithSalt s1 (2 :: Int)
    infixl 0 `hashOverridingBool`

defClashOpts :: ClashOpts
defClashOpts
  = ClashOpts
  { _opt_werror              = False
  , _opt_inlineLimit         = 20
  , _opt_specLimit           = 20
  , _opt_inlineFunctionLimit = 15
  , _opt_inlineConstantLimit = 0
  , _opt_evaluatorFuelLimit  = 20
  , _opt_debug               = debugNone
  , _opt_cachehdl            = True
  , _opt_clear               = False
  , _opt_primWarn            = True
  , _opt_color               = Auto
  , _opt_intWidth            = WORD_SIZE_IN_BITS
  , _opt_hdlDir              = Nothing
  , _opt_hdlSyn              = Other
  , _opt_errorExtra          = False
  , _opt_importPaths         = []
  , _opt_componentPrefix     = Nothing
  , _opt_newInlineStrat      = True
  , _opt_escapedIds          = True
  , _opt_lowerCaseBasicIds   = PreserveCase
  , _opt_ultra               = False
  , _opt_forceUndefined      = Nothing
  , _opt_checkIDir           = True
  , _opt_aggressiveXOpt      = False
  , _opt_aggressiveXOptBB    = False
  , _opt_inlineWFCacheLimit  = 10 -- TODO: find "optimal" value
  , _opt_edalize             = False
  , _opt_renderEnums         = True
  }

-- | Synopsys Design Constraint (SDC) information for a component.
-- Currently this limited to the names and periods of clocks for create_clock.
--
newtype SdcInfo = SdcInfo
  { sdcClock :: [(Text, VDomainConfiguration)]
  }

data ClashEnv = ClashEnv
  { envOpts        :: ClashOpts
  , envTyConMap    :: TyConMap
  , envTupleTyCons :: IntMap TyConName
  , envPrimitives  :: CompiledPrimMap
  , envCustomReprs :: CustomReprs
  } deriving (Generic, NFData)

-- | Render an SDC file from an SdcInfo.
-- The clock periods, waveforms, and targets are all hardcoded.
--
pprSDC :: SdcInfo -> Doc ()
pprSDC = vcat . fmap go . sdcClock
 where
  go (i, dom) =
        -- VDomainConfiguration stores period in ps, SDC expects it in ns.
    let p        = MkFixed (toInteger $ vPeriod dom) :: Fixed E3
        name     = braces (pretty (Text.dropAround (== '\\') i))
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
