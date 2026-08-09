{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Named warnings and the options controlling them.

  Every warning Clash can emit has a name and can be controlled individually
  with GHC-style flags:

    [@-W\<name\>@] enable the warning
    [@-Wno-\<name\>@] disable the warning
    [@-Werror=\<name\>@] enable the warning and promote it to an error
    [@-Wwarn=\<name\>@, @-Wno-error=\<name\>@] demote the warning back to a
    warning, also exempting it from a global @-Werror@

  Note that Clash parses its flags in a separate pass from GHC's, so ordering
  between GHC's global @-Werror@ and @-Wwarn=\<name\>@ is not positional: an
  explicit @-Wwarn=\<name\>@ always wins over a global @-Werror@. Ordering
  among the Clash warning flags themselves is positional (last one wins).
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Clash.Warning
  ( ClashWarning(..)
  , warningName
  , parseWarningName
    -- * Warning options
  , WarningOpts(..)
  , defWarningOpts
  , wopt
  , woptFatal
    -- * Flag-parsing state transitions
  , enableWarning
  , disableWarning
  , promoteWarning
  , demoteWarning
  ) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Set (Set)
import GHC.Generics (Generic)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Warnings Clash can emit. See the module documentation of "Clash.Warning"
-- for the command line flags controlling each of these.
data ClashWarning
  = WarnDubiousPrimitive
  -- ^ A primitive marked with @WarnAlways@ was instantiated, e.g. a primitive
  -- that only approximates its Haskell model.
  --
  -- Flag: @-Wclash-dubious-primitive@
  | WarnNonSynthesizable
  -- ^ A primitive marked with @WarnNonSynthesizable@ was instantiated outside
  -- of a test bench context.
  --
  -- Flag: @-Wclash-non-synthesizable@
  | WarnPrimitiveDefinition
  -- ^ A primitive's Haskell definition looks problematic: it isn't marked
  -- OPAQUE, its result is always an error, or its blackbox uses arguments the
  -- Haskell definition doesn't use.
  --
  -- Flag: @-Wclash-primitive-definition@
  | WarnCastSpecialization
  -- ^ A function is specialized on a non work-free cast, possibly duplicating
  -- work.
  --
  -- Flag: @-Wclash-cast-specialization@
  | WarnIntegerNarrowing
  -- ^ A @toInteger@ conversion narrows its argument to the width of 'Int',
  -- possibly dropping most significant bits.
  --
  -- Flag: @-Wclash-integer-narrowing@
  | WarnUnmatchableConstant
  -- ^ A case subject evaluated to a constant that matches none of the
  -- alternatives, usually a missing reduction rule in the primitive evaluator.
  -- Only reported when invariants are being checked (@-fclash-debug@).
  --
  -- Flag: @-Wclash-unmatchable-constant@
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, NFData, Hashable)

-- | The name of a warning as used in command line flags, e.g.
-- @clash-dubious-primitive@ for 'WarnDubiousPrimitive'.
warningName :: ClashWarning -> String
warningName = \case
  WarnDubiousPrimitive -> "clash-dubious-primitive"
  WarnNonSynthesizable -> "clash-non-synthesizable"
  WarnPrimitiveDefinition -> "clash-primitive-definition"
  WarnCastSpecialization -> "clash-cast-specialization"
  WarnIntegerNarrowing -> "clash-integer-narrowing"
  WarnUnmatchableConstant -> "clash-unmatchable-constant"

-- | Inverse of 'warningName'
parseWarningName :: String -> Maybe ClashWarning
parseWarningName = flip Map.lookup warningsByName
 where
  warningsByName :: Map String ClashWarning
  warningsByName =
    Map.fromList [(warningName w, w) | w <- [minBound .. maxBound]]

-- | Which warnings are enabled, and which of them are fatal. Construct with
-- 'defWarningOpts' and the state transitions below; query with 'wopt' and
-- 'woptFatal'.
data WarningOpts = WarningOpts
  { warn_enabled :: Set ClashWarning
  -- ^ Warnings that are enabled. All warnings are enabled by default.
  , warn_fatal :: Set ClashWarning
  -- ^ Warnings explicitly promoted to errors with @-Werror=\<name\>@
  , warn_nonFatal :: Set ClashWarning
  -- ^ Warnings explicitly demoted with @-Wwarn=\<name\>@; these are also
  -- exempt from a global @-Werror@
  }
  deriving (Show, Eq, Generic, NFData, Hashable)

-- | All warnings enabled, none fatal
defWarningOpts :: WarningOpts
defWarningOpts = WarningOpts
  { warn_enabled = Set.fromList [minBound .. maxBound]
  , warn_fatal = Set.empty
  , warn_nonFatal = Set.empty
  }

-- | Is the given warning enabled?
wopt :: ClashWarning -> WarningOpts -> Bool
wopt w opts = w `Set.member` warn_enabled opts

-- | Should the given warning be promoted to an error? The first argument
-- indicates whether /all/ warnings should be treated as errors (@-Werror@).
woptFatal :: Bool -> ClashWarning -> WarningOpts -> Bool
woptFatal werror w opts =
  (werror || w `Set.member` warn_fatal opts)
    && w `Set.notMember` warn_nonFatal opts

-- | @-W\<name\>@
enableWarning :: ClashWarning -> WarningOpts -> WarningOpts
enableWarning w opts =
  opts { warn_enabled = Set.insert w (warn_enabled opts) }

-- | @-Wno-\<name\>@
disableWarning :: ClashWarning -> WarningOpts -> WarningOpts
disableWarning w opts = opts
  { warn_enabled = Set.delete w (warn_enabled opts)
  , warn_fatal = Set.delete w (warn_fatal opts)
  }

-- | @-Werror=\<name\>@. Implies @-W\<name\>@, like in GHC.
promoteWarning :: ClashWarning -> WarningOpts -> WarningOpts
promoteWarning w opts = opts
  { warn_enabled = Set.insert w (warn_enabled opts)
  , warn_fatal = Set.insert w (warn_fatal opts)
  , warn_nonFatal = Set.delete w (warn_nonFatal opts)
  }

-- | @-Wwarn=\<name\>@ / @-Wno-error=\<name\>@
demoteWarning :: ClashWarning -> WarningOpts -> WarningOpts
demoteWarning w opts = opts
  { warn_fatal = Set.delete w (warn_fatal opts)
  , warn_nonFatal = Set.insert w (warn_nonFatal opts)
  }
