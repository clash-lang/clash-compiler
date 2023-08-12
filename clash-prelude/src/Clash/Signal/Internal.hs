{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017-2019, Myrtle Software Ltd
                  2017-2022, Google Inc.,
                  2021-2023, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

-- See: https://github.com/clash-lang/clash-compiler/commit/721fcfa9198925661cd836668705f817bddaae3c
-- as to why we need this.
{-# OPTIONS_GHC -fno-cpr-anal #-}

{-# OPTIONS_HADDOCK show-extensions not-home #-}

module Clash.Signal.Internal
  ( -- * Datatypes
    Signal(..)
  , head#
  , tail#
    -- * Domains
  , Domain
  , sameDomain
  , KnownDomain(..)
  , KnownConfiguration
  , knownDomainByName
  , ActiveEdge(..)
  , SActiveEdge(..)
  , InitBehavior(..)
  , SInitBehavior(..)
  , ResetKind(..)
  , SResetKind(..)
  , ResetPolarity(..)
  , SResetPolarity(..)
  , DomainConfiguration(..)
  , SDomainConfiguration(..)
  -- ** Configuration type families
  , DomainPeriod
  , DomainActiveEdge
  , DomainResetKind
  , DomainInitBehavior
  , DomainResetPolarity

  , DomainConfigurationPeriod
  , DomainConfigurationActiveEdge
  , DomainConfigurationResetKind
  , DomainConfigurationInitBehavior
  , DomainConfigurationResetPolarity

    -- *** Convenience types

  , HasSynchronousReset
  , HasAsynchronousReset
  , HasDefinedInitialValues

    -- ** Default domains
  , System
  , XilinxSystem
  , IntelSystem
  , vSystem
  , vIntelSystem
  , vXilinxSystem
    -- ** Domain utilities
  , VDomainConfiguration(..)
  , vDomain
  , createDomain
    -- * Clocks
  , Clock (..)
  , ClockN (..)
  , DiffClock (..)
  , hzToPeriod
  , periodToHz
  , ClockAB (..)
  , clockTicks
  , clockTicksEither
    -- ** Enabling
  , Enable(..)
  , toEnable
  , fromEnable
  , enableGen
    -- * Resets
  , Reset(..)
  , unsafeToReset
  , unsafeFromReset
  , unsafeToActiveHigh
  , unsafeToActiveLow
  , unsafeFromActiveHigh
  , unsafeFromActiveLow
  , invertReset
    -- * Basic circuits
  , delay#
  , register#
  , asyncRegister#
  , syncRegister#
  , registerPowerup#
  , mux
    -- * Simulation and testbench functions
  , clockGen
  , tbClockGen
  , Femtoseconds(..)  -- experimental, do not expose in public API
  , fsToHz            -- experimental, do not expose in public API
  , hzToFs            -- experimental, do not expose in public API
  , unFemtoseconds    -- experimental, do not expose in public API
  , mapFemtoseconds   -- experimental, do not expose in public API
  , tbDynamicClockGen -- experimental, do not expose in public API
  , dynamicClockGen   -- experimental, do not expose in public API
  , resetGen
  , resetGenN
    -- * Boolean connectives
  , (.&&.), (.||.)
    -- * Simulation functions (not synthesizable)
  , simulate
    -- ** lazy version
  , simulate_lazy
    -- ** Automaton
  , signalAutomaton
    -- * List \<-\> Signal conversion (not synthesizable)
  , sample
  , sampleN
  , fromList
    -- ** lazy versions
  , sample_lazy
  , sampleN_lazy
  , fromList_lazy
    -- * QuickCheck combinators
  , testFor
    -- * Type classes
    -- ** 'Eq'-like
  , (.==.), (./=.)
    -- ** 'Ord'-like
  , (.<.), (.<=.), (.>=.), (.>.)
    -- ** 'Functor'
  , mapSignal#
    -- ** 'Applicative'
  , signal#
  , appSignal#
    -- ** 'Foldable'
  , foldr#
    -- ** 'Traversable'
  , traverse#
  -- * EXTREMELY EXPERIMENTAL
  , joinSignal#

  -- * Deprecated
  , unsafeFromHighPolarity
  , unsafeFromLowPolarity
  , unsafeToHighPolarity
  , unsafeToLowPolarity
  )
where

import Data.IORef                 (IORef, atomicModifyIORef, newIORef, readIORef)
import Type.Reflection            (Typeable)
import Control.Arrow.Transformer.Automaton
#if !MIN_VERSION_base(4,18,0)
import Control.Applicative        (liftA2)
#endif
import Control.Applicative        (liftA3)
import Control.DeepSeq            (NFData)
import Clash.Annotations.Primitive (hasBlackBox, dontTranslate)
import Data.Binary                (Binary)
import Data.Char                  (isAsciiUpper, isAlphaNum, isAscii)
import Data.Coerce                (coerce)
import Data.Data                  (Data)
import Data.Default.Class         (Default (..))
import Data.Hashable              (Hashable)
import Data.Int                   (Int64)
import Data.Maybe                 (isJust)
import Data.Proxy                 (Proxy(..))
import Data.Ratio                 (Ratio)
import Data.Type.Equality         ((:~:))
import GHC.Generics               (Generic)
import GHC.Stack                  (HasCallStack, withFrozenCallStack)
import GHC.TypeLits               (KnownSymbol, KnownNat, Nat, Symbol, type (<=), sameSymbol)
import Language.Haskell.TH.Syntax -- (Lift (..), Q, Dec)
import Language.Haskell.TH.Compat
import Numeric.Natural            (Natural)
import System.IO.Unsafe           (unsafeInterleaveIO, unsafePerformIO)
import Test.QuickCheck            (Arbitrary (..), CoArbitrary(..), Property,
                                   property)

import Clash.CPP                  (fStrictMapSignal)
import Clash.NamedTypes
import Clash.Promoted.Nat         (SNat (..), snatToNum, snatToNatural)
import Clash.Promoted.Symbol      (SSymbol (..), ssymbolToString)
import Clash.XException
  (NFDataX(..), errorX, isX, deepseqX, defaultSeqX, seqX)

{- $setup
>>> :set -XDataKinds
>>> :set -XMagicHash
>>> :set -XTypeApplications
>>> import Clash.Prelude (SSymbol(..))
>>> import Clash.Signal.Internal
>>> import Clash.Promoted.Nat
>>> import Clash.Promoted.Nat.Literals
>>> import Clash.XException
>>> import Data.Ratio (Ratio)
>>> import Numeric.Natural (Natural)
>>> type System = "System"
>>> let systemClockGen = clockGen @System
>>> let systemResetGen = resetGen @System
>>> import Clash.Explicit.Signal (register)
>>> let registerS = register
>>> let registerA = register
-}

-- * Signal

-- | Determines clock edge memory elements are sensitive to. Not yet
-- implemented.
data ActiveEdge
  -- TODO: Implement in blackboxes:
  = Rising
  -- ^ Elements are sensitive to the rising edge (low-to-high) of the clock.
  | Falling
  -- ^ Elements are sensitive to the falling edge (high-to-low) of the clock.
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Hashable, Binary)

-- | Singleton version of 'ActiveEdge'
data SActiveEdge (edge :: ActiveEdge) where
  SRising  :: SActiveEdge 'Rising
  SFalling :: SActiveEdge 'Falling

instance Show (SActiveEdge edge) where
  show SRising = "SRising"
  show SFalling = "SFalling"

data ResetKind
  = Asynchronous
  -- ^ Elements respond /asynchronously/ to changes in their reset input. This
  -- means that they do /not/ wait for the next active clock edge, but respond
  -- immediately instead. Common on Intel FPGA platforms.
  | Synchronous
  -- ^ Elements respond /synchronously/ to changes in their reset input. This
  -- means that changes in their reset input won't take effect until the next
  -- active clock edge. Common on Xilinx FPGA platforms.
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Hashable, Binary)

-- | Singleton version of 'ResetKind'
data SResetKind (resetKind :: ResetKind) where
  SAsynchronous :: SResetKind 'Asynchronous
  -- See 'Asynchronous' ^

  SSynchronous  :: SResetKind 'Synchronous
  -- See 'Synchronous' ^

instance Show (SResetKind reset) where
  show SAsynchronous = "SAsynchronous"
  show SSynchronous = "SSynchronous"

-- | Determines the value for which a reset line is considered "active"
data ResetPolarity
  = ActiveHigh
  -- ^ Reset is considered active if underlying signal is 'True'.
  | ActiveLow
  -- ^ Reset is considered active if underlying signal is 'False'.
  deriving (Eq, Ord, Show, Read, Generic, NFData, Data, Hashable, Binary)

-- | Singleton version of 'ResetPolarity'
data SResetPolarity (polarity :: ResetPolarity) where
  SActiveHigh :: SResetPolarity 'ActiveHigh
  -- See: 'ActiveHigh' ^

  SActiveLow :: SResetPolarity 'ActiveLow
  -- See: 'ActiveLow' ^

instance Show (SResetPolarity polarity) where
  show SActiveHigh = "SActiveHigh"
  show SActiveLow = "SActiveLow"

data InitBehavior
  = Unknown
  -- ^ Power up value of memory elements is /unknown/.
  | Defined
  -- ^ If applicable, power up value of a memory element is defined. Applies to
  -- 'Clash.Signal.register's for example, but not to
  -- 'Clash.Prelude.BlockRam.blockRam'.
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Hashable, Binary)

data SInitBehavior (init :: InitBehavior) where
  SUnknown :: SInitBehavior 'Unknown
  -- See: 'Unknown' ^

  SDefined :: SInitBehavior 'Defined
  -- See: 'Defined' ^

instance Show (SInitBehavior init) where
  show SUnknown = "SUnknown"
  show SDefined = "SDefined"

-- | A domain with a name (@Domain@). Configures the behavior of various aspects
-- of a circuits. See the documentation of this record's field types for more
-- information on the options.
--
-- See module documentation of "Clash.Explicit.Signal" for more information on
-- how to create custom synthesis domains.
data DomainConfiguration
  = DomainConfiguration
  { _name :: Domain
  -- ^ Domain name
  , _period :: Nat
  -- ^ Period of clock in /ps/
  , _activeEdge :: ActiveEdge
  -- ^ Active edge of the clock
  , _resetKind :: ResetKind
  -- ^ Whether resets are synchronous (edge-sensitive) or asynchronous (level-sensitive)
  , _initBehavior :: InitBehavior
  -- ^ Whether the initial (or "power up") value of memory elements is
  -- unknown/undefined, or configurable to a specific value
  , _resetPolarity :: ResetPolarity
  -- ^ Whether resets are active high or active low
  }
  deriving (Typeable)

-- | Helper type family for 'DomainPeriod'
type family DomainConfigurationPeriod (config :: DomainConfiguration) :: Nat where
  DomainConfigurationPeriod ('DomainConfiguration name period edge reset init polarity) = period

-- | Helper type family for 'DomainActiveEdge'
type family DomainConfigurationActiveEdge (config :: DomainConfiguration) :: ActiveEdge where
  DomainConfigurationActiveEdge ('DomainConfiguration name period edge reset init polarity) = edge

-- | Helper type family for 'DomainResetKind'
type family DomainConfigurationResetKind (config :: DomainConfiguration) :: ResetKind where
  DomainConfigurationResetKind ('DomainConfiguration name period edge reset init polarity) = reset

-- | Helper type family for 'DomainInitBehavior'
type family DomainConfigurationInitBehavior (config :: DomainConfiguration) :: InitBehavior where
  DomainConfigurationInitBehavior ('DomainConfiguration name period edge reset init polarity) = init

-- | Helper type family for 'DomainResetPolarity'
type family DomainConfigurationResetPolarity (config :: DomainConfiguration) :: ResetPolarity where
  DomainConfigurationResetPolarity ('DomainConfiguration name period edge reset init polarity) = polarity

-- | Convenience type to help to extract a period from a domain. Example usage:
--
-- @
-- myFunc :: (KnownDomain dom, DomainPeriod dom ~ 6000) => ...
-- @
type DomainPeriod (dom :: Domain) =
  DomainConfigurationPeriod (KnownConf dom)

-- | Convenience type to help to extract the active edge from a domain. Example
-- usage:
--
-- @
-- myFunc :: (KnownDomain dom, DomainActiveEdge dom ~ 'Rising) => ...
-- @
type DomainActiveEdge (dom :: Domain) =
  DomainConfigurationActiveEdge (KnownConf dom)

-- | Convenience type to help to extract the reset synchronicity from a
-- domain. Example usage:
--
-- @
-- myFunc :: (KnownDomain dom, DomainResetKind dom ~ 'Synchronous) => ...
-- @
type DomainResetKind (dom :: Domain) =
  DomainConfigurationResetKind (KnownConf dom)

-- | Convenience type to constrain a domain to have synchronous resets. Example
-- usage:
--
-- @
-- myFunc :: HasSynchronousReset dom => ...
-- @
--
-- Using this type implies 'KnownDomain'.
--
-- [Click here for usage hints]("Clash.Explicit.Signal#g:conveniencetypes")
type HasSynchronousReset (dom :: Domain) =
  (KnownDomain dom, DomainResetKind dom ~ 'Synchronous)

-- | Convenience type to constrain a domain to have asynchronous resets. Example
-- usage:
--
-- @
-- myFunc :: HasAsynchronousReset dom => ...
-- @
--
-- Using this type implies 'KnownDomain'.
--
-- [Click here for usage hints]("Clash.Explicit.Signal#g:conveniencetypes")
type HasAsynchronousReset (dom :: Domain) =
  (KnownDomain dom, DomainResetKind dom ~ 'Asynchronous)

-- | Convenience type to help to extract the initial value behavior from a
-- domain. Example usage:
--
-- @
-- myFunc :: (KnownDomain dom, DomainInitBehavior dom ~ 'Defined) => ...
-- @
type DomainInitBehavior (dom :: Domain) =
  DomainConfigurationInitBehavior (KnownConf dom)

-- | Convenience type to constrain a domain to have initial values. Example
-- usage:
--
-- @
-- myFunc :: HasDefinedInitialValues dom => ...
-- @
--
-- Using this type implies 'KnownDomain'.
--
-- Note that there is no @UnknownInitialValues dom@ as a component that works
-- without initial values will also work if it does have them.
--
-- [Click here for usage hints]("Clash.Explicit.Signal#g:conveniencetypes")
type HasDefinedInitialValues (dom :: Domain) =
  (KnownDomain dom, DomainInitBehavior dom ~ 'Defined)

-- | Convenience type to help to extract the reset polarity from a domain.
-- Example usage:
--
-- @
-- myFunc :: (KnownDomain dom, DomainResetPolarity dom ~ 'ActiveHigh) => ...
-- @
type DomainResetPolarity (dom :: Domain) =
  DomainConfigurationResetPolarity (KnownConf dom)

-- | Singleton version of 'DomainConfiguration'
data SDomainConfiguration (dom :: Domain) (conf :: DomainConfiguration) where
  SDomainConfiguration ::
    { sName :: SSymbol dom
      -- ^ Domain name
    , sPeriod :: SNat period
    -- ^ Period of clock in /ps/
    , sActiveEdge :: SActiveEdge edge
    -- ^ Active edge of the clock (not yet implemented)
    , sResetKind :: SResetKind reset
    -- ^ Whether resets are synchronous (edge-sensitive) or asynchronous (level-sensitive)
    , sInitBehavior :: SInitBehavior init
    -- ^ Whether the initial (or "power up") value of memory elements is
    -- unknown/undefined, or configurable to a specific value
    , sResetPolarity :: SResetPolarity polarity
    -- ^ Whether resets are active high or active low
    } -> SDomainConfiguration dom ('DomainConfiguration dom period edge reset init polarity)

deriving instance Show (SDomainConfiguration dom conf)

type KnownConfiguration dom conf = (KnownDomain dom, KnownConf dom ~ conf)

-- | A 'KnownDomain' constraint indicates that a circuit's behavior depends on
-- some properties of a domain. See 'DomainConfiguration' for more information.
class (KnownSymbol dom, KnownNat (DomainPeriod dom)) => KnownDomain (dom :: Domain) where
  type KnownConf dom :: DomainConfiguration
  -- | Returns 'SDomainConfiguration' corresponding to an instance's 'DomainConfiguration'.
  --
  -- Example usage:
  --
  -- >>> knownDomain @System
  -- SDomainConfiguration {sName = SSymbol @"System", sPeriod = SNat @10000, sActiveEdge = SRising, sResetKind = SAsynchronous, sInitBehavior = SDefined, sResetPolarity = SActiveHigh}
  knownDomain :: SDomainConfiguration dom (KnownConf dom)

-- | Version of 'knownDomain' that takes a 'SSymbol'. For example:
--
-- >>> knownDomainByName (SSymbol @"System")
-- SDomainConfiguration {sName = SSymbol @"System", sPeriod = SNat @10000, sActiveEdge = SRising, sResetKind = SAsynchronous, sInitBehavior = SDefined, sResetPolarity = SActiveHigh}
knownDomainByName
  :: forall dom
   . KnownDomain dom
  => SSymbol dom
  -> SDomainConfiguration dom (KnownConf dom)
knownDomainByName =
  const knownDomain
{-# INLINE knownDomainByName #-}

-- | A /clock/ (and /reset/) dom with clocks running at 100 MHz
instance KnownDomain System where
  type KnownConf System = 'DomainConfiguration System 10000 'Rising 'Asynchronous 'Defined 'ActiveHigh
  knownDomain = SDomainConfiguration SSymbol SNat SRising SAsynchronous SDefined SActiveHigh

-- | System instance with defaults set for Xilinx FPGAs
instance KnownDomain XilinxSystem where
  type KnownConf XilinxSystem = 'DomainConfiguration XilinxSystem 10000 'Rising 'Synchronous 'Defined 'ActiveHigh
  knownDomain = SDomainConfiguration SSymbol SNat SRising SSynchronous SDefined SActiveHigh

-- | System instance with defaults set for Intel FPGAs
instance KnownDomain IntelSystem where
  type KnownConf IntelSystem = 'DomainConfiguration IntelSystem 10000 'Rising 'Asynchronous 'Defined 'ActiveHigh
  knownDomain = SDomainConfiguration SSymbol SNat SRising SAsynchronous SDefined SActiveHigh

-- | Convenience value to allow easy "subclassing" of System domain. Should
-- be used in combination with 'createDomain'. For example, if you just want to
-- change the period but leave all other settings intact use:
--
-- > createDomain vSystem{vName="System10", vPeriod=10}
--
vSystem :: VDomainConfiguration
vSystem = vDomain (knownDomain @System)

-- | A clock (and reset) dom with clocks running at 100 MHz. Memory elements
-- respond to the rising edge of the clock, and asynchronously to changes in
-- reset signals. It has defined initial values, and active-high resets.
--
-- See module documentation of "Clash.Explicit.Signal" for more information on
-- how to create custom synthesis domains.
type System = ("System" :: Domain)


-- | Convenience value to allow easy "subclassing" of IntelSystem domain. Should
-- be used in combination with 'createDomain'. For example, if you just want to
-- change the period but leave all other settings intact use:
--
-- > createDomain vIntelSystem{vName="Intel10", vPeriod=10}
--
vIntelSystem :: VDomainConfiguration
vIntelSystem = vDomain (knownDomain @IntelSystem)

-- | A clock (and reset) dom with clocks running at 100 MHz. Memory elements
-- respond to the rising edge of the clock, and asynchronously to changes in
-- reset signals. It has defined initial values, and active-high resets.
--
-- See module documentation of "Clash.Explicit.Signal" for more information on
-- how to create custom synthesis domains.
type IntelSystem = ("IntelSystem" :: Domain)

-- | Convenience value to allow easy "subclassing" of XilinxSystem domain. Should
-- be used in combination with 'createDomain'. For example, if you just want to
-- change the period but leave all other settings intact use:
--
-- > createDomain vXilinxSystem{vName="Xilinx10", vPeriod=10}
--
vXilinxSystem :: VDomainConfiguration
vXilinxSystem = vDomain (knownDomain @XilinxSystem)

-- | A clock (and reset) dom with clocks running at 100 MHz. Memory elements
-- respond to the rising edge of the clock, and synchronously to changes in
-- reset signals. It has defined initial values, and active-high resets.
--
-- See module documentation of "Clash.Explicit.Signal" for more information on
-- how to create custom synthesis domains.
type XilinxSystem = ("XilinxSystem" :: Domain)

-- | Same as SDomainConfiguration but allows for easy updates through record update syntax.
-- Should be used in combination with 'vDomain' and 'createDomain'. Example:
--
-- > createDomain (knownVDomain @System){vName="System10", vPeriod=10}
--
-- This duplicates the settings in the 'System' domain, replaces the name and
-- period, and creates an instance for it. As most users often want to update
-- the system domain, a shortcut is available in the form:
--
-- > createDomain vSystem{vName="System10", vPeriod=10}
--
data VDomainConfiguration
  = VDomainConfiguration
  { vName :: String
  -- ^ Corresponds to '_name' on 'DomainConfiguration'
  , vPeriod :: Natural
  -- ^ Corresponds to '_period' on 'DomainConfiguration'
  , vActiveEdge :: ActiveEdge
  -- ^ Corresponds to '_activeEdge' on 'DomainConfiguration'
  , vResetKind :: ResetKind
  -- ^ Corresponds to '_resetKind' on 'DomainConfiguration'
  , vInitBehavior :: InitBehavior
  -- ^ Corresponds to '_initBehavior' on 'DomainConfiguration'
  , vResetPolarity :: ResetPolarity
  -- ^ Corresponds to '_resetPolarity' on 'DomainConfiguration'
  }
  deriving (Eq, Generic, NFData, Show, Read, Binary)

-- | Convert 'SDomainConfiguration' to 'VDomainConfiguration'. Should be used in combination with
-- 'createDomain' only.
vDomain :: SDomainConfiguration dom conf -> VDomainConfiguration
vDomain (SDomainConfiguration dom period edge reset init_ polarity) =
  VDomainConfiguration
    (ssymbolToString dom)
    (snatToNatural period)
    (case edge of {SRising -> Rising; SFalling -> Falling})
    (case reset of {SAsynchronous -> Asynchronous; SSynchronous -> Synchronous})
    (case init_ of {SDefined -> Defined; SUnknown -> Unknown})
    (case polarity of {SActiveHigh -> ActiveHigh; SActiveLow -> ActiveLow})

-- TODO: Function might reject valid type names. Figure out what's allowed.
isValidDomainName :: String -> Bool
isValidDomainName (x:xs) = isAsciiUpper x && all isAscii xs && all isAlphaNum xs
isValidDomainName _ = False

-- | Convenience method to express new domains in terms of others.
--
-- > createDomain (knownVDomain @System){vName="System10", vPeriod=10}
--
-- This duplicates the settings in the "System" domain, replaces the name and
-- period, and creates an instance for it. As most users often want to update
-- the system domain, a shortcut is available in the form:
--
-- > createDomain vSystem{vName="System10", vPeriod=10}
--
-- The function will create two extra identifiers. The first:
--
-- > type System10 = ..
--
-- You can use that as the dom to Clocks\/Resets\/Enables\/Signals. For example:
-- @Signal System10 Int@. Additionally, it will create a 'VDomainConfiguration' that you can
-- use in later calls to 'createDomain':
--
-- > vSystem10 = knownVDomain @System10
--
-- It will also make @System10@ an instance of 'KnownDomain'.
--
-- If either identifier is already in scope it will not be generated a second time.
-- Note: This can be useful for example when documenting a new domain:
--
-- > -- | Here is some documentation for CustomDomain
-- > type CustomDomain = ("CustomDomain" :: Domain)
-- >
-- > -- | Here is some documentation for vCustomDomain
-- > createDomain vSystem{vName="CustomDomain"}
createDomain :: VDomainConfiguration -> Q [Dec]
createDomain (VDomainConfiguration name period edge reset init_ polarity) =
  if isValidDomainName name then do
    kdType <- [t| KnownDomain $nameT |]
    kcType <- [t| ('DomainConfiguration $nameT $periodT $edgeT $resetKindT $initT $polarityT) |]
    sDom <- [| SDomainConfiguration SSymbol SNat $edgeE $resetKindE $initE $polarityE |]

    let vNameImpl = AppE (VarE 'vDomain) (AppTypeE (VarE 'knownDomain) (LitT (StrTyLit name)))
        kdImpl = FunD 'knownDomain [Clause [] (NormalB sDom) []]
        kcImpl = mkTySynInstD ''KnownConf [LitT (StrTyLit name)] kcType
        vName' = mkName ('v':name)

    tySynExists <- isJust <$> lookupTypeName name
    vHelperExists <- isJust <$> lookupValueName ('v':name)

    pure $ concat
      [
        [ -- Type synonym (ex: type System = "System")
          TySynD (mkName name) [] (LitT (StrTyLit name)  `SigT`  ConT ''Domain)
        | not tySynExists
        ]

      , concat
        [ -- vDomain helper (ex: vSystem = vDomain (knownDomain @System))
          [ SigD vName' (ConT ''VDomainConfiguration)
          , FunD vName' [Clause [] (NormalB vNameImpl) []]
          ]
        | not vHelperExists
        ]
      , [ -- KnownDomain instance (ex: instance KnownDomain "System" where ...)
          InstanceD Nothing [] kdType [kcImpl, kdImpl]
        ]
      ]

  else
    error ("Domain names should be a valid Haskell type name, not: " ++ name)
 where

  edgeE =
    pure $
    case edge of
      Rising -> ConE 'SRising
      Falling -> ConE 'SFalling

  resetKindE =
    pure $
    case reset of
      Asynchronous -> ConE 'SAsynchronous
      Synchronous -> ConE 'SSynchronous

  initE =
    pure $
    case init_ of
      Unknown -> ConE 'SUnknown
      Defined -> ConE 'SDefined

  polarityE =
    pure $
    case polarity of
      ActiveHigh -> ConE 'SActiveHigh
      ActiveLow -> ConE 'SActiveLow

  nameT   = pure (LitT (StrTyLit name))
  periodT = pure (LitT (NumTyLit (toInteger period)))

  edgeT =
    pure $
    case edge of
      Rising -> PromotedT 'Rising
      Falling -> PromotedT 'Falling

  resetKindT =
    pure $
    case reset of
      Asynchronous -> PromotedT 'Asynchronous
      Synchronous -> PromotedT 'Synchronous

  initT =
    pure $
    case init_ of
      Unknown -> PromotedT 'Unknown
      Defined -> PromotedT 'Defined

  polarityT =
    pure $
    case polarity of
      ActiveHigh -> PromotedT 'ActiveHigh
      ActiveLow -> PromotedT 'ActiveLow


type Domain = Symbol

-- | We either get evidence that this function was instantiated with the same
-- domains, or Nothing.
sameDomain
  :: forall (domA :: Domain) (domB :: Domain)
   . (KnownDomain domA, KnownDomain domB)
  => Maybe (domA :~: domB)
sameDomain = sameSymbol (Proxy @domA) (Proxy @domB)

infixr 5 :-
{- | Clash has synchronous 'Signal's in the form of:

@
'Signal' (dom :: 'Domain') a
@

Where /a/ is the type of the value of the 'Signal', for example /Int/ or /Bool/,
and /dom/ is the /clock-/ (and /reset-/) domain to which the memory elements
manipulating these 'Signal's belong.

The type-parameter, /dom/, is of the kind 'Domain' - a simple string. That
string refers to a single /synthesis domain/. A synthesis domain describes the
behavior of certain aspects of memory elements in it.

* __NB__: \"Bad things\"™  happen when you actually use a clock period of @0@,
so do __not__ do that!
* __NB__: You should be judicious using a clock with period of @1@ as you can
never create a clock that goes any faster!
* __NB__: For the best compatibility make sure your period is divisible by 2,
because some VHDL simulators don't support fractions of picoseconds.
* __NB__: Whether 'System' has good defaults depends on your target platform.
Check out 'IntelSystem' and 'XilinxSystem' too!

Signals have the <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/roles.html type role>

>>> :i Signal
type role Signal nominal representational
...

as it is safe to coerce the underlying value of a signal, but not safe to coerce
a signal between different synthesis domains.

See the module documentation of "Clash.Signal" for more information about
domains.
-}
type role Signal nominal representational
data Signal (dom :: Domain) a
  -- | The constructor, @(':-')@, is __not__ synthesizable.
  = a :- Signal dom a

head# :: Signal dom a -> a
head# (x' :- _ )  = x'

tail# :: Signal dom a -> Signal dom a
tail# (_  :- xs') = xs'

instance Show a => Show (Signal dom a) where
  show (x :- xs) = show x ++ " " ++ show xs

instance Lift a => Lift (Signal dom a) where
  lift ~(x :- _) = [| signal# x |]
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedFromUntyped
#endif

instance Default a => Default (Signal dom a) where
  def = signal# def

instance Functor (Signal dom) where
  fmap = mapSignal#

mapSignal# :: forall a b dom. (a -> b) -> Signal dom a -> Signal dom b
mapSignal# f = go
 where
  -- See -fstrict-mapSignal documentation in clash-prelude.cabal
  theSeq = if fStrictMapSignal then seqX else flip const
  go ~(xs@(a :- as)) = f a :- (a `theSeq` (xs `seq` go as))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE mapSignal# #-}
{-# ANN mapSignal# hasBlackBox #-}

instance Applicative (Signal dom) where
  pure  = signal#
  (<*>) = appSignal#

signal# :: a -> Signal dom a
signal# a = let s = a :- s in s
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE signal# #-}
{-# ANN signal# hasBlackBox #-}

appSignal# :: Signal dom (a -> b) -> Signal dom a -> Signal dom b
appSignal# (f :- fs) xs@(~(a :- as)) = f a :- (xs `seq` appSignal# fs as) -- See [NOTE: Lazy ap]
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE appSignal# #-}
{-# ANN appSignal# hasBlackBox #-}

instance NFDataX a => NFDataX (Signal domain a) where
  deepErrorX = pure . deepErrorX
  ensureSpine s = case isX s of
    Left e -> deepErrorX e
    Right (a :- s') -> ensureSpine a :- ensureSpine s'
  hasUndefined = error "hasUndefined on (Signal domain a): No sensible implementation exists"
  rnfX = error "rnfX on (Signal domain a): No sensible implementation exists"

{- NOTE: Lazy ap
Signal's ap, i.e (Applicative.<*>), must be lazy in it's second argument:

> appSignal :: Signal clk (a -> b) -> Signal clk a -> Signal clk b
> appSignal (f :- fs) ~(a :- as) = f a :- appSignal fs as

because some feedback loops, such as the loop described in 'system' in the
example at https://hackage.haskell.org/package/clash-prelude-1.0.0/docs/Clash-Prelude-BlockRam.html,
will lead to "Exception <<loop>>".

However, this "naive" lazy version is _too_ lazy and induces spaceleaks.
The current version:

> appSignal# :: Signal clk (a -> b) -> Signal clk a -> Signal clk b
> appSignal# (f :- fs) xs@(~(a :- as)) = f a :- (xs `seq` appSignal# fs as)

Is lazy enough to handle the earlier mentioned feedback loops, but doesn't leak
(as much) memory like the "naive" lazy version, because the Signal constructor
of the second argument is evaluated as soon as the tail of the result is evaluated.
-}


-- | __WARNING: EXTREMELY EXPERIMENTAL__
--
-- The circuit semantics of this operation are unclear and/or non-existent.
-- There is a good reason there is no 'Monad' instance for 'Signal'.
--
-- Is currently treated as 'id' by the Clash compiler.
joinSignal# :: Signal dom (Signal dom a) -> Signal dom a
joinSignal# ~(xs :- xss) = head# xs :- joinSignal# (mapSignal# tail# xss)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE joinSignal# #-}
{-# ANN joinSignal# hasBlackBox #-}

instance Num a => Num (Signal dom a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = signal# . fromInteger

-- | __NB__: Not synthesizable
--
-- __NB__: In \"@'foldr' f z s@\":
--
-- * The function @f@ should be /lazy/ in its second argument.
-- * The @z@ element will never be used.
instance Foldable (Signal dom) where
  foldr = foldr#

-- | __NB__: Not synthesizable
--
-- __NB__: In \"@'foldr#' f z s@\":
--
-- * The function @f@ should be /lazy/ in its second argument.
-- * The @z@ element will never be used.
foldr# :: (a -> b -> b) -> b -> Signal dom a -> b
foldr# f z (a :- s) = a `f` (foldr# f z s)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE foldr# #-}
{-# ANN foldr# hasBlackBox #-}

instance Traversable (Signal dom) where
  traverse = traverse#

traverse# :: Applicative f => (a -> f b) -> Signal dom a -> f (Signal dom b)
traverse# f (a :- s) = (:-) <$> f a <*> traverse# f s
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE traverse# #-}
{-# ANN traverse# hasBlackBox #-}

-- * Clocks, resets, and enables

-- | A signal of booleans, indicating whether a component is enabled. No special
-- meaning is implied, it's up to the component itself to decide how to respond
-- to its enable line. It is used throughout Clash as a global enable signal.
data Enable dom = Enable (Signal dom Bool)

-- | Convert 'Enable' construct to its underlying representation: a signal of
-- bools.
fromEnable :: Enable dom -> Signal dom Bool
fromEnable (Enable x) = x
{-# INLINE fromEnable #-}

-- | Convert a signal of bools to an 'Enable' construct
toEnable :: Signal dom Bool -> Enable dom
toEnable = Enable
{-# INLINE toEnable #-}

-- | Enable generator for some domain. Is simply always True.
enableGen :: Enable dom
enableGen = toEnable (pure True)

-- | A clock signal belonging to a domain named /dom/.
data Clock (dom :: Domain) = Clock
  { -- | Domain associated with the clock
    clockTag :: SSymbol dom

    -- | Periods of the clock. This is an experimental feature used to simulate
    -- clock frequency correction mechanisms. Currently, all ways to contruct
    -- such a clock are hidden from the public API.
  , clockPeriods :: Maybe (Signal dom Femtoseconds)
  }

instance Show (Clock dom) where
  show (Clock dom Nothing) = "<Clock: " ++ ssymbolToString dom ++ ">"
  show (Clock dom _) = "<Dynamic clock: " ++ ssymbolToString dom ++ ">"

-- | The negative or inverted phase of a differential clock signal. HDL
-- generation will treat it the same as 'Clock', except that no @create_clock@
-- command is issued in the SDC file for 'ClockN'. Used in 'DiffClock'.
newtype ClockN (dom :: Domain) = ClockN { clockNTag :: SSymbol dom }

instance Show (ClockN dom) where
  show (ClockN dom) = "<ClockN: " ++ ssymbolToString dom ++ ">"

-- | A differential clock signal belonging to a domain named /dom/. The clock
-- input of a design with such an input has two ports which are in antiphase.
-- The first input is the positive phase, the second the negative phase. When
-- using 'Clash.Annotations.TH.makeTopEntity', the names of the inputs will end
-- in @_p@ and @_n@ respectively.
data DiffClock (dom :: Domain) =
  DiffClock ("p" ::: Clock dom) ("n" ::: ClockN dom)

instance Show (DiffClock dom) where
  show (DiffClock (Clock dom Nothing) _) =
    "<DiffClock: " ++ ssymbolToString dom ++ ">"
  show (DiffClock (Clock dom _) _) =
    "<Dynamic DiffClock: " ++ ssymbolToString dom ++ ">"

-- | Clock generator for simulations. Do __not__ use this clock generator for
-- the /testBench/ function, use 'tbClockGen' instead.
--
-- To be used like:
--
-- @
-- clkSystem = clockGen @System
-- @
--
-- See 'DomainConfiguration' for more information on how to use synthesis domains.
clockGen
  :: KnownDomain dom
  => Clock dom
clockGen = tbClockGen (pure True)

-- | Clock generator to be used in the /testBench/ function.
--
-- To be used like:
--
-- @
-- clkSystem en = tbClockGen @System en
-- @
--
-- === __Example__
--
-- @
-- module Example where
--
-- import "Clash.Explicit.Prelude"
-- import "Clash.Explicit.Testbench"
--
-- -- Fast domain: twice as fast as \"Slow\"
-- 'Clash.Explicit.Prelude.createDomain' 'Clash.Explicit.Prelude.vSystem'{vName=\"Fast\", vPeriod=10}
--
-- -- Slow domain: twice as slow as \"Fast\"
-- 'Clash.Explicit.Prelude.createDomain' 'Clash.Explicit.Prelude.vSystem'{vName=\"Slow\", vPeriod=20}
--
-- topEntity
--   :: 'Clock' \"Fast\"
--   -> 'Reset' \"Fast\"
--   -> 'Enable' \"Fast\"
--   -> 'Clock' \"Slow\"
--   -> 'Signal' \"Fast\" (Unsigned 8)
--   -> 'Signal' \"Slow\" (Unsigned 8, Unsigned 8)
-- topEntity clk1 rst1 en1 clk2 i =
--   let h = register clk1 rst1 en1 0 (register clk1 rst1 en1 0 i)
--       l = register clk1 rst1 en1 0 i
--   in  unsafeSynchronizer clk1 clk2 (bundle (h, l))
--
-- testBench
--   :: 'Signal' \"Slow\" Bool
-- testBench = done
--   where
--     testInput      = 'Clash.Explicit.Testbench.stimuliGenerator' clkA1 rstA1 $('Clash.Sized.Vector.listToVecTH' [1::Unsigned 8,2,3,4,5,6,7,8])
--     expectedOutput = 'Clash.Explicit.Testbench.outputVerifier'   clkB2 rstB2 $('Clash.Sized.Vector.listToVecTH' [(0,0) :: (Unsigned 8, Unsigned 8),(1,2),(3,4),(5,6),(7,8)])
--     done           = expectedOutput (topEntity clkA1 rstA1 enableGen clkB2 testInput)
--     notDone        = not \<$\> done
--     clkA1          = 'tbClockGen' \@\"Fast\" (unsafeSynchronizer clkB2 clkA1 notDone)
--     clkB2          = 'tbClockGen' \@\"Slow\" notDone
--     rstA1          = 'Clash.Signal.resetGen' \@\"Fast\"
--     rstB2          = 'Clash.Signal.resetGen' \@\"Slow\"
-- @
tbClockGen
  :: KnownDomain testDom
  => Signal testDom Bool
  -> Clock testDom
tbClockGen done = Clock (done `seq` SSymbol) Nothing
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE tbClockGen #-}
{-# ANN tbClockGen hasBlackBox #-}

-- | Femtoseconds expressed as an 'Int64'. Is a newtype to prevent accidental
-- mixups with picoseconds - the unit used in 'DomainConfiguration'.
--
newtype Femtoseconds = Femtoseconds Int64
  -- No 'Integral' instance to prevent accidental picoseconds / femtoseconds mixup
  deriving (Show, Eq, Generic, NFDataX, NFData, Lift, Ord)

-- | Strip newtype wrapper 'Femtoseconds'
unFemtoseconds :: Femtoseconds -> Int64
unFemtoseconds (Femtoseconds fs) = fs

-- | Map 'Int64' fields in 'Femtoseconds'
mapFemtoseconds :: (Int64 -> Int64) -> Femtoseconds -> Femtoseconds
mapFemtoseconds f (Femtoseconds fs) = Femtoseconds (f fs)

-- | Clock generator with dynamic clock periods for simulations. This is an
-- experimental feature and hence not part of the public API.
--
-- To be used like:
--
-- @
-- clkSystem = dynamicClockGen @System
-- @
--
-- See 'DomainConfiguration' for more information on how to use synthesis domains.
dynamicClockGen ::
  KnownDomain dom =>
  -- | Clock period in /femto/seconds.
  --
  -- * __NB__: Beware that the periods are given in femtoseconds; this differs
  --           from the usual unit Clash uses to represent period length,
  --           picoseconds.
  --
  -- * __NB__: Beware that not all simulators support femtoseconds. For example,
  --           Vivado's XSIM will round down to nearest picoseconds.
  --
  -- * __NB__: Beware that, by default, Clash will define @`timescale 100fs/100fs@
  --           in its generated Verilog. The latter will make simulators round
  --           time to 100fs. If you rely on more precision you should pass
  --           @-fclash-timescale-precision 1fs@ to Clash.
  Signal dom Femtoseconds ->
  Clock dom
dynamicClockGen periods = tbDynamicClockGen periods (pure True)

-- | Clock generator with dynamic clock periods for simulations. This is an
-- experimental feature and hence not part of the public API. Like 'tbClockGen'
--
--
-- To be used like:
--
-- @
-- clkSystem = dynamicClockGen @System
-- @
--
-- See 'DomainConfiguration' for more information on how to use synthesis domains.
tbDynamicClockGen ::
  KnownDomain dom =>
  -- | Clock period in /femto/seconds.
  --
  -- * __NB__: Beware that the periods are given in femtoseconds; this differs
  --           from the usual unit Clash uses to represent period length,
  --           picoseconds.
  --
  -- * __NB__: Beware that not all simulators support femtoseconds. For example,
  --           Vivado's XSIM will round down to nearest picoseconds.
  --
  -- * __NB__: Beware that, by default, Clash will define @`timescale 100fs/100fs@
  --           in its generated Verilog. The latter will make simulators round
  --           time to 100fs. If you rely on more precision you should pass
  --           @-fclash-timescale-precision 1fs@ to Clash.
  Signal dom Femtoseconds ->
  Signal dom Bool ->
  Clock dom
tbDynamicClockGen periods ena =
  Clock (ena `seq` periods `seq` SSymbol) (Just periods)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE tbDynamicClockGen #-}
{-# ANN tbDynamicClockGen hasBlackBox #-}


-- | Reset generator for simulation purposes. Asserts the reset for a single
-- cycle.
--
-- To be used like:
--
-- @
-- rstSystem = resetGen @System
-- @
--
-- See 'Clash.Explicit.Testbench.tbClockGen' for example usage.
--
-- __NB__: While this can be used in the @testBench@ function, it cannot be
-- synthesized to hardware.
resetGen
  :: forall dom
   . KnownDomain dom
  => Reset dom
resetGen = resetGenN (SNat @1)
{-# INLINE resetGen #-}

-- | Reset generator for simulation purposes. Asserts the reset for the first /n/
-- cycles.
--
-- To be used like:
--
-- @
-- rstSystem5 = resetGen @System d5
-- @
--
-- Example usage:
--
-- >>> sampleN 7 (unsafeToActiveHigh (resetGenN @System d3))
-- [True,True,True,False,False,False,False]
--
-- __NB__: While this can be used in the @testBench@ function, it cannot be
-- synthesized to hardware.
resetGenN
  :: forall dom n
   . (KnownDomain dom, 1 <= n)
  => SNat n
  -- ^ Number of initial cycles to hold reset high
  -> Reset dom
resetGenN n =
  let asserted = replicate (snatToNum n) True in
  unsafeFromActiveHigh (fromList (asserted ++ repeat False))
{-# ANN resetGenN hasBlackBox #-}
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE resetGenN #-}


-- | A reset signal belonging to a domain called /dom/.
--
-- The underlying representation of resets is 'Bool'.
data Reset (dom :: Domain) = Reset (Signal dom Bool)

-- | Non-ambiguous version of 'Clash.Signal.Internal.Ambiguous.resetPolarity'
resetPolarityProxy
  :: forall dom proxy polarity
   . (KnownDomain dom, DomainResetPolarity dom ~ polarity)
  => proxy dom
  -> SResetPolarity polarity
resetPolarityProxy _proxy =
  case knownDomain @dom of
    SDomainConfiguration _dom _period _edge _sync _init polarity ->
      polarity

-- | Convert a reset to an active high reset. Has no effect if reset is already
-- an active high reset. Is unsafe because it can introduce:
--
-- * <Clash-Explicit-Signal.html#metastability meta-stability>
--
-- For asynchronous resets it is unsafe because it can cause combinatorial
-- loops. In case of synchronous resets it can lead to
-- <Clash-Explicit-Signal.html#metastability meta-stability> in the presence of
-- asynchronous resets.
unsafeToActiveHigh
  :: forall dom
   . KnownDomain dom
  => Reset dom
  -> Signal dom Bool
unsafeToActiveHigh (unsafeFromReset -> r) =
  case resetPolarityProxy (Proxy @dom) of
    SActiveHigh -> r
    SActiveLow -> not <$> r
{-# INLINE unsafeToActiveHigh #-}

-- | Convert a reset to an active high reset. Has no effect if reset is already
-- an active high reset. Is unsafe because it can introduce:
--
-- * <Clash-Explicit-Signal.html#metastability meta-stability>
--
-- For asynchronous resets it is unsafe because it can cause combinatorial
-- loops. In case of synchronous resets it can lead to
-- <Clash-Explicit-Signal.html#metastability meta-stability> in the presence of
-- asynchronous resets.
unsafeToHighPolarity
  :: forall dom
   . KnownDomain dom
  => Reset dom
  -> Signal dom Bool
unsafeToHighPolarity = unsafeToActiveHigh
{-# DEPRECATED unsafeToHighPolarity "Use 'unsafeToActiveHigh' instead. This function will be removed in Clash 1.12." #-}
{-# INLINE unsafeToHighPolarity #-}

-- | Convert a reset to an active low reset. Has no effect if reset is already
-- an active low reset. It is unsafe because it can introduce:
--
-- * <Clash-Explicit-Signal.html#metastability meta-stability>
--
-- For asynchronous resets it is unsafe because it can cause combinatorial
-- loops. In case of synchronous resets it can lead to
-- <Clash-Explicit-Signal.html#metastability meta-stability> in the presence of
-- asynchronous resets.
unsafeToActiveLow
  :: forall dom
   . KnownDomain dom
  => Reset dom
  -> Signal dom Bool
unsafeToActiveLow (unsafeFromReset -> r) =
  case resetPolarityProxy (Proxy @dom) of
    SActiveHigh -> not <$> r
    SActiveLow -> r
{-# INLINE unsafeToActiveLow #-}

-- | Convert a reset to an active low reset. Has no effect if reset is already
-- an active low reset. It is unsafe because it can introduce:
--
-- * <Clash-Explicit-Signal.html#metastability meta-stability>
--
-- For asynchronous resets it is unsafe because it can cause combinatorial
-- loops. In case of synchronous resets it can lead to
-- <Clash-Explicit-Signal.html#metastability meta-stability> in the presence of
-- asynchronous resets.
unsafeToLowPolarity
  :: forall dom
   . KnownDomain dom
  => Reset dom
  -> Signal dom Bool
unsafeToLowPolarity = unsafeToActiveLow
{-# DEPRECATED unsafeToLowPolarity "Use 'unsafeToActiveLow' instead. This function will be removed in Clash 1.12." #-}
{-# INLINE unsafeToLowPolarity #-}

-- | 'unsafeFromReset' is unsafe because it can introduce:
--
-- * <Clash-Explicit-Signal.html#metastability meta-stability>
--
-- For asynchronous resets it is unsafe because it can cause combinatorial
-- loops. In case of synchronous resets it can lead to
-- <Clash-Explicit-Signal.html#metastability meta-stability> in the presence of
-- asynchronous resets.
--
-- __NB__: You probably want to use 'unsafeToActiveLow' or
-- 'unsafeToActiveHigh'.
unsafeFromReset
  :: Reset dom
  -> Signal dom Bool
unsafeFromReset (Reset r) = r
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsafeFromReset #-}
{-# ANN unsafeFromReset hasBlackBox #-}

-- | 'unsafeToReset' is unsafe. For asynchronous resets it is unsafe
-- because it can introduce combinatorial loops. In case of synchronous resets
-- it can lead to <Clash-Explicit-Signal.html#metastability meta-stability>
-- issues in the presence of asynchronous resets.
--
-- __NB__: You probably want to use 'unsafeFromActiveLow' or
-- 'unsafeFromActiveHigh'.
unsafeToReset
  :: Signal dom Bool
  -> Reset dom
unsafeToReset r = Reset r
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsafeToReset #-}
{-# ANN unsafeToReset hasBlackBox #-}

-- | Interpret a signal of bools as an active high reset and convert it to
-- a reset signal corresponding to the domain's setting.
--
-- For asynchronous resets it is unsafe because it can cause combinatorial
-- loops. In case of synchronous resets it can lead to
-- <Clash-Explicit-Signal.html#metastability meta-stability> in the presence of
-- asynchronous resets.
unsafeFromHighPolarity
  :: forall dom
   . KnownDomain dom
  => Signal dom Bool
  -- ^ Reset signal that's 'True' when active, and 'False' when inactive.
  -> Reset dom
unsafeFromHighPolarity = unsafeFromActiveHigh
{-# DEPRECATED unsafeFromHighPolarity "Use 'unsafeFromActiveHigh' instead. This function will be removed in Clash 1.12." #-}
{-# INLINE unsafeFromHighPolarity #-}

-- | Interpret a signal of bools as an active high reset and convert it to
-- a reset signal corresponding to the domain's setting.
--
-- For asynchronous resets it is unsafe because it can cause combinatorial
-- loops. In case of synchronous resets it can lead to
-- <Clash-Explicit-Signal.html#metastability meta-stability> in the presence of
-- asynchronous resets.
unsafeFromActiveHigh
  :: forall dom
   . KnownDomain dom
  => Signal dom Bool
  -- ^ Reset signal that's 'True' when active, and 'False' when inactive.
  -> Reset dom
unsafeFromActiveHigh r =
  unsafeToReset $
    case resetPolarityProxy (Proxy @dom) of
      SActiveHigh -> r
      SActiveLow -> not <$> r

-- | Interpret a signal of bools as an active low reset and convert it to
-- a reset signal corresponding to the domain's setting.
--
-- For asynchronous resets it is unsafe because it can cause combinatorial
-- loops. In case of synchronous resets it can lead to
-- <Clash-Explicit-Signal.html#metastability meta-stability> in the presence of
-- asynchronous resets.
unsafeFromLowPolarity
  :: forall dom
   . KnownDomain dom
  => Signal dom Bool
  -- ^ Reset signal that's 'False' when active, and 'True' when inactive.
  -> Reset dom
unsafeFromLowPolarity = unsafeFromActiveLow
{-# DEPRECATED unsafeFromLowPolarity "Use 'unsafeFromActiveLow' instead. This function will be removed in Clash 1.12." #-}
{-# INLINE unsafeFromLowPolarity #-}

-- | Interpret a signal of bools as an active low reset and convert it to
-- a reset signal corresponding to the domain's setting.
--
-- For asynchronous resets it is unsafe because it can cause combinatorial
-- loops. In case of synchronous resets it can lead to
-- <Clash-Explicit-Signal.html#metastability meta-stability> in the presence of
-- asynchronous resets.
unsafeFromActiveLow
  :: forall dom
   . KnownDomain dom
  => Signal dom Bool
  -- ^ Reset signal that's 'False' when active, and 'True' when inactive.
  -> Reset dom
unsafeFromActiveLow r =
  unsafeToReset $
    case resetPolarityProxy (Proxy @dom) of
      SActiveHigh -> not <$> r
      SActiveLow -> r

-- | Invert reset signal
invertReset :: Reset dom -> Reset dom
invertReset = unsafeToReset . fmap not . unsafeFromReset

infixr 2 .||.
-- | The above type is a generalization for:
--
-- @
-- __(.||.)__ :: 'Clash.Signal.Signal' 'Bool' -> 'Clash.Signal.Signal' 'Bool' -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('||') that returns a 'Clash.Signal.Signal' of 'Bool'
(.||.) :: Applicative f => f Bool -> f Bool -> f Bool
(.||.) = liftA2 (||)

infixr 3 .&&.
-- | The above type is a generalization for:
--
-- @
-- __(.&&.)__ :: 'Clash.Signal.Signal' 'Bool' -> 'Clash.Signal.Signal' 'Bool' -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('&&') that returns a 'Clash.Signal.Signal' of 'Bool'
(.&&.) :: Applicative f => f Bool -> f Bool -> f Bool
(.&&.) = liftA2 (&&)

-- [Note: register strictness annotations]
--
-- In order to produce the first (current) value of the register's output
-- signal, 'o', we don't need to know the shape of either input (enable or
-- value-in).  This is important, because both values might be produced from
-- the output in a feedback loop, so we can't know their shape (pattern
-- match) them until we have produced output.
--
-- Thus, we use lazy pattern matching to delay inspecting the shape of
-- either argument until output has been produced.
--
-- However, both arguments need to be evaluated to WHNF as soon as possible
-- to avoid a space-leak.  Below, we explicitly reduce the value-in signal
-- using 'seq' as the tail of our output signal is produced.  On the other
-- hand, because the value of the tail depends on the value of the enable
-- signal 'e', it will be forced by the 'if'/'then' statement and we don't
-- need to 'seq' it explicitly.

delay#
  :: forall dom a
   . ( KnownDomain dom
     , NFDataX a )
  => Clock dom
  -> Enable dom
  -> a
  -> Signal dom a
  -> Signal dom a
delay# (Clock dom _) (fromEnable -> en) powerUpVal0 =
    go powerUpVal1 en
  where
    powerUpVal1 :: a
    powerUpVal1 =
      case knownDomainByName dom of
        SDomainConfiguration _dom _period _edge _sync SDefined _polarity ->
          powerUpVal0
        SDomainConfiguration _dom _period _edge _sync SUnknown _polarity ->
          deepErrorX ("First value of `delay` unknown on domain " ++ show dom)

    go o (e :- es) as@(~(x :- xs)) =
      let o' = if e then x else o
      -- See [Note: register strictness annotations]
      in  o `defaultSeqX` o :- (as `seq` go o' es xs)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE delay# #-}
{-# ANN delay# hasBlackBox #-}

-- | A register with a power up and reset value. Power up values are not
-- supported on all platforms, please consult the manual of your target platform
-- and check the notes below.
--
-- Xilinx: power up values and reset values MUST be the same. If they are not,
-- the Xilinx tooling __will ignore the reset value__ and use the power up value
-- instead. Source: MIA
--
-- Intel: power up values and reset values MUST be the same. If they are not,
-- the Intel tooling __will ignore the power up value__ and use the reset value
-- instead. Source: https://www.intel.com/content/www/us/en/programmable/support/support-resources/knowledge-base/solutions/rd01072011_91.html
register#
  :: forall dom  a
   . ( KnownDomain dom
     , NFDataX a )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> a
  -- ^ Power up value
  -> a
  -- ^ Reset value
  -> Signal dom a
  -> Signal dom a
register# clk@(Clock dom _) rst ena powerUpVal resetVal =
  case knownDomainByName dom of
    SDomainConfiguration _name _period _edge SSynchronous _init _polarity ->
      syncRegister# clk rst ena powerUpVal resetVal
    SDomainConfiguration _name _period _edge SAsynchronous _init _polarity ->
      asyncRegister# clk rst ena powerUpVal resetVal
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE register# #-}
{-# ANN register# hasBlackBox #-}

-- | Acts like 'id' if given domain allows powerup values, but returns a
-- value constructed with 'deepErrorX' otherwise.
registerPowerup#
  :: forall dom a
   . ( KnownDomain dom
     , NFDataX a
     , HasCallStack )
  => Clock dom
  -> a
  -> a
registerPowerup# (Clock dom _) a =
  case knownDomainByName dom of
    SDomainConfiguration _dom _period _edge _sync SDefined _polarity -> a
    SDomainConfiguration _dom _period _edge _sync SUnknown _polarity ->
      deepErrorX ("First value of register undefined on domain " ++ show dom)

-- | Version of 'register#' that simulates a register on an asynchronous
-- domain. Is synthesizable.
asyncRegister#
  :: forall dom  a
   . ( KnownDomain dom
     , NFDataX a )
  => Clock dom
  -- ^ Clock signal
  -> Reset dom
  -- ^ Reset signal
  -> Enable dom
  -- ^ Enable signal
  -> a
  -- ^ Power up value
  -> a
  -- ^ Reset value
  -> Signal dom a
  -> Signal dom a
asyncRegister# clk (unsafeToActiveHigh -> rst) (fromEnable -> ena) initVal resetVal =
  go (registerPowerup# clk initVal) rst ena
 where
  go o (r :- rs) enas@(~(e :- es)) as@(~(x :- xs)) =
    let oR = if r then resetVal else o
        oE = if r then resetVal else (if e then x else o)
        -- [Note: register strictness annotations]
    in  o `defaultSeqX` oR :- (as `seq` enas `seq` go oE rs es xs)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE asyncRegister# #-}
{-# ANN asyncRegister# hasBlackBox #-}

-- | Version of 'register#' that simulates a register on a synchronous
-- domain. Not synthesizable.
syncRegister#
  :: forall dom  a
   . ( KnownDomain dom
     , NFDataX a )
  => Clock dom
  -- ^ Clock signal
  -> Reset dom
  -- ^ Reset signal
  -> Enable dom
  -- ^ Enable signal
  -> a
  -- ^ Power up value
  -> a
  -- ^ Reset value
  -> Signal dom a
  -> Signal dom a
syncRegister# clk (unsafeToActiveHigh -> rst) (fromEnable -> ena) initVal resetVal =
  go (registerPowerup# clk initVal) rst ena
 where
  go o rt@(~(r :- rs)) enas@(~(e :- es)) as@(~(x :- xs)) =
    let oE = if e then x else o
        oR = if r then resetVal else oE
        -- [Note: register strictness annotations]
    in  o `defaultSeqX` o :- (rt `seq` enas `seq` as `seq` go oR rs es xs)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE syncRegister# #-}
{-# ANN syncRegister# dontTranslate #-}

-- | The above type is a generalization for:
--
-- @
-- __mux__ :: 'Clash.Signal.Signal' 'Bool' -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a
-- @
--
-- A multiplexer. Given "@'mux' b t f@", output @t@ when @b@ is 'True', and @f@
-- when @b@ is 'False'.
mux :: Applicative f => f Bool -> f a -> f a -> f a
mux = liftA3 (\b t f -> if b then t else f)
{-# INLINE mux #-}

infix 4 .==.
-- | The above type is a generalization for:
--
-- @
-- __(.==.)__ :: 'Eq' a => 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('==') that returns a 'Clash.Signal.Signal' of 'Bool'
(.==.) :: (Eq a, Applicative f) => f a -> f a -> f Bool
(.==.) = liftA2 (==)

infix 4 ./=.
-- | The above type is a generalization for:
--
-- @
-- __(./=.)__ :: 'Eq' a => 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('/=') that returns a 'Clash.Signal.Signal' of 'Bool'
(./=.) :: (Eq a, Applicative f) => f a -> f a -> f Bool
(./=.) = liftA2 (/=)

infix 4 .<.
-- | The above type is a generalization for:
--
-- @
-- __(.<.)__ :: 'Ord' a => 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('<') that returns a 'Clash.Signal.Signal' of 'Bool'
(.<.) :: (Ord a, Applicative f) => f a -> f a -> f Bool
(.<.) = liftA2 (<)

infix 4 .<=.
-- | The above type is a generalization for:
--
-- @
-- __(.<=.)__ :: 'Ord' a => 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('GHC.TypeNats.<=') that returns a 'Clash.Signal.Signal' of 'Bool'
(.<=.) :: (Ord a, Applicative f) => f a -> f a -> f Bool
(.<=.) = liftA2 (<=)

infix 4 .>.
-- | The above type is a generalization for:
--
-- @
-- __(.>.)__ :: 'Ord' a => 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('>') that returns a 'Clash.Signal.Signal' of 'Bool'
(.>.) :: (Ord a, Applicative f) => f a -> f a -> f Bool
(.>.) = liftA2 (>)

infix 4 .>=.
-- | The above type is a generalization for:
--
-- @
-- __(.>=.)__ :: 'Ord' a => 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' 'Bool'
-- @
--
--  It is a version of ('>=') that returns a 'Clash.Signal.Signal' of 'Bool'
(.>=.) :: (Ord a, Applicative f) => f a -> f a -> f Bool
(.>=.) = liftA2 (>=)

instance Fractional a => Fractional (Signal dom a) where
  (/)          = liftA2 (/)
  recip        = fmap recip
  fromRational = signal# . fromRational

instance Arbitrary a => Arbitrary (Signal dom a) where
  arbitrary = liftA2 (:-) arbitrary arbitrary

instance CoArbitrary a => CoArbitrary (Signal dom a) where
  coarbitrary xs gen = do
    n <- arbitrary
    coarbitrary (take (abs n) (sample_lazy xs)) gen

-- | The above type is a generalization for:
--
-- @
-- __testFor__ :: 'Int' -> 'Clash.Signal.Signal' Bool -> 'Property'
-- @
--
-- @testFor n s@ tests the signal @s@ for @n@ cycles.
--
-- __NB__: This function is not synthesizable
testFor :: Foldable f => Int -> f Bool -> Property
testFor n = property . and . take n . sample

-- * List \<-\> Signal conversion (not synthesizable)

-- | The above type is a generalization for:
--
-- @
-- __sample__ :: 'Clash.Signal.Signal' a -> [a]
-- @
--
-- Get an infinite list of samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Clash.Signal.Signal'
-- at consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
--
-- __NB__: This function is not synthesizable
sample :: (Foldable f, NFDataX a) => f a -> [a]
sample = foldr (\a b -> deepseqX a (a : b)) []

-- | The above type is a generalization for:
--
-- @
-- __sampleN__ :: Int -> 'Clash.Signal.Signal' a -> [a]
-- @
--
-- Get a list of @n@ samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Clash.Signal.Signal'
-- at consecutive clock cycles
--
-- > sampleN 3 s == [s0, s1, s2]
--
-- __NB__: This function is not synthesizable
sampleN :: (Foldable f, NFDataX a) => Int -> f a -> [a]
sampleN n = take n . sample

-- | Create a 'Clash.Signal.Signal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- >>> sampleN 2 (fromList [1,2,3,4,5])
-- [1,2]
--
-- __NB__: This function is not synthesizable
fromList :: NFDataX a => [a] -> Signal dom a
fromList = Prelude.foldr (\a b -> deepseqX a (a :- b)) (errorX "finite list")

-- * Simulation functions (not synthesizable)

-- | Simulate a (@'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' b@) function
-- given a list of samples of type @a@
--
-- >>> simulate (register systemClockGen resetGen enableGen 8) [1, 1, 2, 3]
-- [8,8,1,2,3...
-- ...
--
-- __NB__: This function is not synthesizable
simulate :: (NFDataX a, NFDataX b) => (Signal dom1 a -> Signal dom2 b) -> [a] -> [b]
simulate f = sample . f . fromList

-- | The above type is a generalization for:
--
-- @
-- __sample__ :: 'Clash.Signal.Signal' a -> [a]
-- @
--
-- Get an infinite list of samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Clash.Signal.Signal'
-- at consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
--
-- __NB__: This function is not synthesizable
sample_lazy :: Foldable f => f a -> [a]
sample_lazy = foldr (:) []

-- | The above type is a generalization for:
--
-- @
-- __sampleN__ :: Int -> 'Clash.Signal.Signal' a -> [a]
-- @
--
-- Get a list of @n@ samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Clash.Signal.Signal'
-- at consecutive clock cycles
--
-- > sampleN 3 s == [s0, s1, s2]
--
-- __NB__: This function is not synthesizable
sampleN_lazy :: Foldable f => Int -> f a -> [a]
sampleN_lazy n = take n . sample_lazy

-- | Create a 'Clash.Signal.Signal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- >>> sampleN 2 (fromList [1,2,3,4,5] :: Signal System Int)
-- [1,2]
--
-- __NB__: This function is not synthesizable
fromList_lazy :: [a] -> Signal dom a
fromList_lazy = Prelude.foldr (:-) (error "finite list")

-- * Simulation functions (not synthesizable)

-- | Simulate a (@'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' b@) function
-- given a list of samples of type @a@
--
-- >>> simulate (register systemClockGen resetGen enableGen 8) [1, 1, 2, 3]
-- [8,8,1,2,3...
-- ...
--
-- __NB__: This function is not synthesizable
simulate_lazy :: (Signal dom1 a -> Signal dom2 b) -> [a] -> [b]
simulate_lazy f = sample_lazy . f . fromList_lazy

-- | Calculate the period in __ps__, given a frequency in __Hz__
--
-- I.e., to calculate the clock period for a circuit to run at 240 MHz we get
--
-- >>> hzToPeriod 240e6
-- 4166
--
-- If the value @hzToPeriod@ is applied to is not of the type 'Ratio'
-- 'Natural', you can use @hzToPeriod ('realToFrac' f)@. Note that if @f@ is
-- negative, @realToFrac@ will give an @'Control.Exception.Underflow' ::
-- t'Control.Exception.ArithException'@ without a call stack, making debugging
-- cumbersome.
--
-- Before Clash 1.8, this function always returned a 'Natural'. To get the old
-- behavior of this function, use a type application:
--
-- >>> hzToPeriod @Natural 240e6
-- 4166
--
-- * __NB__: This function is not synthesizable
-- * __NB__: This function is lossy. I.e., @periodToHz . hzToPeriod /= id@.
hzToPeriod :: (HasCallStack, Integral a) => Ratio Natural -> a
hzToPeriod freq
  | freq > 0  = floor ((1.0 / freq) / 1e-12)
  | otherwise = withFrozenCallStack $ error "Zero frequency"

-- | Calculate the period in __fs__, given a frequency in __Hz__
--
-- I.e., to calculate the clock period for a circuit to run at 240 MHz we get
--
-- >>> hzToFs 240e6
-- Femtoseconds 4166666
--
-- If the value @hzToFs@ is applied to is not of the type 'Ratio' 'Natural', you
-- can use @hzToFs ('realToFrac' f)@. Note that if @f@ is negative, @realToFrac@
-- will give an @'Control.Exception.Underflow' ::
-- t'Control.Exception.ArithException'@ without a call stack, making debugging
-- cumbersome.
--
-- * __NB__: This function is not synthesizable
-- * __NB__: This function is lossy. I.e.,  @fsToHz . hzToFs /= id@.
hzToFs :: HasCallStack => Ratio Natural -> Femtoseconds
hzToFs freq
  | freq > 0  = Femtoseconds (floor ((1.0 / freq) / 1e-15))
  | otherwise = withFrozenCallStack $ error "Zero frequency"

-- | Calculate the frequency in __Hz__, given the period in __ps__
--
-- I.e., to calculate the clock frequency of a clock with a period of 5000 ps:
--
-- >>> periodToHz 5000
-- 2.0e8
--
-- Note that if @p@ in @periodToHz ('fromIntegral' p)@ is negative,
-- @fromIntegral@ will give an @'Control.Exception.Underflow' ::
-- t'Control.Exception.ArithException'@ without a call stack, making debugging
-- cumbersome.
--
-- Before Clash 1.8, this function always returned a 'Ratio'
-- 'Natural'. To get the old behavior of this function, use a type application:
--
-- >>> periodToHz @(Ratio Natural) 5000
-- 200000000 % 1
--
-- __NB__: This function is not synthesizable
periodToHz :: (HasCallStack, Fractional a) => Natural -> a
periodToHz period
  | period > 0 = fromRational $ 1.0 / (fromIntegral period * 1e-12)
  | otherwise  = withFrozenCallStack $ error "Zero period"

-- | Calculate the frequency in __Hz__, given the period in __fs__
--
-- I.e., to calculate the clock frequency of a clock with a period of 5000 fs:
--
-- >>> fsToHz (Femtoseconds 5000)
-- 2.0e11
--
-- __NB__: This function is not synthesizable
fsToHz :: (HasCallStack, Fractional a) => Femtoseconds -> a
fsToHz (Femtoseconds period)
  | period > 0 = fromRational $ 1.0 / (fromIntegral period * 1e-15)
  | otherwise  = withFrozenCallStack $ error "Zero period"

-- | Build an 'Automaton' from a function over 'Signal's.
--
-- __NB__: Consumption of continuation of the 'Automaton' must be affine; that
-- is, you can only apply the continuation associated with a particular element
-- at most once.
signalAutomaton ::
  forall dom a b .
  (Signal dom a -> Signal dom b) -> Automaton (->) a b
signalAutomaton dut = Automaton $ \input0 -> unsafePerformIO $ do
  inputRefs <- infiniteRefList Nothing
  let inputs = input0 :- fmap readInput inputRefs
      readInput ref = unsafePerformIO $ do
        val <- readIORef ref
        case val of
          Nothing -> fail "signalAutomaton: non-affine use of continuation"
          Just x  -> return x

  let go (inRef :- inRefs) (out :- rest) = do
        let next :: Automaton (->) a b
            next = Automaton $ \i -> unsafePerformIO $ do
              old <- atomicModifyIORef inRef (\old -> (Just i,old))
              case old of
                Nothing -> return ()
                Just _  -> fail "signalAutomaton: non-affine use of continuation"
              unsafeInterleaveIO (go inRefs rest)
        return (out, next)

  go inputRefs (dut inputs)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE signalAutomaton #-}

infiniteRefList :: a -> IO (Signal dom (IORef a))
infiniteRefList val = go
 where
  go = do
    rest <- unsafeInterleaveIO go
    ref  <- newIORef val
    return (ref :- rest)

data ClockAB
  -- | Clock edge A produced
  = ClockA
  -- | Clock edge B produced
  | ClockB
  -- | Clock edges coincided
  | ClockAB
  deriving (Generic, Eq, Show, NFData, NFDataX)

-- | Given two clocks, produce a list of clock ticks indicating which clock
-- (or both) ticked. Can be used in components handling multiple clocks, such
-- as @unsafeSynchronizer@ or dual clock FIFOs.
--
-- If your primitive does not care about coincided clock edges, it should - by
-- convention - replace it by @ClockB:ClockA:@.
clockTicks ::
  (KnownDomain domA, KnownDomain domB) =>
  Clock domA ->
  Clock domB ->
  [ClockAB]
clockTicks clkA clkB = clockTicksEither (toEither clkA) (toEither clkB)
 where
  toEither ::
    forall dom.
    KnownDomain dom =>
    Clock dom ->
    Either Int64 (Signal dom Int64)
  toEither (Clock _ maybePeriods)
    | Just periods <- maybePeriods =
        Right (unFemtosecondsSignal periods)
    | SDomainConfiguration{sPeriod} <- knownDomain @dom =
        -- Convert to femtoseconds - dynamic clocks use them
        Left (1000 * snatToNum sPeriod)

  -- Coerce whole signal instead of `fmap coerce` to prevent useless constructor
  -- packing and unpacking.
  unFemtosecondsSignal :: forall dom . Signal dom Femtoseconds -> Signal dom Int64
  unFemtosecondsSignal = coerce

-- | Given two clock periods, produce a list of clock ticks indicating which clock
-- (or both) ticked. Can be used in components handling multiple clocks, such
-- as @unsafeSynchronizer@ or dual clock FIFOs.
--
-- If your primitive does not care about coincided clock edges, it should - by
-- convention - replace it by @ClockB:ClockA:@.
clockTicksEither ::
  Either Int64 (Signal domA Int64) ->
  Either Int64 (Signal domB Int64) ->
  [ClockAB]
clockTicksEither clkA clkB =
  case (clkA, clkB) of
    (Left  tA, Left  tB) | tA == tB -> repeat ClockAB
    (Left  tA, Left  tB) -> goStatic 0 tA tB
    (Right tA, Right tB) -> goDynamic 0 tA tB
    (Left  tA, Right tB) -> clockTicksEither (Right (pure tA)) (Right tB)
    (Right tA, Left  tB) -> clockTicksEither (Right tA) (Right (pure tB))
 where
  -- Given
  --   tAbsA = absolute time of next active edge of clock A
  --   tAbsB = absolute time of next active edge of clock B
  -- relativeTime is defined as relativeTime = tAbsB - tAbsA
  --
  -- Put differently, relative time 0 points at the next active edge of
  -- clock A, and relativeTime points at the next active edge of clock B.

  goStatic :: Int64 -> Int64 -> Int64 -> [ClockAB]
  goStatic relativeTime tA tB =
    case compare relativeTime 0 of
      LT -> ClockB  : goStatic (relativeTime + tB)      tA tB
      EQ -> ClockAB : goStatic (relativeTime - tA + tB) tA tB
      GT -> ClockA  : goStatic (relativeTime - tA)      tA tB

  goDynamic :: Int64 -> Signal domA Int64 -> Signal domB Int64 -> [ClockAB]
  goDynamic relativeTime tsA@(~(tA :- tsA0)) tsB@(~(tB :- tsB0)) =
    -- Even though we lazily match on the signal's constructor, this shouldn't
    -- build up a significant chain of chunks as 'relativeTime' gets evaluated
    -- every iteration.
    case compare relativeTime 0 of
      LT -> ClockB  : goDynamic (relativeTime + tB)      tsA  tsB0
      EQ -> ClockAB : goDynamic (relativeTime - tA + tB) tsA0 tsB0
      GT -> ClockA  : goDynamic (relativeTime - tA)      tsA0 tsB
