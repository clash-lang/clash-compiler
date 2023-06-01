{-|
Copyright:    (C) 2023 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>

Lifted signal types and internal data structures for
'Clash.Testbench.Internal.TB' (internal module).
-}
module Clash.Testbench.Internal.Signal where

import Algebra.PartialOrd
import Data.Function (on)

import Hedgehog (PropertyT)

import Data.Array.IO (IOArray)
import Data.IORef (IORef)
import Clash.Prelude
  ( KnownDomain(..), BitPack(..), SDomainConfiguration(..), NFDataX, Type
  , Domain, Signal, Clock, Reset, Enable
  , ssymbolToString
  )

import Clash.FFI.VPI.Module (Module)
import Clash.FFI.VPI.Port (Port, Direction)
import Clash.Testbench.Internal.ID

-- | Test bench design stages
data Stage :: Type where
  USER  :: Stage
  -- ^ The test bench is created in the USER stage. The elements of
  -- the test bench are setup by the user inside the
  -- 'Clash.Testbench.Internal.Monad.TB' monad during this stage.
  FINAL :: Stage
  -- ^ The FINAL stage is reached once the test bench has been created
  -- and all elements of the setup are known. Furthermore,
  -- post-processing of the setup has passed
  -- successfully. Post-processing also introduces the switch from
  -- 'USER' to 'FINAL' on the type level.

-- | The supported simulation modes sources.
data SimMode where
  Internal :: SimMode
  -- ^ Internal pure Haskell based simulation
  External :: SimMode
  -- ^ Co-Simulation via Clash-FFI

-- | Type family for handling simulation mode dependent types.
-- 'SimMode' does not have to be fixed during test bench creation, but
-- will be fixed once the test bench got finalized. Hence, at the
-- final stage the 'SimMode' argument gets obsolete.
type family SimModeDependent (s :: Stage) a where
  SimModeDependent 'USER  a = SimMode -> a
  SimModeDependent 'FINAL a = a

-- | Clash-FFI Port connector.
data PortInterface =
  PortInterface
    { port          :: Port
    , portName      :: String
    , portSize      :: Int
    , portIndex     :: Int
    , portDirection :: Direction
    }

-- | Clash-FFI Module connector.
data ModuleInterface =
  ModuleInterface
    { module_       :: Module
    , inputPort  :: ID () -> PortInterface
      -- TODO: multiple port support vie Bundle/Unbundle
    , outputPort :: PortInterface
    }

data History a =
  History
    { historySize :: IORef Int
    , historyBufferPos :: IORef Int
    , historyBuffer :: IORef (Maybe (IOArray Int (Maybe a)))
    }

-- | Expectations on certain outputs at the given simulation step.
newtype Expectation a = Expectation { expectation :: (Int, a -> PropertyT IO ()) }

-- | Expectations cannot be compared: they are always unequal.
instance Eq (Expectation a) where
  _ == _ = False

-- | Expectations enjoy some partial order on the simulation steps at
-- which they are verified.
instance PartialOrd (Expectation a) where
  leq        (Expectation (x, _)) (Expectation (y, _)) = x <= y
  comparable (Expectation (x, _)) (Expectation (y, _)) = x /= y

-- | The lifted 'Clash.Signal.Signal' type to be used in
-- 'Clash.Testbench.Internal.Monad.TB'.
data TBSignal (s :: Stage) (dom :: Domain) a =
    -- | A signal that is simulated
    SimSignal
      { signalId :: ID SIGNAL
      -- ^ Some unique signal ID
      , signalCurVal :: SimModeDependent s (IO a)
      -- ^ The data value that is captured by the signal at the
      -- current simulation step
      , signalName :: String
      -- ^ Some name identifier for the signal (this name is used for
      -- module port matching in case of simulation with an external
      -- simulator)
      , signalOrigin :: Signal dom a
      -- ^ The Clash signal, out of which the test bench signal has
      -- been created (for internal use only)
      , signalDeps :: [ID ()]
      -- ^ The dependencies of the signal (i.e., all other input
      -- signals whose content is required for computing the values of
      -- this signal)
      , signalExpect :: Expectation a -> IO ()
      -- ^ Registers an expectation on the content of this signal to
      -- be verified during simulation
      , signalVerify :: SimModeDependent s (PropertyT IO ())
      -- ^ The expectation verifier
      , signalHistory :: History a
      -- ^ Bounded history of signal values
      , signalUpdate :: Maybe (a -> IO ())
      -- ^ Overwrites the value of the signal at the current
      -- simulation step (only available in external simulation mode)

      -- TODO: Use proper type families instead of the 'Maybe' wrapper
      -- here.
      , signalPlug :: Maybe ModuleInterface
      -- ^ Some external module interface whose ports match with this
      -- signal's type (only available in external simulation mode)
      , signalPrint :: Maybe (a -> String)
      -- ^ Some optional value printer for inspection of the signal content
      }
    -- | A signal that receives its content via some IO
  | IOInput
      { signalId :: ID SIGNAL
      -- ^ Some unique signal ID
      , signalCurVal :: SimModeDependent s (IO a)
      -- ^ The data value hold by the signal at the current simulation step
      , signalHistory :: History a
      -- ^ Bounded history of signal values
      , signalPrint :: Maybe (a -> String)
      -- ^ Some optional value printer for inspection of the signal content
      }
    -- | A signal that results from composition
  | TBSignal
      { signalId :: ID SIGNAL
      -- ^ This is always 'Clash.Testbench.Internal.ID.NoID', because
      -- it is impossible to keep track of signals that are created
      -- via some functor or applicative composition (note that
      -- tracking those is also not necessary: the corresponding
      -- transformation cannot be run through an external execution
      -- engine anyway)
      , signalCurVal :: SimModeDependent s (IO a)
      -- ^ The data value hold by the signal at the current simulation step
      , signalPrint :: Maybe (a -> String)
      -- ^ Some optional value printer for inspection of the signal content
      }

-- | For internal use only (this is __not__ connected to the data that
-- is hold by the signal)
instance KnownDomain dom => Show (TBSignal s dom a) where
  show = case knownDomain @dom of
    SDomainConfiguration domainName _ _ _ _ _ -> \case
      SimSignal{..} ->
        "Signal \""
          <> signalName <> "\" @"
          <> ssymbolToString domainName <> " "
          <> show signalId <> " "
          <> show signalDeps
      IOInput{..} ->
        "Input " <> show signalId
      TBSignal{} ->
        "TS"

-- | For internal use only (this is __not__ connected to the data that
-- is hold by the signal)
instance Eq (TBSignal s dom a) where
  (==) = (==) `on` signalId

-- | For internal use only (this is __not__ connected to the data that
-- is hold by the signal)
instance Ord (TBSignal s dom a) where
  compare = compare `on` signalId

instance Functor (TBSignal 'USER dom) where
  fmap f s =
    TBSignal
      { signalId     = NoID
      , signalCurVal = fmap f . signalCurVal s
        -- we lose printing abilities at this point. This is fine,
        -- since printing capabilities are recovered automatically
        -- once the mapped signal requires printing capabilities
        -- again.
      , signalPrint  = Nothing
      }

instance Applicative (TBSignal 'USER dom) where
  pure x =
    TBSignal
      { signalId     = NoID
      , signalCurVal = const $ pure x
      , signalPrint  = Nothing
      }

  f <*> s =
    TBSignal
      { signalId     = NoID
      , signalCurVal = \m -> signalCurVal f m <*> signalCurVal s m
      , signalPrint  = Nothing
      }

-- | The lifted 'Clash.Signal.Clock' type to be used in
-- 'Clash.Testbench.Internal.Monad.TB'.
data TBClock (s :: Stage) (dom :: Domain) where
  AutoClock ::
    forall dom.
      KnownDomain dom =>
      TBClock 'USER dom
  TBClock ::
    forall s dom.
      KnownDomain dom =>
        { clock       :: Clock dom
        , clockId     :: ID DOMAIN
        , clockSource :: IO (Clock dom)
        } ->
      TBClock s dom

instance KnownDomain dom => Show (TBClock s dom) where
  show clk = case knownDomain @dom of
    SDomainConfiguration domainName _ _ _ _ _ ->
      "Clock @" <> ssymbolToString domainName <> " " <>
      ( case clk of
          AutoClock   -> "auto"
          TBClock{..} -> show clockId
      )

instance Eq (TBClock s dom) where
  (==) = \case
    AutoClock -> \case
      AutoClock -> True
      _         -> False
    x@TBClock{} -> \case
      y@TBClock{} -> clockId x == clockId y
      _           -> False

instance Ord (TBClock s dom) where
  compare = \case
    AutoClock -> \case
      AutoClock -> EQ
      TBClock{} -> LT
    x@TBClock{} -> \case
      y@TBClock{} -> compare (clockId x) (clockId y)
      AutoClock   -> GT

-- | The lifted 'Clash.Signal.Clock' type to be used in
-- 'Clash.Testbench.Internal.Monad.TB'.
data TBReset (s :: Stage) (dom :: Domain) where
  AutoReset ::
    forall dom.
      KnownDomain dom =>
      TBReset 'USER dom
  TBReset ::
    forall s dom.
      KnownDomain dom =>
        { reset       :: Reset dom
        , resetId     :: ID DOMAIN
        , resetCurVal :: SimModeDependent s (IO Bool)
        , resetUpdate :: Bool -> IO ()
        } ->
      TBReset s dom

instance KnownDomain dom  => Show (TBReset s dom) where
  show rst = case knownDomain @dom of
    SDomainConfiguration domainName _ _ _ _ _ ->
      "Reset @" <> ssymbolToString domainName <> " " <>
      ( case rst of
          AutoReset   -> "auto"
          TBReset{..} -> show resetId
      )

instance Eq (TBReset s dom) where
  (==) = \case
    AutoReset -> \case
      AutoReset -> True
      _         -> False
    x@TBReset{} -> \case
      y@TBReset{} -> resetId x == resetId y
      _           -> False

instance Ord (TBReset s dom) where
  compare = \case
    AutoReset -> \case
      AutoReset -> EQ
      TBReset{} -> LT
    x@TBReset{} -> \case
      y@TBReset{} -> compare (resetId x) (resetId y)
      AutoReset   -> GT

-- | The lifted 'Clash.Signal.Enable' type to be used in
-- 'Clash.Testbench.Internal.Monad.TB'.
data TBEnable (s :: Stage) (dom :: Domain) where
  AutoEnable ::
    forall dom.
      KnownDomain dom =>
      TBEnable 'USER dom
  TBEnable ::
    forall s dom.
    KnownDomain dom =>
      { enable       :: Enable dom
      , enableId     :: ID DOMAIN
      , enableCurVal :: SimModeDependent s (IO Bool)
      , enableUpdate :: Bool -> IO ()
      } ->
    TBEnable s dom

instance KnownDomain dom => Show (TBEnable s dom) where
  show enb = case knownDomain @dom of
    SDomainConfiguration domainName _ _ _ _ _ ->
      "Enable @" <> ssymbolToString domainName <> " " <>
      ( case enb of
          AutoEnable   -> "auto"
          TBEnable{..} -> show enableId
      )

instance Eq (TBEnable s dom) where
  (==) = \case
    AutoEnable -> \case
      AutoEnable -> True
      _         -> False
    x@TBEnable{} -> \case
      y@TBEnable{} -> enableId x == enableId y
      _           -> False

instance Ord (TBEnable s dom) where
  compare = \case
    AutoEnable -> \case
      AutoEnable -> EQ
      TBEnable{} -> LT
    x@TBEnable{} -> \case
      y@TBEnable{} -> compare (enableId x) (enableId y)
      AutoEnable   -> GT

-- | Existential data type wrapper for 'TBSignal'.
data SomeSignal (s :: Stage) where
  SomeSignal ::
    forall s dom a.
      (KnownDomain dom, NFDataX a, BitPack a) =>
      TBSignal s dom a ->
      SomeSignal s

instance Eq (SomeSignal s) where
  (==) = (==) `on` (signalId `onAllSignalTypes`)

instance Ord (SomeSignal s) where
  compare = compare `on` (signalId `onAllSignalTypes`)

instance Show (SomeSignal s) where
  show = (show `onAllSignalTypes`)

-- | Applies a 'TBSignal' transformation inside the existential
-- context of 'SomeSignal'.
--
-- Note that this implementation supports multiple constructors of
-- 'SomeSignal' although there may be only one right now.
onAllSignalTypes ::
  forall s b.
  ( forall dom a.
    (KnownDomain dom, NFDataX a, BitPack a) =>
    TBSignal s dom a -> b
  ) ->
  SomeSignal s ->
  b
onAllSignalTypes f = \case
  SomeSignal s -> f s

-- | The internal 'Clash.Signal.Domain' representation that is used
-- inside 'Clash.Testbench.Internal.TB'.
data TBDomain (s :: Stage) (dom :: Domain) =
  TBDomain
    { domainClock  :: Maybe (TBClock s dom)
    , domainReset  :: Maybe (TBReset s dom)
    , domainEnable :: Maybe (TBEnable s dom)
    , simStepRef   :: IORef Int
    }

-- | Existential data type wrapper for 'TBDomain'.
data SomeDomain (s :: Stage) where
  SomeDomain ::
    forall s dom.
      KnownDomain dom =>
      TBDomain s dom ->
      SomeDomain s

instance Show (SomeDomain s) where
  show = \case
    SomeDomain (_ :: TBDomain s dom) -> case knownDomain @dom of
      SDomainConfiguration domainName _ _ _ _ _ ->
        ssymbolToString domainName

-- | Applies a 'TBDomain' transformation inside the existential
-- context of 'SomeDomain'.
--
-- Note that this implementation supports multiple constructors of
-- 'SomeDomain' although there may be only one right now.
onAllDomainTypes ::
  forall s b.
  ( forall dom.
    KnownDomain dom =>
    TBDomain s dom -> b
  ) ->
  SomeDomain s ->
  b
onAllDomainTypes f = \case
  SomeDomain d -> f d
