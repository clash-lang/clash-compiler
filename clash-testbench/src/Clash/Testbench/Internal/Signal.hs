module Clash.Testbench.Internal.Signal where

import Data.Function (on)

import Clash.Prelude
  ( KnownDomain(..), BitPack(..), SDomainConfiguration(..), NFDataX
  , Domain, Signal, Clock, Reset, Enable
  , ssymbolToString
  )

import Hedgehog (Gen)

import Clash.FFI.VPI.Module (Module)
import Clash.FFI.VPI.Port (Port, Direction)

import Clash.Testbench.Internal.ID

data VPIPort =
  VPIPort
    { port          :: Port
    , portName      :: String
    , portSize      :: Int
    , portIndex     :: Int
    , portDirection :: Direction
    }

data VPIInstance =
  VPIInstance
    { vpiModule     :: Module
    , vpiInputPort  :: ID 'FINAL () -> VPIPort
      -- TODO: multiple port support vie Bundle/Unbundle
    , vpiOutputPort :: VPIPort
    }

data TBSignal (s :: Stage) (dom :: Domain) a =
    TBSignal
      { signalId     :: ID s SIGNAL
      , signalDeps   :: [ID s ()]
      , signalName   :: String
      , signal       :: Signal dom a
      , signalCurVal :: IO a
      , signalUpdate :: Maybe (a -> IO ())
      , signalPrint  :: Maybe (a -> String)
      , vpiInstance  :: Maybe VPIInstance
      }
  | IOInput
      { signalId     :: ID s SIGNAL
      , signalCurVal :: IO a
      , signalPrint  :: Maybe (a -> String)
      }
  | Generator
      { signalId     :: ID s SIGNAL
      , signalCurVal :: IO a
      , signalPrint  :: Maybe (a -> String)
      , generator    :: Gen a
      }

instance (KnownDomain dom, AnyStage s) => Show (TBSignal s dom a) where
  show = case knownDomain @dom of
    SDomainConfiguration domainName _ _ _ _ _ -> \case
      TBSignal{..} ->
        "Signal \""
          <> signalName <> "\" @"
          <> ssymbolToString domainName <> " "
          <> show signalId <> " "
          <> show signalDeps
      IOInput{..} ->
        "Input " <> show signalId
      Generator{..} ->
        "Gen " <> show signalId

instance AnyStage s => Eq (TBSignal s dom a) where
  (==) = (==) `on` signalId

instance AnyStage s => Ord (TBSignal s dom a) where
  compare = compare `on` signalId

instance Functor (TBSignal 'USER dom) where
  fmap f = \case
    TBSignal{..} ->
      TBSignal
        { signalId     = NoID
        , signal       = fmap f signal
        , signalCurVal = f <$> signalCurVal
          -- We cannot update the values of a mapped signal, which
          -- makes sense, since a mapped signal cannot be simulated
          -- externally. It is always defined as the result of
          -- applying 'f' to the given source signal.
        , signalUpdate = Nothing
          -- we lose printing abilities at this point. This is fine,
          -- since printing capabilities are recovered automatically
          -- once the new signal requires printing capabilities again.
        , signalPrint  = Nothing
        , ..
        }
    IOInput{..} ->
      IOInput
        { signalId     = NoID
        , signalCurVal = f <$> signalCurVal
        , signalPrint  = Nothing
        , ..
        }
    Generator{..} ->
      Generator
        { signalId = NoID
        , signalCurVal = f <$> signalCurVal
        , signalPrint  = Nothing
        , generator    = f <$> generator
        }

instance Applicative (TBSignal 'USER dom) where
  pure x =
    IOInput
      { signalId     = NoID
      , signalCurVal = pure x
      , signalPrint  = Nothing
      }

  f <*> s =
    IOInput
      { signalId     = NoID
      , signalCurVal = signalCurVal f <*> signalCurVal s
      , signalPrint  = Nothing
      }

data TBClock (s :: Stage) (dom :: Domain) =
  TBClock
    { clock       :: Clock dom
    , clockId     :: ID s CLOCK
    , clockSource :: IO (Clock dom)
    }

instance (KnownDomain dom, AnyStage s) => Show (TBClock s dom) where
  show TBClock{..} = case knownDomain @dom of
    SDomainConfiguration domainName _ _ _ _ _ ->
      "Clock @"
        <> ssymbolToString domainName <> " "
        <> show clockId

instance AnyStage s => Eq (TBClock s dom) where
  (==) = (==) `on` clockId

instance AnyStage s => Ord (TBClock s dom) where
  compare = compare `on` clockId

data TBReset (s :: Stage) (dom :: Domain) =
  TBReset
    { reset       :: Reset dom
    , resetId     :: ID s RESET
    , resetCurVal :: IO Bool
    }

instance (KnownDomain dom, AnyStage s)=> Show (TBReset s dom) where
  show TBReset{..} = case knownDomain @dom of
    SDomainConfiguration domainName _ _ _ _ _ ->
      "Reset @"
        <> ssymbolToString domainName <> " "
        <> show resetId

instance AnyStage s => Eq (TBReset s dom) where
  (==) = (==) `on` resetId

instance AnyStage s => Ord (TBReset s dom) where
  compare = compare `on` resetId

data TBEnable (s :: Stage) (dom :: Domain) =
  TBEnable
    { enable       :: Enable dom
    , enableId     :: ID s ENABLE
    , enableCurVal :: IO Bool
    }

instance (KnownDomain dom, AnyStage s) => Show (TBEnable s dom) where
  show TBEnable{..} = case knownDomain @dom of
    SDomainConfiguration domainName _ _ _ _ _ ->
      "Enable @"
        <> ssymbolToString domainName <> " "
        <> show enableId

instance AnyStage s => Eq (TBEnable s dom) where
  (==) = (==) `on` enableId

instance AnyStage s => Ord (TBEnable s dom) where
  compare = compare `on` enableId

data SomeSignal (s :: Stage) where
  SomeSignal ::
    forall s dom a.
      (KnownDomain dom, NFDataX a, BitPack a) =>
      TBSignal s dom a ->
      SomeSignal s

instance AnyStage s => Eq (SomeSignal s) where
  (==) = (==) `on` (signalId `onAllSignalTypes`)

instance AnyStage s => Ord (SomeSignal s) where
  compare = compare `on` (signalId `onAllSignalTypes`)

instance AnyStage s => Show (SomeSignal s) where
  show = (show `onAllSignalTypes`)

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
