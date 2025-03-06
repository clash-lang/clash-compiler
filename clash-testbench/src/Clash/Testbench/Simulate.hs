{-|
Copyright:    (C) 2023 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>

All it needs for building and running test benches that are created
from Clash circuitry.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Clash.Testbench.Simulate
  ( TB
  , LiftTB((@@))
  , SimSettings(..)
  , simulate
  , simulateFFI
  , tbProperty
  , tbPropertyFFI
  , ffiCheckGroup
  , ffiHedgehog
  ) where

import Prelude hiding (putStrLn, putStr, print)
import qualified Prelude as Prelude (putStrLn, putStr)

import Control.Concurrent (forkOS)
import Control.Concurrent.MVar
import Control.Exception (catch)
import Control.Monad.IO.Class
import Control.Monad.State.Lazy hiding (lift)
import Data.Proxy

import qualified Hedgehog (Property, Group, property, checkSequential)

import Data.Array ((!))
import Data.Coerce (Coercible)
import Data.IORef
import Data.Bits (complement)
import Data.Typeable (Typeable)
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (free)
import Control.Exception (SomeException, try)
import Data.Int (Int64)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.Array as A

import Clash.Prelude
  ( KnownDomain(..), BitSize, BitPack(..), SNat(..), Bit
  , natVal, resize, low, high, boolToBit
  )

import Clash.FFI.Monad
import Clash.FFI.VPI.Info
import Clash.FFI.VPI.Callback
import Clash.FFI.VPI.Control
import Clash.FFI.VPI.IO
import Clash.FFI.VPI.Module
import Clash.FFI.VPI.Object
import Clash.FFI.VPI.Port

import Clash.Testbench.Internal.ID
import Clash.Testbench.Internal.Signal
import Clash.Testbench.Internal.Monad

-- | Simulation Settings
data SimSettings =
  SimSettings
    { quietRun :: Bool
    , validate :: Bool
    }
  deriving (Eq, Ord, Show)

class PutStr s m where
  putStr :: (?settings :: SimSettings) => s -> m ()
  putStrLn :: (?settings :: SimSettings) => s -> m ()

instance PutStr String IO where
  putStr x = when (not $ quietRun ?settings) $ Prelude.putStr x
  putStrLn x = when (not $ quietRun ?settings) $ Prelude.putStrLn x

instance PutStr B.ByteString IO where
  putStr x = when (not $ quietRun ?settings) $ B.putStr x
  putStrLn x = when (not $ quietRun ?settings) $ B.putStrLn x

instance PutStr B.ByteString (SimCont o) where
  putStr x = when (not $ quietRun ?settings) (simPutStr x >> simFlushIO)
  putStrLn x = when (not $ quietRun ?settings) (simPutStrLn x >> simFlushIO)

instance PutStr String (SimCont o) where
  putStr = putStr . B.pack
  putStrLn = putStrLn . B.pack

{-
class Print m where
  print :: (?settings :: SimSettings, Show a) => a -> m ()

instance Print IO where
  print = putStrLn . show

instance Print (SimCont o) where
  print = putStrLn . B.pack . show
-}

-- | @simulate n testbench@ simulates the @testbench@, created in the
-- 'Clash.Testbench.Simulate.TB' context, for @n@ simulation steps.
--
-- The simulation is run on the native Clash implementation, as given
-- by the Clash signals and signal functions lifted into 'TB'.
simulate :: (MonadIO m, Verify m) => SimSettings -> TB a -> m a
simulate simSettings@SimSettings{..} testbench = do
  let ?settings = simSettings
  (r, Testbench{..}) <- liftIO $ runTB Internal testbench
  replicateM_ tbSimSteps $ do
    forM_ tbDomains $ \(d, map (tbSignalLookup !) -> xs) ->
      (`onAllDomainTypes` d) $ \(TBDomain{..} :: TBDomain 'FINAL dom) -> do
        forM_ xs $ onAllSignalTypes $ \s -> do
          when validate $ case s of
            SimSignal{..} -> verify signalVerify
            _ -> return ()
          v <- liftIO $ signalCurVal s
          when (not quietRun) $ case signalPrint s of
            Nothing    -> return ()
            Just toStr -> liftIO $ putStrLn . (<> toStr v) $ case s of
              IOInput{}   -> "I "
              SimSignal{} -> "O "
              TBSignal{}  -> "S "
        liftIO $ modifyIORef simStepRef (+ 1)
  return r

-- | Turns a test bench design into a 'Hedgehog.Property' to be
-- simulated with Haskell.
tbProperty :: TB () -> Hedgehog.Property
tbProperty = Hedgehog.property . simulate
  SimSettings
    { quietRun = True
    , validate = True
    }

data VPIState =
  VPIState
    { testbench   :: Testbench
      -- multiple clocks are not supported yet, currently all clocks
      -- are synchronously executed.
    , vpiClock    :: Bit
    , vpiSimSteps :: Int
    , vpiInit     :: Bool
    , syncA       :: MVar ()
    , syncB       :: MVar Bool
    }

type Sync = (MVar (), MVar Bool)

ffiHedgehog :: IO Sync
ffiHedgehog = (,) <$> newEmptyMVar <*> newEmptyMVar

ffiCheckGroup :: Sync -> Hedgehog.Group -> IO ()
ffiCheckGroup (syncA, _) g = do
  void $ forkOS (void $ Hedgehog.checkSequential g >> putMVar syncA ())
  takeMVar syncA

-- | @simulate n testbench@ simulates the @testbench@, created in the
-- 'TB' context, for @n@ simulation steps with an external simulator
-- bound via Clash-FFI.
--
-- Note that this function is not executable in a standard Haskell
-- environment, but must to be bound to a @ffiMain@ foreign call that
-- is shipped via a shared library and executed by an external
-- simulator. See Clash-FFI for more details.
simulateFFI :: (MonadIO m, Verify m) => Sync -> SimSettings -> TB a -> m a
simulateFFI (syncA, syncB) simSettings tb = do
  let ?settings = simSettings
  (r, testbench@Testbench{..}) <- liftIO $ runTB External tb
  success <- liftIO $ do
    let vpiClock    = low
        vpiSimSteps = tbSimSteps - 1
        vpiInit     = True
    let ?state = VPIState{..}
    initializeSimulation
    putMVar syncA ()
    takeMVar syncB

  unless (success) $
    -- re-verify at the current cycle to produce a failure in the
    -- current MonadIO context
    forM_ tbDomains $ \(d, map (tbSignalLookup !) -> xs) ->
      (`onAllDomainTypes` d) $ \(TBDomain{} :: TBDomain 'FINAL dom) -> do
        forM_ xs $ onAllSignalTypes $ \case
          SimSignal{..} -> verify signalVerify
          _ -> return ()

  return r

-- | Turns a test bench design into a 'Hedgehog.Property' to be
-- simulated with external simulator.
tbPropertyFFI :: Sync -> TB () -> Hedgehog.Property
tbPropertyFFI sync = Hedgehog.property . simulateFFI sync
  SimSettings
    { quietRun = True
    , validate = True
    }

initializeSimulation :: (?state :: VPIState, ?settings :: SimSettings) => IO ()
initializeSimulation = runSimAction $ do
  -- reset the simulator to ensure some defined initial state
  -- controlSimulator $ Reset Processing Nothing NoDiagnostics

  -- print simulator info
  putStrLn "[ Simulator Info ]"
  Info{..} <- receiveSimulatorInfo
  putStrLn infoProduct
  putStrLn infoVersion
  putStrLn ""

  -- print top modules
  putStrLn "[ Top Modules ]"
  tops' <- topModules
  topNames <- mapM (receiveProperty Name) tops'
  mapM_ putStrLn topNames
  putStrLn ""

  -- iverilog runs into problems if iterated objects are used as a
  -- long-term reference. Hence, they only should be used for
  -- analyzing the architecture upfront. For long-term references to
  -- be reusable during simulation, the objects should be queried via
  -- their architectural name reference instead.
  topM <- M.fromList
    <$> mapM (\x -> (B.unpack x, ) <$> findTopModule x) topNames

  -- add the VPI module references to the signals
  vpiSignals <-
    forM tbSignals $ onAllSignalTypes $ \case
      s@SimSignal{..} ->
        case M.lookup signalName topM of
          Just m  -> (signalId, ) . SomeSignal <$> matchModule m s
          Nothing -> error $ "No module matches \"" <> signalName <> "\""
      x -> return (signalId x, SomeSignal x)

  let
    ?state =
      ?state
        { testbench = testbench
            { tbSignals      = map snd vpiSignals
            , tbSignalLookup = A.array (A.bounds tbSignalLookup) vpiSignals
            }
        }

  putStrLn "[ Simulation start ]"
  putStrLn ""

  nextCB ReadWriteSynch 0 assignInputs
 where
  VPIState{..} = ?state
  Testbench{..} = testbench

assignInputs :: (?state :: VPIState, ?settings :: SimSettings) => SimAction ()
assignInputs = do
--  SimTime time <- receiveTime Sim (Nothing @Object)
--  putStrLn $ "assignInputs " <> show (time, vpiClock, vpiInit)

  forM_ tbDomains $ \(d, map (tbSignalLookup !) -> xs) ->
    (`onAllDomainTypes` d) $ const $ do
      forM_ xs $ onAllSignalTypes $ \case
        SimSignal{..} -> mapM_ (assignModuleInputs signalPlug) signalDeps
        _             -> return ()

  let ?state = ?state { vpiClock = complement vpiClock
                      , vpiInit = False
                      }

  if vpiClock == high
  then nextCB ReadWriteSynch 1 assignInputs
  else nextCB ReadOnlySynch  1 readOutputs

 where
  VPIState{..} = ?state
  Testbench{..} = testbench

  assignModuleInputs ::
    Typeable b =>
    Maybe ModuleInterface ->
    ID () -> SimCont b ()
  assignModuleInputs = \case
    Nothing                  -> const $ return ()
    Just ModuleInterface{..} -> \sid@(SomeID x) ->
      let PortInterface{..} = inputPort sid
      in case x of
         NoID                 -> return ()
         ClockID  _TODO       -> sendV port vpiClock
         ResetID  _TODO       -> sendV port $ boolToBit vpiInit
         EnableID _TODO       -> sendV port high
         i@(SignalID _TODO)
           | vpiClock == high -> return ()
           | otherwise        ->
               (`onAllSignalTypes` (tbSignalLookup ! i)) $ \s ->
                 liftIO (signalCurVal s) >>= \v -> do
                   sendV port v

  sendV :: (BitPack a, Typeable b) => Port -> a -> SimCont b ()
  sendV port v =
    liftIO $ runSimAction $
      sendValue port (BitVectorVal SNat $ pack v)
        $ InertialDelay $ SimTime 0

readOutputs :: (?state :: VPIState, ?settings :: SimSettings) => SimAction ()
readOutputs = do
--  SimTime time <- receiveTime Sim (Nothing @Object)
--  putStrLn $ "readOutputs " <> show time

  failure <- fmap or $ forM tbDomains $ \(d, map (tbSignalLookup !) -> xs) ->
    (`onAllDomainTypes` d) $ \(TBDomain{..} :: TBDomain 'FINAL dom) -> do
      -- receive the outputs
      forM_ xs $ onAllSignalTypes $ \case
        SimSignal{..} -> case signalPlug of
          Nothing -> error "Cannot read from module"
          Just ModuleInterface{..} ->
            receiveValue VectorFmt (port outputPort) >>= \case
              BitVectorVal SNat v -> case signalUpdate of
                Just upd -> liftIO $ upd $ unpack $ resize v
                Nothing  -> error "No signal update"
              _ -> error "Unexpected return format"
        _ -> return ()
      -- print the watched signals
      failure <- fmap or $ forM xs $ onAllSignalTypes $ \s -> do
        failure <-
          if not validate then return False else case s of
            SimSignal{..} -> liftIO $ catch
              (verify signalVerify >> return False)
              (\(_ :: SomeException) -> return True)
            _ -> return False
        v <- liftIO $ signalCurVal s
        when (not quietRun) $ case signalPrint s of
          Nothing    -> return ()
          Just toStr -> putStrLn . B.pack . (<> toStr v) $ case s of
            IOInput{}   -> "I "
            SimSignal{} -> "O "
            TBSignal{}  -> "S "
        return failure
      -- proceed time for all instances not running trough Clash-FFI
      unless failure
        $ liftIO $ modifyIORef simStepRef (+ 1)
      return failure

  if failure then do
    putStrLn ""
    putStrLn "[ Simulation failed ]"

    liftIO $ do
      putMVar syncB False
      takeMVar syncA

    liftIO $ void $ try @SomeException $ runSimAction
      $ controlSimulator $ Finish NoDiagnostics
  else if vpiSimSteps > 0 then do
    let ?state = ?state { vpiSimSteps = vpiSimSteps - 1 }
    nextCB ReadWriteSynch 1 assignInputs
  else do
    putStrLn ""
    putStrLn "[ Simulation done ]"

    liftIO $ do
      putMVar syncB True
      takeMVar syncA

    liftIO $ void $ try @SomeException $ runSimAction
      $ controlSimulator $ Finish NoDiagnostics
 where
  VPIState{..} = ?state
  SimSettings{..} = ?settings
  Testbench{..} = testbench

matchModule ::
  ( ?state :: VPIState, ?settings :: SimSettings
  , KnownDomain dom, BitPack a, Typeable b
  ) => Module -> TBSignal 'FINAL dom a -> SimCont b (TBSignal 'FINAL dom a)
matchModule module_ = \case
  tbs@SimSignal{..} -> do
    ports <- modulePorts module_
    dirs  <- mapM direction ports

    let
      inputPorts  = map fst $ filter (isInput  . snd) $ zip ports dirs
      outputPorts = map fst $ filter (isOutput . snd) $ zip ports dirs

    inputPort <-
      (M.!) . M.fromList
        <$> ( mapM (matchPort module_)
            $ zip signalDeps
            $ map Just inputPorts <> repeat Nothing
            )

    outputPort <- case outputPorts of
      [p] -> do
        portNameBS    <- receiveProperty Name p
        portSize      <- fromEnum <$> getProperty Size p
        portIndex     <- fromEnum <$> getProperty PortIndex p
        portDirection <- direction p

        let portName = B.unpack portNameBS
        port <- getByName (Just module_) portNameBS

        checkPort (toInteger portSize) tbs portDirection

        return $ PortInterface{..}
      _   -> error "TODO: later / "

    return tbs { signalPlug = Just ModuleInterface{..} }
  _ -> error "Unfiltered TBS"

 where
  isInput = \case
    Input -> True
    _     -> False

  isOutput = \case
    Output -> True
    _      -> False

matchPort ::
  (?state :: VPIState, ?settings :: SimSettings, Typeable b) =>
  Module -> (ID (), Maybe Port) -> SimCont b (ID (), PortInterface)
matchPort m = \case
  (_, Nothing)   -> error "Not enough ports"
  (sid, Just p) -> liftIO $ runSimAction $ do
    portNameBS    <- receiveProperty Name p
    portSize      <- fromEnum <$> getProperty Size p
    portIndex     <- fromEnum <$> getProperty PortIndex p
    portDirection <- direction p

    let portName = B.unpack portNameBS
    checkID portName portSize portDirection sid

    -- Get a long-term reference via direct name access. Iterator
    -- references may not be persistent.
    port <- getByName (Just m) portNameBS

    return (sid, PortInterface{..})
 where
  VPIState{..} = ?state
  Testbench{..} = testbench

  match :: forall b. Int -> Int -> String -> String -> SimCont b ()
  match n k tName pName =
    when (n /= k) $ error $ "Not a " <> tName <> " port: " <> pName

  checkID :: forall b. String -> Int -> Direction -> ID () -> SimCont b ()
  checkID name size dir (SomeID x) = case x of
    ClockID{}    -> match size 1 "clock" name
    ResetID{}    -> match size 1 "reset" name
    EnableID{}   -> match size 1 "enable" name
    NoID         -> error "NoID, TODO check"
    i@SignalID{} -> (`onAllSignalTypes` (tbSignalLookup ! i)) $ \s ->
                       checkPort (toInteger size) s dir

checkPort ::
  forall dom a b.
  (BitPack a, KnownDomain dom) =>
  Integer -> TBSignal 'FINAL dom a -> Direction -> SimCont b ()
checkPort s
  | natVal (Proxy @(BitSize a)) /= s = error "port size does not match"
  | otherwise = \case
      IOInput{} -> \case
        Input -> return ()
        _     -> error "No Input"
      _ -> const $ return ()

getByName ::
  (Coercible a Object, Show a, Typeable a, Coercible Object b) =>
  Maybe a -> B.ByteString -> SimCont o b
getByName m name = do
  ref <- liftIO $ newCString $ B.unpack name
  obj <- getChild ref m
  liftIO $ free ref
  return obj

nextCB ::
  (Maybe Object -> Time -> CallbackReason) ->
  Int64 ->
  SimAction () ->
  SimAction ()
nextCB reason time action =
  void $ liftIO $ runSimAction $ registerCallback
    CallbackInfo
      { cbReason  = reason Nothing (SimTime time)
      , cbRoutine = const (runSimAction action >> return 0)
      , cbIndex   = 0
      , cbData    = B.empty
      }
