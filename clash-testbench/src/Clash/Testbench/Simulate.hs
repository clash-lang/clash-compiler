{-|
Copyright:    (C) 2023 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>

All it needs for building and running test benches that are created
from Clash circuitry.
-}
{-# LANGUAGE OverloadedStrings #-}
module Clash.Testbench.Simulate
  ( TB
  , LiftTB((@@))
  , simulate
  , simulateFFI
  , tbProperty
  ) where

import Prelude hiding (putStrLn)
import qualified Prelude (putStrLn)

import Control.Monad.IO.Class
import Control.Monad.State.Lazy hiding (lift)
import Data.Proxy

import qualified Hedgehog (Property, property)

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
import Clash.FFI.VPI.IO
import Clash.FFI.VPI.Callback
import Clash.FFI.VPI.Control
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


-- | @simulate n testbench@ simulates the @testbench@, created in the
-- 'Clash.Testbench.Simulate.TB' context, for @n@ simulation steps.
--
-- The simulation is run on the native Clash implementation, as given
-- by the Clash signals and signal functions lifted into 'TB'.
simulate :: (MonadIO m, Verify m) => SimSettings -> TB a -> m a
simulate SimSettings{..} testbench = do
  (r, Testbench{..}) <- liftIO $ runTB Internal testbench
  replicateM_ (tbSimSteps + 1) $ do
    forM_ tbDomains $ \(d, map (tbSignalLookup !) -> xs) ->
      (`onAllDomainTypes` d) $ \(TBDomain{..} :: TBDomain 'FINAL dom) -> do
--        i <- readIORef simStepRef
        forM_ xs $ onAllSignalTypes $ \s -> do
          when validate $ case s of
            SimSignal{..} -> verify signalVerify
            _ -> return ()
          v <- liftIO $ signalCurVal s
          when (not quietRun) $ case signalPrint s of
            Nothing    -> return ()
            Just toStr -> liftIO $ Prelude.putStrLn . (<> toStr v) $ case s of
              IOInput{}   -> "I "
              SimSignal{} -> "O "
              TBSignal{}  -> "S "
        liftIO $ modifyIORef simStepRef (+ 1)
  return r

-- | Turns a test bench design into a 'Hedgehog.Property' according to
-- the given simulation mode.
tbProperty :: TB () -> Hedgehog.Property
tbProperty = Hedgehog.property . simulate
  SimSettings
    { quietRun = True
    , validate = True
    }

{-

  (_, Testbench{..}) <- liftIO $ runTB Internal testbench
  replicateM_ tbSimSteps $ do
    forM_ tbDomains $ \(d, map (tbSignalLookup !) -> xs) ->
      (`onAllDomainTypes` d) $ \(TBDomain{..} :: TBDomain 'FINAL dom) -> do
        forM_ xs $ onAllSignalTypes $ \case
          SimSignal{..} -> signalVerify
          _             -> return ()

        liftIO $ modifyIORef simStepRef (+ 1)
-}
data VPIState =
  VPIState
    { testbench   :: Testbench
      -- multiple clocks are not supported yet, currently all clocks
      -- are synchronously executed.
    , vpiClock    :: Bit
    , vpiSimSteps :: Int
    , vpiInit     :: Bool
    , simSettings :: SimSettings
    }

-- | @simulate n testbench@ simulates the @testbench@, created in the
-- 'TB' context, for @n@ simulation steps with an external simulator
-- bound via Clash-FFI.
--
-- Note that this function is not executable in a standard Haskell
-- environment, but must to be bound to some @ffiMain@ foreign call
-- that is shipped via a shared library and executed by an external
-- simulator. See Clash-FFI for more details on this.
simulateFFI :: MonadIO m => SimSettings -> TB a -> m a
simulateFFI simSettings tb = do
  (r, testbench@Testbench{..}) <- liftIO $ runTB External tb

  let
    vpiClock    = low
    vpiSimSteps = tbSimSteps
    vpiInit     = True

  let
    ?state = VPIState{..}

  -- print simulator info
  putStrLn "[ Simulator Info ]"
  Info{..} <- liftIO $ runSimAction $ receiveSimulatorInfo
  putStrLn infoProduct
  putStrLn infoVersion
  putStrLn ""

  -- print top modules
  putStrLn "[ Top Modules ]"
  tops' <- liftIO $ runSimAction $ topModules
  topNames <- liftIO $ runSimAction $ mapM (receiveProperty Name) tops'
  mapM_ putStrLn topNames
  putStrLn ""

  -- iverilog runs into problems if iterated objects are used as a
  -- long-term reference. Hence, they only should be used for
  -- analyzing the architecture upfront. For long-term references to
  -- be reusable during simulation, the objects should be queried via
  -- their architectural name reference instead.
  topM <- liftIO $ runSimAction $ M.fromList
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
        { testbench   = testbench
            { tbSignals      = map snd vpiSignals
            , tbSignalLookup = A.array (A.bounds tbSignalLookup) vpiSignals
            }
        }

  putStrLn "[ Simulation start ]"
  putStrLn ""

  nextCB ReadWriteSynch 0 assignInputs

  return r

assignInputs :: MonadIO m => (?state :: VPIState) => m ()
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

  if vpiClock == high || vpiInit
  then nextCB ReadWriteSynch 1 assignInputs
  else nextCB ReadOnlySynch  1 readOutputs

 where
  VPIState{..} = ?state
  Testbench{..} = testbench

  assignModuleInputs ::
    MonadIO m =>
    Maybe ModuleInterface ->
    ID () ->
    m ()
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

  sendV :: (BitPack a, MonadIO m) => Port -> a -> m ()
  sendV port v =
    liftIO $ runSimAction $
      sendValue port (BitVectorVal SNat $ pack v)
        $ InertialDelay $ SimTime 0

readOutputs :: (?state :: VPIState, MonadIO m, Verify m) => m ()
readOutputs = do
--  SimTime time <- receiveTime Sim (Nothing @Object)
--  putStrLn $ "readOutputs " <> show time

  forM_ tbDomains $ \(d, map (tbSignalLookup !) -> xs) ->
    (`onAllDomainTypes` d) $ \(TBDomain{..} :: TBDomain 'FINAL dom) -> do
      -- receive the outputs
      forM_ xs $ onAllSignalTypes $ \case
        SimSignal{..} -> case signalPlug of
          Nothing -> error "Cannot read from module"
          Just ModuleInterface{..} ->
            liftIO $ runSimAction $
              receiveValue VectorFmt (port outputPort) >>= \case
                BitVectorVal SNat v -> case signalUpdate of
                  Just upd -> liftIO $ upd $ unpack $ resize v
                  Nothing  -> error "No signal update"
                _ -> error "Unexpected return format"
        _ -> return ()
      -- print the watched signals
      i <- liftIO $ readIORef simStepRef
      when (i > 0) $ forM_ xs $ onAllSignalTypes $ \s -> do
        when validate $ case s of
          SimSignal{..} -> verify signalVerify
          _ -> return ()
        v <- liftIO $ signalCurVal s
        when (not quietRun) $ case signalPrint s of
          Nothing    -> return ()
          Just toStr -> putStrLn . B.pack . (<> toStr v) $ case s of
            IOInput{}   -> "I "
            SimSignal{} -> "O "
            TBSignal{}  -> "S "
      -- proceed time for all instances not running trough Clash-FFI
      liftIO $ modifyIORef simStepRef (+ 1)

  if vpiSimSteps > 0 then do
    let ?state = ?state { vpiSimSteps = vpiSimSteps - 1 }
    nextCB ReadWriteSynch 1 assignInputs
  else do
    putStrLn ""
    putStrLn "[ Simulation done ]"

    liftIO $ void $ try @SomeException $ runSimAction
      $ controlSimulator $ Finish NoDiagnostics

 where
  VPIState{..} = ?state
  SimSettings{..} = simSettings
  Testbench{..} = testbench

matchModule ::
  (?state :: VPIState, KnownDomain dom, BitPack a, MonadIO m) =>
  Module -> TBSignal 'FINAL dom a -> m (TBSignal 'FINAL dom a)
matchModule module_ = \case
  tbs@SimSignal{..} -> do
    ports <- liftIO $ runSimAction $ modulePorts module_
    dirs  <- liftIO $ runSimAction $ mapM direction ports

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
      [p] -> liftIO $ runSimAction $ do
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
  (?state :: VPIState, MonadIO m) =>
  Module -> (ID (), Maybe Port) -> m (ID (), PortInterface)
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

--putStr :: String -> SimCont a ()
--putStr = simPutStr . B.pack

putStrLn :: (?state :: VPIState, MonadIO m) => B.ByteString -> m ()
putStrLn = liftIO . when (not quietRun) . runSimAction . simPutStrLn
 where
  VPIState{..} = ?state
  SimSettings{..} = simSettings

--print :: Show a => a -> SimCont b ()
--print = simPutStrLn . B.pack . show

nextCB ::
  MonadIO m =>
  (Maybe Object -> Time -> CallbackReason) ->
  Int64 ->
  IO () ->
  m ()
nextCB reason time action =
  void $ liftIO $ runSimAction $ registerCallback
    CallbackInfo
      { cbReason  = reason Nothing (SimTime time)
      , cbRoutine = const (action >> return 0)
      , cbIndex   = 0
      , cbData    = B.empty
      }

tbPropertyFFI :: TB () -> Hedgehog.Property
tbPropertyFFI = Hedgehog.property . simulate
  SimSettings
    { quietRun = True
    , validate = True
    }
