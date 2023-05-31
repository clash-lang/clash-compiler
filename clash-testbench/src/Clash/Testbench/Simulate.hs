{-|
Copyright:    (C) 2023 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>

All it needs for building and running test benches that are created
from Clash circuitry.
-}
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

-- | @simulate n testbench@ simulates the @testbench@, created in the
-- 'Clash.Testbench.Simulate.TB' context, for @n@ simulation steps.
--
-- The simulation is run on the native Clash implementation, as given
-- by the Clash signals and signal functions lifted into 'TB'.
simulate :: Int -> TB a -> IO a
simulate steps testbench = do
  (r, Testbench{..}) <- runTB Internal testbench
  replicateM_ (steps + 1) $ do
    forM_ tbDomains $ \(d, map (tbSignalLookup !) -> xs) ->
      (`onAllDomainTypes` d) $ \(TBDomain{..} :: TBDomain 'FINAL dom) -> do
--        i <- readIORef simStepRef
        forM_ xs $ onAllSignalTypes $ \s -> do
          v <- signalCurVal s
          case signalPrint s of
            Nothing    -> return ()
            Just toStr -> Prelude.putStrLn . (<> toStr v) $ case s of
              IOInput{}   -> "I "
              SimSignal{} -> "O "
              TBSignal{}  -> "S "
        modifyIORef simStepRef (+ 1)

--    forM_ tbSignals $ onAllSignalTypes $ \case
--      SimSignal{..} -> do
--        signalVerify >>= \case
--          Nothing  -> Prelude.putStrLn "✓"
--          Just msg -> error $ "✗ " <> msg
--      _ -> return ()

  return r

tbProperty :: TB () -> Hedgehog.Property
tbProperty testbench = Hedgehog.property $ do
  (_, Testbench{..}) <- liftIO $ runTB Internal testbench
  replicateM_ tbSimSteps $ do
    forM_ tbDomains $ \(d, map (tbSignalLookup !) -> xs) ->
      (`onAllDomainTypes` d) $ \(TBDomain{..} :: TBDomain 'FINAL dom) -> do
        forM_ xs $ onAllSignalTypes $ \s -> do
--          void $ liftIO $ signalCurVal s
          case s of
            SimSignal{..} -> signalVerify
            _             -> return ()

        liftIO $ modifyIORef simStepRef (+ 1)

data VPIState =
  VPIState
    { testbench   :: Testbench
      -- multiple clocks are not supported yet, currently all clocks
      -- are synchronously executed.
    , vpiClock    :: Bit
    , vpiSimSteps :: Int
    , vpiInit     :: Bool
    }

-- | @simulate n testbench@ simulates the @testbench@, created in the
-- 'TB' context, for @n@ simulation steps with an external simulator
-- bound via Clash-FFI.
--
-- Note that this function is not executable in a standard Haskell
-- environment, but must to be bound to some @ffiMain@ foreign call
-- that is shipped via a shared library and executed by an external
-- simulator. See Clash-FFI for more details on this.
simulateFFI :: Int -> TB a -> IO a
simulateFFI steps tb = do
  (r, testbench@Testbench{..}) <- runTB External tb

  let ?testbench = testbench

  runSimAction $ do
    -- print simulator info
    putStrLn "[ Simulator Info ]"
    Info{..} <- receiveSimulatorInfo
    simPutStrLn infoProduct
    simPutStrLn infoVersion
    putStrLn ""

    -- print top modules
    putStrLn "[ Top Modules ]"
    tops' <- topModules
    topNames <- mapM (receiveProperty Name) tops'
    mapM_ simPutStrLn topNames
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
        VPIState
          { vpiClock    = low
          , vpiSimSteps = steps
          , vpiInit     = True
          , testbench   = testbench
              { tbSignals      = map snd vpiSignals
              , tbSignalLookup = A.array (A.bounds tbSignalLookup) vpiSignals
              }
          , ..
          }

    putStrLn "[ Simulation start ]"
    putStrLn ""

    nextCB ReadWriteSynch 0 assignInputs

  return r

assignInputs :: (?state :: VPIState) => SimAction ()
assignInputs = do
--  SimTime time <- receiveTime Sim (Nothing @Object)
--  putStrLn $ "assignInputs " <> show (time, vpiClock, vpiInit)

  forM_ tbDomains $ \(d, map (tbSignalLookup !) -> xs) ->
    (`onAllDomainTypes` d) $ const $ do
      forM_ xs $ onAllSignalTypes $ \case
        SimSignal{..} -> mapM_ (assignModuleInputs signalVPI) signalDeps
        _             -> return ()


  let ?state = ?state { vpiClock = complement vpiClock
                      , vpiInit = False
                      }

  if vpiClock == low || vpiInit
  then nextCB ReadWriteSynch 1 assignInputs
  else nextCB ReadOnlySynch  1 readOutputs

 where
  VPIState{..} = ?state
  Testbench{..} = testbench

  assignModuleInputs :: Typeable b => Maybe VPIInstance -> ID () -> SimCont b ()
  assignModuleInputs = \case
    Nothing              -> const $ return ()
    Just VPIInstance{..} -> \sid@(SomeID x) ->
      let VPIPort{..} = vpiInputPort sid
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
  sendV port v = do
    sendValue port (BitVectorVal SNat $ pack v) $ InertialDelay $ SimTime 0

readOutputs :: (?state :: VPIState) => SimAction ()
readOutputs = do
--  SimTime time <- receiveTime Sim (Nothing @Object)
--  putStrLn $ "readOutputs " <> show time

  forM_ tbDomains $ \(d, map (tbSignalLookup !) -> xs) ->
    (`onAllDomainTypes` d) $ \(TBDomain{..} :: TBDomain 'FINAL dom) -> do
      -- receive the outputs
      forM_ xs $ onAllSignalTypes $ \case
        SimSignal{..} -> case signalVPI of
          Nothing -> error "Cannot read from module"
          Just VPIInstance{..} ->
            receiveValue VectorFmt (port vpiOutputPort) >>= \case
              BitVectorVal SNat v -> case signalUpdate of
                Just upd -> liftIO $ upd $ unpack $ resize v
                Nothing  -> error "No signal update"
              _ -> error "Unexpected return format"
        _ -> return ()
      -- print the watched signals
      i <- liftIO $ readIORef simStepRef
      when (i > 0) $ forM_ xs $ onAllSignalTypes $ \s -> do
        v <- liftIO $ signalCurVal s
        case signalPrint s of
          Nothing    -> return ()
          Just toStr -> putStrLn . (<> toStr v) $ case s of
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
  Testbench{..} = testbench

matchModule ::
  (?testbench :: Testbench, KnownDomain dom, BitPack a, Typeable b) =>
  Module -> TBSignal 'FINAL dom a -> SimCont b (TBSignal 'FINAL dom a)
matchModule vpiModule = \case
  tbs@SimSignal{..} -> do
    ports <- modulePorts vpiModule
    dirs  <- mapM direction ports

    let
      inputPorts  = map fst $ filter (isInput  . snd) $ zip ports dirs
      outputPorts = map fst $ filter (isOutput . snd) $ zip ports dirs

    vpiInputPort <-
      (M.!) . M.fromList
        <$> ( mapM (matchPort vpiModule)
            $ zip signalDeps
            $ map Just inputPorts <> repeat Nothing
            )

    vpiOutputPort <- case outputPorts of
      [p] -> do
        portNameBS    <- receiveProperty Name p
        portSize      <- fromEnum <$> getProperty Size p
        portIndex     <- fromEnum <$> getProperty PortIndex p
        portDirection <- direction p

        let portName = B.unpack portNameBS
        port <- getByName (Just vpiModule) portNameBS

        checkPort (toInteger portSize) tbs portDirection

        return $ VPIPort{..}
      _   -> error "TODO: later / "

    return tbs { signalVPI = Just VPIInstance{..} }
  _ -> error "Unfiltered TBS"

 where
  isInput = \case
    Input -> True
    _     -> False

  isOutput = \case
    Output -> True
    _      -> False

matchPort ::
  (?testbench :: Testbench, Typeable b) =>
  Module -> (ID (), Maybe Port) -> SimCont b (ID (), VPIPort)
matchPort m = \case
  (_, Nothing)     -> error "Not enough ports"
  (sid, Just p) -> do
    portNameBS    <- receiveProperty Name p
    portSize      <- fromEnum <$> getProperty Size p
    portIndex     <- fromEnum <$> getProperty PortIndex p
    portDirection <- direction p

    let portName = B.unpack portNameBS
    checkID portName portSize portDirection sid

    -- Get a long-term reference via direct name access. Iterator
    -- references may not be persistent.
    port <- getByName (Just m) portNameBS

    return (sid, VPIPort{..})
 where
  Testbench{..} = ?testbench

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

putStrLn :: String -> SimCont a ()
putStrLn = simPutStrLn . B.pack

--print :: Show a => a -> SimCont b ()
--print = simPutStrLn . B.pack . show

nextCB ::
  (Maybe Object -> Time -> CallbackReason) ->
  Int64 ->
  SimAction () ->
  SimAction ()
nextCB reason time action =
  void $ registerCallback
    CallbackInfo
      { cbReason  = reason Nothing (SimTime time)
      , cbRoutine = const (runSimAction action >> return 0)
      , cbIndex   = 0
      , cbData    = B.empty
      }
