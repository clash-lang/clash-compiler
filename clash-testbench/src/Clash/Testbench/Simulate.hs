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
  , AutoTB(..)
  , simulate
  , simulateFFI
  ) where

import Prelude hiding (putStrLn)
import qualified Prelude (putStrLn)

import Control.Monad.IO.Class
import Control.Monad.State.Lazy hiding (lift)
import Data.Proxy

import Data.Coerce (Coercible)
import Data.IORef
import Data.Bits (complement)
import Data.Maybe (catMaybes)
import Data.Typeable (Typeable)
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (free)
import Control.Exception (SomeException, try)
import Data.Int (Int64)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

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
import Clash.Testbench.Internal.Auto

-- | @simulate n testbench@ simulates the @testbench@, created in the
-- 'TB' context, for @n@ simulation steps.
--
-- The simulation is run on the native Clash implementation, as given
-- by the Clash signals and signal functions lifted into 'TB'.
simulate :: Int -> TB a -> IO a
simulate steps testbench = do
  (r, Testbench{..}) <- runTB Internal testbench
  replicateM_ (steps + 1) $ do
    forM_ tbSignals $ onAllSignalTypes $ \s -> do
      v <- signalCurVal s
      i <- readIORef tbSimStepRef
      when (i > 0) $ case signalPrint s of
        Nothing    -> return ()
        Just toStr -> Prelude.putStrLn . (<> toStr v) $ case s of
          IOInput{}   -> "I "
          Generator{} -> "I "
          TBSignal{}  -> "O "
    modifyIORef tbSimStepRef (+ 1)
  return r

data VPIState =
  VPIState
    { vpiSignal   :: ID 'FINAL () -> SomeSignal 'FINAL
    , vpiSignals  :: [SomeSignal 'FINAL]
    , vpiStepRef  :: IORef Int
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
-- simulator. See Clash-FFI for more details.
simulateFFI :: Int -> TB a -> IO a
simulateFFI steps testbench = do
  (r, Testbench{..}) <- runTB External testbench

  let ?signalFromID = tbLookupID

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
    tops <- mapM findTopModule topNames

    -- match top modules with the signals --
    vpiSignals <-
        fmap ((<>) (filter (not . isTBSignal) tbSignals) . catMaybes)
      $ mapM matchModule
      $ M.toAscList
      $ M.unionWith (\(x,_) (_,y) -> (x,y))
          ( M.fromList
          $ map (\s -> (signalName `onAllSignalTypes` s, (Just s, Nothing)))
          $ filter isTBSignal tbSignals
          )
          ( M.fromList
          $ zip (map B.unpack topNames)
          $ map (\t -> (Nothing, Just t)) tops
          )

    let
      ?state =
        VPIState
          { vpiStepRef  = tbSimStepRef
          , vpiClock    = low
          , vpiSimSteps = steps
          , vpiSignal   = createIDMap vpiSignals
          , vpiInit     = True
          , ..
          }

    putStrLn "[ Simulation start ]"
    putStrLn ""

    nextCB ReadWriteSynch 0 assignInputs

  return r

 where
  createIDMap a b =
    let f = flip M.lookup $ M.fromAscList $ map (\x -> (SomeID (signalId `onAllSignalTypes` x), x)) a in case f b of
      Just x -> x
      Nothing -> error $ show b

  isTBSignal = \case
    SomeSignal TBSignal{}            -> True
    _                                -> False

assignInputs :: (?state :: VPIState) => SimAction ()
assignInputs = do
--  SimTime time <- receiveTime Sim (Nothing @Object)
--  putStrLn $ "assignInputs " <> show (time, vpiClock, vpiInit)

  forM_ vpiSignals $ onAllSignalTypes $ \case
    TBSignal{..} -> mapM_ (assignModuleInputs vpiInstance) signalDeps
    _            -> return ()


  let ?state = ?state { vpiClock = complement vpiClock
                      , vpiInit = False
                      }

  if vpiClock == low || vpiInit
  then nextCB ReadWriteSynch 1 assignInputs
  else nextCB ReadOnlySynch  1 readOutputs

 where
  VPIState{..} = ?state

  assignModuleInputs :: Typeable b => Maybe VPIInstance -> ID 'FINAL () -> SimCont b ()
  assignModuleInputs = \case
    Nothing              -> const $ return ()
    Just VPIInstance{..} -> \sid@(SomeID x) ->
      let VPIPort{..} = vpiInputPort sid
      in case x of
         ClockID  _TODO -> sendV port vpiClock
         ResetID  _TODO -> sendV port $ boolToBit vpiInit
         EnableID _TODO -> sendV port high
         _
           | vpiClock == high -> return ()
           | otherwise        ->
               (`onAllSignalTypes` vpiSignal sid) $ \s ->
                 liftIO (signalCurVal s) >>= \v -> do
                   sendV port v

  sendV :: (BitPack a, Typeable b) => Port -> a -> SimCont b ()
  sendV port v = do
    sendValue port (BitVectorVal SNat $ pack v) $ InertialDelay $ SimTime 0

readOutputs :: (?state :: VPIState) => SimAction ()
readOutputs = do
--  SimTime time <- receiveTime Sim (Nothing @Object)
--  putStrLn $ "readOutputs " <> show time

  forM_ vpiSignals $ onAllSignalTypes $ \case
    TBSignal{..} -> case vpiInstance of
      Nothing -> error "Cannot read from module"
      Just VPIInstance{..} ->
        receiveValue VectorFmt (port vpiOutputPort) >>= \case
          BitVectorVal SNat v -> case signalUpdate of
            Just upd -> liftIO $ upd $ unpack $ resize v
            Nothing  -> error "No signal update"
          _ -> error "Unexpected return format"
    _ -> return ()

  -- print the watched signals
  i <- liftIO $ readIORef vpiStepRef
  when (i > 0) $ forM_ vpiSignals $ onAllSignalTypes $ \s -> do
    v <- liftIO $ signalCurVal s
    case signalPrint s of
      Nothing    -> return ()
      Just toStr -> putStrLn . (<> toStr v) $ case s of
        IOInput{}   -> "I "
        Generator{} -> "I "
        TBSignal{}  -> "O "

  -- proceed time for all instances not running trough Clash-FFI
  liftIO $ modifyIORef vpiStepRef (+ 1)

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

matchModule ::
  (?signalFromID :: ID 'FINAL () -> SomeSignal 'FINAL, Typeable b) =>
  (String, (Maybe (SomeSignal 'FINAL), Maybe Module)) ->
  SimCont b (Maybe (SomeSignal 'FINAL))
matchModule = \case
  (_, (Just s, Just m)) -> case s of
    SomeSignal s' -> Just . SomeSignal <$> vpiInst m s'
  (name, (_, Nothing)) ->
    error $ "No module matches \"" <> name <> "\""
  (name, (Nothing, _)) -> do
    putStrLn $ "Module not required: \"" <> name <> "\" (ignoring)"
    return Nothing

vpiInst ::
  (?signalFromID :: ID 'FINAL () -> SomeSignal 'FINAL, KnownDomain dom, BitPack a, Typeable b) =>
  Module -> TBSignal 'FINAL dom a -> SimCont b (TBSignal 'FINAL dom a)
vpiInst vpiModule = \case
  tbs@TBSignal{..} -> do
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

    return tbs { vpiInstance = Just VPIInstance{..} }
  _ -> error "Unfiltered TBS"

 where
  isInput = \case
    Input -> True
    _     -> False

  isOutput = \case
    Output -> True
    _      -> False

matchPort ::
  (?signalFromID :: ID 'FINAL () -> SomeSignal 'FINAL, Typeable b) =>
  Module -> (ID 'FINAL (), Maybe Port) -> SimCont b (ID 'FINAL (), VPIPort)
matchPort m = \case
  (_, Nothing)     -> error "Not enough ports"
  (sid, Just p) -> do
    portNameBS    <- receiveProperty Name p
    portSize      <- fromEnum <$> getProperty Size p
    portIndex     <- fromEnum <$> getProperty PortIndex p
    portDirection <- direction p

    let portName = B.unpack portNameBS

    if
      | isSignalID sid -> (`onAllSignalTypes` ?signalFromID sid) $ \s ->
                            checkPort (toInteger portSize) s portDirection
      | isClockID sid  && portSize /= 1 -> error $ "Not a clock port: "  <> portName
      | isResetID sid  && portSize /= 1 -> error $ "Not a reset port: "  <> portName
      | isEnableID sid && portSize /= 1 -> error $ "Not a enable port: " <> portName
      | otherwise -> return ()

    -- Get a long-term reference via direct name access. Iterator
    -- references may not be persitent.
    port <- getByName (Just m) portNameBS

    return (sid, VPIPort{..})

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
