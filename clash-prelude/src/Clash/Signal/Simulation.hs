{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

module Clash.Signal.Simulation where

import           Control.Exception     (evaluate)
import           Control.Monad         (foldM)
import           Data.Bifunctor        (second)
import           Data.ByteString.Lazy  (ByteString)
import           Data.Default          (Default(..))
import           Data.Either.Extra     (maybeToEither)
import           Data.IORef
  (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.List             as L
import qualified Data.Map              as M
import           Data.Time.Clock       (UTCTime, getCurrentTime)
import           Data.Typeable         (Typeable, Proxy(..))
import qualified Debug.Trace
import           GHC.Natural           (Natural)
import           GHC.TypeLits          (KnownNat, natVal, symbolVal)
import           System.IO             (hPutStrLn, stderr)
import           System.IO.Unsafe      (unsafePerformIO)

import           Clash.Class.BitPack   (BitPack(..), BitSize)
import           Clash.Explicit.Prelude.Safe (undefined)
import           Clash.XException      (NFDataX(rnfX), deepseqX)
import           Clash.Signal
  (KnownDomain, SDomainConfiguration(..), knownDomain,
  Signal, sample, fromList,
  Clock, Reset, Enable, fromEnable, unsafeFromReset,
  unbundle)
import           Clash.Sized.Internal.BitVector
  (BitVector(BV))
import           Clash.Sized.Vector    (Vec, toList)
import           Clash.Time            (Time(..), timeInFS, AtOrForTime(..), absTime, clockPeriodTime)

import           Clash.Signal.Simulation.DataType (DataType, compatible, typeRep)

--data DataType = DT String [DataType] deriving (Show)
-- type DataType = ByteString

type Period = Time
type Width = Int
type Value = (Natural, Natural) -- (mask, value)
type Trace = (DataType,Period,Width,[Value])
type TraceMap = M.Map String Trace

type Traceable a = (NFDataX a, BitPack a, Typeable a)

-- | (/name/, /period/): Name of the clock wave in the VCD output, and period of
-- the clock wave. The clock will have 50% duty cycle.
type ClockWave = (String, Time)

-- | Object containing the simulation configuration and the traces captured during simulation.
data Simulation = Simulation
  { simConfig :: Config
  , simTraces :: TraceMap
  , simTimestamp :: UTCTime
  }

-- | Object containing all data that must be globally accessible.
data GlobalData = GlobalData
  { globTraces :: TraceMap
  , globFound :: [String]
  , globMessages :: [String]
  , globFirstRun :: Bool
  }

data Config
  = Config
  { start :: Time
  -- ^ Start the dump at this time
  , stop :: AtOrForTime
  -- ^ Stop the dump at this time, or for this time after start.
  , clockStart :: Time
  -- ^ Start the clocks at this time
  , shiftToZero :: Bool
  -- ^ Shift `start` to time zero in the output?
  , statusMsgs :: Bool
  -- ^ Emit status messages to stdout?
  , warnZeroWidth :: Bool
  -- ^ Emit a warning to stderr when a 0-bit wide signal is traced?
  }
  deriving (Show)

instance Default Config where
  def =
    Config
      { start = TimeFS 0
      , stop = For $ TimeNS 500
      , clockStart = TimeNS 100
      , shiftToZero = True
      , statusMsgs = True
      , warnZeroWidth = True
      }

instance Default GlobalData where
  def =
    GlobalData
      { globTraces = M.empty
      , globFound = []
      , globMessages = []
      , globFirstRun = True
      }


{----------------------------------------
SIMULATION
----------------------------------------}

-- | Create an empty simulation from a configuration.
emptySim :: Config -> IO Simulation
emptySim config = do
  simTimestamp <- getCurrentTime
  return Simulation
    { simConfig = config
    , simTraces = M.empty
    , simTimestamp
    }

globalDataRef :: IORef GlobalData
globalDataRef = unsafePerformIO (newIORef def)
{-# OPAQUE globalDataRef #-}

-- | Simulate a design by forcefully evaluating an output signal.
simulate ::
  forall dom a.
  KnownDomain dom =>
  NFDataX a =>
  -- | Duration
  Time ->
  -- | Clock waves to render
  [ClockWave] ->
  -- | Names of the traces you definitely want to be in the output
  [String] ->
  -- | (One of) the outputs of the circuit containing the traces
  Signal dom a ->
  IO (Either String Simulation)
simulate d = simulateWith def{stop = For $ d + clockStart def}

-- | Simulate a design by forcefully evaluating an output signal.
-- Like 'simulate', but with more options.
simulateWith ::
  forall dom a.
  KnownDomain dom =>
  NFDataX a =>
  Config ->
  -- | Clock waves to render
  [ClockWave] ->
  -- | Names of the traces you definitely want to be in the output
  [String] ->
  -- | (One of) the outputs of the circuit containing the traces
  Signal dom a ->
  IO (Either String Simulation)
simulateWith = simulate0 globalDataRef

-- | Internal simulation function that takes the global reference as a parameter.
simulate0 ::
  forall dom a.
  KnownDomain dom =>
  NFDataX a =>
  IORef GlobalData ->
  Config ->
  [ClockWave] ->
  [String] ->
  Signal dom a ->
  IO (Either String Simulation)
simulate0 ref simConfig@Config{start,stop,clockStart,statusMsgs,warnZeroWidth} clockWaves signals sig = do
  simTimestamp <- getCurrentTime

  firstRun <- atomicModifyIORef' ref (\glob@GlobalData{globFirstRun} -> (glob{globFirstRun=False},globFirstRun))

  if not firstRun && statusMsgs then
    hPutStrLn stderr "Warning: `simulate` can only be safely called once"
  else pure ()

  -- evaluate signal
  let (cStart,cStop) = simulationTimeRangeToCycles simConfig (clockPeriodTime @dom)
  evalResult <- forceEvaluateSignal ref sig signals (cStart, cStop) statusMsgs

  -- create Simulation
  GlobalData{globTraces} <- readIORef ref
  let simTraces = M.union (M.fromList $ L.map (second clockTrace) clockWaves) globTraces
  return $ evalResult >>= const (Right Simulation{simConfig, simTraces, simTimestamp})

simulationTimeRangeToCycles :: Config -> Time -> (Int,Int)
simulationTimeRangeToCycles Config{start,stop,clockStart} period = (fromInteger cStart,fromInteger cStop)
 where
    cStart = max 0
      $ timeInFS (start - clockStart)
      `div` timeInFS period
      + 1 -- t=0 for sample 1
    cStop = max 0
      $ ((timeInFS $ absTime start stop - clockStart) - 1) -- -1 to not include a cycle that starts at <stop>
      `div` timeInFS period
      + 2 -- add 1 for exclusive range



-- | The magic that makes the tracing simulation work.
-- A signal is evaluated within some range, which may be cut short if all wanted
-- signals have been found.
forceEvaluateSignal ::
  forall dom a.
  NFDataX a =>
  IORef GlobalData ->
  Signal dom a ->
  [String] ->
  (Int,Int) ->
  Bool ->
  IO (Either String ())
forceEvaluateSignal ref sig waitFor (start,stop) statusMsgs =
  case (waitFor, statusMsgs) of
    ([],False) -> return $ Right $ deepseqX values' ()
    ([],True)  -> do
      mapM_ printFound values'
      return $ Right ()
    (_,False) -> lookFor values  waitFor
    (_,True)  -> lookFor values' waitFor
 where
  values :: [a]
  values = drop (fromIntegral start) $ take (fromIntegral stop) $ foldr (:) [] sig

  values' :: [a]
  values' =
    if statusMsgs then
      zipWith ($) (progress $ stop - start) values
    else
      values

  progress n = map go [0..n-1]
   where
    go k =
      if (20*k `div` n) /= (20*(k-1) `div` n) then
        Debug.Trace.trace $ show ((20*k) `div` n * 5) ++ "%"
      else id

  -- Reset the found and messages fields of the global data, returning their previous values.
  -- Meant for @atomicModifyIORef'@.
  extractFoundAndMsgs :: GlobalData -> (GlobalData, ([String],[String]))
  extractFoundAndMsgs glob@GlobalData{globFound,globMessages} = (glob{globFound = [], globMessages = []}, (globFound,globMessages))

  -- Evaluate a value and print messages that have popped up.
  printFound :: a -> IO ()
  printFound x = do
    evaluate $ rnfX x
    (_,newMsgs) <- atomicModifyIORef' ref extractFoundAndMsgs
    mapM_ (hPutStrLn stderr) newMsgs
    return ()

  -- Evaluate a list of values, looking for certain signals.
  -- For implementation simplicity, does not check for signals that appear after
  -- evalutating the last value.
  lookFor :: [a] -> [String] -> IO (Either String ())
  lookFor xs' ss' = lookFor' (xs' <> [Clash.Explicit.Prelude.Safe.undefined]) ss' [] True
   where
    lookFor' :: [a] -> [String] -> [String] -> Bool -> IO (Either String ())
    lookFor' _ [] _ _ = return $ Right ()
    lookFor' [] (s:_) _ _ = return $ Left ("Did not find signal " <> s)
    lookFor' xss@(x:xs) sss@(s:ss) found firstTime = do
      (newFound,newMsgs) <- atomicModifyIORef' ref extractFoundAndMsgs

      if statusMsgs then
        mapM_ (hPutStrLn stderr) newMsgs
      else
        pure ()

      let found' = newFound <> found
      if s `elem` (if firstTime then found' else newFound) then do
        lookFor' xss ss found' True
      else do
        evaluate $ rnfX x
        lookFor' xs sss found' False



-- Change when the 'Simulation' starts and stops.
setStartStop ::
  Time ->
  AtOrForTime ->
  Simulation ->
  Simulation
setStartStop start stop sim@Simulation{simConfig} = sim{simConfig=simConfig{start,stop}}

-- | Change when the clocks start in a 'Simulation'.
setClockStart ::
  Time ->
  Simulation ->
  Simulation
setClockStart clockStart sim@Simulation{simConfig} = sim{simConfig=simConfig{clockStart}}

-- | Create a 'ClockWave' for a given domain.
clockWave ::
  forall dom.
  KnownDomain dom =>
  String ->
  ClockWave
clockWave name = (name, clockPeriodTime @dom)

{----------------------------------------
TRACING
----------------------------------------}

-- | Put a trace in the global data.
registerTrace :: String -> Trace -> GlobalData -> Either String GlobalData
registerTrace name trc glob@GlobalData{globTraces} =
  if M.member name globTraces then
    Left ("Trace " <> name <> " already exists")
  else
    Right glob{globTraces = M.insert name trc globTraces}

-- | Mark a signal/signals as found.
registerFound :: [String] -> GlobalData -> GlobalData
registerFound new glob@GlobalData{globFound} = glob{globFound = new <> globFound}

-- | Store a message.
registerMsg :: String -> GlobalData -> GlobalData
registerMsg msg glob@GlobalData{globMessages} = glob{globMessages = msg : globMessages}

-- | Trace a 'Signal'.
-- This converts the signal to a trace, and stores it in global storage.
trace ::
  forall dom a.
  KnownDomain dom =>
  Traceable a =>
  String ->
  Signal dom a ->
  Signal dom a
trace name sig = unsafePerformIO $ atomicModifyIORef' globalDataRef $ \g -> (trace0 @dom name (toTrace sig) g, sig)
{-# OPAQUE trace #-}

trace0 ::
  forall dom.
  KnownDomain dom =>
  String ->
  Trace ->
  GlobalData ->
  GlobalData
trace0 name trc = right . registerTrace fullName trc . registerFound found . registerMsg ("Found signal " <> fullName)
 where
  fullName = replaceDollar (domainName @dom) name
  found = if fullName == name then [name] else [name, fullName]
  right (Right x) = x
  right (Left e) = error e
{-# OPAQUE trace0 #-}

-- | Trace all values in a vector signal individually.
traceVec ::
  forall dom a n.
  KnownDomain dom =>
  Traceable a =>
  KnownNat n =>
  String ->
  Signal dom (Vec n a) ->
  Signal dom (Vec n a)
traceVec name sig = unsafePerformIO $ atomicModifyIORef' globalDataRef $ \g -> (traceVec0 name sig g,sig)
{-# OPAQUE traceVec #-}

traceVec0 ::
  forall dom a n.
  KnownDomain dom =>
  Traceable a =>
  KnownNat n =>
  String ->
  Signal dom (Vec n a) ->
  GlobalData ->
  GlobalData
traceVec0 name sig = registerTraces . registerFound found . registerMsg ("Found vector signal " <> fullName)
 where
  traces = toList $ toTrace <$> unbundle sig
  fullName = replaceDollar (domainName @dom) name
  names = map (\i -> name <> "." <> show i) [0..length traces-1]
  fullNames = map (\i -> fullName <> "." <> show i) [0..length traces-1]
  found = if fullName == name then name:names else [name, fullName]<>names<>fullNames
  registerTraces g = right $ foldM (flip ($)) g $ zipWith registerTrace fullNames traces
  right (Right x) = x
  right (Left e) = error e
{-# OPAQUE traceVec0 #-}

-- | Like 'trace', but operates on a 'Reset'
traceReset ::
  forall dom.
  KnownDomain dom =>
  -- | Name of signal in the simulation output
  String ->
  -- | Reset to trace
  Reset dom ->
  Reset dom
traceReset name rst = trace name (unsafeFromReset rst) `seq` rst
{-# OPAQUE traceReset #-}

-- Like traceReset for enables
traceEnable ::
  forall dom.
  KnownDomain dom =>
  String ->
  Enable dom ->
  Enable dom
traceEnable name en = trace name (fromEnable en) `seq` en
{-# OPAQUE traceEnable #-}

-- Like traceReset for enables
traceClock ::
  forall dom.
  KnownDomain dom =>
  String ->
  Clock dom ->
  Clock dom
traceClock name clk = unsafePerformIO (atomicModifyIORef' globalDataRef ((,clk) . trace0 @dom name trc))
 where
  trc = clockTrace (clockPeriodTime @dom)
{-# OPAQUE traceClock #-}

-- Create a trace from a clock period. The period *must* be an even number of fs.
clockTrace :: Time -> Trace
clockTrace t
  | fs <- timeInFS t, even fs =
    ( typeRep @Bool
    , TimeFS (fs `div` 2)
    , 1
    , L.cycle $ L.map (unsafeToTup . pack) [False,True] )
  | otherwise = error "Cannot create clock trace for odd periods (in fs)"
 where
  unsafeToTup (BV mask value) = (mask, value)


{----------------------------------------
TRACES
----------------------------------------}

-- | Create a 'Trace' from a 'Signal'.
toTrace ::
  forall dom a.
  KnownDomain dom =>
  Traceable a =>
  Signal dom a ->
  Trace
toTrace sig =
  ( typeRep @a
  , clockPeriodTime @dom
  , fromInteger $ natVal (Proxy @(BitSize a))
  , sample (unsafeToTup . pack <$> sig) )
 where
  unsafeToTup (BV mask value) = (mask, value)

-- | Create a 'Signal' from a 'Trace'.
-- This only checks whether the type is correct.
fromTrace ::
  forall dom a.
  Traceable a =>
  Trace ->
  Either String (Signal dom a)
fromTrace (ty,_period,_width,values)
  | compatible ty (typeRep @a) = Right $ fromList $ L.map (unpack . uncurry BV) values
  | otherwise = Left "Trace did not match target type"

-- | Add a 'Trace' to a 'Simulation'
addTrace ::
  String ->
  Trace ->
  Simulation ->
  Either String Simulation
addTrace name trc sim@Simulation{simTraces} =
  if M.member name simTraces then
    Left ("Trace " <> name <> " already exists")
  else
    Right sim{simTraces = M.insert name trc simTraces}

-- | Retrieve a captured 'Trace'.
fetchTrace ::
  String ->
  Simulation ->
  Either String Trace
fetchTrace name Simulation{simTraces} =
  maybeToEither ("Trace " <> name <> " not found") $ M.lookup name simTraces

-- | Retrieve a 'Signal' from the captured traces.
fetch ::
  forall dom a.
  KnownDomain dom =>
  Traceable a =>
  String ->
  Simulation ->
  Either String (Signal dom a)
fetch name sim = fetchTrace name sim >>= fromTrace


{---REST--}
domainName ::
  forall dom.
  KnownDomain dom =>
  String
domainName =
  case knownDomain @dom of
    SDomainConfiguration{sName} -> symbolVal sName

replaceDollar :: String -> String -> String
replaceDollar _ "" = ""
replaceDollar n ('$':rest) = n <> replaceDollar n rest
replaceDollar n (c:rest) = c : replaceDollar n rest
