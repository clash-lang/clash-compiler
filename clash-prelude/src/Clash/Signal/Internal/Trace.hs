{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Signal.Internal.Trace (
  -- * Helper types
  Period, Width, Value,
  TraceMap,

  -- * Traces
  Trace(..),
  toTrace,fromTrace,

  -- * Tracing
  trace,
  traceVec,
  traceReset,traceEnable,

  -- * Simulation
  Traceable,
  Simulation(..),
  Times(..),
  times,
  Config(..),
  simulate, simulateWith,
  emptySim,

  ClockWave,
  clockWave,

  setStartStop,setClockStart,
  addTrace,
  fetchTrace, fetch,

  -- * Internal
  GlobalData(..),
  globalDataRef,

  simulate0,
  forceEvaluateSignal,

  registerTrace,
  registerFound,
  registerMsg,
  registerErr,

  trace0,
  traceVec0,

  -- * Helpers
  simulationTimeRangeToCycles,
  clockTrace,clockTrace0,
  domainName, replaceDollar,
) where

import Prelude hiding (undefined)

import Control.Exception (evaluate)
import Control.Monad (foldM, when)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Default (Default(..))
import Data.Either.Extra (maybeToEither)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Typeable (Typeable, Proxy(..))
import GHC.Natural (Natural)
import GHC.TypeLits (KnownNat, natVal, symbolVal)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

import Clash.Class.BitPack (BitPack(..), BitSize)
import Clash.XException (NFDataX(rnfX), undefined)
import Clash.Signal
  (KnownDomain, SDomainConfiguration(..), knownDomain,
  ActiveEdge(..),
  Signal, sample, sampleN_lazy, fromList,
  Reset, Enable, fromEnable, unsafeFromReset,
  unbundle)
import Clash.Signal.Internal (VDomainConfiguration(..), vDomain)
import Clash.Sized.Internal.BitVector (Bit, BitVector(BV))
import Clash.Sized.Vector (Vec, toList)
import Clash.Time (Time(..), timeInFS, AtOrForTime(..), absTime, clockPeriodTime)

import Clash.Signal.Internal.Trace.CompatibleTypes (DataType, compatible, simpleTypeRep)

type Period = Time
type Width = Int
type Value = (Natural, Natural) -- (mask, value)


data Trace = Trace
  { dataType :: DataType
  , period   :: Period
  , width    :: Width
  , values   :: [Value]
  }

type TraceMap = Map String Trace

type Traceable a = (NFDataX a, BitPack a, Typeable a)

-- | (/name/, /period/): Name of the clock wave in the VCD output, and period of
-- the clock wave. The clock will have 50% duty cycle.
type ClockWave = (String, Time, ActiveEdge)

-- | Object containing the simulation configuration
-- and the traces captured during simulation.
data Simulation = Simulation
  { simTimes :: Times
  , simConfig :: Config
  , simTraces :: TraceMap
  , simTimestamp :: UTCTime
  }

-- | Object containing all data that must be globally accessible.
data GlobalData = GlobalData
  { globTraces :: TraceMap
  , globFound :: [String]
  , globMessages :: [String]
  , globError :: Maybe String
  , globFirstRun :: Bool
  }

data Times
  = Times
  { start :: Time
  -- ^ Start the simulation/dump at this time
  , stop :: AtOrForTime
  -- ^ Stop the dump at this time, or after this time after start.
  , clockStart :: Time
  -- ^ Start the clocks at this time
  }
 deriving (Show)

-- | Create a time window starting at 0 and ending at the specified time,
-- with clocks starting after 100ns.
times :: AtOrForTime -> Times
times stop =  Times{start = 0, stop, clockStart = TimeNS 100}

data Config
  = Config
  { shiftToZero :: Bool
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
      { shiftToZero = True
      , statusMsgs = True
      , warnZeroWidth = True
      }

----------------------------------------------
--                SIMULATION                --
----------------------------------------------

-- | Create an empty simulation from a configuration.
emptySim :: Config -> IO Simulation
emptySim config = do
  simTimestamp <- getCurrentTime
  pure Simulation
    { simConfig = config
    , simTraces = Map.empty
    , simTimestamp
    }

globalDataRef :: IORef GlobalData
globalDataRef = unsafePerformIO (newIORef gd)
 where
  gd = GlobalData
    { globTraces = Map.empty
    , globFound = []
    , globMessages = []
    , globError = Nothing
    , globFirstRun = True
    }
{-# OPAQUE globalDataRef #-}

-- | Simulate a design by forcefully evaluating an output signal.
simulate ::
  forall dom a.
  KnownDomain dom =>
  NFDataX a =>
  -- | Duration after clock start
  Time ->
  -- | Clock waves to render
  [ClockWave] ->
  -- | Names of the traces you definitely want to be in the output
  [String] ->
  -- | (One of) the outputs of the circuit containing the traces
  Signal dom a ->
  IO (Either String Simulation)
simulate d = simulateWith def (times $ For $ d + TimeNS 100)

-- | Simulate a design by forcefully evaluating an output signal.
-- Like 'simulate', but with more options.
simulateWith ::
  forall dom a.
  KnownDomain dom =>
  NFDataX a =>
  -- | Config options.
  Config ->
  -- | Time window to simulate for.
  Times ->
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
  Times ->
  [ClockWave] ->
  [String] ->
  Signal dom a ->
  IO (Either String Simulation)
simulate0 ref simConfig@Config{..} simTimes@Times{..} clockWaves signals sig = do
  simTimestamp <- getCurrentTime

  firstRun <- atomicModifyIORef' ref
    (\glob@GlobalData{globFirstRun} -> (glob{globFirstRun=False},globFirstRun))

  when (not firstRun && statusMsgs) $ do
    hPutStrLn stderr "Warning: `simulate` can only be safely called once"

  -- evaluate signal
  let cycRange = simulationTimeRangeToCycles simTimes (clockPeriodTime @dom)
  evalResult <- forceEvaluateSignal ref sig signals cycRange statusMsgs

  -- create Simulation
  GlobalData{globTraces} <- readIORef ref
  let simTraces = Map.union
        (Map.fromList $ L.map (\(n,p,a) -> (n,clockTrace0 p a)) clockWaves)
        globTraces
  pure $ Simulation{simTimes, simConfig, simTraces, simTimestamp} <$ evalResult

simulationTimeRangeToCycles :: Times -> Time -> (Int,Int)
simulationTimeRangeToCycles Times{start,stop,clockStart} period =
  (fromInteger cStart, fromInteger cStop)
 where
    cStart = max 0
      $ timeInFS (start - clockStart)
      `div` timeInFS period
      + 1 -- t=0 for sample 1
    cStop = max 0
      $ ((timeInFS $ absTime start stop - clockStart) - 1) -- -1 to not include a cycle
      `div` timeInFS period                                -- that starts at <stop>
      + 2 -- add 1 for exclusive range



-- | The magic that makes the tracing simulation work.
-- A signal is evaluated within some range, which may be cut short if all wanted
-- signals have been found.
forceEvaluateSignal ::
  forall dom a.
  KnownDomain dom =>
  NFDataX a =>
  IORef GlobalData ->
  Signal dom a ->
  [String] ->
  (Int,Int) ->
  Bool ->
  IO (Either String ())
forceEvaluateSignal ref sig waitFor (start,stop) statusMsgs =
  case (waitFor, statusMsgs) of
    ([],False) -> ioMapM_         $                map evalAll           values0
    ([],True ) -> ioMapM_         $ progressMap  $ map evalAllWithStatus values0
    (_ ,False) -> waitSig waitFor $                map evalSig           values0
    (_ ,True ) -> waitSig waitFor $ progressMap0 $ map evalSig           values0
 where
  values :: [a]
  values = drop (fromIntegral start) $ sampleN_lazy (fromIntegral stop) sig

  -- Add a single undefined, so that the evaluation function is called at least
  -- once, thus printing any messages/errors that were registered before evaluation
  -- started. Also causes both 0% and 100% to be printed.
  values0 = undefined:values

  -- Execute IO actions, stopping on a Left
  ioMapM_ :: [IO (Either String ())] -> IO (Either String ())
  ioMapM_ = runExceptT . mapM_ (ExceptT)

  -- Fold IO actions with a list of signals to look for, stopping when all are found
  waitSig :: [String] -> [[String] -> IO (Either String [String])] -> IO (Either String ())
  waitSig [] _ = do
    when statusMsgs $ hPutStrLn stderr "All signals found"
    pure $ Right ()
  waitSig toFind [] = do
    pure $ Left $ "Signal(s) not found: " <> L.intercalate ", " toFind
  waitSig toFind (f:fs) = do
    r <- f toFind
    case r of
      Right []      -> pure $ Right ()
      Right toFind0 -> waitSig toFind0 fs
      Left e        -> pure $ Left e

  maybeLeft :: q -> Maybe p -> Either p q
  maybeLeft _ (Just e) = Left e
  maybeLeft x _ = Right x

  -- No signal list, no status messages
  evalAll :: a -> IO (Either String ())
  evalAll x = do
    evaluate $ rnfX x
    GlobalData{globError} <- readIORef ref
    pure $ maybeLeft () globError

  -- Status messages, but no signal list
  evalAllWithStatus :: a -> IO (Either String ())
  evalAllWithStatus x = do
    evaluate $ rnfX x
    (_,newMsgs,err) <- atomicModifyIORef' ref extractFoundAndMsgs
    mapM_ (hPutStrLn stderr) newMsgs
    pure $ maybeLeft () err

  -- Signal list, potentially with messages
  evalSig :: a -> [String] -> IO (Either String [String])
  evalSig x signals = do
    evaluate $ rnfX x
    (found,newMsgs,err) <- atomicModifyIORef' ref extractFoundAndMsgs
    when statusMsgs $ mapM_ (hPutStrLn stderr) newMsgs
    let signals0 = signals L.\\ found
    pure $ maybeLeft signals0 err

  -- Add progress printing to list of evaluation actions
  progressMap :: [IO p] -> [IO p]
  progressMap = progressMap1 0 percentages
   where
    progressMap1 _ _ [] = []
    progressMap1 _ [] s = s
    progressMap1 i ps0@((k,p):ps) (s:ss) =
      if k==i
      then progressMap1 i ps (( do
              r <- s
              hPutStrLn stderr $ show p <> "%"
              pure r
           ) : ss)
      else s : progressMap1 (i+1) ps0 ss

  progressMap0 :: [p -> IO q] -> [p -> IO q]
  progressMap0 = progressMap1 0 percentages
   where
    progressMap1 _ _ [] = []
    progressMap1 _ [] s = s
    progressMap1 i ps0@((k,p):ps) (s:ss) =
      if k==i
      then progressMap1 i ps $ (\x -> ( do
              r <- s x
              hPutStrLn stderr $ show p <> "%"
              pure r
           )) : ss
      else s : progressMap1 (i+1) ps0 ss

  -- list of (sample index, percentage to report)
  percentages = [(p*(stop-start) `div` 100,p) | p <- [0,5..100]]

  -- Reset the found signals and messages fields of the global data,
  -- returning their previous values as well as any potential error message.
  -- Meant for @atomicModifyIORef'@.
  extractFoundAndMsgs :: GlobalData -> (GlobalData, ([String], [String], Maybe String))
  extractFoundAndMsgs glob@GlobalData{..} =
    (glob{globFound = [], globMessages = []}, (globFound,globMessages,globError))



-- | Change when the 'Simulation' starts and stops.
setStartStop ::
  Time ->
  AtOrForTime ->
  Simulation ->
  Simulation
setStartStop start stop sim@Simulation{simTimes} = sim{simTimes=simTimes{start,stop}}

-- | Change when the clocks start in a 'Simulation'.
setClockStart ::
  Time ->
  Simulation ->
  Simulation
setClockStart clockStart sim@Simulation{simTimes} = sim{simTimes=simTimes{clockStart}}

-- | Change the start, stop and clock start times in a 'Simulation'.
setTimes ::
  Times ->
  Simulation ->
  Simulation
setTimes simTimes sim = sim{simTimes}

-- | Create a 'ClockWave' for a given domain.
clockWave ::
  forall dom.
  KnownDomain dom =>
  String ->
  ClockWave
clockWave name = (name, clockPeriodTime @dom, vActiveEdge)
 where
  VDomainConfiguration{vActiveEdge} = vDomain (knownDomain @dom)

----------------------------------------------
--                 TRACING                  --
----------------------------------------------

-- | Put a trace in the global data.
registerTrace :: String -> Trace -> GlobalData -> Either String GlobalData
registerTrace name trc glob@GlobalData{globTraces} =
  if Map.member name globTraces
  then Left ("Trace " <> name <> " already exists")
  else Right glob{globTraces = Map.insert name trc globTraces}

-- | Mark a signal/signals as found.
registerFound :: [String] -> GlobalData -> GlobalData
registerFound new glob@GlobalData{globFound} = glob{globFound = new <> globFound}

-- | Store a message.
registerMsg :: String -> GlobalData -> GlobalData
registerMsg msg glob@GlobalData{globMessages} = glob{globMessages = msg : globMessages}

-- | Store an error message.
registerErr :: String -> GlobalData -> GlobalData
registerErr msg glob = glob{globError = Just msg}

-- | Trace a 'Signal'.
-- This converts the signal to a trace, and stores it in global storage.
trace ::
  forall dom a.
  KnownDomain dom =>
  Traceable a =>
  String ->
  Signal dom a ->
  Signal dom a
trace name sig =
  unsafePerformIO
    $ atomicModifyIORef' globalDataRef (\g -> (trace0 @dom name (toTrace sig) g, sig))
{-# OPAQUE trace #-}

trace0 ::
  forall dom.
  KnownDomain dom =>
  String ->
  Trace ->
  GlobalData ->
  GlobalData
trace0 name trc glob =
  right
    $ registerTrace fullName trc
    $ registerFound found
    $ registerMsg ("Found signal " <> fullName)
    $ glob
 where
  fullName = replaceDollar (domainName @dom) name
  found = if fullName == name then [name] else [name, fullName]

  right (Right x) = x
  right (Left e) = registerErr e glob
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
traceVec name sig =
  unsafePerformIO
    $ atomicModifyIORef' globalDataRef (\g -> (traceVec0 name sig g,sig))
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
traceVec0 name sig glob =
  registerTraces
    $ registerFound found
    $ registerMsg ("Found vector signal " <> fullName)
    $ glob
 where
  traces = toList $ toTrace <$> unbundle sig
  fullName = replaceDollar (domainName @dom) name
  names = map (\i -> name <> ('.' : show i)) [0..length traces-1]
  fullNames = map (\i -> fullName <> ('.' : show i)) [0..length traces-1]
  found = if fullName == name then name:names else [name, fullName]<>names<>fullNames
  registerTraces g = right $ foldM (flip ($)) g $ zipWith registerTrace fullNames traces

  right (Right x) = x
  right (Left e) = registerErr e glob
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
-- traceClock ::
--   forall dom.
--   KnownDomain dom =>
--   String ->
--   Clock dom ->
--   Clock dom
-- traceClock name clk =
--   unsafePerformIO (atomicModifyIORef' globalDataRef ((,clk) . trace0 @dom name trc))
--  where
--   trc = clockTrace (clockPeriodTime @dom) (activeEdge @dom)
-- {-# OPAQUE traceClock #-}

-- | Create a clock trace from a domain.
clockTrace :: forall dom. KnownDomain dom => Trace
clockTrace = clockTrace0 (clockPeriodTime @dom) vActiveEdge
 where
  VDomainConfiguration{vActiveEdge} = vDomain (knownDomain @dom)

-- | Create a trace from a clock period and active edge. The period *must* be an even number of fs.
clockTrace0 :: Time -> ActiveEdge -> Trace
clockTrace0 (TimeFS t) edge
  | (halfPeriod,0) <- t `divMod` 2 = Trace
    { dataType = simpleTypeRep @Bit
    , period = TimeFS halfPeriod
    , width = 1
    , values = L.cycle $ L.map (unsafeToTup . pack) bits }
  | otherwise = error "Cannot create clock trace for odd periods (in fs)"
 where
  unsafeToTup (BV mask value) = (mask, value)
  bits = case edge of
    Rising  -> [0,1]
    Falling -> [1,0] :: [Bit]




----------------------------------------------
--                 TRACES                   --
----------------------------------------------

-- | Create a 'Trace' from a 'Signal'.
toTrace ::
  forall dom a.
  KnownDomain dom =>
  Traceable a =>
  Signal dom a ->
  Trace
toTrace sig = Trace
  (simpleTypeRep @a)
  (clockPeriodTime @dom)
  (fromInteger $ natVal $ Proxy @(BitSize a))
  (sample $ unsafeToTup . pack <$> sig)
 where
  unsafeToTup (BV mask value) = (mask, value)

-- | Create a 'Signal' from a 'Trace'.
-- This only checks whether the type is correct.
fromTrace ::
  forall dom a.
  Traceable a =>
  Trace ->
  Either String (Signal dom a)
fromTrace Trace{dataType, values}
  | compatible dataType (simpleTypeRep @a) =
      Right $ fromList $ L.map (unpack . uncurry BV) values
  | otherwise =
      Left "Trace did not match target data type"

-- | Add a 'Trace' to a 'Simulation'
addTrace ::
  String ->
  Trace ->
  Simulation ->
  Either String Simulation
addTrace name trc sim@Simulation{simTraces} =
  if Map.member name simTraces
  then Left ("Trace " <> name <> " already exists")
  else Right sim{simTraces = Map.insert name trc simTraces}

-- | Retrieve a captured 'Trace'.
fetchTrace ::
  String ->
  Simulation ->
  Either String Trace
fetchTrace name Simulation{simTraces} =
  maybeToEither ("Trace " <> name <> " not found") $ Map.lookup name simTraces

-- | Retrieve a 'Signal' from the captured traces.
fetch ::
  forall dom a.
  KnownDomain dom =>
  Traceable a =>
  String ->
  Simulation ->
  Either String (Signal dom a)
fetch name sim = fetchTrace name sim >>= fromTrace


----------------------------------------------
--                   MISC                   --
----------------------------------------------

-- | Get the name of a domain.
domainName ::
  forall dom.
  KnownDomain dom =>
  String
domainName =
  case knownDomain @dom of
    SDomainConfiguration{sName} -> symbolVal sName

-- | Internal: function to replace $ in signal names with the domain name
replaceDollar :: String -> String -> String
replaceDollar _ "" = ""
replaceDollar n ('$':rest) = n <> replaceDollar n rest
replaceDollar n (c:rest) = c : replaceDollar n rest
