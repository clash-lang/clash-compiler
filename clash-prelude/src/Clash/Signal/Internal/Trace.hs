{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Signal.Internal.Trace (
  -- * Polymorphic tracing
  -- To allow building upon the tracing system
  TraceSystem(..),

  -- * Helper types
  Period, Width, Value,
  TraceMap,

  -- * Traces
  Trace(..),
  toTrace, fromTrace,

  -- * Tracing
  trace,
  traceVec,
  traceReset, traceEnable,

  -- * Simulation
  Traceable,
  Simulation(..),
  Times(..),
  times,
  Config(..),
  simulate, simulateWith,
  emptySim, emptySim0,

  ClockWave,
  clockWave,

  setStartStop, setClockStart, setTimes,
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
  clockTrace, clockTrace0,
  domainName, replaceDollar,
) where

import Prelude hiding (undefined)

import Control.Exception (evaluate)
import Control.Monad (foldM, when)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Default (Default(..))
import Data.Either.Extra (maybeToEither)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Kind (Constraint)
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Typeable (Typeable, Proxy(..))
import GHC.Generics (Generic)
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

----------------------------------------------
--               POLYMORPHISM               --
----------------------------------------------

class (TraceableFor t ClockVal) => TraceSystem t where
  type TraceMeta t
  type SimMeta t
  type GlobMeta t
  type VcdMeta t
  type TraceableFor t a :: Constraint
  traceMeta :: forall a. TraceableFor t a => TraceMeta t
  -- Create vcd metadata from a bunch of simulation stuff
  -- TODO: change input types

  emptySimMeta :: SimMeta t
  fromGlobMeta :: GlobMeta t -> SimMeta t

  emptyGlobMeta :: GlobMeta t

  vcdMeta :: () -> VcdMeta t
  -- Write the vcd metadata if needed. The file name is that of the VCD file.
  writeVcdMeta :: String -> VcdMeta t -> IO ()
  writeVcdMeta _ _ = pure ()
  -- When reading a signal dump file, there is no meta(?)
  -- TODO
  loadMeta :: () -> TraceMeta t

instance TraceSystem () where
  type TraceMeta () = ()
  type SimMeta () = ()
  type GlobMeta () = ()
  type VcdMeta () = ()
  type TraceableFor () a = ()
  traceMeta = ()
  vcdMeta _ = ()
  loadMeta _ = ()
  emptySimMeta = ()
  fromGlobMeta = id
  emptyGlobMeta = ()

----------------------------------------------
--                   BASE                   --
----------------------------------------------

type Period = Time
type Width = Int
type Value = (Natural, Natural) -- (mask, value)

data Trace t = Trace
  { dataType :: DataType
  , period   :: Period
  , width    :: Width
  , values   :: [Value]
  , meta     :: TraceMeta t
  }

type TraceMap t = Map String (Trace t)

type Traceable t a = (NFDataX a, BitPack a, Typeable a, TraceableFor t a)

-- | (/name/, /period/): Name of the clock wave in the VCD output, and period of
-- the clock wave. The clock will have 50% duty cycle.
type ClockWave = (String, Time, ActiveEdge)

-- | Object containing the simulation configuration
-- and the traces captured during simulation.
data Simulation t = Simulation
  { simConfig :: Config
  , simTimes :: Times
  , simTraces :: TraceMap t
  , simTimestamp :: UTCTime
  , simMeta :: SimMeta t
  }

-- | Object containing all data that must be globally accessible.
data GlobalData t = GlobalData
  { globTraces :: TraceMap t
  , globFound :: [String]
  , globMessages :: [String]
  , globError :: Maybe String
  , globFirstRun :: Bool
  , globMeta :: GlobMeta t
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
times :: Time -> Times
times stop = Times{start = 0, stop = At $ stop + clockStart, clockStart}
 where
  clockStart = TimeNS 100

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

-- | Create an empty simulation from a configuration and time settings.
emptySim :: forall t. TraceSystem t => Config -> Times -> IO (Simulation t)
emptySim simConfig simTimes = do
  now <- getCurrentTime
  pure $ emptySim0 simConfig simTimes now

emptySim0 :: forall t. TraceSystem t => Config -> Times -> UTCTime -> (Simulation t)
emptySim0 simConfig simTimes simTimestamp =
  Simulation
    { simConfig
    , simTimes
    , simTraces = Map.empty
    , simTimestamp
    , simMeta = emptySimMeta @t
    }

globalDataRef :: forall t. TraceSystem t =>IORef (GlobalData t)
globalDataRef = unsafePerformIO (newIORef gd)
 where
  gd = GlobalData
    { globTraces = Map.empty
    , globFound = []
    , globMessages = []
    , globError = Nothing
    , globFirstRun = True
    , globMeta = emptyGlobMeta @t
    }
{-# OPAQUE globalDataRef #-}

-- | Simulate a design by forcefully evaluating an output signal.
simulate ::
  forall t dom a.
  TraceSystem t =>
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
  IO (Either String (Simulation t))
simulate d = simulateWith def (times d)

-- | Simulate a design by forcefully evaluating an output signal.
-- Like 'simulate', but with more options.
simulateWith ::
  forall t dom a.
  TraceSystem t =>
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
  IO (Either String (Simulation t))
simulateWith = simulate0 globalDataRef

-- | Internal simulation function that takes the global reference as a parameter.
simulate0 ::
  forall t dom a.
  TraceSystem t =>
  KnownDomain dom =>
  NFDataX a =>
  IORef (GlobalData t) ->
  Config ->
  Times ->
  [ClockWave] ->
  [String] ->
  Signal dom a ->
  IO (Either String (Simulation t))
simulate0 ref simConfig@Config{..} simTimes clockWaves signals sig = do
  simTimestamp <- getCurrentTime

  firstRun <- atomicModifyIORef' ref
    (\glob@GlobalData{globFirstRun} -> (glob{globFirstRun=False},globFirstRun))

  when (not firstRun && statusMsgs) $ do
    hPutStrLn stderr "Warning: `simulate` can only be safely called once"

  -- evaluate signal
  let cycRange = simulationTimeRangeToCycles simTimes (clockPeriodTime @dom)
  evalResult <- forceEvaluateSignal ref sig signals cycRange statusMsgs

  -- create Simulation
  GlobalData{globTraces,globMeta} <- readIORef ref
  let simTraces = Map.union
        (Map.fromList $ L.map (\(n,p,a) -> (n,clockTrace0 @t p a)) clockWaves)
        globTraces
      simMeta = fromGlobMeta @t globMeta
  pure $ Simulation{simConfig, simTimes, simTraces, simTimestamp, simMeta} <$ evalResult

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
  forall t dom a.
  KnownDomain dom =>
  NFDataX a =>
  IORef (GlobalData t) ->
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
  extractFoundAndMsgs :: GlobalData t -> (GlobalData t, ([String], [String], Maybe String))
  extractFoundAndMsgs glob@GlobalData{..} =
    (glob{globFound = [], globMessages = []}, (globFound,globMessages,globError))



-- | Change when the 'Simulation' starts and stops.
setStartStop ::
  Time ->
  AtOrForTime ->
  Simulation t ->
  Simulation t
setStartStop start stop sim@Simulation{simTimes} = sim{simTimes=simTimes{start,stop}}

-- | Change when the clocks start in a 'Simulation'.
setClockStart ::
  Time ->
  Simulation t ->
  Simulation t
setClockStart clockStart sim@Simulation{simTimes} = sim{simTimes=simTimes{clockStart}}

-- | Change the start, stop and clock start times in a 'Simulation'.
setTimes ::
  Times ->
  Simulation t ->
  Simulation t
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
registerTrace :: String -> Trace t -> GlobalData t -> Either String (GlobalData t)
registerTrace name trc glob@GlobalData{globTraces} =
  if Map.member name globTraces
  then Left ("Trace " <> name <> " already exists")
  else Right glob{globTraces = Map.insert name trc globTraces}

-- | Mark a signal/signals as found.
registerFound :: [String] -> GlobalData t -> GlobalData t
registerFound new glob@GlobalData{globFound} = glob{globFound = new <> globFound}

-- | Store a message.
registerMsg :: String -> GlobalData t -> GlobalData t
registerMsg msg glob@GlobalData{globMessages} = glob{globMessages = msg : globMessages}

-- | Store an error message.
registerErr :: String -> GlobalData t -> GlobalData t
registerErr msg glob = glob{globError = Just msg}

-- | Trace a 'Signal'.
-- This converts the signal to a trace, and stores it in global storage.
trace ::
  forall t dom a.
  TraceSystem t =>
  KnownDomain dom =>
  Traceable t a =>
  String ->
  Signal dom a ->
  Signal dom a
trace name sig =
  unsafePerformIO
    $ atomicModifyIORef' globalDataRef (\g -> (trace0 @t @dom name (toTrace sig) g, sig))
{-# OPAQUE trace #-}

trace0 ::
  forall t dom.
  KnownDomain dom =>
  String ->
  Trace t ->
  GlobalData t ->
  GlobalData t
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
  forall t dom a n.
  TraceSystem t =>
  KnownDomain dom =>
  Traceable t a =>
  KnownNat n =>
  String ->
  Signal dom (Vec n a) ->
  Signal dom (Vec n a)
traceVec name sig =
  unsafePerformIO
    $ atomicModifyIORef' globalDataRef (\g -> (traceVec0 @t name sig g,sig))
{-# OPAQUE traceVec #-}

traceVec0 ::
  forall t dom a n.
  TraceSystem t =>
  KnownDomain dom =>
  Traceable t a =>
  KnownNat n =>
  String ->
  Signal dom (Vec n a) ->
  GlobalData t ->
  GlobalData t
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

-- | Type to remember this is not an ordinary bool (and remember polarity and reset type).
newtype ResetVal dom = ResetVal Bool
 deriving (Show, NFDataX, BitPack, Generic)

-- | Like 'trace', but operates on a 'Reset'
traceReset ::
  forall t dom.
  TraceSystem t =>
  KnownDomain dom =>
  Traceable t (ResetVal dom) =>
  -- | Name of signal in the simulation output
  String ->
  -- | Reset to trace
  Reset dom ->
  Reset dom
traceReset name rst = trace @t name (ResetVal @dom <$> unsafeFromReset rst) `seq` rst
{-# OPAQUE traceReset #-}

-- | Type to remember this is not an ordinary bool.
newtype EnableVal dom = EnableVal Bool
 deriving (Show, NFDataX, BitPack, Generic)

-- Like traceReset for enables
traceEnable ::
  forall t dom.
  TraceSystem t =>
  KnownDomain dom =>
  Traceable t (EnableVal dom) =>
  String ->
  Enable dom ->
  Enable dom
traceEnable name en = trace @t name (EnableVal @dom <$> fromEnable en) `seq` en
{-# OPAQUE traceEnable #-}

-- | Create a clock trace from a domain.
clockTrace ::
  forall t dom.
  TraceSystem t => KnownDomain dom =>
  Trace t
clockTrace = clockTrace0 @t (clockPeriodTime @dom) vActiveEdge
 where
  VDomainConfiguration{vActiveEdge} = vDomain (knownDomain @dom)

newtype ClockVal = ClockVal Bool
 deriving (Show, NFDataX, BitPack, Generic)

-- | Create a trace from a clock period and active edge. The period *must* be an even number of fs.
clockTrace0 ::
  forall t.
  TraceSystem t =>
  Time ->
  ActiveEdge ->
  Trace t
clockTrace0 (TimeFS t) edge
  | (halfPeriod,0) <- t `divMod` 2 = Trace
    { dataType = simpleTypeRep @ClockVal
    , period = TimeFS halfPeriod
    , width = 1
    , values = L.cycle $ L.map (unsafeToTup . pack) bits
    , meta = traceMeta @t @ClockVal
    }
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
  forall t dom a.
  TraceSystem t =>
  KnownDomain dom =>
  Traceable t a =>
  Signal dom a ->
  Trace t
toTrace sig = Trace
  (simpleTypeRep @a)
  (clockPeriodTime @dom)
  (fromInteger $ natVal $ Proxy @(BitSize a))
  (sample $ unsafeToTup . pack <$> sig)
  (traceMeta @t @a)
 where
  unsafeToTup (BV mask value) = (mask, value)

-- | Create a 'Signal' from a 'Trace'.
-- This checks whether the type is compatible. See also @compatible@.
fromTrace ::
  forall t dom a.
  Traceable t a =>
  Trace t ->
  Either String (Signal dom a)
fromTrace Trace{dataType, values}
  | compatible dataType (simpleTypeRep @a) =
      Right $ fromList $ L.map (unpack . uncurry BV) values
  | otherwise =
      Left "Trace did not match target data type"

-- | Add a 'Trace' to a 'Simulation'
addTrace ::
  String ->
  Trace t ->
  Simulation t ->
  Either String (Simulation t)
addTrace name trc sim@Simulation{simTraces} =
  if Map.member name simTraces
  then Left ("Trace " <> name <> " already exists")
  else Right sim{simTraces = Map.insert name trc simTraces}

-- | Retrieve a captured 'Trace'.
fetchTrace ::
  String ->
  Simulation t ->
  Either String (Trace t)
fetchTrace name Simulation{simTraces} =
  maybeToEither ("Trace " <> name <> " not found") $ Map.lookup name simTraces

-- | Retrieve a 'Signal' from the captured traces.
fetch ::
  forall t dom a.
  KnownDomain dom =>
  Traceable t a =>
  String ->
  Simulation t ->
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
