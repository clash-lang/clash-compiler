{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Signal.Internal.Trace (
  -- * Tracing
  -- ** Polymorphic tracing
  -- To allow building upon the tracing system
  TraceSystem(..),

  -- ** Helper types
  Period, Width, Value,
  TraceMap,

  -- ** Traces
  Trace(..),
  toTrace, fromTrace,

  -- ** Tracing
  trace,
  traceVec,
  traceReset, traceEnable,

  -- ** Simulation
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

  -- ** Internal
  GlobalData(..),

  simulate0,
  forceEvaluateSignal,

  registerTrace,
  registerFound,
  registerMsg,
  registerErr,

  trace0,
  traceVec0,

  -- ** Helpers
  simulationTimeRangeToCycles,
  clockTrace, clockTrace0,
  domainName, replaceDollar,
  simOnly,

  -- * VCD
  VcdFile(..),
  DeclarationCommand(..),
  SimulationCommand(..),
  Var(..),
  ValueChange(..),
  Scope,IDCode,VcdTime,TimeUnit,
  vcd,
  vcdText,
  writeVcd,
  -- ** Helpers
  renderVcd, renderDecl, renderSim, renderChange,
  vcdCommand,
  iso8601Format,
  printable,
  zipTimed, treeFold, findChangesSimple, combineChangesPerDom,
) where

import Prelude hiding (undefined)

import Control.Exception (evaluate)
import Control.Exception.Extra (errorIO)
import Control.Monad (foldM, when)
import Control.Monad.Except (ExceptT(..), runExceptT)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.Bits (testBit)
import Data.Char (chr, ord)
import Data.Default (Default(..))
import Data.Either.Extra (maybeToEither)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Kind (Constraint)
import qualified Data.List as L
import Data.List (zip4, unzip4, uncons, isInfixOf, partition, transpose)
import Data.List.Extra (snoc)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Typeable (Typeable, Proxy(..))
import GHC.Exts (groupWith)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import GHC.TypeLits (KnownNat, natVal, symbolVal)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

import Clash.Class.BitPack (BitPack(..), BitSize)
import Clash.Magic (clashSimulation)
import Clash.Signal
  (KnownDomain, SDomainConfiguration(..), knownDomain,
  ActiveEdge(..),
  Signal, sample, sampleN_lazy, fromList,
  Reset, Enable, fromEnable, unsafeFromReset,
  unbundle)
import Clash.Signal.Internal (VDomainConfiguration(..), vDomain)
import Clash.Sized.Internal.BitVector (Bit, BitVector(BV))
import Clash.Sized.Vector (Vec, toList)
import Clash.Time
  (Time(..), timeInFS, AtOrForTime(..), absTime, clockPeriodTime, splitTimeUnit)
import Clash.XException (NFDataX(rnfX), undefined)

import Clash.Signal.Internal.Trace.CompatibleTypes (DataType, compatible, simpleTypeRep)

#ifdef CABAL
import           Data.Version          (showVersion)
import qualified Paths_clash_prelude
#endif

----------------------------------------------
--               POLYMORPHISM               --
----------------------------------------------

class (TraceableFor t ClockVal) => TraceSystem t where
  type TraceMeta t
  type SimMeta t
  type GlobMeta t
  type VcdMeta t
  type TraceableFor t a :: Constraint

  globalDataRef :: IORef (GlobalData t)

  -- | Metadata when converting a 'Signal' to a 'Trace'.
  traceMeta :: TraceableFor t a => TraceMeta t

  -- | Initialise global ref metadata.
  emptyGlobMeta :: GlobMeta t
  -- | Convert global meta data to simulation meta data.
  fromGlobMeta :: GlobMeta t -> SimMeta t
  -- | Initialise simulation meta data.
  emptySimMeta :: SimMeta t
  emptySimMeta = fromGlobMeta @t (emptyGlobMeta @t)

  -- | The version name to be used in VCD files
  vcdVersion :: String

  -- | The main processing step of the VCD file.
  -- The function is provided with the time scale and slice, named and labeled traces,
  -- and returns the initial values, simulation commands for the changes,
  -- the metadata for the 'VcdFile', and a list of warnings.
  vcdProcess ::
    VcdTime ->
    (VcdTime, VcdTime, VcdTime) ->
    [(IDCode, String, Trace t)] ->
    Either String ([ValueChange], [SimulationCommand], VcdMeta t, [String])

  -- | When writing a signal dump file, convert the trace metadata to dump metadata.
  storeMeta :: TraceMeta t -> Aeson.Value
  -- | When reading a signal dump file, convert the metadata to trace metadata.
  loadMeta :: Aeson.Value -> TraceMeta t
  -- | When reading a signal dump file with multiple signals as one trace,
  -- convert the metadata to trace metadata.
  loadMetas :: [(String,Aeson.Value)] -> TraceMeta t

instance TraceSystem () where
  type TraceMeta () = ()
  type SimMeta () = ()
  type GlobMeta () = ()
  type VcdMeta () = ()
  type TraceableFor () a = ()

  globalDataRef = unsafePerformIO (newIORef gd)
   where
    gd = GlobalData
      { globTraces = Map.empty
      , globFound = []
      , globMessages = []
      , globError = Nothing
      , globFirstRun = True
      , globMeta = emptyGlobMeta @()
      }
  {-# OPAQUE globalDataRef #-}

  traceMeta = ()
  emptyGlobMeta = ()
  fromGlobMeta = id

#ifdef CABAL
  vcdVersion = "Clash " <> showVersion Paths_clash_prelude.version
#else
  vcdVersion = "development version"
#endif
  vcdProcess v vt traces = (\(i,c) -> (i,c,(),[])) <$> vcdProcessSimple v vt traces

  storeMeta _ = Aeson.Null
  loadMeta _ = ()
  loadMetas _ = ()

----------------------------------------------
--                  TYPES                   --
----------------------------------------------

type Width = Int
type Period = Time
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
simulateWith = simulate0 (globalDataRef @t)

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
clockWave name = (replaceDollar (domainName @dom) name, clockPeriodTime @dom, vActiveEdge)
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
trace name = simOnly $ \sig ->
  unsafePerformIO
    $ atomicModifyIORef' (globalDataRef @t) (\g -> (trace0 @t @dom name (toTrace sig) g, sig))
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
traceVec name = simOnly $ \sig ->
  unsafePerformIO
    $ atomicModifyIORef' (globalDataRef @t) (\g -> (traceVec0 @t name sig g,sig))
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
    , values = cycle $ L.map (unsafeToTup . pack) bits
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

-- | Run function on signal only in simulation
simOnly :: (s->s) -> s -> s
simOnly f sig = if clashSimulation then
    f sig
  else sig

















----------------------------------------------
--             ~ ~ ~ VCD ~ ~ ~              --
----------------------------------------------





----------------------------------------------
--                  TYPES                   --
----------------------------------------------

-- | A complete VCD file.
-- This includes the contents of the VCD file.
data VcdFile t = VcdFile
  { declationCommands :: [DeclarationCommand]
  , simulationCommands :: [SimulationCommand]
  , meta :: VcdMeta t
  }
deriving instance (Show (VcdMeta t)) => Show (VcdFile t)


-- | A VCD declaration command.
data DeclarationCommand
  = Version String                   -- ^ The (Clash) version used to generate the VCD.
  | Date UTCTime                     -- ^ The timestamp the VCD was created.
  | TimeScale Time                   -- ^ The unit of the VCD time stamps.
                                     -- Must be 1, 10 or 100 and an appropriate unit.
  | VarDec Var                       -- ^ Variable declaration.
  | Scope Scope [DeclarationCommand] -- ^ A closed scope declaration.
  | Comment String                   -- ^ A comment.
  deriving (Show)

-- | A VCD simulation command.
data SimulationCommand
  = SimulationTime VcdTime     -- ^ Timestamp.
  | DumpVars [ValueChange]     -- ^ The initial values of variables.
  | ValueChanges [ValueChange] -- ^ Value changes.
 deriving (Show)

-- | A VCD Variable.
data Var
  = Var
  { varSize      :: Width  -- ^ Bit width.
  , varIDCode    :: IDCode -- ^ Variable ID (typically one character).
  , varReference :: String -- ^ Variable name.
  }
  deriving (Show)

-- | A change to a variable's value.
data ValueChange
  = ValueChange
  { changeSize   :: Width  -- ^ Bit width of the value.
  , changeIDCode :: IDCode -- ^ ID of the variable that changed.
  , changeValue  :: Value  -- ^ The new value.
  }
  deriving (Show)

type Scope = String
type IDCode = String
type VcdTime = Integer --TODO: check if Int is faster
type TimeUnit = String


----------------------------------------------
--                GENERATION                --
----------------------------------------------

-- | Create a 'Vcd' object from a 'Simulation'.
-- Also returns warnings that pop up in the creation of the VCD.
vcd ::
  forall t.
  TraceSystem t =>
  Simulation t ->
  Either String (VcdFile t, [String])
vcd Simulation
      { simTimes = Times{start, stop, clockStart}
      , simConfig = Config{shiftToZero, warnZeroWidth}
      , simTraces = traceMap
      , simTimestamp
      }
  | absTime start stop < start
  = Left $
         "VCD: stop was "
      <> show stop
      <> ", which is earlier than start ("
      <> show start
      <> ")."
  | start < 0 && shiftToZero == False
  = Left $
         "VCD: Start time was "
      <> show start
      <> ", but cannot be negative without shifting the start to 0."
  | null allTraces
  = Left "VCD: No traces found."
  | (nm:_) <- offensiveNames
  = Left $
         "VCD: Trace '"
      <> nm
      <> "' contains non-printable ASCII characters, which is not supported by VCD."
  | (nm:_) <- emptyScopes
  = Left $
         "VCD: Trace '"
      <> nm
      <> "' contains empty scope names, which is not supported by VCD."
  | otherwise
  = assemble <$> processed
 where
  offensiveNames = filter (any (not . printable)) allNames
  emptyScopes = filter (\nm -> ".." `isInfixOf` ('.' : nm <> ".")) allNames

  ----------- SPLIT TRACES INTO PARTS -----------

  -- (slightly more complex so we can cut out zero-width traces
  -- while keeping them for the signal declarations, so we can add comments)
  allLabels = concatMap (\s -> map (snoc s) alphabet) ("": allLabels)
    where alphabet = map chr [33..126]

  allTraces = Map.toAscList traceMap
  allNames = Map.keys traceMap

  (nonUnitTraces, unitTraces) =
    partition (\(_l,(_n,t)) -> t.width>0) $ zip allLabels allTraces

  --(labels,names,periods,widths,valuess,metas) = unzip6
  --  $ map (\(l,(n,Trace{..})) -> (l,n,period,width,values,meta)) nonUnitTraces

  unitTraceNames = map (fst . snd) unitTraces
  warnings =
    if warnZeroWidth
    then map
          (\n -> "Warning: Zero-width trace '" <> n <> "' is omitted from the VCD")
          unitTraceNames
    else []

  ----------- VCD FILE CONSTRUCTION ------------

  assemble (initialValues,changes,meta,warn) =
    (VcdFile (headers ++ variables) simulation meta, warn <> warnings)
   where
    headers =
      [ Date simTimestamp
      , Version $ vcdVersion @t
      , TimeScale $ TimeFS vcdTimeScaleFS
      ]

    simulation =
      [ SimulationTime vStart
      , DumpVars initialValues ]
      <> changes
      <> [SimulationTime vStop]

  ------------ SIGNAL DECLARATIONS -------------

  variables = mergeScopes varScopes []
   where
    varScopes = map (\(n,w,l) -> mkScope (splitOn "." n) w l)
      -- $ sort -- automatically sorted because `names` comes from map keys
      $ zip3 allNames (map ((.width) . snd) allTraces) allLabels

  ----------------- TIME UNITS -----------------

  {-

  - gather all periods, take the GCD (in FS). This is the *simulation time scale*
    and starts at the clock start. This is essentially the clock cycle index of the
    GCD'd clock.

  - from the simulation time scale, start/stop time, and clock start time, compute the
    GCD. Then, take the highest power of 10 that fits inside this value. This
    is the *VCD time*, which is used in VCD timestamps.

  -}

  simTimeScaleFS = foldl1 gcd $ map timeInFS (map (\(_,(_,t)) -> t.period) nonUnitTraces)

  vcdTimeScaleFS = largestPow10 1
    $ foldl1 gcd
    $ simTimeScaleFS : map timeInFS [ start
                                    , absTime start stop
                                    , clockStart ]
   where
    largestPow10 p x | x>=10*p = largestPow10 (p*10) x
                     | otherwise = p

  timeOffset =
    if shiftToZero
    then timeInFS (-start) `div` vcdTimeScaleFS
    else 0 :: VcdTime

  vClockStart = timeInFS clockStart           `div` vcdTimeScaleFS + timeOffset :: VcdTime
  vStart      = timeInFS start                `div` vcdTimeScaleFS + timeOffset :: VcdTime
  vStop       = timeInFS (absTime start stop) `div` vcdTimeScaleFS + timeOffset :: VcdTime


  --------------- SIGNAL VALUES ----------------

  -- For 'vcdProcess', see also 'vcdProcessSimple'
  processed = vcdProcess @t
    vcdTimeScaleFS
    (vStart,vClockStart,vStop)
    (map (\(l,(n,t))->(l,n,t)) nonUnitTraces)

-- | Add a scope to a list of scopes.
-- Assumes lexographically sorted signals between the scopes, i.e. @a:[b,c]@.
addScope :: DeclarationCommand -> [DeclarationCommand] -> [DeclarationCommand]
addScope (Scope n xs) (Scope m ys:l)  | n == m = Scope m (mergeScopes xs ys) : l
addScope s l = s:l

-- Merge two lists of scopes.
-- Assumes lexographically sorted signals, i.e. @[a,b]<>[c,d]@.
mergeScopes :: [DeclarationCommand] -> [DeclarationCommand] -> [DeclarationCommand]
mergeScopes [] ys = ys
mergeScopes (x:xs) ys = addScope x $ mergeScopes xs ys

-- | Turns a list of names with the necessary extra information into a set of nested
-- scopes. 0-bit signals are turned into comments.
mkScope :: [String] -> Width -> IDCode -> DeclarationCommand
mkScope [] _ _ = error "(unreachable) empty signal name"
mkScope [varReference] 0 _ = Comment $ "Omitted 0-bit trace " <> varReference
mkScope [varReference] varSize varIDCode = VarDec $ Var{..}
mkScope (n:ns) w l = Scope n [mkScope ns w l]


-- | Take the time parameters and traces, and create
vcdProcessSimple ::
  VcdTime ->
  (VcdTime,VcdTime,VcdTime) ->
  [(IDCode,String,Trace t)] ->
  Either String ([ValueChange],[SimulationCommand])
vcdProcessSimple
    vcdTimeScaleFS
    vTimes
    traces
  = Right (initialValues, changes)
 where
  {-
  - group signals by period
  - per period (domain), slice:
    - slice based on time
    - take the initial samples
    - compare samples to only get changes
  -}

  (labels,periods,widths,valuess) = unzip4
    $ map (\(l,_n,Trace{..}) -> (l,period,width,values)) traces

  domains :: [(Period,([Width],[IDCode],[[Value]]))]
  domains =
    map
      (\trcs -> (fst $ unsafeHead trcs, unzip3 $ map snd trcs))
      $ groupWith fst
      $ zip periods
      $ zip3 widths labels valuess

  unsafeHead (x:_) = x
  unsafeHead [] = error "(unreachable) cannot take head of empty list"

  (initialValues,changes) =
    combineChangesPerDom $ map (findChangesSimple vcdTimeScaleFS vTimes) domains

-- | Combine a list of pairs of initial values and timestamped changes.
combineChangesPerDom ::
  [([ValueChange],[(VcdTime,[ValueChange])])] ->
  ([ValueChange],[SimulationCommand])
combineChangesPerDom changesPerDomain = (initialValues, changes)
 where
  initialValues :: [ValueChange]
  initialValues = concatMap fst changesPerDomain

  -- essentially merge sort with nested merging on equal values
  -- TODO: small to large merging might be better?
  blocks :: [(VcdTime, [ValueChange])]
  blocks =
    treeFold zipTimed $
      map snd changesPerDomain

  changes :: [SimulationCommand]
  changes = concatMap (\(t,cs) -> [SimulationTime t, ValueChanges cs]) blocks

-- | Zip two lists of timestamped lists of changes.
-- A single merge in the merge sort
zipTimed :: [(VcdTime,[a])] -> [(VcdTime,[a])] -> [(VcdTime,[a])]
zipTimed aa [] = aa
zipTimed [] bb = bb
zipTimed aa@((ta,va):as) bb@((tb,vb):bs) =
  case compare ta tb of
    LT -> (ta,    va) : zipTimed as bb
    EQ -> (ta,va<>vb) : zipTimed as bs
    GT -> (tb,    vb) : zipTimed aa bs

-- | Find changes in a list of signals with equal period
findChangesSimple
  :: VcdTime
  -> (VcdTime, VcdTime, VcdTime)
  -> (Period,([Width],[String],[[Value]]))
  -> ([ValueChange],[(VcdTime,[ValueChange])])
findChangesSimple
  vcdTimeScaleFS
  (vStart, vClockStart, vStop)
  (period,(widths0,labels0,valuess0))
  = (initial0, samples0)
 where
  clkEdges :: [VcdTime]
  clkEdges = [vClockStart, vClockStart + vPeriod ..]
    where
    vPeriod = timeInFS period `div` vcdTimeScaleFS

  -- group by clock cycle and add timestamps (the initial value starts at -inf)
  valuessFrom :: [(VcdTime,[Value])]
  valuessFrom = zip (-1000000000000:clkEdges) $ transpose valuess0
    --TODO: minBound, but we're using integers, which are unbounded :/

  -- drop values until we reach the start
  skipStart :: [(VcdTime,[Value])]
  skipStart = map fst $ dropWhile ((<= vStart) . snd) $ zip valuessFrom clkEdges

  ((_,initial), rest) = fromMaybe (error "(unreachable) finite signal") $ uncons skipStart

  samples :: [(VcdTime,[Value])]
  samples = takeWhile ((< vStop) . fst) rest

  filterChanges (_ta,va) (tb,vb) =
    (tb
    , [ValueChange w l b | (a,b,w,l) <- zip4 va vb widths0 labels0, a /= b]
    )

  initial0 :: [ValueChange]
  initial0 = zipWith3 ValueChange widths0 labels0 initial

  samples0 :: [(VcdTime,[ValueChange])]
  samples0 = filter (not . null . snd) $ zipWith filterChanges skipStart samples


----------------------------------------------
--                  OUTPUT                  --
----------------------------------------------

-- | Create a VCD file for the given traces and simulation configuration.
--
-- Zero-width signals are not included, and if `warnZeroWidth` is one, warnings for these signals
-- are returned.
vcdText ::
  forall t.
  TraceSystem t =>
  Simulation t ->
  Either String (Text, [String])
vcdText sim = (first renderVcd) <$> vcd sim

-- | Create a VCD file for the given traces and simulation configuration,
-- and write it to a file.
--
-- Warnings are printed. Errors if the VCD generation fails.
writeVcd ::
  forall t.
  HasCallStack =>
  TraceSystem t =>
  FilePath ->
  Simulation t ->
  IO ()
writeVcd file sim = do
  case vcd sim of
    Left e -> withFrozenCallStack $ errorIO e
    Right (vcdFile, warnings) -> do
      let text = renderVcd vcdFile
      mapM_ (hPutStrLn stderr) warnings
      Data.Text.IO.writeFile file text

-- | Render a 'Vcd' object as 'Text'.
renderVcd ::
  VcdFile t ->
  Text
renderVcd (VcdFile decl sim _) = Text.unlines
  $  map renderDecl decl
  <> ["$enddefinitions $end\n"]
  <> map renderSim sim

-- | Render a VCD 'DeclarationCommand' as 'Text'.
renderDecl ::
  DeclarationCommand ->
  Text
renderDecl = \case
  Version v ->
    vcdCommand "version" [Text.pack v]
  Date t ->
    vcdCommand "date" [Text.pack $ iso8601Format t]
  TimeScale t | (x,u) <- splitTimeUnit t ->
    vcdCommand "timescale" [Text.show x, Text.pack u]
  VarDec Var{varSize, varIDCode, varReference} ->
    vcdCommand "var"
      [ "wire"
      , Text.show varSize
      , Text.pack varIDCode
      , Text.pack varReference ]
  Scope name subs -> Text.intercalate "\n" $
    vcdCommand "scope" ["module", Text.pack name]
    : map renderDecl subs
    <> [vcdCommand "upscope" [] ]
  Comment c ->
    vcdCommand "comment" [Text.pack c]

-- | Render a VCD 'SimulationCommand' as 'Text'.
renderSim ::
  SimulationCommand ->
  Text
renderSim = \case
  SimulationTime t ->
    "#" <> Text.show t
  DumpVars ch ->
    Text.unlines
      $ "$dumpvars"
        : map renderChange ch
        <> ["$end"]
  ValueChanges ch ->
    Text.unlines $ map renderChange ch

-- | Render a VCD 'ValueChange' as 'Text'.
renderChange ::
  ValueChange ->
  Text
renderChange (ValueChange 1 idCode (0, 0)) =
  Text.pack ('0':idCode)
renderChange (ValueChange 1 idCode (0, 1)) =
  Text.pack ('1':idCode)
renderChange (ValueChange 1 idCode (1, _)) =
  Text.pack ('x':idCode)
renderChange (ValueChange 1 idCode (mask, val)) =
  error $
    "Can't format 1 bit wide value for "
      ++ show idCode
      ++ ": value "
      ++ show val
      ++ " and mask "
      ++ show mask
renderChange ValueChange{..} =
  Text.pack $
       ('b' : shorten (map digit $ reverse [0 .. changeSize - 1]))
    <> (' ' : changeIDCode)
 where
  (mask, val) = changeValue
  digit d = case (testBit mask d, testBit val d) of
    (False,False) -> '0'
    (False,True)  -> '1'
    (True,_)      -> 'x'

  -- 01_ -> 1_
  -- 00_ -> 0_ ->
  -- xx_ -> x_ ->
  -- _ -> _
  shorten xs = go xs
   where
    go (x0:xs0@(x1:_)) | extends x0 x1 = go xs0
    go xs0 = xs0

    extends e 'x' = e == 'x'
    extends e _   = e == '0'

-- | Create a VCD command of the form @$<tag> <contents> $end@.
vcdCommand ::
  Text ->
  [Text] ->
  Text
vcdCommand tag contents = Text.unwords $ ('$' `Text.cons` tag) : contents <> ["$end"]

-- | Format the time according to iso8601: @<yyyy>-<mm>-<dd>T<hh>:<mm>:<ss>@.
iso8601Format ::
  UTCTime ->
  String
iso8601Format = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"


----------------------------------------------
--                   MSIC                   --
----------------------------------------------

-- | Check whether a character is a simple printable (visible) ASCII character.
printable :: Char -> Bool
printable (ord -> c) = 33 <= c && c <= 126

-- | Merge items with the given function using a binary tree.
-- A linear fold would incur quadratic runtime.
treeFold :: (a -> a -> a) -> [a] -> a
treeFold f0 xs0 = treeFold0 (length xs0) f0 xs0
 where
  treeFold0 _ _ []  = error "treeFold used on empty sequence" -- unreachable for us
  treeFold0 _ _ [x] = x
  treeFold0 l f xs  = treeFold0 l0 f as `f` treeFold0 (l - l0) f bs
   where
    l0 = l `div` 2
    (as, bs) = splitAt l0 xs
