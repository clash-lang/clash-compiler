{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

module Clash.Signal.Simulation where

import           Control.Monad         (foldM)
import           Data.Binary           (encode)
import           Data.ByteString.Lazy  (ByteString)
import           Data.Default          (Default(..))
import           Data.Either.Extra     (maybeToEither)
import           Data.IORef
  (IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef)
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Typeable         (Typeable, typeRep, Proxy(..))
import           GHC.Natural           (Natural)
import           GHC.TypeLits          (KnownNat, natVal, symbolVal)
import           System.IO.Unsafe      (unsafePerformIO)

import           Clash.Class.BitPack   (BitPack(..), BitSize)
import           Clash.XException      (deepseqX, NFDataX)
import           Clash.Signal          (KnownDomain, knownDomain, Signal, Clock, Reset, Enable, fromEnable, unsafeFromReset, sample, SDomainConfiguration(..), unbundle)
import           Clash.Sized.Internal.BitVector
  (BitVector(BV))
import           Clash.Sized.Vector    (Vec, toList)
import           Clash.Time            (Time(..), timeInFS, AtOrForTime(..), absTime, clockPeriodTime)


type TypeRepBS = ByteString
type Period = Time
type Width = Int
type Value = (Natural, Natural)
type Trace = (TypeRepBS,Period,Width,[Value])
type TraceMap = M.Map String Trace

type Traceable a = (NFDataX a, BitPack a, Typeable a)

-- | (/name/, /period/): Name of the clock wave in the VCD output, and period of
-- the clock wave. The clock will have 50% duty cycle.
type ClockWave = (String, Time)

-- | Object containing the simulation configuration and the traces captured during simulation.
data Simulation = Simulation
  { config :: Config
  , traces :: TraceMap
  }

-- | Object containing all data that must be globally accessible.
data GlobalData = GlobalData
  { globTraces :: TraceMap
  , found :: [String]
  , messages :: [String]
  , firstRun :: Bool
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
      , found = []
      , messages = []
      , firstRun = True
      }


{----------------------------------------
SIMULATION
----------------------------------------}

globalDataRef :: IORef GlobalData
globalDataRef = unsafePerformIO (newIORef def)
{-# OPAQUE globalDataRef #-}

-- | Simulate a design by forcefully evaluating an output signal.
simulate ::
  forall dom a.
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
  NFDataX a =>
  IORef GlobalData ->
  Config ->
  [ClockWave] ->
  [String] ->
  Signal dom a ->
  IO (Either String Simulation)
simulate0 ref conf clockWaves signals sig = do undefined -- ...
-- union (M.fromList $ L.map (second clockTrace) clockWaves) globTraces

-- Change when the 'Simulation' starts and stops.
setStartStop ::
  Time ->
  AtOrForTime ->
  Simulation ->
  Simulation
setStartStop start stop sim@Simulation{config} = sim{config=config{start,stop}}

-- | Change when the clocks start in a 'Simulation'.
setClockStart ::
  Time ->
  Simulation ->
  Simulation
setClockStart clockStart sim@Simulation{config} = sim{config=config{clockStart}}

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
registerTrace name trace glob@GlobalData{globTraces} =
  if M.member name globTraces then
    Left ("Trace " <> name <> " already exists")
  else
    Right glob{globTraces = M.insert name trace globTraces}

-- | Mark a signal/signals as found.
registerFound :: [String] -> GlobalData -> GlobalData
registerFound new glob@GlobalData{found} = glob{found = new <> found}

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
trace0 name trace = right . registerTrace fullName trace . registerFound found
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
traceVec0 name sig = registerTraces . registerFound found
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
    ( encode (typeRep $ Proxy @Bool)
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
  ( encode (typeRep $ Proxy @a)
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
  | ty == encode (typeRep $ Proxy @a) = undefined -- TODO ...
  | otherwise = Left "Trace did not match target type"

-- | Add a 'Trace' to a 'Simulation'
addTrace ::
  String ->
  Trace ->
  Simulation ->
  Either String Simulation
addTrace name trc sim@Simulation{traces} =
  if M.member name traces then
    Left ("Trace " <> name <> " already exists")
  else
    Right sim{traces = M.insert name trc traces}

-- | Retrieve a captured 'Trace'.
fetchTrace ::
  String ->
  Simulation ->
  Either String Trace
fetchTrace name sim@Simulation{traces} =
  maybeToEither ("Trace " <> name <> " not found") $ M.lookup name traces

-- | Retrieve a 'Signal' from the captured traces.
fetch ::
  forall dom a.
  KnownDomain dom =>
  Traceable a =>
  String ->
  Simulation ->
  Either String (Signal dom a)
fetch name sim = fetchTrace name sim >>= fromTrace

{----------------------------------------
STORING SIGNALS
----------------------------------------}

-- | Store a trace in binary form.
store ::
  -- | Name of trace to dump
  String ->
  -- | Number of samples
  Int ->
  Simulation ->
  Either String ByteString
store name samples sim = storeTrace samples <$> fetchTrace name sim

-- | Store a 'Signal' in binary form.
storeSignal ::
  forall dom a.
  KnownDomain dom =>
  Traceable a =>
  -- | Name of trace to dump
  Signal dom a ->
  -- | Number of samples
  Int ->
  ByteString
storeSignal signal samples = storeTrace samples $ toTrace signal

-- | Convert a trace into binary form.
storeTrace :: Int -> Trace -> ByteString
storeTrace samples trc = undefined -- TODO ...

-- | Load a trace from binary form.
load ::
  forall a.
  Traceable a =>
  -- | The name to use for the signal
  String ->
  -- | The binary data
  ByteString ->
  -- | The 'Simulation' to add the signal to
  Simulation ->
  Either String Simulation
load name bin sim = loadTrace @a bin >>= (\trc -> addTrace name trc sim)

-- | Load a 'Signal' from binary form.
loadSignal ::
  forall a dom.
  Traceable a =>
  ByteString ->
  Either String (Signal dom a)
loadSignal bin = loadTrace @a bin >>= fromTrace

-- | Convert binary data into a trace for the type specified.
loadTrace ::
  forall a.
  Traceable a =>
  ByteString ->
  Either String Trace
loadTrace = undefined -- TODO ...


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
