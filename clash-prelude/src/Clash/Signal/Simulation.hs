

module Clash.Signal.Simulation where

type TypeRepBS = String
type Period = Time
type Width = Natural
type Value = [(Natural, Natural)]
type Trace = (TypeRepBS,Period,Width,[Value])
type TraceMap = Map String Trace

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
  { traces :: TraceMap
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
      , clockStart = TimeNS 100
      , shiftToZero = True
      , statusMsgs = True
      , warnZeroWidth = True
      }

instance Default GlobalData where
  def =
    GlobalData
      { traces = []
      , found = []
      , messages = []
      , firstRun = True
      }


{----------------------------------------
SIMULATION
----------------------------------------}

globalDataRef :: IORef globalDataRef
globalDataRef = unsafePerformIO (newIORef def)
{-# OPAQUE globalDataRef #-}

-- | Simulate a design by forcefully evaluating an output signal.
simulate ::
  forall a.
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
  forall a.
  NFDataX a =>
  Config ->
  -- | Clock waves to render
  [ClockWave] ->
  -- | Names of the traces you definitely want to be in the output
  [String] ->
  -- | (One of) the outputs of the circuit containing the traces
  Signal dom a ->
  IO (Either String Simulation)
simulateWith = simulate0 globalData

-- | Internal simulation function that takes the global reference as a parameter.
simulate0 ::
  forall a.
  NFData a =>
  IORef GlobalData ->
  Config ->
  [ClockWave] ->
  [String] ->
  Signal dom a ->
  IO (Either String Simulation)
simulate0 = ...

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
clockWave name = (name, clockPeriod @dom)

{----------------------------------------
TRACING
----------------------------------------}

-- | Put a trace in the global data.
registerTrace :: String -> Trace -> GlobalData -> Either String GlobalData
registerTrace name trace glob@GlobalData{traces,found} =
  if M.member name traces then
    Left ("Trace " <> name <> " already exists")
  else
    Right glob{traces = M.insert name trace}

-- | Mark a signal/signals as found.
registerFound :: [String] -> GlobalData -> GloblaData
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
trace name sig = unsafePerformIO (atomicModifyIORef globalDataRef (trace0 name sig)) `seq` sig
{-# OPAQUE trace #-}

trace0 ::
  forall dom a.
  KnownDomain dom =>
  Traceable a =>
  String ->
  Signal dom a ->
  GlobalData ->
  GlobalData
trace0 name sig = right . registerTrace fullName trace . registerFound found
 where
  trace = toTrace sig
  fullName = replace "$" (domainName @dom) name
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
traceVec name sig = unsafePerformIO (atomicModifyIORef globalDataRef (traceVec0 name sig)) `seq` sig
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
  fullName = replace "$" (domainName @dom) name
  names = map (\i -> name <> "." <> show i) [0..length traces-1]
  fullNames = map (\i -> fullName <> "." <> show i) [0..length traces-1]
  found = if fullName == name then name:names else [name, fullName]<>names<>fullNames
  registerTraces = foldr (.) id $ zipWith registerTrace fullNames traces
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

{----------------------------------------
TRACES
----------------------------------------}

-- | Create a 'Trace' from a 'Signal'.
toTrace ::
  forall dom a.
  KnownDomain dom =>
  Traceable a ->
  Signal dom a ->
  Trace
toTrace sig =
  ( encode (typeRep @a)
  , clockPeriod @dom
  , natVal (Proxy @(BitSize a))
  , sample (unsafeToTup . pack <$> signal) )
 where
  unsafeToTup (BV mask value) = (mask, value)

-- | Create a 'Signal' from a 'Trace'.
-- This only checks whether the type is correct.
fromTrace ::
  forall dom a.
  KnownDomain dom ->
  Traceable a ->
  Trace ->
  Either String (Signal dom a)
fromTrace (ty,_period,_width,values)
  | ty == encode (typeRep @a) = ...
  | otherwise = Left "Trace did not match target type"

-- | Add a 'Trace' to a 'Simulation'
addTrace ::
  String ->
  Trace ->
  Simulation ->
  Either String Simulation
addTrace name trace sim@Simulation{traces} =
  if M.member name traces then
    Left ("Trace " <> name <> " already exists")
  else
    Right sim{traces = M.insert name trace traces}

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
  Traceable a ->
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
store name samples = storeTrace samples <$> fetchTrace name

-- | Store a 'Signal' in binary form.
storeSignal ::
  forall dom a.
  KnownDomain dom =>
  Traceable a =>
  -- | Name of trace to dump
  Signal dom a ->
  -- | Number of samples
  Int ->
  ByteSting
storeSignal signal samples = storeTrace samples $ toTrace signal

-- | Convert a trace into binary form.
storeTrace :: Trace -> ByteString
storeTrace = ...

-- | Load a trace from binary form.
load ::
  forall a dom.
  Traceable a =>
  -- | The name to use for the signal
  String ->
  -- | The binary data
  ByteString ->
  -- | The 'Simulation' to add the signal to
  Simulation ->
  Either String Simulation
load name bin sim = loadTrace @a bin >>= (\trace -> registerTrace name trace sim)

-- | Load a 'Signal' from binary form.
loadSignal ::
  forall a dom.
  Traceable a =>
  ByteString ->
  Either String (Signal dom a)
loadSignal bin = fromTrace <$> loadTrace bin

-- | Convert binary data into a trace for the type specified.
loadTrace ::
  forall a.
  Traceable a =>
  ByteString ->
  Either String Trace
loadTrace = ...
