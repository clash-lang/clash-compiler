

module Clash.Signal.Simulation where

type TypeRepBS = String
type Period = Time
type Width = Natural
type Value = [(Natural, Natural)]

type Traceable a = (NFDataX a, BitPack a, Typeable a)

type Trace = (TypeRepBS,Period,Width,[Value])

type TraceMap = Map String Trace

-- | (/name/, /period/): Name of the clock wave in the VCD output, and period of
-- the clock wave. The clock will have 50% duty cycle.
type ClockWave = (String, Time)

-- | TODO
clockWave ::
  forall dom.
  KnownDomain dom =>
  String ->
  ClockWave
clockWave name = (name, clockPeriod @dom)

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
    VcdConfig
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
simulateWith = ...

-- Change when sim starts and stops
setStartStop ::
  Time ->
  AtOrForTime ->
  Simulation ->
  Simulation
setStartStop start stop sim@Simulation{config} = sim{config=config{start,stop}}

-- Change when clock starts in sim
setClockStart ::
  Time ->
  Simulation ->
  Simulation
setClockStart clockStart sim@Simulation{config} = sim{config=config{clockStart}}





-- Zero-width signals are dropped from the VCD

-- | Create a VCD file for the given traces and simulation configuration.
vcdText ::
  Simulation ->
  Either String Text
vcdText = ...

-- | Create a VCD file for the given traces and simulation configuration,
-- and write it to a file.
-- Errors if the VCD generation fails
writeVcd ::
  FilePath ->
  Simulation ->
  IO ()
writeVcd file sim = do
  text <- assertRight $ vcdText sim
  writeFile file text

-- | Store a trace in binary form.
store ::
  -- | Name of trace to dump
  String ->
  -- | Number of samples
  Int ->
  Simulation ->
  IO ByteString
store = ...

-- | Store a 'Signal' in binary form.
storeSignal ::
  forall dom a.
  KnownDomain dom =>
  Traceable a =>
  Signal dom a ->
  ByteSting
storeSignal = ...

-- | Load a trace from binary form.
load ::
  forall a dom.
  Traceable a =>
  ByteString ->
  Simulation ->
  Either String (Simulation)
load = ...

-- This is replay
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

-- | Convert a trace into binary form.
storeTrace :: Trace -> ByteString
storeTrace = ...

-- | Trace a 'Signal'.
-- This converts the signal to a trace, and stores it in global storage.
trace ::
  forall dom a.
  KnownDomain dom =>
  Traceable a =>
  String ->
  Signal dom a ->
  Signal dom a
trace name sig = unsafePerformIO (atomicModifyIORef globalData (trace0 name sig)) `seq` sig

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

-- put a trace in the global data
registerTrace :: String -> Trace -> GlobalData -> Either String GlobalData
registerTrace name trace glob@GlobalData{traces,found} =
  if M.member name traces then
    Left ("Trace " <> name <> " already exists")
  else
    Right glob{traces = M.insert name trace}

registerFound :: [String] -> GlobalData -> GloblaData
registerFound new glob@GlobalData{found} = glob{found = new <> found}

-- | Trace all values in a vector signal individually.
traceVec ::
  forall dom a n.
  KnownDomain dom =>
  Traceable a =>
  KnownNat n =>
  String ->
  Signal dom (Vec n a) ->
  Signal dom (Vec n a)
traceVec name sig = unsafePerformIO (atomicModifyIORef globalData (traceVec0 name sig)) `seq` sig

traceVec0 ::
  forall dom a n.
  KnownDomain dom =>
  Traceable a =>
  KnownNat n =>
  String ->
  Signal dom (Vec n a) ->
  GlobalData ->
  GlobalData
traceVec0 = ...

-- | Retrieve a 'Signal' from the captured traces.
fetch ::
  forall dom a.
  Traceable a ->
  String ->
  Simulation ->
  Either String (Signal dom a)
fetch name sim = fromTrace <$> fetchTrace name sim

-- | Retrieve a captured 'Trace'.
fetchTrace ::
  String ->
  Simulation ->
  Either String Trace
fetchTrace name sim@Simulation{traces} =
  maybeToEither ("Trace " <> name <> " not found") $ M.lookup name traces

-- | Add a 'Trace' to a 'Simulation'
addTrace ::
  String ->
  Trace ->
  Simulation ->
  Either String Simulation
addTrace name trace sim@Simulation{traces} =
  if M.member name traces then
    Left ("Trace " <> name <> " already registered")
  else
    Right sim{traces = M.insert name trace traces}

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
  | otherwise = "Trace did not match target type"




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

-- Like traceReset for enables
traceEnable ::
  forall dom.
  KnownDomain dom =>
  String ->
  Enable dom ->
  Enable dom
traceEnable name en = trace name (fromEnable en) `seq` en
