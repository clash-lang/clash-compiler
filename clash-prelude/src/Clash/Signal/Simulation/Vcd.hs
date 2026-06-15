

module Clash.Simulation.Vcd (vcdText, writeVcd) where

import Clash.Simulation



data VcdFile = VcdFile [DeclarationCommand] [SimulationCommand]
  deriving (Show)

data DeclarationCommand
  = Version String
  | Date-- UTCTime -- date is only created when turning into text? or
  | TimeScale (Int,TimeUnit)
  | VarDec Var
  | Scope Scope [DeclarationCommand]
  | Comment String
  deriving (Show)

data SimulationCommand
  = SimulationTime VcdTime
  | DumpVars [ValueChange]
  | ValueChanges [ValueChange]
  deriving (Show)

data Var
  = Var
  { varSize   :: Width
  , varIDCode :: IDCode
  , varReference    :: String
  }
  deriving (Show)

data ValueChange
  = ValueChange
  { changeSize :: Width
  , changeIDCode :: IDCode
  , changeValue :: Value
  }
  deriving (Show)

type Scope = String
type IDCode = String
type VcdTime = Int
type TimeUnit = String

-- | Create a 'Vcd' object from a 'Simulation'.
vcd ::
  Simulation ->
  Either String VcdFile
vcd Simulation
      { config = Config
                  { start, stop, clockStart
                  , shiftToZero
                  , statusMsgs
                  , warnZeroWidth
                  }
      , traces = traceMap
      }
  | stop < start =
      Left $ "VCD: stop was " <> show start <> ", which is earlier than start (" <> show startPs <> ")."
  | start < 0 && shiftToZero == False =
      Left $ "VCD: Start time was " <> show start <> ", but cannot be negative without shifting the start to 0."
  | null names =
      Left $ "VCD: No traces found."
  | (nm:_) <- offensiveNames =
      Left $ unwords [ "Trace '" ++ nm ++ "' contains"
                     , "non-printable ASCII characters, which is not"
                     , "supported by VCD." ]
  | (nm:_) <- emptyScopes =
      Left $ unwords [ "Trace '" ++ nm ++ "' contains"
                     , "empty scope names, which is not"
                     , "supported by VCD." ]
  | otherwise =
      Right $ VCDFile
        ( headers ++ variables )
        simulation
 where
  offensiveNames = filter (any (not . printable)) allTraceNames
  emptyScopes = filter (\nm -> ".." `isInfixOf` ('.' : nm <> ".")) allTraceNames

  {--------------------------
    SPLIT TRACES INTO PARTS
  --------------------------}
  -- (slightly more complex to cut out zero-width traces while keeping them for the signal declarations, so we can add comments)

  allLabels = concatMap (\s -> map (snoc s) alphabet) ("": labels)
    where alphabet = map chr [33..126]

  allTraces = L.map second clockTrace <> L.toList traceMap
  (allNames, allTraces) = L.unzip allTraces

  nonUnitTraces = filter (\(_,(_,_,w,_)) = w>0) allTraces

  (labels,names,periods,widths,valuess) = L.unzip5
    $ L.map (\(l,(n,(_t,p,w,vs))) -> (l,n,p,w,vs))
    $ filter (\(_,(_,(_,_,w,_))) = w>0)
    $ L.zip allLabels allTraces

  {--------------------------
    VCD FILE CONSTRUCTION
  --------------------------}

  headers =
    [ Date $ unsafePerformIO getCurrentTime
    , Version $ Data.Version.showVersion Paths_clash_prelude.version
    , TimeScale timeScale
    ]

  simulation =
    [ SimulationTime vStart
    , DumpVars initialValues
    ] -- use dumpvars only for reset values? the values at the start?
    <> blocks
    <> [SimulationTime vStop]

  {--------------------------
    SIGNAL DECLARATIONS
  --------------------------}

  variables = mergeScopes varScopes []
   where
    varScopes = L.map (\(n,w,l) -> mkScope ("logic" : split "." n) w l)
      $ L.sort
      $ L.zip3 names widths labels

  -- essentially x:xs
  addScope :: DeclarationCommand -> [DeclarationCommand] -> [DeclarationCommand]
  addScope (Scope n xs) (Scope m ys:l)  | n == m = Scope m (mergeScopes xs ys) : l
  addScope s l = s:l

  -- essentially xs <> ys
  mergeScopes :: [DeclartionCommand] -> [DeclarationCommand]
  mergeScopes [] ys = ys
  mergeScopes (x:xs) ys = addScope x $ mergeScopes xs ys

  mkScope :: [String] -> Width -> IDCode -> DeclarationCommand
  mkScope [] _ _ = error "empty signal name"
  mkScope [varReference] varSize varIDCode = VarDef $ Var{varSize, varIDCode, varReference}
  mkScope (n:ns) w l = Scope n [mkScope ns w l]

  {--------------------------
    TIME UNITS
  --------------------------}

  {-

  - gather all periods, take the GCD (in FS). This is the *simulation time scale*
    and starts at the clock start. This is essentially the clock cycle index of the
    GCD'd clock.

  - from the simulation time scale, start/stop time, and clock start time, compute the
    GCD. Then, take the highest power of 10 that fits inside this value. This
    is the *VCD time*, which is used in VCD timestamps.

  -}

  simTimeScaleFS = foldl1 gcd L.map timeInFS periods

  vcdTimeScaleFS = largestPow10 1
    $ foldl1 gcd
    $ simTimeFS : L.map timeInFS [ start
                                 , absTime start stop
                                 , absTime start clockStart ]
   where
    largestPow10 p x | x>=10*p = largestPos10 (p*10) x
                     | otherwise = p

  timeScale = timeScale' vcdTimeScaleFS "fs" ["ps","ns","us","ms","s"]
   where
    timeScale' t u (u':r) | t>=1000 = timeScale' (t `div` 1000) u' r
                          | otherwise = (t,u)

  timeOffset = if shiftToZero then timeInFS (-start) `div` vcdTimeScaleFS else 0 :: VcdTime
  timeMult = simTimeScaleFS `div` vcdTimeScaleFS :: VcdTime

  vClockStart = timeInFS clockStart `div` vcdTimeScaleFS :: VcdTime
  vStart = timeInFS start `div` vcdTimeScaleFS + timeOffset :: VcdTime
  vStop = timeInFS stop `div` vcdTimeScaleFS + timeOffset :: VcdTime

  simTimeToVcd x = x*timeMult + timeOffset + vClockStart

  {--------------------------
    SIGNAL VALUES
  --------------------------}

  {-
  - group signals by period
  - per domain, slice:
    - slice based on time
    - take the initial samples
    - compare samples to only get changes
  -}

  domains :: [(VCDTime,([Width],[String],[[Value]]))]
  domains = map (\traces -> (fst $ head traces, unzip3 $ map (snd) traces)) $ groupWith fst $ zip periods $ zip3 widths labels valuess
  changesPerDomain = map mkDomain domains
  initials = concatMap fst changesPerDomain
  bodyParts = concatMap (\(t,v) -> [SimulationTime t, ValueChanges v]) $
              foldl1 zipTimed $
              map snd changesPerDomain


  ...




-- | Create a VCD file for the given traces and simulation configuration.
-- Zero-width signals are dropped.
vcdText ::
  Simulation ->
  Either String Text
vcdText sim = renderVcd <$> vcd sim

-- | Create a VCD file for the given traces and simulation configuration,
-- and write it to a file.
-- Errors if the VCD generation fails.
writeVcd ::
  FilePath ->
  Simulation ->
  IO ()
writeVcd file sim = do
  text <- assertRight $ vcdText sim
  writeFile file text

-- | Render a 'Vcd' object as 'Text'.
renderVcd ::
  VcdFile decl sim ->
  Text
renderVcd = Text.unlines $ L.map renderDecl decl <> L.map renderSim sim

-- | Render a VCD 'DeclarationCommand' as 'Text'.
renderDecl ::
  DeclarationCommand ->
  Text
renderDecl = \case
  Version v ->
    vcdCommand "version" [v]
  Date t ->
    vcdCommand "date" [Text.pack $ iso8601Format t]
  TimeScale (x,unit) ->
    vcdCommand "timescale" [Text.show x, Text.pack unit]
  VarDec Var{varSize, varIDCode, varReference} ->
    vcdCommand "var" ["wire", Text.show varSize, Text.pack varIDCode, Text.pack varReference]
  Scope name subs -> Text.unlines
    vcdCommand "scope" ["module", Text.pack name]
    : L.map renderDecl subs
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
        : L.map renderChange ch
        <> ["$end"]
  ValueChanges ch ->
    Text.unlines $ L.map renderChange ch

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
        'b'
    :  shorten (L.map digit (reverse [0 .. changeSize - 1]))
    <> ' ':changeIDCode
 where
  (mask, val) = changeValue
  digit d = case (testBit mask d, testBit val d) of
    (False,False) -> '0'
    (False,True)  -> '1'
    (True,_)      -> 'x'
  shorten (a:rest@(b:c)) | a == b = shorten rest
  shorten a = a

-- | Create a VCD command of the form @$<tag> <contents> $end@.
vcdCommand ::
  Text ->
  [Text] ->
  Text
vcdCommand tag contents = Text.unwords ("$"<>tag):(contents<>["$end"])

-- | Format the time according to iso8601: @<yyyy>-<mm>-<dd>T<hh>:<mm>:<ss>@.
iso8601Format ::
  UTCTime ->
  String
iso8601Format = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
