

module Clash.Simulation.Vcd (vcdText, writeVcd) where

import Clash.Simulation



data VcdFile = VCDFile [DeclarationCommand] [SimulationCommand]
  deriving (Show)

data DeclarationCommand
  = Version String
  | Date UTCTime
  | TimeScale Int TimeUnit
  | VarDec Var
  | Scope Scope [DeclarationCommand]
  | Comment String
  deriving (Show)

data Var
  = Var
  { varSize   :: Width
  , varIDCode :: IDCode
  , varReference    :: String
  }
  deriving (Show)

type IDCode = String

type Scope = String
type VCDTime = Integer
data SimulationCommand
  = SimulationTime VCDTime
  | DumpVars [ValueChange]
  | ValueChanges [ValueChange]
  deriving (Show)

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
      , traces
      }
  | stop < start =
      Left $ "VCD: stop was " <> show start <> ", which is earlier than start (" <> show startPs <> ")."
  | start < 0 && shiftToZero == False =
      Left $ "VCD: Start time was " <> show start <> ", but cannot be negative without shifting the start to 0."
  | null traces =
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
  offensiveNames = filter (any (not . printable)) traceNames
  emptyScopes = filter (\nm -> ".." `isInfixOf` ('.' : nm <> ".")) traceNames

  -- construction

  headers =
    [ Date ...
    , Version ...
    , TimeScale ...
    ]

  variables = mkScope "logic" (zip3 traceNames labels widths)

  simulation =
    [ SimulationTime vStart
    , DumpVars initialValues
    ] -- use dumpvars only for reset values? the values at the start?
    <> blocks
    <> [SimulationTime vStop]

  -- time

  vStart = ... -- in VcdTime units
  vStop = ...

  -- slicing

  initialValues = ...

  ...





-- | Render a 'Vcd' object as 'Text'.
renderVcd ::
  VcdFile ->
  Text
renderVcd = ...


-- | Create a VCD file for the given traces and simulation configuration.
-- Zero-width signals are dropped.
vcdText ::
  Simulation ->
  Either String Text
vcdText = renderVcd <$> vcd

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
