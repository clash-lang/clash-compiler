{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Signal.Internal.Trace.Vcd (
  VcdFile(..),
  DeclarationCommand(..), SimulationCommand(..),
  Var(..),
  ValueChange(..),
  Scope, IDCode, VcdTime, TimeUnit, Value,
  vcd,
  vcdText,
  writeVcd,
  renderVcd, renderDecl, renderSim, renderChange,
  vcdCommand,
  iso8601Format,
  printable,
  -- * 'vcd' internals for custom implementations
  findChangesSimple,
  combineChangesPerDom,
  zipTimed, treeFold,
) where

import Control.Exception.Extra (errorIO)
import Data.Char (chr, ord)
import Data.Bifunctor (first)
import Data.Bits (testBit)
import Data.List
  (zip4, unzip5, isInfixOf, partition, transpose, uncons)
import Data.List.Extra (snoc)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Version
import GHC.Stack (HasCallStack, withFrozenCallStack)
import System.IO (hPutStrLn, stderr)

import Clash.Signal.Internal.Trace
import Clash.Signal.Internal.Trace.VcdTypes
import Clash.Time (Time(..), timeInFS, absTime, splitTimeUnit)


import qualified Paths_clash_prelude

-- | A complete VCD file.
-- This includes the contents of the VCD file.
data VcdFile t = VcdFile
  { declationCommands :: [DeclarationCommand]
  , simulationCommands :: [SimulationCommand]
  , meta :: VcdMeta t
  }
deriving instance (Show (VcdMeta t)) => Show (VcdFile t)

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

  -- essentially x:xs
  addScope :: DeclarationCommand -> [DeclarationCommand] -> [DeclarationCommand]
  addScope (Scope n xs) (Scope m ys:l)  | n == m = Scope m (mergeScopes xs ys) : l
  addScope s l = s:l

  -- essentially xs <> ys
  mergeScopes :: [DeclarationCommand] -> [DeclarationCommand] -> [DeclarationCommand]
  mergeScopes [] ys = ys
  mergeScopes (x:xs) ys = addScope x $ mergeScopes xs ys

  mkScope :: [String] -> Width -> IDCode -> DeclarationCommand
  mkScope [] _ _ = error "(unreachable) empty signal name"
  mkScope [varReference] 0 _ = Comment $ "Omitted 0-bit trace " <> varReference
  mkScope [varReference] varSize varIDCode = VarDec $ Var{..}
  mkScope (n:ns) w l = Scope n [mkScope ns w l]

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

  processed = vcdProcess @t
    vcdTimeScaleFS
    (vStart,vClockStart,vStop)
    (map (\(l,(n,t))->(l,n,t)) nonUnitTraces) --labels names periods widths valuess metas


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

-- | Check whether a character is a simple printable (visible) ASCII character.
printable :: Char -> Bool
printable (ord -> c) = 33 <= c && c <= 126
