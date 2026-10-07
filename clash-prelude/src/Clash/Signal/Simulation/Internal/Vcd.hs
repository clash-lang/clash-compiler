
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Signal.Simulation.Internal.Vcd (
  VcdFile(..),
  DeclarationCommand(..), SimulationCommand(..),
  Var(..),
  ValueChange(..),
  Scope, IDCode, VcdTime, TimeUnit, Value, TimeScale,
  vcd,
  vcdText,
  writeVcd,
  renderVcd, renderDecl, renderSim, renderChange,
  vcdCommand,
  iso8601Format,
) where

import Control.Exception.Extra (errorIO)
import Data.Char (chr, ord)
import Data.Bits (testBit)
import qualified Data.List as L
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
import GHC.Exts (groupWith)
import GHC.Stack (HasCallStack, withFrozenCallStack)

import           Clash.Signal.Simulation
import           Clash.Time            (timeInFS, absTime)

import qualified Paths_clash_prelude

-- | A complete VCD file.
data VcdFile = VcdFile [DeclarationCommand] [SimulationCommand]
  deriving (Show)

-- | A VCD declaration command.
data DeclarationCommand
  = Version String                   -- ^ The (Clash) version used to generate the VCD.
  | Date UTCTime                     -- ^ The timestamp the VCD was created.
  | TimeScale TimeScale              -- ^ The unit of the VCD time stamps.
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
type TimeScale = (Integer, TimeUnit)

-- | Create a 'Vcd' object from a 'Simulation'.
vcd ::
  Simulation ->
  Either String VcdFile
vcd Simulation
      { simConfig = Config
                  { start, stop, clockStart
                  , shiftToZero
                  --, statusMsgs
                  --, warnZeroWidth
                  }
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
  | null names
  = Left "VCD: No traces found."
  | (nm:_) <- offensiveNames
  = Left $
         "Trace '"
      <> nm
      <> "' contains non-printable ASCII characters, which is not supported by VCD."
  | (nm:_) <- emptyScopes
  = Left $
         "Trace '"
      <> nm
      <> "' contains empty scope names, which is not supported by VCD."
  | otherwise
  = Right $ VcdFile (headers ++ variables) simulation
 where
  offensiveNames = filter (any (not . printable)) allNames
  emptyScopes = filter (\nm -> ".." `L.isInfixOf` ('.' : nm <> ".")) allNames

  ----------------------------------------------
  --         SPLIT TRACES INTO PARTS          --
  ----------------------------------------------

  -- (slightly more complex so we can cut out zero-width traces
  -- while keeping them for the signal declarations, so we can add comments)
  allLabels = concatMap (\s -> map (snoc s) alphabet) ("": labels)
    where alphabet = map chr [33..126]

  allTraces = Map.toList traceMap
  (allNames, _) = L.unzip allTraces

  nonUnitTraces = filter (\(_,(_,(_,_,w,_))) -> w>0) $ zip allLabels allTraces -- TODO: Use warnZeroWidth here?

  (labels,names,periods,widths,valuess) = L.unzip5
    $ L.map (\(l,(n,(_t,p,w,vs))) -> (l,n,p,w,vs)) nonUnitTraces

  ----------------------------------------------
  --          VCD FILE CONSTRUCTION           --
  ----------------------------------------------

  headers =
    [ Date simTimestamp
    , Version $ Data.Version.showVersion Paths_clash_prelude.version
    , TimeScale timeScale
    ]

  simulation =
    [ SimulationTime vStart
    , DumpVars initialValues
    ] -- use dumpvars only for reset values? the values at the start?
    <> changes
    <> [SimulationTime vStop]

  ----------------------------------------------
  --           SIGNAL DECLARATIONS            --
  ----------------------------------------------

  variables = mergeScopes varScopes []
   where
    varScopes = L.map (\(n,w,l) -> mkScope (splitOn "." n) w l)
      $ L.sort
      $ L.zip3 names widths labels

  -- essentially x:xs
  addScope :: DeclarationCommand -> [DeclarationCommand] -> [DeclarationCommand]
  addScope (Scope n xs) (Scope m ys:l)  | n == m = Scope m (mergeScopes xs ys) : l
  addScope s l = s:l

  -- essentially xs <> ys
  mergeScopes :: [DeclarationCommand] -> [DeclarationCommand] -> [DeclarationCommand]
  mergeScopes [] ys = ys
  mergeScopes (x:xs) ys = addScope x $ mergeScopes xs ys

  mkScope :: [String] -> Width -> IDCode -> DeclarationCommand
  mkScope [] _ _ = error "empty signal name"
  mkScope [varReference] varSize varIDCode = VarDec $ Var{..}
  mkScope (n:ns) w l = Scope n [mkScope ns w l]

  ----------------------------------------------
  --               TIME UNITS                 --
  ----------------------------------------------

  {-

  - gather all periods, take the GCD (in FS). This is the *simulation time scale*
    and starts at the clock start. This is essentially the clock cycle index of the
    GCD'd clock.

  - from the simulation time scale, start/stop time, and clock start time, compute the
    GCD. Then, take the highest power of 10 that fits inside this value. This
    is the *VCD time*, which is used in VCD timestamps.

  -}

  simTimeScaleFS = foldl1 gcd $ L.map timeInFS periods

  vcdTimeScaleFS = largestPow10 1
    $ foldl1 gcd
    $ simTimeScaleFS : L.map timeInFS [ start
                                      , absTime start stop
                                      , clockStart ]
   where
    largestPow10 p x | x>=10*p = largestPow10 (p*10) x
                     | otherwise = p

  timeScale = timeScale0 vcdTimeScaleFS "fs" ["ps","ns","us","ms","s"]
   where
    timeScale0 t u [] = (t,u)
    timeScale0 t u (u0:r) | t>=1000 = timeScale0 (t `div` 1000) u0 r
                          | otherwise = (t,u)

  timeOffset =
    if shiftToZero
    then timeInFS (-start) `div` vcdTimeScaleFS
    else 0 :: VcdTime

  vClockStart = timeInFS clockStart `div` vcdTimeScaleFS :: VcdTime
  vStart = timeInFS start `div` vcdTimeScaleFS + timeOffset :: VcdTime
  vStop = timeInFS (absTime start stop) `div` vcdTimeScaleFS + timeOffset :: VcdTime

  -- simTimeToVcd x = x*timeMult + timeOffset + vClockStart

  ----------------------------------------------
  --              SIGNAL VALUES               --
  ----------------------------------------------

  {-
  - group signals by period
  - per domain, slice:
    - slice based on time
    - take the initial samples
    - compare samples to only get changes
  -}

  -- [(period in VCDTime,([widths))]
  domains :: [(Period,([Width],[IDCode],[[Value]]))]
  domains =
    map
      (\traces -> (fst $ unsafeHead traces, unzip3 $ map (snd) traces))
      $ groupWith fst
      $ zip periods
      $ zip3 widths labels valuess

  unsafeHead (x:_) = x
  unsafeHead [] = error "Cannot take head of empty list"

  changesPerDomain :: [([ValueChange],[(VcdTime,[ValueChange])])]
  changesPerDomain = L.map findChanges domains

  initialValues :: [ValueChange]
  initialValues = L.concatMap fst changesPerDomain

  -- essentially merge sort with nested merging on equal values
  blocks :: [(VcdTime, [ValueChange])]
  blocks =
    treeFold zipTimed
      $ (L.map snd changesPerDomain :: [[(VcdTime,[ValueChange])]])

  changes :: [SimulationCommand]
  changes = concatMap (\(t,cs) -> [SimulationTime t, ValueChanges cs]) blocks

  -- TODO: small to large merging is better, but this is likely sufficient

  -- | Merge items with the given function using a binary tree.
  -- A linear fold would incur quadratic runtime.
  treeFold :: (a -> a -> a) -> [a] -> a
  treeFold f0 xs0 = treeFold0 (length xs0) f0 xs0
   where
    treeFold0 _ _ []  = error "treeFold used on empty sequence"
    treeFold0 _ _ [x] = x
    treeFold0 l f xs  = treeFold0 l0 f as `f` treeFold0 (l - l0) f bs
     where
      l0 = l `div` 2
      (as, bs) = L.splitAt l0 xs

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

  -- Find changes in a list of signals with equal period
  findChanges
    :: (Period,([Width],[String],[[Value]]))
    -> ([ValueChange],[(VcdTime,[ValueChange])])
  findChanges (period,(widths0,labels0,valuess0)) = (initial0, samples0)
    where
      clkEdges :: [VcdTime]
      clkEdges = [vClockStart, vClockStart + period0 ..]
       where
        period0 = timeInFS period `div` vcdTimeScaleFS

      -- group by clock cycle and add timestamps (the initial value starts at -inf)
      valuessFrom :: [(VcdTime,[Value])]
      valuessFrom = zip (-1000000000000:clkEdges) $ L.transpose valuess0
        --TODO: minBound, but we're using integers, which are unbounded :/

      skipStart :: [(VcdTime,[Value])]
      skipStart = map fst $ dropWhile ((<= vStart) . snd) $ zip valuessFrom clkEdges

      ((_,initial) , rest) = fromMaybe (error "Finite signal") $ L.uncons skipStart

      samples :: [(VcdTime,[Value])]
      samples = takeWhile ((< vStop) . fst) rest

      filterChanges (_ta,va) (tb,vb) =
        (tb
        , [ValueChange w l b | (a,b,w,l) <- L.zip4 va vb widths0 labels0, a /= b]
        )

      initial0 :: [ValueChange]
      initial0 = zipWith3 ValueChange widths0 labels0 initial

      samples0 :: [(VcdTime,[ValueChange])]
      samples0 = filter (not . null . snd) $ zipWith filterChanges skipStart samples


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
  Data.Text.IO.writeFile file text
 where
  assertRight :: -- TODO; duplicate of the one in Prelude :(
    forall a b.
    HasCallStack =>
    Show a =>
    Either a b ->
    IO b
  assertRight = either err pure
   where
    err a =
      withFrozenCallStack
        $ errorIO
        $ "assertRight: expected a Right value, given " <> show (Left a :: Either a a)

-- | Render a 'Vcd' object as 'Text'.
renderVcd ::
  VcdFile ->
  Text
renderVcd (VcdFile decl sim) = Text.unlines
  $  L.map renderDecl decl
  <> ["$enddefinitions $end\n"]
  <> L.map renderSim sim

-- | Render a VCD 'DeclarationCommand' as 'Text'.
renderDecl ::
  DeclarationCommand ->
  Text
renderDecl = \case
  Version v ->
    vcdCommand "version" [Text.pack v]
  Date t ->
    vcdCommand "date" [Text.pack $ iso8601Format t]
  TimeScale (x,unit) ->
    vcdCommand "timescale" [Text.show x, Text.pack unit]
  VarDec Var{varSize, varIDCode, varReference} ->
    vcdCommand "var"
      [ "wire"
      , Text.show varSize
      , Text.pack varIDCode
      , Text.pack varReference ]
  Scope name subs -> Text.unlines $ -- unlines introduces a trailing newline, but oh well
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
       ('b' : shorten (L.map digit $ reverse [0 .. changeSize - 1]))
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
vcdCommand tag contents = Text.unwords $ ("$"<>tag):(contents<>["$end"])

-- | Format the time according to iso8601: @<yyyy>-<mm>-<dd>T<hh>:<mm>:<ss>@.
iso8601Format ::
  UTCTime ->
  String
iso8601Format = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

-- | Check whether a character is a simple printable (visible) ASCII character.
printable :: Char -> Bool
printable (ord -> c) = 33 <= c && c <= 126
