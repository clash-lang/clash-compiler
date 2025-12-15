{-|
Copyright  :  (C) 2018, Google Inc.
                  2019, Myrtle Software Ltd
                  2022-2025, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Utilities for tracing signals and dumping them in various ways. Example usage:

@
import Clash.Prelude hiding (writeFile)
import Data.Text.IO  (writeFile)

-- | Count and wrap around
subCounter :: SystemClockResetEnable => Signal System (Index 3)
subCounter = traceSignal1 "sub" counter
  where
    counter =
      register 0 (fmap succ' counter)

    succ' c
      | c == maxBound = 0
      | otherwise     = c + 1

-- | Count, but only when my subcounter is wrapping around
mainCounter :: SystemClockResetEnable => Signal System (Signed 64)
mainCounter = traceSignal1 "main" counter
  where
    counter =
      register 0 (fmap succ' $ bundle (subCounter,counter))

    succ' (sc, c)
      | sc == maxBound = c + 1
      | otherwise      = c

-- | Collect traces, and dump them to a VCD file.
main :: IO ()
main = do
  let cntrOut = exposeClockResetEnable mainCounter systemClockGen systemResetGen enableGen
  vcd <- dumpVCD (0, 100) cntrOut ["main", "sub"]
  case vcd of
    Left msg ->
      error msg
    Right contents ->
      writeFile "mainCounter.vcd" contents
@
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

module Clash.Signal.Trace
  (
  -- * Tracing functions
  -- ** Simple
    traceSignal1
  , traceVecSignal1
  -- ** Tracing in a multi-clock environment
  , traceSignal
  , traceVecSignal

  -- * VCD dump functions
  , dumpVCD
  , advancedDumpVCD
  , advancedDumpVCDWindow

  -- * Replay functions
  , dumpReplayable
  , replay

  -- * Internal
  -- ** Types
  , Period
  , Changed
  , Value
  , Width
  , TraceMap
  , TypeRepBS
  , Time
  -- ** Functions
  , timePs, timeNs, timeUs, timeMs, timeToPs, clkCycles, tracedCycles
  , traceSignal#
  , traceVecSignal#
  , dumpVCD#
  , dumpVCD##
  , waitForTraces#
  , traceMap#
  ) where

-- Clash:
import           Clash.Annotations.Primitive (hasBlackBox)
import           Clash.Signal.Internal (fromList)
import           Clash.Signal
  (KnownDomain(..), SDomainConfiguration(..), Signal, Domain, bundle, unbundle, clockPeriod)
import           Clash.Sized.Vector    (Vec, iterateI)
import qualified Clash.Sized.Vector    as Vector
import           Clash.Class.BitPack   (BitPack, BitSize, pack, unpack)
import           Clash.Promoted.Nat    (snatToNum, SNat(..))
import           Clash.Signal.Internal (Signal ((:-)), sample)
import           Clash.XException      (deepseqX, NFDataX)
import           Clash.Sized.Internal.BitVector
  (BitVector(BV))

-- Haskell / GHC:
import           Control.Monad         (foldM)
import           Data.Bits             (testBit)
import           Data.Binary           (encode, decodeOrFail)
import           Data.ByteString.Lazy  (ByteString)
import qualified Data.ByteString.Lazy  as ByteStringLazy
import           Data.Int              (Int64)
import           Data.Char             (ord, chr)
import           Data.IORef
  (IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef)
#if !MIN_VERSION_base(4,20,0)
import           Data.List             (foldl')
#endif
import           Data.List             (foldl1', unzip4, transpose, uncons)
import           Data.List.Extra       (snoc)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (fromMaybe, catMaybes)
import qualified Data.Text             as Text
import           Data.Time.Clock       (UTCTime, getCurrentTime)
import           Data.Time.Format      (formatTime, defaultTimeLocale)
import           GHC.Natural           (Natural)
import           GHC.Stack             (HasCallStack)
import           GHC.TypeLits          (KnownNat, type (+))
import           System.IO.Unsafe      (unsafePerformIO)
import           Type.Reflection       (Typeable, TypeRep, typeRep)

#ifdef CABAL
import qualified Data.Version
import qualified Paths_clash_prelude
#endif

type Period   = Int64
type Changed  = Bool
type Value    = (Natural, Natural) -- (Mask, Value)
type Width    = Int

-- | Serialized TypeRep we need to store for dumpReplayable / replay
type TypeRepBS = ByteString

type TraceMap  = Map.Map String (TypeRepBS, Period, Width, [Value])

-- | Map of traces used by the non-internal trace and dumpvcd functions.
traceMap# :: IORef TraceMap
traceMap# = unsafePerformIO (newIORef Map.empty)
{-# OPAQUE traceMap# #-}

mkTrace
  :: HasCallStack
  => BitPack a
  => NFDataX a
  => Signal dom a
  -> [Value]
mkTrace signal = sample (unsafeToTup . pack <$> signal)
 where
  unsafeToTup (BV mask value) = (mask, value)

-- | Trace a single signal. Will emit an error if a signal with the same name
-- was previously registered.
traceSignal#
  :: forall dom a
   . ( BitPack a
     , NFDataX a
     , Typeable a )
  => IORef TraceMap
  -- ^ Map to store the trace
  -> Int64
  -- ^ The associated clock period for the trace
  -> String
  -- ^ Name of signal in the VCD output
  -> Signal dom a
  -- ^ Signal to trace
  -> IO (Signal dom a)
traceSignal# traceMap period traceName signal =
  atomicModifyIORef' traceMap $ \m ->
    if Map.member traceName m then
      error $ "Already tracing a signal with the name: '" ++ traceName ++ "'."
    else
      ( Map.insert
          traceName
          ( encode (typeRep @a)
          , period
          , width
          , mkTrace signal)
          m
      , signal)
 where
  width = snatToNum (SNat @(BitSize a))
{-# OPAQUE traceSignal# #-}

-- | Trace a single vector signal: each element in the vector will show up as
-- a different trace. If the trace name already exists, this function will emit
-- an error.
traceVecSignal#
  :: forall dom n a
   . ( KnownNat n
     , BitPack a
     , NFDataX a
     , Typeable a )
  => IORef TraceMap
  -- ^ Map to store the traces
  -> Int64
  -- ^ Associated clock period for the trace
  -> String
  -- ^ Name of signal in the VCD output. Will be appended by _0, _1, ..., _n.
  -> Signal dom (Vec (n+1) a)
  -- ^ Signal to trace
  -> IO (Signal dom (Vec (n+1) a))
traceVecSignal# traceMap period vecTraceName (unbundle -> vecSignal) =
  fmap bundle . sequenceA $
    Vector.zipWith trace' (iterateI succ (0 :: Int)) vecSignal
 where
  trace' i s = traceSignal# traceMap period (name' i) s
  name' i    = vecTraceName ++ "_" ++ show i
{-# OPAQUE traceVecSignal# #-}

-- | Trace a single signal. Will emit an error if a signal with the same name
-- was previously registered.
--
-- __NB__: Works correctly when creating VCD files from traced signal in
-- multi-clock circuits. However 'traceSignal1' might be more convenient to
-- use when the domain of your circuit is polymorphic.
traceSignal
  :: forall dom  a
   . ( KnownDomain dom
     , BitPack a
     , NFDataX a
     , Typeable a )
  => String
  -- ^ Name of signal in the VCD output
  -> Signal dom a
  -- ^ Signal to trace
  -> Signal dom a
traceSignal traceName signal =
  case knownDomain @dom of
    SDomainConfiguration{sPeriod} ->
      unsafePerformIO $
        traceSignal# traceMap# (snatToNum sPeriod) traceName signal
{-# OPAQUE traceSignal #-}
{-# ANN traceSignal hasBlackBox #-}

-- | Trace a single signal. Will emit an error if a signal with the same name
-- was previously registered.
--
-- __NB__: Associates the traced signal with a clock period of /1/, which
-- results in incorrect VCD files when working with circuits that have
-- multiple clocks. Use 'traceSignal' when working with circuits that have
-- multiple clocks.
traceSignal1
  :: ( BitPack a
     , NFDataX a
     , Typeable a )
  => String
  -- ^ Name of signal in the VCD output
  -> Signal dom a
  -- ^ Signal to trace
  -> Signal dom a
traceSignal1 traceName signal =
  unsafePerformIO (traceSignal# traceMap# 1 traceName signal)
{-# OPAQUE traceSignal1 #-}
{-# ANN traceSignal1 hasBlackBox #-}

-- | Trace a single vector signal: each element in the vector will show up as
-- a different trace. If the trace name already exists, this function will emit
-- an error.
--
-- __NB__: Works correctly when creating VCD files from traced signal in
-- multi-clock circuits. However 'traceSignal1' might be more convenient to
-- use when the domain of your circuit is polymorphic.
traceVecSignal
  :: forall dom a  n
   . ( KnownDomain dom
     , KnownNat n
     , BitPack a
     , NFDataX a
     , Typeable a )
  => String
  -- ^ Name of signal in debugging output. Will be appended by _0, _1, ..., _n.
  -> Signal dom (Vec (n+1) a)
  -- ^ Signal to trace
  -> Signal dom (Vec (n+1) a)
traceVecSignal traceName signal =
  case knownDomain @dom of
    SDomainConfiguration{sPeriod} ->
      unsafePerformIO $
        traceVecSignal# traceMap# (snatToNum sPeriod) traceName signal
{-# OPAQUE traceVecSignal #-}
{-# ANN traceVecSignal hasBlackBox #-}

-- | Trace a single vector signal: each element in the vector will show up as
-- a different trace. If the trace name already exists, this function will emit
-- an error.
--
-- __NB__: Associates the traced signal with a clock period of /1/, which
-- results in incorrect VCD files when working with circuits that have
-- multiple clocks. Use 'traceSignal' when working with circuits that have
-- multiple clocks.
traceVecSignal1
  :: ( KnownNat n
     , BitPack a
     , NFDataX a
     , Typeable a )
  => String
  -- ^ Name of signal in debugging output. Will be appended by _0, _1, ..., _n.
  -> Signal dom (Vec (n+1) a)
  -- ^ Signal to trace
  -> Signal dom (Vec (n+1) a)
traceVecSignal1 traceName signal =
  unsafePerformIO $ traceVecSignal# traceMap# 1 traceName signal
{-# OPAQUE traceVecSignal1 #-}
{-# ANN traceVecSignal1 hasBlackBox #-}

iso8601Format :: UTCTime -> String
iso8601Format = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

toPeriodMap :: TraceMap -> Map.Map Period [(String, Width, [Value])]
toPeriodMap m = foldl' go Map.empty (Map.assocs m)
  where
    go periodMap (traceName, (_rep, period, width, values)) =
      Map.alter (Just . go') period periodMap
        where
          go' = ((traceName, width, values):) . (fromMaybe [])

flattenMap :: Map.Map a [b] -> [(a, b)]
flattenMap m = concat [[(a, b) | b <- bs] | (a, bs) <- Map.assocs m]

printable :: Char -> Bool
printable (ord -> c) = 33 <= c && c <= 126

-- | Data type for time slicing
data Time (dom::Domain) = Time{timeInPs::Int64} deriving (Eq)
instance Show (Time dom) where
  show Time{timeInPs} = show timeInPs ++ "ps"

instance KnownDomain dom => Num (Time dom) where
  (+) Time{timeInPs=a} Time{timeInPs=b} = Time (a+b)
  (-) Time{timeInPs=a} Time{timeInPs=b} = Time (a-b)
  abs = Time . abs . timeInPs
  fromInteger = Time . (* clockPeriod' @dom) . fromInteger
  (*) _ _ = error "Cannot multiply time values"
  signum _ = error "Cannot turn time into unitless value"

-- | Get the clock period of a domain as a 'Period' value.
clockPeriod' :: forall dom. KnownDomain dom => Int64
clockPeriod' = fromInteger . snatToNum $ clockPeriod @dom

instance Ord (Time dom) where
  (<=) (Time a) (Time b) = a<=b

-- | Time in picoseconds
timePs :: forall dom. Int64 -> Time dom
timePs = Time
-- | Time in nanoseconds
timeNs :: forall dom. Int64 -> Time dom
timeNs = Time . (*1000)
-- | Time in microseconds
timeUs :: forall dom. Int64 -> Time dom
timeUs = Time . (*1000_000)
-- | Time in milliseconds
timeMs :: forall dom. Int64 -> Time dom
timeMs = Time . (*1000_000_000)
-- | Time in clock cycles of the specified domain, i.e. `clkCycles @DomB 5`
clkCycles :: forall dom1 dom2. KnownDomain dom1 => Int64 -> Time dom2
clkCycles = Time . (* clockPeriod' @dom1)
-- | Time in clock cycles in the same domain as the provided signal.
-- This is the same as using number literals.
tracedCycles :: forall dom. KnownDomain dom => Int64 -> Time dom
tracedCycles = Time . (* clockPeriod' @dom)
-- | Get the number of picoseconds.
timeToPs :: forall dom. Time dom -> Int64
timeToPs (Time ps) = ps

-- | Same as @dumpVCD@, but supplied with a custom tracemap and a custom timestamp
dumpVCD##
  :: (Int64,Int64)
  -- ^ [start, stop) in ps
  -> Int64
  -- ^ clock start time in ps (can be negative!)
  -> [(String,Period)]
  -- ^ clock signals as (name, period in ps)
  -> TraceMap --name::String -> (TypeRepBS, Period, Width, [Value])
  -> UTCTime
  -> Either String Text.Text
dumpVCD## (startPs, stopPs) clkStartPs clockWaves traceMap now
  | startPs < 0 =
      error $ "dumpVCD: start was " ++ show startPs ++ "ps, but cannot be negative."
  | stopPs < startPs =
      error $ "dumpVCD: stop was " ++ show stopPs ++ "ps, which is earlier than start (" ++ show startPs ++ "ps)."
  | null traceMap =
      error $ "dumpVCD: no traces found. Extend the given trace names."
  | (nm:_) <- offensiveNames =
      Left $ unwords [ "Trace '" ++ nm ++ "' contains"
                     , "non-printable ASCII characters, which is not"
                     , "supported by VCD." ]
  | otherwise =
      Right $ Text.unlines [ Text.unwords headerDate
                           , Text.unwords headerVersion
                           , Text.unwords headerComment
                           , Text.pack $ unwords headerTimescale
                           , "$scope module logic $end"
                           , Text.intercalate "\n" headerWires
                           , "$upscope $end"
                           , "$enddefinitions $end"
                           , Text.pack ('#':show start)
                           , "$dumpvars"
                           , Text.intercalate "\n" $ map Text.pack inits
                           , "$end"
                           , Text.intercalate "\n" $ bodyParts
                           ]
 where
  offensiveNames = filter (any (not . printable)) traceNames

  -- Generate labels
  labels = concatMap (\s -> map (snoc s) alphabet) ([]: labels)
   where
    alphabet = map chr [33..126]

  -- PREPROCESSING (mostly time related)
  startFs = 1000*startPs
  stopFs = 1000*stopPs
  clkStartFs = 1000*clkStartPs

  -- Individual lists to make everything easier
  -- Periods are in fs
  (traceNames, periodsFs, widths, valuess) =
    unzip4 $
      ( map
        (\(name, (_rep, period, width, values)) -> (name, 1000*period, width, values))
        (Map.toList traceMap) )
      ++
      ( map (\(name,period) -> (name,500*period,1,cycle [(0,0),(0,1)])) clockWaves)

  timescaleFs = 10^(leadingZeros $ reverse $ show timeGCD)
   where
    timeGCD = foldl1' gcd (startFs:stopFs:clkStartFs:periodsFs)
    leadingZeros ('0':r) = 1 + leadingZeros r
    leadingZeros _ = 0
  timescaleWithUnit = go timescaleFs "fs" ["ps","ns","us","ms","s"]
   where
    go x _ (u:us) | x>=1000 = go (x `div` 1000) u us
    go x u _ = show x ++ u

  start    = startFs    `div` timescaleFs
  stop     = stopFs     `div` timescaleFs
  clkStart = clkStartFs `div` timescaleFs
  periods = map (`div` timescaleFs) periodsFs

  -- from here on, all time units are in $timescale


  -- HEADER
  headerDate       = ["$date", Text.pack $ iso8601Format now, "$end"]

#ifdef CABAL
  clashVer         = Data.Version.showVersion Paths_clash_prelude.version
#else
  clashVer         = "development"
#endif
  headerVersion    = ["$version", "Generated by Clash", Text.pack clashVer , "$end"]
  headerComment    = ["$comment", "No comment", "$end"]
  headerTimescale  = ["$timescale", timescaleWithUnit, "$end"]
  headerWires      = [ Text.unwords $ headerWire w l n
                     | (w, l, n) <- (zip3 widths labels traceNames)]
  headerWire w l n = map Text.pack ["$var wire", show w, l, n, "$end"]

  -- FORMAT SIGNALS

  -- | Format single value according to VCD spec
  format :: Width -> String -> Value -> String
  format 1 label (0,0)   = '0': label ++ "\n"
  format 1 label (0,1)   = '1': label ++ "\n"
  format 1 label (1,_)   = 'x': label ++ "\n"
  format 1 label (mask,val) =
    error $ "Can't format 1 bit wide value for " ++ show label ++ ": value " ++ show val ++ " and mask " ++ show mask
  format n label (mask,val) =
    "b" ++ map digit (reverse [0..n-1]) ++ " " ++ label
    where
      digit d = case (testBit mask d, testBit val d) of
        (False,False) -> '0'
        (False,True)  -> '1'
        (True,_)      -> 'x'

  formatters = zipWith format widths labels

  {-
  1) Turn a signal into (from,value) tuples, bounded by START and STOP (end >= START, begin < STOP)
  2) Join blocks with the same values (i.e. make a difference list), only caring about the start time.
  3) Take the first value  as the initial value at START and the rest as value changes.
  -}

  -- Peter's rewrite, which is somehow rather slow when there aren't many changes?
  mkSignal values period fmt = (fmt $ snd initial, events)
   where
    clkEdges = [clkStart, clkStart + period ..]
    valuesEdges = zip (minBound : clkEdges) values
    skip = map fst $ dropWhile ((<= start) . snd) $ zip valuesEdges clkEdges
    (initial, rest) = fromMaybe (error "Finite signal") $ uncons skip
    samples = takeWhile ((< stop) . fst) rest

    events = catMaybes $ zipWith hasChange skip samples

    hasChange (_t1, v1) (t2, v2)
      | v1 /= v2 = Just (t2, fmt v2)
      | otherwise = Nothing

  -- -- uglier version of mkSignal - but it seems to be quite a bit faster when changes
  -- -- are sparse? what's up with that?
  -- mkSignal :: [Value] -> Int64 -> (Value->String) -> (String,[(Int64,String)])
  -- mkSignal values' period fmt = (initial, changes)
  --  where
  --   clkEdges = [clkStart,clkStart+period..]
  --   values = zip (-1:clkEdges) values' -- -1 is ugly but safe since start>0, even if clkStart < 0

  --   mkSlice :: [(Int64,Value)] -> [(Int64,Value)]
  --   mkSlice ((from,v):(rest@((to,_):_))) =
  --     if to < start then -- value ends before clock edge
  --       mkSlice rest
  --     else if from < stop then
  --       (from,v) : mkSlice rest
  --     else [] -- value starts after stop
  --   mkSlice _ = error "unreachable: finite signal" --just to get rid of warnings

  --   mkChanges :: [(Int64,Value)] -> [(Int64,String)]
  --   mkChanges ((t,v):(t',v'):rest) | v==v' = mkChanges ((t,v):rest)
  --   mkChanges ((t,v):rest) = (t,fmt v) : mkChanges rest
  --   mkChanges [] = []

  --   events = mkChanges $ mkSlice values
  --   initial = snd (head events)
  --   changes = tail events

  (inits,changess) = unzip $ zipWith3 mkSignal valuess periods formatters

  groupChanges :: [[(Int64,String)]] -> [(Int64,[String])]
  groupChanges [] = []
  groupChanges (sig:rest) = zipPrep sig $ groupChanges rest
   where
    zipPrep (aa@((ta,va):as)) (bb@((tb,vbs):bs)) =
      case compare ta tb of
        LT -> (ta,  [va]):zipPrep as bb
        EQ -> (ta,va:vbs):zipPrep as bs
        GT -> (tb,   vbs):zipPrep aa bs
    zipPrep [] bb = bb
    zipPrep aa [] = map (\(t,v)->(t,[v])) aa

  bodyParts = map bodyPart (groupChanges changess)

  bodyPart ((Text.pack . show -> t),vals) = Text.intercalate "\n" (pre:changeblock)
   where
    pre = Text.concat ["#",t,"\n"]
    changeblock = map Text.pack vals

-- | Same as @advancedDumpVCD@, but supplied with a custom tracemap
dumpVCD#
  :: NFDataX a
  => IORef TraceMap
  -- ^ Map with collected traces
  -> (Time dom, Time dom)
  -- ^ (offset, number of samples)
  -> Time dom
  -- ^ Clock start time
  -> Signal dom a
  -- ^ (One of) the output(s) the circuit containing the traces
  -> [(String,Period)]
  -- ^ Clock waves to generate
  -> [String]
  -- ^ The names of the traces you definitely want to be dumped to the VCD file
  -> IO (Either String Text.Text)
dumpVCD# traceMap (Time start, Time stop) (Time clkStart) signal clks traceNames = do
  waitForTraces# traceMap signal traceNames
  m <- readIORef traceMap
  fmap (dumpVCD## (start,stop) clkStart clks m) getCurrentTime

-- | Produce a four-state VCD (Value Change Dump) according to IEEE
-- 1364-{1995,2001}. This function fails if a trace name contains either
-- non-printable or non-VCD characters.
--
-- Due to lazy evaluation, the created VCD files might not contain all the
-- traces you were expecting. You therefore have to provide a list of names
-- you definately want to be dumped in the VCD file.
--
-- For example:
--
-- @
-- vcd <- dumpVCD 0 100 cntrOut ["main", "sub"]
-- @
--
-- Clocks start after 100ns, or after 1 clock cycle of the supplied signal.
-- For more options, see 'advancedDumpVCD'.
--
-- Evaluates /cntrOut/ long enough in order for to guarantee that the @main@,
-- and @sub@ traces end up in the generated VCD file.
dumpVCD
  :: NFDataX a
  => KnownDomain dom
  => Time dom
  -- ^ Start
  -> Time dom
  -- ^ Duration
  -> Signal dom a
  -- ^ (One of) the outputs of the circuit containing the traces
  -> [String]
  -- ^ The names of the traces you definitely want to be dumped in the VCD file
  -> IO (Either String Text.Text)
dumpVCD start duration sig = dumpVCD# traceMap# (start,start+duration) (max 1 (timeNs 100)) sig []

-- | Produce a four-state VCD (Value Change Dump) according to IEEE
-- 1364-{1995,2001}. This function fails if a trace name contains either
-- non-printable or non-VCD characters.
--
-- Due to lazy evaluation, the created VCD files might not contain all the
-- traces you were expecting. You therefore have to provide a list of names
-- you definately want to be dumped in the VCD file.
--
-- For example:
--
-- @
-- vcd <- dumpVCD 0 100 cntrOut ["main", "sub"]
-- @
--
--
--
-- Evaluates /cntrOut/ long enough in order for to guarantee that the @main@,
-- and @sub@ traces end up in the generated VCD file.
advancedDumpVCD
  :: NFDataX a
  => KnownDomain dom
  => Time dom
  -- ^ Start
  -> Time dom
  -- ^ Duration
  -> Time dom
  -- ^ Clock start time
  -> Bool
  -- ^ Shift the start of the simulation to `t=0`
  -> Signal dom a
  -- ^ (One of) the outputs of the circuit containing the traces
  -> [(String,Period)]
  -- ^ List of clock waves to generate
  -> [String]
  -- ^ The names of the traces you definitely want to be dumped in the VCD file
  -> IO (Either String Text.Text)
advancedDumpVCD start duration clkStart shift =
  if shift then dumpVCD# traceMap# (    0,      duration) (clkStart-start)
  else          dumpVCD# traceMap# (start,start+duration)  clkStart

-- | Like 'advancedDumpVCD', but uses `(start,stop)` instead of a start and duration.
advancedDumpVCDWindow
  :: NFDataX a
  => KnownDomain dom
  => (Time dom, Time dom)
  -- ^ (start,stop)
  -> Time dom
  -- ^ Clock start
  -> Bool
  -- ^ Shift the start of the simulation to `t=0`
  -> Signal dom a
  -- ^ (One of) the outputs of the circuit containing the traces
  -> [(String,Period)]
  -- ^ List of clock waves to generate
  -> [String]
  -- ^ The names of the traces you definitely want to be dumped in the VCD file
  -> IO (Either String Text.Text)
advancedDumpVCDWindow (start,stop) clkStart shift =
  if shift then dumpVCD# traceMap# (    0,stop-start) (clkStart-start)
  else          dumpVCD# traceMap# (start,stop      )  clkStart

-- | Dump a number of samples to a replayable bytestring.
dumpReplayable
  :: forall a dom
   . NFDataX a
  => Int
  -- ^ Number of samples
  -> Signal dom a
  -- ^ (One of) the outputs of the circuit containing the traces
  -> String
  -- ^ Name of trace to dump
  -> IO ByteString
dumpReplayable n oSignal traceName = do
  waitForTraces# traceMap# oSignal [traceName]
  replaySignal <- (Map.! traceName) <$> readIORef traceMap#
  let (tRep, _period, _width, samples) = replaySignal
  pure (ByteStringLazy.concat (tRep : map encode (take n samples)))

-- | Take a serialized signal (dumped with @dumpReplayable@) and convert it
-- back into a signal. Will error if dumped type does not match requested
-- type. The first value in the signal that fails to decode will stop the
-- decoding process and yield an error. Not that this always happens if you
-- evaluate more values than were originally dumped.
replay
  :: forall a dom n
   . ( Typeable a
     , NFDataX a
     , BitPack a
     , KnownNat n
     , n ~ BitSize a )
  => ByteString
  -> Either String (Signal dom a)
replay bytes0 = samples1
 where
  samples1 =
    case decodeOrFail bytes0 of
      Left (_, _, err) ->
        Left ("Failed to decode typeRep. Parser reported:\n\n" ++ err)
      Right (bytes1, _, _ :: TypeRep a) ->
        let samples0 = decodeSamples bytes1 in
        let err = "Failed to decode value in signal. Parser reported:\n\n " in
        Right (fromList (map (either (error . (err ++)) id) samples0))

-- | Helper function of 'replay'. Decodes ByteString to some type with
-- BitVector as an intermediate type.
decodeSamples
  :: forall a n
   . ( BitPack a
     , KnownNat n
     , n ~ BitSize a )
  => ByteString
  -> [Either String a]
decodeSamples bytes0 =
  case decodeOrFail bytes0 of
    Left (_, _, err) ->
      [Left err]
    Right (bytes1, _, (m, v)) ->
      (Right (unpack (BV m v))) : decodeSamples bytes1

-- | Keep evaluating given signal until all trace names are present.
waitForTraces#
  :: NFDataX a
  => IORef TraceMap
  -- ^ Map with collected traces
  -> Signal dom a
  -- ^ (One of) the output(s) the circuit containing the traces
  -> [String]
  -- ^ The names of the traces you definitely want to be dumped to the VCD file
  -> IO ()
waitForTraces# traceMap signal traceNames = do
  atomicWriteIORef traceMap Map.empty
  rest <- foldM go signal traceNames
  seq rest (return ())
 where
  go (s0 :- ss) nm = do
    m <- readIORef traceMap
    if Map.member nm m then
      deepseqX s0 (return ss)
    else
      deepseqX
        s0
        (go ss nm)
