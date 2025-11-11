{-|
Copyright  :  (C) 2018, Google Inc.
                  2019, Myrtle Software Ltd
                  2022-2024, QBayLogic B.V.
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
{-# LANGUAGE TypeFamilies #-}

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
  -- ** Functions
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
  (KnownDomain(..), SDomainConfiguration(..), Signal, bundle, unbundle)
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
import           Data.Char             (ord, chr)
import           Data.IORef
  (IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef)
#if !MIN_VERSION_base(4,20,0)
import           Data.List             (foldl')
#endif
import           Data.List             (foldl1', unzip4, transpose, uncons)
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

type Period   = Int
type Changed  = Bool
type Value    = (Natural, Natural) -- (Mask, Value)
type Width    = Int

-- | Serialized TypeRep we need to store for dumpReplayable / replay
type TypeRepBS = ByteString

type TraceMap  = Map.Map String (TypeRepBS, Period, Width, [Value])

-- | Map of traces used by the non-internal trace and dumpvcd functions.
traceMap# :: IORef TraceMap
traceMap# = unsafePerformIO (newIORef Map.empty)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE traceMap# #-}

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
  -> Int
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
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE traceSignal# #-}

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
  -> Int
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
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE traceVecSignal# #-}

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
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE traceSignal #-}
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
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE traceSignal1 #-}
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
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE traceVecSignal #-}
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
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE traceVecSignal1 #-}
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

-- | Same as @dumpVCD@, but supplied with a custom tracemap and a custom timestamp
dumpVCD##
  :: (Int, Int)
  -- ^ (offset, number of samples)
  -> TraceMap
  -> UTCTime
  -> Either String Text.Text
dumpVCD## (offset, cycles) traceMap now
  | offset < 0 =
      error $ "dumpVCD: offset was " ++ show offset ++ ", but cannot be negative."
  | cycles < 0 =
      error $ "dumpVCD: cycles was " ++ show cycles ++ ", but cannot be negative."
  | null traceMap =
      error $ "dumpVCD: no traces found. Extend the given trace names."
  | Map.size traceMap > 126 - 33 =
      Left $ "Tracemap contains more than 93 traces, which is not supported by VCD."
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
                           , "#0"
                           , "$dumpvars"
                           , Text.intercalate "\n" initValues
                           , "$end"
                           , Text.intercalate "\n" $ catMaybes bodyParts
                           ]
 where
  offensiveNames = filter (any (not . printable)) traceNames

  labels = map chr [33..126]

  timescale = foldl1' gcd (Map.keys periodMap)
  periodMap = toPeriodMap traceMap

  -- Normalize traces until they have the "same" period. That is, assume
  -- we have two traces; trace A with a period of 20 ps and trace B with
  -- a period of 40 ps:
  --
  --   A: [A1, A2, A3, ...]
  --   B: [B1, B2, B3, ...]
  --
  -- After normalization these look like:
  --
  --   A: [A1, A2, A3, A4, A5, A6, ...]
  --   B: [B1, B1, B2, B2, B3, B3, ...]
  --
  -- ..because B is "twice as slow" as A.
  (periods, traceNames, widths, valuess) =
    unzip4 $ map
      (\(a, (b, c, d)) -> (a, b, c, d))
      (flattenMap periodMap)

  periods' = map (`quot` timescale) periods
  valuess' = map slice $ zipWith normalize periods' valuess
  normalize period (initial:values) = initial : concatMap (replicate period) values
  normalize _      []               = []
  slice values = drop offset $ take cycles values

  headerDate       = ["$date", Text.pack $ iso8601Format now, "$end"]

#ifdef CABAL
  clashVer         = Data.Version.showVersion Paths_clash_prelude.version
#else
  clashVer         = "development"
#endif
  headerVersion    = ["$version", "Generated by Clash", Text.pack clashVer , "$end"]
  headerComment    = ["$comment", "No comment", "$end"]
  headerTimescale  = ["$timescale", (show timescale) ++ "ps", "$end"]
  headerWires      = [ Text.unwords $ headerWire w l n
                     | (w, l, n) <- (zip3 widths labels traceNames)]
  headerWire w l n = map Text.pack ["$var wire", show w, [l], n, "$end"]
  initValues       = map Text.pack $ zipWith ($) formatters inits

  formatters = zipWith format widths labels
  inits = map (maybe (error "dumpVCD##: empty value") fst . uncons) valuess'
  tails = map changed valuess'

  -- | Format single value according to VCD spec
  format :: Width -> Char -> Value -> String
  format 1 label (0,0)   = ['0', label, '\n']
  format 1 label (0,1)   = ['1', label, '\n']
  format 1 label (1,_)   = ['x', label, '\n']
  format 1 label (mask,val) =
    error $ "Can't format 1 bit wide value for " ++ show label ++ ": value " ++ show val ++ " and mask " ++ show mask
  format n label (mask,val) =
    "b" ++ map digit (reverse [0..n-1]) ++ " " ++ [label]
    where
      digit d = case (testBit mask d, testBit val d) of
        (False,False) -> '0'
        (False,True)  -> '1'
        (True,_)      -> 'x'

  -- | Given a list of values, return a list of list of bools indicating
  -- if a value changed. The first value is *not* included in the result.
  changed :: [Value] -> [(Changed, Value)]
  changed (s:ss) = zip (zipWith (/=) (s:ss) ss) ss
  changed []     = []

  bodyParts :: [Maybe Text.Text]
  bodyParts = zipWith go [0..] (map bodyPart (Data.List.transpose tails))
    where
      go :: Int -> Maybe Text.Text -> Maybe Text.Text
      go (Text.pack . show -> n) t =
        let pre = Text.concat ["#", n, "\n"] in
        fmap (Text.append pre) t

  bodyPart :: [(Changed, Value)] -> Maybe Text.Text
  bodyPart values =
    let formatted  = [(c, f v) | (f, (c,v)) <- zip formatters values]
        formatted' = map (Text.pack . snd) $ filter fst $ formatted in
    if null formatted' then Nothing else Just $ Text.intercalate "\n" formatted'

-- | Same as @dumpVCD@, but supplied with a custom tracemap
dumpVCD#
  :: NFDataX a
  => IORef TraceMap
  -- ^ Map with collected traces
  -> (Int, Int)
  -- ^ (offset, number of samples)
  -> Signal dom a
  -- ^ (One of) the output(s) the circuit containing the traces
  -> [String]
  -- ^ The names of the traces you definitely want to be dumped to the VCD file
  -> IO (Either String Text.Text)
dumpVCD# traceMap slice signal traceNames = do
  waitForTraces# traceMap signal traceNames
  m <- readIORef traceMap
  fmap (dumpVCD## slice m) getCurrentTime

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
-- vcd <- dumpVCD (0, 100) cntrOut ["main", "sub"]
-- @
--
-- Evaluates /cntrOut/ long enough in order for to guarantee that the @main@,
-- and @sub@ traces end up in the generated VCD file.
dumpVCD
  :: NFDataX a
  => (Int, Int)
  -- ^ (offset, number of samples)
  -> Signal dom a
  -- ^ (One of) the outputs of the circuit containing the traces
  -> [String]
  -- ^ The names of the traces you definitely want to be dumped in the VCD file
  -> IO (Either String Text.Text)
dumpVCD = dumpVCD# traceMap#

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
