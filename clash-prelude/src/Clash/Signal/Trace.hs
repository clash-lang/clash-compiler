{-|
Copyright  :  (C) 2018, Google Inc.
                  2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

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
  case dumpToVCD (0, 100) cntrOut ["main", "sub"] of
    Left msg ->
      error msg
    Right contents ->
      writeFile "mainCounter.vcd" contents
@
-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

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

  -- * Native
  , dumpToList

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
  -- ** Internal functions
  , traceSignal#
  , traceVecSignal#
  , dumpVCD#
  ) where

-- Clash:
import           Clash.Annotations.Primitive
  (hasBlackBox, idBlackBoxTH)
import           Clash.Signal.Internal
  (Signal(..), Stream(..), SignalTrace(..),  smTraces, fromList, meta#,
   metaNameMap, mergeSignalMeta#)
import           Clash.Signal
  (KnownDomain(..), SDomainConfiguration(..), bundle, unbundle)
import           Clash.Sized.Vector    (Vec, iterateI)
import qualified Clash.Sized.Vector    as Vector
import           Clash.Class.BitPack   (BitPack, BitSize, pack, unpack)
import           Clash.Promoted.Nat    (snatToNum, SNat(..))
import           Clash.Signal.Internal (sample)
import           Clash.XException      (NFDataX)
import           Clash.Sized.Internal.BitVector
  (BitVector(BV))

-- Haskell / GHC:
import           Data.Bits             (testBit)
import           Data.Binary           (encode, decodeOrFail)
import           Data.ByteString.Lazy  (ByteString)
import qualified Data.ByteString.Lazy  as ByteStringLazy
import           Data.Bifunctor        (bimap)
import           Data.Char             (ord, chr)
import           Data.Default          (def)
import qualified Data.List             as List
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HashMap
import           Data.IntMap.Strict    (IntMap)
import qualified Data.IntMap.Strict    as IntMap
import           Data.List             (foldl1', foldl', unzip4, transpose, (\\))
import           Data.Maybe            (fromMaybe, catMaybes)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import           Data.Time.Clock       (UTCTime, getCurrentTime)
import           Data.Time.Format      (formatTime, defaultTimeLocale)
import           GHC.Stack             (HasCallStack)
import           GHC.TypeLits          (KnownNat, type (+))
import           System.IO.Unsafe      (unsafePerformIO)
import           TextShow              (showt)
import           Type.Reflection       (Typeable, TypeRep, typeRep)

#ifdef CABAL
import qualified Data.Version
import qualified Paths_clash_prelude
#endif

type Period   = Int
type Changed  = Bool
type Value    = (Integer, Integer) -- (Mask, Value)
type Width    = Int

type TraceMap  = HashMap Text.Text SignalTrace

mkTrace
  :: ( HasCallStack
     , KnownNat (BitSize a)
     , BitPack a
     , NFDataX a )
  => Stream dom a
  -> [Value]
mkTrace signal =
  let toTup (BV mask value) = (mask, value) in
  sample (toTup . pack <$> signal)

-- | Internal function: trace a single signal.
traceSignal#
  :: forall dom a
   . ( HasCallStack
     , KnownNat (BitSize a)
     , BitPack a
     , NFDataX a
     , Typeable a )
  => Int
  -- ^ The associated clock period for the trace
  -> Text.Text
  -- ^ Name of signal in the VCD output
  -> Signal dom a
  -- ^ Signal to trace
  -> Signal dom a
traceSignal# period traceName ~(Signal meta0 stream) =
  Signal meta2 stream
 where
  width = snatToNum (SNat @ (BitSize a))
  typeRepA = encode (typeRep @a)
  theTrace = SignalTrace typeRepA period width (mkTrace stream)
  meta1 = def{smTraces=const (HashMap.singleton traceName theTrace)}
  meta2 = mergeSignalMeta# meta0 meta1
{-# NOINLINE traceSignal# #-}
{-# ANN traceSignal# hasBlackBox #-}
idBlackBoxTH 'traceSignal# 7

-- | Trace a single vector signal: each element in the vector will show up as
-- a different trace.
traceVecSignal#
  :: forall dom n a
   . ( KnownNat (BitSize a)
     , KnownNat n
     , BitPack a
     , NFDataX a
     , Typeable a )
  => Int
  -- ^ Associated clock period for the trace
  -> Text.Text
  -- ^ Name of signal in the VCD output. Will be appended by _0, _1, ..., _n.
  -> Signal dom (Vec (n+1) a)
  -- ^ Signal to trace
  -> Signal dom (Vec (n+1) a)
traceVecSignal# period vecTraceName (unbundle -> vecSignal) =
  bundle (Vector.zipWith trace' (iterateI succ (0 :: Int)) vecSignal)
 where
  trace' i s = traceSignal# period (name' i) s
  name' i    = vecTraceName <> "_" <> showt i
{-# NOINLINE traceVecSignal# #-}

-- | Trace a single signal. Be careful to only use a name once, as 'traceSignal'
-- will silently drop duplicate names.
--
-- __NB__ Works correctly when creating VCD files from traced signal in
-- multi-clock circuits. However 'traceSignal1' might be more convenient to
-- use when the domain of your circuit is polymorphic.
traceSignal
  :: forall dom  a
   . ( KnownDomain dom
     , KnownNat (BitSize a)
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
    SDomainConfiguration _dom period _edge _reset _init _polarity ->
      traceSignal# (snatToNum period) (Text.pack traceName) signal

-- | Trace a single signal. Be careful to only use a name once, as 'traceSignal'
-- will silently drop duplicate names.
--
-- __NB__ associates the traced signal with a clock period of /1/, which
-- results in incorrect VCD files when working with circuits that have
-- multiple clocks. Use 'traceSignal' when working with circuits that have
-- multiple clocks.
traceSignal1
  :: ( KnownNat (BitSize a)
     , BitPack a
     , NFDataX a
     , Typeable a )
  => String
  -- ^ Name of signal in the VCD output
  -> Signal dom a
  -- ^ Signal to trace
  -> Signal dom a
traceSignal1 traceName signal =
  traceSignal# 1 (Text.pack traceName) signal

-- | Trace a single vector signal: each element in the vector will show up as
-- a different trace. Be careful to only use a name once, as 'traceSignal'
-- will silently drop duplicate names.
--
-- __NB__ Works correctly when creating VCD files from traced signal in
-- multi-clock circuits. However 'traceSignal1' might be more convenient to
-- use when the domain of your circuit is polymorphic.
traceVecSignal
  :: forall dom a  n
   . ( KnownDomain dom
     , KnownNat (BitSize a)
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
    SDomainConfiguration _dom period _edge _reset _init _polarity ->
      traceVecSignal# (snatToNum period) (Text.pack traceName) signal

-- | Trace a single vector signal: each element in the vector will show up as
-- a different trace. Be careful to only use a name once, as 'traceSignal'
-- will silently drop duplicate names.
--
-- __NB__ associates the traced signal with a clock period of /1/, which
-- results in incorrect VCD files when working with circuits that have
-- multiple clocks. Use 'traceSignal' when working with circuits that have
-- multiple clocks.
traceVecSignal1
  :: ( KnownNat (BitSize a)
     , KnownNat n
     , BitPack a
     , NFDataX a
     , Typeable a )
  => String
  -- ^ Name of signal in debugging output. Will be appended by _0, _1, ..., _n.
  -> Signal dom (Vec (n+1) a)
  -- ^ Signal to trace
  -> Signal dom (Vec (n+1) a)
traceVecSignal1 traceName signal =
  traceVecSignal# 1 (Text.pack traceName) signal

iso8601Format :: UTCTime -> String
iso8601Format = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

toPeriodMap :: TraceMap -> IntMap [(Text.Text, Width, [Value])]
toPeriodMap m = foldl' go IntMap.empty (HashMap.toList m)
 where
  go periodMap (traceName, SignalTrace {stPeriod, stWidth, stTrace}) =
    let go' = ((traceName, stWidth, stTrace):) . (fromMaybe []) in
    IntMap.alter (Just . go') stPeriod periodMap

flattenMap :: IntMap [b] -> [(Int, b)]
flattenMap m = concat [[(a, b) | b <- bs] | (a, bs) <- IntMap.toList m]

printable :: Char -> Bool
printable (ord -> c) = 33 <= c && c <= 126

-- | Same as @dumpVCD@, but supplied with a custom timestamp
dumpVCD#
  :: (Int, Int)
  -- ^ (offset, number of samples)
  -> TraceMap
  -> UTCTime
  -> Either Text ByteString
dumpVCD# (offset, cycles) traceMap now
  | offset < 0 =
      error $ "dumpVCD: offset was " ++ show offset ++ ", but cannot be negative."
  | cycles < 0 =
      error $ "dumpVCD: cycles was " ++ show cycles ++ ", but cannot be negative."
  | null traceMap =
      error $ "dumpVCD: no traces found. Extend the given trace names."
  | HashMap.size traceMap > 126 - 33 =
      Left $ "Tracemap contains more than 93 traces, which is not supported by VCD."
  | not $ null $ offensiveNames =
      Left $ Text.unwords [ "Trace '" <> head offensiveNames <> "' contains"
                          , "non-printable ASCII characters, which is not"
                          , "supported by VCD." ]
  | otherwise =
      Right $ ByteStringLazy.fromChunks
            $ map Text.encodeUtf8
            $ map (`Text.append` "\n")
            $ [ Text.unwords headerDate
              , Text.unwords headerVersion
              , Text.unwords headerComment
              , Text.unwords headerTimescale
              , "$scope module logic $end"
              , Text.intercalate "\n" headerWires
              , "$upscope $end"
              , "$enddefinitions $end"
              , "#0"
              , "$dumpvars"
              , Text.intercalate "\n" initValues
              , "$end"
              ] ++ [bp <> "\n" | bp <- catMaybes bodyParts]
 where
  offensiveNames = filter (Text.any (not . printable)) traceNames

  labels = map chr [33..126]

  timescale = foldl1' gcd (IntMap.keys periodMap)
  periodMap = toPeriodMap traceMap

  -- TODO: Use same logic as unsafeSynchronizer

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
  normalize period values = concatMap (replicate period) values
  slice values = drop offset $ take cycles values

  headerDate       = ["$date", Text.pack $ iso8601Format now, "$end"]

#ifdef CABAL
  clashVer         = Data.Version.showVersion Paths_clash_prelude.version
#else
  clashVer         = "development"
#endif
  headerVersion    = ["$version", "Generated by Clash", Text.pack clashVer , "$end"]
  headerComment    = ["$comment", "No comment", "$end"]
  headerTimescale  = ["$timescale", (showt timescale) <> "ps", "$end"]
  headerWires      = [ Text.unwords $ headerWire w l n
                     | (w, l, n) <- (zip3 widths labels traceNames)]
  headerWire w l n = ["$var wire", showt w, Text.singleton l, n, "$end"]
  initValues       = map Text.pack $ zipWith ($) formatters inits

  formatters = zipWith format widths labels
  inits = map head valuess'
  tails = map changed valuess'

  -- | Format single value according to VCD spec
  format :: Width -> Char -> Value -> String
  format 1 label (0,0)   = ['0', label, '\n']
  format 1 label (0,1)   = ['1', label, '\n']
  format 1 label (1,_)   = ['x', label, '\n']
  format 1 label (mask,val) =
    error $ "Can't format 1 bit wide value for "
         ++ show label ++ ": value " ++ show val ++ " and mask " ++ show mask
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
      go (showt -> n) t =
        let pre = Text.concat ["#", n, "\n"] in
        fmap (Text.append pre) t

  bodyPart :: [(Changed, Value)] -> Maybe Text.Text
  bodyPart values =
    let formatted  = [(c, f v) | (f, (c,v)) <- zip formatters values]
        formatted' = map (Text.pack . snd) $ filter fst $ formatted in
    if null formatted' then Nothing else Just $ Text.intercalate "\n" formatted'

-- | Deprecated: see 'dumpToVCD'.
dumpVCD
  :: (Int, Int)
  -- ^ (offset, number of samples)
  -> Signal dom a
  -- ^ (One of) the outputs of the circuit containing the traces
  -> [String]
  -- ^ Check for existence of these traces. 'dumpVCD' will dump all traces it
  -- found.
  -> IO (Either String Text)
dumpVCD slice sig sigNames = pure $
  -- New API ('dumpToVCD') is properly lazy and typed, so we need a lot of type
  -- juggling to keep the old 'dumpVCD' API. Old API also dumped ALL available
  -- traces, so we'll replicate that too.
  case allTraceNames1 of
    Left err -> Left err
    Right allTraceNames2 ->
      bimap
        Text.unpack
        (Text.decodeUtf8 . ByteStringLazy.toStrict)
        (dumpToVCD slice sig allTraceNames2)
 where
  allTraceNames0 = HashMap.keys (metaNameMap (smTraces (meta# sig)))
  allTraceNames1 =
    case sigNames \\ map Text.unpack allTraceNames0 of
      [] -> Right allTraceNames0
      nf -> Left ("Could not find traces for: " <> List.intercalate ", " nf)
{-# DEPRECATED dumpVCD "'dumpVCD' will be removed in Clash 1.4. Use 'dumpToVCD'." #-}


-- | Produce a four-state VCD (Value Change Dump) according to IEEE
-- 1364-{1995,2001}. This function fails if a trace name contains either
-- non-printable or non-VCD characters.
--
-- This function fails if:
--
--   * ..a trace name wasn't present in the set of all detected traces, or..
--   * ..a trace name contains non-printable or non-VCD characters
--   * ..a trace name was used to trace multiple signals
--
-- For example:
--
-- @
-- dumpToVCD (0, 100) cntrOut ["main", "sub"]
-- @
--
dumpToVCD
  :: (Int, Int)
  -- ^ (offset, number of samples)
  -> Signal dom a
  -- ^ (One of) the outputs of the circuit containing the traces
  -> [Text]
  -- ^ The names of the traces to dump.
  -> Either Text ByteString
dumpToVCD slice (Signal meta _stream) sigNames =
  case sigNames \\ HashMap.keys traceMap of
    [] ->
      let nameFilter k _v = k `elem` sigNames in
      dumpVCD# slice (HashMap.filterWithKey nameFilter traceMap) now
    nf -> Left ("Could not find traces for: " <> Text.intercalate ", " nf)
 where
  now = unsafePerformIO getCurrentTime
  traceMap = metaNameMap (smTraces meta)
{-# NOINLINE dumpVCD #-}

-- | Dump traced signal to a list of those elements.
dumpToList
  :: BitPack a
  => Signal dom b
  -> Text
  -> Either Text [a]
dumpToList (Signal meta _stream) traceName =
  case HashMap.lookup traceName (metaNameMap (smTraces meta)) of
    Just t -> Right (map (unpack . uncurry BV) (stTrace t))
    Nothing -> Left ("No trace found for: " <> traceName)
{-# NOINLINE dumpToList #-}

-- | Dump a number of samples to a replayable bytestring.
dumpReplayable
  :: forall a dom
   . (HasCallStack, NFDataX a)
  => Int
  -- ^ Number of samples
  -> Signal dom a
  -- ^ (One of) the outputs of the circuit containing the traces
  -> String
  -- ^ Name of trace to dump
  -> IO ByteString
dumpReplayable n sig traceName = pure (dumpToReplayable n sig (Text.pack traceName))
{-# DEPRECATED dumpReplayable "'dumpReplayable' will be removed in Clash 1.4. Use 'dumpToReplayable'." #-}

-- | Dump a number of samples to a replayable bytestring.
dumpToReplayable
  :: forall a dom
   . (HasCallStack, NFDataX a)
  => Int
  -- ^ Number of samples
  -> Signal dom a
  -- ^ (One of) the outputs of the circuit containing the traces
  -> Text
  -- ^ Name of trace to dump
  -> ByteString
dumpToReplayable n (Signal meta _stream) traceName =
  case HashMap.lookup traceName (metaNameMap (smTraces meta)) of
    Just (SignalTrace{stTypeRepr, stTrace}) ->
      ByteStringLazy.concat (stTypeRepr : map encode (take n stTrace))
    Nothing -> error ("No stream stored for: " <> show traceName)

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
