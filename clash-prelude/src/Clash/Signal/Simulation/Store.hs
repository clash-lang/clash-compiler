{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Signal.Simulation.Store where

import           Data.Aeson            (encode,eitherDecode,ToJSON,FromJSON)
import           Data.ByteString.Lazy  (ByteString)
import qualified Data.ByteString.Lazy  as B
-- import           Data.Maybe            (fromMaybe)
import           Data.Word             (Word8)
import           GHC.Bits              (shiftL,shiftR)
import           GHC.Generics          (Generic)
import           GHC.Natural           (Natural)

import           Clash.Signal          (KnownDomain, Signal)
-- import           Clash.Sized.Internal.BitVector
--   (BitVector(BV))
import           Clash.Time            (clockPeriodTime)

import           Clash.Signal.Simulation
import           Clash.Signal.Simulation.DataType


{-

file format:
<JSON header> \0 <binary blob>

The header looking something like:

{
  "samples": 200,     # number of samples in the file
  "offset": 0,        # optional; note that with an offset of 0, the first value is the value /before/ the first clock edge
  "width": 17,        # number of bits
  "undefined": true,  # whether or not the undefined bits mask is stored; usually true for simulation and false for hardware output
  "values": [         # list of "subsignals"; always at least one present
    {
      "name": "a",
      "type": "A",    # exact type format yet to be determined, but should be easy to combine into other formats, like (named) tuples
      "width": 12,    # widths must add up
    },
    {
      "name": "b",
      "type": "B",
      "width": 5
    }
  ]
}


-}


data FileHeader = FileHeader
  { samples :: Int
  , offset :: Int
  , width :: Int
  , undefined :: Bool
  , signals :: [SignalDescr]
  }
 deriving (Show,Generic,ToJSON,FromJSON)

data SignalDescr = SignalDescr
  { name :: String
  , width :: Int
  , datatype :: DataType
  }
 deriving (Show,Generic,ToJSON,FromJSON)


storeTrace0 :: (Int,Int) -> [SignalDescr] -> [Value] -> ByteString
storeTrace0 (start,stop) signals values = header' <> "\n\0" <> rawValues
 where
  width = sum $ map (\SignalDescr{width=w}->w) signals

  header = FileHeader
    { samples = stop-start
    , offset = start
    , width
    , undefined = True
    , signals = signals
    }
  header' = encode header

  values' = drop start $ take stop $ values
  rawValues = B.pack $ concatMap valToBytes values'

  k = (width+7) `div` 9
  valToBytes (m,v) = concatMap natToBytes [v,m]
  natToBytes n = map (\x -> fromIntegral (n `shiftR` (8*x))) [0..k-1] -- TODO: more efficient implementation?

-- | Store a single trace as a signal dump.
storeTrace :: String -> (Int,Int) -> Trace -> ByteString
storeTrace name samples (ty,_period,width,values) = storeTrace0 samples [SignalDescr{name,width,datatype=ty}] values

-- | Store multiple traces in a single binary file.
-- All traces must have the same clock period.
storeTraces :: (Int,Int) -> [(String,Trace)] -> Either String ByteString
storeTraces samples traces = do
  let same (a:r(b:_)) = a==b && (same r)
      same _ = True
  if same (map (\(_,_,p,_) -> p) traces)
    then Right ()
    else Left "Attempting to store multiple traces with different periods in one signal dump file."

  let meta = map (\(name,(ty,_,width,_)) -> SignalDescr{name, width, datatype=ty}) traces
      values = ... -- combine values

  return $ storeTrace0 samples meta values

-- | Store a trace in binary form.
store ::
  -- | Name of trace to dump
  String ->
  Simulation ->
  Either String ByteString
store name sim@Simulation{simConfig} = do
  trc@(_,period,_,_) <- fetchTrace name sim
  let samples = simulationTimeRangeToCycles simConfig period
  return $ storeTrace name samples trc

-- | Store multiple traces into a single binary.
-- All traces must have the same clock period.
stores ::
  -- | Names of traces to dump
  [String] ->
  Simulation ->
  Either String ByteString
stores names sim@Simulation{simConfig} = do
  traces <- mapM (\n -> fetchTrace n sim) names
  (_,period,_,_) <- case traces of
    [] -> Left "stores called without any trace names"
    (t:_) -> Right t
  let samples = simulationTimeRangeToCycles simConfig period
  storeTraces samples traces

-- | Store a 'Signal' in binary form.
storeSignal ::
  forall dom a.
  KnownDomain dom =>
  Traceable a =>
  -- | Name of trace to dump
  String ->
  -- | Range of samples to dump
  (Int,Int) ->
  -- | The signal to dump
  Signal dom a ->
  ByteString
storeSignal name samples signal = storeTrace name samples $ toTrace signal

-- | Store multiple 'Signal's in binary form.
storeSignals ::
  forall dom a.
  KnownDomain dom =>
  StoreSignal a =>
  Traceable (Unbundled a) =>
  Bundle a =>
  -- | Range of samples to dump
  (Int,Int) ->
  -- | Bundle of signals to dump
  a ->
  ByteString
storeSignals samples sigs = storeTrace0 samples signals values
 where
  (_,_,_,values) = toTrace (bundle sigs)
  signals = storeSignalMeta @a

-- | Helper class to get signal information from anonymous records.
-- Extending could allow tuples to be stored, though the subsignals would be nameless (enumerated).
class StoreSignals a where
  storeSignalMeta :: [SignalDescr]
instance (KnownSymbol x, BitPack a, Typeable a) => StoreSignals (x := Signal dom a) where
  storeSignalMeta =
    [ SignalDescr
      { name = symbolVal (Proxy @x)
      , width = natVal (Proxy @(BitSize a))
      , datatype = typeRep @a } ]
instance (StoreSignals a, StorSignals b) => StoreSignas (a :&: b) where
  storeSignalMeta = storeSignalMeta @a <> storeSignalMeta @b

-- | Read the file but do not finalize the trace yet
loadTrace0 ::
  Period ->
  ByteString ->
  Either String (FileHeader,[Value])
loadTrace0 period bs =
  let (header,rest) = B.span (/=0) bin
      rawValues' = B.drop 1 rest

  header@FileHeader{width,undefined=storesUndefined,offset,signals} <- eitherDecode header

  let k = (width+7) `div` 8
      ty = constructRecord $ map (\SignalDescr{name,datatype} -> (name,datatype)) signals
      values =
        if storesUndefined then
          undefinedValues
        else
          definedValues

      undefinedValues = f rawValues
       where
        f (v:m:r) = (m,v) : f r
        f _ = []
      definedValues = map (0,) rawValues

      rawValues :: [Natural]
      rawValues = map bsToNat $ cut rawValues'
       where
        cut b | B.null b = []
              | otherwise = v : cut r
         where (v,r) = B.splitAt (fromIntegral k) b

      bsToNat bs = sum $ zipWith (shiftL . fromIntegral) (B.unpack bs) [0,8..8*k-1]

      undef = ((1 `shiftL` width)-1,0)

  return
    ( header
    , (replicate offset undef) <> values <> repeat undef
    )

-- | Convert binary data into a trace with the given clock period.
loadTrace ::
  Period ->
  ByteString ->
  Either String Trace
loadTrace period bs = do
  (header, values) <- loadTrace0 period bs
  let FileHeader{width, signals} = header
      ty = constructRecord $ map (\SignalDescr{name,datatype} -> (name,datatype)) signals

  return
    ( ty
    , period
    , width
    , values
    )

-- | Load multiple traces from a single binary.
loadTraces :: Period -> ByteString -> Either String [(String,Trace)]
loadTraces period bs = do
  (header, values) <- loadTrace0 period bs

  let FileHeader{width, signals} = header
      names, widths, types = uncurry3 (map (\SignalDescr n w t -> (n w t))) signals
      cWidths = scanl (+) 0 widths
      ranges = zip cWidths (drop 1 cWidths)

  if width /= last cWidths then
    Left "Subsignal widths do not add up to signal width"
  else Right ()

  let slice (from, to) (m,v) = (go m, go v)
       where
        lo = width-to
        bits = to-from
        go n = (n `shiftR` lo) .&. ((1 `shiftL` bits) - 1)

      valuess = map (\r -> map (slice r) values) ranges
      traces = zipWith3 (\t w vs -> (t, period, w, vs)) types widths valuess

  return $ zip names traces

-- | Load a trace from binary form.
load ::
  forall dom.
  KnownDomain dom =>
  -- | The name to use for the signal
  String ->
  -- | The binary data
  ByteString ->
  -- | The 'Simulation' to add the signal to
  Simulation ->
  Either String Simulation
load name bs sim = do
  trc <- loadTrace (clockPeriodTime @dom) bs
  addTrace name trc sim

-- | Load multiple traces from a single binary.
-- The traces will be loaded into a single domain.
loads :: forall dom. KnownDomain dom => ByteString -> Simulation -> Either String Simulation
loads bs sim = do
  traces <- loadTraces (clockPeriodTime @dom)
  foldM (\s (n,t) -> addTrace n t s) sim traces

-- | Load a 'Signal' from binary form.
loadSignal ::
  forall a dom.
  KnownDomain dom =>
  Traceable a =>
  ByteString ->
  Either String (Signal dom a)
loadSignal bs = do
  trc <- loadTrace (clockPeriodTime @dom) bs
  fromTrace trc

-- | Load multiple 'Signal's from binary form.
loadSignals ::
  forall a dom.
  KnownDomain dom =>
  Traceable a =>
  Bundle a =>
  ByteString ->
  Either String (Unbundled dom a)
loadSignals bs = do
  sig <- loadSignal @a @dom bs
  return $ unbundle sig
