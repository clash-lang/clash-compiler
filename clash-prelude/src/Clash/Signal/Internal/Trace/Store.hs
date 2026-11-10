{- |

Module for storing traces in files.

-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Signal.Internal.Trace.Store (
  FileHeader(..),
  SignalDescr(..),

  -- * Storing
  storeTrace0,
  storeTrace,
  storeTraces,
  store,
  stores,
  storeSignal,

  -- * Loading
  loadHeader,
  loadTrace0,
  loadTrace,
  loadTraces,
  load,
  loads,
  loadSignal,
  loadSignals,
) where

import Control.Monad (foldM, when)
import Data.Aeson hiding (Value) --(encode, eitherDecode, ToJSON(..), FromJSON(..), Array)
import qualified Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)
import Data.List (transpose, zipWith4)
import Data.Maybe (fromMaybe)
import GHC.Bits (shiftL, shiftR, (.|.), (.&.))
import GHC.Generics (Generic)
import GHC.Natural (Natural)

import Clash.Signal (KnownDomain, Signal, Bundle, Unbundled, unbundle)
import Clash.Time (clockPeriodTime)

import Clash.Signal.Internal.Trace
import Clash.Signal.Internal.Trace.CompatibleTypes


{-

file format:
<JSON header> \0 <binary blob>

The header looking something like:

{
  "samples": 200,     # number of samples in the file
  "offset": 0,        # optional; note that with an offset of 0, the first value
                      # is the value /before/ the first clock edge
  "width": 17,        # number of bits
  "hasMask": true,    # whether or not the undefined bits mask is stored;
                      # usually true for simulation and false for hardware output
  "values": [         # list of "subsignals"; always at least one present
    {
      "name": "a",
      "type": "A",    # exact type format yet to be determined,
                      # but should be easy to combine into other formats,
                      # like (named) tuples
      "width": 12,    # widths must add up
      "meta": <json>  # optional metadata field
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
  , hasMask :: Bool
  , signals :: Either SignalDescr [SignalDescr]
  }
 deriving (Show,Generic)

data SignalDescr = SignalDescr
  { name :: String
  , width :: Int
  , dataType :: DataType
  , jsonMeta :: Data.Aeson.Value
  }
 deriving (Show,Generic)

instance ToJSON FileHeader where
  toJSON FileHeader{..} = object
    [ "samples" .= samples
    , "offset"  .= offset
    , "width"   .= width
    , "hasMask" .= hasMask
    , "signals" .= either toJSON toJSON signals
    ]

instance FromJSON FileHeader where
  parseJSON = withObject "Header" $ \v -> do
    samples <- v .: "samples"
    offset  <- v .: "offset"
    width   <- v .: "width"
    hasMask <- v .: "hasMask"
    signals0 <- v .: "signals"
    signals <- case signals0 of
      Array _ -> Right <$> parseJSON signals0
      _       -> Left <$> parseJSON signals0
    pure FileHeader{..}

instance ToJSON SignalDescr where
  toJSON SignalDescr{..} = object
    ( [ "name"     .= name
      , "width"    .= width
      , "dataType" .= dataType
      ] <> (if jsonMeta/=Null then ["meta" .= jsonMeta] else []) )

instance FromJSON SignalDescr where
  parseJSON = withObject "SignalDescr" $ \v -> do
    name     <- v .: "name"
    width    <- v .: "width"
    dataType <- v .: "dataType"
    metaM <- v .:? "meta"
    let jsonMeta = fromMaybe Null metaM
    pure SignalDescr{..}

----------------------------------------------
--                 STORING                  --
----------------------------------------------

storeTrace0 :: (Int,Int) -> Either SignalDescr [SignalDescr] -> [Value] -> ByteString
storeTrace0 (start,stop) signals values = header0 <> "\n\0" <> rawValues
 where
  width = sum $ map (\SignalDescr{width=w}->w) $
    case signals of
      Left  ds  -> [ds]
      Right dss -> dss

  header = FileHeader
    { samples = stop-start
    , offset = start
    , width
    , hasMask = True
    , signals
    }
  header0 = encode header

  values0 = drop start $ take stop $ values
  rawValues = B.pack $ concatMap valToBytes values0

  k = (width+7) `div` 8
  valToBytes (m,v) = concatMap natToBytes [m,v]
  natToBytes n = map (\x -> fromIntegral (n `shiftR` (8*x))) [0..k-1] -- TODO: more efficient implementation?

-- | Store a single trace as a signal dump.
storeTrace ::
  forall t.
  TraceSystem t =>
  String ->
  (Int,Int) ->
  Trace t ->
  ByteString
storeTrace name samples Trace{..} =
  storeTrace0
    samples
    (Left SignalDescr{name, width, dataType, jsonMeta = storeMeta @t meta})
    values

-- | Store multiple traces in a single binary file.
-- All traces must have the same clock period.
storeTraces ::
  forall t.
  TraceSystem t =>
  (Int, Int) ->
  [(String, Trace t)] ->
  Either String ByteString
storeTraces samples traces = do
  let same (a:r@(b:_)) = a==b && (same r)
      same _ = True
  if same (map ((.period) . snd) traces)
  then do
    Right ()
  else do
    Left
      "Attempting to store multiple traces with different periods in one signal dump file."

  let signals = map descrFromTrace traces
      descrFromTrace (name,Trace{width,dataType,meta}) =
        SignalDescr{name, width, dataType, jsonMeta = storeMeta @t meta}
      widths = map ((.width) . snd) traces
      ccat vs = foldl
        (\(v,m) (w,(v0,m0)) -> ((v `shiftL` w).|.v0, (m `shiftL` w).|.m0)) (0,0)
        (zip widths vs)
      values = map ccat $ transpose $ map ((.values) . snd) traces -- combine values TODO: make more efficient

  Right $ storeTrace0 samples (Right signals) values

-- | Store a trace in binary form.
store ::
  forall t.
  TraceSystem t =>
  -- | Name of trace to dump
  String ->
  Simulation t ->
  Either String ByteString
store name sim@Simulation{simTimes} = do
  trc <- fetchTrace name sim
  let samples = simulationTimeRangeToCycles simTimes trc.period
  pure $ storeTrace name samples trc

-- | Store multiple traces into a single binary.
-- All traces must have the same clock period.
stores ::
  forall t.
  TraceSystem t =>
  -- | Names of traces to dump
  [String] ->
  Simulation t ->
  Either String ByteString
stores names sim@Simulation{simTimes} = do
  traces <- mapM (\n -> fetchTrace n sim) names
  Trace{period} <- case traces of
    [] -> Left "stores called without any trace names"
    (t:_) -> Right t
  let samples = simulationTimeRangeToCycles simTimes period
  storeTraces samples (zip names traces) -- periods are checked here

-- | Store a 'Signal' in binary form.
storeSignal ::
  forall t dom a.
  TraceSystem t =>
  KnownDomain dom =>
  Traceable t a =>
  -- | Name of trace to dump
  String ->
  -- | Range of samples to dump
  (Int,Int) ->
  -- | The signal to dump
  Signal dom a ->
  ByteString
storeSignal name samples signal = storeTrace name samples $ toTrace @t signal


----------------------------------------------
--                 LOADING                  --
----------------------------------------------

-- | Load only the header of a trace dump file.
-- This may be used to inspect what signals it has,
-- which values are defined, etc.
loadHeader ::
  ByteString ->
  Either String FileHeader
loadHeader bs = do
  let (header0,_) = B.span (/=0) bs
  eitherDecode header0

-- | Read the file but do not finalize the trace yet.
loadTrace0 ::
  ByteString ->
  Either String (FileHeader,[Value])
loadTrace0 bs = do
  let (header0,rest) = B.span (/=0) bs
      rawValues0 = B.drop 1 rest

  header@FileHeader{width,hasMask=storesUndefined,offset} <- eitherDecode header0

  let k = (width+7) `div` 8
      values =
        if storesUndefined
        then undefinedValues
        else definedValues

      undefinedValues = f rawValues
       where
        f (m:v:r) = (m,v) : f r
        f _ = []
      definedValues = map (0,) rawValues

      rawValues :: [Natural]
      rawValues = map bsToNat $ cut rawValues0
       where
        cut b | B.null b = []
              | otherwise = v : cut r
         where (v,r) = B.splitAt (fromIntegral k) b

      bsToNat bs0 = sum $ zipWith (shiftL . fromIntegral) (B.unpack bs0) [0,8..8*k-1]

      undef = ((1 `shiftL` width)-1,0)

  pure
    ( header
    , (replicate offset undef) <> values <> repeat undef
    )

-- | Convert binary data into a trace with the given clock period.
loadTrace ::
  forall t.
  TraceSystem t =>
  Period ->
  ByteString ->
  Either String (Trace t)
loadTrace period bs = do
  (header, values) <- loadTrace0 bs
  let FileHeader{width, signals} = header
  pure $ case signals of
    Left ds -> Trace
      { dataType=ds.dataType
      , meta = loadMeta @t ds.jsonMeta
      , ..}
    Right dss -> Trace
      { dataType = constructTuple $ map (.dataType) dss
      , meta = loadMetas @t $ map (\ds -> (ds.name,ds.jsonMeta)) dss
      , ..}

-- | Load multiple traces from a single binary.
loadTraces ::
  forall t.
  TraceSystem t =>
  Period ->
  ByteString ->
  Either String [(String, Trace t)]
loadTraces period bs = do
  (header, values) <- loadTrace0 bs

  let FileHeader{width, signals} = header
      (names, widths, types) = case signals of
        Left  ds  -> ([ds.name], [ds.width], [ds.dataType])
        Right dss -> unzip3 $ map (\ds -> (ds.name, ds.width, ds.dataType)) dss
      cWidths = scanl (+) 0 widths
      ranges = zip cWidths (drop 1 cWidths)

  when (width /= last cWidths) $ do
    Left "Subsignal widths do not add up to signal width"

  let slice0 (from, to) (m,v) = (go m, go v)
       where
        lo = width-to
        bits = to-from
        go n = (n `shiftR` lo) .&. ((1 `shiftL` bits) - 1)

      valuess = case signals of
        Left  _ -> [values] -- Slicing all values would be inefficient; skip
        Right _ -> map (\r -> map (slice0 r) values) ranges
      metas = case signals of
        Left  ds  -> [loadMeta @t ds.jsonMeta]
        Right dss -> map (loadMeta @t . (.jsonMeta)) dss
      traces = zipWith4 (flip Trace period) types widths valuess metas

  Right $ zip names traces

-- | Load a trace from binary form.
load ::
  forall t dom.
  TraceSystem t =>
  KnownDomain dom =>
  -- | The name to use for the signal
  String ->
  -- | The binary data
  ByteString ->
  -- | The 'Simulation' to add the signal to
  Simulation t ->
  Either String (Simulation t)
load name bs sim = do
  trc <- loadTrace (clockPeriodTime @dom) bs
  addTrace name trc sim

-- | Load multiple traces from a single binary.
-- The traces will be loaded into a single domain.
loads ::
  forall t dom.
  TraceSystem t =>
  KnownDomain dom =>
  ByteString ->
  Simulation t ->
  Either String (Simulation t)
loads bs sim = do
  traces <- loadTraces (clockPeriodTime @dom) bs
  foldM (\s (n,t) -> addTrace n t s) sim traces

-- | Load a 'Signal' from binary form.
-- See also @compatible@.
loadSignal ::
  forall t dom a.
  TraceSystem t =>
  KnownDomain dom =>
  Traceable t a =>
  ByteString ->
  Either String (Signal dom a)
loadSignal bs = do
  trc <- loadTrace @t (clockPeriodTime @dom) bs
  fromTrace @t trc

-- | Load multiple 'Signal's from binary form.
-- See also @compatible@.
loadSignals ::
  forall t dom a.
  TraceSystem t =>
  KnownDomain dom =>
  Traceable t a =>
  Bundle a =>
  ByteString ->
  Either String (Unbundled dom a)
loadSignals bs = do
  sig <- loadSignal @t @dom @a bs
  pure $ unbundle sig
