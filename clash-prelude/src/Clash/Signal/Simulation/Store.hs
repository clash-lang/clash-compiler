{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Clash.Signal.Simulation.Store where

import           Data.Aeson
import           Data.ByteString.Lazy  (ByteString)
import qualified Data.ByteString.Lazy  as B
import           Data.Maybe            (fromMaybe)
import           Data.Word             (Word8)
import           GHC.Generics          (Generic)
import           GHC.Natural           (Natural)

import           Clash.Signal          (KnownDomain, Signal)
import           Clash.Sized.Internal.BitVector
  (BitVector(BV))
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
  , values :: [ValueDescr]
  }
 deriving (Show,Generic,ToJSON,FromJSON)

data ValueDescr = ValueDescr
  { name :: String
  , width :: Int
  , datatype :: DataType
  }
 deriving (Show,Generic,ToJSON,FromJSON)

-- | Store a trace in binary form.
store ::
  -- | Name of trace to dump
  String ->
  -- | Number of samples
  Int ->
  Simulation ->
  Either String ByteString
store name samples sim = storeTrace samples <$> fetchTrace name sim

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

-- | Convert a trace into binary form.
storeTrace :: String -> (Int,Int) -> Trace -> ByteString
storeTrace name (start,stop) (ty,period,width,values) = header' <> "\0" <> rawValues
 where
  header = FileHeader
    { samples = stop-start
    , offset = start
    , width
    , undefined = True
    , values =
        [ ValueDescr
            { name
            , width
            , datatype = ty
            }
        ]
    }
  k = (width+7) `div` 9
  header' = encode header
  values' = drop start $ take stop $ values
  rawValues = B.pack $ concatMap valToBytes values
  valToBytes (m,v) = concatMap natToBytes [v,m]
  natToBytes :: Natural -> [Word8]
  natToBytes i = map (\x -> fromIntegral (i >> (8*x))) [0..k-1] -- TODO: more efficient implementation, and check endianness

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
load name bin sim = loadTrace (clockPeriodTime @dom) bin >>= (\trc -> addTrace name trc sim)

-- | Load a 'Signal' from binary form.
loadSignal ::
  forall a dom.
  Traceable a =>
  ByteString ->
  Either String (Signal dom a)
loadSignal bin = loadTrace (clockPeriodTime @dom) bin >>= fromTrace

-- | Convert binary data into a trace with the given clock period.
loadTrace ::
  Period ->
  ByteString ->
  Either String Trace
loadTrace period bin
  = Right
  ( ty
  , period
  , width
  , (replicate offset ((1<<width)-1,0)) <> values )
 where
  (header',rest) = B.span (/=0) bin
  rawValues' = B.drop 1 rest'

  Right header@FileHeader{width,undefined=storesUndefined,offset,values=subsignals} = eitherDecode header'

  k = (width+7) `div` 8

  ty = ... subsignals

  values =
    if storesUndefined then
      undefinedValues
    else
      definedValues

  undefinedValues = f rawValues
   where
    f (v:m:r) = BV m v : f r
    f _ = []
  definedValues = map (BV 0) rawValues

  rawValues = map toInteger $ cut rawValues'
   where
    cut b | B.null b = []
          | otherwise = v : cut r
     where (v,r) = B.splitAt (fromIntegral k) b



-- | Load multiple traces from a single binary.
loadTraces :: Period -> ByteString -> Either String [(String,Trace)]
loadTraces  = Prelude.undefined -- TODO

-- | Load multiple traces from a single binary.
-- The traces will be loaded into a single domain.
loads :: forall dom. KnownDomain dom => Period -> ByteString -> Simulation -> Either String Simulation
loads = Prelude.undefined -- TODO

-- | Store multiple traces in a single binary file.
-- All traces must have the same clock period.
storeTraces :: (Int,Int) -> [(String,Trace)] -> Either String ByteString
storeTraces = Prelude.undefined -- TODO

-- | Store multiple traces into a single binary.
-- All traces must have the same clock period.
stores :: [String] -> Simulation -> Either String ByteString
stores = Prelude.undefined -- TODO
