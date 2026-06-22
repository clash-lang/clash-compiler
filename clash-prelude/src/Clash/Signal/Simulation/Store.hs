

module Clash.Signal.Simulation.Store where


import           Data.ByteString.Lazy  (ByteString)
import           Data.ByteString.Lazy  as B

import           Clash.Signal.Simulation


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
  , offset :: Maybe Int
  , width :: Int
  , undefined :: Bool
  , values :: [ValueDescr]
  }

data ValueDescr = ValueDescr
  { name :: String
  , width :: Int
  , datatype :: DataType
  }

type DataType = (String,[DataType])

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
  Signal dom a ->
  -- | Number of samples
  Int ->
  ByteString
storeSignal signal samples = storeTrace (0,samples) $ toTrace signal

-- | Convert a trace into binary form.
storeTrace :: (Int,Int) -> Trace -> ByteString
storeTrace _samples _trc = undefined -- TODO ...

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
loadSignal bin = loadTrace @a bin >>= fromTrace

-- | Convert binary data into a trace with the given clock period.
loadTrace ::
  Period ->
  ByteString ->
  Either String Trace
loadTrace bin 
  | n /= header.width = Left "Data type width does not match"
  = Right
  ( encode (typeRep $ Proxy @a)
  , clockPeriodTime @dom
  , fromInteger $ natVal (Proxy @(BitSize a))
  , values )
 where
  (header,rest) = B.span (/=0) bin
  rawValues' = B.drop 1 rest'

  n = natVal (Proxy @(BitSize a))
  k = (n+7) `div` 8

  values =
    if header.undefined then
      undefinedValues
    else
      definedValues

  offset = fromMaybe 0 header.offset

  undefinedValues = f rawValues
   where
    f (v:m:r) = BV m v : f r
  definedValues = L.map (BV 0) rawValues

  rawValues = L.toInteger $ cut rawValues'
   where
    cut B.null = []
    cut b = v : cut r
     where (v,r) = B.splitAt k b




-- | Load multiple traces from a single binary.
loadTraces :: Period ...

-- | Load multiple traces from a single binary.
-- The traces will be loaded into a single domain.
loads :: forall dom. KnownDomain dom => ...

-- | Store multiple traces in a single binary file.
storeTraces :: ... [(String,Trace)]

-- | Store multiple traces into a single binary.
-- All traces must have the same clock period.
stores :: ... [String]
