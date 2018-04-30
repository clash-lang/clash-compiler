{-|
  Copyright  :  (C) 2018, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utilities for tracing signals and dumping them in various ways. Example usage:

  @
  import Clash.Prelude
  import Clash.Signal.Trace (dumpVCD, unsafeTraceSignal, fromVCDError)
  import Data.Either        (Either(..))
  import Data.Text.IO       (writeFile)

  -- | Count and wrap around
  subCounter :: Signal System (Index 3)
  subCounter = unsafeTraceSignal "sub" counter
    where
      counter =
        register 0 (fmap succ' counter)

      succ' c
        | c == maxBound = 0
        | otherwise     = c + 1

  -- | Count, but only when my subcounter is wrapping around
  mainCounter :: Signal System (Signed 64)
  mainCounter = unsafeTraceSignal "main" counter
    where
      counter =
        register 0 (fmap succ' $ bundle subcounter counter)

      succ' (sc, c)
        | sc == maxBound = c + 1
        | otherwise      = c

  -- | Setup clock and reset lines for example
  main :: IO ()
  main = exposeClockReset main' systemClockGen systemResetGen

  -- | Collect traces, and dump them to a VCD file.
  main :: SystemClockReset => IO ()
  main = do
    waitForTraces mainCounter ["main", "sub"]
    vcd <- dumpVCD (0, 10)
    case vcd of
      Left err ->
        error (fromVCDError err)
      Right contents ->
        writeFile "dumped.vcd" contents
  @
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

module Clash.Signal.Trace
  (
  -- * Tracing functions
    traceSignal
  , traceSignalN
  , traceVecSignal
  , traceVecSignalN
  , unsafeTraceSignal
  , unsafeTraceSignalN
  , unsafeTraceVecSignal
  , unsafeTraceVecSignalN
  , waitForTrace
  , waitForTraces
  , getTraces

  -- * VCD dump functions
  , VCDError(..)
  , fromVCDError
  , dumpVCD
  , dumpVCD'
  , dumpVCD''
  ) where

-- Clash:
import           Clash.Signal.Internal        (clockPeriod)
import           Clash.Signal                 (Signal, HiddenClockReset, bundle, unbundle, hasClock)
import           Clash.Sized.Vector           (Vec, iterateI)
import           Clash.Class.BitPack          (BitPack, BitSize, pack)
import           Clash.Prelude                (sample)
import qualified Clash.Prelude                as CP
import           Clash.Promoted.Nat           (snatToInteger, SNat(..))

-- Haskell / GHC:
import           Control.DeepSeq              (NFData, deepseq)
import           Control.Monad                (foldM)
import           Data.Bits                    (testBit)
import           Data.Char                    (ord, chr)
import           Data.IORef                   (IORef, atomicModifyIORef', newIORef, readIORef)
import           Data.List                    (foldl1', foldl', unzip4)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (fromMaybe, catMaybes)
import qualified Data.Text                    as Text
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (formatTime, defaultTimeLocale)
import           GHC.Stack                    (HasCallStack)
import           GHC.TypeLits                 (KnownNat, type (+))
import           System.IO.Unsafe             (unsafePerformIO)

type Period   = Int
type Changed  = Bool
type Value    = Integer
type Width    = Int
type TraceMap = Map.Map String (Period, Width, [Value])

traceMapIO :: IORef TraceMap
traceMapIO = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE traceMapIO #-}

mkTrace
  :: HiddenClockReset domain gated synchronous
  => HasCallStack
  => KnownNat (BitSize a)
  => BitPack a
  => NFData a
  => Signal domain a
  -> [Value]
mkTrace signal = map (toInteger . pack) (sample signal)


-- | Trace a single signal. Will emit error if a signal with the same name was
-- previously registered.
traceSignal
  :: forall domain gated synchronous a
   . HiddenClockReset domain gated synchronous
  => HasCallStack
  => KnownNat (BitSize a)
  => BitPack a
  => NFData a
  => String
  -- ^ Name of signal in debugging output
  -> Signal domain a
  -- ^ Signal to trace
  -> IO (Signal domain a)
traceSignal traceName signal =
  let width  = fromIntegral $ snatToInteger $ SNat @ (BitSize a) in
  let period = clockPeriod hasClock in
  atomicModifyIORef' traceMapIO $ \traceMap ->
    if Map.member traceName traceMap then
      error $ "'" ++ traceName ++ "' already in trace map."
    else
      (Map.insert traceName (period, width, mkTrace signal) traceMap, signal)
{-# NOINLINE traceSignal #-}

-- | Trace a single vector signal: each element in the vector will show up as
-- a different trace. If the trace name already exists, this function will emit
-- an error.
traceVecSignal
  :: HiddenClockReset domain gated synchronous
  => HasCallStack
  => KnownNat (BitSize a)
  => KnownNat n
  => BitPack a
  => NFData a
  => String
  -- ^ Name of signal in debugging output. Will be appended by _0, _1, ..., _n.
  -> Signal domain (Vec (n+1) a)
  -- ^ Signal to trace
  -> IO (Signal domain (Vec (n+1) a))
traceVecSignal vecTraceName (unbundle -> vecSignal) =
  fmap bundle $ sequence $ CP.zipWith trace' (iterateI succ (0 :: Int)) vecSignal
    where
      trace' i s = traceSignal (name' i) s
      name' i    = vecTraceName ++ "_" ++ show i
{-# NOINLINE traceVecSignal #-}

-- | Same as @traceSignal@, but append /_num/ to name.
traceSignalN
  :: forall domain gated synchronous a n
   . HiddenClockReset domain gated synchronous
  => HasCallStack
  => KnownNat (BitSize a)
  => BitPack a
  => NFData a
  => String
  -- ^ Name of signal in debugging output
  -> SNat n
  -- ^ Number appended to name
  -> Signal domain a
  -- ^ Signal to trace
  -> IO (Signal domain a)
traceSignalN traceName num =
  traceSignal (traceName ++ "_" ++ show (snatToInteger num))

-- | Same as @traceVecSignal@, but append /_num/ to name.
traceVecSignalN
  :: HiddenClockReset domain gated synchronous
  => HasCallStack
  => KnownNat (BitSize a)
  => KnownNat n
  => BitPack a
  => NFData a
  => String
  -- ^ Name of signal in debugging output. Will be appended by _0, _1, ..., _n.
  -> SNat n
  -- ^ Number appended to name
  -> Signal domain (Vec (n+1) a)
  -- ^ Signal to trace
  -> IO (Signal domain (Vec (n+1) a))
traceVecSignalN traceName num =
  traceVecSignal (traceName ++ "_" ++ show (snatToInteger num))

------------------------
-------- UNSAFE --------
------------------------
-- | Same as @traceSignal@, but without IO requirement. To use this safely, make
-- sure at least one value from this signal is evaluated before using dump
-- functions. Also see @waitForTraces@.
unsafeTraceSignal
  :: forall domain gated synchronous a
   . HiddenClockReset domain gated synchronous
  => HasCallStack
  => KnownNat (BitSize a)
  => BitPack a
  => NFData a
  => String
  -- ^ Name of signal in debugging output
  -> Signal domain a
  -- ^ Signal to trace
  -> Signal domain a
unsafeTraceSignal traceName signal =
  unsafePerformIO $ traceSignal traceName signal

-- | Same as @traceVecSignal@, but without IO requirement. To use this safely,
-- make sure at least one value from this signal is evaluated before using dump
-- functions. Also see @waitForTraces@.
unsafeTraceVecSignal
  :: HiddenClockReset domain gated synchronous
  => HasCallStack
  => KnownNat (BitSize a)
  => KnownNat n
  => BitPack a
  => NFData a
  => String
  -- ^ Name of signal in debugging output. Will be appended by _0, _1, ..., _n.
  -> Signal domain (Vec (n+1) a)
  -- ^ Signal to trace
  -> Signal domain (Vec (n+1) a)
unsafeTraceVecSignal traceName signal =
  unsafePerformIO $ traceVecSignal traceName signal

-- | Same as @traceSignalN@, but without IO requirement. To use this safely,
-- make sure at least one value from this signal is evaluated before using dump
-- functions. Also see @waitForTraces@.
unsafeTraceSignalN
  :: forall domain gated synchronous a n
   . HiddenClockReset domain gated synchronous
  => HasCallStack
  => KnownNat (BitSize a)
  => BitPack a
  => NFData a
  => String
  -- ^ Name of signal in debugging output
  -> SNat n
  -- ^ Number appended to name
  -> Signal domain a
  -- ^ Signal to trace
  -> Signal domain a
unsafeTraceSignalN traceName num signal =
  unsafePerformIO $ traceSignalN traceName num signal

-- | Same as @traceVecSignalN@, but without IO requirement. To use this safely,
-- make sure at least one value from this signal is evaluated before using dump
-- functions. Also see @waitForTraces@.
unsafeTraceVecSignalN
  :: HiddenClockReset domain gated synchronous
  => HasCallStack
  => KnownNat (BitSize a)
  => KnownNat n
  => BitPack a
  => NFData a
  => String
  -- ^ Name of signal in debugging output. Will be appended by _0, _1, ..., _n.
  -> SNat n
  -- ^ Number appended to name
  -> Signal domain (Vec (n+1) a)
  -- ^ Signal to trace
  -> Signal domain (Vec (n+1) a)
unsafeTraceVecSignalN traceName num signal =
  unsafePerformIO $ traceVecSignalN traceName num signal

waitForTrace'
  :: NFData a
  => [a]
  -> String
  -> IO [a]
waitForTrace' signal traceName = do
  traceMap <- readIORef traceMapIO
  if Map.member traceName traceMap then
    return signal
  else
    deepseq
      (head signal)
      (waitForTrace' (tail signal) traceName)

-- | Keep evaluating given signal until give trace name is present
waitForTrace
  :: HiddenClockReset domain gated synchronous
  => NFData a
  => Signal dom a
  -> String
  -> IO ()
waitForTrace signal traceName =
  waitForTraces signal [traceName]

-- | Keep evaluating given signal until all trace names are present.
waitForTraces
  :: HiddenClockReset domain gated synchronous
  => NFData a
  => Signal dom a
  -> [String]
  -> IO ()
waitForTraces signal traceNames = do
  rest <- foldM waitForTrace' (sample signal) traceNames
  return $ deepseq (head rest) ()

-------------------------
-------- DUMPING --------
-------------------------
newtype VCDError = VCDError String

fromVCDError :: VCDError -> String
fromVCDError (VCDError err) = err

iso8601Format :: UTCTime -> String
iso8601Format = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

toPeriodMap :: TraceMap -> Map.Map Period [(String, Width, [Value])]
toPeriodMap m = foldl' go Map.empty (Map.assocs m)
  where
    go periodMap (traceName, (period, width, values)) =
      Map.alter (Just . go') period periodMap
        where
          go' = ((traceName, width, values):) . (fromMaybe [])

flattenMap :: Map.Map a [b] -> [(a, b)]
flattenMap m = concat [[(a, b) | b <- bs] | (a, bs) <- Map.assocs m]

printable :: Char -> Bool
printable (ord -> c) = 33 <= c && c <= 126

-- | Obtain current traceMap. When using one of the unsafe trace functions, you
-- probably want to look at @waitForTraces@.
getTraces :: IO TraceMap
getTraces = readIORef traceMapIO

-- | Same as @dumpVCD@, but supplied with a custom tracemap and a custom timestamp
dumpVCD''
  :: HasCallStack
  => (Int, Int)
  -> TraceMap
  -> UTCTime
  -> Either VCDError Text.Text
dumpVCD'' (offset, cycles) traceMap now
  | offset < 0 =
      error $ "dumpVCD'': offset was " ++ show offset ++ ", but cannot be negative."
  | cycles < 0 =
      error $ "dumpVCD'': cycles was " ++ show cycles ++ ", but cannot be negative."
  | null traceMap =
      error $ "dumpVCD'': no traces found in tracemap. See function 'waitForTraces'."
  | Map.size traceMap > 126 - 33 =
      Left $ VCDError "Tracemap contains more than 93 traces, which is not supported by VCD."
  | not $ null $ offensiveNames =
      Left $ VCDError $ unwords [ "Trace '" ++ head offensiveNames ++ "' contains"
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
        normalize period values = concatMap (replicate period) values
        slice values = drop offset $ take cycles values

        -- TODO: Clash version number
        headerDate       = ["$date", Text.pack $ iso8601Format now, "$end"]
        headerVersion    = ["$version", "Generated by Clash 0.99", "$end"]
        headerComment    = ["$comment", "No comment", "$end"]
        headerTimescale  = ["$timescale", (show timescale) ++ "ps", "$end"]
        headerWires      = [Text.unwords $ headerWire w l n | (w, l, n) <- (zip3 widths labels traceNames)]
        headerWire w l n = map Text.pack ["$var wire", show w, [l], n, "$end"]
        initValues       = map Text.pack $ zipWith ($) formatters inits

        formatters = zipWith format widths labels
        inits = map head valuess'
        tails = map changed valuess'

        -- | Format single value according to VCD spec
        format :: Width -> Char -> Value -> String
        format 1 label 0   = ['0', label, '\n']
        format 1 label 1   = ['1', label, '\n']
        format 1 label val =
          error $ "Width of " ++ show label ++ " was " ++ show val
        format n label val =
          let b2b b = if b then '1' else '0' in
          "b" ++ map (b2b . testBit val) (reverse [0..n-1]) ++ " " ++ [label]

        -- | Given a list of values, return a list of list of bools indicating
        -- if a value changed. The first value is *not* included in the result.
        changed :: [Value] -> [(Changed, Value)]
        changed (s:ss) = zip (zipWith (/=) (s:ss) ss) ss
        changed []     = []

        bodyParts :: [Maybe Text.Text]
        bodyParts = zipWith go [0..] (map bodyPart tails)
          where
            go :: Int -> Maybe Text.Text -> Maybe Text.Text
            go (Text.pack . show -> n) t =
              let pre = Text.concat ["#", n, "\n"] in
              fmap (Text.append pre) t

        bodyPart :: [(Changed, Value)] -> Maybe Text.Text
        bodyPart values =
          let formatted  = [(c, f v) | (f, (c,v)) <- zip formatters values] in
          let formatted' = map (Text.pack . snd) $ filter fst $ formatted in
          if null formatted' then Nothing else Just $ Text.intercalate "\n" formatted'

-- | Same as @dumpVCD@, but supplied with a custom tracemap
dumpVCD'
  :: HasCallStack
  => (Int, Int)
  -> TraceMap
  -> IO (Either VCDError Text.Text)
dumpVCD' slice traceMap =
  fmap (dumpVCD'' slice traceMap) getCurrentTime

-- | Produce a four-state VCD (Value Change Dump) according to IEEE
-- 1364-{1995,2001}. This function fails if a trace name contains either
-- non-printable or non-VCD characters.
dumpVCD
  :: HasCallStack
  => (Int, Int)
  -- ^ (offset, number of samples)
  -> IO (Either VCDError Text.Text)
dumpVCD slice =
  dumpVCD' slice =<< getTraces
