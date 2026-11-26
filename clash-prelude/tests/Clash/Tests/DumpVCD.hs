{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Tests.DumpVCD where

import Control.Monad (foldM)
import Control.Monad.Morph (generalize, hoist)
import Data.Binary (encode)
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)
import Type.Reflection (typeRep)

import Clash.Signal.Trace
import Clash.Sized.BitVector (BitVector, size#)
import Hedgehog.Extra (LockstepWalk (..), combinations, lockstepWalk)

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type SignalData = BitVector 32

failWithMsg :: (H.MonadTest m, HasCallStack) => String -> m a
failWithMsg msg = withFrozenCallStack $ H.annotate msg >> H.failure

data Metadata
  = Metadata
  { timeScale :: VCDTime
  , nameMap :: Map String IDCode
  }
  deriving (Show)

parseMetadata ::
  [DeclarationCommand] ->
  Either String Metadata
parseMetadata dcs = go Nothing Nothing dcs
 where
  go Nothing m (TimeScale t u : dcs0) = do
    let
      scale u0 =
        case u0 of
          S -> (* 1000) <$> scale MS
          MS -> (* 1000) <$> scale US
          US -> (* 1000) <$> scale NS
          NS -> (* 1000) <$> scale PS
          PS -> Right t
          FS -> Left "$timescale fs unsupported; raw time in ps"
    t0 <- scale u
    go (Just t0) m dcs0
  go ts Nothing (Vars vs : dcs0) = do
    map0 <- goVars Map.empty vs
    go ts (Just map0) dcs0
  go (Just ts) (Just m) [] = Right $ Metadata ts m
  go Nothing _ [] = Left "No $timescale in declarations"
  go _ Nothing [] = Left "No $scope section in declarations"
  go _ _ (TimeScale _ _ : _) = Left "Multiple $timescale sections in declarations"
  go _ _ (Vars _ : _) =
    -- This is not an error in VCD files, but we don't emit it so we don't handle it.
    Left "Don't know what to do with multiple $scope sections in declarations"

  goVars map0 [] = Right map0
  goVars map0 (Var{..} : vs)
    | Map.member varReference map0 =
        Left $ "Duplicate $var section for variable named " <> show varReference
    | otherwise = goVars (Map.insert varReference varIDCode map0) vs

-- Are time stamps in order? Do they start at 0? Is every time stamp followed by actual
-- data?
--
-- We exploit knowledge about how @dumpVCD@ formats the resulting file. The @fixedStart@
-- is not mandated by VCD, it's just what @dumpVCD@ currently outputs.
saneTimestamps ::
  (H.MonadTest m) =>
  LockstepWalk m SimulationCommand
saneTimestamps = LockstepWalk $ fixedStart (0 :: Int) []
 where
  fixedStart 3 [SimulationTime t0, DumpVars _, SimulationTime t1] end sc = do
    H.diff t1 (>=) 0
    H.diff t0 (<) t1
    go True t1 end sc
  fixedStart 3 starts _ _ =
    failWithMsg $
      "Simulation doesn't start with proper $dumpvars\n\n"
        <> "The simulation starts with:\n"
        <> show starts
  fixedStart n starts False sc = pure $ LockstepWalk $ fixedStart (n + 1) (starts ++ [sc])
  fixedStart _ _ True _ = failWithMsg "Simulation ends prematurely"

  go _ t0 _ (SimulationValueChange _) = pure $ LockstepWalk $ go False t0
  go False t0 False (SimulationTime t1) = H.diff t0 (<) t1 >> pure (LockstepWalk $ go True t1)
  go True t0 _ (SimulationTime _) =
    failWithMsg $ "At time " <> show t0 <> ": Timestamp without change data"
  go _ _ True sc@(SimulationTime _) =
    -- A simulation can't end with a timestamp, a timestamp is always followed by values.
    failWithMsg $ "The final simulation command is " <> show sc
  go _ _ _ (DumpVars _) =
    -- This is not an error in VCD files, but we don't emit it so we don't handle it.
    failWithMsg $ "Don't know what to do with multiple $dumpvars sections"

-- The signal changes every period. Are all the samples there, and are there no extra
-- samples?
signalCorrect ::
  forall m.
  (H.MonadTest m, HasCallStack) =>
  String ->
  IDCode ->
  VCDTime ->
  VCDTime ->
  [Value] ->
  LockstepWalk m SimulationCommand
signalCorrect name idCode period end es = LockstepWalk initF
 where
  initF _ (SimulationTime t) =
    pure $ LockstepWalk $ go t (zip (-1 : [0, period .. end]) es)
  initF _ sc =
    failWithMsg $ "First simulation command is not SimulationTime but " <> show sc

  go t es0 lastC (SimulationValueChange (ValueChange _ idCode0 v))
    | idCode /= idCode0 = checkFinal lastC es0 >> pure (LockstepWalk $ go t es0)
    | otherwise = do
        es1 <- parseChange t es0 idCode0 v
        checkFinal lastC es1
        pure $ LockstepWalk $ go t es1
  go _ es0 _ (SimulationTime t) = pure $ LockstepWalk $ go t es0
  go t es0 _ (DumpVars vars) = do
    es2 <-
      foldM
        (\es1 (ValueChange _ idCode0 val) -> parseChange t es1 idCode0 val)
        es0
        vars
    pure $ LockstepWalk $ go t es2

  checkFinal False _ = pure ()
  checkFinal True ((t, _) : _) = do
    annotateSignal
    failWithMsg $ "Sample at time " <> show t <> " never came"
  checkFinal True _ = pure ()

  parseChange _ es0 idCode0 _
    | idCode /= idCode0 = pure es0
  parseChange t (e : es1) _ v
    | (t, v) == e = pure es1
    | otherwise = do
        annotateSignal
        (t, v) H.=== e
        H.failure
  parseChange _ [] _ _ = do
    annotateSignal
    failWithMsg $ "Too many samples in VCD file"

  annotateSignal :: (HasCallStack) => m ()
  annotateSignal =
    withFrozenCallStack $
      H.annotate $
        "Signal name: "
          <> show name
          <> "\n"
          <> "Signal identifier code: "
          <> show idCode

{- | Check timing of individual signals when tracing multiple domains

If we have multiple signals, each with their own period, that change every sample, does
the resulting VCD contain all the samples at the right times, and do timestamps and value
changes strictly alternate?

This test would have caught the issue that PR #3076 fixed, and serves as a good
basic sanity check of VCD generation.
-}
multiDomainTrace :: H.Property
multiDomainTrace =
  H.property $
    -- By default, property tests run in @PropertyT IO@. But we do a /lot/ of binds, and
    -- I'm unsure whether binds have an inherent cost in 'IO'. It doesn't seem like it,
    -- but running in 'IO' also doesn't give us anything.
    -- So we run in @PropertyT Identity@ instead.
    hoist generalize $ do
      -- It seems silly to test a single signal, but we do want it as a shrink option just
      -- in case the error occurs even for a single signal.
      k <-
        H.forAll $
          Gen.shrink (\x -> if x > 1 then [1] else []) $
            Gen.int $
              Range.linear 2 4
      -- @common@ scales all periods by a common power of ten factor
      common <- H.forAll $ fmap (10 ^) $ Gen.integral_ $ Range.constant (0 :: Int) 4
      -- Interesting behavior of multiple signals is captured by the relation between them.
      -- The relation is defined by the ratios between the periods, which depends only on
      -- the prime factors they do not share. Hence, primes are excellent candidates. It
      -- feels prudent to also include 1 as the other extreme, where for /every/ sample of
      -- a slower signal, there is also a new sample of the fastest signal.
      periodFactors <- H.forAll $ combinations [1, 2, 3, 5, 7, 11, 13, 17, 19, 23] k
      -- Maybe add one more signal in one of the domains
      duplSignal <- H.forAll $ Gen.maybe $ Gen.integral_ $ Range.constant 0 (k - 1)
      let
        -- After @product periods@ time, the timing starts to repeat, so this seems a good
        -- time to stop.
        --
        -- Maximum duration is about 10,000 samples with the list of primes above. That
        -- does make the test run pretty long, though. It seems unlikely that an anomaly
        -- would show up only after 1,000 samples.
        --
        -- The end of sampling is defined pretty weirdly in 'dumpVCD' currently, but with
        -- this definition of @sampleDuration@, we should have enough samples and not
        -- oversample too much.
        sampleDuration = min 1000 $ product periodFactors + 10
        signalNames =
          take k ["a", "b", "c", "d"] ++ if isJust duplSignal then ["e"] else []
        periods = map (* common) periodFactors
        periods0
          | Just i <- duplSignal = periods ++ [periods !! i]
          | otherwise = periods
        traceMap = Map.fromAscList $ zipWith (\n p -> (n, oneTrace p)) signalNames periods0
      VCDFile vcdDecs vcdSims <- H.evalEither $ dumpVCD1# (0, sampleDuration) traceMap
      Metadata{..} <- H.evalEither $ parseMetadata vcdDecs
      Map.keysSet nameMap H.=== Map.keysSet traceMap
      -- 'dumpVCD' currently outputs timestamp 0 twice: once for $dumpvars and again for
      -- the first real value changes. But this is probably a bug; the standard doesn't
      -- say, and GTKWave and Surfer accept it, but it sounds like it's not well-formed.
      -- For now, we alter the timestamp for $dumpvars. The timestamp -1 is not valid in a
      -- VCD file, but we can match on it in our tests.
      vcdSims0 <-
        case L.uncons vcdSims of
          Just (sc, vcdSims1) ->
            sc H.=== SimulationTime 0 >> pure (SimulationTime (-1) : vcdSims1)
          Nothing ->
            failWithMsg "VCDFile has no SimulationCommands"
      let
        -- The end of sampling is defined pretty weirdly in 'dumpVCD' currently
        end = sampleDuration - 2
        signalCorrects =
          map
            ( \(name, (idCode, period, vs)) ->
                signalCorrect name idCode (period `div` timeScale) end vs
            )
            $ Map.assocs
            $ Map.intersectionWith
              (\idCode (_, period, _, vs) -> (idCode, period, vs))
              nameMap
              traceMap
      lockstepWalk (saneTimestamps : signalCorrects) vcdSims0
 where
  oneTrace period =
    ( encode (typeRep @SignalData)
    , period
    , size# (0 :: SignalData)
    , map (0,) [0 ..]
    )

tests :: TestTree
tests =
  testGroup
    "DumpVCD"
    [ testPropertyNamed "multiDomainTrace" "multiDomainTrace" $ multiDomainTrace
    ]
