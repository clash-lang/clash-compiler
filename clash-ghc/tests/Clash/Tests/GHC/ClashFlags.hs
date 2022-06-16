{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Tests.GHC.ClashFlags where

import Data.IORef (newIORef, readIORef)

import GHC (GenLocated(..), noLoc, unLoc)
#if MIN_VERSION_ghc(9,0,0)
import GHC.Driver.CmdLine (Warn(..))
import GHC.Utils.Misc (OverridingBool(Never, Auto))
#else
import CmdLineParser (Warn(..))
import Util (OverridingBool(Never, Auto))
#endif

import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)
import Test.Tasty.HUnit
import Test.Tasty.Runners (TestTree(..))
import Test.Tasty.TH

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Lens (Lens', (^.))

import Data.Text (Text)

import Clash.Driver.Types

import Clash.GHC.ClashFlags (parseClashFlags, renderClashFlags)

-- Enable usage of Hedgehog operators:
instance Show (GenLocated l e) where
  show _ = "<GenLocated>"
deriving instance Show Warn
deriving instance Eq Warn

-- | Parse given flags. Return leftover flags, warnings, and clash options.
runParse :: [String] -> IO ([String], [Warn], ClashOpts)
runParse args = do
  optsRef <- newIORef defClashOpts
  (leftovers, warns) <- parseClashFlags optsRef (map noLoc args)
  opts <- readIORef optsRef
  pure (map unLoc leftovers, warns, opts)

-- | Parse given flags, fail if any flags are left unparsed or if any warnings
-- are emitted.
runParseClean :: [String] -> IO ClashOpts
runParseClean args = do
  (leftovers, warns, opts) <- runParse args
  [] @=? leftovers
  assertBool "" (null warns)
  pure opts

runRoundtrip :: [String] -> Assertion
runRoundtrip args0 = do
  (_, _, opts0) <- runParse args0
  let args1 = renderClashFlags opts0
  (_, _, opts1) <- runParse args1
  opts0 @=? opts1

runRoundtripClean :: [String] -> Assertion
runRoundtripClean args0 = do
  opts0 <- runParseClean args0
  let args1 = renderClashFlags opts0
  opts1 <- runParseClean args1
  opts0 @=? opts1

-- | Parser should return 'defClashOpts' if no arguments are given.
case_parseNoArgs :: Assertion
case_parseNoArgs = do
  opts <- runParseClean []
  defClashOpts @=? opts

-- | Caching should be disabled through 'fclash-no-cache'
case_cache :: Assertion
case_cache = do
  opts <- runParseClean ["-fclash-no-cache"]
  defClashOpts{_opt_cachehdl=False} @=? opts

-- | Caching should be disabled through 'fclash-no-cache'
case_opposite_flags :: Assertion
case_opposite_flags = do
  opts <- runParseClean ["-fclash-no-cache", "-fclash-cache"]
  defClashOpts @=? opts

-- | Deprecated flags should yield a warning
case_deprecated :: Assertion
case_deprecated = do
  (leftovers, warns, _opts) <- runParse ["-fclash-clean"]
  [] @=? leftovers
  1 @=? length warns

-- | Non-existing flags should be passed through
case_nonExisting :: Assertion
case_nonExisting = do
  (leftovers, warns, _opts) <- runParse ["-fclash-non-existing"]
  ["-fclash-non-existing"] @=? leftovers
  0 @=? length warns

-- | Rendering default options should yield no flags
case_renderDefault :: Assertion
case_renderDefault = [] @=? renderClashFlags defClashOpts

-- | Test whether -Werror (a GHC flag, not a Clash flag despite being in
-- ClashOpts) is rendered upon setting it.
case_werrorRender :: Assertion
case_werrorRender = ["-Werror"] @=? renderClashFlags defClashOpts{_opt_werror=True}

-- | Test whether -Wcolor (a GHC flag, not a Clash flag despite being in
-- ClashOpts) is rendered upon setting it.
case_colorRender :: Assertion
case_colorRender =
  let opts = defClashOpts{_opt_color=Never} in
  ["-fdiagnostics-color=never"] @=? renderClashFlags opts

-- | Test whether we can set some values in `ClashOpt` and do a render/parse roundtrip
-- without losing information.
case_roundtripEmpty :: Assertion
case_roundtripEmpty = runRoundtripClean []

-- | Test whether we can set some values in `ClashOpt` and do a render/parse roundtrip
-- without losing information.
case_roundtripCache :: Assertion
case_roundtripCache = runRoundtripClean ["-fclash-cache"]

-- | Test whether we can set some values in `ClashOpt` and do a render/parse roundtrip
-- without losing information.
case_roundtripXopt :: Assertion
case_roundtripXopt = runRoundtripClean ["-fclash-aggressive-x-optimization"]

-- | Test whether we can set some values in `ClashOpt` and do a render/parse roundtrip
-- without losing information.
case_roundtripXoptDisable :: Assertion
case_roundtripXoptDisable =
  runRoundtripClean
    [ "-fclash-aggressive-x-optimization"
    , "-fclash-no-aggressive-x-optimization-blackboxes" ]

-- | Whether we want to generate a random value or use a default. This makes sure
-- 'genClashOpts' shrinks towards 'defClashOpts'. Furthermore,
data FuzzMode = Default | Fuzz
  deriving (Show, Eq, Enum, Bounded)

-- | Generate whether to fuzz or not. Shrinks towards 'Default'.
genFuzzMode :: H.Gen FuzzMode
genFuzzMode = Gen.enumBounded

-- | Return given generator if fuzz mode is set to 'Fuzz'. Else, return a constant
-- value retrieved with the given lens.
genWithFuzzMode :: H.Gen a -> Lens' ClashOpts a -> FuzzMode -> H.Gen a
genWithFuzzMode _genA opt Default = pure (defClashOpts ^. opt)
genWithFuzzMode genA _opt Fuzz = genA

genBool :: Lens' ClashOpts Bool -> FuzzMode -> H.Gen Bool
genBool = genWithFuzzMode Gen.enumBounded

genMaybe :: Lens' ClashOpts (Maybe a) -> H.Gen a -> FuzzMode -> H.Gen (Maybe a)
genMaybe opt genA = genWithFuzzMode (Gen.maybe genA) opt

genEnumBounded :: (Enum a, Bounded a) => Lens' ClashOpts a -> FuzzMode -> H.Gen a
genEnumBounded = genWithFuzzMode Gen.enumBounded

genPosInt :: Lens' ClashOpts Int -> FuzzMode -> H.Gen Int
genPosInt = genWithFuzzMode (Gen.int (Range.linear 0 maxBound))

genWord :: Lens' ClashOpts Word -> FuzzMode -> H.Gen Word
genWord =
  -- XXX: We currently only accept words from 0 to the max bound of Int. See
  --      'wordSuffix'.
  genWithFuzzMode (Gen.word (Range.linear 0 (fromIntegral (maxBound :: Int))))

genElement :: Lens' ClashOpts a -> [a] -> FuzzMode -> H.Gen a
genElement opt elements = genWithFuzzMode (Gen.element elements) opt

genSimpleString :: H.Gen String
genSimpleString = Gen.string (Range.linear 1 10) Gen.lower

genSimpleText :: H.Gen Text
genSimpleText = Gen.text (Range.linear 1 10) Gen.lower

genClashOpts :: HasCallStack => _ -> H.Gen ClashOpts
genClashOpts
  -- Fuzz arguments are supplied externally for nicer error reporting
  ( _opt_aggressiveXOpt_fuzz
  , _opt_aggressiveXOptBB_fuzz
  , _opt_cachehdl_fuzz
  , _opt_checkIDir_fuzz
  , _opt_clear_fuzz
  , _opt_componentPrefix_fuzz
  , _opt_edalize_fuzz
  , _opt_errorExtra_fuzz
  , _opt_escapedIds_fuzz
  , _opt_evaluatorFuelLimit_fuzz
  , _opt_forceUndefined_fuzz
  , _opt_hdlDir_fuzz
  , _opt_hdlSyn_fuzz
  , _opt_inlineConstantLimit_fuzz
  , _opt_inlineFunctionLimit_fuzz
  , _opt_inlineLimit_fuzz
  , _opt_inlineWFCacheLimit_fuzz
  , _opt_intWidth_fuzz
  , _opt_lowerCaseBasicIds_fuzz
  , _opt_newInlineStrat_fuzz
  , _opt_primWarn_fuzz
  , _opt_renderEnums_fuzz
  , _opt_specLimit_fuzz
  , _opt_ultra_fuzz
  , _dbg_countTransformations_fuzz
  , _dbg_historyFile_fuzz
  , _dbg_invariants_fuzz
  , _dbg_transformationInfo_fuzz
  , _dbg_transformations_fuzz
  , _dbg_transformationsFrom_fuzz
  , _dbg_transformationsLimit_fuzz ) = do
  let
    _opt_werror = False
    _opt_color  = Auto
    _opt_importPaths = []

  _opt_aggressiveXOpt      <- gen_opt_aggressiveXOpt      _opt_aggressiveXOpt_fuzz
  _opt_aggressiveXOptBB    <- gen_opt_aggressiveXOptBB    _opt_aggressiveXOptBB_fuzz
  _opt_cachehdl            <- gen_opt_cachehdl            _opt_cachehdl_fuzz
  _opt_checkIDir           <- gen_opt_checkIDir           _opt_checkIDir_fuzz
  _opt_clear               <- gen_opt_clear               _opt_clear_fuzz
  _opt_componentPrefix     <- gen_opt_componentPrefix     _opt_componentPrefix_fuzz
  _opt_edalize             <- gen_opt_edalize             _opt_edalize_fuzz
  _opt_errorExtra          <- gen_opt_errorExtra          _opt_errorExtra_fuzz
  _opt_escapedIds          <- gen_opt_escapedIds          _opt_escapedIds_fuzz
  _opt_evaluatorFuelLimit  <- gen_opt_evaluatorFuelLimit  _opt_evaluatorFuelLimit_fuzz
  _opt_forceUndefined      <- gen_opt_forceUndefined      _opt_forceUndefined_fuzz
  _opt_hdlDir              <- gen_opt_hdlDir              _opt_hdlDir_fuzz
  _opt_hdlSyn              <- gen_opt_hdlSyn              _opt_hdlSyn_fuzz
  _opt_inlineConstantLimit <- gen_opt_inlineConstantLimit _opt_inlineConstantLimit_fuzz
  _opt_inlineFunctionLimit <- gen_opt_inlineFunctionLimit _opt_inlineFunctionLimit_fuzz
  _opt_inlineLimit         <- gen_opt_inlineLimit         _opt_inlineLimit_fuzz
  _opt_inlineWFCacheLimit  <- gen_opt_inlineWFCacheLimit  _opt_inlineWFCacheLimit_fuzz
  _opt_intWidth            <- gen_opt_intWidth            _opt_intWidth_fuzz
  _opt_lowerCaseBasicIds   <- gen_opt_lowerCaseBasicIds   _opt_lowerCaseBasicIds_fuzz
  _opt_newInlineStrat      <- gen_opt_newInlineStrat      _opt_newInlineStrat_fuzz
  _opt_primWarn            <- gen_opt_primWarn            _opt_primWarn_fuzz
  _opt_renderEnums         <- gen_opt_renderEnums         _opt_renderEnums_fuzz
  _opt_specLimit           <- gen_opt_specLimit           _opt_specLimit_fuzz
  _opt_ultra               <- gen_opt_ultra               _opt_ultra_fuzz

  _dbg_countTransformations <- gen_dbg_countTransformations _dbg_countTransformations_fuzz
  _dbg_historyFile          <- gen_dbg_historyFile          _dbg_historyFile_fuzz
  _dbg_invariants           <- gen_dbg_invariants           _dbg_invariants_fuzz
  _dbg_transformationInfo   <- gen_dbg_transformationInfo   _dbg_transformationInfo_fuzz
  _dbg_transformations      <- gen_dbg_transformations      _dbg_transformations_fuzz
  _dbg_transformationsFrom  <- gen_dbg_transformationsFrom  _dbg_transformationsFrom_fuzz
  _dbg_transformationsLimit <- gen_dbg_transformationsLimit _dbg_transformationsLimit_fuzz

  pure ClashOpts{_opt_debug=DebugOpts{..}, ..}
 where
  gen_opt_aggressiveXOpt = genBool opt_aggressiveXOpt
  gen_opt_aggressiveXOptBB = genBool opt_aggressiveXOptBB
  gen_opt_cachehdl = genBool opt_cachehdl
  gen_opt_checkIDir = genBool opt_checkIDir
  gen_opt_clear = genBool opt_clear
  gen_opt_componentPrefix = genMaybe opt_componentPrefix genSimpleText
  gen_opt_edalize = genBool opt_edalize
  gen_opt_errorExtra = genBool opt_errorExtra
  gen_opt_escapedIds = genBool opt_escapedIds
  gen_opt_evaluatorFuelLimit = genWord opt_evaluatorFuelLimit
  gen_opt_forceUndefined = genMaybe opt_forceUndefined (Gen.maybe (Gen.element [0, 1]))
  gen_opt_hdlDir = genMaybe opt_hdlDir genSimpleString
  gen_opt_inlineConstantLimit = genWord opt_inlineConstantLimit
  gen_opt_inlineFunctionLimit = genWord opt_inlineFunctionLimit
  gen_opt_inlineLimit = genPosInt opt_inlineLimit
  gen_opt_inlineWFCacheLimit = genWord opt_inlineWFCacheLimit
  gen_opt_intWidth = genElement opt_intWidth [32, 64]
  gen_opt_lowerCaseBasicIds = genEnumBounded opt_lowerCaseBasicIds
  gen_opt_primWarn = genBool opt_primWarn
  gen_opt_renderEnums = genBool opt_renderEnums
  gen_opt_ultra = genBool opt_ultra
  gen_opt_newInlineStrat = genBool opt_newInlineStrat
  gen_opt_specLimit = genEnumBounded opt_specLimit
  gen_opt_hdlSyn = genEnumBounded opt_hdlSyn

  gen_dbg_countTransformations = genBool (opt_debug . dbg_countTransformations)
  gen_dbg_historyFile = genMaybe (opt_debug . dbg_historyFile) genSimpleString
  gen_dbg_invariants = genBool (opt_debug . dbg_invariants)
  gen_dbg_transformationInfo = genEnumBounded (opt_debug . dbg_transformationInfo)
  gen_dbg_transformationsFrom = genMaybe (opt_debug . dbg_transformationsFrom) (Gen.word Range.constantBounded)
  gen_dbg_transformationsLimit = genMaybe (opt_debug . dbg_transformationsLimit) (Gen.word Range.constantBounded)

  -- TODO
  gen_dbg_transformations _fuzz = pure mempty

-- | Generate a 'ClashOpts' structure
hprop_parseRender :: H.Property
hprop_parseRender = H.property $ do
  -- We generate all fuzz modes here (instead of in 'genClashOpts') so Hedgehog
  -- can print which properties are fuzzed. After shrinking we only expect the
  -- "broken" flags to be set to 'Fuzz'.
  _opt_aggressiveXOpt_fuzz      <- H.forAll genFuzzMode
  _opt_aggressiveXOptBB_fuzz    <- H.forAll genFuzzMode
  _opt_cachehdl_fuzz            <- H.forAll genFuzzMode
  _opt_checkIDir_fuzz           <- H.forAll genFuzzMode
  _opt_clear_fuzz               <- H.forAll genFuzzMode
  _opt_componentPrefix_fuzz     <- H.forAll genFuzzMode
  _opt_edalize_fuzz             <- H.forAll genFuzzMode
  _opt_errorExtra_fuzz          <- H.forAll genFuzzMode
  _opt_escapedIds_fuzz          <- H.forAll genFuzzMode
  _opt_evaluatorFuelLimit_fuzz  <- H.forAll genFuzzMode
  _opt_forceUndefined_fuzz      <- H.forAll genFuzzMode
  _opt_hdlDir_fuzz              <- H.forAll genFuzzMode
  _opt_hdlSyn_fuzz              <- H.forAll genFuzzMode
  _opt_inlineConstantLimit_fuzz <- H.forAll genFuzzMode
  _opt_inlineFunctionLimit_fuzz <- H.forAll genFuzzMode
  _opt_inlineLimit_fuzz         <- H.forAll genFuzzMode
  _opt_inlineWFCacheLimit_fuzz  <- H.forAll genFuzzMode
  _opt_intWidth_fuzz            <- H.forAll genFuzzMode
  _opt_lowerCaseBasicIds_fuzz   <- H.forAll genFuzzMode
  _opt_newInlineStrat_fuzz      <- H.forAll genFuzzMode
  _opt_primWarn_fuzz            <- H.forAll genFuzzMode
  _opt_renderEnums_fuzz         <- H.forAll genFuzzMode
  _opt_specLimit_fuzz           <- H.forAll genFuzzMode
  _opt_ultra_fuzz               <- H.forAll genFuzzMode

  _dbg_countTransformations_fuzz <- H.forAll genFuzzMode
  _dbg_historyFile_fuzz          <- H.forAll genFuzzMode
  _dbg_invariants_fuzz           <- H.forAll genFuzzMode
  _dbg_transformationInfo_fuzz   <- H.forAll genFuzzMode
  _dbg_transformations_fuzz      <- H.forAll genFuzzMode
  _dbg_transformationsFrom_fuzz  <- H.forAll genFuzzMode
  _dbg_transformationsLimit_fuzz <- H.forAll genFuzzMode

  opts0 <- H.forAll $
    genClashOpts
      ( _opt_aggressiveXOpt_fuzz
      , _opt_aggressiveXOptBB_fuzz
      , _opt_cachehdl_fuzz
      , _opt_checkIDir_fuzz
      , _opt_clear_fuzz
      , _opt_componentPrefix_fuzz
      , _opt_edalize_fuzz
      , _opt_errorExtra_fuzz
      , _opt_escapedIds_fuzz
      , _opt_evaluatorFuelLimit_fuzz
      , _opt_forceUndefined_fuzz
      , _opt_hdlDir_fuzz
      , _opt_hdlSyn_fuzz
      , _opt_inlineConstantLimit_fuzz
      , _opt_inlineFunctionLimit_fuzz
      , _opt_inlineLimit_fuzz
      , _opt_inlineWFCacheLimit_fuzz
      , _opt_intWidth_fuzz
      , _opt_lowerCaseBasicIds_fuzz
      , _opt_newInlineStrat_fuzz
      , _opt_primWarn_fuzz
      , _opt_renderEnums_fuzz
      , _opt_specLimit_fuzz
      , _opt_ultra_fuzz
      , _dbg_countTransformations_fuzz
      , _dbg_historyFile_fuzz
      , _dbg_invariants_fuzz
      , _dbg_transformationInfo_fuzz
      , _dbg_transformations_fuzz
      , _dbg_transformationsFrom_fuzz
      , _dbg_transformationsLimit_fuzz )

  let flags = renderClashFlags opts0
  H.footnote ("flags: " <> show flags)

  (leftovers, warns, opts1) <- H.evalIO (runParse flags)

  [] H.=== leftovers
  [] H.=== warns

  H.diff opts0 (==) opts1

testCases :: [TestTree]
testCases =
  case $(testGroupGenerator) of
    TestGroup _ tree -> tree
    _ -> error "Internal error"

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.GHC.ClashFlags"
    [ -- tasty-th doesn't account for Hedgehog tests
      testGroup "cases" testCases
    , testGroup "randomized"
      [ testPropertyNamed
          "render/parse are each others inverses"
          "hprop_parseRender"
          hprop_parseRender
      ]
    ]
