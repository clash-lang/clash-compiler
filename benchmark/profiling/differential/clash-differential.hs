{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Runs a comparison of "Comparisons" on serialized designs: the inputs of
  normalization (@*.bin@, see @clash-profile-normalization-prepare@), or of
  netlist generation, i.e. normalized bindings (@*.net.bin@, see
  @clash-profile-netlist-prepare@). Or runs a comparison at the sites
  captured while normalizing designs (@*.corpus@, see
  "Clash.Rewrite.Capture"). See README.md.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (forM, forM_, unless, when)
import Data.Binary (decode)
import qualified Data.ByteString.Lazy as B
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (getFileSize)
import System.FilePath ((</>), takeFileName)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode (..))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Process (StdStream (CreatePipe), createProcess, proc, std_out)
import Text.Read (readMaybe)

import Clash.Core.Name (nameOcc)
import Clash.Core.TyCon (TyConMap)
import Clash.Core.Scoped.TypeEnv (mkTypeEnv)
import Clash.Core.Scoped.Syntax (globalIdName, globalIdLegacy)
import Clash.Core.TermInfo (termSizeSmallerThan)
import Clash.Core.Var (varName)
import Clash.Core.VarEnv (mapMaybeVarEnv, mkVarSet)
import Clash.Driver.Types
  (Binding (..), ClashEnv (..), ClashOpts (..), defClashOpts)
-- For the orphan 'Binary' instance of 'TopEntityT'
import Clash.Driver.Manifest ()
import Clash.Netlist.Types (TopEntityT (topId))
import Clash.Rewrite.Capture
import Clash.Rewrite.Types (RewriteEnv (..))

import Clash.GHC.Evaluator (evaluator)
import Clash.GHC.NetlistTypes (ghcTypeToHWType)

import SerialiseInstances
  (NetlistInputs, NormalizationInputs, decodeCorpus, unremoveBBfunc)

import Comparisons (comparisons, siteComparisons)
import Registry (currentRewrites, lookupCurrent)
import Test.Clash.Differential
import Test.Clash.Differential.Sites
import Prims (PrimsOptions (..), defPrimsOptions, comparePrims)
import WhnfReplay (ReplayOptions (..), whnfReplay)
import Normalized (NormOptions (..), compareNormalized)

data Options = Options
  { optShow :: Int
  , optLines :: Int
  , optWidth :: Int
  , optBinding :: Maybe String
  , optTimeout :: Int
  -- ^ Seconds
  , optTransformation :: Maybe String
  }

defOptions :: Options
defOptions = Options 3 30 200 Nothing 20 Nothing

usage :: String
usage = unlines
  [ "Usage: clash-differential [OPTIONS] COMPARISON FILE..."
  , "       clash-differential --sites [OPTIONS] COMPARISON CORPUS..."
  , "       clash-differential --replay [OPTIONS] CORPUS..."
  , "       clash-differential --corpus-stats CORPUS..."
  , "       clash-differential --list"
  , "       clash-differential whnf [--reps N] [--show N] [--top N] [--call N]"
  , "                               [--prims] [--check-prims] DESIGN.bin DUMP..."
  , "       clash-differential prims [--count N] [--seed N] [--show N] [--prim NAME] [--open] DESIGN.bin..."
  , "       clash-differential normalized [--show N] DIR_A DIR_B DESIGN.bin..."
  , ""
  , "Runs COMPARISON on every binding of the serialized designs FILE..., which"
  , "are *.bin files written by clash-profile-normalization-prepare, or"
  , "*.net.bin files written by clash-profile-netlist-prepare."
  , ""
  , "With --sites, runs a site comparison at the sites at which its"
  , "transformation fired while normalizing the designs, captured in the"
  , "*.corpus files that clash-profile-normalization-run writes when"
  , "CLASH_CAPTURE_REWRITES is set. With --replay, reruns every captured"
  , "transformation at its sites, and reports where it doesn't reproduce the"
  , "captured result. With --corpus-stats, counts the captures."
  , ""
  , "Options:"
  , "  --show N        print the first N differences per design (default 3)"
  , "  --lines N       print at most N lines of a term (default 30)"
  , "  --width N       print at most N characters of a line (default 200)"
  , "  --binding NAME  only compare the bindings whose name contains NAME;"
  , "                  for sites: the sites in such bindings"
  , "  --timeout S     sites: give up on a side after S seconds (default 20)"
  , "  --transformation NAME"
  , "                  --replay: only replay this transformation"
  , ""
  , "The whnf command replays the whnfRW calls in DUMP with the evaluators of"
  , "Clash Core and of scoped Clash Core, with the global bindings of"
  , "DESIGN.bin, compares their results and, with +RTS -T, measures their"
  , "allocation (--reps runs, default 3), and shows the --top N calls with the"
  , "most extra allocation in the scoped evaluator. --call N only replays the"
  , "call with index N. --prims reports how often every primitive was reduced"
  , "by a ported reduction and through the adapter; --check-prims also compares"
  , "every call of a ported reduction with the adapter. See WhnfReplay.hs."
  , ""
  , "The normalized command compares the normalized bindings of DESIGN.bin...,"
  , "that clash-profile-normalization-run writes with CLASH_NORMALIZED_OUT set,"
  , "in DIR_A and DIR_B (DIR/<design>.norm.bin), e.g. of the normalizer of"
  , "Clash Core and that of scoped Clash Core. Globals are matched by where"
  , "they are used, from the top entity on. See Normalized.hs."
  , ""
  , "The prims command compares every ported reduction of a primitive with the"
  , "reduction of Clash Core, through the adapter, on --count N (default 100)"
  , "calls generated from the type of the primitive in the designs. With"
  , "--open, arguments it can't generate are opaque values. See Prims.hs."
  ]

main :: IO ()
main = getArgs >>= \case
  "whnf" : args -> case parseWhnfArgs (ReplayOptions 3 3 0 Nothing False False) args of
    Just (opts, design : dumps@(_:_)) -> do
      (bm, tcm, _, _, _, _, _, _) <- decode <$> B.readFile design :: IO NormalizationInputs
      forM_ dumps (whnfReplay opts tcm bm)
    _ -> die usage
  "prims" : args -> case parsePrimsArgs defPrimsOptions args of
    Just (opts, designs@(_:_)) -> do
      inputs <- forM designs $ \design -> do
        (bm, tcm, _, _, _, _, _, _) <- decode <$> B.readFile design :: IO NormalizationInputs
        pure (design, bm, tcm)
      ok <- comparePrims opts inputs
      unless ok exitFailure
    _ -> die usage
  "normalized" : args -> case parseArgs defOptions args of
    Just (opts, dirA : dirB : designs@(_:_)) -> do
      oks <- forM designs $ \design -> do
        let base = takeFileName (dropBinSuffix design)
            nopts = NormOptions (optShow opts) (optLines opts) (optWidth opts)
        compareNormalized nopts normalizedKnownDifferences design
          (dirA </> base <> ".norm.bin") (dirB </> base <> ".norm.bin")
      unless (and oks) exitFailure
    _ -> die usage
  ["--list"] -> do
    putStrLn "Comparisons on bindings:"
    forM_ comparisons $ \(Comparison nm desc _ _ kds) -> do
      putStrLn ("  " <> nm <> ": " <> desc)
      forM_ kds $ \kd -> putStrLn ("    known difference " <> kdName kd <> ": " <> kdDescription kd)
    putStrLn "Comparisons at captured sites (--sites):"
    forM_ siteComparisons $ \sc -> do
      putStrLn ("  " <> scName sc <> ": " <> scDescription sc)
      forM_ (scKnownDifferences sc) $ \kd ->
        putStrLn ("    known difference " <> kdName kd <> ": " <> kdDescription kd)
  "--sites" : args -> case parseArgs defOptions args of
    Just (opts, nm : files@(_:_)) -> case List.find ((== nm) . scName) siteComparisons of
      Just sc -> runSites opts sc files
      Nothing -> die ("Unknown site comparison: " <> nm <> ", see --list")
    _ -> die usage
  "--replay" : args -> case parseArgs defOptions args of
    Just (opts, files@(_:_)) -> runReplay opts files
    _ -> die usage
  "--corpus-stats" : files@(_:_) -> corpusStats files
  args -> case parseArgs defOptions args of
    Just (opts, nm : files@(_:_)) -> case List.find (\c -> name c == nm) comparisons of
      Just cmp -> run opts cmp files
      Nothing -> die ("Unknown comparison: " <> nm <> ", see --list")
    _ -> die usage
 where
  name (Comparison nm _ _ _ _) = nm

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitWith (ExitFailure 2)

-- | The options of the whnf command, and the other arguments
parseWhnfArgs :: ReplayOptions -> [String] -> Maybe (ReplayOptions, [String])
parseWhnfArgs opts = \case
  [] -> Just (opts, [])
  "--reps" : n : rest -> readMaybe n >>= \k -> parseWhnfArgs opts {roReps = k} rest
  "--show" : n : rest -> readMaybe n >>= \k -> parseWhnfArgs opts {roShow = k} rest
  "--top" : n : rest -> readMaybe n >>= \k -> parseWhnfArgs opts {roTop = k} rest
  "--call" : n : rest -> readMaybe n >>= \k -> parseWhnfArgs opts {roOnly = Just k} rest
  "--prims" : rest -> parseWhnfArgs opts {roPrims = True} rest
  "--check-prims" : rest -> parseWhnfArgs opts {roCheckPrims = True} rest
  a : rest
    | "--" `List.isPrefixOf` a -> Nothing
    | otherwise -> fmap (a :) <$> parseWhnfArgs opts rest

-- | The options of the prims command, and the other arguments
parsePrimsArgs :: PrimsOptions -> [String] -> Maybe (PrimsOptions, [String])
parsePrimsArgs opts = \case
  [] -> Just (opts, [])
  "--count" : n : rest -> readMaybe n >>= \k -> parsePrimsArgs opts {poCount = k} rest
  "--seed" : n : rest -> readMaybe n >>= \k -> parsePrimsArgs opts {poSeed = k} rest
  "--show" : n : rest -> readMaybe n >>= \k -> parsePrimsArgs opts {poShow = k} rest
  "--prim" : nm : rest -> parsePrimsArgs opts {poPrim = Just nm} rest
  "--open" : rest -> parsePrimsArgs opts {poOpen = True} rest
  a : rest
    | "--" `List.isPrefixOf` a -> Nothing
    | otherwise -> fmap (a :) <$> parsePrimsArgs opts rest

-- | Options, which can be anywhere, and the other arguments
parseArgs :: Options -> [String] -> Maybe (Options, [String])
parseArgs opts = \case
  [] -> Just (opts, [])
  "--show" : n : rest -> readMaybe n >>= \k -> parseArgs opts {optShow = k} rest
  "--lines" : n : rest -> readMaybe n >>= \k -> parseArgs opts {optLines = k} rest
  "--width" : n : rest -> readMaybe n >>= \k -> parseArgs opts {optWidth = k} rest
  "--binding" : nm : rest -> parseArgs opts {optBinding = Just nm} rest
  "--timeout" : n : rest -> readMaybe n >>= \k -> parseArgs opts {optTimeout = k} rest
  "--transformation" : nm : rest -> parseArgs opts {optTransformation = Just nm} rest
  a : rest
    | "--" `List.isPrefixOf` a -> Nothing
    | otherwise -> fmap (a :) <$> parseArgs opts rest

run :: Options -> Comparison -> [FilePath] -> IO ()
run opts cmp files = do
  summaries <- forM files $ \f -> do
    d <- loadDesign opts f
    s <- compareDesign (optShow opts) cmp d
    forM_ (sumMismatches s) $ \(i, m) ->
      putStr (renderMismatch (optLines opts) (optWidth opts) i m)
    putStrLn (renderSummary f s)
    hFlush stdout
    pure s
  when (length files > 1) $
    putStrLn (renderSummary "total" (mconcat summaries) {sumMismatches = []})
  unless (all ((== 0) . sumDifferent) summaries) exitFailure

-- | Load a serialized design
loadDesign :: Options -> FilePath -> IO Design
loadDesign opts f = do
  (bindings, tcm, tupTcm, primMap, reprs, tops, domains) <-
    if ".net.bin" `List.isSuffixOf` f
      then do
        (bm, topEnts, prims, tcm0, reprs0, _top, domains0) <-
          decode <$> B.readFile f :: IO NetlistInputs
        pure (bm, tcm0, mempty, prims, reprs0, map (globalIdLegacy . topId) topEnts, domains0)
      else do
        (bm, tcm0, tupTcm0, prims, reprs0, tops0, _top, domains0) <-
          decode <$> B.readFile f :: IO NormalizationInputs
        pure (bm, tcm0, tupTcm0, prims, reprs0, tops0, domains0)
  let clashEnv = ClashEnv
        { envOpts = defClashOpts
        , envTyConMap = tcm
        , envTupleTyCons = tupTcm
        , envPrimitives = fmap (fmap unremoveBBfunc) primMap
        , envCustomReprs = reprs
        , envDomains = domains
        }
      env = RewriteEnv
        { _clashEnv = clashEnv
        , _typeTranslator = ghcTypeToHWType (opt_intWidth (envOpts clashEnv))
        , _evaluator = evaluator
        , _topEntities = mkVarSet tops
        , _scopedTypeEnv = mkTypeEnv tcm
        }
      selected = case optBinding opts of
        Nothing -> bindings
        Just nm -> mapMaybeVarEnv
          (\b -> if Text.pack nm `Text.isInfixOf` nameOcc (globalIdName (bindingId b))
                   then Just b else Nothing)
          bindings
  mkDesign env selected

-- * Captured sites

-- | Load the captures of a design: the corpus, how often every
-- transformation fired, and the captures, which are decoded as they are
-- consumed
loadCorpus :: FilePath -> IO (Corpus, CaptureLog)
loadCorpus f = do
  bytes <-
    if ".zst" `List.isSuffixOf` f
      then do
        (_, Just h, _, _) <- createProcess (proc "zstd" ["-q", "-d", "-c", f]) {std_out = CreatePipe}
        B.hGetContents h
      else B.readFile f
  let ((_, tcm, tupTcm, primMap, reprs, _, _, domains), cl) = decodeCorpus bytes
      fs = clFinal cl
      clashEnv = ClashEnv
        { envOpts = applyCaptureOpts (fsOpts fs) defClashOpts
        , envTyConMap = tcm
        , envTupleTyCons = tupTcm
        , envPrimitives = fmap (fmap unremoveBBfunc) primMap
        , envCustomReprs = reprs
        , envDomains = domains
        }
      env = RewriteEnv
        { _clashEnv = clashEnv
        , _typeTranslator = ghcTypeToHWType (opt_intWidth (envOpts clashEnv))
        , _evaluator = evaluator
        , _topEntities = mkVarSet (fsTopEntities fs)
        , _scopedTypeEnv = mkTypeEnv tcm
        }
  c <- mkCorpus env fs (clBindingEpochs cl)
  pure (c, cl)

-- | Whether a site is in a binding selected with --binding
inBinding :: Options -> Capture -> Bool
inBinding opts cap = case optBinding opts of
  Nothing -> True
  Just nm -> Text.pack nm `Text.isInfixOf` nameOcc (varName (fst (capCurFun cap)))

runSites :: Options -> SiteComparison -> [FilePath] -> IO ()
runSites opts sc files = do
  summaries <- forM files $ \f -> do
    (c, cl) <- loadCorpus f
    s <- compareCorpus (optTimeout opts * 1000000) (optShow opts) (inBinding opts) sc c (clCaptures cl)
    forM_ (ssMismatches s) $ \(cap, m) ->
      putStr (renderSite (optLines opts) (optWidth opts) cap m)
    putStrLn (renderSiteSummary f s)
    hFlush stdout
    pure s
  when (length files > 1) $
    putStrLn (renderSiteSummary "total" (mconcat summaries))
  unless (all ok summaries) exitFailure
 where
  ok s = ssNotReproduced s == 0 && ssNotFired s == 0 && sumDifferent (ssScoped s) == 0

-- | Rerun every captured transformation at its sites, in one pass over the
-- captures of every corpus
runReplay :: Options -> [FilePath] -> IO ()
runReplay opts files = do
  results <- forM files $ \f -> do
    (c, cl) <- loadCorpus f
    let selected cap = inBinding opts cap
          && maybe True ((== capName cap) . Text.pack) (optTransformation opts)
        go !acc [] = pure acc
        go !acc (cap : caps)
          | not (selected cap) = go acc caps
          | otherwise = case lookupCurrent (capName cap) of
              Nothing -> go (Map.insertWith (\_ old -> old) (capName cap) Nothing acc) caps
              Just (rw, kds) -> do
                let sc = SiteComparison
                      { scName = Text.unpack (capName cap)
                      , scDescription = ""
                      , scTransformations = [capName cap]
                      , scCurrent = rw
                      , scScoped = Nothing
                      , scKnownDifferences = kds
                      }
                o <- compareSite (optTimeout opts * 1000000) sc c cap
                let s0 = maybe mempty id (Map.findWithDefault Nothing (capName cap) acc)
                    s1 = addOutcome (optShow opts) cap o s0
                go (Map.insert (capName cap) (Just s1) acc) caps
    rs <- go Map.empty (clCaptures cl)
    forM_ (Map.toList rs) $ \case
      (nm, Nothing) ->
        putStrLn (f <> ": " <> Text.unpack nm <> ": not in Registry.currentRewrites, not replayed")
      (nm, Just s) -> do
        forM_ (reverse (ssMismatches s)) $ \(cap, m) ->
          putStr (renderSite (optLines opts) (optWidth opts) cap m)
        putStrLn (renderSiteSummary (f <> ": " <> Text.unpack nm) s)
    hFlush stdout
    pure (Map.mapMaybe id rs)
  putStrLn "\nPer transformation, over all corpora:"
  let total = Map.unionsWith (<>) [Map.map (\s -> s {ssMismatches = []}) r | r <- results]
  forM_ (Map.toList total) $ \(nm, s) ->
    putStrLn (renderSiteSummary (Text.unpack nm) s)
  unless (all (\s -> ssNotReproduced s == 0 && ssNotFired s == 0) total) exitFailure

-- | Statistics of the captures of a transformation in a corpus
data Stats = Stats
  { stFired :: !Word
  , stCaptured :: !Int
  , stLarge :: !Int
  -- ^ Captures of large sites, see 'Clash.Rewrite.Capture.largeSite'
  , stDesigns :: !Int
  }

instance Semigroup Stats where
  Stats a b c d <> Stats a' b' c' d' = Stats (a + a') (b + b') (c + c') (d + d')

-- | Count the captures of corpora, per transformation
corpusStats :: [FilePath] -> IO ()
corpusStats files = do
  perFile <- forM files $ \f -> do
    (_, CaptureLog _ firings _ caps) <- loadCorpus f
    size <- getFileSize f
    let large cap = not (termSizeSmallerThan largeSite (capInput cap)
                         && termSizeSmallerThan largeSite (capOutput cap))
        step m cap =
          Map.insertWith (<>) (capName cap) (Stats 0 1 (if large cap then 1 else 0) 0) m
        captured = List.foldl' step Map.empty caps
    putStrLn (f <> ": " <> show (sum (map stCaptured (Map.elems captured))) <> " captures of "
              <> show (Map.size captured) <> " transformations, "
              <> mib size <> " on disk")
    pure (Map.unionWith (<>)
            (Map.fromList [(nm, Stats n 0 0 1) | (nm, n) <- firings])
            captured)
  let total = Map.unionsWith (<>) perFile
      known = concat [nms | (nms, _, _) <- currentRewrites]
      missing = filter (`Map.notMember` total) known
      unknown = filter (isNothing . lookupCurrent) (Map.keys total)
  putStrLn "\ntransformation: fired, captured (of which large), designs"
  forM_ (Map.toList total) $ \(nm, st) ->
    putStrLn ("  " <> Text.unpack nm <> ": " <> show (stFired st) <> ", " <> show (stCaptured st)
              <> " (" <> show (stLarge st) <> "), " <> show (stDesigns st))
  unless (null missing) $
    putStrLn ("\nNever captured: " <> List.intercalate ", " (map Text.unpack missing))
  unless (null unknown) $
    putStrLn ("Captured, but not in Registry.currentRewrites: "
              <> List.intercalate ", " (map Text.unpack (unknown :: [Text])))

-- | A number of bytes, in MiB
mib :: Integral a => a -> String
mib n = show (fromIntegral (round (fromIntegral n / (1024 * 1024 / 10 :: Double)) :: Int) / 10 :: Double) <> " MiB"

-- | The name of a design without the @.bin@ suffix of its normalization
-- inputs
dropBinSuffix :: FilePath -> FilePath
dropBinSuffix f
  | ".bin" `List.isSuffixOf` f = take (length f - 4) f
  | otherwise = f

-- | The known differences between the normalized bindings of the normalizer
-- of Clash Core and those of scoped Clash Core, see the README
normalizedKnownDifferences :: TyConMap -> [KnownDifference]
normalizedKnownDifferences tcm =
  [ coercionForm, primTypeForm, patternTypeForms tcm, literalNameTicks, sharedSelectors
  , recToNonRec, nonRecLetOrder, letrecOrder, deadLetBindings ]
