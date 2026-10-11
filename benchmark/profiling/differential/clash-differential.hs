{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Runs a comparison of "Comparisons" on serialized designs: the inputs of
  normalization (@*.bin@, see @clash-profile-normalization-prepare@), or of
  netlist generation, i.e. normalized bindings (@*.net.bin@, see
  @clash-profile-netlist-prepare@). See README.md.
-}

{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (forM, forM_, unless, when)
import Data.Binary (decode)
import qualified Data.ByteString.Lazy as B
import qualified Data.List as List
import qualified Data.Text as Text
import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode (..))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Text.Read (readMaybe)

import Clash.Core.Name (nameOcc)
import Clash.Core.Scoped.TypeEnv (addGlobalTypes, mkTypeEnv)
import Clash.Core.Var (varName)
import Clash.Core.VarEnv (mapMaybeVarEnv, mkVarSet)
import Clash.Driver.Types
  (Binding (..), ClashEnv (..), ClashOpts (..), defClashOpts)
-- For the orphan 'Binary' instance of 'TopEntityT'
import Clash.Driver.Manifest ()
import Clash.Netlist.Types (TopEntityT (topId))
import Clash.Rewrite.Types (RewriteEnv (..))

import Clash.GHC.Evaluator (evaluator)
import Clash.GHC.NetlistTypes (ghcTypeToHWType)

import SerialiseInstances (NetlistInputs, NormalizationInputs, unremoveBBfunc)

import Comparisons (comparisons)
import Test.Clash.Differential

data Options = Options
  { optShow :: Int
  , optLines :: Int
  , optWidth :: Int
  , optBinding :: Maybe String
  }

defOptions :: Options
defOptions = Options 3 30 200 Nothing

usage :: String
usage = unlines
  [ "Usage: clash-differential [OPTIONS] COMPARISON FILE..."
  , "       clash-differential --list"
  , ""
  , "Runs COMPARISON on every binding of the serialized designs FILE..., which"
  , "are *.bin files written by clash-profile-normalization-prepare, or"
  , "*.net.bin files written by clash-profile-netlist-prepare."
  , ""
  , "Options:"
  , "  --show N        print the first N differences per design (default 3)"
  , "  --lines N       print at most N lines of a term (default 30)"
  , "  --width N       print at most N characters of a line (default 200)"
  , "  --binding NAME  only compare the bindings whose name contains NAME"
  ]

main :: IO ()
main = getArgs >>= \case
  ["--list"] -> forM_ comparisons $ \(Comparison nm desc _ _ kds) -> do
    putStrLn (nm <> ": " <> desc)
    forM_ kds $ \kd -> putStrLn ("  known difference " <> kdName kd <> ": " <> kdDescription kd)
  args -> case parseArgs defOptions args of
    Just (opts, nm : files@(_:_)) -> case List.find (\c -> name c == nm) comparisons of
      Just cmp -> run opts cmp files
      Nothing -> die ("Unknown comparison: " <> nm <> ", see --list")
    _ -> die usage
 where
  name (Comparison nm _ _ _ _) = nm
  die msg = hPutStrLn stderr msg >> exitWith (ExitFailure 2)

-- | Options, which can be anywhere, and the other arguments
parseArgs :: Options -> [String] -> Maybe (Options, [String])
parseArgs opts = \case
  [] -> Just (opts, [])
  "--show" : n : rest -> readMaybe n >>= \k -> parseArgs opts {optShow = k} rest
  "--lines" : n : rest -> readMaybe n >>= \k -> parseArgs opts {optLines = k} rest
  "--width" : n : rest -> readMaybe n >>= \k -> parseArgs opts {optWidth = k} rest
  "--binding" : nm : rest -> parseArgs opts {optBinding = Just nm} rest
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
        pure (bm, tcm0, mempty, prims, reprs0, map topId topEnts, domains0)
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
        , _scopedTypeEnv = addGlobalTypes (fmap bindingId bindings) (mkTypeEnv tcm)
        }
      selected = case optBinding opts of
        Nothing -> bindings
        Just nm -> mapMaybeVarEnv
          (\b -> if Text.pack nm `Text.isInfixOf` nameOcc (varName (bindingId b))
                   then Just b else Nothing)
          bindings
  mkDesign env selected
