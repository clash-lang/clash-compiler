{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Compares the results of normalizing serialized designs in two runs
  ("Normalized"), and checks the round trip through the 'Data.Binary.Binary'
  instances of scoped Clash Core ("RoundTrip"). See README.md.
-}

{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (forM, unless)
import qualified Data.List as List
import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode (..))
import System.FilePath ((</>), takeFileName)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import Normalized (NormOptions (..), compareNormalized)
import RoundTrip (roundTrip)

data Options = Options
  { optShow :: Int
  , optLines :: Int
  , optWidth :: Int
  }

defOptions :: Options
defOptions = Options 3 30 200

usage :: String
usage = unlines
  [ "Usage: clash-differential normalized [OPTIONS] DIR_A DIR_B DESIGN.bin..."
  , "       clash-differential binary [OPTIONS] FILE..."
  , ""
  , "The normalized command compares the normalized bindings of DESIGN.bin...,"
  , "that clash-profile-normalization-run writes with CLASH_NORMALIZED_OUT set,"
  , "in DIR_A and DIR_B (DIR/<design>.norm.bin), e.g. of the normalizers of two"
  , "commits. Globals are matched by where they are used, from the top entity"
  , "on. See Normalized.hs."
  , ""
  , "The binary command (or roundtrip) checks that the round trip through the"
  , "Binary instances of scoped Clash Core gives every binding of the"
  , "serialized designs FILE... back: *.bin files written by"
  , "clash-profile-normalization-prepare, *.net.bin files written by"
  , "clash-profile-netlist-prepare, or *.norm.bin files. See RoundTrip.hs."
  , ""
  , "Options:"
  , "  --show N        print the first N differences per design (default 3)"
  , "  --lines N       print at most N lines of a term (default 30)"
  , "  --width N       print at most N characters of a line (default 200)"
  ]

main :: IO ()
main = getArgs >>= \case
  "normalized" : args -> case parseArgs defOptions args of
    Just (opts, dirA : dirB : designs@(_:_)) -> do
      oks <- forM designs $ \design -> do
        let base = takeFileName (dropBinSuffix design)
            nopts = NormOptions (optShow opts) (optLines opts) (optWidth opts)
        compareNormalized nopts design
          (dirA </> base <> ".norm.bin") (dirB </> base <> ".norm.bin")
      unless (and oks) exitFailure
    _ -> die usage
  cmd : args | cmd `elem` ["binary", "roundtrip"] -> case parseArgs defOptions args of
    Just (opts, files@(_:_)) -> do
      oks <- forM files (roundTrip (optShow opts) (optLines opts) (optWidth opts))
      unless (and oks) exitFailure
    _ -> die usage
  _ -> die usage

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitWith (ExitFailure 2)

-- | Options, which can be anywhere, and the other arguments
parseArgs :: Options -> [String] -> Maybe (Options, [String])
parseArgs opts = \case
  [] -> Just (opts, [])
  "--show" : n : rest -> readMaybe n >>= \k -> parseArgs opts {optShow = k} rest
  "--lines" : n : rest -> readMaybe n >>= \k -> parseArgs opts {optLines = k} rest
  "--width" : n : rest -> readMaybe n >>= \k -> parseArgs opts {optWidth = k} rest
  a : rest
    | "--" `List.isPrefixOf` a -> Nothing
    | otherwise -> fmap (a :) <$> parseArgs opts rest

-- | The name of a design without the @.bin@ suffix of its normalization
-- inputs
dropBinSuffix :: FilePath -> FilePath
dropBinSuffix f
  | ".bin" `List.isSuffixOf` f = take (length f - 4) f
  | otherwise = f
