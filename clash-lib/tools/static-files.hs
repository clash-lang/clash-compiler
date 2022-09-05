{-|
Copyright   :  (C) 2022     , Google Inc.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Produce static files that are useful when working with Clash designs.
-}

{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (when)
import Control.Monad.Extra (whenM, unlessM)
import Prelude
import System.Console.Docopt
  (Docopt, docopt, isPresent, getArg, longOption, parseArgsOrExit)
import System.Directory (copyFile, doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (takeDirectory)

import Clash.DataFiles

patterns :: Docopt
patterns = [docopt|
Obtain static files useful when working with Clash designs

Currently, only the Tcl connector is available.

Usage:
  static-files [--force] [--verbose] --tcl-connector=<outfile>

Options:
  -f, --force                  Overwrite existing files
  -v, --verbose                Explain what is being done
  --tcl-connector=<outfile>    Write a Tcl script to a file that can parse Clash
                               manifest JSON files and emit the correct commands
                               for loading the design into Vivado
|]

-- Checks whether it looks like we can write a file in the location @path@,
-- accounting for the @--force@ command line argument. Exit with a descriptive
-- error message if something's amiss.
createOkayOrDie ::
  FilePath ->
  Bool ->
  IO ()
createOkayOrDie path force = do
  let pathDir = takeDirectory path
  unlessM (doesDirectoryExist pathDir) $
    die $ "Directory not found: " ++ pathDir
  whenM (doesDirectoryExist path) $
    die $ path ++ " is a directory. Please specify a file name."
  exists <- doesFileExist path
  when (exists && not force) $
    die $ path ++ " already exists and --force not specified. " ++
                  "Refusing to overwrite."

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs
  -- Since we got here, we know we got invoked with the sole mandatory option
  -- @--tcl-connector@ and its mandatory argument
  let force = args `isPresent` (longOption "force")
      verbose = args `isPresent` (longOption "verbose")
      Just outFile = args `getArg` (longOption "tcl-connector")
  createOkayOrDie outFile force
  inFile <- tclConnector
  copyFile inFile outFile
  when verbose $ putStrLn $ "Tcl Connector written to " ++ outFile
