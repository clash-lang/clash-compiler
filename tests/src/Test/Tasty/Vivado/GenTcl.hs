{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Generate a TCL script to simulate generated VHDL
--
-- Run with @vivado -mode batch -source ...@
module Test.Tasty.Vivado.GenTcl where

import Data.Char (isLower)
import Data.List (isSuffixOf)
import Data.Maybe (mapMaybe)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import System.FilePath ((</>))

import Clash.Driver.LocatedManifest
  (LocatedManifest (..), entityDirectory, manifestLocationFromStrName)
import Clash.Driver.Manifest (Manifest (..))
import Paths_clash_testsuite (getDataDir)
import Test.Tasty.Common (getManifests)

import qualified Data.Text as T

data SourceType = VhdlSource | VerilogSource | SystemVerilogSource | TclSource
  deriving (Eq, Show)

data HdlSource = HdlSource
  { hdlType :: SourceType
  , hdlLib :: T.Text
  , hdlFile :: FilePath
  } deriving Show

-- | Given a file from a Manifest file, convert it into a more digestible 'HdlSource'
-- data structure. Supports all file types mentioned in 'SourceType'.
toHdlSource ::
  -- | Directory with HDL files
  FilePath ->
  -- | VHDL/SystemVerilog library name
  T.Text ->
  -- | Filename of file in directory given earlier
  FilePath ->
  -- | @Just HdlSource@ if file extension was recognized, otherwise @Nothing@
  Maybe HdlSource
toHdlSource dir lib filename
  | ".vhdl" `isSuffixOf` filename = go VhdlSource
  | ".v"    `isSuffixOf` filename = go VerilogSource
  | ".sv"   `isSuffixOf` filename = go SystemVerilogSource
  | ".tcl"  `isSuffixOf` filename = go TclSource
  | otherwise = Nothing
 where
  go ty = Just (HdlSource { hdlType=ty, hdlLib=lib, hdlFile=dir </> filename })

-- | Convert all generated files into 'HdlSource's for a given HDL directory and
-- corresponding 'Manifest'.
manifestToHdlSources :: LocatedManifest -> [HdlSource]
manifestToHdlSources manifest@LocatedManifest{lmManifest}=
  let
    paths = fst <$> fileNames lmManifest
    lib = topComponent lmManifest
  in
    mapMaybe (toHdlSource (entityDirectory manifest) lib) paths

-- | Convert all generated files into 'HdlSource's for a given HDL directory and
-- corresponding 'Manifest's.
manifestsToHdlSources :: [LocatedManifest] -> [HdlSource]
manifestsToHdlSources = concatMap manifestToHdlSources

-- | From @PortProductsSum_testBench@, extract @testBench@ (for instance)
stripEntity ::
  T.Text ->
  T.Text
stripEntity entityName =
  let parts = T.split (=='_') entityName
    in T.intercalate "_" (filter (isLower . T.head) parts)

simProjFromClashEntities ::
  -- | HDL source dir
  FilePath ->
  -- | Qualified name
  String ->
  IO [HdlSource]
simProjFromClashEntities hdlDir qualName = do
  let manifestPath = manifestLocationFromStrName hdlDir qualName
  [(_, mHdl)] <- getManifests manifestPath
  let deps = T.unpack <$> transitiveDependencies mHdl
  nextSimProj <- traverse (simProjFromClashEntities hdlDir) deps
  pure $ mconcat nextSimProj <> manifestToHdlSources (LocatedManifest manifestPath mHdl)

-- | Read @clash-manifest.json@ and create a TCL script to instantiate relevant
-- IP, read sources.
tclFromManifest ::
  -- | HDL source dir
  FilePath ->
  -- | Module name
  String ->
  -- | Entity
  String ->
  -- | TCL script
  IO String
tclFromManifest src moduleName entity = do
  proj <- simProjFromClashEntities src (moduleName ++ "." ++ hsEntity)
  cabalDir <- getDataDir
  pure $ mkSimulationTcl cabalDir entity proj
 where
  hsEntity = T.unpack (stripEntity (T.pack entity))

mkBaseTcl ::
  -- | Cabal @data@ dir
  FilePath ->
  [HdlSource] ->
  String
mkBaseTcl cabalDir hdlSources =
  [i|
set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR

source {#{tclDataFile}}

namespace eval tmpclash {}

|]

  ++ loadTclIfs

  ++ [i|
create_project -in_memory
set tmpclash::ips [clash::runClashScripts]
if {$tmpclash::ips ne {}} {
  set tmpclash::ipFiles [get_property IP_FILE [get_ips $tmpclash::ips]]
}
close_project

if {$tmpclash::ips ne {}} {
  read_ip $tmpclash::ipFiles
  generate_target {synthesis simulation} [get_ips $tmpclash::ips]
}

|]

  ++ readSources

  ++ [i|
# Compiler doesn't topologically sort source files (bug)
update_compile_order -fileset [current_fileset]
|]
 where
  tclDataFile = cabalDir </> "data-files" </> "tcl" </> "clash_namespace.tcl"

  loadTclIfs =
    flip concatMap hdlSources $ \HdlSource{..} ->
      case hdlType of
        TclSource -> [i|clash::loadTclIface {#{hdlLib}} {#{hdlFile}}\n|]
        _ -> []

  readSources =
    flip concatMap hdlSources $ \HdlSource{..} ->
      case hdlType of
        VhdlSource ->
          [i|read_vhdl -library {#{hdlLib}} {#{hdlFile}}\n|]
        VerilogSource ->
          [i|read_verilog {#{hdlFile}}\n|]
        _ -> []

mkSimulationTcl ::
  -- | Cabal @data@ dir
  FilePath ->
  -- | Entity
  String ->
  [HdlSource] ->
  String
mkSimulationTcl cabalDir entity hdlSources =
  mkBaseTcl cabalDir hdlSources <> unindent [i|
    set_property TOP {#{entity}} [current_fileset -sim]

    save_project_as sim project -force

    set_property RUNTIME all [current_fileset -sim]

    launch_simulation
  |]
