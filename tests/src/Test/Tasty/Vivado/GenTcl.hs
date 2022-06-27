{-# LANGUAGE QuasiQuotes #-}

-- | Generate a TCL script to simulate generated VHDL
--
-- Run with @vivado -mode batch -source ...@
module Test.Tasty.Vivado.GenTcl ( HdlSource (..), tclFromManifest ) where

import Paths_clash_testsuite (getDataDir)

import Clash.Driver.Manifest (Manifest (..))
import Clash.Annotations.Primitive (HDL (..))
import Test.Tasty.Common (getManifests)

import Data.Char (isLower)
import Data.List (isSuffixOf)
import Data.String.Interpolate (i)

import System.FilePath ((</>))
import qualified Data.Text as T

data HdlSource = HdlSource { directory :: FilePath, buildTarget :: HDL }

data SimProj = SimProj { hdlFiles :: [(T.Text, FilePath)]
                       , ipGen :: [FilePath]
                       } deriving Show

instance Semigroup SimProj where
  (<>) (SimProj hdl0 ip0) (SimProj hdl1 ip1) =
    SimProj (hdl0<>hdl1) (ip0<>ip1)

instance Monoid SimProj where
  mempty = SimProj [] []

thread :: [a -> a] -> a -> a
thread = foldr (.) id

-- | Given something like @topEntity_dcfifo_240A2A9D3FF8D64B.tcl@, return the
-- IP name, @dcfifo@
parseTcl :: FilePath -> String
parseTcl fp = takeWhile (/= '_') (drop 1 $ dropWhile (/= '_') fp)

mSimProj :: FilePath -> T.Text -> FilePath -> SimProj -> SimProj
mSimProj dir lib fp p@(SimProj hdl ip)
  | ".vhdl" `isSuffixOf` fp || ".v" `isSuffixOf` fp = SimProj ((lib, dir </> fp):hdl) ip
  | ".tcl" `isSuffixOf` fp = SimProj hdl ((dir </> fp):ip)
  | otherwise = p

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
  IO SimProj
simProjFromClashEntities hdlDir qualName = do
  [(_,mHdl)] <- getManifests (hdlDir </> qualName </> "clash-manifest.json")
  let deps = T.unpack <$> transitiveDependencies mHdl
  nextSimProj <- traverse (simProjFromClashEntities hdlDir) deps
  pure $ mconcat nextSimProj <> asSimProj qualName mHdl

-- | Read @clash-manifest.json@ and create a TCL script to instantiate relevant
-- IP, read sources.
tclFromManifest ::
  HdlSource ->
  -- | Module name
  String ->
  -- | Entity
  String ->
  -- | TCL script
  IO String
tclFromManifest src moduleName entity = do
  proj <- simProjFromClashEntities (directory src) (moduleName ++ "." ++ hsEntity)
  cabalDir <- getDataDir
  pure $ mkTcl cabalDir entity src proj
 where
  hsEntity = T.unpack (stripEntity (T.pack entity))

asSimProj :: FilePath -> Manifest -> SimProj
asSimProj dir m =
  let fps = fst <$> fileNames m
    in thread (mSimProj dir (topComponent m) <$> fps) mempty

mkTcl ::
  FilePath ->
  -- | Cabal @data@ dir
  String ->
  -- | Entity
  HdlSource ->
  SimProj ->
  String
mkTcl cabalDir entity hdl (SimProj hdls tcls) =
  [i|
set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR

file mkdir ip
file mkdir project

source #{cabalDir}/data-files/tcl/clash_namespace.tcl

namespace eval myspace {}

set myspace::hdl_dir #{hdlDir}

|]

  ++ concatMap (\tcl ->
      [i|clash::loadTclIface #{parseTcl tcl} ${myspace::hdl_dir}/#{tcl}\n|]) tcls

  ++
      [i|
create_project -in_memory
set myspace::ips [clash::runClashScripts]
if {$myspace::ips ne {}} {
  set myspace::ipFiles [get_property IP_FILE [get_ips $myspace::ips]]
}
close_project

if {$myspace::ips ne {}} {
  read_ip $myspace::ipFiles
  generate_target {synthesis simulation} [get_ips $myspace::ips]
}
|]

  ++ readSources

  ++ [i|
# Compiler doesn't topologically sort source files (bug)
update_compile_order -fileset [current_fileset]

set_property TOP #{entity} [current_fileset -sim]

save_project_as sim project -force

set_property RUNTIME all [current_fileset -sim]

launch_simulation
|]
 where
  hdlDir = directory hdl
  readSources = case buildTarget hdl of
    Verilog ->
     concatMap (\v ->
      [i|read_verilog ${myspace::hdl_dir}/#{v}\n|]) (snd <$> hdls)
    VHDL ->
     concatMap (\(lib, vhdl) ->
        [i|read_vhdl -library #{lib} ${myspace::hdl_dir}/#{vhdl}\n|])
      hdls
    SystemVerilog -> error "SystemVerilog not implemented for Vivado tests"
