{-# LANGUAGE QuasiQuotes #-}

-- | Generate a TCL script to simulate generated VHDL
--
-- Run with @vivado -mode batch -source ...@
module Clash.Vivado ( HdlSource (..), fromModuleName ) where

import Paths_clash_testsuite (getDataDir)

import Clash.Driver.Manifest (readManifest, Manifest (..))
import Clash.Annotations.Primitive (HDL (..))

import Data.String.Interpolate (i)
import Data.List (isSuffixOf)

import System.FilePath ((</>))
import qualified Data.Text as T

data HdlSource = HdlSource { directory :: FilePath, buildTarget :: HDL }

data SimProj = SimProj { vhdlFiles :: [(T.Text, FilePath)]
                       , verilogFiles :: [(T.Text, FilePath)]
                       , ipGen :: [FilePath]
                       } deriving Show

instance Semigroup SimProj where
  (<>) (SimProj vhd0 v0 ip0) (SimProj vhd1 v1 ip1) =
    SimProj (vhd0<>vhd1) (v0<>v1) (ip0<>ip1)

instance Monoid SimProj where
  mempty = SimProj [] [] []

thread :: [a -> a] -> a -> a
thread = foldr (.) id

-- | Given something like @topEntity_dcfifo_240A2A9D3FF8D64B.tcl@, return the
-- IP name, @dcfifo@
parseTcl :: FilePath -> String
parseTcl fp = takeWhile (/= '_') (drop 1 $ dropWhile (/= '_') fp)

mSimProj :: FilePath -> T.Text -> FilePath -> SimProj -> SimProj
mSimProj dir lib fp p@(SimProj vhd v ip)
  | ".vhdl" `isSuffixOf` fp = SimProj ((lib, dir </> fp):vhd) v ip
  | ".vhd" `isSuffixOf` fp = SimProj ((lib, dir </> fp):vhd) v ip
  | ".v" `isSuffixOf` fp = SimProj vhd ((lib, dir </> fp):v) ip
  | ".tcl" `isSuffixOf` fp = SimProj vhd v ((dir </> fp):ip)
  | otherwise = p

simProjFromClashEntities ::
  -- | HDL source dir
  FilePath ->
  -- | Qualified name
  String ->
  IO SimProj
simProjFromClashEntities hdlDir qualName = do
  (Just mHdl) <- readManifest (hdlDir </> qualName ++ "/clash-manifest.json")
  let deps = T.unpack <$> transitiveDependencies mHdl
  nextSimProj <- traverse (simProjFromClashEntities hdlDir) deps
  pure $ mconcat nextSimProj <> asSimProj qualName mHdl

fromModuleName ::
  HdlSource ->
  -- | Module name
  String ->
  -- | Entity
  String ->
  -- | TCL script
  IO String
fromModuleName src moduleName entity = do
  proj <- simProjFromClashEntities (directory src) (moduleName ++ "." ++ entity)
  cabalDir <- getDataDir
  pure $ mkTcl cabalDir entity src proj

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
mkTcl cabalDir entity hdl (SimProj vhdls vs tcls) =
  [i|
source #{cabalDir}/tcl/clash_namespace.tcl

# create clash namespace
namespace eval clash {}

set clash::hdl_dir #{hdlDir}

# for cleanup
file delete -force xilinx_tmp
file mkdir xilinx_tmp
cd xilinx_tmp

create_project -in_memory

set clash::ips [list]
|]

  ++ concatMap (\tcl ->
      [i|clash::loadTclIface #{parseTcl tcl} ${clash::hdl_dir}/#{tcl}\n|]) tcls

  ++
      [i|

set clash::ips [clash::runClashScripts]
set clash::ipFiles [get_property IP_FILE [get_ips $clash::ips]]

read_ip $clash::ipFiles

generate_target {synthesis simulation} [get_ips $clash::ips]

foreach modName $clash::ips {
  add_files .gen/sources_1/ip/${modName}/synth
}
|]

  ++ readSources

  ++ [i|
update_compile_order -fileset [current_fileset]

set_property top #{entity} [get_fileset sim_1]

save_project_as sim -force

launch_simulation -simset sim_1 -mode behavioral

run -all

quit
|]
 where
  hdlDir = directory hdl
  readSources = case buildTarget hdl of
    Verilog ->
     concatMap (\v ->
      [i|read_verilog ${clash::hdl_dir}/#{v}\n|]) (snd <$> vs)
    VHDL ->
     concatMap (\(lib, vhdl) ->
        [i|read_vhdl -library #{lib} ${clash::hdl_dir}/#{vhdl}\n|])
      vhdls
    SystemVerilog -> error "SystemVerilog not implemented for Vivado tests"
