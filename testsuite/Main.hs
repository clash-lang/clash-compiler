module Main (main) where

import Control.DeepSeq            (deepseq)

import Test.Tasty
import Test.Tasty.Program

import qualified System.Directory   as Directory
import qualified System.Environment as Environment
import           System.FilePath    ((</>))
-- import System.Exit                (ExitCode(..))
import qualified System.IO          as IO
import qualified System.Process     as Process

import qualified Paths_clash_testsuite

data TestRun
  = Compile
  | CompileBuild    [BuildTarget]
  | CompileBuildRun [BuildTarget]

data BuildTarget
  = VHDL | Verilog

aq :: IO FilePath
aq = do
  ddir <- Paths_clash_testsuite.getDataDir
  ghdl_import_prm <- Directory.getPermissions (ddir </> "ghdl_import.sh")
  Directory.setPermissions (ddir </> "ghdl_import.sh") (Directory.setOwnerExecutable True ghdl_import_prm)
  path <- Environment.getEnv "PATH"
  let path' = ddir ++ ":" ++ path
  Environment.setEnv "PATH" path'
  Directory.createDirectoryIfMissing True "examples/vhdl/FIR/work"
  return path

rel :: FilePath -> IO ()
rel path = do
  Directory.removeDirectoryRecursive "examples/vhdl"
  Environment.setEnv "PATH" path

main :: IO ()
main = defaultMain $ withResource aq rel $ const (testGroup "examples/FIR.hs"
  [ testProgram "CLaSH" "cabal" ["exec","clash","--","--vhdl","FIR.hs"] (Just "examples")
  , testProgram "GHDL (import)" "ghdl_import.sh" ["examples/vhdl/FIR"] Nothing
  , testProgram "GHDL (make)" "ghdl" ["-m","--workdir=work","testbench"] (Just "examples/vhdl/FIR")
  , testProgram "GHDL (sim)" "ghdl" ["-r","testbench","--assert-level=error"] (Just "examples/vhdl/FIR")
  ])
