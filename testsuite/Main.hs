module Main (main) where

import Test.Tasty
import Test.Tasty.Program

main :: IO ()
main = defaultMain $ testGroup "examples/FIR.hs"
  [ testProgram "Cabal" "cabal" ["sandbox","init","--sandbox=../.cabal-sandbox"] (Just "examples")
  , testProgram "CLaSH" "cabal" ["exec","clash","--","--vhdl","FIR.hs"] (Just "examples")
  , testProgram "System (mkdir)" "mkdir" ["work"] (Just "examples/vhdl/FIR")
  , testProgram "GHDL (import)" "ghdl_import.sh" ["examples/vhdl/FIR"] Nothing
  , testProgram "GHDL (make)" "ghdl" ["-m","--workdir=work","testbench"] (Just "examples/vhdl/FIR")
  , testProgram "GHDL (sim)" "ghdl" ["-r","testbench","--assert-level=error"] (Just "examples/vhdl/FIR")
  ]
