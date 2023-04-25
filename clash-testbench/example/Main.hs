{-# LANGUAGE RecursiveDo #-}
module Main where

import Clash.Testbench

import Calculator (OPC(..))
import qualified Calculator (topEntity)

myTestbench
  :: TB ()
myTestbench = mdo
  input <- inputFromList Pop [Imm 1, Push, Imm 2, Push, Pop, Pop, Pop, ADD]
  output <- ("topEntity" @@ Calculator.topEntity) auto auto auto input
  watch input
  watch output

main :: IO ()
main = simulate 10 myTestbench

foreign export ccall "clash_ffi_main"
  ffiMain :: IO ()

ffiMain :: IO ()
ffiMain = simulateFFI 10 myTestbench
