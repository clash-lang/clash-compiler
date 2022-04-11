{-# LANGUAGE OverloadedStrings #-}

module Clash.FFI.Iverilog where

import           Data.ByteString (ByteString)
import           Data.Foldable (for_)
import           Data.String (fromString)

import qualified Clash.FFI.VPI.Callback as VPI
import qualified Clash.FFI.VPI.Handle as VPI
import qualified Clash.FFI.VPI.IO as VPI
import qualified Clash.FFI.VPI.Info as VPI
import qualified Clash.FFI.VPI.Module as VPI
import qualified Clash.FFI.VPI.Net as VPI
import qualified Clash.FFI.VPI.Object as VPI
import qualified Clash.FFI.VPI.Parameter as VPI
import qualified Clash.FFI.VPI.Port as VPI
import qualified Clash.FFI.VPI.Reg as VPI
import qualified Clash.FFI.Monad as Sim
import           Clash.FFI.View

foreign export ccall "clash_ffi_main"
  clashMain :: IO ()

clashMain :: IO ()
clashMain =
  Sim.runSimAction $ do
    VPI.simPutStrLn "Hello from Haskell"

    info <- VPI.unsafeReceiveSimulatorInfo
    VPI.simPutStrLn ("Found simulator: " <> fromString (show info))

    time <- VPI.unsafeReceiveTime @VPI.Object VPI.Sim Nothing
    VPI.simPutStrLn ("Current time: " <> fromString (show time))

    modules <- VPI.topModules

    for_ modules $ \m -> do
      mName <- VPI.moduleFullName m
      VPI.simPutStrLn ("Found module: " <> mName)

      params <- VPI.moduleParameters m

      for_ params $ \p -> do
        pName <- VPI.parameterName p
        pSize <- VPI.parameterSize @Int p
        pVal  <- VPI.parameterValue p

        VPI.simPutStrLn
          ("Found parameter: " <> pName <> ", size " <> fromString (show pSize) <> ", value: " <> fromString (show pVal))

      ports <- VPI.modulePorts m

      for_ ports $ \p -> do
        pName <- VPI.portName p
        VPI.simPutStrLn ("Found port: " <> pName)

      nets <- VPI.moduleNets m

      for_ nets $ \n -> do
        nName <- VPI.netName n
        nSize <- VPI.netSize @Int n
        nVal  <- VPI.netValue n

        VPI.simPutStrLn
          ("Found net: " <> nName <> ", size " <> fromString (show nSize) <> ", value: " <> fromString (show nVal))

      regs <- VPI.moduleRegs m

      for_ regs $ \r -> do
        rName <- VPI.regName r
        rSize <- VPI.regSize @Int r
        rVal  <- VPI.regValue r

        VPI.simPutStrLn
          ("Found reg: " <> rName <> ", size " <> fromString (show rSize) <> ", value: " <> fromString (show rVal))

        rCb <- VPI.registerCallback (monitorCallback r)
        VPI.freeHandle rCb

monitorCallback :: VPI.Reg -> VPI.CallbackInfo ByteString
monitorCallback reg = VPI.CallbackInfo
  { VPI.cbReason  = VPI.AfterValueChange reg VPI.Sim VPI.VectorFmt
  , VPI.cbRoutine = routine
  , VPI.cbIndex   = 0
  , VPI.cbData    = ""
  }
 where
  routine ptr =
    Sim.runSimAction $ do
      hName  <- VPI.regName reg
      size   <- VPI.regSize reg

      cinfo  <- Sim.readPtr ptr
      time   <- peekReceive @VPI.Time (VPI.ccbTime cinfo)
      cvalue <- Sim.readPtr (VPI.ccbValue cinfo)
      value  <- receive @VPI.Value (cvalue, size)

      VPI.simPutStrLn
        ("[" <> fromString (show time) <> "]: " <> hName <> " = " <> fromString (show value))

      VPI.simFlushIO

      pure 0

