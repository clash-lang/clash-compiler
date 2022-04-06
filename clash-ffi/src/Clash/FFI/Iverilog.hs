{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.FFI.Iverilog where

import qualified Control.Monad.IO.Class as IO (liftIO)
import           Data.ByteString (ByteString)
import           Data.Foldable (for_)
import           Data.String (fromString)
import qualified Foreign.Storable as FFI

import qualified Clash.FFI.VPI.Callback as VPI
import qualified Clash.FFI.VPI.IO as VPI
import qualified Clash.FFI.VPI.Info as VPI
import qualified Clash.FFI.VPI.Module as VPI
import qualified Clash.FFI.VPI.Net as VPI
import qualified Clash.FFI.VPI.Object as VPI
import qualified Clash.FFI.VPI.Parameter as VPI
import qualified Clash.FFI.VPI.Property as VPI
import qualified Clash.FFI.VPI.Port as VPI
import qualified Clash.FFI.VPI.Reg as VPI
import qualified Clash.FFI.VPI.Time as VPI
import qualified Clash.FFI.VPI.Value as VPI
import qualified Clash.FFI.Monad as Sim
import           Clash.FFI.View

foreign export ccall "clash_ffi_main"
  clashMain :: IO ()

clashMain :: IO ()
clashMain =
  Sim.runSimAction $ do
    VPI.simPutStrLn "Hello from Haskell"

    cinfoPtr <- VPI.simulatorInfo Sim.stackPtr
    info     <- unsafePeekReceive @VPI.Info cinfoPtr
    VPI.simPutStrLn ("Found simulator: " <> fromString (show info))

    ctimePtr <- VPI.simulationTime Sim.stackPtr VPI.Sim Nothing
    time     <- unsafePeekReceive @VPI.Time ctimePtr
    VPI.simPutStrLn ("Current time: " <> fromString (show time))

    modules <- VPI.topModules

    for_ modules $ \m -> do
      mName <- VPI.moduleFullName m
      VPI.simPutStrLn ("Found module: " <> mName)

      params <- VPI.moduleParameters m

      for_ @_ @_ @_ @() params $ \p -> do
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

      for_ @_ @_ @_ @() nets $ \n -> do
        nName <- VPI.netName n
        nSize <- VPI.netSize @Int n
        nVal  <- VPI.netValue n

        VPI.simPutStrLn
          ("Found net: " <> nName <> ", size " <> fromString (show nSize) <> ", value: " <> fromString (show nVal))

      regs <- VPI.moduleRegs m

      for_ @_ @_ @_ @() regs $ \r -> do
        rName <- VPI.regName r
        rSize <- VPI.regSize @Int r
        rVal  <- VPI.regValue r

        VPI.simPutStrLn
          ("Found reg: " <> rName <> ", size " <> fromString (show rSize) <> ", value: " <> fromString (show rVal))

        rCb <- VPI.registerCallback (monitorCallback (VPI.regHandle r))
        VPI.freeHandle (VPI.callbackHandle rCb)

monitorCallback :: VPI.Handle -> VPI.CallbackInfo ByteString
monitorCallback handle = VPI.CallbackInfo
  { VPI.cbReason  = VPI.AfterValueChange handle VPI.Sim VPI.VectorFmt
  , VPI.cbRoutine = routine
  , VPI.cbIndex   = 0
  , VPI.cbData    = ""
  }
 where
  routine ptr =
    Sim.runSimAction $ do
      hName  <- VPI.receiveProperty VPI.Name handle
      size   <- VPI.getProperty VPI.Size handle

      cinfo  <- IO.liftIO (FFI.peek ptr)
      time   <- peekReceive @VPI.Time (VPI.ccbTime cinfo)
      cvalue <- IO.liftIO (FFI.peek (VPI.ccbValue cinfo))
      value  <- receive @VPI.Value (cvalue, size)

      VPI.simPutStrLn
        ("[" <> fromString (show time) <> "]: " <> hName <> " = " <> fromString (show value))

      pure 0

