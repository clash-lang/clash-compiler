{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.Primitive.Transformations
  ( transformationsPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)

import Clash.GHC.PartialEval.Primitive.Strategy

-- TODO Most of these should go into a blacklist prims module.
transformationsPrims :: HashMap Text PrimImpl
transformationsPrims = HashMap.fromList
  [ ("EmptyCase", liftUndefined)
  , ("Clash.XException.errorX", liftUndefined)

    -- Blacklist start
  , ("_CO_", liftId)
  , ("_TY_", liftId)
  , ("Clash.Explicit.BlockRam.blockRam#", liftId)
  , ("Clash.Explicit.DDR.ddrIn#", liftId)
  , ("Clash.Explicit.DDR.ddrOut#", liftId)
  , ("Clash.Explicit.Signal.veryUnsafeSynchronizer", liftId)
  , ("Clash.Explicit.SimIO.bindSimIO#", liftId)
  , ("Clash.Explicit.SimIO.display", liftId)
  , ("Clash.Explicit.SimIO.finish", liftId)
  , ("Clash.Explicit.SimIO.getChar", liftId)
  , ("Clash.Explicit.SimIO.isEOF", liftId)
  , ("Clash.Explicit.SimIO.mealyIO", liftId)
  , ("Clash.Explicit.SimIO.openFile", liftId)
  , ("Clash.Explicit.SimIO.pureSimIO#", liftId)
  , ("Clash.Explicit.SimIO.readReg", liftId)
  , ("Clash.Explicit.SimIO.reg", liftId)
  , ("Clash.Explicit.SimIO.writeReg", liftId)
  , ("Clash.Explicit.Testbench.assert", liftId)
  , ("Clash.Explicit.Testbench.assertBitVector", liftId)
  , ("Clash.Explicit.Testbench.tbClockGen", liftId)
  , ("Clash.Explicit.Testbench.tbEnableGen", liftId)
  , ("Clash.Explicit.Verification.check", liftId)
  , ("Clash.Intel.ClockGen.altpll", liftId)
  , ("Clash.Prelude.ROM.asyncRom#", liftId)
  , ("Clash.Signal.BiSignal.mergeBiSignalOuts", liftId)
  , ("Clash.Signal.BiSignal.readFromBiSignal#", liftId)
  , ("Clash.Signal.BiSignal.veryUnsafeToBiSignalIn", liftId)
  , ("Clash.Signal.BiSignal.writeToBiSignal#", liftId)
  , ("Clash.Signal.Internal.clockGen", liftId)
  , ("Clash.Signal.Internal.delay#", liftId)
  , ("Clash.Signal.Internal.register#", liftId)
  , ("Clash.Signal.Internal.resetGenN", liftId)
  , ("Clash.Signal.Internal.unsafeFromReset", liftId)
  , ("Clash.Signal.Internal.unsafeToReset", liftId)
  , ("Clash.Normalize.Primitives.removedArg", liftId)
  , ("Clash.Normalize.Primitives.undefined", liftId)
  , ("Clash.XException.deepseqX", liftId)
  , ("Clash.XException.hwSeqX", liftId)
  , ("Clash.XException.seqX", liftId)
  , ("Data.Text.Show.unpackCString#", liftId)
  , ("Data.Typeable.Internal.$wmkTrCon", liftId)
  , ("Data.Typeable.Internal.fpTYPELiftedRep", liftId)
  , ("GHC.CString.unpackFoldrCString#", liftId)
  , ("GHC.Prim.catch#", liftId)
  , ("GHC.Prim.raiseIO#", liftId)
  , ("GHC.Prim.seq#", liftId)
  ]
