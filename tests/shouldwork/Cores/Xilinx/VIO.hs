{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module VIO where

import Clash.Prelude
import Clash.Cores.Xilinx.VIO
import Clash.Annotations.TH
import Clash.Annotations.BitRepresentation
import Clash.Explicit.Testbench

import Control.Monad (unless)
import Control.Monad.Extra (anyM)
import GHC.Stack (HasCallStack)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.FilePath.Glob (globDir1)

import qualified Language.Haskell.TH as TH
import qualified Data.List as L

type Dom = XilinxSystem

top :: "result" ::: Unsigned 8
top = 0
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE top #-}

makeTopEntity 'top

noInputTrue ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom Bool
noInputTrue = vioProbe @Dom inNames outNames True
 where
  inNames = Nil
  outNames = singleton "probe_out"
{-# ANN noInputTrue (TestBench 'top) #-}

makeTopEntity 'noInputTrue


noInputFalse ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom Bool
noInputFalse = vioProbe @Dom inNames outNames False
 where
  inNames = Nil
  outNames = singleton "probe_out"
{-# ANN noInputFalse (TestBench 'top) #-}

makeTopEntity 'noInputFalse


noInputLow ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom Bit
noInputLow = vioProbe @Dom inNames outNames low
 where
  inNames = Nil
  outNames = singleton "probe_out"
{-# ANN noInputLow (TestBench 'top) #-}

makeTopEntity 'noInputLow


noInputHigh ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom Bit
noInputHigh = vioProbe @Dom inNames outNames high
 where
  inNames = Nil
  outNames = singleton "probe_out"
{-# ANN noInputHigh (TestBench 'top) #-}

makeTopEntity 'noInputHigh


noInputSigned ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom (Signed 2)
noInputSigned = vioProbe @Dom inNames outNames (-1)
 where
  inNames = Nil
  outNames = singleton "probe_out"
{-# ANN noInputSigned (TestBench 'top) #-}

makeTopEntity 'noInputSigned


noInputUnsigned ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom (Unsigned 2)
noInputUnsigned = vioProbe @Dom inNames outNames 3
 where
  inNames = Nil
  outNames = singleton "probe_out"
{-# ANN noInputUnsigned (TestBench 'top) #-}

makeTopEntity 'noInputUnsigned


noInputBitVector ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom (BitVector 7)
noInputBitVector = vioProbe @Dom inNames outNames 111
 where
  inNames = Nil
  outNames = singleton "probe_out"
{-# ANN noInputBitVector (TestBench 'top) #-}

makeTopEntity 'noInputBitVector


noInputPair ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom (Bit, Bool)
noInputPair = vioProbe @Dom inNames outNames (high, False)
 where
  inNames = Nil
  outNames = $(listToVecTH (L.map (("probe_out_" <>) . show) [0,1]))
{-# ANN noInputPair (TestBench 'top) #-}

makeTopEntity 'noInputPair


noInputVec ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom (Vec 4 (Unsigned 2))
noInputVec = vioProbe @Dom inNames outNames (0 :> 1 :> 2 :> 3 :> Nil)
 where
  inNames = Nil
  outNames = $(listToVecTH (L.map (("probe_out_" <>) . show) [0..3]))
{-# ANN noInputVec (TestBench 'top) #-}

makeTopEntity 'noInputVec


data D1 = D1 Bool Bit (Unsigned 2)

noInputCustom ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom D1
noInputCustom = vioProbe @Dom inNames outNames (D1 True high 1)
 where
  inNames = Nil
  outNames = $(listToVecTH (L.map (("probe_out_" <>) . show) [0..2]))
{-# ANN noInputCustom (TestBench 'top) #-}

makeTopEntity 'noInputCustom


data D2 = D2 Bool (Vec 2 D1)

noInputNested ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom D2
noInputNested = vioProbe @Dom inNames outNames (D2 True (D1 True high 1 :> D1 False low 0 :> Nil))
 where
  inNames = Nil
  outNames = $(listToVecTH (L.map (("probe_out_" <>) . show) [0..1]))
{-# ANN noInputNested (TestBench 'top) #-}

makeTopEntity 'noInputNested


data T = R Bool Bool
{-# ANN module (DataReprAnn
                  $(liftQ [t|T|])
                  3
                  [ ConstrRepr 'R 0b111 0b000 [0b010, 0b001]
                  ]) #-}
{- TODO: Custom bit representations are not supported within VIOs
   yet. See Clash.Cores.Xilinx.VIO.Internal.BlackBoxes for details.
noInputCustomRep ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom T
noInputCustomRep = vioProbe @Dom (R True False)

makeTopEntityWithName 'noInputCustomRep ""
-}


singleInputBool ::
  "clk" ::: Clock Dom ->
  "inp" ::: Signal Dom Bool ->
  "result" ::: Signal Dom ()
singleInputBool = vioProbe @Dom inNames outNames ()
 where
  inNames = singleton "probe_in"
  outNames = Nil
{-# ANN singleInputBool (TestBench 'top) #-}

makeTopEntity 'singleInputBool


singleInputBit ::
  "clk" ::: Clock Dom ->
  "inp" ::: Signal Dom Bit ->
  "result" ::: Signal Dom ()
singleInputBit = vioProbe @Dom inNames outNames ()
 where
  inNames = singleton "probe_in"
  outNames = Nil
{-# ANN singleInputBit (TestBench 'top) #-}

makeTopEntity 'singleInputBit


singleInputSigned ::
  "clk" ::: Clock Dom ->
  "inp" ::: Signal Dom (Signed 2) ->
  "result" ::: Signal Dom ()
singleInputSigned = vioProbe @Dom inNames outNames ()
 where
  inNames = singleton "probe_in"
  outNames = Nil
{-# ANN singleInputSigned (TestBench 'top) #-}

makeTopEntity 'singleInputSigned


singleInputUnsigned ::
  "clk" ::: Clock Dom ->
  "inp" ::: Signal Dom (Unsigned 2) ->
  "result" ::: Signal Dom ()
singleInputUnsigned = vioProbe @Dom inNames outNames ()
 where
  inNames = singleton "probe_in"
  outNames = Nil
{-# ANN singleInputUnsigned (TestBench 'top) #-}

makeTopEntity 'singleInputUnsigned


singleInputBitVector ::
  "clk" ::: Clock Dom ->
  "inp" ::: Signal Dom (BitVector 7) ->
  "result" ::: Signal Dom ()
singleInputBitVector = vioProbe @Dom inNames outNames ()
 where
  inNames = singleton "probe_in"
  outNames = Nil
{-# ANN singleInputBitVector (TestBench 'top) #-}

makeTopEntity 'singleInputBitVector


singleInputPair ::
  "clk" ::: Clock Dom ->
  "inp" ::: Signal Dom (Bit, Bool) ->
  "result" ::: Signal Dom ()
singleInputPair = vioProbe @Dom inNames outNames ()
 where
  inNames = $(listToVecTH (L.map (("probe_in_" <>) . show) [0..1]))
  outNames = Nil
{-# ANN singleInputPair (TestBench 'top) #-}

makeTopEntity 'singleInputPair


singleInputVec ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom (Vec 4 (Unsigned 2)) ->
  "result" ::: Signal Dom ()
singleInputVec = vioProbe @Dom inNames outNames ()
 where
  inNames = $(listToVecTH (L.map (("probe_in_" <>) . show) [0..3]))
  outNames = Nil
{-# ANN singleInputVec (TestBench 'top) #-}

makeTopEntity 'singleInputVec


singleInputCustom ::
  "clk" ::: Clock Dom ->
  "inp" ::: Signal Dom D1 ->
  "result" ::: Signal Dom ()
singleInputCustom = vioProbe @Dom inNames outNames ()
 where
  inNames = $(listToVecTH (L.map (("probe_in_" <>) . show) [0..2]))
  outNames = Nil
{-# ANN singleInputCustom (TestBench 'top) #-}

makeTopEntity 'singleInputCustom


singleInputNested ::
  "clk" ::: Clock Dom ->
  "inp" ::: Signal Dom D2 ->
  "result" ::: Signal Dom ()
singleInputNested = vioProbe @Dom inNames outNames ()
 where
  inNames = $(listToVecTH (L.map (("probe_in_" <>) . show) [0..1]))
  outNames = Nil
{-# ANN singleInputNested (TestBench 'top) #-}

makeTopEntity 'singleInputNested


multipleInputs ::
  "clk" ::: Clock Dom ->
  "in1" ::: Signal Dom Bit ->
  "in2" ::: Signal Dom Bool ->
  "in3" ::: Signal Dom (Unsigned 3) ->
  "in4" ::: Signal Dom (Signed 4) ->
  "in5" ::: Signal Dom (Bit, Bool, Bit) ->
  "in6" ::: Signal Dom (Vec 3 (Unsigned 2)) ->
  "in7" ::: Signal Dom D1 ->
  "in8" ::: Signal Dom (BitVector 7) ->
  "result" ::: Signal Dom (Vec 0 Bool)
multipleInputs = vioProbe @Dom inNames outNames Nil
 where
  inNames = $(listToVecTH (L.map (("probe_in_" <>) . show) [0..7]))
  outNames = Nil
{-# ANN multipleInputs (TestBench 'top) #-}

makeTopEntity 'multipleInputs


inputsAndOutputs ::
  "clk" ::: Clock Dom ->
  "in1" ::: Signal Dom   Bit ->
  "in2" ::: Signal Dom   Bool ->
  "in3" ::: Signal Dom ( Unsigned 3 ) ->
  "in4" ::: Signal Dom ( Signed 4 ) ->
  "in5" ::: Signal Dom ( Bit, Bool, Bit ) ->
  "in6" ::: Signal Dom ( Vec 3 (Unsigned 2) ) ->
  "in7" ::: Signal Dom   D1 ->
  "in8" ::: Signal Dom ( BitVector 7 ) ->
  "result" ::: Signal Dom ( Bit
                       , Bool
                       , Unsigned 5
                       , Signed 2
                       , (Bool, Bit, Bool)
                       , Vec 2 (Unsigned 3)
                       , D1
                       , BitVector 6
                       )
inputsAndOutputs = vioProbe @Dom inNames outNames initVals
 where
  inNames = $(listToVecTH (L.map (("probe_in_" <>) . show) [0..7]))
  outNames = $(listToVecTH (L.map (("probe_out_" <>) . show) [0..7]))
  initVals =
    ( low
    , True
    , 1
    , -1
    , (True, low, False)
    , 5 :> 3 :> Nil
    , D1 False high 0
    , 0b111000
    )
{-# ANN inputsAndOutputs (TestBench 'top) #-}

makeTopEntity 'inputsAndOutputs

withSetName ::
  "clk" ::: Clock Dom ->
  "arg" ::: Signal Dom Bit ->
  "result" ::: Signal Dom Bit
withSetName =
  setName @"my_vio" $
    vioProbe @Dom ("a" :> Nil) ("b" :> Nil) low
{-# ANN withSetName (TestBench 'top) #-}

makeTopEntity 'withSetName

withSetNameNoResult ::
  "clk" ::: Clock Dom ->
  "arg" ::: Signal Dom Bit ->
  "result" ::: Signal Dom ()
withSetNameNoResult =
  setName @"my_vio" $
    vioProbe @Dom ("a" :> Nil) (Nil) ()
{-# ANN withSetNameNoResult (TestBench 'top) #-}

makeTopEntity 'withSetNameNoResult

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs

  test topDir 'withSetName
  test topDir 'withSetNameNoResult

 where
  test :: HasCallStack => FilePath -> TH.Name -> IO ()
  test topDir nm = do
    let hdlDir = topDir </> show nm
    paths <- L.sort <$> globDir1 "*.vhdl" hdlDir
    result <- anyM containsMyVio paths
    unless result $ error $ "'my_vio' not found in any of: " <> show paths

  containsMyVio :: FilePath -> IO Bool
  containsMyVio path = do
    contents <- readFile path
    pure $ "my_vio" `L.isInfixOf` contents
