module VIO where

import Clash.Prelude
import Clash.Cores.Xilinx.VIO
import Clash.Annotations.TH
import Clash.Annotations.BitRepresentation
import Clash.Explicit.Testbench

type Dom = XilinxSystem

top :: "result" ::: Unsigned 8
top = 0
{-# NOINLINE top #-}

makeTopEntity 'top

noInputTrue ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom Bool
noInputTrue = vioProbe @Dom True
{-# ANN noInputTrue (TestBench 'top) #-}

makeTopEntity 'noInputTrue


noInputFalse ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom Bool
noInputFalse = vioProbe @Dom False
{-# ANN noInputFalse (TestBench 'top) #-}

makeTopEntity 'noInputFalse


noInputLow ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom Bit
noInputLow = vioProbe @Dom low
{-# ANN noInputLow (TestBench 'top) #-}

makeTopEntity 'noInputLow


noInputHigh ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom Bit
noInputHigh = vioProbe @Dom high
{-# ANN noInputHigh (TestBench 'top) #-}

makeTopEntity 'noInputHigh


noInputSigned ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom (Signed 2)
noInputSigned = vioProbe @Dom (-1)
{-# ANN noInputSigned (TestBench 'top) #-}

makeTopEntity 'noInputSigned


noInputUnsigned ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom (Unsigned 2)
noInputUnsigned = vioProbe @Dom 3
{-# ANN noInputUnsigned (TestBench 'top) #-}

makeTopEntity 'noInputUnsigned


noInputBitVector ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom (BitVector 7)
noInputBitVector = vioProbe @Dom 111
{-# ANN noInputBitVector (TestBench 'top) #-}

makeTopEntity 'noInputBitVector


noInputPair ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom (Bit, Bool)
noInputPair = vioProbe @Dom (high, False)
{-# ANN noInputPair (TestBench 'top) #-}

makeTopEntity 'noInputPair


noInputVec ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom (Vec 4 (Unsigned 2))
noInputVec = vioProbe @Dom (0 :> 1 :> 2 :> 3 :> Nil)
{-# ANN noInputVec (TestBench 'top) #-}

makeTopEntity 'noInputVec


data D1 = D1 Bool Bit (Unsigned 2)

noInputCustom ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom D1
noInputCustom = vioProbe @Dom (D1 True high 1)
{-# ANN noInputCustom (TestBench 'top) #-}

makeTopEntity 'noInputCustom


data D2 = D2 Bool (Vec 2 D1)

noInputNested ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom D2
noInputNested = vioProbe @Dom (D2 True (D1 True high 1 :> D1 False low 0 :> Nil))
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
singleInputBool = vioProbe @Dom ()
{-# ANN singleInputBool (TestBench 'top) #-}

makeTopEntity 'singleInputBool


singleInputBit ::
  "clk" ::: Clock Dom ->
  "inp" ::: Signal Dom Bit ->
  "result" ::: Signal Dom ()
singleInputBit = vioProbe @Dom ()
{-# ANN singleInputBit (TestBench 'top) #-}

makeTopEntity 'singleInputBit


singleInputSigned ::
  "clk" ::: Clock Dom ->
  "inp" ::: Signal Dom (Signed 2) ->
  "result" ::: Signal Dom ()
singleInputSigned = vioProbe @Dom ()
{-# ANN singleInputSigned (TestBench 'top) #-}

makeTopEntity 'singleInputSigned


singleInputUnsigned ::
  "clk" ::: Clock Dom ->
  "inp" ::: Signal Dom (Unsigned 2) ->
  "result" ::: Signal Dom ()
singleInputUnsigned = vioProbe @Dom ()
{-# ANN singleInputUnsigned (TestBench 'top) #-}

makeTopEntity 'singleInputUnsigned


singleInputBitVector ::
  "clk" ::: Clock Dom ->
  "inp" ::: Signal Dom (BitVector 7) ->
  "result" ::: Signal Dom ()
singleInputBitVector = vioProbe @Dom ()
{-# ANN singleInputBitVector (TestBench 'top) #-}

makeTopEntity 'singleInputBitVector


singleInputPair ::
  "clk" ::: Clock Dom ->
  "inp" ::: Signal Dom (Bit, Bool) ->
  "result" ::: Signal Dom ()
singleInputPair = vioProbe @Dom ()
{-# ANN singleInputPair (TestBench 'top) #-}

makeTopEntity 'singleInputPair


singleInputVec ::
  "clk" ::: Clock Dom ->
  "result" ::: Signal Dom (Vec 4 (Unsigned 2)) ->
  "result" ::: Signal Dom ()
singleInputVec = vioProbe @Dom ()
{-# ANN singleInputVec (TestBench 'top) #-}

makeTopEntity 'singleInputVec


singleInputCustom ::
  "clk" ::: Clock Dom ->
  "inp" ::: Signal Dom D1 ->
  "result" ::: Signal Dom ()
singleInputCustom = vioProbe @Dom ()
{-# ANN singleInputCustom (TestBench 'top) #-}

makeTopEntity 'singleInputCustom


singleInputNested ::
  "clk" ::: Clock Dom ->
  "inp" ::: Signal Dom D2 ->
  "result" ::: Signal Dom ()
singleInputNested = vioProbe @Dom ()
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
multipleInputs = vioProbe @Dom Nil
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
inputsAndOutputs = vioProbe @Dom
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
