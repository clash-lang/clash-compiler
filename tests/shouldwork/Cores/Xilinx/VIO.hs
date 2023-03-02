module VIO where

import Clash.Prelude
import Clash.Cores.Xilinx.VIO
import Clash.Annotations.TH
import Clash.Annotations.BitRepresentation

type Dom = XilinxSystem

noInputTrue ::
  "clk" ::: Clock Dom ->
  "out" ::: Signal Dom Bool
noInputTrue = vioProbe @Dom True

makeTopEntityWithName 'noInputTrue ""


noInputFalse ::
  "clk" ::: Clock Dom ->
  "out" ::: Signal Dom Bool
noInputFalse = vioProbe @Dom False

makeTopEntityWithName 'noInputFalse ""


noInputLow ::
  "clk" ::: Clock Dom ->
  "out" ::: Signal Dom Bit
noInputLow = vioProbe @Dom low

makeTopEntityWithName 'noInputLow ""


noInputHigh ::
  "clk" ::: Clock Dom ->
  "out" ::: Signal Dom Bit
noInputHigh = vioProbe @Dom high

makeTopEntityWithName 'noInputHigh ""


noInputSigned ::
  "clk" ::: Clock Dom ->
  "out" ::: Signal Dom (Signed 2)
noInputSigned = vioProbe @Dom (-1)

makeTopEntityWithName 'noInputSigned ""


noInputUnsigned ::
  "clk" ::: Clock Dom ->
  "out" ::: Signal Dom (Unsigned 2)
noInputUnsigned = vioProbe @Dom 3

makeTopEntityWithName 'noInputUnsigned ""


noInputBitVector ::
  "clk" ::: Clock Dom ->
  "out" ::: Signal Dom (BitVector 7)
noInputBitVector = vioProbe @Dom 111

makeTopEntityWithName 'noInputBitVector ""


noInputPair ::
  "clk" ::: Clock Dom ->
  "out" ::: Signal Dom (Bit, Bool)
noInputPair = vioProbe @Dom (high, False)

makeTopEntityWithName 'noInputPair ""


noInputVec ::
  "clk" ::: Clock Dom ->
  "out" ::: Signal Dom (Vec 4 (Unsigned 2))
noInputVec = vioProbe @Dom (0 :> 1 :> 2 :> 3 :> Nil)

makeTopEntityWithName 'noInputVec ""


data D1 = D1 Bool Bit (Unsigned 2)

noInputCustom ::
  "clk" ::: Clock Dom ->
  "out" ::: Signal Dom D1
noInputCustom = vioProbe @Dom (D1 True high 1)

makeTopEntityWithName 'noInputCustom ""


data D2 = D2 Bool (Vec 2 D1)

noInputNested ::
  "clk" ::: Clock Dom ->
  "out" ::: Signal Dom D2
noInputNested = vioProbe @Dom (D2 True (D1 True high 1 :> D1 False low 0 :> Nil))

makeTopEntityWithName 'noInputNested ""


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
  "out" ::: Signal Dom T
noInputCustomRep = vioProbe @Dom (R True False)

makeTopEntityWithName 'noInputCustomRep ""
-}


singleInputBool ::
  "clk" ::: Clock Dom ->
  "in" ::: Signal Dom Bool ->
  "out" ::: Signal Dom ()
singleInputBool = vioProbe @Dom ()

makeTopEntityWithName 'singleInputBool ""


singleInputBit ::
  "clk" ::: Clock Dom ->
  "in" ::: Signal Dom Bit ->
  "out" ::: Signal Dom ()
singleInputBit = vioProbe @Dom ()

makeTopEntityWithName 'singleInputBit ""


singleInputSigned ::
  "clk" ::: Clock Dom ->
  "in" ::: Signal Dom (Signed 2) ->
  "out" ::: Signal Dom ()
singleInputSigned = vioProbe @Dom ()

makeTopEntityWithName 'singleInputSigned ""


singleInputUnsigned ::
  "clk" ::: Clock Dom ->
  "in" ::: Signal Dom (Unsigned 2) ->
  "out" ::: Signal Dom ()
singleInputUnsigned = vioProbe @Dom ()

makeTopEntityWithName 'singleInputUnsigned ""


singleInputBitVector ::
  "clk" ::: Clock Dom ->
  "in" ::: Signal Dom (BitVector 7) ->
  "out" ::: Signal Dom ()
singleInputBitVector = vioProbe @Dom ()

makeTopEntityWithName 'singleInputBitVector ""


singleInputPair ::
  "clk" ::: Clock Dom ->
  "in" ::: Signal Dom (Bit, Bool) ->
  "out" ::: Signal Dom ()
singleInputPair = vioProbe @Dom ()

makeTopEntityWithName 'singleInputPair ""


singleInputVec ::
  "clk" ::: Clock Dom ->
  "out" ::: Signal Dom (Vec 4 (Unsigned 2)) ->
  "out" ::: Signal Dom ()
singleInputVec = vioProbe @Dom ()

makeTopEntityWithName 'singleInputVec ""


singleInputCustom ::
  "clk" ::: Clock Dom ->
  "in" ::: Signal Dom D1 ->
  "out" ::: Signal Dom ()
singleInputCustom = vioProbe @Dom ()

makeTopEntityWithName 'singleInputCustom ""


singleInputNested ::
  "clk" ::: Clock Dom ->
  "in" ::: Signal Dom D2 ->
  "out" ::: Signal Dom ()
singleInputNested = vioProbe @Dom ()

makeTopEntityWithName 'singleInputNested ""


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
  "out" ::: Signal Dom (Vec 0 Bool)
multipleInputs = vioProbe @Dom Nil

makeTopEntityWithName 'multipleInputs ""


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
  "out" ::: Signal Dom ( Bit
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

makeTopEntityWithName 'inputsAndOutputs ""
