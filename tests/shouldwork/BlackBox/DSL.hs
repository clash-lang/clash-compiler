{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module DSL where

import qualified Prelude as P
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import qualified Clash.Primitives.DSL as DSL
import Clash.Annotations.Primitive (Primitive(InlineYamlPrimitive))
import Clash.Netlist.Id (Identifier)
import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N
import qualified Clash.Netlist.BlackBox.Types as N
import Data.String.Interpolate (i)

funcBBF :: N.BlackBoxFunction
funcBBF _ _ _ _ =
  pure $ Right
    ( N.emptyBlackBoxMeta{N.bbKind = N.TDecl}
    , N.BBFunction (show 'funcTF) 0 funcTF )


funcTF :: N.TemplateFunction
funcTF = N.TemplateFunction [] (const True) $ \bbCtx -> do
  let
    [maybeInput] = P.map fst (DSL.tInputs bbCtx)
    [tResult] = P.map DSL.ety (DSL.tResults bbCtx)

  DSL.declarationReturn bbCtx "func_block" $ do
    (constrBit, contents) <- DSL.deconstructMaybe maybeInput ("enable", "data")
    pure [DSL.constructProduct tResult [constrBit, contents]]

func :: Maybe a -> (Bit, a)
func Nothing = (0, errorX "no data")
func (Just a) = (0, a)
{-# NOINLINE func #-}
{-# ANN func (InlineYamlPrimitive [minBound..maxBound] [i|
BlackBoxHaskell:
    name: DSL.func
    templateFunction: DSL.funcBBF
    workInfo: Always
|]) #-}

topEntity :: Signal System (Maybe Bit) -> Signal System (Bit, Bit)
topEntity maybeBit = fmap func maybeBit

testBench :: Signal System Bool
testBench = done
 where
  done = expectedOutput (pack <$> topEntity testInput)
  clk  = tbSystemClockGen (not <$> done)
  rst  = systemResetGen

  testInput =
    stimuliGenerator clk rst $ Nothing :> Just 0 :> Just 1 :> Nil

  testOutput :: Vec 3 (Bit, Bit)
  testOutput =
       (0, errorX "undefined")
    :> (1, 0)
    :> (1, 1)
    :> Nil

  expectedOutput =
    outputVerifierBitVector' clk rst (map pack testOutput)
