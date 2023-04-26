{-|
  Copyright   :  (C) 2023, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Xpm.Cdc.ArraySingle.Internal where

import Prelude
import Clash.Explicit.Prelude
  (type (<=), SNat, BitVector, Clock, KnownNat, KnownDomain, errorX, unsafeSynchronizer)

import Clash.Annotations.Primitive (Primitive(..), HDL(..), hasBlackBox)
import Clash.Backend (Backend)
import Clash.Netlist.Types (TemplateFunction(..), BlackBoxContext)
import Clash.Promoted.Nat (snatToNum)
import Clash.Signal.Internal (Signal((:-)))

import Control.Monad.State (State)
import Data.List.Infinite (Infinite(..), (...))
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import Text.Show.Pretty (ppShow)

import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N
import qualified Clash.Primitives.DSL as DSL

xpmCdcArraySingleTF :: TemplateFunction
xpmCdcArraySingleTF =
  TemplateFunction
    [registerInput, initBehavior, stages, clkSrc, clkDst, input]
    (const True)
    xpmCdcArraySingleTF#
 where
  _2LteN
    :< _stagesLte10
    :< _1LteN
    :< _nLte1024
    :< _knownDomainSrc
    :< _knownDomainDst
    :< _knownNatN
    :< _hasCallStack
    :< registerInput
    :< initBehavior
    :< stages
    :< clkSrc
    :< clkDst
    :< input
    :< _ = (0...)

xpmCdcArraySingleTF# :: Backend backend => BlackBoxContext -> State backend Doc
xpmCdcArraySingleTF# bbCtx
  | [ _2LteStages
    , _stagesLte10
    , _1LteN
    , _nLte1024
    , _knownDomainSrc
    , _knownDomainDst
    , _knownNatN
    , _hasCallStack
    , DSL.getBool -> Just registerInput
    , DSL.getBool -> Just initValues
    , DSL.tExprToInteger -> Just stages
    , clkSrc
    , clkDst
    , input
    ] <- map fst (DSL.tInputs bbCtx)
  , [resultTy] <- map DSL.ety (DSL.tResults bbCtx)
  = do

    let
      compName :: Text
      compName = "xpm_cdc_array_single"

      width :: Integral a => a
      width = DSL.tySize resultTy

    instName <- Id.make (compName <> "_inst")
    DSL.declarationReturn bbCtx (compName <> "_block") $ do
      result <- DSL.declare "result" (N.BitVector width)

      let
        generics :: [(Text, DSL.LitHDL)]
        generics =
          [ ("DEST_SYNC_FF", DSL.I stages)
          , ("INIT_SYNC_FF", if initValues then 1 else 0)
          , ("SIM_ASSERT_CHK", 0)
          , ("SRC_INPUT_REG", if registerInput then 1 else 0)
          , ("WIDTH", DSL.I width)
          ]

        inps :: [(Text, DSL.TExpr)]
        inps =
          [ ("src_clk", clkSrc)
          , ("dest_clk", clkDst)
          , ("src_in", input)
          ]

        outs :: [(Text, DSL.TExpr)]
        outs =
          [ ("dest_out", result)
          ]

      DSL.instDecl
        N.Empty
        (Id.unsafeMake compName)
        instName
        generics
        inps
        outs

      pure [result]

xpmCdcArraySingleTF# bbCtx = error (ppShow bbCtx)

{-# NOINLINE xpmCdcArraySingle# #-}
{-# ANN xpmCdcArraySingle# hasBlackBox #-}
{-# ANN xpmCdcArraySingle#
  let
    primName = show 'xpmCdcArraySingle#
    tfName = show 'xpmCdcArraySingleTF
  in InlineYamlPrimitive [VHDL] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      format: Haskell
      libraries: ["xpm"]
      imports: ["xpm.vcomponents.all"]
      templateFunction: #{tfName}
  |] #-}
{-# ANN xpmCdcArraySingle#
  let
    primName = show 'xpmCdcArraySingle#
    tfName = show 'xpmCdcArraySingleTF
  in InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      format: Haskell
      templateFunction: #{tfName}
  |] #-}
-- | Primitive used in 'Clash.Cores.Xilinx.Xpm.Cdc.ArraySingle.xpmCdcArraySingle'
xpmCdcArraySingle# ::
  forall stages n src dst.
  ( 2 <= stages, stages <= 10
  , 1 <= n, n <= 1024
  , KnownDomain src
  , KnownDomain dst
  , KnownNat n
  , HasCallStack
  ) =>
  -- | Register input
  Bool ->
  -- | Initial values supported
  Bool ->
  SNat stages ->
  Clock src ->
  Clock dst ->
  Signal src (BitVector n) ->
  Signal dst (BitVector n)
xpmCdcArraySingle# registerInput initValuesSupported stages clkSrc clkDst input
  | registerInput = go (snatToNum stages) (initVal :- input)
  | otherwise     = go (snatToNum stages) input
 where
  initVal
    | initValuesSupported = 0
    | otherwise = errorX "xpmCdcArraySingle: initial values undefined"

  go :: Word -> Signal src (BitVector n) -> Signal dst (BitVector n)
  go 0 src = unsafeSynchronizer clkSrc clkDst src
  go n src = initVal :- go (n - 1) src
