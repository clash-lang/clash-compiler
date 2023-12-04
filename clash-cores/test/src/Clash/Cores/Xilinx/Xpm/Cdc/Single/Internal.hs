{-|
  Copyright   :  (C) 2023, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Xpm.Cdc.Single.Internal where

import Prelude

import Control.Monad.State (State)
import Data.Bifunctor (second)
import Data.List.Infinite (Infinite(..), (...))
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import Text.Show.Pretty (ppShow)

import Clash.Annotations.Primitive (Primitive(..), HDL(..), hasBlackBox)
import Clash.Backend (Backend)
import Clash.Explicit.Prelude
  ( type (<=), SNat, Clock, KnownDomain, BitPack(BitSize), NFDataX, deepErrorX
  , unsafeSynchronizer, unpack )
import Clash.Netlist.Types (TemplateFunction(..), BlackBoxContext)
import Clash.Promoted.Nat (snatToNum)
import Clash.Signal.Internal (Signal((:-)))

import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N
import qualified Clash.Primitives.DSL as DSL

xpmCdcSingleTF :: TemplateFunction
xpmCdcSingleTF =
  TemplateFunction
    [registerInput, initBehavior, stages, clkSrc, clkDst, input]
    (const True)
    xpmCdcSingleTF#
 where
  _2LteN
    :< _stagesLte10
    :< _knownDomainSrc
    :< _knownDomainDst
    :< _hasCallStack
    :< _nfdatax
    :< _bitpack
    :< _bitsize
    :< registerInput
    :< initBehavior
    :< stages
    :< clkSrc
    :< clkDst
    :< input
    :< _ = (0...)

xpmCdcSingleTF# :: Backend backend => BlackBoxContext -> State backend Doc
xpmCdcSingleTF# bbCtx
  | [ _2LteStages
    , _stagesLte10
    , _knownDomainSrc
    , _knownDomainDst
    , _hasCallStack
    , _nfdatax
    , _bitpack
    , _bitsize
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
      compName = "xpm_cdc_single"

    instName <- Id.make (compName <> "_inst")
    DSL.declarationReturn bbCtx (compName <> "_block") $ do
      resultBit <- DSL.declare "result_bit" N.Bit
      inputBit <- DSL.bitCoerce "src_in" N.Bit input

      let
        generics :: [(Text, DSL.LitHDL)]
        generics =
          [ ("DEST_SYNC_FF", DSL.I stages)
          , ("INIT_SYNC_FF", if initValues then 1 else 0)
          , ("SIM_ASSERT_CHK", 0)
          , ("SRC_INPUT_REG", if registerInput then 1 else 0)
          ]

        inps :: [(Text, DSL.TExpr)]
        inps =
          [ ("src_clk", clkSrc)
          , ("dest_clk", clkDst)
          , ("src_in", inputBit)
          ]

        outs :: [(Text, DSL.TExpr)]
        outs =
          [ ("dest_out", resultBit)
          ]

      DSL.instDecl
        N.Empty
        (Id.unsafeMake compName)
        instName
        (map (second DSL.litTExpr) generics)
        inps
        outs

      pure <$> DSL.bitCoerce "result" resultTy resultBit

xpmCdcSingleTF# bbCtx = error (ppShow bbCtx)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE xpmCdcSingle# #-}
{-# ANN xpmCdcSingle# hasBlackBox #-}
{-# ANN xpmCdcSingle#
  let
    primName = show 'xpmCdcSingle#
    tfName = show 'xpmCdcSingleTF
  in InlineYamlPrimitive [VHDL] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      format: Haskell
      libraries: ["xpm"]
      imports: ["xpm.vcomponents.all"]
      templateFunction: #{tfName}
  |] #-}
{-# ANN xpmCdcSingle#
  let
    primName = show 'xpmCdcSingle#
    tfName = show 'xpmCdcSingleTF
  in InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      format: Haskell
      templateFunction: #{tfName}
  |] #-}
-- | Primitive used in 'Clash.Cores.Xilinx.Xpm.Cdc.Single.xpmCdcSingle'
xpmCdcSingle# ::
  forall stages a src dst.
  ( 2 <= stages, stages <= 10
  , KnownDomain src
  , KnownDomain dst
  , HasCallStack
  , NFDataX a
  , BitPack a
  , BitSize a ~ 1
  ) =>
  -- | Register input
  Bool ->
  -- | Initial value usage
  Bool ->
  SNat stages ->
  Clock src ->
  Clock dst ->
  Signal src a ->
  Signal dst a
xpmCdcSingle# registerInput initValuesUsed stages clkSrc clkDst input
  | registerInput = go (snatToNum stages) (initVal :- input)
  | otherwise     = go (snatToNum stages) input
 where
  initVal
    | initValuesUsed = unpack 0
    | otherwise = deepErrorX "xpmCdcSingle: initial values undefined"

  go :: Word -> Signal src a -> Signal dst a
  go 0 src = unsafeSynchronizer clkSrc clkDst src
  go n src = initVal :- go (n - 1) src
