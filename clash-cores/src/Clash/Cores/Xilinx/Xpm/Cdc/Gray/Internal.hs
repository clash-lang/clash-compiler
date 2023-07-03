{-|
  Copyright   :  (C) 2023, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Xpm.Cdc.Gray.Internal where

import Prelude
import Clash.Explicit.Prelude
  ( type (<=), KnownNat, SNat, Unsigned, Clock, KnownDomain, errorX
  , unsafeSynchronizer )

import Clash.Annotations.Primitive (Primitive(..), HDL(..), hasBlackBox)
import Clash.Backend (Backend)
import Clash.Netlist.Types (TemplateFunction(..), BlackBoxContext)
import Clash.Promoted.Nat (snatToNum)
import Clash.Signal.Internal (Signal((:-)))

import Control.Monad.State (State)
import Data.Bifunctor (second)
import Data.List.Infinite (Infinite(..), (...))
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import Text.Show.Pretty (ppShow)

import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N
import qualified Clash.Primitives.DSL as DSL

xpmCdcGrayTF :: TemplateFunction
xpmCdcGrayTF =
  TemplateFunction
    [initBehavior, stages, clkSrc, clkDst, input]
    (const True)
    xpmCdcGrayTF#
 where
  _2LteN
    :< _nLte32
    :< _2LteStages
    :< _stagesLte10
    :< _knownNatN
    :< _knownDomainSrc
    :< _knownDomainDst
    :< _hasCallStack
    :< initBehavior
    :< stages
    :< clkSrc
    :< clkDst
    :< input
    :< _ = (0...)

xpmCdcGrayTF# :: Backend backend => BlackBoxContext -> State backend Doc
xpmCdcGrayTF# bbCtx
  | [ _2LteN
    , _nLte32
    , _2LteStages
    , _stagesLte10
    , _knownNatN
    , _knownDomainSrc
    , _knownDomainDst
    , _hasCallStack
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
      compName = "xpm_cdc_gray"

      width :: Integral a => a
      width = DSL.tySize resultTy

    instName <- Id.make (compName <> "_inst")
    DSL.declarationReturn bbCtx (compName <> "_block") $ do
      inputBv <- DSL.toBV "src_in_bin_bv" input
      resultBv <- DSL.declare "dest_out_bin_bv" (N.BitVector width)
      result <- DSL.fromBV "dest_out_bin" resultTy resultBv

      let
        generics :: [(Text, DSL.LitHDL)]
        generics =
          [ ("DEST_SYNC_FF", DSL.I stages)
          , ("INIT_SYNC_FF", if initValues then 1 else 0)
          , ("REG_OUTPUT", 0)
          , ("SIM_ASSERT_CHK", 0)
          , ("SIM_LOSSLESS_GRAY_CHK", 0)
          , ("WIDTH", DSL.I width)
          ]

        inps :: [(Text, DSL.TExpr)]
        inps =
          [ ("src_clk", clkSrc)
          , ("dest_clk", clkDst)
          , ("src_in_bin", inputBv)
          ]

        outs :: [(Text, DSL.TExpr)]
        outs =
          [ ("dest_out_bin", resultBv)
          ]

      DSL.instDecl
        N.Empty
        (Id.unsafeMake compName)
        instName
        (map (second DSL.litTExpr) generics)
        inps
        outs

      pure [result]

xpmCdcGrayTF# bbCtx = error (ppShow bbCtx)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE xpmCdcGray# #-}
{-# ANN xpmCdcGray# hasBlackBox #-}
{-# ANN xpmCdcGray#
  let
    primName = show 'xpmCdcGray#
    tfName = show 'xpmCdcGrayTF
  in InlineYamlPrimitive [VHDL] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      format: Haskell
      libraries: ["xpm"]
      imports: ["xpm.vcomponents.all"]
      templateFunction: #{tfName}
  |] #-}
{-# ANN xpmCdcGray#
  let
    primName = show 'xpmCdcGray#
    tfName = show 'xpmCdcGrayTF
  in InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      format: Haskell
      templateFunction: #{tfName}
  |] #-}
-- | Primitive used in 'Clash.Cores.Xilinx.Xpm.Cdc.Gray.xpmCdcGray'
xpmCdcGray# ::
  forall stages n src dst.
  ( 2 <= n, n <= 32
  , 2 <= stages, stages <= 10
  , KnownNat n
  , KnownDomain src
  , KnownDomain dst
  , HasCallStack
  ) =>
  -- | Initial value usage
  Bool ->
  SNat stages ->
  Clock src ->
  Clock dst ->
  Signal src (Unsigned n) ->
  Signal dst (Unsigned n)
xpmCdcGray# initValuesSupported stages clkSrc clkDst input =
  go (snatToNum stages) (initVal :- input)
 where
  initVal
    | initValuesSupported = 0
    | otherwise = errorX "xpmCdcGray: initial values undefined"

  go :: Word -> Signal src (Unsigned n) -> Signal dst (Unsigned n)
  go 0 src = unsafeSynchronizer clkSrc clkDst src
  go n src = initVal :- go (n - 1) src
