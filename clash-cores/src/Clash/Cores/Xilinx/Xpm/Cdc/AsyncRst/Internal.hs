{-|
  Copyright   :  (C) 2023, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Xpm.Cdc.AsyncRst.Internal where

import Prelude
import Clash.Explicit.Prelude
  ( type (<=), SNat(..), Clock, Reset, KnownDomain, BitVector, SResetPolarity(..)
  , ResetPolarity(ActiveHigh, ActiveLow)
  , (.<<+), unsafeSynchronizer, deepErrorX, unsafeToReset, resetPolarity
  , unsafeToReset, unsafeFromReset, enableGen, bitToBool, boolToBit, msb )

import Clash.Annotations.Primitive (Primitive(..), HDL(..), hasBlackBox)
import Clash.Backend (Backend)
import Clash.Netlist.Types (TemplateFunction(..), BlackBoxContext)
import Clash.Signal.Internal (asyncRegister#, invertReset)

import Control.Applicative (liftA2)
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

xpmCdcAsyncRstTF :: TemplateFunction
xpmCdcAsyncRstTF =
  TemplateFunction
    [knownDomainSrc, knownDomainDst, initBehavior, stages, clkSrc, clkDst, rstSrc]
    (const True)
    xpmCdcAsyncRstTF#
 where
  _2LteN
    :< _stagesLte10
    :< knownDomainSrc
    :< knownDomainDst
    :< _hasCallStack
    :< initBehavior
    :< stages
    :< clkSrc
    :< clkDst
    :< rstSrc
    :< _ = (0...)

xpmCdcAsyncRstTF# :: Backend backend => BlackBoxContext -> State backend Doc
xpmCdcAsyncRstTF# bbCtx
  | [ _2LteStages
    , _stagesLte10
    , DSL.ety -> N.Void (Just (N.KnownDomain _domainSrc _ _ _ _ activeHighSrcRp))
    , DSL.ety -> N.Void (Just (N.KnownDomain domainDst  _ _ _ _ activeHighDstRp))
    , _hasCallStack
    , DSL.getBool -> Just initValues
    , DSL.tExprToInteger -> Just stages
    , _clkSrc
    , clkDst
    , rstSrc
    ] <- map fst (DSL.tInputs bbCtx)
  , [resultTy] <- map DSL.ety (DSL.tResults bbCtx)
  = do

    let
      activeHighSrc =
        case activeHighSrcRp of
          ActiveHigh -> True
          ActiveLow -> False

      activeHighDst =
        case activeHighDstRp of
          ActiveHigh -> True
          ActiveLow -> False

      compName :: Text
      compName = "xpm_cdc_async_rst"

    instName <- Id.make (compName <> "_inst")
    DSL.declarationReturn bbCtx (compName <> "_block") $ do
      result0 <- DSL.declare "result" (N.Reset domainDst)

      let
        generics :: [(Text, DSL.LitHDL)]
        generics =
          [ ("DEST_SYNC_FF", DSL.I stages)
          , ("INIT_SYNC_FF", if initValues then 1 else 0)
          , ("RST_ACTIVE_HIGH", if activeHighSrc then 1 else 0)
          ]

        inps :: [(Text, DSL.TExpr)]
        inps =
          [ ("dest_clk", clkDst)
          , ("src_arst", rstSrc)
          ]

        outs :: [(Text, DSL.TExpr)]
        outs =
          [ ("dest_arst", result0)
          ]

      DSL.instDecl
        N.Empty
        (Id.unsafeMake compName)
        instName
        generics
        inps
        outs

      result1 <-
        if activeHighSrc /= activeHighDst
        then DSL.notExpr "result" result0
        else pure result0

      pure <$> DSL.bitCoerce "result" resultTy result1

xpmCdcAsyncRstTF# bbCtx = error (ppShow bbCtx)

{-# NOINLINE xpmCdcAsyncRst# #-}
{-# ANN xpmCdcAsyncRst# hasBlackBox #-}
{-# ANN xpmCdcAsyncRst#
  let
    primName = show 'xpmCdcAsyncRst#
    tfName = show 'xpmCdcAsyncRstTF
  in InlineYamlPrimitive [VHDL] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      format: Haskell
      libraries: ["xpm"]
      imports: ["xpm.vcomponents.all"]
      templateFunction: #{tfName}
  |] #-}
{-# ANN xpmCdcAsyncRst#
  let
    primName = show 'xpmCdcAsyncRst#
    tfName = show 'xpmCdcAsyncRstTF
  in InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      format: Haskell
      templateFunction: #{tfName}
  |] #-}
-- | Primitive used in 'Clash.Cores.Xilinx.Xpm.Cdc.AsyncRst.xpmCdcAsyncRst'
xpmCdcAsyncRst# ::
  forall stages src dst.
  ( 2 <= stages, stages <= 10
  , KnownDomain src
  , KnownDomain dst
  , HasCallStack
  ) =>
  -- | Initial values supported
  Bool ->
  SNat stages ->
  Clock src ->
  Clock dst ->
  Reset src ->
  Reset dst
xpmCdcAsyncRst# initValuesSupported SNat clkSrc clkDst rstSrc =
  unsafeToReset (bitToBool . msb <$> shiftReg0)
 where
  shiftReg0 = asyncRegister# clkDst rstDst enableGen initVal resetVal shiftReg1
  shiftReg1 = liftA2 (.<<+) shiftReg0 (pure (boolToBit (not activeHighDst)))

  activeHighSrc :: Bool
  activeHighSrc =
    case resetPolarity @src of
      SActiveHigh -> True
      SActiveLow -> False

  activeHighDst :: Bool
  activeHighDst =
    case resetPolarity @dst of
      SActiveHigh -> True
      SActiveLow -> False

  initVal :: BitVector stages
  initVal
    | initValuesSupported = resetVal
    | otherwise = deepErrorX "xpmCdcAsyncRst: initial values undefined"

  resetVal :: BitVector stages
  resetVal = if activeHighDst then maxBound else 0

  rstDst :: Reset dst
  rstDst
    | activeHighSrc /= activeHighDst = unsafeSyncReset (invertReset rstSrc)
    | otherwise                      = unsafeSyncReset rstSrc

  unsafeSyncReset :: Reset src -> Reset dst
  unsafeSyncReset = unsafeToReset . unsafeSynchronizer clkSrc clkDst . unsafeFromReset
