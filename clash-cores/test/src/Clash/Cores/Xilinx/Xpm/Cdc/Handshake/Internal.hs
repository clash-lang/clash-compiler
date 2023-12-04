{-|
  Copyright   :  (C) 2023, Google LLC,
                     2023, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Xpm.Cdc.Handshake.Internal where

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
  ( type (<=), type (:::), SNat, Clock, KnownDomain, Signal, BitPack(BitSize)
  , NFDataX, deepErrorX, unsafeSynchronizer, enableGen, delay, toEnable,
  (.&&.), unpack )
import Clash.Netlist.Types (TemplateFunction(..), BlackBoxContext)

import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N
import qualified Clash.Primitives.DSL as DSL

import Clash.Cores.Xilinx.Xpm.Cdc.Single (XpmCdcSingleConfig(..), xpmCdcSingleWith)

xpmCdcHandshakeTF :: TemplateFunction
xpmCdcHandshakeTF =
  TemplateFunction
    [initBehavior, srcStages0, dstStages0, clkSrc, clkDst, srcIn, srcSend, dstAck]
    (const True)
    xpmCdcHandshakeTF#
 where
  _2LteSrcStages
    :< _srcStagesLte10
    :< _2LteDstStages
    :< _dstStagesLte10
    :< _1LteBitsize
    :< _bitsizeLte1024
    :< _knownDomainSrc
    :< _knownDomainDst
    :< _bitpackA
    :< _nfdataxA
    :< _hasCallStack
    :< initBehavior
    :< srcStages0
    :< dstStages0
    :< clkSrc
    :< clkDst
    :< srcIn
    :< srcSend
    :< dstAck
    :< _ = (0...)

xpmCdcHandshakeTF# :: Backend backend => BlackBoxContext -> State backend Doc
xpmCdcHandshakeTF# bbCtx
  | [ _2LteSrcStages, _srcStagesLte10
    , _2LteDstStages, _dstStagesLte10
    , _1LteBitsize, _bitsizeLte1024
    , _knownDomainSrc
    , _knownDomainDst
    , _bitpackA
    , _nfdataxA
    , _hasCallStack
    , DSL.getBool -> Just initValues0
    , DSL.tExprToInteger -> Just srcStages0
    , DSL.tExprToInteger -> Just dstStages0
    , clkSrc
    , clkDst
    , srcIn
    , srcSend
    , dstAck
    ] <- map fst (DSL.tInputs bbCtx)
  = do

    let
      compName :: Text
      compName = "xpm_cdc_handshake"

      width :: Integral a => a
      width = DSL.tySize (DSL.ety srcIn)

    instName <- Id.make (compName <> "_inst")
    DSL.declarationReturn bbCtx (compName <> "_block") $ do
      srcSendBit <- DSL.bitCoerce "src_send" N.Bit srcSend
      dstAckBit <- DSL.bitCoerce "dst_ack" N.Bit dstAck

      destOutBv <- DSL.declare "dest_out" (N.BitVector width)
      destOut <- DSL.bitCoerce "dest_out" (DSL.ety srcIn) destOutBv

      srcInBv <- DSL.bitCoerce "src_in" (N.BitVector width) srcIn

      destReqBit <- DSL.declare "dest_req" N.Bit
      destReq <- DSL.boolFromBit "dest_req" destReqBit

      srcRcvBit <- DSL.declare "src_rcv" N.Bit
      srcRcv <- DSL.boolFromBit "src_rcv" srcRcvBit

      let
        generics :: [(Text, DSL.LitHDL)]
        generics =
          [ ("DEST_EXT_HSK", DSL.I 1)
          , ("DEST_SYNC_FF", DSL.I dstStages0)
          , ("INIT_SYNC_FF", if initValues0 then 1 else 0)
          , ("SIM_ASSERT_CHK", 0)
          , ("SRC_SYNC_FF", DSL.I srcStages0)
          , ("WIDTH", DSL.I width)
          ]

        inps :: [(Text, DSL.TExpr)]
        inps =
          [ ("src_clk", clkSrc)
          , ("dest_clk", clkDst)
          , ("src_in", srcInBv)
          , ("src_send", srcSendBit)
          , ("dest_ack", dstAckBit)
          ]

        outs :: [(Text, DSL.TExpr)]
        outs =
          [ ("dest_out", destOutBv)
          , ("dest_req", destReqBit)
          , ("src_rcv", srcRcvBit)
          ]

      DSL.instDecl
        N.Empty
        (Id.unsafeMake compName)
        instName
        (map (second DSL.litTExpr) generics)
        inps
        outs

      pure [DSL.tuple [destOut, destReq, srcRcv]]

xpmCdcHandshakeTF# bbCtx = error (ppShow bbCtx)

{-# NOINLINE xpmCdcHandshake# #-}
{-# ANN xpmCdcHandshake# hasBlackBox #-}
{-# ANN xpmCdcHandshake#
  let
    primName = show 'xpmCdcHandshake#
    tfName = show 'xpmCdcHandshakeTF
  in InlineYamlPrimitive [VHDL] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      format: Haskell
      libraries: ["xpm"]
      imports: ["xpm.vcomponents.all"]
      templateFunction: #{tfName}
  |] #-}
{-# ANN xpmCdcHandshake#
  let
    primName = show 'xpmCdcHandshake#
    tfName = show 'xpmCdcHandshakeTF
  in InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      format: Haskell
      templateFunction: #{tfName}
  |] #-}
-- | Primitive used in 'Clash.Cores.Xilinx.Xpm.Cdc.Handshake.xpmCdcHandshake'
xpmCdcHandshake# ::
  forall srcStages dstStages a src dst.
  ( 2 <= srcStages, srcStages <= 10
  , 2 <= dstStages, dstStages <= 10
  , 1 <= BitSize a, BitSize a <= 1024
  , KnownDomain src
  , KnownDomain dst
  , BitPack a
  , NFDataX a
  , HasCallStack
  ) =>
  -- | Initial value usage
  Bool ->
  SNat srcStages ->
  SNat dstStages ->
  Clock src ->
  Clock dst ->
  "src_in"   ::: Signal src a ->
  "src_send" ::: Signal src Bool ->
  "dst_ack"  ::: Signal dst Bool ->

  ( "dest_out" ::: Signal dst a
  , "dest_req" ::: Signal dst Bool
  , "src_rcv"  ::: Signal src Bool
  )
xpmCdcHandshake# initVals srcStages dstStages clkSrc clkDst srcIn srcSend dstAck =
  (dstOut, dstReq, srcRcv)
 where
  defOpts :: forall stages. SNat stages -> XpmCdcSingleConfig stages
  defOpts nStages = XpmCdcSingleConfig
    { stages = nStages
    , initialValues = initVals
    , registerInput = False }

  srcSendFfSynced = xpmCdcSingleWith (defOpts dstStages) clkSrc clkDst srcSendFf
  srcRcv = xpmCdcSingleWith (defOpts srcStages) clkDst clkSrc dstAck

  srcSendFf = delay clkSrc enableGen (initVal False) srcSend
  srcHsDataFf = delay clkSrc (toEnable (not <$> srcSendFf)) (initVal (unpack 0)) srcIn
  dstOutEna = toEnable (srcSendFfSynced .&&. fmap not dstReq)

  dstOut =
    delay
      clkDst dstOutEna (initVal (unpack 0))
      (unsafeSynchronizer clkSrc clkDst srcHsDataFf)

  dstReq = delay clkDst enableGen (initVal False) srcSendFfSynced

  initVal :: forall x . NFDataX x => x -> x
  initVal v
    | initVals = v
    | otherwise = deepErrorX "xpmCdcHandshake: initial values undefined"
