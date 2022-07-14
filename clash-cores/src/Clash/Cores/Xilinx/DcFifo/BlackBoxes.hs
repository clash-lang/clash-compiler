{-|
  Copyright   :  (C) 2022 Google Inc
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Blackbox implementation for primitives in "Clash.Cores.Xilinx.DcFifo.Explicit".
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Cores.Xilinx.DcFifo.BlackBoxes where

import Prelude

import Clash.Core.Literal (Literal(NaturalLiteral))
import Clash.Core.TermLiteral (termToDataError, TermLiteralSNat(..))
import Clash.Core.Term (Term(Literal))
import qualified Clash.Primitives.DSL as DSL
import Clash.Netlist.BlackBox.Types (BlackBoxFunction, emptyBlackBoxMeta)
import Clash.Netlist.Types (TemplateFunction(..), BlackBox(BBFunction))
import Clash.Netlist.Util (orNothing)

import Clash.Netlist.Id (Identifier)
import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N
import qualified Clash.Netlist.BlackBox.Types as N

import Control.Monad (unless)
import Data.Either (lefts)
import Data.Maybe (catMaybes)
import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.DcFifo.Explicit (DcConfig(..))
import Clash.Cores.Xilinx.DcFifo.Instances ()
import Clash.Cores.Xilinx.Common (toTclBool, renderTcl, defIpConfig, IpConfig (properties))

-- | Blackbox function for 'Clash.Cores.Xilinx.Fifo.dcFifo'. It parses the "DcConfig"
-- supplied to 'dcFifo' from its Term representation, and passes them to two
-- template functions:
--
--   * 'dcFifoTclTF': renders TCL file calling Xilinx's `create_ip`
--   * 'dcFifoTF': instantiates IP generated in 'dcFifoTclTF'
--
-- Additionally, it generates an unique module name for the Xilinx IP.
--
dcFifoBBF :: HasCallStack => BlackBoxFunction
dcFifoBBF _isD _primName args _resTys = do
  let
    [  knownNatN, _knownDomainWrite, _knownDomainRead, _knownNatDepth
     , _constraint1, _constraint2
     , either error id . termToDataError -> dcConfig
     , _wClk, _rClk, _rst, _wData
     , _rEnable

    -- TODO: Make this blackbox return multiple results, instead of a tuple. See:
    --       https://github.com/clash-lang/clash-compiler/pull/1560
    --  , _, _, _, _, _, _, _
     ] = lefts args

  dcFifoName <- Id.makeBasic "dcfifo"

  let depth =
        case knownNatN of
          Literal (NaturalLiteral n) -> fromInteger n
          _ -> error "Unexpected type of knownNatN!"

  pure (Right (bbMeta depth dcFifoName dcConfig, bb dcFifoName dcConfig))
 where
  bbMeta width dcFifoName dcConfig = emptyBlackBoxMeta
    { N.bbKind = N.TDecl
    , N.bbIncludes =
      [ ( ("dcfifo", "tcl")
        , BBFunction (show 'dcFifoTclTF) 0 (dcFifoTclTF width dcFifoName dcConfig))
      ]
    -- TODO: Make this blackbox return multiple results, instead of a tuple. See:
    --       https://github.com/clash-lang/clash-compiler/pull/1560
    -- , N.bbResultNames =
    --   [ N.BBTemplate [N.Text "wr_reset_busy"]
    --   , N.BBTemplate [N.Text "wr_full"]
    --   , N.BBTemplate [N.Text "wr_data_count"]

    --   , N.BBTemplate [N.Text "rd_reset_busy"]
    --   , N.BBTemplate [N.Text "rd_empty"]
    --   , N.BBTemplate [N.Text "rd_data_count"]
    --   , N.BBTemplate [N.Text "rd_dout"]
    --   ]
    }

  bb :: Identifier -> DcConfig TermLiteralSNat -> BlackBox
  bb dcFifoName dcConfig = BBFunction (show 'dcFifoTF) 0 (dcFifoTF dcFifoName dcConfig)

-- | Instantiate IP generate with 'dcFifoTclTF'.
dcFifoTF :: Identifier -> DcConfig TermLiteralSNat -> TemplateFunction
dcFifoTF dcFifoName DcConfig{..} = TemplateFunction [] (const True) $ \bbCtx -> do
  let
    TermLiteralSNat (fromIntegral -> depth) = dcDepth

    [  _knownNatN, _knownDomainWrite, _knownDomainRead, _knownNatDepth
     , _constraint1, _constraint2
     , _dcConfig
     , wClk, rClk, rst, wDataM
     , rEnable
     ] = map fst (DSL.tInputs bbCtx)

    [tResult] = map DSL.ety (DSL.tResults bbCtx)

  dcFifoInstName <- Id.makeBasic "dcfifo_inst"

  -- -1 for maybe
  let dataTy = N.BitVector (DSL.tySize (DSL.ety wDataM) - 1)
  let
    blockInps =
      [ ("rst", N.Bit)
      , ("wr_clk", N.Bit)
      , ("rd_clk", N.Bit)
      , ("din", dataTy)
      , ("wr_en", N.Bit)
      , ("rd_en", N.Bit)
      ]
    blockOuts = catMaybes
      [ Just ("dout", dataTy)
      , Just ("full", N.Bit)
      , Just ("empty", N.Bit)
      , Just ("wr_rst_busy", N.Bit)
      , Just ("rd_rst_busy", N.Bit)
      , dcReadDataCount `orNothing` ("rd_data_count", N.BitVector depth)
      , dcWriteDataCount `orNothing` ("wr_data_count", N.BitVector depth)
      , dcUnderflow `orNothing` ("underflow", N.Bit)
      , dcOverflow `orNothing` ("overflow", N.Bit)
      ]

  DSL.declarationReturn bbCtx "dcfifo_inst_block" $ do

    DSL.compInBlock "dcfifo" blockInps blockOuts

    (wEna, wData) <- DSL.deconstructMaybe wDataM ("wr_ena", "rd_din")

    wrResetBusy <- DSL.declare "wr_reset_busy" N.Bit
    wrFull      <- DSL.declare "wr_full"       N.Bit
    wrOver      <- DSL.declare "wr_overflow"   N.Bit
    wrDataCount <- DSL.declare "wr_data_count" (N.BitVector depth)
    rdResetBusy <- DSL.declare "rd_reset_busy" N.Bit
    rdEmpty     <- DSL.declare "rd_empty"      N.Bit
    rdDataCount <- DSL.declare "rd_data_count" (N.BitVector depth)
    rdUnder     <- DSL.declare "rd_underflow"  N.Bit
    rdDout      <- DSL.declare "rd_dout"       (DSL.ety wData)

    wrFullBool  <- DSL.boolFromBit "wr_full_bool" wrFull
    rdEmptyBool <- DSL.boolFromBit "rd_empty_bool" rdEmpty
    wrOverBool  <- DSL.boolFromBit "wr_over_bool" wrOver
    rdUnderBool <- DSL.boolFromBit "rd_under_bool" rdUnder

    rEnableBit <- DSL.boolToBit "rd_enable" rEnable

    wrDataCountUnsigned <- DSL.unsignedFromBitVector "wr_data_count_unsigned" wrDataCount
    rdDataCountUnsigned <- DSL.unsignedFromBitVector "rd_data_count_unsigned" rdDataCount

    wrResetBusyBool <- DSL.boolFromBit "wr_reset_busy_bool" wrResetBusy
    rdResetBusyBool <- DSL.boolFromBit "rd_reset_busy_bool" rdResetBusy

    let
      inps =
        [ ("rst", rst)
        , ("wr_clk", wClk)
        , ("rd_clk", rClk)
        , ("din", wData)
        , ("wr_en", wEna)
        , ("rd_en", rEnableBit)
        ]

      outs = catMaybes
        [ Just ("wr_rst_busy", wrResetBusy)
        , Just ("rd_rst_busy", rdResetBusy)
        , Just ("full", wrFull)
        , Just ("empty", rdEmpty)
        , Just ("dout", rdDout)
        , dcReadDataCount  `orNothing` ("rd_data_count", rdDataCount)
        , dcWriteDataCount `orNothing` ("wr_data_count", wrDataCount)
        , dcUnderflow      `orNothing` ("underflow", rdUnder)
        , dcOverflow       `orNothing` ("overflow", wrOver)
        ]

      DSL.TExpr _ (N.Identifier rdDataCountId Nothing) = rdDataCount
      DSL.TExpr _ (N.Identifier wrDataCountId Nothing) = wrDataCount

    unless dcReadDataCount $
      DSL.addDeclaration (N.Assignment rdDataCountId N.Cont (DSL.eex (DSL.bvLit depth 0)))

    unless dcWriteDataCount $
      DSL.addDeclaration (N.Assignment wrDataCountId N.Cont (DSL.eex (DSL.bvLit depth 0)))

    DSL.instDecl N.Empty dcFifoName dcFifoInstName [] inps outs

    pure [DSL.constructProduct
      tResult
      [ wrResetBusyBool, wrFullBool,  wrOverBool, wrDataCountUnsigned
      , rdResetBusyBool, rdEmptyBool, rdUnderBool, rdDataCountUnsigned, rdDout
      ]]

-- | Generate TCL file that calls Xilinx's `create_ip` with the options supplied
-- in the second argument.
dcFifoTclTF :: Int -> Identifier -> DcConfig TermLiteralSNat -> TemplateFunction
dcFifoTclTF width dcFifoName DcConfig{..} =
  TemplateFunction [] (const True) (const (pure (renderTcl ipConfig)))
 where
  ipConfig = (defIpConfig "fifo_generator" "13.2" dcFifoName){properties = props}
  TermLiteralSNat depth = dcDepth

  props =
    [ ("Fifo_Implementation",    "Independent_Clocks_Block_RAM")
    , ("Performance_Options",    "Standard_FIFO")
    , ("Reset_Type",             "Asynchronous_Reset")
    , ("Full_Flags_Reset_Value", "1")
    , ("Input_Data_Width",       show width)
    , ("Output_Data_Width",      show width)
    , ("Write_Data_Count",       toTclBool dcWriteDataCount)
    , ("Overflow_Flag",          toTclBool dcOverflow)
    , ("Write_Data_Count_Width", show depth)
    , ("Read_Data_Count",        toTclBool dcReadDataCount)
    , ("Underflow_Flag",         toTclBool dcUnderflow)
    , ("Read_Data_Count_Width",  show depth)
    , ("Enable_Safety_Circuit",  toTclBool True)
    ]
