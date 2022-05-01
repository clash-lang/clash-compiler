{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Cores.Xilinx.DcFifo.BlackBoxes where

import Prelude

import Clash.Core.TermLiteral (termToDataError, TermLiteralSNat(..))
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
    [  _knownNatN, _knownDomainWrite, _knownDomainRead
     , _constraint1, _constraint2
     , either error id . termToDataError -> dcConfig
     , _wClk, _rClk, _rst, _wData
     , _wEnable, _rEnable

    -- TODO: Make this blackbox return multiple results, instead of a tuple. See:
    --       https://github.com/clash-lang/clash-compiler/pull/1560
    --  , _, _, _, _, _, _, _
     ] = lefts args

  dcFifoName <- Id.makeBasic "dcfifo"

  pure (Right (bbMeta dcFifoName dcConfig, bb dcFifoName dcConfig))
 where
  bbMeta dcFifoName dcConfig = emptyBlackBoxMeta
    { N.bbKind = N.TDecl
    , N.bbIncludes =
      [ ( ("dcfifo", "tcl")
        , BBFunction (show 'dcFifoTclTF) 0 (dcFifoTclTF dcFifoName dcConfig))
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

    [  _knownNatN, _knownDomainWrite, _knownDomainRead
     , _constraint1, _constraint2
     , _dcConfig
     , wClk, rClk, rst, wData
     , wEnable, rEnable
     ] = map fst (DSL.tInputs bbCtx)

  dcFifoInstName <- Id.makeBasic "dcfifo_inst"

  DSL.declarationReturn bbCtx "dcfifo_inst_block" $ do
    wrResetBusy <- DSL.declare "wr_reset_busy" N.Wire N.Bit
    wrFull      <- DSL.declare "wr_full"       N.Wire N.Bit
    wrDataCount <- DSL.declare "wr_data_count" N.Wire (N.BitVector depth)
    rdResetBusy <- DSL.declare "rd_reset_busy" N.Wire N.Bit
    rdEmpty     <- DSL.declare "rd_empty"      N.Wire N.Bit
    rdDataCount <- DSL.declare "rd_data_count" N.Wire (N.BitVector depth)
    rdDout      <- DSL.declare "rd_dout"       N.Wire (DSL.ety wData)

    wEnableBit <- DSL.boolToBit "wr_enable" wEnable
    rEnableBit <- DSL.boolToBit "rd_enable" rEnable

    let
      inps =
        [ ("rst", rst)
        , ("wr_clk", wClk)
        , ("rd_clk", rClk)
        , ("din", wData)
        , ("wr_en", wEnableBit)
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
        ]

      DSL.TExpr _ (N.Identifier rdDataCountId Nothing) = rdDataCount
      DSL.TExpr _ (N.Identifier wrDataCountId Nothing) = wrDataCount

    unless dcReadDataCount $
      DSL.addDeclaration (N.Assignment rdDataCountId (DSL.eex (DSL.bvLit depth 0)))

    unless dcWriteDataCount $
      DSL.addDeclaration (N.Assignment wrDataCountId (DSL.eex (DSL.bvLit depth 0)))

    DSL.instDecl N.Entity dcFifoName dcFifoInstName [] inps outs

    pure [DSL.tuple
      [ wrResetBusy, wrFull, wrDataCount
      , rdResetBusy, rdEmpty, rdDataCount, rdDout
      ]]

-- | Generate TCL file that calls Xilinx's `create_ip` with the options supplied
-- in the second argument.
dcFifoTclTF :: Identifier -> DcConfig TermLiteralSNat -> TemplateFunction
dcFifoTclTF dcFifoName DcConfig{..} =
  TemplateFunction [] (const True) (const (pure (renderTcl ipConfig)))
 where
  ipConfig = (defIpConfig "fifo_generator" "13.2" dcFifoName){properties = props}
  TermLiteralSNat depth = dcDepth

  props =
    [ ("Fifo_Implementation",    show dcImplementation)
    , ("Performance_Options",    show dcReadMode)
    , ("Reset_Type",             "Asynchronous_Reset")
    , ("Full_Flags_Reset_Value", "1")
    , ("Write_Data_Count",       toTclBool dcWriteDataCount)
    , ("Write_Data_Count_Width", show depth)
    , ("Read_Data_Count",        toTclBool dcReadDataCount)
    , ("Read_Data_Count_Width",  show depth)
    , ("Enable_Safety_Circuit",  toTclBool True)
    ]
