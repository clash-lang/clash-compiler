{-|
  Copyright   :  (C) 2022 Google Inc
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Blackbox implementation for primitives in "Clash.Cores.Xilinx.DcFifo".
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.DcFifo.Internal.BlackBoxes where

import Prelude

import Control.Monad.State (State)
import Data.Either (lefts)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)

import Clash.Backend (Backend)
import Clash.Core.TermLiteral (termToDataError)
import Clash.Netlist.BlackBox.Types (BlackBoxFunction, emptyBlackBoxMeta)
import Clash.Netlist.Types (TemplateFunction(..), BlackBox(BBFunction), BlackBoxContext)
import Clash.Netlist.Util (orNothing, stripVoid)
import Clash.Signal.Internal (ResetKind(..))
import Clash.Promoted.Nat (snatToNum)

import qualified Clash.Primitives.DSL as DSL
import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N
import qualified Clash.Netlist.BlackBox.Types as N

import Clash.Cores.Xilinx.DcFifo.Internal.Instances ()
import Clash.Cores.Xilinx.DcFifo.Internal.Types (DcConfig(..))
import Clash.Cores.Xilinx.Internal
  (defIpConfig, IpConfig(properties), property, renderTcl, TclPurpose(..))

-- | Blackbox function for 'Clash.Cores.Xilinx.DcFifo.dcFifo'. It parses the
-- @DcConfig@ supplied to @dcFifo@ from its Term representation, and passes them
-- to two template functions:
--
--   * 'dcFifoTclTF': renders Tcl file conforming to the /Clash\<->Tcl API/,
--   creating the Xilinx IP with @create_ip@
--   * 'dcFifoTF': instantiates IP generated in @dcFifoTclTF@
dcFifoBBF :: HasCallStack => BlackBoxFunction
dcFifoBBF _isD _primName args _resTys
  |  [  _knownDomainWrite, _knownDomainRead
     , _nfDataX, _knownNatDepth
     , _constraint1, _constraint2, _hasCallStack
     , either error id . termToDataError -> dcConfig
     , _wClk, _wRst, _rClk, _rRst, _wData
     , _rEnable

    -- TODO: Make this blackbox return multiple results, instead of a tuple. See:
    --       https://github.com/clash-lang/clash-compiler/pull/1560
    --  , _, _, _, _, _, _, _
     ] <- lefts args
  =
 let
  bbMeta = emptyBlackBoxMeta
    { N.bbKind = N.TDecl
    , N.bbIncludes =
      [ ( ("dcfifo", "clash.tcl")
        , BBFunction (show 'dcFifoTclTF) 0 (dcFifoTclTF dcConfig))
      ]
    -- TODO: Make this blackbox return multiple results, instead of a tuple. See:
    --       https://github.com/clash-lang/clash-compiler/pull/1560
    -- , N.bbResultNames =
    --   , N.BBTemplate [N.Text "wr_full"]
    --   , N.BBTemplate [N.Text "wr_data_count"]

    --   , N.BBTemplate [N.Text "rd_empty"]
    --   , N.BBTemplate [N.Text "rd_data_count"]
    --   , N.BBTemplate [N.Text "rd_dout"]
    --   ]
    }

  bb :: BlackBox
  bb = BBFunction (show 'dcFifoTF) 0 (dcFifoTF dcConfig)
 in
  pure $ Right (bbMeta, bb)

dcFifoBBF _ _ args _ = error ("dcFifoBBF, bad args: " <> show args)

-- | Instantiate IP generated with 'dcFifoTclTF'
dcFifoTF :: HasCallStack => DcConfig n -> TemplateFunction
dcFifoTF config =
  TemplateFunction
    -- ( KnownDomain write        -- 0
    -- , KnownDomain read         -- 1
    -- , NFDataX a                -- 2
    -- , KnownNat depth           -- 3
    -- , 4 <= depth               -- 4
    -- , depth <= 17              -- 5
    -- , HasCallStack             -- 6
    -- ) =>
    -- DcConfig (SNat depth) ->   -- 7 Note: argument passed to this function
    -- Clock write ->             -- 8
    -- Reset write ->             -- 9
    -- Clock read ->              -- 10
    -- Reset read ->              -- 11
    -- Signal write (Maybe a) ->  -- 12
    -- Signal read Bool ->        -- 13
    [0, 1, 7, 8, 9, 10, 11, 12, 13]
    (const True)
    (dcFifoBBTF config)
 where

dcFifoBBTF ::
  Backend s =>
  DcConfig n ->
  BlackBoxContext ->
  State s Doc
dcFifoBBTF DcConfig{..} bbCtx
  | [  knownDomainWrite, knownDomainRead
    , _nfDataX, _knownNatDepth
    , _constraint1, _constraint2, _hasCallStack
    , _dcConfig
    , wClk, wRst, rClk, rRst, wDataM
    , rEnable
    ] <- map fst (DSL.tInputs bbCtx)
  , [tResult] <- map DSL.ety (DSL.tResults bbCtx)
  , [dcFifoName] <- N.bbQsysIncName bbCtx
  = do
  let
    depth = snatToNum dcDepth

  dcFifoInstName <- Id.makeBasic "dcfifo_inst"

  -- -1 for maybe
  let dataTy = N.BitVector (DSL.tySize (DSL.ety wDataM) - 1)
  let
    compInps =
      [ ("wr_clk", N.Bit)
      , ("wr_rst", N.Bit)
      , ("rd_clk", N.Bit)
      , ("rd_rst", N.Bit)
      , ("din", dataTy)
      , ("wr_en", N.Bit)
      , ("rd_en", N.Bit)
      ]
    compOuts = catMaybes
      [ Just ("dout", dataTy)
      , Just ("full", N.Bit)
      , Just ("empty", N.Bit)
      , dcReadDataCount `orNothing` ("rd_data_count", N.BitVector depth)
      , dcWriteDataCount `orNothing` ("wr_data_count", N.BitVector depth)
      , dcUnderflow `orNothing` ("underflow", N.Bit)
      , dcOverflow `orNothing` ("overflow", N.Bit)
      ]

  DSL.declarationReturn bbCtx "dcfifo_inst_block" $ do

    DSL.compInBlock dcFifoName compInps compOuts

    (wEna, wData) <- DSL.deconstructMaybe wDataM ("wr_enable", "wr_din")

    wrFull      <- DSL.declare "wr_full"       N.Bit
    rdEmpty     <- DSL.declare "rd_empty"      N.Bit
    rdDoutBV    <- DSL.declare "rd_dout_bv"    dataTy

    wDataBV     <- DSL.toBV    "wr_din_bv"     wData

    rdDout      <- DSL.fromBV "rd_dout" (DSL.ety wData) rdDoutBV

    wrFullBool  <- DSL.boolFromBit "wr_full_bool" wrFull
    rdEmptyBool <- DSL.boolFromBit "rd_empty_bool" rdEmpty
    rEnableBit <- DSL.boolToBit "rd_enable" rEnable

    wRstHigh <-
      let domty = DSL.ety knownDomainWrite
      in case stripVoid domty of
           N.KnownDomain _ _ _ Synchronous _ _ ->
             DSL.unsafeToActiveHigh "wr_rst_high" domty wRst
           N.KnownDomain _ _ _ Asynchronous _ _ ->
             error $
               show 'dcFifoTF <> ": dcFifo only supports synchronous resets"
           _ ->
             error $ show 'dcFifoTF <> ": Bug: Not a KnownDomain " <>
                     "constraint, mismatch between function and its blackbox"

    rRstHigh <-
      let domty = DSL.ety knownDomainRead
      in case stripVoid domty of
           N.KnownDomain _ _ _ Synchronous _ _ ->
             DSL.unsafeToActiveHigh "rd_rst_high" domty rRst
           N.KnownDomain _ _ _ Asynchronous _ _ ->
             error $
               show 'dcFifoTF <> ": dcFifo only supports synchronous resets"
           _ ->
             error $ show 'dcFifoTF <> ": Bug: Not a KnownDomain " <>
                     "constraint, mismatch between function and its blackbox"

    (rdDataCountUnsigned, rdDataCountPort) <-
      if dcReadDataCount then do
        rdDataCount <- DSL.declare "rd_data_count" (N.BitVector depth)
        rdDataCountUnsigned <-
          DSL.unsignedFromBitVector "rd_data_count_unsigned" rdDataCount
        pure (rdDataCountUnsigned, Just ("rd_data_count", rdDataCount))
      else do
        rdDataCountUnsigned <-
          DSL.declare "rd_data_count_unsigned" (N.Unsigned depth)
        pure (rdDataCountUnsigned, Nothing)

    (wrDataCountUnsigned, wrDataCountPort) <-
      if dcWriteDataCount then do
        wrDataCount <- DSL.declare "wr_data_count" (N.BitVector depth)
        wrDataCountUnsigned <-
          DSL.unsignedFromBitVector "wr_data_count_unsigned" wrDataCount
        pure (wrDataCountUnsigned, Just ("wr_data_count", wrDataCount))
      else do
        wrDataCountUnsigned <-
          DSL.declare "wr_data_count_unsigned" (N.Unsigned depth)
        pure (wrDataCountUnsigned, Nothing)

    (rdUnderBool, rdUnderPort) <-
      if dcUnderflow then do
        rdUnder <- DSL.declare "rd_underflow"   N.Bit
        rdUnderBool <- DSL.boolFromBit "rd_under_bool" rdUnder
        pure (rdUnderBool, Just ("underflow", rdUnder))
      else do
        rdUnderBool <- DSL.declare "rd_under_bool" N.Bool
        pure (rdUnderBool, Nothing)

    (wrOverBool, wrOverPort) <-
      if dcOverflow then do
        wrOver <- DSL.declare "wr_overflow"   N.Bit
        wrOverBool <- DSL.boolFromBit "wr_over_bool" wrOver
        pure (wrOverBool, Just ("overflow", wrOver))
      else do
        wrOverBool <- DSL.declare "wr_over_bool" N.Bool
        pure (wrOverBool, Nothing)

    let
      inps =
        [ ("wr_clk", wClk)
        , ("wr_rst", wRstHigh)
        , ("rd_clk", rClk)
        , ("rd_rst", rRstHigh)
        , ("din", wDataBV)
        , ("wr_en", wEna)
        , ("rd_en", rEnableBit)
        ]

      outs = catMaybes
        [ Just ("full", wrFull)
        , Just ("empty", rdEmpty)
        , Just ("dout", rdDoutBV)
        , rdDataCountPort
        , wrDataCountPort
        , rdUnderPort
        , wrOverPort
        ]

    DSL.instDecl N.Empty (Id.unsafeMake dcFifoName) dcFifoInstName [] inps outs

    pure [DSL.constructProduct
      tResult
      [ wrFullBool,  wrOverBool, wrDataCountUnsigned
      , rdEmptyBool, rdUnderBool, rdDataCountUnsigned, rdDout
      ]]

dcFifoBBTF _ bbCtx = error ("dcFifoBBTF, bad bbCtx: " <> show bbCtx)

-- | Renders Tcl file conforming to the /Clash\<->Tcl API/, creating the Xilinx
-- IP with @create_ip@
dcFifoTclTF :: HasCallStack => DcConfig n -> TemplateFunction
dcFifoTclTF conf =
  TemplateFunction
    -- ( KnownDomain write        -- 0
    -- , KnownDomain read         -- 1
    -- , NFDataX a                -- 2
    -- , KnownNat depth           -- 3
    -- , 4 <= depth               -- 4
    -- , depth <= 17              -- 5
    -- , HasCallStack             -- 6
    -- ) =>
    -- DcConfig (SNat depth) ->   -- 7 Note: argument passed to this function
    -- Clock write ->             -- 8
    -- Reset write ->             -- 9
    -- Clock read ->              -- 10
    -- Reset read ->              -- 11
    -- Signal write (Maybe a) ->  -- 12
    -- Signal read Bool ->        -- 13
    [7, 12]
    (const True)
    (dcFifoTclBBTF conf)

dcFifoTclBBTF ::
  Backend s =>
  DcConfig n ->
  BlackBoxContext ->
  State s Doc
dcFifoTclBBTF DcConfig{..} bbCtx
  | [dcFifoName] <- N.bbQsysIncName bbCtx
  , [  _knownDomainWrite, _knownDomainRead, _nfDataX
    , _knownNatDepth, _constraint1, _constraint2, _hasCallStack
    , _dcConfig, _wClk, _wRst, _rClk, _rRst, wDataM, _rEnable
    ] <- map fst (DSL.tInputs bbCtx)
  , let width = DSL.tySize (DSL.ety wDataM) - 1 :: Int -- (-1) cause it's Maybe
  = pure (renderTcl [IpConfigPurpose $ ipConfig dcFifoName width])
  where
  depth = snatToNum @Int dcDepth
  ipConfig nm width = (defIpConfig "fifo_generator" "13.2" nm){properties = props width}

  -- NOTE: The product guide listed the wrong default value for
  -- "use_dout_register" (which in reality defaults to "true" for UltraScale
  -- devices). To err on the side of caution, we list all parameters that are
  -- valid for the selected configuration (but not listing superfluous
  -- parameters that configure things that are irrelevant for the selected
  -- configuration). The list is ordered as the "User Parameters" section of the
  -- product guide.
  props width =
    [ property @Text "INTERFACE_TYPE"               "Native"
    , property @Text "Fifo_Implementation"          "Independent_Clocks_Block_RAM"
    , property @Int  "synchronization_stages"       2
    , property @Text "Performance_Options"          "Standard_FIFO"
    , property       "asymmetric_port_width"        False
    , property       "Input_Data_Width"             width
    , property @Int  "Input_Depth"                  (2 ^ depth)
    , property       "Output_Data_Width"            width
    , property @Int  "Output_Depth"                 (2 ^ depth)
    , property       "enable_low_latency"           False
    , property       "use_dout_register"            False
    , property       "Enable_ECC"                   False
    , property       "Use_Embedded_Registers"       False
    , property       "Reset_Pin"                    True
    , property       "Enable_Reset_Synchronization" False
    , property       "Enable_Safety_Circuit"        False
    , property @Int  "Full_Flags_Reset_Value"       0
    , property       "Use_Dout_Reset"               False
    , property       "Almost_Full_Flag"             False
    , property       "Almost_Empty_Flag"            False
    , property       "Write_Acknowledge_Flag"       False
    , property       "Overflow_Flag"                dcOverflow
    , property @Text "Overflow_Sense"               "Active_High"
    , property       "Valid_Flag"                   False
    , property       "Underflow_Flag"               dcUnderflow
    , property @Text "Underflow_Sense"              "Active_High"
    , property @Text "Programmable_Full_Type"       "No_Programmable_Full_Threshold"
    , property @Text "Programmable_Empty_Type"      "No_Programmable_Empty_Threshold"
    , property       "Write_Data_Count"             dcWriteDataCount
    , property       "Write_Data_Count_Width"       depth
    , property       "Read_Data_Count"              dcReadDataCount
    , property       "Read_Data_Count_Width"        depth
    , property       "Disable_Timing_Violations"    False
    ]

dcFifoTclBBTF _ bbCtx = error ("dcFifoTclBBTF, bad bbCtx: " <> show bbCtx)
