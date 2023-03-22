{-|
  Copyright   :  (C) 2022 Google Inc
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Blackbox implementation for primitives in "Clash.Cores.Xilinx.BlockRam"
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Cores.Xilinx.BlockRam.BlackBoxes where

import Prelude

import Control.Monad.State (State)
import Data.List.Infinite (Infinite(..), (...))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)

import Clash.Backend (Backend)
import Clash.Cores.Xilinx.Internal
  (defIpConfig, IpConfig(properties), property, renderTcl, TclPurpose(..))
import Clash.Netlist.Types (TemplateFunction(..), BlackBoxContext)

import qualified Clash.Primitives.DSL as DSL
import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N

-- | Arguments used by this black box
used :: [Int]
used =
  [ knownNatNAddrs, nBytes
  , aClk, aEna, aAddr, aByteEna, aDat
  , bClk, bEna, bAddr, bByteEna, bDat
  ]
 where
  _hasCallStack
    :< _risingEdgeConstraintA
    :< _risingEdgeConstraintB
    :< knownNatNAddrs
    :< _knownDomainA
    :< _knownDomainB
    :< _nfDataX
    :< _bitpack
    :< nBytes
    :< _multipleOf8Constraint
    :< aClk :< aEna :< aAddr :< aByteEna :< aDat
    :< bClk :< bEna :< bAddr :< bByteEna :< bDat
    :< _ = ((0 :: Int)...)

-- | Instantiate IP generated with 'tdpbramTclTF'
tdpbramTF :: HasCallStack => TemplateFunction
tdpbramTF = TemplateFunction used (const True) tdpbramBBTF

tdpbramBBTF :: Backend s => BlackBoxContext -> State s Doc
tdpbramBBTF bbCtx
  | [ _hasCallStack
    , _risingEdgeConstraintA
    , _risingEdgeConstraintB
    , _knownNatNAddrs
    , _knownDomainA
    , _knownDomainB
    , _nfDataX
    , _bitpack
    , (fmap fromIntegral . DSL.tExprToInteger -> Just nBytes)
    , _multipleOf8Constraint
    , aClk, aEna, aAddr, aByteEna, aDat
    , bClk, bEna, bAddr, bByteEna, bDat
    ] <- map fst (DSL.tInputs bbCtx)
  , [tResult] <- map DSL.ety (DSL.tResults bbCtx)
  , [tdpbramName] <- N.bbQsysIncName bbCtx
  = do
  tdpbramInstName <- Id.makeBasic "tdpbram_inst"

  let
    dataTy = DSL.ety aDat
    dataBvTy = N.BitVector (DSL.tySize (DSL.ety aDat))
    compInps =
      [ ("clka", N.Bit)
      , ("ena", N.Bit)
      , ("wea", N.BitVector nBytes)
      , ("addra", N.BitVector (DSL.tySize (DSL.ety aAddr)))
      , ("dina", dataBvTy)

      , ("clkb", N.Bit)
      , ("enb", N.Bit)
      , ("web", N.BitVector nBytes)
      , ("addrb", N.BitVector (DSL.tySize (DSL.ety bAddr)))
      , ("dinb", dataBvTy)
      ]
    compOuts =
      [ ("douta", dataBvTy)
      , ("doutb", dataBvTy)
      ]

  DSL.declarationReturn bbCtx "dcfifo_inst_block" $ do

    DSL.compInBlock tdpbramName compInps compOuts

    doutaBv <- DSL.declare "douta_bv" dataBvTy
    doutbBv <- DSL.declare "doutb_bv" dataBvTy

    douta <- DSL.fromBV "douta" dataTy doutaBv
    doutb <- DSL.fromBV "doutb" dataTy doutbBv

    dinaBv <- DSL.toBV "dina_bv" aDat
    dinbBv <- DSL.toBV "dinb_bv" bDat

    aAddrBv <- DSL.toBV "addra_bv" aAddr
    bAddrBv <- DSL.toBV "addrb_bv" bAddr

    aEnaBit <- DSL.boolToBit "ena_bit" aEna
    bEnaBit <- DSL.boolToBit "enb_bit" bEna

    let
      inps =
        [ ("clka",  aClk)
        , ("ena",   aEnaBit)
        , ("wea",   aByteEna)
        , ("addra", aAddrBv)
        , ("dina",  dinaBv)

        , ("clkb",  bClk)
        , ("enb",   bEnaBit)
        , ("web",   bByteEna)
        , ("addrb", bAddrBv)
        , ("dinb",  dinbBv)
        ]
      outs =
        [ ("douta", doutaBv)
        , ("doutb", doutbBv)
        ]

    DSL.instDecl N.Empty (Id.unsafeMake tdpbramName) tdpbramInstName [] inps outs

    pure [DSL.constructProduct tResult [douta, doutb]]

tdpbramBBTF bbCtx = error ("tdpbramBBTF, bad bbCtx: " <> show bbCtx)

-- | Renders Tcl file conforming to the /Clash\<->Tcl API/, creating the Xilinx
-- IP with @create_ip@
tdpbramTclTF :: HasCallStack => TemplateFunction
tdpbramTclTF = TemplateFunction used (const True) tdpbramTclBBTF

tdpbramTclBBTF :: Backend s => BlackBoxContext -> State s Doc
tdpbramTclBBTF bbCtx
  | [tdpbramName] <- N.bbQsysIncName bbCtx
  , [ _hasCallStack
    , _risingEdgeConstraintA
    , _risingEdgeConstraintB
    , DSL.tExprToInteger -> Just depth
    , _knownDomainA
    , _knownDomainB
    , _nfDataX
    , _bitpack
    , DSL.tExprToInteger -> Just width
    , _multipleOf8Constraint
    , _aClk, _aEna, _aAddr, _aByteEna, _aDat
    , _bClk, _bEna, _bAddr, _bByteEna, _bDat
    ] <- map fst (DSL.tInputs bbCtx)
  = pure (renderTcl [IpConfigPurpose $ ipConfig depth (8*width) tdpbramName])
  where
  ipConfig depth width nm =
    (defIpConfig "blk_mem_gen" "8.4" nm){properties = props depth width}

  props depth width =
    [ property @Text "Memory_Type"                                "True_Dual_Port_RAM"
    , property       "Use_Byte_Write_Enable"                      True
    , property @Int  "Byte_Size"                                  8
    , property       "Write_Depth_A"                              depth

    , property @Text "Enable_A"                                   "Use_ENA_Pin"
    , property       "Register_PortA_Output_of_Memory_Primitives" False
    , property       "Write_Width_A"                              width
    , property       "Read_Width_A"                               width

    , property @Text "Enable_B"                                   "Use_ENB_Pin"
    , property       "Register_PortB_Output_of_Memory_Primitives" False
    , property       "Write_Width_B"                              width
    , property       "Read_Width_B"                               width
    ]

tdpbramTclBBTF bbCtx = error ("tdpbramTclBBTF, bad bbCtx: " <> show bbCtx)
