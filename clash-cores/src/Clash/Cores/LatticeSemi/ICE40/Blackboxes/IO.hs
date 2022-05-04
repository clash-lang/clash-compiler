{-|
  Copyright   :  (C) 2019, Foamspace corp
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  HDL generation functionality for LATTICE ICE IO primitives.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Cores.LatticeSemi.ICE40.Blackboxes.IO (sbioTF) where

import           Prelude

import           Control.Monad.State
import           Data.Monoid (Ap(getAp))
import           Data.Text.Prettyprint.Doc.Extra
import           GHC.Stack
  (HasCallStack, prettyCallStack, callStack)

import           Clash.Backend
import           Clash.Netlist.Ast.Type (HWType(..), PortDirection(..))
import qualified Clash.Netlist.Id as Id
import           Clash.Netlist.Types
import           Clash.Netlist.Util (instPort)

pinConfigLiteral
  :: HasCallStack
  => Bool
  -> ()
pinConfigLiteral True = ()
pinConfigLiteral False =
  error $
    "The first argument of 'Clash.Cores.LatticeSemi.IO.sbio', configuring " <>
    "SB_IO's pinType, must be statically known. The given argument either " <>
    "wasn't or Clash failed to deduce it was. To force Clash to calculate " <>
    "the argument at compile time, use Template Haskell. For example:\n\n" <>

    "    let pinType = $(lift (spiConfig PIN_INPUT PIN_OUTPUT_TRISTATE)) in\n" <>
    "    sbio pinType pkgPin latchInput ..\n\n" <>

    prettyCallStack callStack

-- | Generates HDL for SB_IO for the LATTICE ICE
sbioTF :: TemplateFunction
sbioTF = TemplateFunction used valid sbioTemplate
 where
  used = [1..8]
  valid = const True

sbioTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
sbioTemplate bbCtx = do
  let compName = Id.unsafeMake "SB_IO"  -- Hardcoded for now

  sbio <- Id.makeBasic "sbio"
  sbio_inst <- Id.makeBasic "sbio_inst"

  dIn0 <- Id.makeBasic "dIn0"
  dIn1 <- Id.makeBasic "dIn1"

  let
    resultTuple =
      DataCon
        resTy
        (DC (resTy,0))
        [ Identifier dIn0 Nothing
        , Identifier dIn1 Nothing
        ]

  getAp $ blockDecl sbio $
    [ NetDecl Nothing dIn0 Bit
    , NetDecl Nothing dIn1 Bit
    , InstDecl Comp Nothing [] compName sbio_inst
      [ (instPort "PIN_TYPE", BitVector 6, pinConfig)
      ]
      (NamedPortMap [
        -- NOTE: Direction is set to 'In', but will be rendered as inout due to
        -- its the type packackagePinTy
        (instPort "PACKAGE_PIN", In, packagePinTy, packagePin)
      , (instPort "LATCH_INPUT_VALUE", In, Bit, latchInput)
      -- TODO: If clock is constantly enabled, docs recommend  not connecting
      -- TODO: CLOCK_ENABLE at all.
      , (instPort "CLOCK_ENABLE", In, Bool, en)
      , (instPort "INPUT_CLK", In, clkTy, clk)
      , (instPort "OUTPUT_CLK", In, clkTy, clk)
      , (instPort "OUTPUT_ENABLE", In, Bool, outputEnable)
      , (instPort "D_OUT_0", In, Bit, dOut0)
      , (instPort "D_OUT_1", In, Bit, dOut1)
      , (instPort "D_IN_0", Out, Bit, Identifier dIn0 Nothing)
      , (instPort "D_IN_1", Out, Bit, Identifier dIn1 Nothing)
      ])
    , Assignment result resultTuple
    ]
 where
  [ _HasCallStack
   , (clk, clkTy, _)
   , (en, _enTy, _)
   , (pinConfig, _pinTy, pinConfigLiteral -> ())
   , (packagePin, packagePinTy, _)
   , (latchInput, Bit, _)
   , (dOut0, Bit, _)
   , (dOut1, Bit, _)
   , (outputEnable, Bool, _)
   ] = bbInputs bbCtx

  [(Identifier result Nothing,resTy)] = bbResults bbCtx
