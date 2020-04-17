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
import           Data.Semigroup.Monad               (getMon)
import           Data.Text.Prettyprint.Doc.Extra
import           GHC.Stack
  (HasCallStack, prettyCallStack, callStack)

import           Clash.Backend
import qualified Clash.Netlist.Id as Id
import           Clash.Netlist.Types

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

  let u = Id.unsafeMake

  let
    resultTuple =
      DataCon
        resTy
        (DC (resTy,0))
        [ Identifier dIn0 Nothing
        , Identifier dIn1 Nothing
        ]

  getMon $ blockDecl sbio $
    [ NetDecl Nothing dIn0 Bit
    , NetDecl Nothing dIn1 Bit
    , InstDecl Comp Nothing [] compName sbio_inst
      [ (Identifier (u "PIN_TYPE") Nothing, BitVector 6, pinConfig)
      ]
      [ -- NOTE: Direction is set to 'In', but will be rendered as inout due to
        -- its the type packackagePinTy
        (Identifier (u "PACKAGE_PIN") Nothing, In, packagePinTy, packagePin)
      , (Identifier (u "LATCH_INPUT_VALUE") Nothing, In, Bit, latchInput)
      -- TODO: If clock is constantly enabled, docs recommend  not connecting
      -- TODO: CLOCK_ENABLE at all.
      , (Identifier (u "CLOCK_ENABLE") Nothing, In, Bool, en)
      , (Identifier (u "INPUT_CLK") Nothing, In, clkTy, clk)
      , (Identifier (u "OUTPUT_CLK") Nothing, In, clkTy, clk)
      , (Identifier (u "OUTPUT_ENABLE") Nothing, In, Bool, outputEnable)
      , (Identifier (u "D_OUT_0") Nothing, In, Bit, dOut0)
      , (Identifier (u "D_OUT_1") Nothing, In, Bit, dOut1)
      , (Identifier (u "D_IN_0") Nothing, Out, Bit, Identifier dIn0 Nothing)
      , (Identifier (u "D_IN_1") Nothing, Out, Bit, Identifier dIn1 Nothing)
      ]
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
