{-|
  Copyright   :  (C) 2019, Foamspace corp
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  HDL generation functionality for LATTICE ICE IO primitives.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Cores.LatticeSemi.ICE40.Blackboxes.IO
  ( sbioTF
  , sbioDDRTF
  ) where

import           Prelude

import           Control.Monad.State
import           Data.Semigroup.Monad               (getMon)
import           Data.Text.Prettyprint.Doc.Extra
import           GHC.Stack
  (HasCallStack, prettyCallStack, callStack)

import           Clash.Backend
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

sbioTF :: TemplateFunction
sbioTF = TemplateFunction used valid sbioTemplate
 where
  used = [2..8]
  valid = const True

sbioTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
sbioTemplate bbCtx = do
  let compName = Id.unsafeMake "SB_IO"

  sbio <- Id.makeBasic "sbio"
  sbio_inst <- Id.makeBasic "sbio_inst"

  getMon $ blockDecl sbio $
    [ InstDecl Comp Nothing [] compName sbio_inst
      [ (instPort "PIN_TYPE", BitVector 6, pinConfig)
      ]
      (NamedPortMap
        [ (instPort "PACKAGE_PIN", In, packagePinTy, packagePin)
        , (instPort "LATCH_INPUT_VALUE", In, Bit, latchInput)
        , (instPort "CLOCK_ENABLE", In, Bool, en)
        , (instPort "INPUT_CLK", In, clkTy, clk)
        , (instPort "OUTPUT_CLK", In, clkTy, clk)
        , (instPort "OUTPUT_ENABLE", In, Bool, outputEnable)
        , (instPort "D_OUT_0", In, Bit, dOut0)
        , (instPort "D_OUT_1", In, Bit, Literal (Just (Bit, 1)) (BitLit Z))
        , (instPort "D_IN_0", Out, Bit, resultId)
        ])
    ]
 where
  [ _HasCallStack
   , _KnownDomain
   , (clk, clkTy, _)
   , (en, _, _)
   , (pinConfig, _, pinConfigLiteral -> ())
   , (packagePin, packagePinTy, _)
   , (latchInput, Bit, _)
   , (dOut0, Bit, _)
   , (outputEnable, Bool, _)
   ] = bbInputs bbCtx

  [(resultId, _)] = bbResults bbCtx

-- | Generates HDL for SB_IO for the LATTICE ICE
sbioDDRTF :: TemplateFunction
sbioDDRTF = TemplateFunction used valid sbioDDRTemplate
 where
  used = [4..11]
  valid = const True

sbioDDRTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
sbioDDRTemplate bbCtx = do
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

  getMon $ blockDecl sbio $
    [ NetDecl Nothing dIn0 Bit
    , NetDecl Nothing dIn1 Bit
    , InstDecl Comp Nothing [] compName sbio_inst
      [ (instPort "PIN_TYPE", BitVector 6, pinConfig)
      ]
      (NamedPortMap [
        -- NOTE: Direction is set to 'In', but will be rendered as inout due to
        -- its the type packackagePinTy
        (instPort "PACKAGE_PIN", In, packagePinTy, packagePin)
      , (instPort "LATCH_INPUT_VALUE", In, Bit, Literal (Just (Bit, 1)) (BitLit Z))
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
   , _KnownDomain
   , _SlowConfiguration
   , _FastConfiguration
   , (clk, clkTy, _)
   , (en, _enTy, _)
   , (pinConfig, _pinTy, pinConfigLiteral -> ())
   , (packagePin, packagePinTy, _)
   , (dOut0, Bit, _)
   , (dOut1, Bit, _)
   , (outputEnable, Bool, _)
   ] = bbInputs bbCtx

  [(Identifier result Nothing,resTy)] = bbResults bbCtx
