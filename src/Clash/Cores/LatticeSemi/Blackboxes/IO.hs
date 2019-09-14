{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Cores.LatticeSemi.Blackboxes.IO (sbioTF) where

import           Prelude

import           Control.Monad.State
import           Data.Semigroup.Monad               (getMon)
import           Data.Text.Prettyprint.Doc.Extra
import qualified Data.Text as TextS
import           GHC.Stack
  (HasCallStack, prettyCallStack, callStack)

import           Clash.Backend
import           Clash.Netlist.BlackBox.Util
import           Clash.Netlist.Id
import           Clash.Netlist.Types

match
  :: (HasCallStack, Show a, Eq a)
  => a
  -> a
  -> ()
match a0 a1 =
  if a0 == a1 then
    ()
  else
    error $ "Couldn't match " <> show a0 <> " with " <> show a1 <> " at "
         <> prettyCallStack callStack


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
  let mkId = mkUniqueIdentifier Basic
      compName = "SB_IO"  -- Hardcoded for now

  sbio <- mkId "sbio"
  sbio_inst <- mkId "sbio_inst"

  dIn0 <- mkId "dIn0"
  dIn1 <- mkId "dIn1"

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
    , InstDecl Comp Nothing compName sbio_inst
      [ (Identifier "PIN_TYPE" Nothing, BitVector 6, pinConfig)
      ]
      [ -- NOTE: Direction is set to 'In', but will be rendered as inout due to
        -- its the type packackagePinTy
        (Identifier "PACKAGE_PIN" Nothing, In, packagePinTy, packagePin)
      , (Identifier "LATCH_INPUT_VALUE" Nothing, In, Bit, latchInput)
      -- TODO: If clock is constantly enabled, docs recommend  not connecting
      -- TODO: CLOCK_ENABLE at all.
      , (Identifier "CLOCK_ENABLE" Nothing, In, Bool, en)
      , (Identifier "INPUT_CLK" Nothing, In, clkTy, clk)
      , (Identifier "OUTPUT_CLK" Nothing, In, clkTy, clk)
      , (Identifier "OUTPUT_ENABLE" Nothing, In, Bool, outputEnable)
      , (Identifier "D_OUT_0" Nothing, In, Bit, dOut0)
      , (Identifier "D_OUT_1" Nothing, In, Bit, dOut1)
      , (Identifier "D_IN_0" Nothing, Out, Bit, Identifier dIn0 Nothing)
      , (Identifier "D_IN_1" Nothing, Out, Bit, Identifier dIn1 Nothing)
      ]
    , Assignment result resultTuple
    ]
 where
  [ _HasCallStack
   , (clk, clkTy, _)
   , (en, enTy, _)
   , (pinConfig, pinTy, match True -> ())
   , (packagePin, packagePinTy, _)
   , (latchInput, Bit, _)
   , (dOut0, Bit, _)
   , (dOut1, Bit, _)
   , (outputEnable, Bool, _)
   ] = bbInputs bbCtx

  (Identifier result Nothing,resTy@(Product _ _ (tail -> tys))) = bbResult bbCtx
