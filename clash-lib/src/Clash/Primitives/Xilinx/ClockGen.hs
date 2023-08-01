{-|
  Copyright   :  (C) 2023, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Blackbox template functions for
  Clash.Xilinx.ClockGen.{clockWizard,clockWizardDifferential}
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Primitives.Xilinx.ClockGen
  ( clockWizardTF
  , clockWizardTclTF
  , clockWizardDifferentialTF
  , clockWizardDifferentialTclTF
  ) where

import Control.Monad.State (State)
import Data.List.Infinite (Infinite(..), (...))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Prettyprinter.Interpolate (__di)
import Text.Show.Pretty (ppShow)

import Clash.Signal (periodToHz)

import Clash.Backend (Backend)
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
import Clash.Netlist.Util (stripVoid)
import qualified Clash.Primitives.DSL as DSL
import Data.Text.Prettyprint.Doc.Extra (Doc)

usedArguments :: [Int]
usedArguments = [knownDomIn, knownDomOut, clk, rst]
 where
  knownDomIn
    :< knownDomOut
    :< _knownDomLock
    :< clk
    :< rst
    :< _ = (0...)

clockWizardTF :: TemplateFunction
clockWizardTF =
  TemplateFunction usedArguments valid (clockWizardTemplate False)
 where
  valid = const True

clockWizardDifferentialTF :: TemplateFunction
clockWizardDifferentialTF =
  TemplateFunction usedArguments valid (clockWizardTemplate True)
 where
  valid = const True

clockWizardTemplate
  :: Backend s
  => Bool
  -> BlackBoxContext
  -> State s Doc
clockWizardTemplate isDifferential bbCtx
  |   knownDomIn
    : _knownDomOut
    : _knownDomLock
    : clk
    : rst
    : _ <- map fst (DSL.tInputs bbCtx)
  , [DSL.ety -> resultTy] <- DSL.tResults bbCtx
  , [compName] <- bbQsysIncName bbCtx
  = do
      clkWizInstName <- Id.makeBasic $ fromMaybe "clk_wiz" $ bbCtxName bbCtx
      DSL.declarationReturn bbCtx blockName $ do

        rstHigh <- DSL.unsafeToActiveHigh "reset" (DSL.ety knownDomIn) rst
        pllOut <- DSL.declare "pllOut" Bit
        locked <- DSL.declare "locked" Bit
        pllLock <- DSL.boolFromBit "pllLock" locked

        let compInps = compClkInps <> [ ("reset", Bit) ]
            compOuts =
              [ ("clk_out1", Bit)
              , ("locked", Bit)
              ]
            inps = clkInps clk <> [ ("reset", rstHigh) ]
            outs =
              [ ("clk_out1", pllOut)
              , ("locked", locked)
              ]

        DSL.compInBlock compName compInps compOuts
        DSL.instDecl Empty (Id.unsafeMake compName) clkWizInstName [] inps outs

        pure [DSL.constructProduct resultTy [pllOut, pllLock]]
  | otherwise
  = error $ ppShow bbCtx
 where
  blockName | isDifferential = "clockWizardDifferential"
            | otherwise      = "clockWizard"
  compClkInps | isDifferential = [ ("clk_in1_p", Bit)
                                 , ("clk_in1_n", Bit)
                                 ]
              | otherwise      = [ ("clk_in1", Bit) ]
  clkInps clk
    | isDifferential
    , DataCon (Product "Clash.Signal.Internal.DiffClock" _ clkTys) _ clkEs
      <- DSL.eex clk
    , [clkP@(Identifier _ Nothing), clkN@(Identifier _ Nothing)] <- clkEs
    , [clkPTy, clkNTy] <- clkTys
    = [ ("clk_in1_p", DSL.TExpr clkPTy clkP)
      , ("clk_in1_n", DSL.TExpr clkNTy clkN)
      ]
    | not isDifferential
    = [ ("clk_in1", clk) ]
    | otherwise
    = error $ ppShow bbCtx

clockWizardTclTF :: TemplateFunction
clockWizardTclTF =
  TemplateFunction usedArguments valid (clockWizardTclTemplate False)
 where
  valid = const True

clockWizardDifferentialTclTF :: TemplateFunction
clockWizardDifferentialTclTF =
  TemplateFunction usedArguments valid (clockWizardTclTemplate True)
 where
  valid = const True

clockWizardTclTemplate
  :: Backend s
  => Bool
  -> BlackBoxContext
  -> State s Doc
clockWizardTclTemplate isDifferential bbCtx
  |   (_,stripVoid -> (KnownDomain _ clkInPeriod _ _ _ _),_)
    : (_,stripVoid -> (KnownDomain _ clkOutPeriod _ _ _ _),_)
    : _knownDomLock
    : _ <- bbInputs bbCtx
  , [(Identifier _ Nothing,Product {})] <- bbResults bbCtx
  , [compName] <- bbQsysIncName bbCtx
  =
  let
    clkInFreq :: Double
    clkInFreq  = periodToHz (fromInteger clkInPeriod) / 1e6
    clkOutFreq :: Double
    clkOutFreq = periodToHz (fromInteger clkOutPeriod) / 1e6

    differentialPinString :: T.Text
    differentialPinString = if isDifferential
      then "Differential_clock_capable_pin"
      else "Single_ended_clock_capable_pin"

    bbText = [__di|
      namespace eval $tclIface {
        variable api 1
        variable scriptPurpose createIp
        variable ipName {#{compName}}

        proc createIp {ipName0 args} {
          create_ip \\
            -name clk_wiz \\
            -vendor xilinx.com \\
            -library ip \\
            -version 6.0 \\
            -module_name $ipName0 \\
            {*}$args

          set_property \\
            -dict [list \\
                        CONFIG.PRIM_SOURCE #{differentialPinString} \\
                        CONFIG.PRIM_IN_FREQ #{clkInFreq} \\
                        CONFIG.CLKOUT1_REQUESTED_OUT_FREQ #{clkOutFreq} \\
                  ] [get_ips $ipName0]
          return
        }
      }|]
   in pure bbText
  | otherwise
  = error ("clockWizardTclTemplate: bad bbContext: " <> show bbCtx)
