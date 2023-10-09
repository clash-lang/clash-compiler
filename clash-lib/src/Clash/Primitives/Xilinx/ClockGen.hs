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
import Data.String.Interpolate (i)
import qualified Data.Text as T
import Prettyprinter.Interpolate (__di)
import Text.Show.Pretty (ppShow)

import Clash.Signal (periodToHz)

import Clash.Backend (Backend)
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
import Clash.Netlist.Util (stripVoid)
import qualified Clash.Primitives.DSL as DSL
import Data.Text.Extra (showt)
import Data.Text.Prettyprint.Doc.Extra (Doc)

usedArguments :: [Int]
usedArguments = [knownDomIn, clocksCxt, clk, rst]
 where
  knownDomIn
    :< _clocksClass
    :< clocksCxt
    :< _numOutClocks
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
  | [ _knownDomIn
    , _clocksClass
    , _clocksCxt
    , _numOutClocks
    , clk
    , rst
    ] <- map fst (DSL.tInputs bbCtx)
  , [DSL.ety -> resultTy] <- DSL.tResults bbCtx
  , Product _ _ (init -> pllOutTys) <- resultTy
  , [compName] <- bbQsysIncName bbCtx
  = do
      clkWizInstName <- Id.makeBasic $ fromMaybe "clk_wiz" $ bbCtxName bbCtx
      DSL.declarationReturn bbCtx blockName $ do

        rstHigh <- DSL.unsafeToActiveHigh "reset" rst
        pllOuts <- DSL.declareN "pllOut" pllOutTys
        locked <- DSL.declare "locked" Bit
        pllLock <- DSL.boolFromBit "pllLock" locked

        let pllOutNames =
              map (\n -> "clk_out" <> showt n) [1 .. length pllOutTys]
            compInps = compClkInps <> [ ("reset", Bit) ]
            compOuts = zip pllOutNames pllOutTys <> [("locked", Bit)]
            inps = clkInps clk <> [ ("reset", rstHigh) ]
            outs = zip pllOutNames pllOuts <> [("locked", locked)]

        DSL.compInBlock compName compInps compOuts
        DSL.instDecl Empty (Id.unsafeMake compName) clkWizInstName [] inps outs

        pure [DSL.constructProduct resultTy (pllOuts <> [pllLock])]
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
  |   (_,stripVoid -> kdIn,_)
    : _clocksClass
    : (_,stripVoid -> Product _ _ (init -> kdOuts),_)
    : _ <- bbInputs bbCtx
  , [compName] <- bbQsysIncName bbCtx
  = let
    clkFreq (KnownDomain _ p _ _ _ _) =
      periodToHz (fromInteger p) / 1e6 :: Double
    clkFreq _ =
      error $ "Internal error: not a KnownDomain\n" <> ppShow bbCtx

    clkInFreq = clkFreq kdIn
    clkOutFreqs = map clkFreq kdOuts

    clkOutProps = concat
      [ [ [i|CONFIG.CLKOUT#{n}_USED true \\|]
        , [i|CONFIG.CLKOUT#{n}_REQUESTED_OUT_FREQ #{clkOutFreq} \\|]
        ]
      | (clkOutFreq, n) <- zip clkOutFreqs [(1::Word)..]
      ]

    differentialPinString :: T.Text
    differentialPinString = if isDifferential
      then "Differential_clock_capable_pin"
      else "Single_ended_clock_capable_pin"

    propIndent = T.replicate 18 " "
    props = T.intercalate "\n"  . map (propIndent <>) $
      [ [i|CONFIG.PRIM_SOURCE #{differentialPinString} \\|]
      , [i|CONFIG.PRIM_IN_FREQ #{clkInFreq} \\|]
      ] <> clkOutProps

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
      #{props}
                  ] [get_ips $ipName0]
          return
        }
      }|]
    in pure bbText
  | otherwise
  = error ("clockWizardTclTemplate: bad bbContext: " <> show bbCtx)
