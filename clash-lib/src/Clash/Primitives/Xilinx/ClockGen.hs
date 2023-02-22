{-|
  Copyright   :  (C) 2023, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Blackbox template functions for
  Clash.Xilinx.ClockGen.{clockWizard,clockWizardDifferential}
-}

{-# LANGUAGE QuasiQuotes #-}

module Clash.Primitives.Xilinx.ClockGen where

import Control.Monad.State (State)
import qualified Data.String.Interpolate as I

import Clash.Signal (periodToHz)

import Clash.Backend (Backend)
import Clash.Netlist.BlackBox.Util (exprToString)
import Clash.Netlist.Types
import Clash.Netlist.Util (stripVoid)
import Data.Text.Prettyprint.Doc.Extra (Doc)


clockWizardTclTF :: TemplateFunction
clockWizardTclTF =
  TemplateFunction used valid (clockWizardTclTemplate False)
 where
  knownDomIn = 0
  knownDomOut = 1
  name = 2
  -- clk = 3
  -- rst = 4
  used = [knownDomIn, knownDomOut, name]
  valid = const True

clockWizardDifferentialTclTF :: TemplateFunction
clockWizardDifferentialTclTF =
  TemplateFunction used valid (clockWizardTclTemplate True)
 where
  knownDomIn = 0
  knownDomOut = 1
  name = 2
  -- clkN = 3
  -- clkP = 4
  -- rst = 5
  used = [knownDomIn, knownDomOut, name]
  valid = const True


clockWizardTclTemplate
  :: Backend s
  => Bool
  -> BlackBoxContext
  -> State s Doc
clockWizardTclTemplate isDifferential bbCtx
  | (_,stripVoid -> (KnownDomain _ clkInPeriod _ _ _ _),_) <- bbInputs bbCtx !! 0
  , (_,stripVoid -> (KnownDomain _ clkOutPeriod _ _ _ _),_) <- bbInputs bbCtx !! 1
  , (nm,_,_) <- bbInputs bbCtx !! 2
  , [(Identifier _ Nothing,Product {})] <- bbResults bbCtx
  , Just compName <- exprToString nm
  =
  let
    clkInFreq :: Double
    clkInFreq  = periodToHz (fromInteger clkInPeriod) / 1e6
    clkOutFreq :: Double
    clkOutFreq = periodToHz (fromInteger clkOutPeriod) / 1e6

    differentialPinString = if isDifferential
      then "Differential_clock_capable_pin"
      else "Single_ended_clock_capable_pin"

    bbText = [I.__i|
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
