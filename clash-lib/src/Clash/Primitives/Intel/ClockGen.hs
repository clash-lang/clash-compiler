{-|
  Copyright   :  (C) 2018     , Google Inc.,
                     2021-2023, QBayLogic B.V.,
                     2022     , Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Blackbox template functions for Clash.Intel.ClockGen.{alteraPll,altpll}
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Primitives.Intel.ClockGen where

import Clash.Backend
import Clash.Netlist.BlackBox.Util
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
import Clash.Netlist.Util
import qualified Clash.Primitives.DSL as DSL
import Clash.Signal (periodToHz)
import Data.Text.Extra (showt)

import Control.Monad.State
import Data.List.Infinite (Infinite(..), (...))
import Data.Text.Prettyprint.Doc.Extra
import Text.Show.Pretty (ppShow)

import qualified Data.String.Interpolate as I
import qualified Data.Text as TextS
import qualified Prettyprinter.Interpolate as I

altpllTF :: TemplateFunction
altpllTF = TemplateFunction used valid altpllTemplate
 where
  knownDomIn
    :< _knownDomOut
    :< nm
    :< clk
    :< rst
    :< _ = (0...)
  used = [ knownDomIn, nm, clk, rst ]
  valid bbCtx
    | (nm0,_,_) <- bbInputs bbCtx !! nm
    , Just _ <- exprToString nm0
    , [(_,Product {})] <- bbResults bbCtx
    = True
  valid _ = False

altpllQsysTF :: TemplateFunction
altpllQsysTF = TemplateFunction used valid altpllQsysTemplate
 where
  knownDomIn
    :< knownDomOut
    :< _name
    :< _clk
    :< _rst
    :< _ = (0...)
  used = [ knownDomIn, knownDomOut ]
  valid = const True

alteraPllTF :: TemplateFunction
alteraPllTF = TemplateFunction used valid alteraPllTemplate
 where
  _clocksClass
    :< knownDomIn
    :< _clocksCxt
    :< nm
    :< clk
    :< rst
    :< _ = (0...)
  used = [ knownDomIn, nm, clk, rst ]
  valid bbCtx
    | (nm0,_,_) <- bbInputs bbCtx !! nm
    , Just _ <- exprToString nm0
    , [(_,Product {})] <- bbResults bbCtx
    = True
  valid _ = False

alteraPllQsysTF :: TemplateFunction
alteraPllQsysTF = TemplateFunction used valid alteraPllQsysTemplate
 where
  _clocksClass
    :< knownDomIn
    :< clocksCxt
    :< _name
    :< _clk
    :< _rst
    :< _ = (0...)
  used = [ knownDomIn, clocksCxt ]
  valid = const True

alteraPllTemplate
  :: forall s
   . Backend s
  => BlackBoxContext
  -> State s Doc
alteraPllTemplate bbCtx
  | [ _clocksClass
    , knownDomIn
    , _clocksCxt
    , name0
    , clk
    , rst
    ] <- map fst (DSL.tInputs bbCtx)
  , Just name1 <- DSL.getStr name0
  , [DSL.ety -> resultTy] <- DSL.tResults bbCtx
  , Product _ _ (init -> pllOutTys) <- resultTy
  , [compName] <- bbQsysIncName bbCtx
  = do
      instName <- Id.makeBasic $ TextS.pack name1

      -- TODO: unsafeMake is dubious here: I don't think we take names in
      -- TODO: bbQsysIncName into account when generating fresh ids
      let compNameId = Id.unsafeMake compName

      DSL.declarationReturn bbCtx "altera_pll_block" $ do

        rstHigh <- DSL.unsafeToActiveHigh "reset" (DSL.ety knownDomIn) rst
        pllOuts <- DSL.declareN "pllOut" pllOutTys
        locked <- DSL.declare "locked" Bit
        pllLock <- DSL.boolFromBit "pllLock" locked

        let
          pllOutNames =
            map (\n -> "outclk_" <> showt n) [(0::Int) .. length pllOutTys - 1]
          compInps =
            [ ("refclk", DSL.ety clk)
            , ("rst", DSL.ety rstHigh)
            ]
          compOuts = zip pllOutNames pllOutTys  <> [("locked", Bit)]
          inps =
            [ ("refclk", clk)
            , ("rst", rstHigh)
            ]
          outs = zip pllOutNames pllOuts <> [("locked", locked)]

        DSL.compInBlock compName compInps compOuts
        DSL.instDecl Empty compNameId instName [] inps outs

        pure [DSL.constructProduct resultTy (pllOuts <> [pllLock])]
  | otherwise
  = error $ ppShow bbCtx

altpllTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
altpllTemplate bbCtx
  | [ knownDomIn
    , _knownDomOut
    , name0
    , clk
    , rst
    ] <- map fst (DSL.tInputs bbCtx)
  , Just name1 <- DSL.getStr name0
  , [DSL.ety -> resultTy] <- DSL.tResults bbCtx
  , Product _ _ (pllOutTy:_) <- resultTy
  , [compName] <- bbQsysIncName bbCtx
  = do
      instName <- Id.makeBasic $ TextS.pack name1

      -- TODO: unsafeMake is dubious here: I don't think we take names in
      -- TODO: bbQsysIncName into account when generating fresh ids
      let compNameId = Id.unsafeMake compName

      DSL.declarationReturn bbCtx "altpll_block" $ do

        rstHigh <- DSL.unsafeToActiveHigh "reset" (DSL.ety knownDomIn) rst
        pllOut <- DSL.declare "pllOut" pllOutTy
        locked <- DSL.declare "locked" Bit
        pllLock <- DSL.boolFromBit "pllLock" locked

        let
          compInps =
            [ ("clk", DSL.ety clk)
            , ("areset", DSL.ety rstHigh)
            ]
          compOuts =
            [ ("c0", pllOutTy)
            , ("locked", Bit)
            ]
          inps =
            [ ("clk", clk)
            , ("areset", rstHigh)
            ]
          outs =
            [ ("c0", pllOut)
            , ("locked", locked)
            ]

        DSL.compInBlock compName compInps compOuts
        DSL.instDecl Empty compNameId instName [] inps outs

        pure [DSL.constructProduct resultTy [pllOut, pllLock]]
  | otherwise
  = error $ ppShow bbCtx

altpllQsysTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
altpllQsysTemplate bbCtx
  |   (_,stripVoid -> kdIn,_)
    : (_,stripVoid -> kdOut,_)
    : _ <- bbInputs bbCtx
  , KnownDomain _ clkInPeriod _ _ _ _ <- kdIn
  , KnownDomain _ clkOutPeriod _ _ _ _ <- kdOut
  = let
      clkOutFreq :: Double
      clkOutFreq = periodToHz (fromIntegral clkOutPeriod) / 1e6
      clklcm = lcm clkInPeriod clkOutPeriod
      clkmult = clklcm `quot` clkOutPeriod
      clkdiv = clklcm `quot` clkInPeriod
      -- Note [QSys file templates]
      -- This QSys file template was derived from a "full" QSys system with a single
      -- "altpll" IP. Module parameters were then stripped on a trial-and-error
      -- basis to get a template that has the minimal number of parameters, but
      -- still has the desired, working, configuration.
      bbText = [I.__di|
        <?xml version="1.0" encoding="UTF-8"?>
        <system name="$${FILENAME}">
          <module
            name="altpll0"
            kind="altpll"
            enabled="1"
            autoexport="1">
          <parameter name="AVALON_USE_SEPARATE_SYSCLK" value="NO" />
          <parameter name="BANDWIDTH" value="" />
          <parameter name="BANDWIDTH_TYPE" value="AUTO" />
          <parameter name="CLK0_DIVIDE_BY" value="#{clkdiv}" />
          <parameter name="CLK0_DUTY_CYCLE" value="50" />
          <parameter name="CLK0_MULTIPLY_BY" value="#{clkmult}" />
          <parameter name="CLK0_PHASE_SHIFT" value="0" />
          <parameter name="COMPENSATE_CLOCK" value="CLK0" />
          <parameter name="INCLK0_INPUT_FREQUENCY" value="#{clkInPeriod}" />
          <parameter name="OPERATION_MODE" value="NORMAL" />
          <parameter name="PLL_TYPE" value="AUTO" />
          <parameter name="PORT_ARESET" value="PORT_USED" />
          <parameter name="PORT_INCLK0" value="PORT_USED" />
          <parameter name="PORT_LOCKED" value="PORT_USED" />
          <parameter name="PORT_clk0" value="PORT_USED" />
          <parameter name="HIDDEN_IS_FIRST_EDIT" value="0" />
          <parameter name="HIDDEN_CONSTANTS">
            CT\#PORT_clk0 PORT_USED
            CT\#CLK0_MULTIPLY_BY #{clkmult}
            CT\#WIDTH_CLOCK 5
            CT\#LPM_TYPE altpll
            CT\#PLL_TYPE AUTO
            CT\#CLK0_PHASE_SHIFT 0
            CT\#OPERATION_MODE NORMAL
            CT\#COMPENSATE_CLOCK CLK0
            CT\#INCLK0_INPUT_FREQUENCY #{clkInPeriod}
            CT\#PORT_INCLK0 PORT_USED
            CT\#PORT_ARESET PORT_USED
            CT\#BANDWIDTH_TYPE AUTO
            CT\#CLK0_DUTY_CYCLE 50
            CT\#CLK0_DIVIDE_BY #{clkdiv}
            CT\#PORT_LOCKED PORT_USED</parameter>
          <parameter name="HIDDEN_IF_PORTS">
            IF\#phasecounterselect {input 4}
            IF\#locked {output 0}
            IF\#reset {input 0}
            IF\#clk {input 0}
            IF\#phaseupdown {input 0}
            IF\#scandone {output 0}
            IF\#readdata {output 32}
            IF\#write {input 0}
            IF\#scanclk {input 0}
            IF\#phasedone {output 0}
            IF\#address {input 2}
            IF\#c0 {output 0}
            IF\#writedata {input 32}
            IF\#read {input 0}
            IF\#areset {input 0}
            IF\#scanclkena {input 0}
            IF\#scandataout {output 0}
            IF\#configupdate {input 0}
            IF\#phasestep {input 0}
            IF\#scandata {input 0}</parameter>
          <parameter name="HIDDEN_MF_PORTS">
            MF\#areset 1
            MF\#clk 1
            MF\#locked 1
            MF\#inclk 1</parameter>
          <parameter name="HIDDEN_PRIVATES">
            PT\#PHASE_SHIFT0 0.00000000
            PT\#DIV_FACTOR0 #{clkdiv}
            PT\#EFF_OUTPUT_FREQ_VALUE0 #{clkOutFreq}
            PT\#MULT_FACTOR0 #{clkmult}
            PT\#DUTY_CYCLE0 50.00000000</parameter>
          </module>
        </system>
        |]
      in
        pure bbText
  | otherwise
  = error ("altpllQsysTemplate: bad bbContext: " <> show bbCtx)

alteraPllQsysTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
alteraPllQsysTemplate bbCtx
  |   _clocksClass
    : (_,stripVoid -> kdIn,_)
    : (_,stripVoid -> Product _ _ (init -> kdOuts),_)
    : _ <- bbInputs bbCtx
  = let
      cklFreq (KnownDomain _ p _ _ _ _)
        = periodToHz (fromIntegral p) / 1e6 :: Double
      cklFreq _ = error "internal error: not a KnownDomain"

      clkOuts = TextS.intercalate "\n"
        [[I.i|  <parameter name="gui_output_clock_frequency#{n}" value="#{f}"/>|]
        | (n,f) <- zip [(0 :: Word)..] (map cklFreq kdOuts)
        ]

      -- See Note [QSys file templates] on how this qsys template was derived.
      bbText = [I.__di|
        <?xml version="1.0" encoding="UTF-8"?>
        <system name="$${FILENAME}">
        <module
            name="pll_0"
            kind="altera_pll"
            enabled="1"
            autoexport="1">
          <parameter name="gui_feedback_clock" value="Global Clock" />
          <parameter name="gui_number_of_clocks" value="#{length kdOuts}" />
          <parameter name="gui_operation_mode" value="direct" />
        #{clkOuts}
          <parameter name="gui_pll_mode" value="Integer-N PLL" />
          <parameter name="gui_reference_clock_frequency" value="#{cklFreq kdIn}" />
          <parameter name="gui_use_locked" value="true" />
        </module>
        </system>
        |]
    in
      pure bbText
  | otherwise
  = error ("alteraPllQsysTemplate: bad bbContext: " <> show bbCtx)
