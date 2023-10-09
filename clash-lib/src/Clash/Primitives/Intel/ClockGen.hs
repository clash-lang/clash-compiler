{-|
  Copyright   :  (C) 2018     , Google Inc.,
                     2021-2023, QBayLogic B.V.,
                     2022     , Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Blackbox template functions for Clash.Intel.ClockGen
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Primitives.Intel.ClockGen where

import Control.Monad.State
import Data.List (zip4)
import Data.List.Infinite (Infinite(..), (...))
import Data.Maybe (fromMaybe)
import Data.Text.Prettyprint.Doc.Extra
import Text.Show.Pretty (ppShow)

import Clash.Backend
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
import Clash.Netlist.Util
import qualified Clash.Primitives.DSL as DSL
import Clash.Signal (periodToHz)
import Data.Text.Extra (showt)

import qualified Data.String.Interpolate as I
import qualified Data.Text as TextS
import qualified Prettyprinter.Interpolate as I

data Variant = Altpll | AlteraPll

hdlUsed :: [Int]
hdlUsed = [ knownDomIn, clk, rst ]
 where
  knownDomIn
    :< _clocksClass
    :< _clocksCxt
    :< _numOutClocks
    :< clk
    :< rst
    :< _ = (0...)

hdlValid :: BlackBoxContext -> Bool
hdlValid bbCtx | [(_,Product {})] <- bbResults bbCtx = True
hdlValid _ = False

qsysUsed :: [Int]
qsysUsed = [ knownDomIn, clocksCxt ]
 where
  knownDomIn
    :< _clocksClass
    :< clocksCxt
    :< _ = (0...)

altpllTF :: TemplateFunction
altpllTF = TemplateFunction hdlUsed hdlValid (hdlTemplate Altpll)

altpllQsysTF :: TemplateFunction
altpllQsysTF = TemplateFunction qsysUsed valid altpllQsysTemplate
 where
  valid = const True

alteraPllTF :: TemplateFunction
alteraPllTF = TemplateFunction hdlUsed hdlValid (hdlTemplate AlteraPll)

alteraPllQsysTF :: TemplateFunction
alteraPllQsysTF = TemplateFunction qsysUsed valid alteraPllQsysTemplate
 where
  valid = const True

hdlTemplate ::
  forall s .
  Backend s =>
  Variant ->
  BlackBoxContext ->
  State s Doc
hdlTemplate variant bbCtx
  | [ knownDomIn
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
    let
      stdName Altpll = "altpll"
      stdName AlteraPll = "altera_pll"
      pllOutName Altpll = "c"
      pllOutName AlteraPll = "outclk_"
      clkInName Altpll = "clk"
      clkInName AlteraPll = "refclk"
      rstName Altpll = "areset"
      rstName AlteraPll = "rst"

    instName <- Id.makeBasic $ fromMaybe (stdName variant) $ bbCtxName bbCtx

    -- TODO: unsafeMake is dubious here: I don't think we take names in
    -- TODO: bbQsysIncName into account when generating fresh ids
    let compNameId = Id.unsafeMake compName

    DSL.declarationReturn bbCtx (stdName variant <> "_block") $ do

      rstHigh <- DSL.unsafeToActiveHigh "reset" (DSL.ety knownDomIn) rst
      pllOuts <- DSL.declareN "pllOut" pllOutTys
      locked <- DSL.declare "locked" Bit
      pllLock <- DSL.boolFromBit "pllLock" locked

      let
        pllOutNames =
          map (\n -> pllOutName variant <> showt n)
            [0 .. length pllOutTys - 1]
        compInps =
          [ (clkInName variant, DSL.ety clk)
          , (rstName variant, DSL.ety rstHigh)
          ]
        compOuts = zip pllOutNames pllOutTys  <> [("locked", Bit)]
        inps =
          [ (clkInName variant, clk)
          , (rstName variant, rstHigh)
          ]
        outs = zip pllOutNames pllOuts <> [("locked", locked)]

      DSL.compInBlock compName compInps compOuts
      DSL.instDecl Empty compNameId instName [] inps outs

      pure [DSL.constructProduct resultTy (pllOuts <> [pllLock])]
  | otherwise
  = error $ ppShow bbCtx

altpllQsysTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
altpllQsysTemplate bbCtx
  |   (_,stripVoid -> (KnownDomain _ clkInPeriod _ _ _ _),_)
    : _clocksClass
    : (_,stripVoid -> Product _ _ (init -> kdOuts),_)
    : _ <- bbInputs bbCtx
  = let
    clkPeriod (KnownDomain _ p _ _ _ _) = p
    clkPeriod _ =
      error $ "Internal error: not a KnownDomain\n" <> ppShow bbCtx

    clkFreq p = periodToHz (fromInteger p) / 1e6 :: Double

    clkOutPeriods = map clkPeriod kdOuts
    clkLcms = map (lcm clkInPeriod) clkOutPeriods
    clkMults = zipWith quot clkLcms clkOutPeriods
    clkDivs = map (`quot` clkInPeriod) clkLcms
    clkOutFreqs = map clkFreq clkOutPeriods

    qsysParams = TextS.intercalate "\n  "
      [[I.__i|
        <parameter name="PORT_clk#{n}" value="PORT_USED" />
          <parameter name="CLK#{n}_MULTIPLY_BY" value="#{clkMult}" />
          <parameter name="CLK#{n}_DIVIDE_BY" value="#{clkDiv}" />
          <parameter name="CLK#{n}_DUTY_CYCLE" value="50" />
          <parameter name="CLK#{n}_PHASE_SHIFT" value="0" />
        |]
      | (clkMult, clkDiv, n) <- zip3 clkMults clkDivs [(0 :: Word)..]
      ]

    qsysConsts = TextS.intercalate "\n    "
      [[I.__i|
        CT\#PORT_clk#{n} PORT_USED
            CT\#CLK#{n}_MULTIPLY_BY #{clkMult}
            CT\#CLK#{n}_DIVIDE_BY #{clkDiv}
            CT\#CLK#{n}_DUTY_CYCLE 50
            CT\#CLK#{n}_PHASE_SHIFT 0
        |]
      | (clkMult, clkDiv, n) <- zip3 clkMults clkDivs [(0 :: Word)..]
      ]

    qsysPorts =
      TextS.intercalate "\n    "
        [[I.i|IF\#c#{n} {output 0}|] | n <- [0 .. length kdOuts - 1]]

    qsysPrivs = TextS.intercalate "\n    "
      [[I.__i|
        PT\#MULT_FACTOR#{n} #{clkMult}
            PT\#DIV_FACTOR#{n} #{clkDiv}
            PT\#EFF_OUTPUT_FREQ_VALUE#{n} #{clkOutFreq}
            PT\#DUTY_CYCLE#{n} 50.00000000
            PT\#PHASE_SHIFT0 0.00000000
        |]
      | (clkMult, clkDiv, clkOutFreq, n) <-
          zip4 clkMults clkDivs clkOutFreqs [(0 :: Word)..]
      ]

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
        #{qsysParams}
        <parameter name="COMPENSATE_CLOCK" value="CLK0" />
        <parameter name="INCLK0_INPUT_FREQUENCY" value="#{clkInPeriod}" />
        <parameter name="OPERATION_MODE" value="NORMAL" />
        <parameter name="PLL_TYPE" value="AUTO" />
        <parameter name="PORT_ARESET" value="PORT_USED" />
        <parameter name="PORT_INCLK0" value="PORT_USED" />
        <parameter name="PORT_LOCKED" value="PORT_USED" />
        <parameter name="HIDDEN_IS_FIRST_EDIT" value="0" />
        <parameter name="HIDDEN_CONSTANTS">
          #{qsysConsts}
          CT\#WIDTH_CLOCK 5
          CT\#LPM_TYPE altpll
          CT\#PLL_TYPE AUTO
          CT\#OPERATION_MODE NORMAL
          CT\#COMPENSATE_CLOCK CLK0
          CT\#INCLK0_INPUT_FREQUENCY #{clkInPeriod}
          CT\#PORT_INCLK0 PORT_USED
          CT\#PORT_ARESET PORT_USED
          CT\#BANDWIDTH_TYPE AUTO
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
          #{qsysPorts}
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
          #{qsysPrivs}</parameter>
        </module>
      </system>
      |]
    in
      pure bbText
  | otherwise
  = error $ ppShow bbCtx

alteraPllQsysTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
alteraPllQsysTemplate bbCtx
  |   (_,stripVoid -> kdIn,_)
    : _clocksClass
    : (_,stripVoid -> Product _ _ (init -> kdOuts),_)
    : _ <- bbInputs bbCtx
  = let
    clkFreq (KnownDomain _ p _ _ _ _)
      = periodToHz (fromIntegral p) / 1e6 :: Double
    clkFreq _ =
      error $ "Internal error: not a KnownDomain\n" <> ppShow bbCtx

    clkOuts = TextS.intercalate "\n"
      [[I.i|  <parameter name="gui_output_clock_frequency#{n}" value="#{f}"/>|]
      | (n,f) <- zip [(0 :: Word)..] (map clkFreq kdOuts)
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
        <parameter name="gui_reference_clock_frequency" value="#{clkFreq kdIn}" />
        <parameter name="gui_use_locked" value="true" />
      </module>
      </system>
      |]
    in
      pure bbText
  | otherwise
  = error $ ppShow bbCtx
