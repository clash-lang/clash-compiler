{-|
  Copyright   :  (C) 2018     , Google Inc.,
                     2021-2022, QBayLogic B.V.
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

import Control.Monad.State
import Data.Monoid (Ap(getAp))
import qualified Data.String.Interpolate as I
import Data.Text.Prettyprint.Doc.Extra

import qualified Data.Text as TextS
import Data.Text.Extra (showt)

altpllTF :: TemplateFunction
altpllTF = TemplateFunction used valid altpllTemplate
 where
  used         = [0..4]
  valid bbCtx
    | [_,_,(nm,_,_),_,_] <- bbInputs bbCtx
    , Just _ <- exprToString nm
    , [(Identifier _ Nothing,Product {})] <- bbResults bbCtx
    = True
  valid _ = False

altpllQsysTF :: TemplateFunction
altpllQsysTF = TemplateFunction used valid altpllQsysTemplate
 where
  used = [0..4]
  valid bbCtx
    | [_,_,(nm,_,_),_,_] <- bbInputs bbCtx
    , Just _ <- exprToString nm
    , [(Identifier _ Nothing,Product {})] <- bbResults bbCtx
    = True
  valid _ = False

alteraPllTF :: TemplateFunction
alteraPllTF = TemplateFunction used valid alteraPllTemplate
 where
  used         = [1..20]
  valid bbCtx
    | ((nm,_,_):_) <- drop 3 (bbInputs bbCtx)
    , Just _ <- exprToString nm
    = True
  valid _ = False

alteraPllQsysTF :: TemplateFunction
alteraPllQsysTF = TemplateFunction used valid alteraPllQsysTemplate
 where
  used  = [1..20]
  valid bbCtx
    | ((nm,_,_):_) <- drop 3 (bbInputs bbCtx)
    , Just _ <- exprToString nm
    = True
  valid _ = False

alteraPllTemplate
  :: forall s
   . Backend s
  => BlackBoxContext
  -> State s Doc
alteraPllTemplate bbCtx = do
 locked <- Id.makeBasic "locked"
 pllLock <- Id.makeBasic "pllLock"
 alteraPll <- Id.makeBasic "altera_pll_block"
 alteraPll_inst <- Id.makeBasic instname0

 clocks <- Id.nextN (length tys) =<< Id.make "pllOut"

  -- TODO: unsafeMake is dubious here: I don't think we take names in
  -- TODO: bbQsysIncName into account when generating fresh ids
 let compName = Id.unsafeMake (head (bbQsysIncName bbCtx))

 let outclkPorts = map (\n -> instPort ("outclk_" <> showt n)) [(0 :: Int)..length clocks-1]

 getAp $ blockDecl alteraPll $ concat
  [[ NetDecl Nothing locked Bit
   , NetDecl Nothing pllLock Bool]
  ,[ NetDecl Nothing clkNm ty | (clkNm,ty) <- zip clocks tys]
  ,[ InstDecl Comp Nothing [] compName alteraPll_inst [] $ NamedPortMap $ concat
      [ [ (instPort "refclk", In, clkTy, clk)
        , (instPort "rst", In, rstTy, rst)]
      , [ (p, Out, ty, Identifier k Nothing) | (k, ty, p) <- zip3 clocks tys outclkPorts ]
      , [(instPort "locked", Out, Bit, Identifier locked Nothing)]]
   , CondAssignment pllLock Bool (Identifier locked Nothing) Bit
      [(Just (BitLit H),Literal Nothing (BoolLit True))
      ,(Nothing        ,Literal Nothing (BoolLit False))]
   , Assignment result Cont (DataCon resTy (DC (resTy,0)) $ concat
                          [[Identifier k Nothing | k <- clocks]
                          ,[Identifier pllLock Nothing]])

   ]
  ]
 where
  [(Identifier result Nothing,resTy@(Product _ _ (init -> tys)))] = bbResults bbCtx
  [(nm,_,_),(clk,clkTy,_),(rst,rstTy,_)] = drop 3 (bbInputs bbCtx)
  Just nm' = exprToString nm
  instname0 = TextS.pack nm'

altpllTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
altpllTemplate bbCtx = do
 pllOut <- Id.make "pllOut"
 locked <- Id.make "locked"
 pllLock <- Id.make "pllLock"
 alteraPll <- Id.make "altpll_block"
 alteraPll_inst <- Id.make instname0

 -- TODO: unsafeMake is dubious here: I don't think we take names in
 -- TODO: bbQsysIncName into account when generating fresh ids
 let compName = Id.unsafeMake (head (bbQsysIncName bbCtx))

 getAp $ blockDecl alteraPll
  [ NetDecl Nothing locked  Bit
  , NetDecl Nothing pllLock Bool
  , NetDecl Nothing pllOut clkOutTy
  , InstDecl Comp Nothing [] compName alteraPll_inst [] $ NamedPortMap $
      [ (instPort "clk", In, clkTy, clk)
      , (instPort "areset", In, rstTy, rst)
      , (instPort "c0", Out, clkOutTy, Identifier pllOut Nothing)
      , (instPort "locked", Out, Bit, Identifier locked Nothing)]
  , CondAssignment pllLock Bool (Identifier locked Nothing) Bit
      [(Just (BitLit H),Literal Nothing (BoolLit True))
      ,(Nothing        ,Literal Nothing (BoolLit False))]
  , Assignment result Cont (DataCon resTy (DC (resTy,0))
                        [Identifier pllOut Nothing
                        ,Identifier pllLock Nothing])

  ]
 where
  [_,_,(nm,_,_),(clk,clkTy,_),(rst,rstTy,_)] = bbInputs bbCtx
  [(Identifier result Nothing,resTy@(Product _ _ [clkOutTy,_]))] = bbResults bbCtx
  Just nm' = exprToString nm
  instname0 = TextS.pack nm'

altpllQsysTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
altpllQsysTemplate bbCtx = pure bbText
 where
  ((_,stripVoid -> kdIn,_):(_,stripVoid -> kdOut,_):_) = bbInputs bbCtx
  KnownDomain _ clkInPeriod _ _ _ _ = kdIn
  KnownDomain _ clkOutPeriod _ _ _ _ = kdOut
  clkOutFreq :: Double
  clkOutFreq = (1.0 / (fromInteger clkOutPeriod * 1.0e-12)) / 1e6
  clklcm = lcm clkInPeriod clkOutPeriod
  clkmult = clklcm `quot` clkOutPeriod
  clkdiv = clklcm `quot` clkInPeriod
  -- Note [QSys file templates]
  -- This QSys file template was derived from a "full" QSys system with a single
  -- "altpll" IP. Module parameters were then stripped on a trial-and-error
  -- basis to get a template that has the minimal number of parameters, but
  -- still has the desired, working, configuration.
  bbText = [I.__i|
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

alteraPllQsysTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
alteraPllQsysTemplate bbCtx = pure bbText
 where
  (_:(_,stripVoid -> kdIn,_):(_,stripVoid -> kdOutsProd,_):_) = bbInputs bbCtx
  kdOuts = case kdOutsProd of
    Product _ _ ps -> ps
    KnownDomain {} -> [kdOutsProd]
    _ -> error "internal error: not a Product or KnownDomain"

  cklFreq (KnownDomain _ p _ _ _ _)
    = (1.0 / (fromInteger p * 1.0e-12 :: Double)) / 1e6
  cklFreq _ = error "internal error: not a KnownDomain"

  clkOuts = TextS.intercalate "\n"
    [[I.i|  <parameter name="gui_output_clock_frequency#{n}" value="#{f}"/>|]
    | (n,f) <- zip [(0 :: Word)..] (map cklFreq kdOuts)
    ]

  -- See Note [QSys file templates] on how this qsys template was derived.
  bbText = [I.__i|
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
