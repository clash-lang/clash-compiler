{-|
  Copyright   :  (C) 2018, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Blackbox template functions for Clash.Intel.ClockGen.{alteraPll,altpll}
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Primitives.Intel.ClockGen where

import Clash.Backend
import Clash.Netlist.BlackBox.Util
import Clash.Netlist.Id
import Clash.Netlist.Types
import Clash.Netlist.Util hiding (mkUniqueIdentifier)

import Control.Monad.State

import Data.Semigroup.Monad
import qualified Data.String.Interpolate.IsString as I
import Data.Text.Prettyprint.Doc.Extra

import qualified Data.Text as TextS

altpllTF :: TemplateFunction
altpllTF = TemplateFunction used valid altpllTemplate
 where
  used         = [0..4]
  valid bbCtx
    | [_,_,(nm,_,_),_,_] <- bbInputs bbCtx
    , Just _ <- exprToString nm
    , (Identifier _ Nothing,Product {}) <- bbResult bbCtx
    = True
  valid _ = False

altpllQsysTF :: TemplateFunction
altpllQsysTF = TemplateFunction used valid altpllQsysTemplate
 where
  used = [0..4]
  valid bbCtx
    | [_,_,(nm,_,_),_,_] <- bbInputs bbCtx
    , Just _ <- exprToString nm
    , (Identifier _ Nothing,Product {}) <- bbResult bbCtx
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
  :: Backend s
  => BlackBoxContext
  -> State s Doc
alteraPllTemplate bbCtx = do
 let mkId = mkUniqueIdentifier Basic
 locked <- mkId "locked"
 pllLock <- mkId "pllLock"
 alteraPll <- mkId "altera_pll_block"
 alteraPll_inst <- mkId instname0

 clocks <- traverse (mkUniqueIdentifier Extended)
                    [TextS.pack ("pllOut" ++ show n) | n <- [0..length tys - 1]]
 getMon $ blockDecl alteraPll $ concat
  [[ NetDecl Nothing locked  rstTy
   , NetDecl' Nothing Reg pllLock (Right Bool) Nothing]
  ,[ NetDecl Nothing clkNm ty | (clkNm,ty) <- zip clocks tys]
  ,[ InstDecl Comp Nothing [] compName alteraPll_inst [] $ concat
      [[(Identifier "refclk" Nothing,In,clkTy,clk)
       ,(Identifier "rst" Nothing,In,rstTy,rst)]
      ,[(Identifier (TextS.pack ("outclk_" ++ show n)) Nothing,Out,ty,Identifier k Nothing)
       |(k,ty,n) <- zip3 clocks tys [(0 :: Int)..]  ]
      ,[(Identifier "locked" Nothing,Out,rstTy,Identifier locked Nothing)]]
   , CondAssignment pllLock Bool (Identifier locked Nothing) rstTy
      [(Just (BitLit H),Literal Nothing (BoolLit True))
      ,(Nothing        ,Literal Nothing (BoolLit False))]
   , Assignment result (DataCon resTy (DC (resTy,0)) $ concat
                          [[Identifier k Nothing | k <- clocks]
                          ,[Identifier pllLock Nothing]])

   ]
  ]
 where
  (Identifier result Nothing,resTy@(Product _ _ (init -> tys))) = bbResult bbCtx
  [(nm,_,_),(clk,clkTy,_),(rst,rstTy,_)] = drop 3 (bbInputs bbCtx)
  Just nm' = exprToString nm
  instname0 = TextS.pack nm'
  compName = head (bbQsysIncName bbCtx)

altpllTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
altpllTemplate bbCtx = do
 let mkId = mkUniqueIdentifier Basic
 pllOut <- mkId "pllOut"
 locked <- mkId "locked"
 pllLock <- mkId "pllLock"
 alteraPll <- mkId "altpll_block"
 alteraPll_inst <- mkId instname0
 getMon $ blockDecl alteraPll
  [ NetDecl Nothing locked  Bit
  , NetDecl' Nothing Reg pllLock (Right Bool) Nothing
  , NetDecl Nothing pllOut clkOutTy
  , InstDecl Comp Nothing [] compName alteraPll_inst []
      [(Identifier "clk" Nothing,In,clkTy,clk)
      ,(Identifier "areset" Nothing,In,rstTy,rst)
      ,(Identifier "c0" Nothing,Out,clkOutTy,Identifier pllOut Nothing)
      ,(Identifier "locked" Nothing,Out,Bit,Identifier locked Nothing)]
  , CondAssignment pllLock Bool (Identifier locked Nothing) rstTy
      [(Just (BitLit H),Literal Nothing (BoolLit True))
      ,(Nothing        ,Literal Nothing (BoolLit False))]
  , Assignment result (DataCon resTy (DC (resTy,0))
                        [Identifier pllOut Nothing
                        ,Identifier pllLock Nothing])

  ]
 where
  [_,_,(nm,_,_),(clk,clkTy,_),(rst,rstTy,_)] = bbInputs bbCtx
  (Identifier result Nothing,resTy@(Product _ _ [clkOutTy,_])) = bbResult bbCtx
  Just nm' = exprToString nm
  instname0 = TextS.pack nm'
  compName = head (bbQsysIncName bbCtx)


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
  bbText = [I.i|<?xml version="1.0" encoding="UTF-8"?>
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
    CT#PORT_clk0 PORT_USED
    CT#CLK0_MULTIPLY_BY #{clkmult}
    CT#WIDTH_CLOCK 5
    CT#LPM_TYPE altpll
    CT#PLL_TYPE AUTO
    CT#CLK0_PHASE_SHIFT 0
    CT#OPERATION_MODE NORMAL
    CT#COMPENSATE_CLOCK CLK0
    CT#INCLK0_INPUT_FREQUENCY #{clkInPeriod}
    CT#PORT_INCLK0 PORT_USED
    CT#PORT_ARESET PORT_USED
    CT#BANDWIDTH_TYPE AUTO
    CT#CLK0_DUTY_CYCLE 50
    CT#CLK0_DIVIDE_BY #{clkdiv}
    CT#PORT_LOCKED PORT_USED</parameter>
  <parameter name="HIDDEN_IF_PORTS">
    IF#phasecounterselect {input 4}
    IF#locked {output 0}
    IF#reset {input 0}
    IF#clk {input 0}
    IF#phaseupdown {input 0}
    IF#scandone {output 0}
    IF#readdata {output 32}
    IF#write {input 0}
    IF#scanclk {input 0}
    IF#phasedone {output 0}
    IF#address {input 2}
    IF#c0 {output 0}
    IF#writedata {input 32}
    IF#read {input 0}
    IF#areset {input 0}
    IF#scanclkena {input 0}
    IF#scandataout {output 0}
    IF#configupdate {input 0}
    IF#phasestep {input 0}
    IF#scandata {input 0}</parameter>
  <parameter name="HIDDEN_MF_PORTS">
    MF#areset 1
    MF#clk 1
    MF#locked 1
    MF#inclk 1</parameter>
  <parameter name="HIDDEN_PRIVATES">
    PT#PHASE_SHIFT0 0.00000000
    PT#DIV_FACTOR0 #{clkdiv}
    PT#EFF_OUTPUT_FREQ_VALUE0 #{clkOutFreq}
    PT#MULT_FACTOR0 #{clkmult}
    PT#DUTY_CYCLE0 50.00000000</parameter>
  </module>
</system>|]

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

  clkOuts = TextS.unlines
    [[I.i|<parameter name="gui_output_clock_frequency#{n}" value="#{f}"/>|]
    | (n,f) <- zip [(0 :: Word)..] (map cklFreq kdOuts)
    ]

  -- See Note [QSys file templates] on how this qsys template was derived.
  bbText = [I.i|<?xml version="1.0" encoding="UTF-8"?>
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
</system>|]
