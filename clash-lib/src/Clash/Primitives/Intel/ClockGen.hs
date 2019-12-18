{-|
  Copyright   :  (C) 2018, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Blackbox template functions for Clash.Intel.ClockGen.{alteraPll,altpll}
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Primitives.Intel.ClockGen where

import Clash.Backend
import Clash.Netlist.BlackBox.Util
import Clash.Netlist.Id
import Clash.Netlist.Types

import Control.Monad.State

import Data.Semigroup.Monad
import Data.Text.Prettyprint.Doc.Extra

import qualified Data.Text as TextS

altpllTF :: TemplateFunction
altpllTF = TemplateFunction used valid altpllTemplate
 where
  used         = [0,1,2]
  valid bbCtx
    | [(nm,_,_),_,_] <- bbInputs bbCtx
    , Just _ <- exprToString nm
    , (Identifier _ Nothing,Product {}) <- bbResult bbCtx
    = True
  valid _ = False

alteraPllTF :: TemplateFunction
alteraPllTF = TemplateFunction used valid alteraPllTemplate
 where
  used         = [1,2,3]
  valid bbCtx
    | [_,(nm,_,_),_,_] <- bbInputs bbCtx
    , Just _ <- exprToString nm
    , (Identifier _ Nothing,Product {}) <- bbResult bbCtx
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
 alteraPll <- mkId "alteraPll"
 alteraPll_inst <- mkId "alterPll_inst"

 clocks <- traverse (mkUniqueIdentifier Extended)
                    [TextS.pack ("pllOut" ++ show n) | n <- [0..length tys - 1]]
 getMon $ blockDecl alteraPll $ concat
  [[ NetDecl Nothing locked  rstTy
   , NetDecl Nothing pllLock Bool]
  ,[ NetDecl Nothing clkNm ty | (clkNm,ty) <- zip clocks tys]
  ,[ InstDecl Comp Nothing compName alteraPll_inst [] $ concat
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
  [_,(nm,_,_),(clk,clkTy,_),(rst,rstTy,_)] = bbInputs bbCtx
  (Identifier result Nothing,resTy@(Product _ _ (tail -> tys))) = bbResult bbCtx
  Just nm' = exprToString nm
  compName = TextS.pack nm'

altpllTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
altpllTemplate bbCtx = do
 let mkId = mkUniqueIdentifier Basic
 pllOut <- mkId "pllOut"
 locked <- mkId "locked"
 pllLock <- mkId "pllLock"
 alteraPll <- mkId "altpll"
 alteraPll_inst <- mkId "altpll_inst"
 getMon $ blockDecl alteraPll
  [ NetDecl Nothing locked  Bit
  , NetDecl Nothing pllLock Bool
  , NetDecl Nothing pllOut clkOutTy
  , InstDecl Comp Nothing compName alteraPll_inst []
      [(Identifier "inclk0" Nothing,In,clkTy,clk)
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
  [(nm,_,_),(clk,clkTy,_),(rst,rstTy,_)] = bbInputs bbCtx
  (Identifier result Nothing,resTy@(Product _ _ [clkOutTy,_])) = bbResult bbCtx
  Just nm' = exprToString nm
  compName = TextS.pack nm'

