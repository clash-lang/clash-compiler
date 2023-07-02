{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module T1506A where

import Clash.Explicit.Prelude

topEntity ::
  Clock XilinxSystem -> Reset XilinxSystem -> Enable XilinxSystem ->
  Clock IntelSystem -> Reset IntelSystem -> Enable IntelSystem ->
  Signal XilinxSystem Int -> Signal XilinxSystem (Maybe (Maybe Int)) ->
  Signal IntelSystem Int -> Signal IntelSystem (Maybe (Maybe Int)) ->
  _
topEntity xClk xRst {- 1 xRst -} xEna iClk iRst {- 1 iRst -} iEna xa xb ia ib =
  ( register xClk xRst xEna      (errorX "X")   xa -- 0 xRst
  , register xClk xRst enableGen (errorX "Y")   xa -- 0 xRst
  , register xClk xRst xEna      0              xa -- 1 xRst
  , register xClk xRst enableGen 0              xa -- 1 xRst
  , autoReg  xClk xRst xEna      (Just Nothing) xb -- 2 xRst
  , autoReg  xClk xRst enableGen (Just Nothing) xb -- 2 xRst

  , register iClk iRst iEna      (errorX "X")   ia -- 0 iRst
  , register iClk iRst enableGen (errorX "Y")   ia -- 0 iRst
  , register iClk iRst iEna      0              ia -- 2 iRst
  , register iClk iRst enableGen 0              ia -- 2 iRst
  , autoReg  iClk iRst iEna      (Just Nothing) ib -- 4 iRst
  , autoReg  iClk iRst enableGen (Just Nothing) ib -- 4 iRst
  )
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}
