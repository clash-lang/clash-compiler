{-|
  Copyright   :  (C) 2020, Foamspace corp & Christoph Mayer
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  hcab14@gmail.com

  LATTICE ECP5 IO primitives. Implementations are documented in the
  <http://www.latticesemi.com/-/media/LatticeSemi/Documents/ApplicationNotes/EH/FPGA-TN-02032-1-2-ECP5-ECP5G-sysIO-Usage-Guide.ashx?document_id=50464>.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Cores.LatticeSemi.ECP5.Blackboxes.IO (bbTF) where

import           Clash.Backend
import           Clash.Netlist.BlackBox.Util
import           Clash.Netlist.Id
import           Clash.Netlist.Types
import           Control.Monad.State             (State())
import           Data.Semigroup.Monad            (getMon)
import           Data.Text as TextS
import           Data.Text.Prettyprint.Doc.Extra
import           Prelude

-- | Generates HDL for ECP5 bidirectional buffer BB
--   FPGA TN 02032 sysIO
--     BB buf7 (.I(Q_out7), .T(Q_tri7), .O(buf_Data7), .B(Data[7]));
bbTF :: TemplateFunction
bbTF = TemplateFunction used valid bbTemplate
 where
  used = [3..6]
  valid = const True

bbTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
bbTemplate bbCtx = do
  let mkId = mkUniqueIdentifier Basic

  bb      <- mkId "bb"
  bb_inst <- mkId "bb_inst"
  dIn     <- mkId "dIn"

  getMon $ blockDecl bb $
    [ NetDecl Nothing dIn Bit
    , InstDecl Comp Nothing [] compName bb_inst
      [
      ]
      [ -- NOTE: Direction is set to 'In', but will be rendered as inout due to
        -- its type packagePinTy
        (Identifier "B" Nothing, In, packagePinTy, packagePin)
      , (Identifier "T" Nothing, In,  Bool, outputEnable)
      , (Identifier "I" Nothing, In,  Bit, dOut)
      , (Identifier "O" Nothing, Out, Bit, Identifier dIn Nothing)
      ]
    , Assignment result (Identifier dIn Nothing)
    ]
 where
  [  _HasCallStack
   , _HasBiSignalDefault
   , _KnownDomain
   , (intrinsicName, String, _)
   , (packagePin, packagePinTy, _)
   , (dOut, Bit, _)
   , (outputEnable, Bool, _)
   ] = bbInputs bbCtx

  Just compName' = exprToString intrinsicName
  compName = TextS.pack compName'

  (Identifier result Nothing,_) = bbResult bbCtx
