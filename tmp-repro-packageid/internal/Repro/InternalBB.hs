{-# LANGUAGE OverloadedStrings #-}

module Repro.InternalBB (bb, bbTF, bbV) where

import Control.Monad.Trans.State (State)
import Data.Monoid (Ap(getAp))
import Data.Text.Prettyprint.Doc.Extra (Doc)

import Clash.Prelude
import Clash.Annotations.Primitive (Primitive(..), HDL(..), hasBlackBox)
import Clash.Backend (Backend, blockDecl)
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types

bbTF :: TemplateFunction
bbTF = TemplateFunction used valid bbTemplate
 where
  used    = [0]
  valid _ = True

bbTemplate :: Backend s => BlackBoxContext -> State s Doc
bbTemplate _ = do
  x <- Id.make "x123"
  getAp $ blockDecl x []

{-# ANN bb (InlinePrimitive [VHDL] "[ { \"BlackBox\" : { \"name\" : \"Repro.InternalBB.bb\", \"kind\": \"Declaration\", \"format\": \"Haskell\", \"templateFunction\": \"Repro.InternalBB.bbTF\"}} ]") #-}
bb :: Signal System Int -> Signal System Int
bb = id
{-# OPAQUE bb #-}

{-# ANN bbV hasBlackBox #-}
bbV :: Signal System Int -> Signal System Int
bbV = id
{-# OPAQUE bbV #-}
