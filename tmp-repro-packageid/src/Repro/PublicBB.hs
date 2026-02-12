{-# LANGUAGE OverloadedStrings #-}

module Repro.PublicBB (bbTF) where

import Control.Monad.Trans.State (State)
import Data.Monoid (Ap(getAp))
import Data.Text.Prettyprint.Doc.Extra (Doc)

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
