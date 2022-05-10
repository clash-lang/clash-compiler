{-|
  Copyright   :  (C) 2018, Google Inc,
                     2022, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE RoleAnnotations #-}

module Clash.Netlist.Types where

import Control.DeepSeq (NFData)

data Declaration
data Component
data Expr
data BlackBox
data TopEntityT

instance NFData BlackBox

type role NetlistMonad nominal
data NetlistMonad a
