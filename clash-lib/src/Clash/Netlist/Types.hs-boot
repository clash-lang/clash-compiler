{-|
  Copyright   :  (C) 2018, Google Inc,
                     2022, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE RoleAnnotations #-}

module Clash.Netlist.Types where

import Control.DeepSeq (NFData)
import Control.Lens (Lens')
import Data.Hashable

data IdentifierType
data Identifier
data IdentifierSet
data Declaration
data Component
data Expr
data BlackBox
data TopEntityT

instance NFData BlackBox

class Monad m => IdentifierSetMonad m where
  identifierSetM :: (IdentifierSet -> IdentifierSet) -> m IdentifierSet

class HasIdentifierSet s where
  identifierSet :: Lens' s IdentifierSet

type role NetlistMonad nominal
data NetlistMonad a
data PreserveCase = PreserveCase | ToLower
instance Hashable PreserveCase
instance Eq PreserveCase
instance Show PreserveCase
instance NFData PreserveCase
