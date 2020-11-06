{-|
  Copyright   :  (C) 2018, Google Inc
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE RoleAnnotations #-}

module Clash.Netlist.Types where

import Control.Lens (Lens')
import Data.Hashable

data IdentifierType
data Identifier
data IdentifierSet
data HWType
data Declaration
data Component
data Expr
data BlackBox

class Monad m => IdentifierSetMonad m where
  identifierSetM :: (IdentifierSet -> IdentifierSet) -> m IdentifierSet

class HasIdentifierSet s where
  identifierSet :: Lens' s IdentifierSet

type role NetlistMonad nominal
data NetlistMonad a
data PreserveCase = PreserveCase | ToLower
instance Hashable PreserveCase
