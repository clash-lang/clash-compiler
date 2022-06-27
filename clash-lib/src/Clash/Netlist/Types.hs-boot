{-|
  Copyright   :  (C) 2018, Google Inc,
                     2022, QBayLogic B.V.
                     2022, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE RoleAnnotations #-}

module Clash.Netlist.Types where

import Control.DeepSeq (NFData)
import Control.Lens (Lens')
import Data.Aeson (FromJSON)
import Data.Binary (Binary)
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Text (Text)

data IdentifierType
data Identifier
data IdentifierSet
data HWType
data CompDecl
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

data Blocking
  = NonBlocking
  | Blocking

instance Binary Blocking
instance Eq Blocking
instance Hashable Blocking
instance NFData Blocking
instance Show Blocking

data Usage
  = Cont
  | Proc Blocking

instance Binary Usage
instance Eq Usage
instance FromJSON Usage
instance Hashable Usage
instance NFData Usage
instance Show Usage

type UsageMap = Map Text Usage
