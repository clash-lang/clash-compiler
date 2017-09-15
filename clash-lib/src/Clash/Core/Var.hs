{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                          2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Variables in CoreHW
-}

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Clash.Core.Var
  ( Var (..)
  , Id
  , TyVar
  , modifyVarName
  )
where

import Control.DeepSeq                  (NFData (..))
import Data.Hashable                    (Hashable)
import Data.Typeable                    (Typeable)
import GHC.Generics                     (Generic)
import Unbound.Generics.LocallyNameless (Alpha,Embed,Subst(..))

import Clash.Core.Name                  (Name)
import {-# SOURCE #-} Clash.Core.Term   (Term)
import {-# SOURCE #-} Clash.Core.Type   (Kind, Type)

-- | Variables in CoreHW
data Var a
  -- | Constructor for type variables
  = TyVar
  { varName :: Name a
  , varKind :: Embed Kind
  }
  -- | Constructor for term variables
  | Id
  { varName :: Name a
  , varType :: Embed Type
  }
  deriving (Eq,Show,Generic,NFData,Hashable)

-- | Term variable
type Id    = Var Term
-- | Type variable
type TyVar = Var Type

instance (Typeable a, Alpha a) => Alpha (Var a)
instance Generic b => Subst Term (Var b)
instance Generic b => Subst Type (Var b)

-- | Change the name of a variable
modifyVarName ::
  (Name a -> Name a)
  -> Var a
  -> Var a
modifyVarName f (TyVar n k) = TyVar (f n) k
modifyVarName f (Id n t)    = Id (f n) t
