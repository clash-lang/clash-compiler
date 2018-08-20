{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2017-2018, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Variables in CoreHW
-}

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Clash.Core.Var
  ( Attr' (..)
  , Var (..)
  , Id
  , TyVar
  , modifyVarName
  , attrName
  )
where


import Control.DeepSeq                  (NFData (..))
import Data.Binary                      (Binary)
import Data.Hashable                    (Hashable)
import Data.Typeable                    (Typeable)
import GHC.Generics                     (Generic)
import Unbound.Generics.LocallyNameless (Alpha,Embed,Subst(..))
import Clash.Core.Name                  (Name)
import {-# SOURCE #-} Clash.Core.Term   (Term)
import {-# SOURCE #-} Clash.Core.Type   (Kind, Type)


-- | Interal version of Clash.Annotation.SynthesisAttributes.Attr.
--
-- Needed because Clash.Annotation.SynthesisAttributes.Attr uses the Symbol
-- kind for names, which do not have a term-level representation
data Attr'
  = BoolAttr' String Bool
  | IntegerAttr' String Integer
  | StringAttr' String String
  | Attr' String
  deriving (Eq, Show, NFData, Generic, Hashable, Typeable, Alpha, Ord, Binary)

instance Subst Type Attr'
instance Subst Term Attr'

attrName :: Attr' -> String
attrName (BoolAttr' n _)    = n
attrName (IntegerAttr' n _) = n
attrName (StringAttr' n _)  = n
attrName (Attr' n)          = n

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
  deriving (Eq,Show,Generic,NFData,Hashable,Binary)

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
