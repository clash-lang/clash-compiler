{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2017-2018, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Variables in CoreHW
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Core.Var
  ( Attr' (..)
  , Var (..)
  , IdScope (..)
  , Id
  , TyVar
  , mkId
  , mkLocalId
  , mkGlobalId
  , mkTyVar
  , setVarUnique
  , setVarType
  , setIdScope
  , modifyVarName
  , isGlobalId
  , isLocalId
  , attrName
  )
where


import Control.DeepSeq                  (NFData (..))
import Data.Binary                      (Binary)
import Data.Function                    (on)
import Data.Hashable                    (Hashable(hashWithSalt))
import GHC.Generics                     (Generic)
import Clash.Core.Name                  (Name (..))
import {-# SOURCE #-} Clash.Core.Term   (Term, TmName)
import {-# SOURCE #-} Clash.Core.Type   (Kind, Type, TyName)
import Clash.Unique


-- | Interal version of Clash.Annotations.SynthesisAttributes.Attr.
--
-- Needed because Clash.Annotations.SynthesisAttributes.Attr uses the Symbol
-- kind for names, which do not have a term-level representation
data Attr'
  = BoolAttr' String Bool
  | IntegerAttr' String Integer
  | StringAttr' String String
  | Attr' String
  deriving (Eq, Show, NFData, Generic, Hashable, Ord, Binary)

attrName :: Attr' -> String
attrName (BoolAttr' n _)    = n
attrName (IntegerAttr' n _) = n
attrName (StringAttr' n _)  = n
attrName (Attr' n)          = n

-- | Variables in CoreHW
data Var a
  -- | Constructor for type variables
  = TyVar
  { varName :: !(Name a)
  , varUniq :: {-# UNPACK #-} !Unique
  -- ^ Invariant: forall x . varUniq x ~ nameUniq (varName x)
  , varType :: Kind
  }
  -- | Constructor for term variables
  | Id
  { varName :: !(Name a)
  , varUniq :: {-# UNPACK #-} !Unique
  -- ^ Invariant: forall x . varUniq x ~ nameUniq (varName x)
  , varType :: Type
  , idScope :: IdScope
  }
  deriving (Show,Generic,NFData,Binary)

-- | Gets a _key_ in the DBMS sense: a value that uniquely identifies a
-- Var. In case of a "Var" that is its unique and (if applicable) scope
varKey :: Var a -> (Unique, Maybe IdScope)
varKey TyVar{varUniq} = (varUniq, Nothing)
varKey Id{varUniq,idScope} = (varUniq, Just idScope)

instance Hashable (Var a) where
  hashWithSalt salt a = hashWithSalt salt (varKey a)

instance Eq (Var a) where
  (==) = (==) `on` varKey
  (/=) = (/=) `on` varKey

instance Ord (Var a) where
  compare = compare `on` varKey

instance Uniquable (Var a) where
  getUnique = varUniq
  setUnique var u = var {varUniq=u, varName=(varName var){nameUniq=u}}

data IdScope = GlobalId | LocalId
  deriving (Show,Generic,NFData,Hashable,Binary,Eq,Ord)

-- | Term variable
type Id    = Var Term
-- | Type variable
type TyVar = Var Type

-- | Change the name of a variable
modifyVarName ::
  (Name a -> Name a)
  -> Var a
  -> Var a
modifyVarName f (TyVar n _ k) =
  let n' = f n
  in  TyVar n' (nameUniq n') k
modifyVarName f (Id n _ t s) =
  let n' = f n
  in  Id n' (nameUniq n') t s

-- | Make a type variable
mkTyVar
  :: Kind
  -> TyName
  -> TyVar
mkTyVar tyKind tyName = TyVar tyName (nameUniq tyName) tyKind

-- | Make a term variable
mkId
  :: Type
  -> IdScope
  -> TmName
  -> Id
mkId tmType scope tmName = Id tmName (nameUniq tmName) tmType scope

mkLocalId
  :: Type
  -> TmName
  -> Id
mkLocalId tmType tmName = Id tmName (nameUniq tmName) tmType LocalId

mkGlobalId
  :: Type
  -> TmName
  -> Id
mkGlobalId tmType tmName = Id tmName (nameUniq tmName) tmType GlobalId

setVarUnique
  :: Var a
  -> Unique
  -> Var a
setVarUnique v u = v { varUniq = u, varName = (varName v) {nameUniq = u} }

setVarType
  :: Var a
  -> Type
  -> Var a
setVarType v t = v { varType = t }

isGlobalId
  :: Var a
  -> Bool
isGlobalId (Id {idScope = GlobalId}) = True
isGlobalId _ = False

isLocalId
  :: Var a
  -> Bool
isLocalId (Id {idScope = LocalId}) = True
isLocalId _  = False

setIdScope
  :: IdScope
  -> Var a
  -> Var a
setIdScope s (Id nm u t _) = Id nm u t s
setIdScope _ v = v
