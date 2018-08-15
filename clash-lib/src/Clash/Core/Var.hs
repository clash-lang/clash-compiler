{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2017-2018, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Variables in CoreHW
-}

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RankNTypes            #-}

module Clash.Core.Var
  ( Attr' (..)
  , Var (..)
  , Id
  , TyVar
  , mkId
  , mkTyVar
  , setVarUnique
  , modifyVarName
  , attrName
  )
where


import Control.DeepSeq                  (NFData (..))
import Data.Binary                      (Binary)
import Data.Function                    (on)
import Data.Hashable                    (Hashable)
import GHC.Generics                     (Generic)
import Clash.Core.Name                  (Name (..))
import {-# SOURCE #-} Clash.Core.Term   (Term, TmName)
import {-# SOURCE #-} Clash.Core.Type   (Kind, Type, TyName)
import Clash.Unique


-- | Interal version of Clash.Annotation.SynthesisAttributes.Attr.
--
-- Needed because Clash.Annotation.SynthesisAttributes.Attr uses the Symbol
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
  , varType :: Kind
  }
  -- | Constructor for term variables
  | Id
  { varName :: !(Name a)
  , varUniq :: {-# UNPACK #-} !Unique
  , varType :: Type
  }
  deriving (Show,Generic,NFData,Hashable,Binary)

instance Eq (Var a) where
  (==) = (==) `on` varUniq
  (/=) = (/=) `on` varUniq

instance Ord (Var a) where
  compare = compare `on` varUniq

instance Uniquable (Var a) where
  getUnique = varUniq

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
modifyVarName f (Id n _ t) =
  let n' = f n
  in  Id n' (nameUniq n') t

-- | Make a type variable
mkTyVar
  :: Kind
  -> TyName
  -> TyVar
mkTyVar tyKind tyName = TyVar tyName (nameUniq tyName) tyKind

-- | Make a term variable
mkId
  :: Type
  -> TmName
  -> Id
mkId tmType tmName = Id tmName (nameUniq tmName) tmType

setVarUnique
  :: Var a
  -> Unique
  -> Var a
setVarUnique v u = v { varUniq = u, varName = (varName v) {nameUniq = u} }
