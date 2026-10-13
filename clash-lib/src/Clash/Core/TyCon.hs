{-|
  Copyright   :  (C) 2012-2016, University of Twente
                     2021-2024, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Type Constructors in CoreHW

  The signatures of type constructors in scoped Clash Core
  ("Clash.Core.Sig"): the signature of a newtype ('newTyConSig') and the
  equations of a type family ('tyFamEqns'), and the instantiation of a newtype
  ('newTyConInstRhs').
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Core.TyCon
  ( TyCon (..)
  , TyConName
  , TyConMap
  , AlgTyConRhs (..)
    -- * Constructors
  , mkAlgTyCon
  , mkPromotedDataCon
  , mkFunTyCon
  , mkPrimTyCon
  , mkNewTyConRhs
  , isTupleTyConLike
  , isPrimTc
  , isNewTypeTc
  , isPromotedDc
  , tyConDataCons
    -- * Newtypes
  , NewTyConSig (..)
  , newTyConSig
  , newTyConInstRhs
    -- * Type families
  , TyFamEqn (..)
  , tyFamEqns
    -- * Data constructors
  , DataConSig (..)
  )
where

-- External Import
import Control.DeepSeq
import Data.Function                          (on)
import qualified Data.List as List
import qualified Data.Text as T

-- Internal Imports
import {-# SOURCE #-} Clash.Core.DataCon       (DataCon)
import Clash.Core.Name
import Clash.Core.Sig
import Clash.Core.Subst
import Clash.Core.Syntax hiding (Name)
import qualified Clash.Core.Syntax     as Scoped
import Clash.Data.UniqMap (UniqMap)
import Clash.Unique

-- | Type Constructor. Make type constructors with the constructors below,
-- such as 'mkAlgTyCon', which take the unique from the name.
data TyCon
  -- | Algorithmic DataCons
  = AlgTyCon
  { tyConUniq   :: {-# UNPACK #-} !Unique
  , tyConName   :: !TyConName   -- ^ Name of the TyCon
  , tyConKind   :: Scoped.Kind 'Scoped.VoidS
  -- ^ Kind of the TyCon, which is closed. Lazy.
  , tyConArity  :: !Int         -- ^ Number of type arguments
  , algTcRhs    :: !AlgTyConRhs -- ^ DataCon definitions
  , isClassTc   :: !Bool        -- ^ Is this a class dictionary?
  }
  | PromotedDataCon
  { tyConUniq   :: {-# UNPACK #-} !Unique -- invariant (same as dcUniq)
  , tyConName   :: !TyConName   -- ^ Name of the TyCon
  , tyConKind   :: Scoped.Kind 'Scoped.VoidS
  -- ^ Kind of the TyCon, which is closed. Lazy.
  , tyConArity  :: !Int         -- ^ Number of type arguments
  , tyConData   :: !DataCon     -- ^ DataCon which is promoted
  }
  -- | Function TyCons (e.g. type families)
  | FunTyCon
  { tyConUniq   :: {-# UNPACK #-} !Unique
  , tyConName   :: !TyConName      -- ^ Name of the TyCon
  , tyConKind   :: Scoped.Kind 'Scoped.VoidS
  -- ^ Kind of the TyCon, which is closed. Lazy.
  , tyConArity  :: !Int            -- ^ Number of type arguments
  , tyConEqns   :: [TyFamEqn]
  -- ^ The equations, in order, see "Clash.Core.Sig". Lazy.
  }
  -- | Primitive TyCons
  | PrimTyCon
  { tyConUniq    :: {-# UNPACK #-} !Unique
  , tyConName    :: !TyConName  -- ^ Name of the TyCon
  , tyConKind    :: Scoped.Kind 'Scoped.VoidS
  -- ^ Kind of the TyCon, which is closed. Lazy.
  , tyConArity   :: !Int        -- ^ Number of type arguments
  }
  deriving (Show)

-- | Doesn't force the kinds and the equations, which are lazy
instance NFData TyCon where
  rnf tc = rnf (tyConName tc) `seq` rnf (tyConArity tc) `seq` case tc of
    AlgTyCon _ _ _ _ rhs cls -> rnf rhs `seq` rnf cls
    PromotedDataCon _ _ _ _ dc -> rnf dc
    FunTyCon {} -> ()
    PrimTyCon {} -> ()

instance Eq TyCon where
  (==) = (==) `on` tyConUniq
  (/=) = (/=) `on` tyConUniq

instance Uniquable TyCon where
  getUnique = tyConUniq
  setUnique tyCon u = tyCon {tyConUniq=u}

-- | TyCon reference
type TyConName = Name TyCon
type TyConMap  = UniqMap TyCon

-- | The RHS of an Algebraic Datatype
data AlgTyConRhs
  = DataTyCon
  { dataCons :: [DataCon]        -- ^ The DataCons of a TyCon
  }
  | NewTyCon
  { dataCon   :: !DataCon        -- ^ The newtype DataCon
  , ntSig :: NewTyConSig
  -- ^ The type variables of the newtype, and the argument type of the
  -- newtype DataCon in eta-reduced form, which is just the representation of
  -- the TyCon, see "Clash.Core.Sig". Lazy.
  }
  deriving (Show)

-- | Doesn't force the signature, which is lazy
instance NFData AlgTyConRhs where
  rnf = \case
    DataTyCon dcs -> rnf dcs
    NewTyCon dc _ -> rnf dc

-- | An algebraic data type: name, closed kind, arity, data constructors, and
-- whether it is a class dictionary
mkAlgTyCon :: TyConName -> Scoped.Kind 'Scoped.VoidS -> Int -> AlgTyConRhs -> Bool -> TyCon
mkAlgTyCon nm = AlgTyCon (nameUniq nm) nm

-- | A promoted data constructor: name, closed kind, arity, and the data
-- constructor
mkPromotedDataCon :: TyConName -> Scoped.Kind 'Scoped.VoidS -> Int -> DataCon -> TyCon
mkPromotedDataCon nm = PromotedDataCon (nameUniq nm) nm

-- | A type family: name, closed kind, arity, and equations
mkFunTyCon :: TyConName -> Scoped.Kind 'Scoped.VoidS -> Int -> [TyFamEqn] -> TyCon
mkFunTyCon nm = FunTyCon (nameUniq nm) nm

-- | A primitive type constructor: name, closed kind, and arity
mkPrimTyCon :: TyConName -> Scoped.Kind 'Scoped.VoidS -> Int -> TyCon
mkPrimTyCon nm = PrimTyCon (nameUniq nm) nm

-- | The right-hand side of a newtype: its data constructor and signature
mkNewTyConRhs :: DataCon -> NewTyConSig -> AlgTyConRhs
mkNewTyConRhs = NewTyCon

-- | Does the TyCon look like a tuple TyCon
isTupleTyConLike :: TyConName -> Bool
isTupleTyConLike nm = tupleName (T.takeWhileEnd (/= '.') (nameOcc nm))
  where
    tupleName nm0
      | Just ('(', nm1) <- T.uncons nm0
      , Just (nm2, ')') <- T.unsnoc nm1
      = T.all (== ',') nm2
    tupleName _ =
      any (`T.isPrefixOf` (nameOcc nm)) $ map T.pack
        [ "GHC.Tuple.Prim.Tuple"
        , "GHC.Tuple.Tuple"
        -- GHC >= 9.14 hosts the Tuple type constructors here, with
        -- 'base'\''s 'GHC.Tuple' re-exporting them.
        , "GHC.Internal.Tuple.Tuple"
        ]

-- | Get the DataCons belonging to a TyCon
tyConDataCons :: TyCon -> [DataCon]
tyConDataCons (AlgTyCon {algTcRhs = DataTyCon { dataCons = cons}}) = cons
tyConDataCons (AlgTyCon {algTcRhs = NewTyCon  { dataCon  = con }}) = [con]
tyConDataCons _                                                    = []

isPrimTc
  :: TyCon
  -> Bool
isPrimTc PrimTyCon{} = True
isPrimTc _ = False

isNewTypeTc
  :: TyCon
  -> Bool
isNewTypeTc (AlgTyCon {algTcRhs = NewTyCon {}}) = True
isNewTypeTc _ = False

isPromotedDc
  :: TyCon
  -> Bool
isPromotedDc PromotedDataCon{} = True
isPromotedDc _ = False

-- * Newtypes

-- | The signature of a newtype. 'Nothing' if the type constructor is not a
-- newtype.
newTyConSig :: TyCon -> Maybe NewTyConSig
newTyConSig = \case
  AlgTyCon {algTcRhs = NewTyCon {ntSig}} -> Just ntSig
  _ -> Nothing

-- | The representation type of a newtype applied to the given types: the types
-- are substituted for the type variables of the newtype, and the remaining
-- ones are applied to the result. 'Nothing' if there are fewer types than
-- type variables.
--
-- See 'Clash.Core.Type.coreView1'.
newTyConInstRhs :: Distinct n => Scope n -> NewTyConSig -> [Type n] -> Maybe (Type n)
newTyConInstRhs sc (NewTyConSig tvs rhs) = instTele (closedSubst sc) tvs rhs

-- Substitute the types for the variables of the telescope, and apply the
-- remaining ones
instTele
  :: Distinct n => Subst i n -> Tele TyBinder i p -> Type p -> [Type n] -> Maybe (Type n)
instTele s TNil rhs tys = Just (List.foldl' AppTy (substTy s rhs) tys)
instTele s (TCons b bs) rhs (ty:tys) = instTele (extendTySubst s b ty) bs rhs tys
instTele _ TCons {} _ [] = Nothing

-- * Type families

-- | The equations of a type family, in order. 'Nothing' if the type
-- constructor is not a type family.
tyFamEqns :: TyCon -> Maybe [TyFamEqn]
tyFamEqns = \case
  FunTyCon {tyConEqns} -> Just tyConEqns
  _ -> Nothing
