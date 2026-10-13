{-|
  Copyright   :  (C) 2012-2016, University of Twente
                     2021-2024, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Type Constructors in CoreHW
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
  )
where

-- External Import
import Control.DeepSeq
import Data.Function                          (on)
import qualified Data.Text as T

-- Internal Imports
import Clash.Core.DataCon                     (DataCon)
import Clash.Core.Name
import Clash.Core.Scoped.Sig                  (NewTyConSig, TyFamEqn)
import qualified Clash.Core.Scoped.Syntax     as Scoped
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
  -- ^ The equations, in order, see "Clash.Core.Scoped.Sig". Lazy.
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
  -- the TyCon, see "Clash.Core.Scoped.Sig". Lazy.
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
