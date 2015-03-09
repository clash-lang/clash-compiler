{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Type Constructors in CoreHW
module CLaSH.Core.TyCon
  ( TyCon (..)
  , TyConName
  , AlgTyConRhs (..)
  , mkKindTyCon
  , isTupleTyConLike
  , tyConDataCons
  )
where

-- External Import
import                Control.DeepSeq
import                Data.Monoid                           (mempty)
import                Data.Typeable                         hiding (TyCon,tyConName)
import                GHC.Generics
import                Unbound.Generics.LocallyNameless
import                Unbound.Generics.LocallyNameless.Name (Name(..))

-- Internal Imports
import {-# SOURCE #-} CLaSH.Core.DataCon      (DataCon)
import {-# SOURCE #-} CLaSH.Core.Term         (Term)
import {-# SOURCE #-} CLaSH.Core.Type         (Kind, TyName, Type)
import                CLaSH.Util

-- | Type Constructor
data TyCon
  -- | Algorithmic DataCons
  = AlgTyCon
  { tyConName   :: TyConName   -- ^ Name of the TyCon
  , tyConKind   :: Kind        -- ^ Kind of the TyCon
  , tyConArity  :: Int         -- ^ Number of type arguments
  , algTcRhs    :: AlgTyConRhs -- ^ DataCon definitions
  }
  -- | Function TyCons (e.g. type families)
  | FunTyCon
  { tyConName   :: TyConName       -- ^ Name of the TyCon
  , tyConKind   :: Kind            -- ^ Kind of the TyCon
  , tyConArity  :: Int             -- ^ Number of type arguments
  , tyConSubst  :: [([Type],Type)] -- ^ List of: ([LHS match types], RHS type)
  }
  -- | Primitive TyCons
  | PrimTyCon
  { tyConName    :: TyConName  -- ^ Name of the TyCon
  , tyConKind    :: Kind       -- ^ Kind of the TyCon
  , tyConArity   :: Int        -- ^ Number of type arguments
  }
  -- | To close the loop on the type hierarchy
  | SuperKindTyCon
  { tyConName :: TyConName     -- ^ Name of the TyCon
  }
  deriving (Generic,Typeable)

instance Show TyCon where
  show (AlgTyCon       {tyConName = n}) = "AlgTyCon: " ++ show n
  show (FunTyCon       {tyConName = n}) = "FunTyCon: " ++ show n
  show (PrimTyCon      {tyConName = n}) = "PrimTyCon: " ++ show n
  show (SuperKindTyCon {tyConName = n}) = "SuperKindTyCon: " ++ show n

instance Eq TyCon where
  (==) = (==) `on` tyConName

instance Ord TyCon where
  compare = compare `on` tyConName

-- | TyCon reference
type TyConName = Name TyCon

-- | The RHS of an Algebraic Datatype
data AlgTyConRhs
  = DataTyCon
  { dataCons :: [DataCon]        -- ^ The DataCons of a TyCon
  }
  | NewTyCon
  { dataCon   :: DataCon         -- ^ The newtype DataCon
  , ntEtadRhs :: ([TyName],Type) -- ^ The argument type of the newtype
                                 -- DataCon in eta-reduced form, which is
                                 -- just the representation of the TyCon.
                                 -- The TyName's are the type-variables from
                                 -- the corresponding TyCon.
  }
  deriving (Show,Generic)

instance Alpha TyCon where
  aeq' c tc1 tc2      = aeq' c (tyConName tc1) (tyConName tc2)

  fvAny' _ _ tc       = pure tc

  close _ _ tc        = tc
  open _ _ tc         = tc

  isPat _             = mempty
  isTerm _            = True

  nthPatFind _        = Left
  namePatFind _ _     = Left 0

  swaps' _ _ tc       = tc
  lfreshen' _ tc cont = cont tc mempty
  freshen' _ tc       = return (tc,mempty)

  acompare' c tc1 tc2 = acompare' c (tyConName tc1) (tyConName tc2)


instance Alpha AlgTyConRhs

instance Subst Type TyCon
instance Subst Type AlgTyConRhs

instance Subst Term TyCon
instance Subst Term AlgTyConRhs

instance NFData TyCon where
  rnf tc = case tc of
    AlgTyCon nm ki ar rhs     -> rnf nm `seq` rnf ki `seq` rnf ar `seq` rnf rhs
    FunTyCon nm ki ar tcSubst -> rnf nm `seq` rnf ki `seq` rnf ar `seq` rnf tcSubst
    PrimTyCon nm ki ar        -> rnf nm `seq` rnf ki `seq` rnf ar
    SuperKindTyCon nm         -> rnf nm

instance NFData (Name TyCon) where
  rnf nm = case nm of
    (Fn s i) -> rnf s `seq` rnf i
    (Bn l r) -> rnf l `seq` rnf r

instance NFData AlgTyConRhs where
  rnf rhs = case rhs of
    DataTyCon dcs   -> rnf dcs
    NewTyCon dc eta -> rnf dc `seq` rnf eta

-- | Create a Kind out of a TyConName
mkKindTyCon :: TyConName
            -> Kind
            -> TyCon
mkKindTyCon name kind
  = PrimTyCon name kind 0

-- | Does the TyCon look like a tuple TyCon
isTupleTyConLike :: TyConName -> Bool
isTupleTyConLike nm = tupleName (name2String nm)
  where
    tupleName nm'
      | '(' <- head nm'
      , ')' <- last nm'
      = all (== ',') (init $ tail nm')
    tupleName _ = False

-- | Get the DataCons belonging to a TyCon
tyConDataCons :: TyCon -> [DataCon]
tyConDataCons (AlgTyCon {algTcRhs = DataTyCon { dataCons = cons}}) = cons
tyConDataCons (AlgTyCon {algTcRhs = NewTyCon  { dataCon  = con }}) = [con]
tyConDataCons _                                                    = []
