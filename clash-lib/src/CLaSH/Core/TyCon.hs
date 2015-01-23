{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

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
import                Unbound.LocallyNameless as Unbound hiding (rnf)
import                Unbound.LocallyNameless.Name (Name(Nm,Bn))

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
  deriving Show

Unbound.derive [''TyCon,''AlgTyConRhs]

instance Alpha TyCon where
  swaps' _ _ d    = d
  fv' _ _         = emptyC
  lfreshen' _ a f = f a empty
  freshen' _ a    = return (a,empty)
  aeq' _ tc1 tc2  = aeq (tyConName tc1) (tyConName tc2)
  acompare' _ tc1 tc2 = acompare (tyConName tc1) (tyConName tc2)
  open _ _ d      = d
  close _ _ d     = d
  isPat _         = error "isPat TyCon"
  isTerm _        = error "isTerm TyCon"
  isEmbed _       = error "isEmbed TyCon"
  nthpatrec _     = error "nthpatrec TyCon"
  findpatrec _ _  = error "findpatrec TyCon"

instance Alpha AlgTyConRhs

instance Subst Type TyCon
instance Subst Type AlgTyConRhs

instance Subst Term TyCon
instance Subst Term AlgTyConRhs

instance NFData TyCon where
  rnf tc = case tc of
    AlgTyCon nm ki ar rhs   -> rnf nm `seq` rnf ki `seq` rnf ar `seq` rnf rhs
    FunTyCon nm ki ar subst -> rnf nm `seq` rnf ki `seq` rnf ar `seq` rnf subst
    PrimTyCon nm ki ar      -> rnf nm `seq` rnf ki `seq` rnf ar
    SuperKindTyCon nm       -> rnf nm

instance NFData (Name TyCon) where
  rnf nm = case nm of
    (Nm _ s)   -> rnf s
    (Bn _ l r) -> rnf l `seq` rnf r

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
    tupleName nm
      | '(' <- head nm
      , ')' <- last nm
      = all (== ',') (init $ tail nm)
    tupleName _ = False

-- | Get the DataCons belonging to a TyCon
tyConDataCons :: TyCon -> [DataCon]
tyConDataCons (AlgTyCon {algTcRhs = DataTyCon { dataCons = cons}}) = cons
tyConDataCons (AlgTyCon {algTcRhs = NewTyCon  { dataCon  = con }}) = [con]
tyConDataCons _                                                    = []
