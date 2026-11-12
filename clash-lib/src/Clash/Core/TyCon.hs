{-|
  Copyright   :  (C) 2012-2016, University of Twente
                     2021-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Type Constructors in CoreHW
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Clash.Core.TyCon
  ( TyCon (..)
  , TyConName
  , TyConMap
  , AlgTyConRhs (..)
  , mkKindTyCon
  , isTupleTyConLike
  , isPrimTc
  , isNewTypeTc
  , isPromotedDc
  , tyConDataCons
  , dataFamilyInst
  )
where

-- External Import
import Control.DeepSeq
import Data.Binary                            (Binary)
import Data.Function                          (on)
import qualified Data.Text as T
import GHC.Generics

-- Internal Imports
import Clash.Core.DataCon                     (DataCon)
import Clash.Core.Name
import {-# SOURCE #-} Clash.Core.Type         (Kind, Type)
import Clash.Core.Var                         (TyVar)
import Clash.Data.UniqMap (UniqMap)
import Clash.Unique

-- | Type Constructor
data TyCon
  -- | Algorithmic DataCons
  = AlgTyCon
  { tyConUniq   :: {-# UNPACK #-} !Unique
  , tyConName   :: !TyConName   -- ^ Name of the TyCon
  , tyConKind   :: !Kind        -- ^ Kind of the TyCon
  , tyConArity  :: !Int         -- ^ Number of type arguments
  , algTcRhs    :: !AlgTyConRhs -- ^ DataCon definitions
  , isClassTc   :: !Bool        -- ^ Is this a class dictionary?
  , tyConFamilyInst :: !(Maybe ([TyVar],Type))
  -- ^ For a data-family representation: parameters and source family application.
  }
  | PromotedDataCon
  { tyConUniq   :: {-# UNPACK #-} !Unique -- invariant (same as dcUniq)
  , tyConName   :: !TyConName   -- ^ Name of the TyCon
  , tyConKind   :: !Kind        -- ^ Kind of the TyCon
  , tyConArity  :: !Int         -- ^ Number of type arguments
  , tyConData   :: !DataCon     -- ^ DataCon which is promoted
  }
  -- | Function TyCons (e.g. type families)
  | FunTyCon
  { tyConUniq   :: {-# UNPACK #-} !Unique
  , tyConName   :: !TyConName      -- ^ Name of the TyCon
  , tyConKind   :: !Kind           -- ^ Kind of the TyCon
  , tyConArity  :: !Int            -- ^ Number of type arguments
  , tyConSubst  :: [([Type],Type)] -- ^ List of: ([LHS match types], RHS type)
  }
  -- | Primitive TyCons
  | PrimTyCon
  { tyConUniq    :: {-# UNPACK #-} !Unique
  , tyConName    :: !TyConName  -- ^ Name of the TyCon
  , tyConKind    :: !Kind       -- ^ Kind of the TyCon
  , tyConArity   :: !Int        -- ^ Number of type arguments
  }
  deriving (Show,Generic,NFData,Binary)

{-
instance Show TyCon where
  show (AlgTyCon       {tyConName = n}) = "AlgTyCon: " ++ show n
  show (FunTyCon       {tyConName = n}) = "FunTyCon: " ++ show n
  show (PrimTyCon      {tyConName = n}) = "PrimTyCon: " ++ show n
-}

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
  , ntEtadRhs :: ([TyVar],Type)  -- ^ The argument type of the newtype
                                 -- DataCon in eta-reduced form, which is
                                 -- just the representation of the TyCon.
                                 -- The TyName's are the type-variables from
                                 -- the corresponding TyCon.
  }
  deriving (Show,Generic,NFData,Binary)

-- | Create a Kind out of a TyConName
mkKindTyCon :: TyConName
            -> Kind
            -> TyCon
mkKindTyCon name kind
  = PrimTyCon (nameUniq name) name kind 0

-- | The source family application of a data-family representation constructor.
-- The type variables are the representation parameters needed by the mapping.
-- Trailing parameters can be eta-reduced, e.g. @R:TaggedBool@ maps directly to
-- @Tagged Bool@, with a label supplied later to either constructor.
dataFamilyInst :: TyCon -> Maybe ([TyVar],Type)
dataFamilyInst AlgTyCon{tyConFamilyInst = inst} = inst
dataFamilyInst _ = Nothing

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
