{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
  Copyright   :  (C) 2012-2016, University of Twente
                     2021-2024, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Type Constructors in CoreHW
-}
module Clash.Core.TyCon
  ( TyCon (..),
    TyConName,
    TyConMap,
    AlgTyConRhs (..),
    mkKindTyCon,
    isTupleTyConLike,
    isPrimTc,
    isNewTypeTc,
    isPromotedDc,
    tyConDataCons,
  )
where

-- Internal Imports
import Clash.Core.DataCon (DataCon)
import Clash.Core.Name
import {-# SOURCE #-} Clash.Core.Type (Kind, Type)
import Clash.Core.Var (TyVar)
import Clash.Data.UniqMap (UniqMap)
import Clash.Unique

-- External Import
import Control.DeepSeq
import Data.Binary (Binary)
import Data.Function (on)
import qualified Data.Text as T
import GHC.Generics

-- | Type Constructor
data TyCon
  = -- | Algorithmic DataCons
    AlgTyCon
      { tyConUniq :: {-# UNPACK #-} !Unique,
        -- | Name of the TyCon
        tyConName :: !TyConName,
        -- | Kind of the TyCon
        tyConKind :: !Kind,
        -- | Number of type arguments
        tyConArity :: !Int,
        -- | DataCon definitions
        algTcRhs :: !AlgTyConRhs,
        -- | Is this a class dictionary?
        isClassTc :: !Bool
      }
  | PromotedDataCon
      { tyConUniq :: {-# UNPACK #-} !Unique, -- invariant (same as dcUniq)
        -- | Name of the TyCon
        tyConName :: !TyConName,
        -- | Kind of the TyCon
        tyConKind :: !Kind,
        -- | Number of type arguments
        tyConArity :: !Int,
        -- | DataCon which is promoted
        tyConData :: !DataCon
      }
  | -- | Function TyCons (e.g. type families)
    FunTyCon
      { tyConUniq :: {-# UNPACK #-} !Unique,
        -- | Name of the TyCon
        tyConName :: !TyConName,
        -- | Kind of the TyCon
        tyConKind :: !Kind,
        -- | Number of type arguments
        tyConArity :: !Int,
        -- | List of: ([LHS match types], RHS type)
        tyConSubst :: [([Type], Type)]
      }
  | -- | Primitive TyCons
    PrimTyCon
      { tyConUniq :: {-# UNPACK #-} !Unique,
        -- | Name of the TyCon
        tyConName :: !TyConName,
        -- | Kind of the TyCon
        tyConKind :: !Kind,
        -- | Number of type arguments
        tyConArity :: !Int
      }
  deriving (Show, Generic, NFData, Binary)

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
  setUnique tyCon u = tyCon {tyConUniq = u}

-- | TyCon reference
type TyConName = Name TyCon

type TyConMap = UniqMap TyCon

-- | The RHS of an Algebraic Datatype
data AlgTyConRhs
  = DataTyCon
      { -- | The DataCons of a TyCon
        dataCons :: [DataCon]
      }
  | NewTyCon
      { -- | The newtype DataCon
        dataCon :: !DataCon,
        -- | The argument type of the newtype
        -- DataCon in eta-reduced form, which is
        -- just the representation of the TyCon.
        -- The TyName's are the type-variables from
        -- the corresponding TyCon.
        ntEtadRhs :: ([TyVar], Type)
      }
  deriving (Show, Generic, NFData, Binary)

-- | Create a Kind out of a TyConName
mkKindTyCon ::
  TyConName ->
  Kind ->
  TyCon
mkKindTyCon name kind =
  PrimTyCon (nameUniq name) name kind 0

-- | Does the TyCon look like a tuple TyCon
isTupleTyConLike :: TyConName -> Bool
isTupleTyConLike nm = tupleName (T.takeWhileEnd (/= '.') (nameOcc nm))
  where
    tupleName nm0
      | Just ('(', nm1) <- T.uncons nm0,
        Just (nm2, ')') <- T.unsnoc nm1 =
          T.all (== ',') nm2
    tupleName _ =
      any (`T.isPrefixOf` (nameOcc nm)) $
        map
          T.pack
          [ "GHC.Tuple.Prim.Tuple",
            "GHC.Tuple.Tuple",
            -- GHC >= 9.14 hosts the Tuple type constructors here, with
            -- 'base'\''s 'GHC.Tuple' re-exporting them.
            "GHC.Internal.Tuple.Tuple"
          ]

-- | Get the DataCons belonging to a TyCon
tyConDataCons :: TyCon -> [DataCon]
tyConDataCons (AlgTyCon {algTcRhs = DataTyCon {dataCons = cons}}) = cons
tyConDataCons (AlgTyCon {algTcRhs = NewTyCon {dataCon = con}}) = [con]
tyConDataCons _ = []

isPrimTc ::
  TyCon ->
  Bool
isPrimTc PrimTyCon {} = True
isPrimTc _ = False

isNewTypeTc ::
  TyCon ->
  Bool
isNewTypeTc (AlgTyCon {algTcRhs = NewTyCon {}}) = True
isNewTypeTc _ = False

isPromotedDc ::
  TyCon ->
  Bool
isPromotedDc PromotedDataCon {} = True
isPromotedDc _ = False
