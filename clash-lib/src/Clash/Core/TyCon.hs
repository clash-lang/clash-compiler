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
  , mkKindTyCon
  , isTupleTyConLike
  , isPrimTc
  , isNewTypeTc
  , isPromotedDc
  , tyConDataCons
  )
where

-- External Import
import Control.DeepSeq
import Data.Binary                            (Binary (..), getWord8, putWord8)
import Data.Function                          (on)
import qualified Data.Text as T

-- Internal Imports
import Clash.Core.DataCon                     (DataCon)
import Clash.Core.Name
import {-# SOURCE #-} Clash.Core.Scoped.Convert
  (mkAlgTyConFromClash, mkFunTyConFromClash, mkNewTyConRhsFromClash,
   mkPrimTyConFromClash, mkPromotedDataConFromClash)
import Clash.Core.Scoped.Sig                  (NewTyConSig, TyFamEqn)
import qualified Clash.Core.Scoped.Syntax     as Scoped
import {-# SOURCE #-} Clash.Core.Type         (Kind, Type)
import Clash.Core.Var                         (TyVar)
import Clash.Data.UniqMap (UniqMap)
import Clash.Unique

-- | Type Constructor
--
-- = Transition
--
-- The scoped fields, 'tyConKind', 'tyConEqns' and 'ntSig', are the truth.
-- The fields named @Legacy@ hold the same as Clash Core's types, for the
-- code that still uses Clash Core. They are derived lazily, and are deleted
-- with Clash Core (package B of the port). Make type constructors with the
-- constructors of "Clash.Core.Scoped.Convert", such as
-- 'Clash.Core.Scoped.Convert.mkAlgTyCon' and
-- 'Clash.Core.Scoped.Convert.mkAlgTyConFromClash', which derive the side they
-- are not given.
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
  , tyConLegacyKind :: Kind
  -- ^ Transitional, see above. Lazy.
  }
  | PromotedDataCon
  { tyConUniq   :: {-# UNPACK #-} !Unique -- invariant (same as dcUniq)
  , tyConName   :: !TyConName   -- ^ Name of the TyCon
  , tyConKind   :: Scoped.Kind 'Scoped.VoidS
  -- ^ Kind of the TyCon, which is closed. Lazy.
  , tyConArity  :: !Int         -- ^ Number of type arguments
  , tyConData   :: !DataCon     -- ^ DataCon which is promoted
  , tyConLegacyKind :: Kind
  -- ^ Transitional, see above. Lazy.
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
  , tyConLegacyKind :: Kind
  -- ^ Transitional, see above. Lazy.
  , tyConLegacySubst :: [([Type],Type)]
  -- ^ Transitional, see above: the equations as a list of: ([LHS match
  -- types], RHS type), whose free type variables are the pattern variables
  }
  -- | Primitive TyCons
  | PrimTyCon
  { tyConUniq    :: {-# UNPACK #-} !Unique
  , tyConName    :: !TyConName  -- ^ Name of the TyCon
  , tyConKind    :: Scoped.Kind 'Scoped.VoidS
  -- ^ Kind of the TyCon, which is closed. Lazy.
  , tyConArity   :: !Int        -- ^ Number of type arguments
  , tyConLegacyKind :: Kind
  -- ^ Transitional, see above. Lazy.
  }
  deriving (Show)

-- | Doesn't force the kinds and the equations, which are derived lazily (see
-- 'TyCon')
instance NFData TyCon where
  rnf tc = rnf (tyConName tc) `seq` rnf (tyConArity tc) `seq` case tc of
    AlgTyCon _ _ _ _ rhs cls _ -> rnf rhs `seq` rnf cls
    PromotedDataCon _ _ _ _ dc _ -> rnf dc
    FunTyCon {} -> ()
    PrimTyCon {} -> ()

-- | Transitional: through the types of Clash Core, in the format of the
-- derived instance of the type constructors of Clash Core, so that the
-- serialized inputs stay the same
instance Binary TyCon where
  put = \case
    AlgTyCon u nm _ ar rhs cls k ->
      putWord8 0 >> put u >> put nm >> put k >> put ar >> put rhs >> put cls
    PromotedDataCon u nm _ ar dc k ->
      putWord8 1 >> put u >> put nm >> put k >> put ar >> put dc
    FunTyCon u nm _ ar _ k eqns ->
      putWord8 2 >> put u >> put nm >> put k >> put ar >> put eqns
    PrimTyCon u nm _ ar k ->
      putWord8 3 >> put u >> put nm >> put k >> put ar
  get = getWord8 >>= \case
    0 -> mkAlgTyConFromClash <$> get <*> get <*> get <*> get <*> get <*> get
    1 -> mkPromotedDataConFromClash <$> get <*> get <*> get <*> get <*> get
    2 -> mkFunTyConFromClash <$> get <*> get <*> get <*> get <*> get
    3 -> mkPrimTyConFromClash <$> get <*> get <*> get <*> get
    t -> fail ("Binary TyCon: unknown tag " <> show t)

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
  , ntSig :: NewTyConSig
  -- ^ The type variables of the newtype, and the argument type of the
  -- newtype DataCon in eta-reduced form, which is just the representation of
  -- the TyCon, see "Clash.Core.Scoped.Sig". Lazy.
  , ntLegacyEtadRhs :: ([TyVar],Type)
  -- ^ Transitional, see 'TyCon': 'ntSig' as types of Clash Core. The
  -- TyName's are the type-variables from the corresponding TyCon. Lazy.
  }
  deriving (Show)

-- | Doesn't force the signature, see 'TyCon'
instance NFData AlgTyConRhs where
  rnf = \case
    DataTyCon dcs -> rnf dcs
    NewTyCon dc _ _ -> rnf dc

-- | Transitional, see the instance of 'TyCon'
instance Binary AlgTyConRhs where
  put = \case
    DataTyCon dcs -> putWord8 0 >> put dcs
    NewTyCon dc _ rhs -> putWord8 1 >> put dc >> put rhs
  get = getWord8 >>= \case
    0 -> DataTyCon <$> get
    1 -> mkNewTyConRhsFromClash <$> get <*> get
    t -> fail ("Binary AlgTyConRhs: unknown tag " <> show t)

-- | Create a Kind out of a TyConName
mkKindTyCon :: TyConName
            -> Kind
            -> TyCon
mkKindTyCon name kind
  = mkPrimTyConFromClash (nameUniq name) name kind 0

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
