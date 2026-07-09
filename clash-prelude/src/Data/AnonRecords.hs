{-|
Copyright  :  (C) 2026     , QBayLogic B.V.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

The module includes a way to construct anonymous records.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures -Wterm-variable-capture #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

module Data.AnonRecords (
  (:&:)(..),
  (:=)(..),
  HasField,
  WithField(..),
  AsTuple(..),
) where

import GHC.Generics (Generic)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy
import Data.Type.Bool (type (||))
import Data.Typeable (Typeable)

import Clash.Signal
import Clash.Class.BitPack (BitPack)
import Clash.XException (NFDataX)

infixr 3 :=
newtype (:=) (x::Symbol) a = L{unLabel::a}
  deriving (Generic, BitPack, NFDataX, Typeable)

infixr 2 :&:
data (:&:) a b = a :&: b
  deriving (Show, Generic, BitPack, NFDataX, Typeable)

-- SHOW

instance (KnownSymbol x, Show a) => Show (x := a) where
  -- show (L a) = show (symbolVal $ Proxy @x) <> " := " <> show a
  show (L a) = "L @" <> show (symbolVal $ Proxy @x) <> " " <> show a

 -- BUNDLE

instance (Bundle a, Bundle b) => Bundle (a :&: b) where
  type Unbundled dom (a :&: b) = (Unbundled dom a) :&: (Unbundled dom b)
  bundle (a :&: b) = (:&:) <$> (bundle a) <*> (bundle b)

  unbundle ab = (unbundle $ left <$> ab) :&: (unbundle $ right <$> ab)
   where
    left (a:&:_) = a
    right (_:&:b) = b

instance Bundle (x := a) where
  type Unbundled dom (x := a) = x := Signal dom a
  bundle (L sig) = L <$> sig
  unbundle sig = L $ unLabel <$> sig


-- TUPLES
-- from/to records up to size 3, presently

class AsTuple a where
  type Tupled a
  fromTuple :: Tupled a -> a
  toTuple :: a -> Tupled a

instance AsTuple (x := a) where
  type Tupled (x:=a) = a
  fromTuple a = L a
  toTuple (L a) = a

instance AsTuple (x:=a :&: y:=b) where
  type Tupled (x:=a :&: y:=b) = (a,b)
  fromTuple (a,b) = L a :&: L b
  toTuple (L a :&: L b) = (a,b)

instance AsTuple (x:=a :&: y:=b :&: z:=c) where
  type Tupled (x:=a :&: y:=b :&: z:=c) = (a,b,c)
  fromTuple (a,b,c) = L a :&: L b :&: L c
  toTuple (L a :&: L b :&: L c) = (a,b,c)


-- FIELD ACCESS

type family HasField f a where
  HasField x (x:=_) = True
  HasField x (a:&:b) = HasField x a || HasField x b
  HasField _ _ = False

class WithField (f::Symbol) a where
  type FieldType f a
  getField :: a -> FieldType f a
  setField :: FieldType f a -> a -> a

instance WithField f (f := a) where
  type FieldType f (f:=a) = a
  getField (L x) = x
  setField x _ = L x

class WithField' f a left where
  type FieldType' f a left
  getField' :: a -> FieldType' f a left
  setField' :: FieldType' f a left -> a -> a

instance (WithField' f (a:&:b) (HasField f a)) => WithField f (a:&:b) where
  type FieldType f (a:&:b) = FieldType' f (a:&:b) (HasField f a)
  getField   ab = getField' @f @(a:&:b) @(HasField f a)   ab
  setField x ab = setField' @f @(a:&:b) @(HasField f a) x ab

instance (WithField f a, HasField f a ~ True) => WithField' f (a:&:b) True where
  type FieldType' f (a:&:b) True = FieldType f a
  getField'   (a:&:_) = getField @f a
  setField' x (a:&:b) = setField @f x a :&: b

instance (WithField f b, HasField f a ~ False) => WithField' f (a:&:b) False where
  type FieldType' f (a:&:b) False = FieldType f b
  getField'   (_:&:b) = getField @f b
  setField' y (a:&:b) = a :&: setField @f y b
