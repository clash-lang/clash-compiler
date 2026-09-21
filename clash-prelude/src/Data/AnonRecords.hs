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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures -Wterm-variable-capture #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

module Data.AnonRecords (
  (:&:)(..),
  (:=)(..),
  pattern (:=),
  HasField,
  WithField, WithoutField,
  AccessField(..), InsertField(..), DeleteField(..),
  AsTuple(..),
) where

import GHC.Generics (Generic)
import GHC.OverloadedLabels (IsLabel(..))
import qualified GHC.Records
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy
import Data.Type.Equality (type (==))
import Data.Type.Bool (type (||))
import Data.Typeable (Typeable)

import Clash.Signal
import Clash.Class.BitPack (BitPack)
import Clash.XException (NFDataX)

infixr 3 :=
newtype (:=) (x::Symbol) a = L a
  deriving (Generic, BitPack, NFDataX, Typeable)

infixr 2 :&:
data (:&:) a b = a :&: b
  deriving (Show, Generic, BitPack, NFDataX, Typeable)

-- CONSTRUCTOR PATTERN

data FieldLabelProxy f = FieldLabelProxy
  deriving (Show, Generic, BitPack, NFDataX, Typeable)

pattern (:=) :: FieldLabelProxy f -> a -> f:=a
pattern (:=) p x <- (withFLP -> (p,x)) where
  (:=) _ x = L x

withFLP :: f:=a -> (FieldLabelProxy f, a)
withFLP (L x) = (FieldLabelProxy, x)

instance (f~f2) => IsLabel f (FieldLabelProxy f2) where
  fromLabel = FieldLabelProxy

-- FIELD ACCESS

type family HasField f a where
  HasField x (x:=_) = True
  HasField x (a:&:b) = HasField x a || HasField x b
  HasField _ _ = False

class AccessField (f::Symbol) a where
  type FieldType f a
  getField :: a -> FieldType f a
  setField :: FieldType f a -> a -> a

instance AccessField f (f := a) where
  type FieldType f (f:=a) = a
  getField (L x) = x
  setField x _ = L x

instance (AccessField f (f2:=b), FieldType f (f2:=b) ~ a) => GHC.Records.HasField f (f2:=b) a where
  getField = getField @f @(f2:=b)

instance (AccessField' f (a:&:b) (HasField f a)) => AccessField f (a:&:b) where
  type FieldType f (a:&:b) = FieldType' f (a:&:b) (HasField f a)
  getField   ab = getField' @f @(a:&:b) @(HasField f a)   ab
  setField x ab = setField' @f @(a:&:b) @(HasField f a) x ab

instance (AccessField f (l:&:r), FieldType f (l:&:r) ~ a) => GHC.Records.HasField f (l:&:r) a where
  getField = getField @f @(l:&:r)

class AccessField' f a left where
  type FieldType' f a left
  getField' :: a -> FieldType' f a left
  setField' :: FieldType' f a left -> a -> a

instance (AccessField f a, HasField f a ~ True) => AccessField' f (a:&:b) True where
  type FieldType' f (a:&:b) True = FieldType f a
  getField'   (a:&:_) = getField @f a
  setField' x (a:&:b) = setField @f x a :&: b

instance (AccessField f b, HasField f a ~ False) => AccessField' f (a:&:b) False where
  type FieldType' f (a:&:b) False = FieldType f b
  getField'   (_:&:b) = getField @f b
  setField' y (a:&:b) = a :&: setField @f y b

-- FIELD ADDITION

type family WithField (f::Symbol) a r where
  WithField f a () = f:=a
  WithField f a (f:=b) = f:=a
  WithField f a ((f:=b) :&: r) = (f:=a) :&: r
  WithField f a (l :&: r) = l :&: WithField f a r

class InsertField f a r where
  insertField :: a -> r -> WithField f a r

instance InsertField f a () where
  insertField x _ = L @f x

instance (InsertField' f a (f2:=b) (f==f2)) => InsertField f a (f2:=b) where
  insertField = insertField' @f @a @(f2:=b) @(f==f2)

instance (InsertField' f a (f2:=b :&: r) (f==f2)) => InsertField f a (f2:=b :&: r) where
  insertField = insertField' @f @a @(f2:=b :&: r) @(f==f2)

class InsertField' f a r (eq::Bool) where
  insertField' :: a -> r -> WithField f a r

instance (WithField f a (f2:=b) ~ (f:=a)) => InsertField' f a (f2:=b) True where
  insertField' x _ = L x

instance (WithField f a (f2:=b) ~ (f2:=b :&: f:=a)) => InsertField' f a (f2:=b) False where
  insertField' x r = r :&: L x

instance (WithField f a (l :&: r) ~ (f:=a :&: r)) => InsertField' f a (l :&: r) True where
  insertField' x (_ :&: r) = L x :&: r

instance (InsertField f a r, WithField f a (l :&: r) ~ (l :&: WithField f a r)) => InsertField' f a (l :&: r) False where
  insertField' x (l :&: r) = l :&: insertField @f @a @r x r


-- FIELD REMOVAL

type family WithoutField (f::Symbol) r where
  WithoutField f (f:=a) = ()
  WithoutField f ((f:=a) :&: r) = r
  WithoutField f (l :&: (f:=a)) = l
  WithoutField f (l :&: r) = WithoutField f r

class DeleteField (f::Symbol) r where
  deleteField :: r -> WithoutField f r

instance (f~f2) => DeleteField f (f2:=b) where
  deleteField _ = ()

instance (DeleteField' f (f2:=b :&: f3:=b) (f==f2) (f==f3)) => DeleteField f (f2:=b :&: f3:=b) where
  deleteField = deleteField' @f @_ @(f==f2) @(f==f3)

instance (DeleteField' f (f2:=b :&: r :&: rr) (f==f2) False) => DeleteField f (f2:=b :&: r :&: rr) where
  deleteField = deleteField' @f @_ @(f==f2) @False

class DeleteField' (f::Symbol) r (eqL::Bool) (eqR::Bool) where
  deleteField' :: r -> WithoutField f r

instance (WithoutField f (l :&: r) ~ r) => DeleteField' f (l :&: r) True eqR where
  deleteField' (_ :&: r) = r

instance (WithoutField f (l :&: r) ~ l) => DeleteField' f (l :&: r) False True where
  deleteField' (l :&: _) = l

instance (DeleteField f r, WithoutField f (l :&: r) ~ (l :&: WithoutField f r)) => DeleteField' f (l :&: r) False False where
  deleteField' (l :&: r) = l :&: deleteField @f r


-- SHOW

instance (KnownSymbol x, Show a) => Show (x := a) where
  show (L a) = "#" <> (symbolVal $ Proxy @x) <> ":=" <> show a

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
  unbundle sig = L $ (\(L x) -> x) <$> sig


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
