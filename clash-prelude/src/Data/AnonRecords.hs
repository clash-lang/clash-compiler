{-|
Copyright  :  (C) 2026, QBayLogic B.V.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Module to construct and use anonymous records.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures -Wterm-variable-capture #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

module Data.AnonRecords (
  (:&:)(..),
  (:=)(..),
  pattern (:=), FieldLabelProxy,
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
import Clash.CPP (maxTupleSize)
import Clash.XException (NFDataX)

import Data.Internal.TH.AnonRecords (deriveAsTuple)

-- RECORD COMPONENTS

-- | Anonymous record field.
-- See '(:=)' for constructing fields with explicit field labels
infixr 3 :=
newtype (:=) (x::Symbol) a = L a
  deriving (Generic, BitPack, NFDataX, Typeable)

-- | Anonymous record product type.
-- This operator should always be applied as a right infix operator:
--
-- > "a":=a :&: "b":=b :&: "c":=c
-- > "a":=a :&: ("b":=b :&: "c":=c)
--
-- Changing the structure will break most record functionality,
-- because class instances will be missing.
infixr 2 :&:
data (:&:) a b = a :&: b
  deriving (Show, Generic, BitPack, NFDataX, Typeable)


-- CONSTRUCTOR PATTERN

-- | A proxy for field labels, to be used as labels with '(:=)'.
-- Because the 'IsLabel' instance, you can write @#field@
-- rather than @FieldLabelProxy \@"field"@.
data FieldLabelProxy (f::Symbol) = FieldLabelProxy
  deriving (Show, Generic, BitPack, NFDataX, Typeable)

-- | Constructor that can be used in combination with 'FieldLabelProxy' labels
-- to add field names with cleaner syntax. Instead of writing:
--
-- > L @"x" x :&: L @"y" y
--
-- you can write:
--
-- > #x:=x :&: #y:=y
pattern (:=) :: FieldLabelProxy f -> a -> f:=a
pattern (:=) p x <- (withFLP -> (p,x)) where
  (:=) _ x = L x
{-# COMPLETE (:=) #-}

withFLP :: f:=a -> (FieldLabelProxy f, a)
withFLP (L x) = (FieldLabelProxy, x)

instance IsLabel f (FieldLabelProxy f) where
  fromLabel = FieldLabelProxy


-- FIELD ACCESS

-- | Type-level boolean indicating whether a field exists in a record.
type family HasField f a where
  HasField x (x:=_) = True
  HasField x (a:&:b) = HasField x a || HasField x b
  HasField _ _ = False

-- | Class for getting and setting record fields.
class AccessField (f::Symbol) a where
  type FieldType f a
  getField :: a -> FieldType f a
  -- | Set the value of a field. To replace the value with one of a different type,
  -- see 'insertField'.
  setField :: FieldType f a -> a -> a
  -- | Modify a field value in place.
  modifyField :: (FieldType f a -> FieldType f a) -> a -> a
  modifyField m r = setField @f (m $ getField @f r) r

instance AccessField f (f := a) where
  type FieldType f (f:=a) = a
  getField (L x) = x
  setField x _ = L x

instance (AccessField f (f:=b), FieldType f (f:=b) ~ a) => GHC.Records.HasField f (f:=b) a where
  getField = getField @f @(f:=b)

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

-- | Returns the record type after a field has been inserted.
type family WithField (f::Symbol) a r where
  WithField f a () = f:=a
  WithField f a (f:=b) = f:=a
  WithField f a ((f:=b) :&: r) = (f:=a) :&: r
  WithField f a (l :&: r) = l :&: WithField f a r

-- | Class for inserting fields into records.
class InsertField f a r where
  -- | If the record already has a field with the provided name, it is replaced.
  -- Otherwise, the field is added to the end.
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

-- | Class denoting that a field may be removed from a record.
class DeleteField (f::Symbol) r where
  -- | Remove a field from a record. Only possible if the field exists.
  deleteField :: r -> WithoutField f r

instance DeleteField f (f:=b) where
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

{- | Class for converting between records and tuples.

__NB__: The documentation only shows instances up to /3/-tuples. By
default, instances up to and including /12/-tuples will exist. If the flag
@large-tuples@ is set instances up to the GHC imposed limit will exist. The
GHC imposed limit is either 62 or 64 depending on the GHC version.
-}
class AsTuple a where
  type Tupled a
  fromTuple :: Tupled a -> a
  toTuple :: a -> Tupled a

instance AsTuple (x := a) where
  type Tupled (x:=a) = a
  fromTuple a = L a
  toTuple (L a) = a

deriveAsTuple 2 maxTupleSize
