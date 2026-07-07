-- {-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures -Wterm-variable-capture #-}

module Data.AnonRecords where


import GHC.TypeLits
import Data.Proxy
import Data.Type.Bool (type (||))

import Clash.Signal
-- import Clash.Signal.Bundle

infixr 3 :=
-- data (:=) x a where
--   (:=) :: forall (x :: Symbol) -> a -> x := a
-- -- GADT problematic!
data (:=) (x::Symbol) a = L{unLabel::a}

infixr 2 :&:
data (:&:) a b = a :&: b deriving Show

-- -- extra pattern for when you do not want to add the labels explicitly
-- pattern L a <- x := a where
--   L a = fillX a
--    where
--     fillX :: forall x' a. a -> x' := a
--     fillX a = x' := a

-- SHOW

instance (KnownSymbol x, Show a) => Show (x := a) where
  show (L a) = symbolVal (Proxy @x) <> " := " <> show a


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

type family HasField a f where
  HasField (x:=_) x = True
  HasField (_:=_) _ = False
  HasField (a:&:b) x = HasField a x || HasField b x
  HasField _ _ = False

class WithField a (f::Symbol) where
  type FieldType a f
  getField :: forall f' -> (f~f') => a -> FieldType a f
  setField :: forall f' -> (f~f') => FieldType a f -> a -> a

instance WithField (f := a) f where
  type FieldType (f:=a) f = a
  getField _ (L x) = x
  setField _ x _ = L x

class WithField' a f b where
  type FieldType' a f b
  getField' :: a -> FieldType' a f b
  setField' :: FieldType' a f b -> a -> a

instance (WithField' (a:&:b) f (HasField a f)) => WithField (a:&:b) f where
  type FieldType (a:&:b) f = FieldType' (a:&:b) f (HasField a f)
  getField _ ab = getField' @(a:&:b) @f @(HasField a f) ab
  setField _ x ab = setField' @(a:&:b) @f @(HasField a f) x ab

instance (WithField a f, HasField a f ~ True) => WithField' (a:&:b) f True where
  type FieldType' (a:&:b) f True = FieldType a f
  getField' (x:&:_) = getField f x
  setField' x (a:&:b) = setField f x a :&: b

instance (WithField b f, HasField b f ~ True) => WithField' (a:&:b) f False where
  type FieldType' (a:&:b) f False = FieldType b f
  getField' (_:&:y) = getField f y
  setField' y (a:&:b) = a :&: setField f y b
