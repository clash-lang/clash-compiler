{-|
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Clash.GHC.PartialEval.Primitive.FromAst
  ( FromAst(..)
  , fromValueForce
  ) where

import Control.Monad (guard, when)
import Control.Monad.Catch (throwM)
import Data.Binary.IEEE754 (wordToDouble, wordToFloat)
import Data.Primitive.ByteArray (ByteArray(..))
import Data.Proxy
import Data.Reflection (reifyNat)

#if MIN_VERSION_base(4,15,0)
import GHC.Num.Integer (Integer(..))
import GHC.Num.Natural (Natural(..))
#else
import GHC.Integer.GMP.Internals (Integer(..), BigNat(..))
import GHC.Natural (Natural(..))
#endif

import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownNat, SomeNat(..), type (-), someNatVal)
import GHC.Types (Int(..), Word(..))
import Unsafe.Coerce

#if MIN_VERSION_ghc(9,0,0)
import GHC.Builtin.Names
import GHC.Types.Unique (getKey)
#else
import PrelNames
import Unique (getKey)
#endif

import Clash.Promoted.Nat
import Clash.Sized.Internal.BitVector as BV
import Clash.Sized.Internal.Index as I
import Clash.Sized.Internal.Signed as S
import Clash.Sized.Internal.Unsigned as U
import Clash.Sized.Vector as Vec
import Clash.XException (maybeIsX)

import Clash.Core.DataCon
import Clash.Core.Literal
import Clash.Core.Name (nameOcc)
import Clash.Core.PartialEval.Monad
import Clash.Core.PartialEval.NormalForm
import Clash.Core.Term (PrimInfo(..))

import {-# SOURCE #-} Clash.GHC.PartialEval.Eval
import Clash.GHC.PartialEval.Primitive.Info
import Clash.GHC.PartialEval.Primitive.Unboxed

fromValueForce :: (HasCallStack, FromAst a) => Value -> Eval a
fromValueForce value = do
  forced <- forceEval value
  when (isUndefined forced) (throwM ResultUndefined)

  fromValue forced

-- | FromAst gets value of a given type from some representation of an AST.
-- Failure means that the AST does not represent the value directly, but some
-- computation that returns a value of the desired type.
--
-- Extracting from Term or Value means primitives can be implemented lazily.
-- Consider the Term AST for
--
--   True && (let ... in False)
--
-- Initially, fromTerm can be used to potentially extract a value without
-- evaluating the term. This works for the LHS of (&&), but not the RHS.
-- However, if the RHS is evaluated, then fromValue will yield the False,
-- allowing the primitive (&&) to be evaluated. With judicial use of evaluate,
-- primitives can be implemented as lazy in all arguments desired.
--
class FromAst a where
  fromValue :: (HasCallStack) => Value -> Eval a

instance FromAst Value where
  fromValue = pure

#if !MIN_VERSION_base(4,15,0)
instance FromAst BigNat where
  fromValue (stripValue -> value) =
    case value of
      VData _ [Left x] env ->
        setLocalEnv env $ do
          !(ByteArray ba) <- boxByteArray <$> fromValueForce x
          pure (BN# ba)

      _ -> throwM (CannotConvert (Just value))
#endif

instance FromAst Bool where
  fromValue (stripValue -> value) =
    case value of
      VData dc [] _
        | dcUniq dc == getKey falseDataConKey -> pure False
        | dcUniq dc == getKey trueDataConKey  -> pure True

      _ -> throwM (CannotConvert (Just value))

instance FromAst UByteArray where
  fromValue (stripValue -> value) =
    case value of
      VLiteral (ByteArrayLiteral ba) -> pure (UByteArray ba)
      _ -> throwM (CannotConvert (Just value))

instance FromAst Char where
  fromValue (stripValue -> value) =
    case value of
      VNeutral (NePrim pr [Left x]) -> do
        guard (primName pr == "GHC.Types.C#")
        boxChar <$> fromValueForce x

      VData dc [Left x] env -> do
        guard (nameOcc (dcName dc) == "GHC.Types.C#")
        boxChar <$> setLocalEnv env (fromValueForce x)

      _ -> throwM (CannotConvert (Just value))

instance FromAst UChar where
  fromValue (stripValue -> value) =
    case value of
      VLiteral (CharLiteral c) -> pure (UChar c)
      _ -> throwM (CannotConvert (Just value))

instance FromAst Integer where
  fromValue (stripValue -> value) =
    case value of
      VLiteral (IntegerLiteral x) -> pure x

      VData dc [Left x] env ->
        setLocalEnv env $
          case dcTag dc of
            1 -> do
              !(UInt (I# i)) <- fromValueForce x
#if MIN_VERSION_base(4,15,0)
              pure (IS i)
#else
              pure (S# i)
#endif

            2 -> do
              !(UByteArray (ByteArray ba)) <- fromValueForce x
#if MIN_VERSION_base(4,15,0)
              pure (IP ba)
#else
              pure (Jp# (BN# ba))
#endif

            3 -> do
              !(UByteArray (ByteArray ba)) <- fromValueForce x
#if MIN_VERSION_base(4,15,0)
              pure (IN ba)
#else
              pure (Jn# (BN# ba))
#endif

            _ -> throwM (CannotConvert (Just value))

      _ -> throwM (CannotConvert (Just value))

instance FromAst Int where
  fromValue (stripValue -> value) =
    case value of
      VNeutral (NePrim pr [Left x]) -> do
        guard (primName pr == "GHC.Types.I#")
        boxInt <$> fromValueForce x

      VData dc [Left x] env -> do
        guard (nameOcc (dcName dc) == "GHC.Types.I#")
        boxInt <$> setLocalEnv env (fromValueForce x)

      _ -> throwM (CannotConvert (Just value))

instance FromAst UInt where
  fromValue (stripValue -> value) =
    case value of
      VLiteral (IntLiteral x) -> pure (UInt (fromInteger x))
      _ -> throwM (CannotConvert (Just value))

instance FromAst Natural where
  fromValue (stripValue -> value) =
    case value of
      VLiteral (NaturalLiteral x) ->
        pure (fromInteger x)

      VData dc [Left x] env ->
        setLocalEnv env $
          case dcTag dc of
            1 -> do
              !(UWord (W# i)) <- fromValueForce x
#if MIN_VERSION_base(4,15,0)
              pure (NS i)
#else
              pure (NatS# i)
#endif

            2 -> do
              !(UByteArray (ByteArray ba)) <- fromValueForce x
#if MIN_VERSION_base(4,15,0)
              pure (NB ba)
#else
              pure (NatJ# (BN# ba))
#endif

            _ -> throwM (CannotConvert (Just value))

      _ -> throwM (CannotConvert (Just value))

instance FromAst Word where
  fromValue (stripValue -> value) =
    case value of
      VNeutral (NePrim pr [Left x]) -> do
        guard (primName pr == "GHC.Types.W#")
        boxWord <$> fromValueForce x

      VData dc [Left x] env -> do
        guard (nameOcc (dcName dc) == "GHC.Types.W#")
        boxWord <$> setLocalEnv env (fromValueForce x)

      _ -> throwM (CannotConvert (Just value))

instance FromAst UWord where
  fromValue (stripValue -> value) =
    case value of
      VLiteral (WordLiteral x) -> pure (UWord (fromInteger x))
      _ -> throwM (CannotConvert (Just value))

instance FromAst Float where
  fromValue (stripValue -> value) =
    case value of
      VNeutral (NePrim pr [Left x]) -> do
        guard (primName pr == "GHC.Types.F#")
        boxFloat <$> fromValueForce x

      VData dc [Left x] env -> do
        guard (nameOcc (dcName dc) == "GHC.Types.F#")
        boxFloat <$> setLocalEnv env (fromValueForce x)

      _ -> throwM (CannotConvert (Just value))

instance FromAst UFloat where
  fromValue (stripValue -> value) =
    case value of
      VLiteral (FloatLiteral x) -> pure (UFloat (wordToFloat x))
      _ -> throwM (CannotConvert (Just value))

instance FromAst Double where
  fromValue (stripValue -> value) =
    case value of
      VNeutral (NePrim pr [Left x]) -> do
        guard (primName pr == "GHC.Types.D#")
        boxDouble <$> fromValueForce x

      VData dc [Left x] env -> do
        guard (nameOcc (dcName dc) == "GHC.Types.D#")
        boxDouble <$> setLocalEnv env (fromValueForce x)

      _ -> throwM (CannotConvert (Just value))

instance FromAst UDouble where
  fromValue (stripValue -> value) =
    case value of
      VLiteral (DoubleLiteral x) -> pure (UDouble (wordToDouble x))
      _ -> throwM (CannotConvert (Just value))

instance FromAst String where
  fromValue (stripValue -> value) =
    case value of
      VLiteral (StringLiteral x) -> pure x
      VNeutral (NePrim pr [Left s]) -> do
        guard (primName pr == "GHC.CString.unpackCString#")
        fromValueForce s

      _ -> throwM (CannotConvert (Just value))

instance FromAst (SNat n) where
  fromValue (stripValue -> value) =
    case value of
      VData dc [Right n, Left knN] _ -> do
        guard (nameOcc (dcName dc) == "Clash.Promoted.Nat.SNat")
        szN <- typeSize n (Just knN)

        case someNatVal szN of
          Just (SomeNat pN) -> pure (unsafeCoerce (snatProxy pN))
          _ -> throwM (CannotConvert (Just value))

      _ -> throwM (CannotConvert (Just value))

instance FromAst Bit where
  fromValue (stripValue -> value) =
    case value of
      VNeutral (NePrim pr [Left m, Left i]) -> do
        guard (primName pr == "Clash.Sized.Internal.BitVector.fromInteger##")
        !(UWord (W# mask)) <- fromValueForce m
        !int <- fromValueForce i

        pure (BV.fromInteger## mask int)

      _ -> throwM (CannotConvert (Just value))

instance FromAst (BitVector n) where
  fromValue (stripValue -> value) =
    case value of
      VNeutral (NePrim pr [Right n, Left knN, Left m, Left i]) -> do
        guard (primName pr == "Clash.Sized.Internal.BitVector.fromInteger#")
        szN <- typeSize n (Just knN)

        reifyNat szN (\pN -> unsafeCoerce (go pN m i))

      _ -> throwM (CannotConvert (Just value))
   where
    go :: forall m. (KnownNat m)
       => Proxy m -> Value -> Value -> Eval (BitVector m)
    go Proxy m i = BV.fromInteger# <$> fromValueForce m <*> fromValueForce i

instance FromAst (Index n) where
  fromValue (stripValue -> value) =
    case value of
      VNeutral (NePrim pr [Right n, Left knN, Left i]) -> do
        guard (primName pr == "Clash.Sized.Internal.Index.fromInteger#")
        szN <- typeSize n (Just knN)
        reifyNat szN (\pN -> unsafeCoerce (go pN i))

      _ -> throwM (CannotConvert (Just value))
   where
    go :: forall m. (KnownNat m) => Proxy m -> Value -> Eval (Index m)
    go Proxy i = do
      a <- fromValueForce i

      case maybeIsX (I.fromInteger# a) of
        Just r  -> pure r
        Nothing -> throwM ResultUndefined

instance FromAst (Signed n) where
  fromValue (stripValue -> value) =
    case value of
      VNeutral (NePrim pr [Right n, Left knN, Left i]) -> do
        guard (primName pr == "Clash.Sized.Internal.Signed.fromInteger#")
        szN <- typeSize n (Just knN)
        reifyNat szN (\pN -> unsafeCoerce (go pN i))

      _ -> throwM (CannotConvert (Just value))
   where
    go :: forall m. (KnownNat m) => Proxy m -> Value -> Eval (Signed m)
    go Proxy i = S.fromInteger# <$> fromValueForce i

instance FromAst (Unsigned n) where
  fromValue (stripValue -> value) =
    case value of
      VNeutral (NePrim pr [Right n, Left knN, Left i]) -> do
        guard (primName pr == "Clash.Sized.Internal.Unsigned.fromInteger#")
        szN <- typeSize n (Just knN)
        reifyNat szN (\pN -> unsafeCoerce (go pN i))

      _ -> throwM (CannotConvert (Just value))
   where
    go :: forall m. (KnownNat m) => Proxy m -> Value -> Eval (Unsigned m)
    go Proxy i = U.fromInteger# <$> fromValueForce i

instance (FromAst a) => FromAst (Vec n a) where
  fromValue (stripValue -> value) =
    case value of
      VData _ args _
        |  [Right _, Right _, Left _] <- args
        -> pure (unsafeCoerce Nil)

        |  [Right n, Right _, Right _, Left _, Left x, Left y] <- args
        -> do szN <- typeSize n Nothing
              reifyNat szN (\pN -> unsafeCoerce <$> go pN x y)

      _ -> throwM (CannotConvert (Just value))
   where
    go :: forall m. (KnownNat m)
       => Proxy m -> Value -> Value -> Eval (Vec m a)
    go Proxy x y = do
      a  <- fromValueForce @a x
      as <- fromValueForce @(Vec (m - 1) a) y

      pure (unsafeCoerce (Cons a as))

instance (FromAst a) => FromAst (Ref a) where
  fromValue (stripValue -> value) =
    case value of
      VNeutral (NePrim pr [Right _, Left i]) -> do
        guard (primName pr == "Clash.Transformations.ref")
        !(UInt addr) <- fromValueForce i
        refVal <- getRef addr >>= fromValueForce

        pure (Ref (Just addr) refVal)

      _ -> throwM (CannotConvert (Just value))
