{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Clash.GHC.PartialEval.Internal
  ( PrimEval
  , runPrimEval
  , PrimImpl
  , liftId
  , liftNullary
  , liftUnary
  , liftBinary
  , liftBox
  , withRecovery
  , withExceptions
  , typeInfo
  , typeArgsDcs
  , typeDcs
  , typeSize
  , resultType
  , UByteArray(..)
  , UChar(..)
  , UInt(..)
  , UWord(..)
  , UFloat(..)
  , UDouble(..)
  , UTuple2(..)
  , UTuple4(..)
  , LVec(..)
  , Ref(..)
  , fromTermOrValue
  , FromAst(..)
  , ToAst(..)

    -- Re-exported
  , Alternative(..)
  , lift
  ) where

import Control.Applicative (Alternative(..))
import Control.Category ((>>>))
import Control.Exception
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (runExcept)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Bitraversable (bitraverse)
import Data.Either (lefts)
import Data.Foldable (asum)
import Data.Primitive.ByteArray (ByteArray(..))
import Data.Proxy
import Data.Reflection (reifyNat)
import Data.Text (Text)
import GHC.Int
import GHC.Integer.GMP.Internals (Integer(..), BigNat(..))
import GHC.Natural (Natural(..), naturalFromInteger, naturalToInteger)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownNat)
import GHC.Types hiding (Type)
import GHC.Word
import System.IO.Unsafe (unsafeDupablePerformIO)
import Unsafe.Coerce

import PrelNames
import Unique (getKey)

import Clash.Sized.Internal.BitVector as BV
import Clash.Sized.Internal.Index as I
import Clash.Sized.Internal.Signed as S
import Clash.Sized.Internal.Unsigned as U
import Clash.Sized.Vector as V

import Clash.Core.DataCon
import Clash.Core.Evaluator.Models
import Clash.Core.Evaluator.Semantics
import Clash.Core.Literal
import Clash.Core.Term
import Clash.Core.TermInfo (applyTypeToArgs)
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Core.TysPrim
import Clash.Core.Util (dataConInstArgTys, primCo, tyNatSize)
import Clash.Unique

type PrimEval = MaybeT Eval
type PrimImpl = Evaluator -> PrimInfo -> [Either Term Type] -> PrimEval Value

-- | Evaluate a primitive to obtain some value. If the primitive cannot be
-- evaluated, the supplied action is used to generate a default value.
--
runPrimEval :: (Show a) => PrimEval a -> Eval a -> Eval a
runPrimEval (MaybeT m) x = m >>= maybe x pure

-- | The primitive is already a value, and is repackaged as NePrim after its
-- arguments are evaluated.
--
liftId :: PrimImpl
liftId e p args = do
  vals <- lift $ traverse (bitraverse (evaluateWhnf e) evaluateType) args
  pure (VNeu (NePrim p vals))

liftNullary :: (ToAst a) => a -> PrimImpl
liftNullary x _e p args = do
  resTy <- resultType p args
  toValue x resTy

-- | Lift a unary function to an implemenatation for a primitive operation.
-- This is used for primitives where the evaluation does not differ from the
-- semantics of the underlying Haskell implemenatation.
--
liftUnary :: (FromAst a, ToAst b) => (a -> b) -> PrimImpl
liftUnary f e p args
  | [x] <- lefts args
  = do a <- fromTermOrValue e x
       resTy <- resultType p args
       toValue (f a) resTy

  | otherwise
  = empty

-- | Lift a binary function to an implementation for a primitive operation.
-- See liftUnary for more information.
--
liftBinary
  :: (FromAst a, FromAst b, ToAst c)
  => (a -> b -> c)
  -> PrimImpl
liftBinary f e p args
  | [x, y] <- lefts args
  = do a <- fromTermOrValue e x
       b <- fromTermOrValue e y
       resTy <- resultType p args
       toValue (f a b) resTy

  | otherwise
  = empty

-- | Lift a constructor for a boxed type, e.g. I# for Int. Attempting to use
-- this function on other constructors may fail as it expects a unary
-- constructor.
--
liftBox :: PrimImpl
liftBox _ p args
  | [Left x] <- args
  = do env <- lift getLocalEnv
       [boxDc] <- typeDcs (primType p)
       pure (VData boxDc [Left x] env)

  | otherwise
  = empty

withRecovery :: a -> a -> a
withRecovery def x =
  unsafeDupablePerformIO $ catch (evaluate x)
    (\(_ :: SomeException) -> pure def)

withExceptions :: PrimImpl -> PrimImpl
withExceptions act e p args =
  unsafeDupablePerformIO $
    catch (evaluate $ act e p args)
      (\(_e :: SomeException) -> pure (liftId e p args))

-- | Type constructor name, args, data constructors.
--
typeInfo :: (HasCallStack) => Type -> PrimEval (TyConName, [Type], [DataCon])
typeInfo ty
  | TyConApp tcNm args <- tyView $ snd (splitFunForallTy ty)
  = do tcm <- lift getTyConMap
       tc <- MaybeT $ pure (lookupUniqMap tcNm tcm)

       pure (tcNm, args, tyConDataCons tc)

  | otherwise
  = error "typeInfo: Not a TyConApp"

typeArgsDcs :: Type -> PrimEval ([Type], [DataCon])
typeArgsDcs = fmap (\(_, tys, dcs) -> (tys, dcs)) . typeInfo

typeDcs :: Type -> PrimEval [DataCon]
typeDcs = fmap (\(_, _, dcs) -> dcs) . typeInfo

-- | Get the size of a type, e.g. 4 for BitVector 4. The optional term argument
-- is for a KnownNat constraint for the type, it can be more effective to
-- extract the size from this if it is known.
--
-- TODO use evaluateType (if still necessary)
--
typeSize :: Type -> Maybe Term -> PrimEval Integer
typeSize ty = \case
  Just kn -> fromTerm kn <|> go ty
  Nothing -> go ty
 where
  go a = do
    normA <- lift (evaluateType a)
    tcm <- lift getTyConMap

    case tyView normA of
      -- Assume TyCons have only a single size parameter, meaning the first
      -- argument which we can get a size from is the value. If this stops
      -- being the case this branch must change.
      TyConApp _ args -> asum (fmap go args)
      FunTy _ b -> go b
      OtherType b -> either (const empty) pure $ runExcept (tyNatSize tcm b)

-- | Apply all arguments to a primitive to determine the exact result of the
-- operation. If information is needed about the specific result type for
-- a primitive call and primType is returned, functions in ToAst may fail.
--
resultType :: PrimInfo -> [Either Term Type] -> PrimEval Type
resultType p args = do
  tcm <- lift getTyConMap
  pure (applyTypeToArgs (Prim p) tcm (primType p) args)

-- Newtypes for when something is expected to have an unboxed representation.
-- Needed beause we can't have a class instance on unboxed data.

newtype UByteArray = UByteArray { boxByteArray :: ByteArray }
newtype UChar = UChar { boxChar :: Char }
newtype UInt = UInt { boxInt :: Int }
newtype UWord = UWord {boxWord :: Word }
newtype UFloat = UFloat { boxFloat :: Float }
newtype UDouble = UDouble { boxDouble :: Double }
newtype UTuple2 a b = UTuple2 { boxTuple2 :: (a, b) }
newtype UTuple4 a b c d = UTuple4 { boxTuple4 :: (a, b, c, d) }

data Ref a = Ref { refAddr :: Maybe Int, refValue :: a }

data LVec a
  = LNil
  | LCons a Term

-- | Attempt to inspect an argument, evaluating it if necessary.
--
fromTermOrValue :: (FromAst a) => Evaluator -> Term -> PrimEval a
fromTermOrValue e x = fromTerm x <|> (lift (evaluateWhnf e x) >>= fromValue)

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
  {-# MINIMAL fromTerm #-}

  fromTerm  :: Term -> PrimEval a

  fromValue :: Value -> PrimEval a
  fromValue = fromTerm . asTerm
  {-# INLINE fromValue #-}

instance FromAst Term where
  fromTerm = pure

instance FromAst BigNat where
  fromTerm = stripTicks >>> \case
    Data _dc `App` x -> do
      !(ByteArray ba) <- boxByteArray <$> fromTerm x
      pure (BN# ba)

    _ -> empty

instance FromAst Bool where
  fromTerm = stripTicks >>> \case
    Data dc
      | dcUniq dc == getKey falseDataConKey -> pure False
      | dcUniq dc == getKey trueDataConKey  -> pure True

    _ -> empty

instance FromAst UByteArray where
  fromTerm = stripTicks >>> \case
    Literal (ByteArrayLiteral ba) -> pure (UByteArray ba)
    _ -> empty

instance FromAst Char where
  fromTerm = stripTicks >>> \case
    Data dc `App` x
      |  dcUniq dc == getKey charDataConKey
      -> boxChar <$> fromTerm x

    _ -> empty

  fromValue = stripValueTicks >>> \case
    VData dc [Left x] _env
      |  dcUniq dc == getKey charDataConKey
      -> boxChar <$> fromTerm x

    VNeu (NePrim p [Left x])
      |  primName p == "GHC.Types.C#"
      -> boxChar <$> fromValue x

    _ -> empty

instance FromAst UChar where
  fromTerm = stripTicks >>> \case
    Literal (CharLiteral c) -> pure (UChar c)
    _ -> empty

instance FromAst Integer where
  fromTerm = stripTicks >>> \case
    Literal (IntegerLiteral x) -> pure x

    Data dc `App` x
      | dcTag dc == 1 -> do
          UInt (I# i) <- fromTerm x
          pure (S# i)

      | dcTag dc == 2 -> do
          UByteArray (ByteArray ba) <- fromTerm x
          pure (Jp# (BN# ba))

      | dcTag dc == 3 -> do
          UByteArray (ByteArray ba) <- fromTerm x
          pure (Jn# (BN# ba))

    _ -> empty

instance FromAst Int where
  fromTerm = stripTicks >>> \case
    Data dc `App` x
      |  dcUniq dc == getKey intDataConKey
      -> boxInt <$> fromTerm x

    _ -> empty

  fromValue = stripValueTicks >>> \case
    VData dc [Left x] _env
      |  dcUniq dc == getKey intDataConKey
      -> boxInt <$> fromTerm x

    VNeu (NePrim p [Left x])
      |  primName p == "GHC.Int.I#"
      -> boxInt <$> fromValue x

    _ -> empty

instance FromAst UInt where
  fromTerm = stripTicks >>> \case
    Literal (IntLiteral x) -> pure (UInt $ fromInteger x)
    _ -> empty

instance FromAst Natural where
  fromTerm = stripTicks >>> \case
    Literal (NaturalLiteral x) ->
      pure (naturalFromInteger x)

    Data dc `App` x
      | dcTag dc == 1 -> do
          UWord (W# i) <- fromTerm x
          pure (NatS# i)

      | dcTag dc == 2 -> do
          UByteArray (ByteArray ba) <- fromTerm x
          pure (NatJ# (BN# ba))

    _ -> empty

instance FromAst Word where
  fromTerm = stripTicks >>> \case
    Data dc `App` x
      |  dcUniq dc == getKey wordDataConKey
      -> boxWord <$> fromTerm x

    _ -> empty

  fromValue = stripValueTicks >>> \case
    VData dc [Left x] _env
      |  dcUniq dc == getKey wordDataConKey
      -> boxWord <$> fromTerm x

    VNeu (NePrim p [Left x])
      |  primName p == "GHC.Types.W#"
      -> boxWord <$> fromValue x

    _ -> empty

instance FromAst UWord where
  fromTerm = stripTicks >>> \case
    Literal (WordLiteral x) -> pure (UWord $ fromInteger x)
    _ -> empty

instance FromAst Float where
  fromTerm = stripTicks >>> \case
    Data dc `App` x
      |  dcUniq dc == getKey floatDataConKey
      -> boxFloat <$> fromTerm x

    _ -> empty

  fromValue = stripValueTicks >>> \case
    VData dc [Left x] _env
      |  dcUniq dc == getKey floatDataConKey
      -> boxFloat <$> fromTerm x

    VNeu (NePrim p [Left x])
      |  primName p == "GHC.Types.F#"
      -> boxFloat <$> fromValue x

    _ -> empty

instance FromAst UFloat where
  fromTerm = stripTicks >>> \case
    Literal (FloatLiteral x) -> pure (UFloat $ fromRational x)
    _ -> empty

instance FromAst Double where
  fromTerm = stripTicks >>> \case
    Data dc `App` x
      |  dcUniq dc == getKey doubleDataConKey
      -> boxDouble <$> fromTerm x

    _ -> empty

  fromValue = stripValueTicks >>> \case
    VData dc [Left x] _env
      |  dcUniq dc == getKey doubleDataConKey
      -> boxDouble <$> fromTerm x

    VNeu (NePrim p [Left x])
      |  primName p == "GHC.Types.D#"
      -> boxDouble <$> fromValue x

    _ -> empty

instance FromAst UDouble where
  fromTerm = stripTicks >>> \case
    Literal (DoubleLiteral x) -> pure (UDouble $ fromRational x)
    _ -> empty

instance FromAst Bit where
  fromTerm (stripTicks -> term)
    | Prim p `App` m `App` i <- term
    , primName p == "Clash.Sized.Internal.BitVector.fromInteger##"
    = do UWord (W# m') <- fromTerm m
         i' <- fromTerm i

         pure (fromInteger## m' i')

    | otherwise
    = empty

instance FromAst (BitVector n) where
  fromTerm (stripTicks -> term)
    | Prim p `TyApp` n `App` knN `App` m `App` i <- term
    , primName p == "Clash.Sized.Internal.BitVector.fromInteger#"
    = do sz <- typeSize n (Just knN)
         reifyNat sz (\proxy -> unsafeCoerce (go proxy m i))

    | otherwise
    = empty
   where
    go :: forall m. (KnownNat m) => Proxy m -> Term -> Term -> PrimEval (BitVector m)
    go Proxy x y = do
      res <- BV.fromInteger# <$> fromTerm x <*> fromTerm y
      pure res

instance FromAst (Index n) where
  fromTerm (stripTicks -> term)
    | Prim p `TyApp` n `App` knN `App` i <- term
    , primName p == "Clash.Sized.Internal.Index.fromInteger#"
    = do sz <- typeSize n (Just knN)
         reifyNat sz (\proxy -> unsafeCoerce (go proxy i))

    | otherwise
    = empty
   where
    go :: forall m. (KnownNat m) => Proxy m -> Term -> PrimEval (Index m)
    go Proxy x = I.fromInteger# <$> fromTerm x

instance FromAst (Signed n) where
  fromTerm (stripTicks -> term)
    | Prim p `TyApp` n `App` knN `App` i <- term
    , primName p == "Clash.Sized.Internal.Signed.fromInteger#"
    = do sz <- typeSize n (Just knN)
         reifyNat sz (\proxy -> unsafeCoerce (go proxy i))

    | otherwise
    = empty
   where
    go :: forall m. (KnownNat m) => Proxy m -> Term -> PrimEval (Signed m)
    go Proxy x = S.fromInteger# <$> fromTerm x

instance FromAst (Unsigned n) where
  fromTerm (stripTicks -> term)
    | Prim p `TyApp` n `App` knN `App` i <- term
    , primName p == "Clash.Sized.Internal.Unsigned.fromInteger#"
    = do sz <- typeSize n (Just knN)
         reifyNat sz (\proxy -> unsafeCoerce (go proxy i))

    | otherwise
    = empty
   where
    go :: forall m. (KnownNat m) => Proxy m -> Term -> PrimEval (Unsigned m)
    go Proxy x = U.fromInteger# <$> fromTerm x

instance (FromAst a) => FromAst (LVec a) where
  fromTerm (stripTicks -> term)
    | Data _dc `TyApp` _nTy `TyApp` _aTy `App` _co <- term
    = pure LNil

    | Data _dc `TyApp` _nTy `TyApp` _aTy `TyApp` _eTy `App` _co `App` x `App` y <- term
    = do a <- fromTerm x
         pure (LCons a y)

    | otherwise
    = empty

instance (FromAst a) => FromAst (Ref a) where
  fromTerm (stripTicks -> term)
    | Prim p `TyApp` _aTy `App` x <- term
    , primName p == "Clash.Transformations.ref"
    = do addr <- boxInt <$> fromTerm x
         bind <- lift (getPrimsIO addr)
         a <- fromTerm bind

         pure (Ref (Just addr) a)

    | otherwise
    = empty

-- | When evaluating a primitive, the result needs to be converted back into
-- a Value. When the result is a primtive or data constructor, the arguments
-- are created by using toTerm instead. See Clash.Core.Evaluator.Models.Value.
--
class ToAst a where
  {-# MINIMAL toValue #-}

  toTerm :: a -> Type -> PrimEval Term
  toTerm x = fmap asTerm . toValue x

  toValue :: a -> Type -> PrimEval Value

instance ToAst BigNat where
  toValue (BN# ba) ty = do
    env <- lift getLocalEnv
    [bnDc] <- typeDcs ty
    a <- toTerm (UByteArray (ByteArray ba)) byteArrayPrimTy

    pure (VData bnDc [Left a] env)

instance ToAst Bool where
  toValue b ty = do
    env <- lift getLocalEnv
    [falseDc, trueDc] <- typeDcs ty

    pure (VData (if b then trueDc else falseDc) [] env)

instance ToAst UByteArray where
  toValue = const . pure . VLit . ByteArrayLiteral . boxByteArray

mkPrimChar :: Type -> PrimEval PrimInfo
mkPrimChar ty = do
  (cNm, _, _) <- typeInfo ty

  pure $ PrimInfo
    { primName = "GHC.Types.C#"
    , primType = mkFunTy charPrimTy (mkTyConTy cNm)
    , primWorkInfo = WorkNever
    , primCoreId = Nothing
    }

instance ToAst Char where
  toValue x ty = do
    box <- lift isKeepLifted

    if box then do
      [charDc] <- typeDcs ty
      env <- lift getLocalEnv
      a <- toTerm (UChar x) charPrimTy
      pure (VData charDc [Left a] env)
    else do
      p <- mkPrimChar ty
      a <- toValue (UChar x) charPrimTy
      pure (VNeu (NePrim p [Left a]))

instance ToAst UChar where
  toValue = const . pure . VLit . CharLiteral . boxChar

instance ToAst Integer where
  toValue = const . pure . VLit . IntegerLiteral

-- I#, I8#, I16#, I32#, I64# :: Int# -> Int{,8,16,32,64}
mkPrimInt :: Text -> Type -> PrimEval PrimInfo
mkPrimInt nm ty = do
  (iNm, _, _) <- typeInfo ty
  let pTy = mkFunTy intPrimTy (mkTyConTy iNm)

  pure $ PrimInfo
    { primName = nm
    , primType = pTy
    , primWorkInfo = WorkNever
    , primCoreId = Nothing
    }

instance ToAst Int where
  toValue x ty = do
    box <- lift isKeepLifted

    if box then do
      [intDc] <- typeDcs ty
      env <- lift getLocalEnv
      a <- toTerm (UInt x) intPrimTy
      pure (VData intDc [Left a] env)
    else do
      p <- mkPrimInt "GHC.Types.I#" ty
      a <- toValue (UInt x) intPrimTy
      pure (VNeu (NePrim p [Left a]))

instance ToAst UInt where
  toValue = const . pure . VLit . IntLiteral . toInteger . boxInt

instance ToAst Int8 where
  toValue !(I8# x) ty = do
    box <- lift isKeepLifted

    if box then do
      [int8Dc] <- typeDcs ty
      env <- lift getLocalEnv
      a <- toTerm (UInt (I# x)) intPrimTy
      pure (VData int8Dc [Left a] env)
    else do
      p <- mkPrimInt "GHC.Int.I8#" ty
      a <- toValue (UInt (I# x)) intPrimTy
      pure (VNeu (NePrim p [Left a]))

instance ToAst Int16 where
  toValue !(I16# x) ty = do
    box <- lift isKeepLifted

    if box then do
      [int16Dc] <- typeDcs ty
      env <- lift getLocalEnv
      a <- toTerm (UInt (I# x)) intPrimTy
      pure (VData int16Dc [Left a] env)
    else do
      p <- mkPrimInt "GHC.Int.I16#" ty
      a <- toValue (UInt (I# x)) intPrimTy
      pure (VNeu (NePrim p [Left a]))

instance ToAst Int32 where
  toValue !(I32# x) ty = do
    box <- lift isKeepLifted

    if box then do
      [int32Dc] <- typeDcs ty
      env <- lift getLocalEnv
      a <- toTerm (UInt (I# x)) intPrimTy
      pure (VData int32Dc [Left a] env)
    else do
      p <- mkPrimInt "GHC.Int.I32#" ty
      a <- toValue (UInt (I# x)) intPrimTy
      pure (VNeu (NePrim p [Left a]))

instance ToAst Int64 where
  toValue !(I64# x) ty = do
    box <- lift isKeepLifted

    if box then do
      [int64Dc] <- typeDcs ty
      env <- lift getLocalEnv
      a <- toTerm (UInt (I# x)) intPrimTy
      pure (VData int64Dc [Left a] env)
    else do
      p <- mkPrimInt "GHC.Int.I64#" ty
      a <- toValue (UInt (I# x)) intPrimTy
      pure (VNeu (NePrim p [Left a]))

instance ToAst Natural where
  toValue = const . pure . VLit . NaturalLiteral . naturalToInteger

-- W#, W8#, W16#, W32#, W64# :: Word# -> Word{,8,16,32,64}
mkPrimWord :: Text -> Type -> PrimEval PrimInfo
mkPrimWord nm ty = do
  (wNm, _, _) <- typeInfo ty
  let pTy = mkFunTy wordPrimTy (mkTyConTy wNm)

  pure $ PrimInfo
    { primName = nm
    , primType = pTy
    , primWorkInfo = WorkNever
    , primCoreId = Nothing
    }

instance ToAst Word where
  toValue x ty = do
    box <- lift isKeepLifted

    if box then do
      [wordDc] <- typeDcs ty
      env <- lift getLocalEnv
      a <- toTerm (UWord x) wordPrimTy
      pure (VData wordDc [Left a] env)
    else do
      p <- mkPrimWord "GHC.Types.W#" ty
      a <- toValue (UWord x) wordPrimTy
      pure (VNeu (NePrim p [Left a]))

instance ToAst UWord where
  toValue = const . pure . VLit . WordLiteral . toInteger . boxWord

instance ToAst Word8 where
  toValue !(W8# x) ty = do
    box <- lift isKeepLifted

    if box then do
      [word8Dc] <- typeDcs ty
      env <- lift getLocalEnv
      a <- toTerm (UWord (W# x)) wordPrimTy
      pure (VData word8Dc [Left a] env)
    else do
      p <- mkPrimWord "GHC.Word.W8#" ty
      a <- toValue (UWord (W# x)) wordPrimTy
      pure (VNeu (NePrim p [Left a]))

instance ToAst Word16 where
  toValue !(W16# x) ty = do
    box <- lift isKeepLifted

    if box then do
      [word16Dc] <- typeDcs ty
      env <- lift getLocalEnv
      a <- toTerm (UWord (W# x)) wordPrimTy
      pure (VData word16Dc [Left a] env)
    else do
      p <- mkPrimWord "GHC.Word.W16#" ty
      a <- toValue (UWord (W# x)) wordPrimTy
      pure (VNeu (NePrim p [Left a]))

instance ToAst Word32 where
  toValue !(W32# x) ty = do
    box <- lift isKeepLifted

    if box then do
      [word32Dc] <- typeDcs ty
      env <- lift getLocalEnv
      a <- toTerm (UWord (W# x)) wordPrimTy
      pure (VData word32Dc [Left a] env)
    else do
      p <- mkPrimWord "GHC.Word.W32#" ty
      a <- toValue (UWord (W# x)) wordPrimTy
      pure (VNeu (NePrim p [Left a]))

instance ToAst Word64 where
  toValue !(W64# x) ty = do
    box <- lift isKeepLifted

    if box then do
      [word64Dc] <- typeDcs ty
      env <- lift getLocalEnv
      a <- toTerm (UWord (W# x)) wordPrimTy
      pure (VData word64Dc [Left a] env)
    else do
      p <- mkPrimWord "GHC.Word.W64#" ty
      a <- toValue (UWord (W# x)) wordPrimTy
      pure (VNeu (NePrim p [Left a]))

mkPrimFloat :: Type -> PrimEval PrimInfo
mkPrimFloat ty = do
  (fNm, _, _) <- typeInfo ty

  pure $ PrimInfo
    { primName = "GHC.Types.F#"
    , primType = mkFunTy floatPrimTy (mkTyConTy fNm)
    , primWorkInfo = WorkNever
    , primCoreId = Nothing
    }

instance ToAst Float where
  toValue x ty = do
    box <- lift isKeepLifted

    if box then do
      [floatDc] <- typeDcs ty
      env <- lift getLocalEnv
      a <- toTerm (UFloat x) floatPrimTy
      pure (VData floatDc [Left a] env)
    else do
      p <- mkPrimFloat ty
      a <- toValue (UFloat x) floatPrimTy
      pure (VNeu (NePrim p [Left a]))

instance ToAst UFloat where
  toValue = const . pure . VLit . FloatLiteral . toRational . boxFloat

mkPrimDouble :: Type -> PrimEval PrimInfo
mkPrimDouble ty = do
  (dNm, _, _) <- typeInfo ty

  pure $ PrimInfo
    { primName = "GHC.Types.D#"
    , primType = mkFunTy doublePrimTy (mkTyConTy dNm)
    , primWorkInfo = WorkNever
    , primCoreId = Nothing
    }

instance ToAst Double where
  toValue x ty = do
    box <- lift isKeepLifted

    if box then do
      [doubleDc] <- typeDcs ty
      env <- lift getLocalEnv
      a <- toTerm (UDouble x) doublePrimTy
      pure (VData doubleDc [Left a] env)
    else do
      p <- mkPrimDouble ty
      a <- toValue (UDouble x) doublePrimTy
      pure (VNeu (NePrim p [Left a]))

instance ToAst UDouble where
  toValue = const . pure . VLit . DoubleLiteral . toRational . boxDouble

instance ToAst Ordering where
  toValue x ty = do
    env <- lift getLocalEnv
    [ltDc, eqDc, gtDc] <- typeDcs ty

    case x of
      LT -> pure (VData ltDc [] env)
      EQ -> pure (VData eqDc [] env)
      GT -> pure (VData gtDc [] env)

instance ToAst Term where
  toTerm x = const (pure x)
  toValue = error ("Term.toValue: Operation not supported")

instance ToAst Value where
  toValue = const . pure

instance (ToAst a, ToAst b) => ToAst (a, b) where
  toValue (a, b) ty = do
    env <- lift getLocalEnv
    ([aTy, bTy], [tupDc]) <- typeArgsDcs ty
    a' <- toTerm a aTy
    b' <- toTerm b bTy

    pure (VData tupDc [Right aTy, Right bTy, Left a', Left b'] env)

instance (ToAst a, ToAst b) => ToAst (UTuple2 a b) where
  toValue (UTuple2 (a, b)) ty = do
    env <- lift getLocalEnv
    ([aRep, bRep, aTy, bTy], [tupDc]) <- typeArgsDcs ty
    a' <- toTerm a aTy
    b' <- toTerm b bTy

    pure (VData tupDc
      [Right aRep, Right bRep, Right aTy, Right bTy, Left a', Left b'] env)

instance (ToAst a, ToAst b, ToAst c, ToAst d) => ToAst (a, b, c, d) where
  toValue (a, b, c, d) ty = do
    env <- lift getLocalEnv
    ([aTy, bTy, cTy, dTy], [tupDc]) <- typeArgsDcs ty
    a' <- toTerm a aTy
    b' <- toTerm b bTy
    c' <- toTerm c cTy
    d' <- toTerm d dTy

    pure $ flip (VData tupDc) env
      [ Right aTy, Right bTy, Right cTy, Right dTy
      , Left a', Left b', Left c', Left d'
      ]

instance (ToAst a, ToAst b, ToAst c, ToAst d) => ToAst (UTuple4 a b c d) where
  toValue (UTuple4 (a, b, c, d)) ty = do
    env <- lift getLocalEnv
    ([aRep, bRep, cRep, dRep, aTy, bTy, cTy, dTy], [tupDc]) <- typeArgsDcs ty
    a' <- toTerm a aTy
    b' <- toTerm b bTy
    c' <- toTerm c cTy
    d' <- toTerm d dTy

    pure $ VData tupDc
      [ Right aRep, Right bRep, Right cRep, Right dRep
      , Right aTy, Right bTy, Right cTy, Right dTy
      , Left a', Left b', Left c', Left d'
      ] env

-- fromInteger## :: Word# -> Integer -> Bit
mkPrimBit :: Type -> PrimEval PrimInfo
mkPrimBit ty = do
  (bNm, args, _) <- typeInfo ty
  let pTy = mkPolyFunTy (mkTyConApp bNm args) [Right wordPrimTy, Right integerPrimTy]

  pure $ PrimInfo
    { primName = "Clash.Sized.Internal.BitVector.fromInteger##"
    , primType = pTy
    , primWorkInfo = WorkNever
    , primCoreId = Nothing
    }

instance ToAst Bit where
  toValue (Bit m i) ty = do
    env <- lift getLocalEnv
    p <- mkPrimBit ty
    m' <- toTerm (UWord m) wordPrimTy
    i' <- toTerm (toInteger i) integerPrimTy

    pure (VPrim p [Left m', Left i'] env)

-- fromInteger# :: forall n. KnownNat n => Natural -> Integer -> BitVector n
mkPrimBV :: Type -> PrimEval PrimInfo
mkPrimBV ty = do
  (bvNm, _, _) <- typeInfo ty
  nTv <- lift (mkUniqueTyVar "n" typeNatKind)

  let pTy = mkPolyFunTy (mkTyConApp bvNm [VarTy nTv])
              [Left nTv, Right naturalPrimTy, Right naturalPrimTy, Right integerPrimTy]

  pure $ PrimInfo
    { primName = "Clash.Sized.Internal.BitVector.fromInteger#"
    , primType = pTy
    , primWorkInfo = WorkNever
    , primCoreId = Nothing
    }

instance ToAst (BitVector n) where
  toValue (BV m i) ty = do
    env <- lift getLocalEnv
    p <- mkPrimBV ty
    n <- typeSize ty Nothing
    m' <- toTerm m naturalPrimTy
    i' <- toTerm (naturalToInteger i) integerPrimTy

    pure (VPrim p [Right (LitTy (NumTy n)), Left (Literal (NaturalLiteral n)), Left m', Left i'] env)

-- fromInteger# :: forall n. Knownnat n => Integer -> Index n
mkPrimIndex :: Type -> PrimEval PrimInfo
mkPrimIndex ty = do
  (iNm, _, _) <- typeInfo ty
  nTy <- lift (mkUniqueTyVar "n" typeNatKind)
  let pTy = mkPolyFunTy (mkTyConApp iNm [VarTy nTy])
              [Left nTy, Right naturalPrimTy, Right integerPrimTy]

  pure $ PrimInfo
    { primName = "Clash.Sized.Internal.Index.fromInteger#"
    , primType = pTy
    , primWorkInfo = WorkNever
    , primCoreId = Nothing
    }

instance ToAst (Index n) where
  toValue (I i) ty = do
    env <- lift getLocalEnv
    p <- mkPrimIndex ty
    n <- typeSize ty Nothing
    i' <- toTerm i integerPrimTy

    pure $ VPrim p [Right (LitTy (NumTy n)), Left (Literal (NaturalLiteral n)), Left i'] env

-- fromInteger# :: forall n. Knownnat n => Integer -> Signed n
mkPrimSigned :: Type -> PrimEval PrimInfo
mkPrimSigned ty = do
  (sNm, _, _) <- typeInfo ty
  nTy <- lift (mkUniqueTyVar "n" typeNatKind)
  let pTy = mkPolyFunTy (mkTyConApp sNm [VarTy nTy])
              [Left nTy, Right naturalPrimTy, Right integerPrimTy]

  pure $ PrimInfo
    { primName = "Clash.Sized.Internal.Signed.fromInteger#"
    , primType = pTy
    , primWorkInfo = WorkNever
    , primCoreId = Nothing
    }

instance ToAst (Signed n) where
  toValue (S i) ty = do
    env <- lift getLocalEnv
    p <- mkPrimSigned ty
    n <- typeSize ty Nothing
    i' <- toTerm i integerPrimTy

    pure $ VPrim p [Right (LitTy (NumTy n)), Left (Literal (NaturalLiteral n)), Left i'] env

-- fromInteger# :: forall n. Knownnat n => Integer -> Unsigned n
mkPrimUnsigned :: Type -> PrimEval PrimInfo
mkPrimUnsigned ty = do
  (uNm, _, _) <- typeInfo ty
  nTy <- lift (mkUniqueTyVar "n" typeNatKind)
  let pTy = mkPolyFunTy (mkTyConApp uNm [VarTy nTy])
              [Left nTy, Right naturalPrimTy, Right integerPrimTy]

  pure $ PrimInfo
    { primName = "Clash.Sized.Internal.Unsigned.fromInteger#"
    , primType = pTy
    , primWorkInfo = WorkNever
    , primCoreId = Nothing
    }

instance ToAst (Unsigned n) where
  toValue (U i) ty = do
    env <- lift getLocalEnv
    p <- mkPrimUnsigned ty
    n <- typeSize ty Nothing
    i' <- toTerm i integerPrimTy

    pure $ VPrim p [Right (LitTy (NumTy n)), Left (Literal (NaturalLiteral n)), Left i'] env

instance (ToAst a) => ToAst (Vec n a) where
  toValue vec ty = do
    env <- lift getLocalEnv
    ([nTy, aTy], [nilDc, consDc]) <- typeArgsDcs ty

    case vec of
      Nil -> do
        [coTy] <- MaybeT (pure $ dataConInstArgTys nilDc [nTy, aTy])
        let co = primCo coTy

        pure (VData nilDc [Right nTy, Right aTy, Left co] env)

      Cons a as -> do
        sz <- typeSize nTy Nothing
        let pnTy = LitTy (NumTy (sz - 1))
        [coTy, _, vecTy] <- MaybeT (pure $ dataConInstArgTys consDc [nTy, aTy, pnTy])
        let co = primCo coTy

        x <- toTerm a aTy
        xs <- toTerm as vecTy
        pure (VData consDc [Right nTy, Right aTy, Right pnTy, Left co, Left x, Left xs] env)

instance (ToAst a) => ToAst (LVec a) where
  toValue vec ty = do
    env <- lift getLocalEnv
    ([nTy, aTy], [nilDc, consDc]) <- typeArgsDcs ty

    case vec of
      LNil -> do
        [coTy] <- MaybeT (pure $ dataConInstArgTys nilDc [nTy, aTy])
        let co = primCo coTy
        pure (VData nilDc [Right nTy, Right aTy, Left co] env)

      LCons a xs -> do
        sz <- typeSize nTy Nothing
        let pnTy = LitTy (NumTy (sz - 1))
        [coTy, _, _] <- MaybeT (pure $ dataConInstArgTys consDc [nTy, aTy, pnTy])
        let co = primCo coTy
        x <- toTerm a aTy

        pure $ flip (VData consDc) env
          [ Right nTy, Right aTy, Right pnTy
          , Left co, Left x, Left xs
          ]

-- ref :: forall a. Int# -> a
mkPrimRef :: PrimEval PrimInfo
mkPrimRef = do
  aTv <- lift (mkUniqueTyVar "a" liftedTypeKind)
  let pTy = mkPolyFunTy (VarTy aTv) [Left aTv, Right intPrimTy]

  pure $ PrimInfo
    { primName = "Clash.Transformations.ref"
    , primType = pTy
    , primWorkInfo = WorkNever
    , primCoreId = Nothing
    }

instance (ToAst a) => ToAst (Ref a) where
  toValue (Ref ma x) ty = do
    env <- lift getLocalEnv
    p <- mkPrimRef
    tm <- toTerm x ty

    addr <- case ma of
      Just addr -> do
        lift (updatePrimsIO addr tm)
        pure addr

      Nothing -> lift (insertPrimsIO tm)

    let addrLit = Literal (IntLiteral (toInteger addr))
    pure $ VPrim p [Right ty, Left addrLit] env

