{-|
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.Primitive.ToAst where

import Control.Applicative (Alternative(..))
import Data.Binary.IEEE754 (doubleToWord, floatToWord)
import Data.Text (Text)
import GHC.Int

#if !MIN_VERSION_base(4,15,0)
import Data.Primitive.ByteArray (ByteArray(..))
import GHC.Integer.GMP.Internals (BigNat(..))
#endif

import GHC.Natural (Natural(..))
import GHC.Stack (HasCallStack)
import GHC.Word

import Clash.Promoted.Nat
import Clash.Sized.Internal.BitVector as BV
import Clash.Sized.Internal.Index as I
import Clash.Sized.Internal.Signed as S
import Clash.Sized.Internal.Unsigned as U
import Clash.Sized.Vector as V

import Clash.Core.Literal
import Clash.Core.PartialEval.Monad
import Clash.Core.PartialEval.NormalForm
import Clash.Core.Term (PrimInfo(..), PrimUnfolding(..), IsMultiPrim(..), WorkInfo(..))
import Clash.Core.Type
import Clash.Core.TysPrim
import Clash.Core.Util

import Clash.GHC.PartialEval.Primitive.Info
import Clash.GHC.PartialEval.Primitive.Unboxed

-- | When evaluating a primitive, the result needs to be converted back into
-- a Value. When the result is a primtive or data constructor, the arguments
-- are created by using toValue instead. See Clash.Core.Evaluator.Models.Value.
--
class ToAst a where
  toValue :: (HasCallStack) => a -> Type -> Eval Value

#if !MIN_VERSION_base(4,15,0)
instance ToAst BigNat where
  toValue (BN# ba) ty = do
    env <- getLocalEnv
    [bnDc] <- resultDataCons ty
    a <- toValue (UByteArray (ByteArray ba)) byteArrayPrimTy

    pure (VData bnDc [Left a] env)
#endif

instance ToAst Bool where
  toValue b ty = do
    env <- getLocalEnv
    [falseDc, trueDc] <- resultDataCons ty

    pure (VData (if b then trueDc else falseDc) [] env)

instance ToAst UByteArray where
  toValue = const . pure . VLiteral . ByteArrayLiteral . boxByteArray

mkPrimChar :: Type -> Eval PrimInfo
mkPrimChar ty = do
  (cNm, []) <- resultTyCon ty

  pure $ PrimInfo
    { primName = "GHC.Types.C#"
    , primType = mkFunTy charPrimTy (mkTyConTy cNm)
    , primWorkInfo = WorkNever
    , primMultiResult = SingleResult
    , primUnfolding = NoUnfolding
    }

instance ToAst Char where
  toValue x ty = do
    p <- mkPrimChar ty
    a <- toValue (UChar x) charPrimTy
    pure (VNeutral (NePrim p [Left a]))

instance ToAst UChar where
  toValue = const . pure . VLiteral . CharLiteral . boxChar

instance ToAst Integer where
  toValue = const . pure . VLiteral . IntegerLiteral

-- I#, I8#, I16#, I32#, I64# :: Int# -> Int{,8,16,32,64}
mkPrimInt :: Text -> Type -> Eval PrimInfo
mkPrimInt nm ty = do
  (iNm, []) <- resultTyCon ty
  let pTy = mkFunTy intPrimTy (mkTyConTy iNm)

  pure $ PrimInfo
    { primName = nm
    , primType = pTy
    , primWorkInfo = WorkNever
    , primMultiResult = SingleResult
    , primUnfolding = NoUnfolding
    }

instance ToAst Int where
  toValue x ty = do
    p <- mkPrimInt "GHC.Types.I#" ty
    a <- toValue (UInt x) intPrimTy
    pure (VNeutral (NePrim p [Left a]))

instance ToAst UInt where
  toValue = const . pure . VLiteral . IntLiteral . toInteger . boxInt

instance ToAst Int8 where
  toValue !(I8# x) ty = do
    p <- mkPrimInt "GHC.Int.I8#" ty
    a <- toValue (UInt (I# x)) intPrimTy
    pure (VNeutral (NePrim p [Left a]))

instance ToAst Int16 where
  toValue !(I16# x) ty = do
    p <- mkPrimInt "GHC.Int.I16#" ty
    a <- toValue (UInt (I# x)) intPrimTy
    pure (VNeutral (NePrim p [Left a]))

instance ToAst Int32 where
  toValue !(I32# x) ty = do
    p <- mkPrimInt "GHC.Int.I32#" ty
    a <- toValue (UInt (I# x)) intPrimTy
    pure (VNeutral (NePrim p [Left a]))

instance ToAst Int64 where
  toValue !(I64# x) ty = do
    p <- mkPrimInt "GHC.Int.I64#" ty
    a <- toValue (UInt (I# x)) intPrimTy
    pure (VNeutral (NePrim p [Left a]))

instance ToAst Natural where
  toValue = const . pure . VLiteral . NaturalLiteral . toInteger

-- W#, W8#, W16#, W32#, W64# :: Word# -> Word{,8,16,32,64}
mkPrimWord :: Text -> Type -> Eval PrimInfo
mkPrimWord nm ty = do
  (wNm, []) <- resultTyCon ty
  let pTy = mkFunTy wordPrimTy (mkTyConTy wNm)

  pure $ PrimInfo
    { primName = nm
    , primType = pTy
    , primWorkInfo = WorkNever
    , primMultiResult = SingleResult
    , primUnfolding = NoUnfolding
    }

instance ToAst Word where
  toValue x ty = do
    p <- mkPrimWord "GHC.Types.W#" ty
    a <- toValue (UWord x) wordPrimTy
    pure (VNeutral (NePrim p [Left a]))

instance ToAst UWord where
  toValue = const . pure . VLiteral . WordLiteral . toInteger . boxWord

instance ToAst Word8 where
  toValue !(W8# x) ty = do
    p <- mkPrimWord "GHC.Word.W8#" ty
    a <- toValue (UWord (W# x)) wordPrimTy
    pure (VNeutral (NePrim p [Left a]))

instance ToAst Word16 where
  toValue !(W16# x) ty = do
    p <- mkPrimWord "GHC.Word.W16#" ty
    a <- toValue (UWord (W# x)) wordPrimTy
    pure (VNeutral (NePrim p [Left a]))

instance ToAst Word32 where
  toValue !(W32# x) ty = do
    p <- mkPrimWord "GHC.Word.W32#" ty
    a <- toValue (UWord (W# x)) wordPrimTy
    pure (VNeutral (NePrim p [Left a]))

instance ToAst Word64 where
  toValue !(W64# x) ty = do
    p <- mkPrimWord "GHC.Word.W64#" ty
    a <- toValue (UWord (W# x)) wordPrimTy
    pure (VNeutral (NePrim p [Left a]))

mkPrimFloat :: Type -> Eval PrimInfo
mkPrimFloat ty = do
  (fNm, []) <- resultTyCon ty

  pure $ PrimInfo
    { primName = "GHC.Types.F#"
    , primType = mkFunTy floatPrimTy (mkTyConTy fNm)
    , primWorkInfo = WorkNever
    , primMultiResult = SingleResult
    , primUnfolding = NoUnfolding
    }

instance ToAst Float where
  toValue x ty = do
    p <- mkPrimFloat ty
    a <- toValue (UFloat x) floatPrimTy
    pure (VNeutral (NePrim p [Left a]))

instance ToAst UFloat where
  toValue = const . pure . VLiteral . FloatLiteral . floatToWord . boxFloat

mkPrimDouble :: Type -> Eval PrimInfo
mkPrimDouble ty = do
  (dNm, []) <- resultTyCon ty

  pure $ PrimInfo
    { primName = "GHC.Types.D#"
    , primType = mkFunTy doublePrimTy (mkTyConTy dNm)
    , primWorkInfo = WorkNever
    , primMultiResult = SingleResult
    , primUnfolding = NoUnfolding
    }

instance ToAst Double where
  toValue x ty = do
    p <- mkPrimDouble ty
    a <- toValue (UDouble x) doublePrimTy
    pure (VNeutral (NePrim p [Left a]))

instance ToAst UDouble where
  toValue = const . pure . VLiteral . DoubleLiteral . doubleToWord . boxDouble

instance ToAst Ordering where
  toValue x ty = do
    env <- getLocalEnv
    [ltDc, eqDc, gtDc] <- resultDataCons ty

    case x of
      LT -> pure (VData ltDc [] env)
      EQ -> pure (VData eqDc [] env)
      GT -> pure (VData gtDc [] env)

instance ToAst Value where
  toValue = const . pure

instance (ToAst a, ToAst b) => ToAst (a, b) where
  toValue (a, b) ty = do
    env <- getLocalEnv
    [aTy, bTy] <- snd <$> resultTyCon ty
    [tupDc] <- resultDataCons ty
    a' <- toValue a aTy
    b' <- toValue b bTy

    pure (VData tupDc [Right aTy, Right bTy, Left a', Left b'] env)

instance (ToAst a, ToAst b) => ToAst (UTuple2 a b) where
  toValue (UTuple2 (a, b)) ty = do
    env <- getLocalEnv
    [aRep, bRep, aTy, bTy] <- snd <$> resultTyCon ty
    [tupDc] <- resultDataCons ty
    a' <- toValue a aTy
    b' <- toValue b bTy

    pure (VData tupDc
      [Right aRep, Right bRep, Right aTy, Right bTy, Left a', Left b'] env)

instance (ToAst a, ToAst b, ToAst c, ToAst d) => ToAst (a, b, c, d) where
  toValue (a, b, c, d) ty = do
    env <- getLocalEnv
    [aTy, bTy, cTy, dTy] <- snd <$> resultTyCon ty
    [tupDc] <- resultDataCons ty
    a' <- toValue a aTy
    b' <- toValue b bTy
    c' <- toValue c cTy
    d' <- toValue d dTy

    pure $ flip (VData tupDc) env
      [ Right aTy, Right bTy, Right cTy, Right dTy
      , Left a', Left b', Left c', Left d'
      ]

instance (ToAst a, ToAst b, ToAst c, ToAst d) => ToAst (UTuple4 a b c d) where
  toValue (UTuple4 (a, b, c, d)) ty = do
    env <- getLocalEnv
    [aRep, bRep, cRep, dRep, aTy, bTy, cTy, dTy] <- snd <$> resultTyCon ty
    [tupDc] <- resultDataCons ty
    a' <- toValue a aTy
    b' <- toValue b bTy
    c' <- toValue c cTy
    d' <- toValue d dTy

    pure $ VData tupDc
      [ Right aRep, Right bRep, Right cRep, Right dRep
      , Right aTy, Right bTy, Right cTy, Right dTy
      , Left a', Left b', Left c', Left d'
      ] env

instance ToAst (SNat n) where
  toValue snat ty = do
    let i = snatToInteger snat
    env <- getLocalEnv
    [snatDc] <- resultDataCons ty

    pure (VData snatDc
      [Right (LitTy (NumTy i)), Left (VLiteral (NaturalLiteral i))] env)

-- fromInteger## :: Word# -> Integer -> Bit
mkPrimBit :: Type -> Eval PrimInfo
mkPrimBit ty = do
  (bNm, args) <- resultTyCon ty
  let pTy = mkPolyFunTy (mkTyConApp bNm args) [Right wordPrimTy, Right integerPrimTy]

  pure $ PrimInfo
    { primName = "Clash.Sized.Internal.BitVector.fromInteger##"
    , primType = pTy
    , primWorkInfo = WorkNever
    , primMultiResult = SingleResult
    , primUnfolding = NoUnfolding
    }

instance ToAst Bit where
  toValue (Bit m i) ty = do
    pr <- mkPrimBit ty
    m' <- toValue (UWord m) wordPrimTy
    i' <- toValue (toInteger i) integerPrimTy

    pure (VNeutral (NePrim pr [Left m', Left i']))

-- fromInteger# :: forall n. KnownNat n => Natural -> Integer -> BitVector n
mkPrimBV :: Type -> Eval PrimInfo
mkPrimBV ty = do
  bvNm <- fst <$> resultTyCon ty
  nTv <- getUniqueTyVar "n" typeNatKind

  let pTy = mkPolyFunTy (mkTyConApp bvNm [VarTy nTv])
              [Left nTv, Right naturalPrimTy, Right naturalPrimTy, Right integerPrimTy]

  pure $ PrimInfo
    { primName = "Clash.Sized.Internal.BitVector.fromInteger#"
    , primType = pTy
    , primWorkInfo = WorkNever
    , primMultiResult = SingleResult
    , primUnfolding = NoUnfolding
    }

instance ToAst (BitVector n) where
  toValue (BV m i) ty = do
    pr  <- mkPrimBV ty
    n   <- typeSize ty Nothing
    m'  <- toValue m naturalPrimTy
    i'  <- toValue (toInteger i) integerPrimTy

    pure . VNeutral $ NePrim pr
      [ Right (LitTy (NumTy n))
      , Left (VLiteral (NaturalLiteral n))
      , Left m'
      , Left i'
      ]

-- fromInteger# :: forall n. Knownnat n => Integer -> Index n
mkPrimIndex :: Type -> Eval PrimInfo
mkPrimIndex ty = do
  iNm <- fst <$> resultTyCon ty
  nTy <- getUniqueTyVar "n" typeNatKind
  let pTy = mkPolyFunTy (mkTyConApp iNm [VarTy nTy])
              [Left nTy, Right naturalPrimTy, Right integerPrimTy]

  pure $ PrimInfo
    { primName = "Clash.Sized.Internal.Index.fromInteger#"
    , primType = pTy
    , primWorkInfo = WorkNever
    , primMultiResult = SingleResult
    , primUnfolding = NoUnfolding
    }

instance ToAst (Index n) where
  toValue (I i) ty = do
    pr <- mkPrimIndex ty
    n  <- typeSize ty Nothing
    i' <- toValue i integerPrimTy

    pure . VNeutral $ NePrim pr
      [Right (LitTy (NumTy n)), Left (VLiteral (NaturalLiteral n)), Left i']

-- fromInteger# :: forall n. Knownnat n => Integer -> Signed n
mkPrimSigned :: Type -> Eval PrimInfo
mkPrimSigned ty = do
  sNm <- fst <$> resultTyCon ty
  nTy <- getUniqueTyVar "n" typeNatKind
  let pTy = mkPolyFunTy (mkTyConApp sNm [VarTy nTy])
              [Left nTy, Right naturalPrimTy, Right integerPrimTy]

  pure $ PrimInfo
    { primName = "Clash.Sized.Internal.Signed.fromInteger#"
    , primType = pTy
    , primWorkInfo = WorkNever
    , primMultiResult = SingleResult
    , primUnfolding = NoUnfolding
    }

instance ToAst (Signed n) where
  toValue (S i) ty = do
    pr <- mkPrimSigned ty
    n  <- typeSize ty Nothing
    i' <- toValue i integerPrimTy

    pure . VNeutral $ NePrim pr
      [Right (LitTy (NumTy n)), Left (VLiteral (NaturalLiteral n)), Left i']

-- fromInteger# :: forall n. Knownnat n => Integer -> Unsigned n
mkPrimUnsigned :: Type -> Eval PrimInfo
mkPrimUnsigned ty = do
  uNm <- fst <$> resultTyCon ty
  nTy <- getUniqueTyVar "n" typeNatKind
  let pTy = mkPolyFunTy (mkTyConApp uNm [VarTy nTy])
              [Left nTy, Right naturalPrimTy, Right integerPrimTy]

  pure $ PrimInfo
    { primName = "Clash.Sized.Internal.Unsigned.fromInteger#"
    , primType = pTy
    , primWorkInfo = WorkNever
    , primMultiResult = SingleResult
    , primUnfolding = NoUnfolding
    }

instance ToAst (Unsigned n) where
  toValue (U i) ty = do
    pr <- mkPrimUnsigned ty
    n  <- typeSize ty Nothing
    i' <- toValue (toInteger i) integerPrimTy

    pure . VNeutral $ NePrim pr
      [Right (LitTy (NumTy n)), Left (VLiteral (NaturalLiteral n)), Left i']

instance (ToAst a) => ToAst (Vec n a) where
  toValue vec ty = do
    env <- getLocalEnv
    [nTy, aTy] <- snd <$> resultTyCon ty
    [nilDc, consDc] <- resultDataCons ty

    case vec of
      Nil ->
        case dataConInstArgTys nilDc [nTy, aTy] of
          Just [coTy] ->
            let co = VThunk (primCo coTy) env
             in pure (VData nilDc [Right nTy, Right aTy, Left co] env)

          _ -> empty

      Cons a as -> do
        szN <- typeSize nTy Nothing
        let pnTy = LitTy (NumTy (szN - 1))

        case dataConInstArgTys consDc [nTy, aTy, pnTy] of
          Just [coTy, _, vecTy] -> do
            let co = VThunk (primCo coTy) env
            x  <- toValue a aTy
            xs <- toValue as vecTy

            pure (VData consDc [Right nTy, Right aTy, Right pnTy, Left co, Left x, Left xs] env)

          _ -> empty

-- ref :: forall a. Int# -> a
mkPrimRef :: Eval PrimInfo
mkPrimRef = do
  aTv <- getUniqueTyVar "a" liftedTypeKind
  let pTy = mkPolyFunTy (VarTy aTv) [Left aTv, Right intPrimTy]

  pure $ PrimInfo
    { primName = "Clash.Transformations.ref"
    , primType = pTy
    , primWorkInfo = WorkNever
    , primMultiResult = SingleResult
    , primUnfolding = NoUnfolding
    }

instance (ToAst a) => ToAst (Ref a) where
  toValue (Ref ma x) ty = do
    pr  <- mkPrimRef
    val <- toValue x ty

    addr <- maybe getAddr pure ma
    setRef addr val

    let addrLit = VLiteral (IntLiteral (toInteger addr))
    pure . VNeutral $ NePrim pr [Right ty, Left addrLit]
