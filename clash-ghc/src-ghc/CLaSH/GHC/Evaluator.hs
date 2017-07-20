{-|
  Copyright   :  (C) 2013-2016, University of Twente, 2017, QBayLogic
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UnboxedTuples     #-}

module CLaSH.GHC.Evaluator where

import           Control.Monad.Trans.Except (runExcept)
import qualified Data.Bifunctor      as Bifunctor
import           Data.Bits
import qualified Data.Either         as Either
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe          (catMaybes)
import qualified Data.List           as List
import           Data.Proxy          (Proxy)
import           Data.Reflection     (reifyNat)
import           Data.Text           (Text)
-- import           Data.Word
import           GHC.Int
import           GHC.Prim
import           GHC.Real            (Ratio (..))
import           GHC.TypeLits        (KnownNat)
import           GHC.Word
import           Unbound.Generics.LocallyNameless (runFreshM, bind, embed)

import           CLaSH.Core.DataCon  (DataCon (..))
import           CLaSH.Core.Literal  (Literal (..))
import           CLaSH.Core.Name     (Name (..), string2SystemName)
import           CLaSH.Core.Pretty   (showDoc)
import           CLaSH.Core.Term     (Term (..))
import           CLaSH.Core.Type     (Type (..), ConstTy (..), LitTy (..),
                                      TypeView (..), mkFunTy,
                                      mkTyConApp, splitFunForallTy, tyView)
import           CLaSH.Core.TyCon    (TyCon, TyConOccName, tyConDataCons)
import           CLaSH.Core.TysPrim
import           CLaSH.Core.Util     (collectArgs,mkApps,mkRTree,mkVec,termType,
                                      tyNatSize)
import           CLaSH.Core.Var      (Var (..))
import           CLaSH.Util          (clogBase, flogBase, curLoc)

import qualified CLaSH.Sized.Internal.BitVector as BitVector
import qualified CLaSH.Sized.Internal.Signed    as Signed
import qualified CLaSH.Sized.Internal.Unsigned  as Unsigned
import CLaSH.Sized.Internal.BitVector(BitVector(..))
import CLaSH.Sized.Internal.Signed   (Signed   (..))
import CLaSH.Sized.Internal.Unsigned (Unsigned (..))

reduceConstant :: HashMap.HashMap TyConOccName TyCon -> Bool -> Term -> Term
reduceConstant tcm isSubj e@(collectArgs -> (Prim nm ty, args)) = case nm of
  "GHC.Prim.eqChar#" | Just (i,j) <- charLiterals tcm isSubj args
    -> boolToIntLiteral (i == j)

  "GHC.Prim.neChar#" | Just (i,j) <- charLiterals tcm isSubj args
    -> boolToIntLiteral (i /= j)

  "GHC.Prim.+#" | Just (i,j) <- intLiterals tcm isSubj args
    -> integerToIntLiteral (i+j)

  "GHC.Prim.-#" | Just (i,j) <- intLiterals tcm isSubj args
    -> integerToIntLiteral (i-j)

  "GHC.Prim.*#" | Just (i,j) <- intLiterals tcm isSubj args
    -> integerToIntLiteral (i*j)

  "GHC.Prim.quotInt#" | Just (i,j) <- intLiterals tcm isSubj args
    -> integerToIntLiteral (i `quot` j)

  "GHC.Prim.remInt#" | Just (i,j) <- intLiterals tcm isSubj args
    -> integerToIntLiteral (i `rem` j)

  "GHC.Prim.quotRemInt#" | Just (i,j) <- intLiterals tcm isSubj args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           (q,r)   = quotRem i j
           ret     = mkApps (Data tupDc) (map Right tyArgs ++
                    [Left (integerToIntLiteral q), Left (integerToIntLiteral r)])
       in  ret

  "GHC.Prim.negateInt#"
    | [Literal (IntLiteral i)] <- reduceTerms tcm isSubj args
    -> integerToIntLiteral (negate i)

  "GHC.Prim.>#" | Just (i,j) <- intLiterals tcm isSubj args
    -> boolToIntLiteral (i > j)

  "GHC.Prim.>=#" | Just (i,j) <- intLiterals tcm isSubj args
    -> boolToIntLiteral (i >= j)

  "GHC.Prim.==#" | Just (i,j) <- intLiterals tcm isSubj args
    -> boolToIntLiteral (i == j)

  "GHC.Prim./=#" | Just (i,j) <- intLiterals tcm isSubj args
    -> boolToIntLiteral (i /= j)

  "GHC.Prim.<#" | Just (i,j) <- intLiterals tcm isSubj args
    -> boolToIntLiteral (i < j)

  "GHC.Prim.<=#" | Just (i,j) <- intLiterals tcm isSubj args
    -> boolToIntLiteral (i <= j)

  "GHC.Prim.eqWord#" | Just (i,j) <- wordLiterals tcm isSubj args
    -> boolToIntLiteral (i == j)

  "GHC.Prim.neWord#" | Just (i,j) <- wordLiterals tcm isSubj args
    -> boolToIntLiteral (i /= j)

  "GHC.Prim.tagToEnum#"
    | [Right (ConstTy (TyCon tcN)), Left (Literal (IntLiteral i))] <-
      map (Bifunctor.bimap (reduceConstant tcm isSubj) id) args
    -> let dc = do { tc <- HashMap.lookup (nameOcc tcN) tcm
                   ; let dcs = tyConDataCons tc
                   ; List.find ((== (i+1)) . toInteger . dcTag) dcs
                   }
       in maybe e Data dc

  "GHC.Prim.int2Word#"
    | [Literal (IntLiteral i)] <- reduceTerms tcm isSubj args
    -> Literal . WordLiteral . toInteger $ (fromInteger :: Integer -> Word) i -- for overflow behaviour

  "GHC.Prim.plusWord2#" | Just (i,j) <- wordLiterals tcm isSubj args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           !(W# a)  = fromInteger i
           !(W# b)  = fromInteger j
           !(# h, l #) = plusWord2# a b
       in  mkApps (Data tupDc) (map Right tyArgs ++
                   [ Left (Literal . WordLiteral . toInteger $ W# h)
                   , Left (Literal . WordLiteral . toInteger $ W# l)])

  "GHC.Prim.timesWord2#" | Just (i,j) <- wordLiterals tcm isSubj args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           !(W# a)  = fromInteger i
           !(W# b)  = fromInteger j
           !(# h, l #) = timesWord2# a b
       in  mkApps (Data tupDc) (map Right tyArgs ++
                   [ Left (Literal . WordLiteral . toInteger $ W# h)
                   , Left (Literal . WordLiteral . toInteger $ W# l)])

  "GHC.Prim.subWordC#" | Just (i,j) <- wordLiterals tcm isSubj args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           !(W# a)  = fromInteger i
           !(W# b)  = fromInteger j
           !(# d, c #) = subWordC# a b
       in  mkApps (Data tupDc) (map Right tyArgs ++
                   [ Left (Literal . WordLiteral . toInteger $ W# d)
                   , Left (Literal . IntLiteral . toInteger $ I# c)])

  "GHC.Prim.uncheckedShiftL#"
    | [ Literal (WordLiteral w)
      , Literal (IntLiteral  i)
      ] <- reduceTerms tcm isSubj args
    -> Literal (WordLiteral (w `shiftL` fromInteger i))

  "GHC.Classes.geInt" | Just (i,j) <- intCLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i >= j)

  "GHC.Integer.Logarithms.integerLogBase#"
    | Just (a,b) <- integerLiterals tcm isSubj args
    , Just c <- flogBase a b
    -> (Literal . IntLiteral . toInteger) c

  "GHC.Integer.Type.smallInteger"
    | [Literal (IntLiteral i)] <- reduceTerms tcm isSubj args
    -> Literal (IntegerLiteral i)

  "GHC.Integer.Type.integerToInt"
    | [Literal (IntegerLiteral i)] <- reduceTerms tcm isSubj args
    -> integerToIntLiteral i

  "GHC.Integer.Type.plusInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> integerToIntegerLiteral (i+j)

  "GHC.Integer.Type.minusInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> integerToIntegerLiteral (i-j)

  "GHC.Integer.Type.timesInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> integerToIntegerLiteral (i*j)

  "GHC.Integer.Type.negateInteger#"
    | [Literal (IntegerLiteral i)] <- reduceTerms tcm isSubj args
    -> integerToIntegerLiteral (negate i)

  "GHC.Integer.Type.divInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> integerToIntegerLiteral (i `div` j)

  "GHC.Integer.Type.modInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> integerToIntegerLiteral (i `mod` j)

  "GHC.Integer.Type.quotInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> integerToIntegerLiteral (i `quot` j)

  "GHC.Integer.Type.remInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> integerToIntegerLiteral (i `rem` j)

  "GHC.Integer.Type.divModInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> let (_,tyView -> TyConApp ubTupTcNm [liftedKi,_,intTy,_]) = splitFunForallTy ty
           (Just ubTupTc) = HashMap.lookup (nameOcc ubTupTcNm) tcm
           [ubTupDc] = tyConDataCons ubTupTc
           (d,m) = divMod i j
       in  mkApps (Data ubTupDc) [ Right liftedKi, Right liftedKi
                                 , Right intTy,    Right intTy
                                 , Left (Literal (IntegerLiteral d))
                                 , Left (Literal (IntegerLiteral m))
                                 ]

  "GHC.Integer.Type.gtInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i > j)

  "GHC.Integer.Type.geInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i >= j)

  "GHC.Integer.Type.eqInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i == j)

  "GHC.Integer.Type.neqInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i /= j)

  "GHC.Integer.Type.ltInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i < j)

  "GHC.Integer.Type.leInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i <= j)

  "GHC.Integer.Type.gtInteger#" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToIntLiteral (i > j)

  "GHC.Integer.Type.geInteger#" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToIntLiteral (i >= j)

  "GHC.Integer.Type.eqInteger#" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToIntLiteral (i == j)

  "GHC.Integer.Type.neqInteger#" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToIntLiteral (i /= j)

  "GHC.Integer.Type.ltInteger#" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToIntLiteral (i < j)

  "GHC.Integer.Type.leInteger#" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToIntLiteral (i <= j)

  "GHC.Integer.Type.shiftRInteger"
    | [Literal (IntegerLiteral i), Literal (IntLiteral j)] <- reduceTerms tcm isSubj args
    -> integerToIntegerLiteral (i `shiftR` fromInteger j)

  "GHC.Integer.Type.shiftLInteger"
    | [Literal (IntegerLiteral i), Literal (IntLiteral j)] <- reduceTerms tcm isSubj args
    -> integerToIntegerLiteral (i `shiftL` fromInteger j)

  "GHC.Integer.Type.wordToInteger"
    | [Literal (WordLiteral w)] <- reduceTerms tcm isSubj args
    -> Literal (IntegerLiteral w)

  "GHC.Natural.NatS#"
    | [Literal (WordLiteral w)] <- reduceTerms tcm isSubj args
    -> Literal (NaturalLiteral w)

  "GHC.TypeLits.natVal"
#if MIN_VERSION_ghc(8,2,0)
    | [Literal (NaturalLiteral n), _] <- reduceTerms tcm isSubj args
    -> integerToIntegerLiteral n
#else
    | [Literal (IntegerLiteral i), _] <- reduceTerms tcm isSubj args
    -> integerToIntegerLiteral i
#endif

#if MIN_VERSION_ghc(8,2,0)
  "GHC.TypeNats.natVal"
    | [Literal (NaturalLiteral n), _] <- reduceTerms tcm isSubj args
    -> Literal (NaturalLiteral n)
#endif

  "GHC.Types.I#"
    | isSubj
    , [Literal (IntLiteral i)] <- reduceTerms tcm isSubj args
    ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
            (Just intTc) = HashMap.lookup (nameOcc intTcNm) tcm
            [intDc] = tyConDataCons intTc
        in  mkApps (Data intDc) [Left (Literal (IntLiteral i))]

  "GHC.Float.$w$sfromRat''" -- XXX: Very fragile
    | [Literal (IntLiteral _minEx)
      ,Literal (IntLiteral matDigs)
      ,Literal (IntegerLiteral n)
      ,Literal (IntegerLiteral d)] <- reduceTerms tcm isSubj args
    -> case fromInteger matDigs of
          matDigs'
            | matDigs' == floatDigits (undefined :: Float)
            -> Literal (FloatLiteral (toRational (fromRational (n :% d) :: Float)))
            | matDigs' == floatDigits (undefined :: Double)
            -> Literal (DoubleLiteral (toRational (fromRational (n :% d) :: Double)))
          _ -> error $ $(curLoc) ++ "GHC.Float.$w$sfromRat'': Not a Float or Double: " ++ showDoc e

  "GHC.Float.$w$sfromRat''1" -- XXX: Very fragile
    | [Literal (IntLiteral _minEx)
      ,Literal (IntLiteral matDigs)
      ,Literal (IntegerLiteral n)
      ,Literal (IntegerLiteral d)] <- reduceTerms tcm isSubj args
    -> case fromInteger matDigs of
          matDigs'
            | matDigs' == floatDigits (undefined :: Float)
            -> Literal (FloatLiteral (toRational (fromRational (n :% d) :: Float)))
            | matDigs' == floatDigits (undefined :: Double)
            -> Literal (DoubleLiteral (toRational (fromRational (n :% d) :: Double)))
          _ -> error $ $(curLoc) ++ "GHC.Float.$w$sfromRat'': Not a Float or Double: " ++ showDoc e

  "GHC.Integer.Type.doubleFromInteger"
    | [Literal (IntegerLiteral i)] <- reduceTerms tcm isSubj args
    -> Literal (DoubleLiteral (toRational (fromInteger i :: Double)))

  "GHC.Prim.double2Float#"
    | [Literal (DoubleLiteral d)] <- reduceTerms tcm isSubj args
    -> Literal (FloatLiteral (toRational (fromRational d :: Float)))

  "GHC.Prim.divideFloat#"
    | [Literal (FloatLiteral f1)
      ,Literal (FloatLiteral f2)] <- reduceTerms tcm isSubj args
    -> Literal (FloatLiteral (toRational (fromRational f1 / fromRational f2 :: Float)))

  "GHC.Base.eqString"
    | [(_,[Left (Literal (StringLiteral s1))])
      ,(_,[Left (Literal (StringLiteral s2))])
      ] <- map collectArgs (Either.lefts args)
    -> boolToBoolLiteral tcm ty (s1 == s2)

  "CLaSH.Promoted.Nat.powSNat"
    | [Right a, Right b] <- (map (runExcept . tyNatSize tcm) . Either.rights) args
    -> let c = case a of
                 2 -> 1 `shiftL` (fromInteger b)
                 _ -> a ^ b
           (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
           (Just snatTc) = HashMap.lookup (nameOcc snatTcNm) tcm
           [snatDc] = tyConDataCons snatTc
       in  mkApps (Data snatDc) [ Right (LitTy (NumTy c))
#if MIN_VERSION_ghc(8,2,0)
                                , Left (Literal (NaturalLiteral c))]
#else
                                , Left (Literal (IntegerLiteral c))]
#endif

  "CLaSH.Promoted.Nat.flogBaseSNat"
    | [_,_,Right a, Right b] <- (map (runExcept . tyNatSize tcm) . Either.rights) args
    , Just c <- flogBase a b
    , let c' = toInteger c
    -> let (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
           (Just snatTc) = HashMap.lookup (nameOcc snatTcNm) tcm
           [snatDc] = tyConDataCons snatTc
       in  mkApps (Data snatDc) [ Right (LitTy (NumTy c'))
#if MIN_VERSION_ghc(8,2,0)
                                , Left (Literal (NaturalLiteral c'))]
#else
                                , Left (Literal (IntegerLiteral c'))]
#endif

  "CLaSH.Promoted.Nat.clogBaseSNat"
    | [_,_,Right a, Right b] <- (map (runExcept . tyNatSize tcm) . Either.rights) args
    , Just c <- clogBase a b
    , let c' = toInteger c
    -> let (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
           (Just snatTc) = HashMap.lookup (nameOcc snatTcNm) tcm
           [snatDc] = tyConDataCons snatTc
       in  mkApps (Data snatDc) [ Right (LitTy (NumTy c'))
#if MIN_VERSION_ghc(8,2,0)
                                , Left (Literal (NaturalLiteral c'))]
#else
                                , Left (Literal (IntegerLiteral c'))]
#endif

  "CLaSH.Promoted.Nat.logBaseSNat"
    | [_,Right a, Right b] <- (map (runExcept . tyNatSize tcm) . Either.rights) args
    , Just c <- flogBase a b
    , let c' = toInteger c
    -> let (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
           (Just snatTc) = HashMap.lookup (nameOcc snatTcNm) tcm
           [snatDc] = tyConDataCons snatTc
       in  mkApps (Data snatDc) [ Right (LitTy (NumTy c'))
#if MIN_VERSION_ghc(8,2,0)
                                , Left (Literal (NaturalLiteral c'))]
#else
                                , Left (Literal (IntegerLiteral c'))]
#endif

  "CLaSH.Sized.Internal.BitVector.eq#" | Just (i,j) <- bitVectorLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i == j)

  "CLaSH.Sized.Internal.BitVector.neq#" | Just (i,j) <- bitVectorLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i /= j)

  "CLaSH.Sized.Internal.BitVector.shiftL#"
    | Just (nTy,kn,i,j) <- bvLitIntLit tcm isSubj args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in mkBvLit ty nTy kn val
      where
        op :: KnownNat n => BitVector n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (BitVector.shiftL# u i)
  "CLaSH.Sized.Internal.BitVector.shiftR#"
    | Just (nTy,kn,i,j) <- bvLitIntLit tcm isSubj args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in mkBvLit ty nTy kn val
      where
        op :: KnownNat n => BitVector n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (BitVector.shiftR# u i)

  "CLaSH.Sized.Internal.BitVector.rotateL#"
    | Just (nTy,kn,i,j) <- bvLitIntLit tcm isSubj args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in mkBvLit ty nTy kn val
      where
        op :: KnownNat n => BitVector n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (BitVector.rotateL# u i)
  "CLaSH.Sized.Internal.BitVector.rotateR#"
    | Just (nTy,kn,i,j) <- bvLitIntLit tcm isSubj args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in mkBvLit ty nTy kn val
      where
        op :: KnownNat n => BitVector n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (BitVector.rotateR# u i)

  "CLaSH.Sized.Internal.Index.eq#" | Just (i,j) <- indexLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i == j)

  "CLaSH.Sized.Internal.Index.neq#" | Just (i,j) <- indexLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i /= j)

  "CLaSH.Sized.Internal.Signed.eq#" | Just (i,j) <- signedLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i == j)

  "CLaSH.Sized.Internal.Signed.neq#" | Just (i,j) <- signedLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i /= j)

  "CLaSH.Sized.Internal.Signed.+#"
    | Just (_, kn) <- extractKnownNat tcm args
    , Just val <- reifyNat kn (liftSigned2 (Signed.+#) ty tcm isSubj args)
    -> val
  "CLaSH.Sized.Internal.Signed.-#"
    | Just (_, kn) <- extractKnownNat tcm args
    , Just val <- reifyNat kn (liftSigned2 (Signed.-#) ty tcm isSubj args)
    -> val
  "CLaSH.Sized.Internal.Signed.*#"
    | Just (_, kn) <- extractKnownNat tcm args
    , Just val <- reifyNat kn (liftSigned2 (Signed.*#) ty tcm isSubj args)
    -> val

  "CLaSH.Sized.Internal.Signed.minBound#"
    | Just (litTy,mb) <- extractKnownNat tcm args
    -> let minB = negate (2 ^ (mb - 1))
       in  mkSignedLit ty litTy mb minB

  "CLaSH.Sized.Internal.Signed.maxBound#"
    | Just (litTy,mb) <- extractKnownNat tcm args
    -> let maxB = (2 ^ (mb - 1)) - 1
       in mkSignedLit ty litTy mb maxB

  "CLaSH.Sized.Internal.Signed.shiftL#"
    | Just (nTy,kn,i,j) <- signedLitIntLit tcm isSubj args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in mkSignedLit ty nTy kn val
      where
        op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (Signed.shiftL# u i)
  "CLaSH.Sized.Internal.Signed.shiftR#"
    | Just (nTy,kn,i,j) <- signedLitIntLit tcm isSubj args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in mkSignedLit ty nTy kn val
      where
        op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (Signed.shiftR# u i)

  "CLaSH.Sized.Internal.Signed.rotateL#"
    | Just (nTy,kn,i,j) <- signedLitIntLit tcm isSubj args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in mkSignedLit ty nTy kn val
      where
        op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (Signed.rotateL# u i)
  "CLaSH.Sized.Internal.Signed.rotateR#"
    | Just (nTy,kn,i,j) <- signedLitIntLit tcm isSubj args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in mkSignedLit ty nTy kn val
      where
        op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (Signed.rotateR# u i)

  "CLaSH.Sized.Internal.Signed.toInteger#"
    | [collectArgs -> (Prim nm' _,[Right _, Left _, Left (Literal (IntegerLiteral i))])] <-
      (map (reduceConstant tcm isSubj) . Either.lefts) args
    , nm' == "CLaSH.Sized.Internal.Signed.fromInteger#"
    -> integerToIntegerLiteral i

  "CLaSH.Sized.Internal.Unsigned.size#"
    | Just (_, kn) <- extractKnownNat tcm args
    -> let ty' = runFreshM (termType tcm e)
           (TyConApp intTcNm _) = tyView ty'
           (Just intTc) = HashMap.lookup (nameOcc intTcNm) tcm
           [intCon] = tyConDataCons intTc
       in  mkApps (Data intCon) [Left (Literal (IntLiteral kn))]

  "CLaSH.Sized.Internal.Unsigned.pack#"
    | Just (nTy, kn) <- extractKnownNat tcm args
    , [i] <- unsignedLiterals' tcm isSubj args
    -> mkBvLit ty nTy kn i
  "CLaSH.Sized.Internal.Unsigned.unpack#"
    | Just (nTy, kn) <- extractKnownNat tcm args
    , [i] <- bvLiterals' tcm isSubj args
    -> mkUnsignedLit ty nTy kn i

  "CLaSH.Sized.Internal.Unsigned.+#"
    | Just (_, kn) <- extractKnownNat tcm args
    , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.+#) ty tcm isSubj args)
    -> val
  "CLaSH.Sized.Internal.Unsigned.-#"
    | Just (_, kn) <- extractKnownNat tcm args
    , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.-#) ty tcm isSubj args)
    -> val
  "CLaSH.Sized.Internal.Unsigned.*#"
    | Just (_, kn) <- extractKnownNat tcm args
    , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.*#) ty tcm isSubj args)
    -> val

  "CLaSH.Sized.Internal.Unsigned.quot#"
    | Just (_, kn) <- extractKnownNat tcm args
    , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.quot#) ty tcm isSubj args)
    -> val
  "CLaSH.Sized.Internal.Unsigned.rem#"
    | Just (_, kn) <- extractKnownNat tcm args
    , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.rem#) ty tcm isSubj args)
    -> val

  "CLaSH.Sized.Internal.Unsigned.resize#" -- forall n m . KnownNat m => Unsigned n -> Unsigned m
    | (Right _ : Right mTy : _) <- args
    , Right km <- runExcept (tyNatSize tcm mTy)
    , [i] <- unsignedLiterals' tcm isSubj args
    -> let bitsKeep = (bit (fromInteger km)) - 1
           val = i .&. bitsKeep
    in mkUnsignedLit ty mTy km val


  "CLaSH.Sized.Internal.Unsigned.plus#" -- :: Unsigned m -> Unsigned n -> Unsigned (Max m n + 1)
    | Just (i,j) <- unsignedLiterals tcm isSubj args
    -> let resTy = runFreshM (termType tcm e)
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
       in  mkUnsignedLit resTy resSizeTy resSizeInt (i+j)

  "CLaSH.Sized.Internal.Unsigned.minus#"
    | [i,j] <- unsignedLiterals' tcm isSubj args
    -> let resTy = runFreshM (termType tcm e)
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
           val = reifyNat resSizeInt (runSizedF (Unsigned.-#) i j)
      in  mkUnsignedLit resTy resSizeTy resSizeInt val

  "CLaSH.Sized.Internal.Unsigned.times#"
    | Just (i,j) <- unsignedLiterals tcm isSubj args
    -> let resTy = runFreshM (termType tcm e)
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
       in  mkUnsignedLit resTy resSizeTy resSizeInt (i*j)

  "CLaSH.Sized.Internal.Unsigned.negate#"
    | Just (nTy, kn) <- extractKnownNat tcm args
    , [i] <- unsignedLiterals' tcm isSubj args
    -> let val = reifyNat kn (op (fromInteger i))
    in mkUnsignedLit ty nTy kn val
    where
      op :: KnownNat n => Unsigned n -> Proxy n -> Integer
      op u _ = toInteger (Unsigned.negate# u)

  "CLaSH.Sized.Internal.Unsigned.and#"
    | Just (i,j) <- unsignedLiterals tcm isSubj args
    , Just (nTy, kn) <- extractKnownNat tcm args
    -> mkUnsignedLit ty nTy kn (i .&. j)
  "CLaSH.Sized.Internal.Unsigned.or#"
    | Just (i,j) <- unsignedLiterals tcm isSubj args
    , Just (nTy, kn) <- extractKnownNat tcm args
    -> mkUnsignedLit ty nTy kn (i .|. j)
  "CLaSH.Sized.Internal.Unsigned.xor#"
    | Just (i,j) <- unsignedLiterals tcm isSubj args
    , Just (nTy, kn) <- extractKnownNat tcm args
    -> mkUnsignedLit ty nTy kn (i `xor` j)

  "CLaSH.Sized.Internal.Unsigned.complement#"
    | [i] <- unsignedLiterals' tcm isSubj args
    , Just (nTy, kn) <- extractKnownNat tcm args
    -> let val = reifyNat kn (op (fromInteger i))
    in mkUnsignedLit ty nTy kn val
    where
      op :: KnownNat n => Unsigned n -> Proxy n -> Integer
      op u _ = toInteger (Unsigned.complement# u)

  "CLaSH.Sized.Internal.Unsigned.lt#" | Just (i,j) <- unsignedLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i <  j)
  "CLaSH.Sized.Internal.Unsigned.le#" | Just (i,j) <- unsignedLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i <= j)
  "CLaSH.Sized.Internal.Unsigned.gt#" | Just (i,j) <- unsignedLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i >  j)
  "CLaSH.Sized.Internal.Unsigned.ge#" | Just (i,j) <- unsignedLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i >= j)

  "CLaSH.Sized.Internal.Unsigned.eq#" | Just (i,j) <- unsignedLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i == j)

  "CLaSH.Sized.Internal.Unsigned.neq#" | Just (i,j) <- unsignedLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i /= j)

  "CLaSH.Sized.Internal.Unsigned.minBound#"
    | Just (nTy,len) <- extractKnownNat tcm args
    -> mkUnsignedLit ty nTy len 0

  "CLaSH.Sized.Internal.Unsigned.maxBound#"
    | Just (litTy,mb) <- extractKnownNat tcm args
    -> let maxB = (2 ^ mb) - 1
       in  mkUnsignedLit ty litTy mb maxB

  "CLaSH.Sized.Internal.Unsigned.shiftL#" -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
    | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm isSubj args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in mkUnsignedLit ty nTy kn val
      where
        op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (Unsigned.shiftL# u i)
  "CLaSH.Sized.Internal.Unsigned.shiftR#" -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
    | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm isSubj args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in mkUnsignedLit ty nTy kn val
      where
        op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (Unsigned.shiftR# u i)

  "CLaSH.Sized.Internal.Unsigned.rotateL#" -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
    | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm isSubj args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in mkUnsignedLit ty nTy kn val
      where
        op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (Unsigned.rotateL# u i)
  "CLaSH.Sized.Internal.Unsigned.rotateR#" -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
    | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm isSubj args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in mkUnsignedLit ty nTy kn val
      where
        op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (Unsigned.rotateR# u i)

  "CLaSH.Sized.Internal.Unsigned.toInteger#"
    | [collectArgs -> (Prim nm' _,[Right _, Left _, Left (Literal (IntegerLiteral i))])] <-
      (map (reduceConstant tcm isSubj) . Either.lefts) args
    , nm' == "CLaSH.Sized.Internal.Unsigned.fromInteger#"
    -> integerToIntegerLiteral i

  "CLaSH.Sized.RTree.treplicate"
    | isSubj
    , (TyConApp treeTcNm [lenTy,argTy]) <- tyView (runFreshM (termType tcm e))
    , Right len <- runExcept (tyNatSize tcm lenTy)
    -> let (Just treeTc) = HashMap.lookup (nameOcc treeTcNm) tcm
           [lrCon,brCon] = tyConDataCons treeTc
       in  mkRTree lrCon brCon argTy len (replicate (2^len) (last $ Either.lefts args))

  "CLaSH.Sized.Vector.replicate"
    | isSubj
    , (TyConApp vecTcNm [lenTy,argTy]) <- tyView (runFreshM (termType tcm e))
    , Right len <- runExcept (tyNatSize tcm lenTy)
    -> let (Just vecTc) = HashMap.lookup (nameOcc vecTcNm) tcm
           [nilCon,consCon] = tyConDataCons vecTc
       in  mkVec nilCon consCon argTy len (replicate (fromInteger len) (last $ Either.lefts args))

  "CLaSH.Sized.Vector.maxIndex"
    | isSubj
    , [nTy, _] <- Either.rights args
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> let ty' = runFreshM (termType tcm e)
           (TyConApp intTcNm _) = tyView ty'
           (Just intTc) = HashMap.lookup (nameOcc intTcNm) tcm
           [intCon] = tyConDataCons intTc
       in  mkApps (Data intCon) [Left (Literal (IntLiteral (toInteger (n - 1))))]

  "CLaSH.Sized.Vector.length"
    | isSubj
    , [nTy, _] <- Either.rights args
    , Right n <-runExcept (tyNatSize tcm nTy)
    -> let ty' = runFreshM (termType tcm e)
           (TyConApp intTcNm _) = tyView ty'
           (Just intTc) = HashMap.lookup (nameOcc intTcNm) tcm
           [intCon] = tyConDataCons intTc
       in  mkApps (Data intCon) [Left (Literal (IntLiteral (toInteger n)))]

  _ -> e

reduceConstant _ _ e = e

reduceTerms :: HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> [Term]
reduceTerms tcm isSubj = map (reduceConstant tcm isSubj) . Either.lefts

integerLiterals :: HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> Maybe (Integer,Integer)
integerLiterals tcm isSubj args = case reduceTerms tcm isSubj args of
  [Literal (IntegerLiteral i), Literal (IntegerLiteral j)] -> Just (i,j)
  _ -> Nothing

intLiterals :: HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> Maybe (Integer,Integer)
intLiterals tcm isSubj args = case reduceTerms tcm isSubj args of
  [Literal (IntLiteral i), Literal (IntLiteral j)] -> Just (i,j)
  _ -> Nothing

intCLiterals :: HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> Maybe (Integer,Integer)
intCLiterals tcm isSubj args = case reduceTerms tcm isSubj args of
  ([collectArgs -> (Data _,[Left (Literal (IntLiteral i))])
   ,collectArgs -> (Data _,[Left (Literal (IntLiteral j))])])
    -> Just (i,j)
  _ -> Nothing

wordLiterals :: HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> Maybe (Integer,Integer)
wordLiterals tcm isSubj args = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
  [Literal (WordLiteral i), Literal (WordLiteral j)] -> Just (i,j)
  _ -> Nothing

charLiterals :: HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> Maybe (Char,Char)
charLiterals tcm isSubj args = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
  [Literal (CharLiteral i), Literal (CharLiteral j)] -> Just (i,j)
  _ -> Nothing

sizedLiterals :: Text -> HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> Maybe (Integer,Integer)
sizedLiterals szCon tcm isSubj args
  = case reduceTerms tcm isSubj args of
      ([ collectArgs -> (Prim nm  _,[Right _, Left _, Left (Literal (IntegerLiteral i))])
       , collectArgs -> (Prim nm' _,[Right _, Left _, Left (Literal (IntegerLiteral j))])])
        | nm  == szCon
        , nm' == szCon -> Just (i,j)
      _ -> Nothing

sizedLiterals' :: Text -> HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> [Integer]
sizedLiterals' szCon tcm isSubj args
  -- = [ i | i <- maybeToList (sized), arg <- reduceTerms tcm isSubj args]
  = catMaybes $ map (sizedLiteral szCon) $ reduceTerms tcm isSubj args
    -- case reduceTerms tcm isSubj args of
    --   ([ collectArgs -> (Prim nm  _,[Right _, Left _, Left (Literal (IntegerLiteral i))])
    --    , collectArgs -> (Prim nm' _,[Right _, Left _, Left (Literal (IntegerLiteral j))])])
    --     | nm  == szCon
    --     , nm' == szCon -> Just (i,j)
    --   _ -> Nothing

sizedLiteral :: Text -> Term -> Maybe Integer
sizedLiteral szCon term = case collectArgs term of
  (Prim nm  _,[Right _, Left _, Left (Literal (IntegerLiteral i))]) | nm == szCon -> Just i
  _ -> Nothing


bitVectorLiterals :: HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> Maybe (Integer,Integer)
bitVectorLiterals = sizedLiterals "CLaSH.Sized.Internal.BitVector.fromInteger#"

indexLiterals :: HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> Maybe (Integer,Integer)
indexLiterals = sizedLiterals "CLaSH.Sized.Internal.Index.fromInteger#"

signedLiterals :: HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> Maybe (Integer,Integer)
signedLiterals = sizedLiterals "CLaSH.Sized.Internal.Signed.fromInteger#"

signedLiterals' :: HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> [Integer]
signedLiterals' = sizedLiterals' "CLaSH.Sized.Internal.Signed.fromInteger#"

unsignedLiterals :: HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> Maybe (Integer,Integer)
unsignedLiterals = sizedLiterals "CLaSH.Sized.Internal.Unsigned.fromInteger#"

unsignedLiterals' :: HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> [Integer]
unsignedLiterals' = sizedLiterals' "CLaSH.Sized.Internal.Unsigned.fromInteger#"

bvLiterals' :: HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> [Integer]
bvLiterals' = sizedLiterals' "CLaSH.Sized.Internal.BitVector.fromInteger#"


-- Tries to match literal arguments to a function like
--   (Unsigned.shiftL#  :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n)
sizedLitIntLit :: Text -> HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> Maybe (Type,Integer,Integer,Integer)
sizedLitIntLit szCon tcm isSubj args
  = case args of
      (Right nTy : _) | Right kn <- runExcept (tyNatSize tcm nTy)
                      , [ _
                        , (collectArgs -> (Prim nm _, [Right _, Left _, Left (Literal (IntegerLiteral i)) ]))
                        , (collectArgs -> (Prim _ _, [Left (Literal (IntLiteral j))] ))
                        ] <- reduceTerms tcm isSubj args
                      , nm == szCon
                      -> Just (nTy,kn,i,j)
      _ -> Nothing

bvLitIntLit, signedLitIntLit, unsignedLitIntLit :: HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> Maybe (Type,Integer,Integer,Integer)
bvLitIntLit       = sizedLitIntLit "CLaSH.Sized.Internal.BitVector.fromInteger#"
signedLitIntLit   = sizedLitIntLit "CLaSH.Sized.Internal.Signed.fromInteger#"
unsignedLitIntLit = sizedLitIntLit "CLaSH.Sized.Internal.Unsigned.fromInteger#"

-- From an argument list to function of type
--   forall n. KnownNat n => ...
-- extract (nTy,nInt)
-- where nTy is the Type of n
-- and   nInt is its value as an Integer
extractKnownNat :: HashMap.HashMap TyConOccName TyCon -> [Either a Type] -> Maybe (Type, Integer)
extractKnownNat tcm args = case args of
  (Right nTy : _) | Right nInt <- runExcept (tyNatSize tcm nTy)
    -> Just (nTy, nInt)
  _ -> Nothing

-- Construct a constant value term of a sized type
mkSizedLit
  :: (Type -> Term)    -- type constructor?
  -> Type    -- ????
  -> Type    -- forall n.
  -> Integer -- KnownNat n
  -> Integer -- value
  -> Term
mkSizedLit conPrim ty nTy kn val
  = mkApps (conPrim sTy) [Right nTy,Left (Literal (NaturalLiteral kn)),Left (Literal (IntegerLiteral ( val)))]
  where
    (_,sTy) = splitFunForallTy ty

mkBvLit, mkSignedLit, mkUnsignedLit :: Type -> Type -> Integer -> Integer -> Term
mkBvLit       = mkSizedLit bvConPrim
mkSignedLit   = mkSizedLit signedConPrim
mkUnsignedLit = mkSizedLit unsignedConPrim

boolToIntLiteral :: Bool -> Term
boolToIntLiteral b = if b then Literal (IntLiteral 1) else Literal (IntLiteral 0)

boolToBoolLiteral :: HashMap.HashMap TyConOccName TyCon -> Type -> Bool -> Term
boolToBoolLiteral tcm ty b =
 let (_,tyView -> TyConApp boolTcNm []) = splitFunForallTy ty
     (Just boolTc) = HashMap.lookup (nameOcc boolTcNm) tcm
     [falseDc,trueDc] = tyConDataCons boolTc
     retDc = if b then trueDc else falseDc
 in  Data retDc

integerToIntLiteral :: Integer -> Term
integerToIntLiteral = Literal . IntLiteral . toInteger . (fromInteger :: Integer -> Int) -- for overflow behaviour

integerToWordLiteral :: Integer -> Term
integerToWordLiteral = Literal . WordLiteral . toInteger . (fromInteger :: Integer -> Word) -- for overflow behaviour

integerToIntegerLiteral :: Integer -> Term
integerToIntegerLiteral = Literal . IntegerLiteral

bvConPrim :: Type -> Term
bvConPrim (tyView -> TyConApp bvTcNm _)
  = Prim "CLaSH.Sized.Internal.BitVector.fromInteger#" (ForAllTy (bind nTV funTy))
  where
#if MIN_VERSION_ghc(8,2,0)
    funTy        = foldr1 mkFunTy [naturalPrimTy,integerPrimTy,mkTyConApp bvTcNm [nVar]]
#else
    funTy        = foldr1 mkFunTy [integerPrimTy,integerPrimTy,mkTyConApp bvTcNm [nVar]]
#endif
    nName      = string2SystemName "n"
    nVar       = VarTy typeNatKind nName
    nTV        = TyVar nName (embed typeNatKind)
bvConPrim _ = error $ $(curLoc) ++ "called with incorrect type"

signedConPrim :: Type -> Term
signedConPrim (tyView -> TyConApp signedTcNm _)
  = Prim "CLaSH.Sized.Internal.Signed.fromInteger#" (ForAllTy (bind nTV funTy))
  where
#if MIN_VERSION_ghc(8,2,0)
    funTy        = foldr1 mkFunTy [naturalPrimTy,integerPrimTy,mkTyConApp signedTcNm [nVar]]
#else
    funTy        = foldr1 mkFunTy [integerPrimTy,integerPrimTy,mkTyConApp signedTcNm [nVar]]
#endif
    nName      = string2SystemName "n"
    nVar       = VarTy typeNatKind nName
    nTV        = TyVar nName (embed typeNatKind)
signedConPrim _ = error $ $(curLoc) ++ "called with incorrect type"

unsignedConPrim :: Type -> Term
unsignedConPrim (tyView -> TyConApp unsignedTcNm _)
  = Prim "CLaSH.Sized.Internal.Unsigned.fromInteger#" (ForAllTy (bind nTV funTy))
  where
#if MIN_VERSION_ghc(8,2,0)
    funTy        = foldr1 mkFunTy [naturalPrimTy,integerPrimTy,mkTyConApp unsignedTcNm [nVar]]
#else
    funTy        = foldr1 mkFunTy [integerPrimTy,integerPrimTy,mkTyConApp unsignedTcNm [nVar]]
#endif
    nName        = string2SystemName "n"
    nVar         = VarTy typeNatKind nName
    nTV          = TyVar nName (embed typeNatKind)
unsignedConPrim _ = error $ $(curLoc) ++ "called with incorrect type"


-- Lift a
liftUnsigned2 :: KnownNat n
              => (Unsigned n -> Unsigned n -> Unsigned n)
              -> Type
              -> HashMap.HashMap TyConOccName TyCon
              -> Bool
              -> [Either Term Type]
              -> Proxy n
              -> Maybe Term
liftUnsigned2 = liftSized2 unsignedLiterals' mkUnsignedLit

liftSigned2 :: KnownNat n
              => (Signed n -> Signed n -> Signed n)
              -> Type
              -> HashMap.HashMap TyConOccName TyCon
              -> Bool
              -> [Either Term Type]
              -> Proxy n
              -> Maybe Term
liftSigned2 = liftSized2 signedLiterals' mkSignedLit

liftSized2 :: (KnownNat n, Integral (sized n))
           => (HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> [Integer])
           -> (Type -> Type -> Integer -> Integer -> Term)
           -> (sized n -> sized n -> sized n)
           -> Type
           -> HashMap.HashMap TyConOccName TyCon
           -> Bool
           -> [Either Term Type]
           -> Proxy n
           -> Maybe Term
liftSized2 extractLitArgs mkLit f ty tcm isSubj args p
  | Just (nTy, kn) <- extractKnownNat tcm args
  , [i,j] <- extractLitArgs tcm isSubj args
  = let val = runSizedF f i j p
    in Just $ mkLit ty nTy kn val
  | otherwise = Nothing

runSizedF
  :: (KnownNat n, Integral (sized n))
  => (sized n -> sized n -> sized n)
  -> Integer
  -> Integer
  -> Proxy n
  -> Integer
runSizedF f i j _ = toInteger $ f (fromInteger i) (fromInteger j)
