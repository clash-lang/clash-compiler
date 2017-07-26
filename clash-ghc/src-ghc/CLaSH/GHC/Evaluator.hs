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
import           Data.Bits           (shiftL,shiftR)
import qualified Data.Either         as Either
import qualified Data.HashMap.Strict as HashMap
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
                                      TypeView (..), tyView, mkFunTy,
                                      mkTyConApp, splitFunForallTy)
import           CLaSH.Core.TyCon    (TyCon, TyConOccName, tyConDataCons)
import           CLaSH.Core.TysPrim
import           CLaSH.Core.Util     (collectArgs,mkApps,mkRTree,mkVec,termType,
                                      tyNatSize)
import           CLaSH.Core.Var      (Var (..))
import           CLaSH.Util          (clogBase, flogBase, curLoc)

import qualified CLaSH.Sized.Internal.BitVector as BitVector
import qualified CLaSH.Sized.Internal.Unsigned  as Unsigned
import CLaSH.Sized.Internal.BitVector(BitVector(..))
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
      -> let val = reifyNat kn (\p -> op p (fromInteger i) (fromInteger j))
      in mkBvLit nTy kn val
      where
        op :: KnownNat n => Proxy n -> BitVector n -> Int -> Integer
        op _ u i = toInteger (BitVector.shiftL# u i)
  "CLaSH.Sized.Internal.BitVector.shiftR#"
    | Just (nTy,kn,i,j) <- bvLitIntLit tcm isSubj args
      -> let val = reifyNat kn (\p -> op p (fromInteger i) (fromInteger j))
      in mkBvLit nTy kn val
      where
        op :: KnownNat n => Proxy n -> BitVector n -> Int -> Integer
        op _ u i = toInteger (BitVector.shiftR# u i)

  "CLaSH.Sized.Internal.BitVector.rotateL#"
    | Just (nTy,kn,i,j) <- bvLitIntLit tcm isSubj args
      -> let val = reifyNat kn (\p -> op p (fromInteger i) (fromInteger j))
      in mkBvLit nTy kn val
      where
        op :: KnownNat n => Proxy n -> BitVector n -> Int -> Integer
        op _ u i = toInteger (BitVector.rotateL# u i)
  "CLaSH.Sized.Internal.BitVector.rotateR#"
    | Just (nTy,kn,i,j) <- bvLitIntLit tcm isSubj args
      -> let val = reifyNat kn (\p -> op p (fromInteger i) (fromInteger j))
      in mkBvLit nTy kn val
      where
        op :: KnownNat n => Proxy n -> BitVector n -> Int -> Integer
        op _ u i = toInteger (BitVector.rotateR# u i)

  "CLaSH.Sized.Internal.Index.eq#" | Just (i,j) <- indexLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i == j)

  "CLaSH.Sized.Internal.Index.neq#" | Just (i,j) <- indexLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i /= j)

  "CLaSH.Sized.Internal.Signed.eq#" | Just (i,j) <- signedLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i == j)

  "CLaSH.Sized.Internal.Signed.neq#" | Just (i,j) <- signedLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i /= j)

  "CLaSH.Sized.Internal.Signed.minBound#"
    | [Right litTy,kn] <- args
    , Right mb <- runExcept (tyNatSize tcm litTy)
    -> let minB = negate (2 ^ (mb - 1))
       in  mkApps signedConPrim [Right litTy,kn,Left (Literal (IntegerLiteral minB))]

  "CLaSH.Sized.Internal.Signed.maxBound#"
    | [Right litTy,kn] <- args
    , Right mb <- runExcept (tyNatSize tcm litTy)
    -> let maxB = (2 ^ (mb - 1)) - 1
       in  mkApps signedConPrim [Right litTy,kn,Left (Literal (IntegerLiteral maxB))]

  "CLaSH.Sized.Internal.Signed.toInteger#"
    | [collectArgs -> (Prim nm' _,[Right _, Left _, Left (Literal (IntegerLiteral i))])] <-
      (map (reduceConstant tcm isSubj) . Either.lefts) args
    , nm' == "CLaSH.Sized.Internal.Signed.fromInteger#"
    -> integerToIntegerLiteral i

  "CLaSH.Sized.Internal.Unsigned.eq#" | Just (i,j) <- unsignedLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i == j)

  "CLaSH.Sized.Internal.Unsigned.neq#" | Just (i,j) <- unsignedLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i /= j)

  "CLaSH.Sized.Internal.Unsigned.minBound#"
    | [Right nTy] <- args
    , Right len <- runExcept (tyNatSize tcm nTy)
    -> let kn = Left (Literal (IntegerLiteral (toInteger len)))
       in  mkApps unsignedConPrim [Right nTy,kn,Left (Literal (IntegerLiteral 0))]

  "CLaSH.Sized.Internal.Unsigned.maxBound#"
    | [Right litTy,kn] <- args
    , Right mb <- runExcept (tyNatSize tcm litTy)
    -> let maxB = (2 ^ mb) - 1
       in  mkApps unsignedConPrim [Right litTy,kn,Left (Literal (IntegerLiteral maxB))]

  "CLaSH.Sized.Internal.Unsigned.shiftL#" -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
    | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm isSubj args
      -> let val :: Integer
             val = reifyNat kn (\p -> myshift p (U i) (fromInteger j))
      in mkApps unsignedConPrim [Right nTy,Left (Literal (NaturalLiteral kn)),Left (Literal (IntegerLiteral ( val)))]
      where
        myshift :: KnownNat n => Proxy n -> Unsigned n -> Int -> Integer
        myshift _ u i = Unsigned.unsafeToInteger (Unsigned.shiftL# u i)

  "CLaSH.Sized.Internal.Unsigned.shiftR#" -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n"
    | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm isSubj args
      -> let val :: Integer
             val = reifyNat kn (\p -> myshift p (U i) (fromInteger j))
      in mkApps unsignedConPrim [Right nTy,Left (Literal (NaturalLiteral kn)),Left (Literal (IntegerLiteral ( val)))]
      where
        myshift :: KnownNat n => Proxy n -> Unsigned n -> Int -> Integer
        myshift _ u i = Unsigned.unsafeToInteger (Unsigned.shiftR# u i)


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

bitVectorLiterals :: HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> Maybe (Integer,Integer)
bitVectorLiterals = sizedLiterals "CLaSH.Sized.Internal.BitVector.fromInteger#"

indexLiterals :: HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> Maybe (Integer,Integer)
indexLiterals = sizedLiterals "CLaSH.Sized.Internal.Index.fromInteger#"

signedLiterals :: HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> Maybe (Integer,Integer)
signedLiterals = sizedLiterals "CLaSH.Sized.Internal.Signed.fromInteger#"

unsignedLiterals :: HashMap.HashMap TyConOccName TyCon -> Bool -> [Either Term Type] -> Maybe (Integer,Integer)
unsignedLiterals = sizedLiterals "CLaSH.Sized.Internal.Unsigned.fromInteger#"


-- Tries to match literal arguments to a function like (Unsigned.shiftL#  :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n)
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

-- Construct a constant value term of a sized type
mkSizedLit
  :: Term    -- type constructor?
  -> Type    -- forall n.
  -> Integer -- KnownNat
  -> Integer -- value
  -> Term
mkSizedLit conPrim nTy kn val = mkApps conPrim [Right nTy,Left (Literal (NaturalLiteral kn)),Left (Literal (IntegerLiteral ( val)))]

mkBvLit, mkSignedLit, mkUnsignedLit :: Type -> Integer -> Integer -> Term
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

bvConPrim :: Term
bvConPrim = Prim "CLaSH.Sized.Internal.BitVector.fromInteger#" (ForAllTy (bind nTV funTy))
  where
#if MIN_VERSION_ghc(8,2,0)
    funTy        = foldr1 mkFunTy [naturalPrimTy,integerPrimTy,mkTyConApp signedTcNm [nVar]]
#else
    funTy        = foldr1 mkFunTy [integerPrimTy,integerPrimTy,mkTyConApp signedTcNm [nVar]]
#endif
    signedTcNm = string2SystemName "CLaSH.Sized.Internal.BitVector.Signed"
    nName      = string2SystemName "n"
    nVar       = VarTy typeNatKind nName
    nTV        = TyVar nName (embed typeNatKind)

signedConPrim :: Term
signedConPrim = Prim "CLaSH.Sized.Internal.Signed.fromInteger#" (ForAllTy (bind nTV funTy))
  where
#if MIN_VERSION_ghc(8,2,0)
    funTy        = foldr1 mkFunTy [naturalPrimTy,integerPrimTy,mkTyConApp signedTcNm [nVar]]
#else
    funTy        = foldr1 mkFunTy [integerPrimTy,integerPrimTy,mkTyConApp signedTcNm [nVar]]
#endif
    signedTcNm = string2SystemName "CLaSH.Sized.Internal.Signed.Signed"
    nName      = string2SystemName "n"
    nVar       = VarTy typeNatKind nName
    nTV        = TyVar nName (embed typeNatKind)

unsignedConPrim :: Term
unsignedConPrim = Prim "CLaSH.Sized.Internal.Unsigned.fromInteger#" (ForAllTy (bind nTV funTy))
  where
#if MIN_VERSION_ghc(8,2,0)
    funTy        = foldr1 mkFunTy [naturalPrimTy,integerPrimTy,mkTyConApp unsignedTcNm [nVar]]
#else
    funTy        = foldr1 mkFunTy [integerPrimTy,integerPrimTy,mkTyConApp unsignedTcNm [nVar]]
#endif
    unsignedTcNm = string2SystemName "CLaSH.Sized.Internal.Unsigned.Unsigned"
    nName        = string2SystemName "n"
    nVar         = VarTy typeNatKind nName
    nTV          = TyVar nName (embed typeNatKind)
