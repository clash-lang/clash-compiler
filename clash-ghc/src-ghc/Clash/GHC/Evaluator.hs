{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017     , QBayLogic, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE UnboxedTuples     #-}

module Clash.GHC.Evaluator where

import           Control.Concurrent.Supply  (Supply,freshId)
import           Control.Monad              (ap)
import           Control.Monad.Trans.Except (runExcept)
import           Data.Bits
import           Data.Char           (chr,ord)
import qualified Data.Either         as Either
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap         as IntMap
import           Data.Maybe
  (fromMaybe, mapMaybe)
import qualified Data.List           as List
import qualified Data.Primitive.ByteArray as ByteArray
import           Data.Proxy          (Proxy)
import           Data.Reflection     (reifyNat)
import           Data.Text           (Text)
import qualified Data.Vector.Primitive as Vector
import           Debug.Trace         (trace)
import           GHC.Float
import           GHC.Int
import           GHC.Integer         (decodeDoubleInteger,encodeDoubleInteger)
import           GHC.Integer.GMP.Internals (Integer (..), BigNat (..))
#if MIN_VERSION_base(4,12,0)
import           GHC.Natural
#endif
import           GHC.Prim
import           GHC.Real            (Ratio (..))
import           GHC.TypeLits        (KnownNat)
import           GHC.Types           (IO (..))
import           GHC.Word
import           System.IO.Unsafe    (unsafeDupablePerformIO)
import qualified Unbound.Generics.LocallyNameless.Name as U
import           Unbound.Generics.LocallyNameless
  (Fresh (..), bind, embed, rebind, runFreshM, makeName)

import           BasicTypes          (Boxity (..))
import           Name                (getSrcSpan, nameOccName, occNameString)
import           PrelNames
  (typeNatAddTyFamNameKey, typeNatMulTyFamNameKey, typeNatSubTyFamNameKey)
import           SrcLoc              (wiredInSrcSpan)
import qualified TyCon
import           TysWiredIn          (tupleTyCon)
import           Unique              (getKey)

import           Clash.Class.BitPack (pack,unpack)
import           Clash.Core.DataCon  (DataCon (..), dataConInstArgTys)
import           Clash.Core.Evaluator
  (Heap (..), PrimEvaluator, Value (..), valToTerm, whnf)
import           Clash.Core.Literal  (Literal (..))
import           Clash.Core.Name
  (Name (..), NameSort (..), name2String, string2SystemName)
import           Clash.Core.Term     (Pat (..), Term (..))
import           Clash.Core.Type
  (Type (..), ConstTy (..), LitTy (..), TypeView (..), applyTy, mkFunTy, mkTyConApp,
   splitFunForallTy, tyView)
import           Clash.Core.TyCon
  (TyCon, TyConMap, TyConName, TyConOccName, tyConDataCons)
import           Clash.Core.TysPrim
import           Clash.Core.Util     (mkApps,mkRTree,mkVec,tyNatSize)
import           Clash.Core.Var      (Var (..))
import           Clash.GHC.GHC2Core  (modNameM)
import           Clash.Rewrite.Util  (mkSelectorCase)
import           Clash.Util
  (MonadUnique (..), clogBase, flogBase, curLoc)

import Clash.Promoted.Nat.Unsafe (unsafeSNat)
import qualified Clash.Sized.Internal.BitVector as BitVector
import qualified Clash.Sized.Internal.Signed    as Signed
import qualified Clash.Sized.Internal.Unsigned  as Unsigned
import Clash.Sized.Internal.BitVector(BitVector(..), Bit(..))
import Clash.Sized.Internal.Signed   (Signed   (..))
import Clash.Sized.Internal.Unsigned (Unsigned (..))
import Clash.XException (isX)

newtype PrimEvalMonad a = PEM { runPEM :: Supply -> (a,Supply) }

instance Functor PrimEvalMonad where
  fmap f m = PEM (\s -> case runPEM m s of (a,s') -> (f a, s'))

instance Applicative PrimEvalMonad where
  pure  = return
  (<*>) = ap

instance Monad PrimEvalMonad where
  return a = PEM (\s -> (a,s))
  m >>= k  = PEM (\s -> case runPEM m s of (a,s') -> runPEM (k a) s')

instance Fresh PrimEvalMonad where
  fresh (U.Fn nm _)  =
    PEM (\s -> case freshId s of
                 (!i,!s') ->  let !i' = toInteger i
                              in  (U.Fn nm i',s'))
  fresh nm@(U.Bn {}) = PEM (\s -> (nm,s))

instance MonadUnique PrimEvalMonad where
  getUniqueM = PEM (\s -> case freshId s of (!i,!s') -> (i,s'))

reduceConstant :: PrimEvaluator
reduceConstant isSubj gbl tcm h k nm ty tys args = case nm of
-----------------
-- GHC.Prim.Char#
-----------------
  "GHC.Prim.gtChar#" | Just (i,j) <- charLiterals args
    -> reduce (boolToIntLiteral (i > j))
  "GHC.Prim.geChar#" | Just (i,j) <- charLiterals args
    -> reduce (boolToIntLiteral (i >= j))
  "GHC.Prim.eqChar#" | Just (i,j) <- charLiterals args
    -> reduce (boolToIntLiteral (i == j))
  "GHC.Prim.neChar#" | Just (i,j) <- charLiterals args
    -> reduce (boolToIntLiteral (i /= j))
  "GHC.Prim.ltChar#" | Just (i,j) <- charLiterals args
    -> reduce (boolToIntLiteral (i < j))
  "GHC.Prim.leChar#" | Just (i,j) <- charLiterals args
    -> reduce (boolToIntLiteral (i <= j))
  "GHC.Prim.ord#" | [i] <- charLiterals' args
    -> reduce (integerToIntLiteral (toInteger $ ord i))

----------------
-- GHC.Prim.Int#
----------------
  "GHC.Prim.+#" | Just (i,j) <- intLiterals args
    -> reduce (integerToIntLiteral (i+j))
  "GHC.Prim.-#" | Just (i,j) <- intLiterals args
    -> reduce (integerToIntLiteral (i-j))
  "GHC.Prim.*#" | Just (i,j) <- intLiterals args
    -> reduce (integerToIntLiteral (i*j))

  "GHC.Prim.mulIntMayOflo#" | Just (i,j) <- intLiterals  args
    -> let !(I# a)  = fromInteger i
           !(I# b)  = fromInteger j
           c :: Int#
           c = mulIntMayOflo# a b
       in  reduce (integerToIntLiteral (toInteger $ I# c))

  "GHC.Prim.quotInt#" | Just (i,j) <- intLiterals args
    -> reduce (integerToIntLiteral (i `quot` j))
  "GHC.Prim.remInt#" | Just (i,j) <- intLiterals args
    -> reduce (integerToIntLiteral (i `rem` j))
  "GHC.Prim.quotRemInt#" | Just (i,j) <- intLiterals args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           (q,r)   = quotRem i j
           ret     = mkApps (Data tupDc) (map Right tyArgs ++
                    [Left (integerToIntLiteral q), Left (integerToIntLiteral r)])
       in  reduce ret

  "GHC.Prim.andI#" | Just (i,j) <- intLiterals args
    -> reduce (integerToIntLiteral (i .&. j))
  "GHC.Prim.orI#" | Just (i,j) <- intLiterals args
    -> reduce (integerToIntLiteral (i .|. j))
  "GHC.Prim.xorI#" | Just (i,j) <- intLiterals args
    -> reduce (integerToIntLiteral (i `xor` j))
  "GHC.Prim.notI#" | [i] <- intLiterals' args
    -> reduce (integerToIntLiteral (complement i))

  "GHC.Prim.negateInt#"
    | [Lit (IntLiteral i)] <- args
    -> reduce (integerToIntLiteral (negate i))

  "GHC.Prim.addIntC#" | Just (i,j) <- intLiterals args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           !(I# a)  = fromInteger i
           !(I# b)  = fromInteger j
           !(# d, c #) = addIntC# a b
       in  reduce $
           mkApps (Data tupDc) (map Right tyArgs ++
                   [ Left (Literal . IntLiteral . toInteger $ I# d)
                   , Left (Literal . IntLiteral . toInteger $ I# c)])
  "GHC.Prim.subIntC#" | Just (i,j) <- intLiterals args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           !(I# a)  = fromInteger i
           !(I# b)  = fromInteger j
           !(# d, c #) = subIntC# a b
       in  reduce $
           mkApps (Data tupDc) (map Right tyArgs ++
                   [ Left (Literal . IntLiteral . toInteger $ I# d)
                   , Left (Literal . IntLiteral . toInteger $ I# c)])

  "GHC.Prim.>#" | Just (i,j) <- intLiterals args
    -> reduce (boolToIntLiteral (i > j))
  "GHC.Prim.>=#" | Just (i,j) <- intLiterals args
    -> reduce (boolToIntLiteral (i >= j))
  "GHC.Prim.==#" | Just (i,j) <- intLiterals args
    -> reduce (boolToIntLiteral (i == j))
  "GHC.Prim./=#" | Just (i,j) <- intLiterals args
    -> reduce (boolToIntLiteral (i /= j))
  "GHC.Prim.<#" | Just (i,j) <- intLiterals args
    -> reduce (boolToIntLiteral (i < j))
  "GHC.Prim.<=#" | Just (i,j) <- intLiterals args
    -> reduce (boolToIntLiteral (i <= j))

  "GHC.Prim.chr#" | [i] <- intLiterals' args
    -> reduce (charToCharLiteral (chr $ fromInteger i))

  "GHC.Prim.int2Word#"
    | [Lit (IntLiteral i)] <- args
    -> reduce . Literal . WordLiteral . toInteger $ (fromInteger :: Integer -> Word) i -- for overflow behaviour

  "GHC.Prim.int2Float#"
    | [Lit (IntLiteral i)] <- args
    -> reduce . Literal . FloatLiteral  . toRational $ (fromInteger i :: Float)
  "GHC.Prim.int2Double#"
    | [Lit (IntLiteral i)] <- args
    -> reduce . Literal . DoubleLiteral . toRational $ (fromInteger i :: Double)

  "GHC.Prim.word2Float#"
    | [Lit (WordLiteral i)] <- args
    -> reduce . Literal . FloatLiteral  . toRational $ (fromInteger i :: Float)
  "GHC.Prim.word2Double#"
    | [Lit (WordLiteral i)] <- args
    -> reduce . Literal . DoubleLiteral . toRational $ (fromInteger i :: Double)

  "GHC.Prim.uncheckedIShiftL#"
    | [ Lit (IntLiteral i)
      , Lit (IntLiteral s)
      ] <- args
    -> reduce (integerToIntLiteral (i `shiftL` fromInteger s))
  "GHC.Prim.uncheckedIShiftRA#"
    | [ Lit (IntLiteral i)
      , Lit (IntLiteral s)
      ] <- args
    -> reduce (integerToIntLiteral (i `shiftR` fromInteger s))
  "GHC.Prim.uncheckedIShiftRL#" | Just (i,j) <- intLiterals args
    -> let !(I# a)  = fromInteger i
           !(I# b)  = fromInteger j
           c :: Int#
           c = uncheckedIShiftRL# a b
       in  reduce (integerToIntLiteral (toInteger $ I# c))

-----------------
-- GHC.Prim.Word#
-----------------
  "GHC.Prim.plusWord#" | Just (i,j) <- wordLiterals args
    -> reduce (integerToWordLiteral (i+j))

  "GHC.Prim.subWordC#" | Just (i,j) <- wordLiterals args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           !(W# a)  = fromInteger i
           !(W# b)  = fromInteger j
           !(# d, c #) = subWordC# a b
       in  reduce $
           mkApps (Data tupDc) (map Right tyArgs ++
                   [ Left (Literal . WordLiteral . toInteger $ W# d)
                   , Left (Literal . IntLiteral . toInteger $ I# c)])

  "GHC.Prim.plusWord2#" | Just (i,j) <- wordLiterals args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           !(W# a)  = fromInteger i
           !(W# b)  = fromInteger j
           !(# h', l #) = plusWord2# a b
       in  reduce $
           mkApps (Data tupDc) (map Right tyArgs ++
                   [ Left (Literal . WordLiteral . toInteger $ W# h')
                   , Left (Literal . WordLiteral . toInteger $ W# l)])

  "GHC.Prim.minusWord#" | Just (i,j) <- wordLiterals args
    -> reduce (integerToWordLiteral (i-j))
  "GHC.Prim.timesWord#" | Just (i,j) <- wordLiterals args
    -> reduce (integerToWordLiteral (i*j))

  "GHC.Prim.timesWord2#" | Just (i,j) <- wordLiterals args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           !(W# a)  = fromInteger i
           !(W# b)  = fromInteger j
           !(# h', l #) = timesWord2# a b
       in  reduce $
           mkApps (Data tupDc) (map Right tyArgs ++
                   [ Left (Literal . WordLiteral . toInteger $ W# h')
                   , Left (Literal . WordLiteral . toInteger $ W# l)])

  "GHC.Prim.quotWord#" | Just (i,j) <- wordLiterals args
    -> reduce (integerToWordLiteral (i `quot` j))
  "GHC.Prim.remWord#" | Just (i,j) <- wordLiterals args
    -> reduce (integerToWordLiteral (i `rem` j))
  "GHC.Prim.quotRemWord#" | Just (i,j) <- wordLiterals args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           (q,r)   = quotRem i j
           ret     = mkApps (Data tupDc) (map Right tyArgs ++
                    [Left (integerToWordLiteral q), Left (integerToWordLiteral r)])
       in  reduce ret
  "GHC.Prim.quotRemWord2#" | [i,j,k'] <- wordLiterals' args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           !(W# a)  = fromInteger i
           !(W# b)  = fromInteger j
           !(W# c)  = fromInteger k'
           !(# x, y #) = quotRemWord2# a b c
       in  reduce $
           mkApps (Data tupDc) (map Right tyArgs ++
                   [ Left (Literal . WordLiteral . toInteger $ W# x)
                   , Left (Literal . WordLiteral . toInteger $ W# y)])

  "GHC.Prim.and#" | Just (i,j) <- wordLiterals args
    -> reduce (integerToWordLiteral (i .&. j))
  "GHC.Prim.or#" | Just (i,j) <- wordLiterals args
    -> reduce (integerToWordLiteral (i .|. j))
  "GHC.Prim.xor#" | Just (i,j) <- wordLiterals args
    -> reduce (integerToWordLiteral (i `xor` j))
  "GHC.Prim.not#" | [i] <- wordLiterals' args
    -> reduce (integerToWordLiteral (complement i))

  "GHC.Prim.uncheckedShiftL#"
    | [ Lit (WordLiteral w)
      , Lit (IntLiteral  i)
      ] <- args
    -> reduce (Literal (WordLiteral (w `shiftL` fromInteger i)))
  "GHC.Prim.uncheckedShiftRL#"
    | [ Lit (WordLiteral w)
      , Lit (IntLiteral  i)
      ] <- args
    -> reduce (Literal (WordLiteral (w `shiftR` fromInteger i)))

  "GHC.Prim.word2Int#"
    | [Lit (WordLiteral i)] <- args
    -> reduce . Literal . IntLiteral . toInteger $ (fromInteger :: Integer -> Int) i -- for overflow behaviour

  "GHC.Prim.gtWord#" | Just (i,j) <- wordLiterals args
    -> reduce (boolToIntLiteral (i > j))
  "GHC.Prim.geWord#" | Just (i,j) <- wordLiterals args
    -> reduce (boolToIntLiteral (i >= j))
  "GHC.Prim.eqWord#" | Just (i,j) <- wordLiterals args
    -> reduce (boolToIntLiteral (i == j))
  "GHC.Prim.neWord#" | Just (i,j) <- wordLiterals args
    -> reduce (boolToIntLiteral (i /= j))
  "GHC.Prim.ltWord#" | Just (i,j) <- wordLiterals args
    -> reduce (boolToIntLiteral (i < j))
  "GHC.Prim.leWord#" | Just (i,j) <- wordLiterals args
    -> reduce (boolToIntLiteral (i <= j))

  "GHC.Prim.popCnt8#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word8) $ i
  "GHC.Prim.popCnt16#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word16) $ i
  "GHC.Prim.popCnt32#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word32) $ i
  "GHC.Prim.popCnt64#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word64) $ i
  "GHC.Prim.popCnt#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . popCount . (fromInteger :: Integer -> Word) $ i

  "GHC.Prim.clz8#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word8) $ i
  "GHC.Prim.clz16#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word16) $ i
  "GHC.Prim.clz32#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word32) $ i
  "GHC.Prim.clz64#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word64) $ i
  "GHC.Prim.clz#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . countLeadingZeros . (fromInteger :: Integer -> Word) $ i

  "GHC.Prim.ctz8#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i .&. (bit 8 - 1)
  "GHC.Prim.ctz16#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i .&. (bit 16 - 1)
  "GHC.Prim.ctz32#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i .&. (bit 32 - 1)
  "GHC.Prim.ctz64#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word64) $ i .&. (bit 64 - 1)
  "GHC.Prim.ctz#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . countTrailingZeros . (fromInteger :: Integer -> Word) $ i

  "GHC.Prim.byteSwap16#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . byteSwap16 . (fromInteger :: Integer -> Word16) $ i
  "GHC.Prim.byteSwap32#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . byteSwap32 . (fromInteger :: Integer -> Word32) $ i
  "GHC.Prim.byteSwap64#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . byteSwap64 . (fromInteger :: Integer -> Word64) $ i
  "GHC.Prim.byteSwap#" | [i] <- wordLiterals' args -- assume 64bits
    -> reduce . integerToWordLiteral . toInteger . byteSwap64 . (fromInteger :: Integer -> Word64) $ i

------------
-- Narrowing
------------
  "GHC.Prim.narrow8Int#" | [i] <- intLiterals' args
    -> let !(I# a)  = fromInteger i
           b = narrow8Int# a
       in  reduce . Literal . IntLiteral . toInteger $ I# b
  "GHC.Prim.narrow16Int#" | [i] <- intLiterals' args
    -> let !(I# a)  = fromInteger i
           b = narrow16Int# a
       in  reduce . Literal . IntLiteral . toInteger $ I# b
  "GHC.Prim.narrow32Int#" | [i] <- intLiterals' args
    -> let !(I# a)  = fromInteger i
           b = narrow32Int# a
       in  reduce . Literal . IntLiteral . toInteger $ I# b
  "GHC.Prim.narrow8Word#" | [i] <- wordLiterals' args
    -> let !(W# a)  = fromInteger i
           b = narrow8Word# a
       in  reduce . Literal . WordLiteral . toInteger $ W# b
  "GHC.Prim.narrow16Word#" | [i] <- wordLiterals' args
    -> let !(W# a)  = fromInteger i
           b = narrow16Word# a
       in  reduce . Literal . WordLiteral . toInteger $ W# b
  "GHC.Prim.narrow32Word#" | [i] <- wordLiterals' args
    -> let !(W# a)  = fromInteger i
           b = narrow32Word# a
       in  reduce . Literal . WordLiteral . toInteger $ W# b

----------
-- Double#
----------
  "GHC.Prim.>##"  | Just r <- liftDDI (>##)  args
    -> reduce r
  "GHC.Prim.>=##" | Just r <- liftDDI (>=##) args
    -> reduce r
  "GHC.Prim.==##" | Just r <- liftDDI (==##) args
    -> reduce r
  "GHC.Prim./=##" | Just r <- liftDDI (/=##) args
    -> reduce r
  "GHC.Prim.<##"  | Just r <- liftDDI (<##)  args
    -> reduce r
  "GHC.Prim.<=##" | Just r <- liftDDI (<=##) args
    -> reduce r
  "GHC.Prim.+##"  | Just r <- liftDDD (+##)  args
    -> reduce r
  "GHC.Prim.-##"  | Just r <- liftDDD (-##)  args
    -> reduce r
  "GHC.Prim.*##"  | Just r <- liftDDD (*##)  args
    -> reduce r
  "GHC.Prim./##"  | Just r <- liftDDD (/##)  args
    -> reduce r

  "GHC.Prim.negateDouble#" | Just r <- liftDD negateDouble# args
    -> reduce r
  "GHC.Prim.fabsDouble#" | Just r <- liftDD fabsDouble# args
    -> reduce r

  "GHC.Prim.double2Int#" | [i] <- doubleLiterals' args
    -> let !(D# a) = fromRational i
           r = double2Int# a
       in  reduce . Literal . IntLiteral . toInteger $ I# r
  "GHC.Prim.double2Float#"
    | [Lit (DoubleLiteral d)] <- args
    -> reduce (Literal (FloatLiteral (toRational (fromRational d :: Float))))


  "GHC.Prim.expDouble#" | Just r <- liftDD expDouble# args
    -> reduce r
  "GHC.Prim.logDouble#" | Just r <- liftDD logDouble# args
    -> reduce r
  "GHC.Prim.sqrtDouble#" | Just r <- liftDD sqrtDouble# args
    -> reduce r
  "GHC.Prim.sinDouble#" | Just r <- liftDD sinDouble# args
    -> reduce r
  "GHC.Prim.cosDouble#" | Just r <- liftDD cosDouble# args
    -> reduce r
  "GHC.Prim.tanDouble#" | Just r <- liftDD tanDouble# args
    -> reduce r
  "GHC.Prim.asinDouble#" | Just r <- liftDD asinDouble# args
    -> reduce r
  "GHC.Prim.acosDouble#" | Just r <- liftDD acosDouble# args
    -> reduce r
  "GHC.Prim.atanDouble#" | Just r <- liftDD atanDouble# args
    -> reduce r
  "GHC.Prim.sinhDouble#" | Just r <- liftDD sinhDouble# args
    -> reduce r
  "GHC.Prim.coshDouble#" | Just r <- liftDD coshDouble# args
    -> reduce r
  "GHC.Prim.tanhDouble#" | Just r <- liftDD tanhDouble# args
    -> reduce r
  "GHC.Prim.**##" | Just r <- liftDDD (**##) args
    -> reduce r
-- decodeDouble_2Int# :: Double# -> (#Int#, Word#, Word#, Int##)
  "GHC.Prim.decodeDouble_2Int#" | [i] <- doubleLiterals' args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           !(D# a) = fromRational i
           !(# p, q, r, s #) = decodeDouble_2Int# a
       in reduce $
          mkApps (Data tupDc) (map Right tyArgs ++
                   [ Left (Literal . IntLiteral  . toInteger $ I# p)
                   , Left (Literal . WordLiteral . toInteger $ W# q)
                   , Left (Literal . WordLiteral . toInteger $ W# r)
                   , Left (Literal . IntLiteral  . toInteger $ I# s)])
-- decodeDouble_Int64# :: Double# -> (#Int#, Int##)
  "GHC.Prim.decodeDouble_Int64#" | [i] <- doubleLiterals' args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           !(D# a) = fromRational i
           !(# p, q #) = decodeDouble_Int64# a
       in reduce $
          mkApps (Data tupDc) (map Right tyArgs ++
                   [ Left (Literal . IntLiteral  . toInteger $ I# p)
                   , Left (Literal . IntLiteral  . toInteger $ I# q)])

--------
-- Float
--------
  "GHC.Prim.gtFloat#"  | Just r <- liftFFI gtFloat# args
    -> reduce r
  "GHC.Prim.geFloat#"  | Just r <- liftFFI geFloat# args
    -> reduce r
  "GHC.Prim.eqFloat#"  | Just r <- liftFFI eqFloat# args
    -> reduce r
  "GHC.Prim.neFloat#"  | Just r <- liftFFI neFloat# args
    -> reduce r
  "GHC.Prim.ltFloat#"  | Just r <- liftFFI ltFloat# args
    -> reduce r
  "GHC.Prim.leFloat#"  | Just r <- liftFFI leFloat# args
    -> reduce r

  "GHC.Prim.plusFloat#"  | Just r <- liftFFF plusFloat# args
    -> reduce r
  "GHC.Prim.minusFloat#"  | Just r <- liftFFF minusFloat# args
    -> reduce r
  "GHC.Prim.timesFloat#"  | Just r <- liftFFF timesFloat# args
    -> reduce r
  "GHC.Prim.divideFloat#"  | Just r <- liftFFF divideFloat# args
    -> reduce r

  "GHC.Prim.negateFloat#"  | Just r <- liftFF negateFloat# args
    -> reduce r
  "GHC.Prim.fabsFloat#"  | Just r <- liftFF fabsFloat# args
    -> reduce r

  "GHC.Prim.float2Int#" | [i] <- floatLiterals' args
    -> let !(F# a) = fromRational i
           r = float2Int# a
       in  reduce . Literal . IntLiteral . toInteger $ I# r

  "GHC.Prim.expFloat#"  | Just r <- liftFF expFloat# args
    -> reduce r
  "GHC.Prim.logFloat#"  | Just r <- liftFF logFloat# args
    -> reduce r
  "GHC.Prim.sqrtFloat#"  | Just r <- liftFF sqrtFloat# args
    -> reduce r
  "GHC.Prim.sinFloat#"  | Just r <- liftFF sinFloat# args
    -> reduce r
  "GHC.Prim.cosFloat#"  | Just r <- liftFF cosFloat# args
    -> reduce r
  "GHC.Prim.tanFloat#"  | Just r <- liftFF tanFloat# args
    -> reduce r
  "GHC.Prim.asinFloat#"  | Just r <- liftFF asinFloat# args
    -> reduce r
  "GHC.Prim.acosFloat#"  | Just r <- liftFF acosFloat# args
    -> reduce r
  "GHC.Prim.atanFloat#"  | Just r <- liftFF atanFloat# args
    -> reduce r
  "GHC.Prim.sinhFloat#"  | Just r <- liftFF sinhFloat# args
    -> reduce r
  "GHC.Prim.coshFloat#"  | Just r <- liftFF coshFloat# args
    -> reduce r
  "GHC.Prim.tanhFloat#"  | Just r <- liftFF tanhFloat# args
    -> reduce r
  "GHC.Prim.powerFloat#"  | Just r <- liftFFF powerFloat# args
    -> reduce r

#if MIN_VERSION_base(4,12,0)
  -- GHC.Float.asinh  -- XXX: Very fragile
  --  $w$casinh is the Double specialisation of asinh
  --  $w$casinh1 is the Float specialisation of asinh
  "GHC.Float.$w$casinh" | Just r <- liftDD go args
    -> reduce r
    where go f = case asinh (D# f) of
                   D# f' -> f'
  "GHC.Float.$w$casinh1" | Just r <- liftFF go args
    -> reduce r
    where go f = case asinh (F# f) of
                   F# f' -> f'
#endif

  "GHC.Prim.float2Double#" | [i] <- floatLiterals' args
    -> let !(F# a) = fromRational i
           r = float2Double# a
       in  reduce . Literal . DoubleLiteral . toRational $ D# r


  "GHC.Prim.newByteArray#"
    | [iV,PrimVal rwNm rwTy _ _] <- args
    , [i] <- intLiterals' [iV]
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           Heap (gh,p) ph ids = h
           lit = Literal (ByteArrayLiteral (Vector.replicate (fromInteger i) 0))
           h' = Heap (IntMap.insert p lit gh,p+1) ph ids
           mbaTy = mkFunTy intPrimTy (last tyArgs)
           newE = mkApps (Data tupDc) (map Right tyArgs ++
                    [Left (Prim rwNm rwTy)
                    ,Left (mkApps (Prim "GHC.Prim.MutableByteArray#" mbaTy)
                                  [Left (Literal . IntLiteral $ toInteger p)])
                    ])
       in Just (h',k,newE)

  "GHC.Prim.setByteArray#"
    | [PrimVal _mbaNm _mbaTy _ [baV]
      ,offV,lenV,cV
      ,PrimVal rwNm rwTy _ _
      ] <- args
    , [ba,off,len,c] <- intLiterals' [baV,offV,lenV,cV]
    -> let Heap (gh,p) ph ids = h
           Just (Literal (ByteArrayLiteral (Vector.Vector voff vlen ba1))) =
              IntMap.lookup (fromInteger ba) gh
           !(I# off') = fromInteger off
           !(I# len') = fromInteger len
           !(I# c')   = fromInteger c
           ba2 = unsafeDupablePerformIO $ do
                  ByteArray.MutableByteArray mba <- ByteArray.unsafeThawByteArray ba1
                  svoid (setByteArray# mba off' len' c')
                  ByteArray.unsafeFreezeByteArray (ByteArray.MutableByteArray mba)
           ba3 = Literal (ByteArrayLiteral (Vector.Vector voff vlen ba2))
           h'  = Heap (IntMap.insert (fromInteger ba) ba3 gh,p) ph ids
       in Just (h',k,Prim rwNm rwTy)

  "GHC.Prim.writeWordArray#"
    | [PrimVal _mbaNm _mbaTy _  [baV]
      ,iV,wV
      ,PrimVal rwNm rwTy _ _
      ] <- args
    , [ba,i] <- intLiterals' [baV,iV]
    , [w] <- wordLiterals' [wV]
    -> let Heap (gh,p) ph ids = h
           Just (Literal (ByteArrayLiteral (Vector.Vector off len ba1))) =
              IntMap.lookup (fromInteger ba) gh
           !(I# i') = fromInteger i
           !(W# w') = fromIntegral w
           ba2 = unsafeDupablePerformIO $ do
                  ByteArray.MutableByteArray mba <- ByteArray.unsafeThawByteArray ba1
                  svoid (writeWordArray# mba i' w')
                  ByteArray.unsafeFreezeByteArray (ByteArray.MutableByteArray mba)
           ba3 = Literal (ByteArrayLiteral (Vector.Vector off len ba2))
           h'  = Heap (IntMap.insert (fromInteger ba) ba3 gh,p) ph ids
       in Just (h',k,Prim rwNm rwTy)

  "GHC.Prim.unsafeFreezeByteArray#"
    | [PrimVal _mbaNm _mbaTy _ [baV]
      ,PrimVal rwNm rwTy _ _
      ] <- args
    , [ba] <-  intLiterals' [baV]
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           Heap (gh,_) _ _ = h
           Just ba' = IntMap.lookup (fromInteger ba) gh
       in  reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                      [Left (Prim rwNm rwTy)
                      ,Left ba'])

  "GHC.Prim.sizeofByteArray#"
    | [Lit (ByteArrayLiteral ba)] <- args
    -> reduce (Literal (IntLiteral (toInteger (Vector.length ba))))

  "GHC.Prim.indexWordArray#"
    | [Lit (ByteArrayLiteral (Vector.Vector _ _ (ByteArray.ByteArray ba))),iV] <- args
    , [i] <- intLiterals' [iV]
    -> let !(I# i') = fromInteger i
           !w       = indexWordArray# ba i'
       in  reduce (Literal (WordLiteral (toInteger (W# w))))

  "GHC.Prim.getSizeofMutBigNat#"
    | [PrimVal _mbaNm _mbaTy _ [baV]
      ,PrimVal rwNm rwTy _ _
      ] <- args
    , [ba] <- intLiterals' [baV]
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           Heap (gh,_) _ _ = h
           Just (Literal (ByteArrayLiteral ba')) = IntMap.lookup (fromInteger ba) gh
           lit = Literal (IntLiteral (toInteger (Vector.length ba')))
       in  reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                      [Left (Prim rwNm rwTy)
                      ,Left lit])

  "GHC.Prim.resizeMutableByteArray#"
    | [PrimVal mbaNm mbaTy _ [baV]
      ,iV
      ,PrimVal rwNm rwTy _ _
      ] <- args
    , [ba,i] <- intLiterals' [baV,iV]
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           Heap (gh,p) ph ids = h
           Just (Literal (ByteArrayLiteral (Vector.Vector 0 _ ba1)))
            = IntMap.lookup (fromInteger ba) gh
           !(I# i') = fromInteger i
           ba2 = unsafeDupablePerformIO $ do
                   ByteArray.MutableByteArray mba <- ByteArray.unsafeThawByteArray ba1
                   mba' <- IO (\s -> case resizeMutableByteArray# mba i' s of
                                 (# s', mba' #) -> (# s', ByteArray.MutableByteArray mba' #))
                   ByteArray.unsafeFreezeByteArray mba'
           ba3 = Literal (ByteArrayLiteral (Vector.Vector 0 (I# i') ba2))
           h'  = Heap (IntMap.insert p ba3 gh,p+1) ph ids
           newE = mkApps (Data tupDc) (map Right tyArgs ++
                    [Left (Prim rwNm rwTy)
                    ,Left (mkApps (Prim mbaNm mbaTy)
                                  [Left (Literal . IntLiteral $ toInteger p)])
                    ])
       in  Just (h',k,newE)

  "GHC.Prim.shrinkMutableByteArray#"
    | [PrimVal _mbaNm _mbaTy _ [baV]
      ,lenV
      ,PrimVal rwNm rwTy _ _
      ] <- args
    , [ba,len] <- intLiterals' [baV,lenV]
    -> let Heap (gh,p) ph ids = h
           Just (Literal (ByteArrayLiteral (Vector.Vector voff vlen ba1))) =
              IntMap.lookup (fromInteger ba) gh
           !(I# len') = fromInteger len
           ba2 = unsafeDupablePerformIO $ do
                  ByteArray.MutableByteArray mba <- ByteArray.unsafeThawByteArray ba1
                  svoid (shrinkMutableByteArray# mba len')
                  ByteArray.unsafeFreezeByteArray (ByteArray.MutableByteArray mba)
           ba3 = Literal (ByteArrayLiteral (Vector.Vector voff vlen ba2))
           h'  = Heap (IntMap.insert (fromInteger ba) ba3 gh,p) ph ids
       in Just (h',k,Prim rwNm rwTy)

  "GHC.Prim.copyByteArray#"
    | [Lit (ByteArrayLiteral (Vector.Vector _ _ (ByteArray.ByteArray src_ba)))
      ,src_offV
      ,PrimVal _mbaNm _mbaTy _ [dst_mbaV]
      ,dst_offV, nV
      ,PrimVal rwNm rwTy _ _
      ] <- args
    , [src_off,dst_mba,dst_off,n] <- intLiterals' [src_offV,dst_mbaV,dst_offV,nV]
    -> let Heap (gh,p) ph ids = h
           Just (Literal (ByteArrayLiteral (Vector.Vector voff vlen dst_ba))) =
              IntMap.lookup (fromInteger dst_mba) gh
           !(I# src_off') = fromInteger src_off
           !(I# dst_off') = fromInteger dst_off
           !(I# n')       = fromInteger n
           ba2 = unsafeDupablePerformIO $ do
                  ByteArray.MutableByteArray dst_mba1 <- ByteArray.unsafeThawByteArray dst_ba
                  svoid (copyByteArray# src_ba src_off' dst_mba1 dst_off' n')
                  ByteArray.unsafeFreezeByteArray (ByteArray.MutableByteArray dst_mba1)
           ba3 = Literal (ByteArrayLiteral (Vector.Vector voff vlen ba2))
           h'  = Heap (IntMap.insert (fromInteger dst_mba) ba3 gh,p) ph ids
       in Just (h',k,Prim rwNm rwTy)

  "GHC.Prim.readWordArray#"
    | [PrimVal _mbaNm _mbaTy _  [baV]
      ,offV
      ,PrimVal rwNm rwTy _ _
      ] <- args
    , [ba,off] <- intLiterals' [baV,offV]
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           Heap (gh,_) _ _ = h
           Just (Literal (ByteArrayLiteral (Vector.Vector _ _ ba1))) =
              IntMap.lookup (fromInteger ba) gh
           !(I# off') = fromInteger off
           w = unsafeDupablePerformIO $ do
                  ByteArray.MutableByteArray mba <- ByteArray.unsafeThawByteArray ba1
                  IO (\s -> case readWordArray# mba off' s of
                        (# s', w' #) -> (# s',  W# w' #))
           newE = mkApps (Data tupDc) (map Right tyArgs ++
                    [Left (Prim rwNm rwTy)
                    ,Left (Literal (WordLiteral (toInteger w)))
                    ])
       in reduce newE

-- decodeFloat_Int# :: Float# -> (#Int#, Int##)
  "GHC.Prim.decodeFloat_Int#" | [i] <- floatLiterals' args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           !(F# a) = fromRational i
           !(# p, q #) = decodeFloat_Int# a
       in reduce $
          mkApps (Data tupDc) (map Right tyArgs ++
                   [ Left (Literal . IntLiteral  . toInteger $ I# p)
                   , Left (Literal . IntLiteral  . toInteger $ I# q)])


  "GHC.Prim.tagToEnum#"
    | [ConstTy (TyCon tcN)] <- tys
    , [Lit (IntLiteral i)]  <- args
    -> let dc = do { tc <- HashMap.lookup (nameOcc tcN) tcm
                   ; let dcs = tyConDataCons tc
                   ; List.find ((== (i+1)) . toInteger . dcTag) dcs
                   }
       in ((h,k,) . Data) <$> dc


  "GHC.Classes.geInt" | Just (i,j) <- intCLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i >= j))

  "GHC.Classes.&&"
    | [DC lCon _
      ,DC rCon _] <- args
    -> reduce $ boolToBoolLiteral tcm ty
         ((name2String (dcName lCon) == "GHC.Types.True") &&
          (name2String (dcName rCon) == "GHC.Types.True"))

  "GHC.Classes.||"
    | [DC lCon _
      ,DC rCon _] <- args
    -> reduce $ boolToBoolLiteral tcm ty
         ((name2String (dcName lCon) == "GHC.Types.True") ||
          (name2String (dcName rCon) == "GHC.Types.True"))

  "GHC.Classes.divInt#" | Just (i,j) <- intLiterals args
    -> reduce (integerToIntLiteral (i `div` j))

  "GHC.Classes.not"
    | [DC bCon _] <- args
    -> reduce (boolToBoolLiteral tcm ty (name2String (dcName bCon) == "GHC.Types.False"))

  "GHC.Integer.Logarithms.integerLogBase#"
    | Just (a,b) <- integerLiterals args
    , Just c <- flogBase a b
    -> (reduce . Literal . IntLiteral . toInteger) c

  "GHC.Integer.Type.smallInteger"
    | [Lit (IntLiteral i)] <- args
    -> reduce (Literal (IntegerLiteral i))

  "GHC.Integer.Type.integerToInt"
    | [i] <- integerLiterals' args
    -> reduce (integerToIntLiteral i)

  "GHC.Integer.Type.decodeDoubleInteger" -- :: Double# -> (#Integer, Int##)
    | [Lit (DoubleLiteral i)] <- args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           !(D# a)  = fromRational i
           !(# b, c #) = decodeDoubleInteger a
    in reduce $
       mkApps (Data tupDc) (map Right tyArgs ++
                [ Left (integerToIntegerLiteral b)
                , Left (integerToIntLiteral . toInteger $ I# c)])

  "GHC.Integer.Type.encodeDoubleInteger" -- :: Integer -> Int# -> Double#
    | [iV, Lit (IntLiteral j)] <- args
    , [i] <- integerLiterals' [iV]
    -> let !(I# k') = fromInteger j
           r = encodeDoubleInteger i k'
    in  reduce . Literal . DoubleLiteral . toRational $ D# r

  "GHC.Integer.Type.quotRemInteger" -- :: Integer -> Integer -> (#Integer, Integer#)
    | [i, j] <- integerLiterals' args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           (q,r) = quotRem i j
    in reduce $
         mkApps (Data tupDc) (map Right tyArgs ++
                [ Left (integerToIntegerLiteral q)
                , Left (integerToIntegerLiteral r)])

  "GHC.Integer.Type.plusInteger" | Just (i,j) <- integerLiterals args
    -> reduce (integerToIntegerLiteral (i+j))

  "GHC.Integer.Type.minusInteger" | Just (i,j) <- integerLiterals args
    -> reduce (integerToIntegerLiteral (i-j))

  "GHC.Integer.Type.timesInteger" | Just (i,j) <- integerLiterals args
    -> reduce (integerToIntegerLiteral (i*j))

  "GHC.Integer.Type.negateInteger"
    | [i] <- integerLiterals' args
    -> reduce (integerToIntegerLiteral (negate i))

  "GHC.Integer.Type.divInteger" | Just (i,j) <- integerLiterals args
    -> reduce (integerToIntegerLiteral (i `div` j))

  "GHC.Integer.Type.modInteger" | Just (i,j) <- integerLiterals args
    -> reduce (integerToIntegerLiteral (i `mod` j))

  "GHC.Integer.Type.quotInteger" | Just (i,j) <- integerLiterals args
    -> reduce (integerToIntegerLiteral (i `quot` j))

  "GHC.Integer.Type.remInteger" | Just (i,j) <- integerLiterals args
    -> reduce (integerToIntegerLiteral (i `rem` j))

  "GHC.Integer.Type.divModInteger" | Just (i,j) <- integerLiterals args
    -> let (_,tyView -> TyConApp ubTupTcNm [liftedKi,_,intTy,_]) = splitFunForallTy ty
           (Just ubTupTc) = HashMap.lookup (nameOcc ubTupTcNm) tcm
           [ubTupDc] = tyConDataCons ubTupTc
           (d,m) = divMod i j
       in  reduce $
           mkApps (Data ubTupDc) [ Right liftedKi, Right liftedKi
                                 , Right intTy,    Right intTy
                                 , Left (Literal (IntegerLiteral d))
                                 , Left (Literal (IntegerLiteral m))
                                 ]

  "GHC.Integer.Type.gtInteger" | Just (i,j) <- integerLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i > j))

  "GHC.Integer.Type.geInteger" | Just (i,j) <- integerLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i >= j))

  "GHC.Integer.Type.eqInteger" | Just (i,j) <- integerLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i == j))

  "GHC.Integer.Type.neqInteger" | Just (i,j) <- integerLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i /= j))

  "GHC.Integer.Type.ltInteger" | Just (i,j) <- integerLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i < j))

  "GHC.Integer.Type.leInteger" | Just (i,j) <- integerLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i <= j))

  "GHC.Integer.Type.gtInteger#" | Just (i,j) <- integerLiterals args
    -> reduce (boolToIntLiteral (i > j))

  "GHC.Integer.Type.geInteger#" | Just (i,j) <- integerLiterals args
    -> reduce (boolToIntLiteral (i >= j))

  "GHC.Integer.Type.eqInteger#" | Just (i,j) <- integerLiterals args
    -> reduce (boolToIntLiteral (i == j))

  "GHC.Integer.Type.neqInteger#" | Just (i,j) <- integerLiterals args
    -> reduce (boolToIntLiteral (i /= j))

  "GHC.Integer.Type.ltInteger#" | Just (i,j) <- integerLiterals args
    -> reduce (boolToIntLiteral (i < j))

  "GHC.Integer.Type.leInteger#" | Just (i,j) <- integerLiterals args
    -> reduce (boolToIntLiteral (i <= j))

  "GHC.Integer.Type.shiftRInteger"
    | [iV, Lit (IntLiteral j)] <- args
    , [i] <- integerLiterals' [iV]
    -> reduce (integerToIntegerLiteral (i `shiftR` fromInteger j))

  "GHC.Integer.Type.shiftLInteger"
    | [iV, Lit (IntLiteral j)] <- args
    , [i] <- integerLiterals' [iV]
    -> reduce (integerToIntegerLiteral (i `shiftL` fromInteger j))

  "GHC.Integer.Type.wordToInteger"
    | [Lit (WordLiteral w)] <- args
    -> reduce (Literal (IntegerLiteral w))

  "GHC.Natural.NatS#"
    | [Lit (WordLiteral w)] <- args
    -> reduce (Literal (NaturalLiteral w))

#if MIN_VERSION_base(4,12,0)
  "GHC.Natural.naturalToInteger"
    | [i] <- naturalLiterals' args
    -> reduce (Literal (IntegerLiteral i))

  -- GHC.shiftLNatural --- XXX: Fragile worker of GHC.shiflLNatural
  "GHC.Natural.$wshiftLNatural"
    | [nV,iV] <- args
    , [n] <- naturalLiterals' [nV]
    , [i] <- intLiterals' [iV]
    -> let r = shiftLNatural (fromInteger n) (fromInteger i)
       in  reduce (Literal (NaturalLiteral (toInteger r)))
#endif

  -- GHC.Real.^  -- XXX: Very fragile
  --   ^_f, $wf, $wf1 are specialisations of the internal function f in the implementation of (^) in GHC.Real
  "GHC.Real.^_f"  -- :: Integer -> Integer -> Integer
    | [i,j] <- integerLiterals' args
    -> reduce (integerToIntegerLiteral $ i ^ j)
  "GHC.Real.$wf"  -- :: Integer -> Int# -> Integer
    | [iV, Lit (IntLiteral j)] <- args
    , [i] <- integerLiterals' [iV]
    -> reduce (integerToIntegerLiteral $ i ^ j)
  "GHC.Real.$wf1" -- :: Int# -> Int# -> Int#
    | [Lit (IntLiteral i), Lit (IntLiteral j)] <- args
    -> reduce (integerToIntLiteral $ i ^ j)


  "GHC.TypeLits.natVal"
    | [Lit (NaturalLiteral n), _] <- args
    -> reduce (integerToIntegerLiteral n)

  "GHC.TypeNats.natVal"
    | [Lit (NaturalLiteral n), _] <- args
    -> reduce (Literal (NaturalLiteral n))

  "GHC.Types.C#"
    | isSubj
    , [Lit (CharLiteral c)] <- args
    ->  let (_,tyView -> TyConApp charTcNm []) = splitFunForallTy ty
            (Just charTc) = HashMap.lookup (nameOcc charTcNm) tcm
            [charDc] = tyConDataCons charTc
        in  reduce (mkApps (Data charDc) [Left (Literal (CharLiteral c))])

  "GHC.Types.I#"
    | isSubj
    , [Lit (IntLiteral i)] <- args
    ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
            (Just intTc) = HashMap.lookup (nameOcc intTcNm) tcm
            [intDc] = tyConDataCons intTc
        in  reduce (mkApps (Data intDc) [Left (Literal (IntLiteral i))])
  "GHC.Int.I8#"
    | isSubj
    , [Lit (IntLiteral i)] <- args
    ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
            (Just intTc) = HashMap.lookup (nameOcc intTcNm) tcm
            [intDc] = tyConDataCons intTc
        in  reduce (mkApps (Data intDc) [Left (Literal (IntLiteral i))])
  "GHC.Int.I16#"
    | isSubj
    , [Lit (IntLiteral i)] <- args
    ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
            (Just intTc) = HashMap.lookup (nameOcc intTcNm) tcm
            [intDc] = tyConDataCons intTc
        in  reduce (mkApps (Data intDc) [Left (Literal (IntLiteral i))])
  "GHC.Int.I32#"
    | isSubj
    , [Lit (IntLiteral i)] <- args
    ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
            (Just intTc) = HashMap.lookup (nameOcc intTcNm) tcm
            [intDc] = tyConDataCons intTc
        in  reduce (mkApps (Data intDc) [Left (Literal (IntLiteral i))])
  "GHC.Int.I64#"
    | isSubj
    , [Lit (IntLiteral i)] <- args
    ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
            (Just intTc) = HashMap.lookup (nameOcc intTcNm) tcm
            [intDc] = tyConDataCons intTc
        in  reduce (mkApps (Data intDc) [Left (Literal (IntLiteral i))])

  "GHC.Types.W#"
    | isSubj
    , [Lit (WordLiteral c)] <- args
    ->  let (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
            (Just wordTc) = HashMap.lookup (nameOcc wordTcNm) tcm
            [wordDc] = tyConDataCons wordTc
        in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral c))])
  "GHC.Word.W8#"
    | isSubj
    , [Lit (WordLiteral c)] <- args
    ->  let (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
            (Just wordTc) = HashMap.lookup (nameOcc wordTcNm) tcm
            [wordDc] = tyConDataCons wordTc
        in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral c))])
  "GHC.Word.W16#"
    | isSubj
    , [Lit (WordLiteral c)] <- args
    ->  let (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
            (Just wordTc) = HashMap.lookup (nameOcc wordTcNm) tcm
            [wordDc] = tyConDataCons wordTc
        in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral c))])
  "GHC.Word.W32#"
    | isSubj
    , [Lit (WordLiteral c)] <- args
    ->  let (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
            (Just wordTc) = HashMap.lookup (nameOcc wordTcNm) tcm
            [wordDc] = tyConDataCons wordTc
        in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral c))])
  "GHC.Word.W64#"
    | [Lit (WordLiteral c)] <- args
    ->  let (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
            (Just wordTc) = HashMap.lookup (nameOcc wordTcNm) tcm
            [wordDc] = tyConDataCons wordTc
        in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral c))])

  "GHC.Float.$w$sfromRat''" -- XXX: Very fragile
    | [Lit (IntLiteral _minEx)
      ,Lit (IntLiteral matDigs)
      ,nV
      ,dV] <- args
    , [n,d] <- integerLiterals' [nV,dV]
    -> case fromInteger matDigs of
          matDigs'
            | matDigs' == floatDigits (undefined :: Float)
            -> reduce (Literal (FloatLiteral (toRational (fromRational (n :% d) :: Float))))
            | matDigs' == floatDigits (undefined :: Double)
            -> reduce (Literal (DoubleLiteral (toRational (fromRational (n :% d) :: Double))))
          _ -> error $ $(curLoc) ++ "GHC.Float.$w$sfromRat'': Not a Float or Double"

  "GHC.Float.$w$sfromRat''1" -- XXX: Very fragile
    | [Lit (IntLiteral _minEx)
      ,Lit (IntLiteral matDigs)
      ,nV
      ,dV] <- args
    , [n,d] <- integerLiterals' [nV,dV]
    -> case fromInteger matDigs of
          matDigs'
            | matDigs' == floatDigits (undefined :: Float)
            -> reduce (Literal (FloatLiteral (toRational (fromRational (n :% d) :: Float))))
            | matDigs' == floatDigits (undefined :: Double)
            -> reduce (Literal (DoubleLiteral (toRational (fromRational (n :% d) :: Double))))
          _ -> error $ $(curLoc) ++ "GHC.Float.$w$sfromRat'': Not a Float or Double"

  "GHC.Integer.Type.doubleFromInteger"
    | [i] <- integerLiterals' args
    -> reduce (Literal (DoubleLiteral (toRational (fromInteger i :: Double))))

  "GHC.Base.eqString"
    | [PrimVal _ _ _ [Lit (StringLiteral s1)]
      ,PrimVal _ _ _ [Lit (StringLiteral s2)]
      ] <- args
    -> reduce (boolToBoolLiteral tcm ty (s1 == s2))
    | otherwise -> error (show args)


  "Clash.Class.BitPack.packDouble#" -- :: Double -> BitVector 64
    | [DC _ [Left arg]] <- args
    , (h2,[],Literal (DoubleLiteral i)) <- whnf reduceConstant gbl tcm True (h,[],arg)
    -> let resTyInfo = extractTySizeInfo tcm ty tys
       in  Just (h2,k,mkBitVectorLit' resTyInfo 0 (BitVector.unsafeToInteger $ (pack :: Double -> BitVector 64) $ fromRational i))

  "Clash.Class.BitPack.packFloat#" -- :: Float -> BitVector 32
    | [DC _ [Left arg]] <- args
    , (h2,[],Literal (FloatLiteral i)) <- whnf reduceConstant gbl tcm True (h,[],arg)
    -> let resTyInfo = extractTySizeInfo tcm ty tys
       in  Just (h2,k,mkBitVectorLit' resTyInfo 0 (BitVector.unsafeToInteger $ (pack :: Float -> BitVector 32) $ fromRational i))

  "Clash.Class.BitPack.unpackFloat#"
    | [i] <- bitVectorLiterals' args
    -> reduce (Literal (FloatLiteral (toRational $ (unpack :: BitVector 32 -> Float) (toBV i))))

  "Clash.Class.BitPack.unpackDouble#"
    | [i] <- bitVectorLiterals' args
    -> reduce (Literal (DoubleLiteral (toRational $ (unpack :: BitVector 64 -> Double) (toBV i))))


  "Clash.Promoted.Nat.powSNat"
    | [Right a, Right b] <- map (runExcept . tyNatSize tcm) tys
    -> let c = case a of
                 2 -> 1 `shiftL` (fromInteger b)
                 _ -> a ^ b
           (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
           (Just snatTc) = HashMap.lookup (nameOcc snatTcNm) tcm
           [snatDc] = tyConDataCons snatTc
       in  reduce $
           mkApps (Data snatDc) [ Right (LitTy (NumTy c))
                                , Left (Literal (NaturalLiteral c))]

  "Clash.Promoted.Nat.flogBaseSNat"
    | [_,_,Right a, Right b] <- map (runExcept . tyNatSize tcm) tys
    , Just c <- flogBase a b
    , let c' = toInteger c
    -> let (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
           (Just snatTc) = HashMap.lookup (nameOcc snatTcNm) tcm
           [snatDc] = tyConDataCons snatTc
       in  reduce $
           mkApps (Data snatDc) [ Right (LitTy (NumTy c'))
                                , Left (Literal (NaturalLiteral c'))]

  "Clash.Promoted.Nat.clogBaseSNat"
    | [_,_,Right a, Right b] <- map (runExcept . tyNatSize tcm) tys
    , Just c <- clogBase a b
    , let c' = toInteger c
    -> let (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
           (Just snatTc) = HashMap.lookup (nameOcc snatTcNm) tcm
           [snatDc] = tyConDataCons snatTc
       in  reduce $
           mkApps (Data snatDc) [ Right (LitTy (NumTy c'))
                                , Left (Literal (NaturalLiteral c'))]

  "Clash.Promoted.Nat.logBaseSNat"
    | [_,Right a, Right b] <- map (runExcept . tyNatSize tcm) tys
    , Just c <- flogBase a b
    , let c' = toInteger c
    -> let (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
           (Just snatTc) = HashMap.lookup (nameOcc snatTcNm) tcm
           [snatDc] = tyConDataCons snatTc
       in  reduce $
           mkApps (Data snatDc) [ Right (LitTy (NumTy c'))
                                , Left (Literal (NaturalLiteral c'))]

------------
-- BitVector
------------
-- Constructor
  "Clash.Sized.Internal.BitVector.BV"
    | [Right _] <- map (runExcept . tyNatSize tcm) tys
    , Just (m,i) <- integerLiterals args
    -> let resTyInfo = extractTySizeInfo tcm ty tys
       in  reduce (mkBitVectorLit' resTyInfo m i)

  "Clash.Sized.Internal.BitVector.Bit"
    | Just (m,i) <- integerLiterals args
    -> reduce (mkBitLit ty m i)

-- Initialisation
  "Clash.Sized.Internal.BitVector.size#"
    | Just (_, kn) <- extractKnownNat tcm tys
    -> let (_,tyView -> TyConApp intTcNm _) = splitFunForallTy ty
           (Just intTc) = HashMap.lookup (nameOcc intTcNm) tcm
           [intCon] = tyConDataCons intTc
       in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral kn))])
  "Clash.Sized.Internal.BitVector.maxIndex#"
    | Just (_, kn) <- extractKnownNat tcm tys
    -> let (_,tyView -> TyConApp intTcNm _) = splitFunForallTy ty
           (Just intTc) = HashMap.lookup (nameOcc intTcNm) tcm
           [intCon] = tyConDataCons intTc
       in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral (kn-1)))])

-- Construction
  "Clash.Sized.Internal.BitVector.high"
    -> reduce (mkBitLit ty 0 1)
  "Clash.Sized.Internal.BitVector.low"
    -> reduce (mkBitLit ty 0 0)

-- Eq
  "Clash.Sized.Internal.BitVector.eq##" | [(0,i),(0,j)] <- bitLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i == j))
  "Clash.Sized.Internal.BitVector.neq##" | [(0,i),(0,j)] <- bitLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i /= j))

-- Ord
  "Clash.Sized.Internal.BitVector.lt##" | [(0,i),(0,j)] <- bitLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i <  j))
  "Clash.Sized.Internal.BitVector.ge##" | [(0,i),(0,j)] <- bitLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i >= j))
  "Clash.Sized.Internal.BitVector.gt##" | [(0,i),(0,j)] <- bitLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i >  j))
  "Clash.Sized.Internal.BitVector.le##" | [(0,i),(0,j)] <- bitLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i <= j))

-- Bits
  "Clash.Sized.Internal.BitVector.and##"
    | [(0,i),(0,j)] <- bitLiterals args
    -> reduce (mkBitLit ty 0 (i .&. j))
  "Clash.Sized.Internal.BitVector.or##"
    | [(0,i),(0,j)] <- bitLiterals args
    -> reduce (mkBitLit ty 0 (i .|. j))
  "Clash.Sized.Internal.BitVector.xor##"
    | [(0,i),(0,j)] <- bitLiterals args
    -> reduce (mkBitLit ty 0 (i `xor` j))

  "Clash.Sized.Internal.BitVector.complement##"
    | [(0,i)] <- bitLiterals args
    -> reduce (mkBitLit ty 0 (complement i))

-- Pack
  "Clash.Sized.Internal.BitVector.pack#"
    | [(msk,i)] <- bitLiterals args
    -> let resTyInfo = extractTySizeInfo tcm ty tys
       in  reduce (mkBitVectorLit' resTyInfo msk i)

  "Clash.Sized.Internal.BitVector.unpack#"
    | [(msk,i)] <- bitVectorLiterals' args
    -> reduce (mkBitLit ty msk i)

-- Concatenation
  "Clash.Sized.Internal.BitVector.++#" -- :: KnownNat m => BitVector n -> BitVector m -> BitVector (n + m)
    | Just (_,m) <- extractKnownNat tcm tys
    , [(mski,i),(mskj,j)] <- bitVectorLiterals' args
    -> let val = i `shiftL` fromInteger m .|. j
           msk = mski `shiftL` fromInteger m .|. mskj
           resTyInfo = extractTySizeInfo tcm ty tys
       in reduce (mkBitVectorLit' resTyInfo msk val)

-- Reduction
  "Clash.Sized.Internal.BitVector.reduceAnd#" -- :: KnownNat n => BitVector n -> Bit
    | [i] <- bitVectorLiterals' args
    , Just (_, kn) <- extractKnownNat tcm tys
    -> let resTy = getResultTy tcm ty tys
           val = reifyNat kn (op (toBV i))
       in reduce (mkBitLit resTy 0 val)
    where
      op :: KnownNat n => BitVector n -> Proxy n -> Integer
      op u _ = toInteger (BitVector.reduceAnd# u)
  "Clash.Sized.Internal.BitVector.reduceOr#" -- :: KnownNat n => BitVector n -> Bit
    | [i] <- bitVectorLiterals' args
    , Just (_, kn) <- extractKnownNat tcm tys
    -> let resTy = getResultTy tcm ty tys
           val = reifyNat kn (op (toBV i))
       in reduce (mkBitLit resTy 0 val)
    where
      op :: KnownNat n => BitVector n -> Proxy n -> Integer
      op u _ = toInteger (BitVector.reduceOr# u)
  "Clash.Sized.Internal.BitVector.reduceXor#" -- :: KnownNat n => BitVector n -> Bit
    | [i] <- bitVectorLiterals' args
    , Just (_, kn) <- extractKnownNat tcm tys
    -> let resTy = getResultTy tcm ty tys
           val = reifyNat kn (op (toBV i))
       in reduce (mkBitLit resTy 0 val)
    where
      op :: KnownNat n => BitVector n -> Proxy n -> Integer
      op u _ = toInteger (BitVector.reduceXor# u)


-- Indexing
  "Clash.Sized.Internal.BitVector.index#" -- :: KnownNat n => BitVector n -> Int -> Bit
    | Just (_,kn,i,j) <- bitVectorLitIntLit tcm tys args
      -> let resTy = getResultTy tcm ty tys
             (msk,val) = reifyNat kn (op (toBV i) (fromInteger j))
         in reduce (mkBitLit resTy msk val)
      where
        op :: KnownNat n => BitVector n -> Int -> Proxy n -> (Integer,Integer)
        op u i _ = (m, v)
          where Bit m v = (BitVector.index# u i)
  "Clash.Sized.Internal.BitVector.replaceBit#" -- :: :: KnownNat n => BitVector n -> Int -> Bit -> BitVector n
    | Just (_, n) <- extractKnownNat tcm tys
    , [ _
      , PrimVal bvNm _ _ [_, Lit (IntegerLiteral mskBv), Lit (IntegerLiteral bv)]
      , valArgs -> Just [Literal (IntLiteral i)]
      , PrimVal bNm  _ _ [_, Lit (IntegerLiteral mskB), Lit (IntegerLiteral b)]
      ] <- args
    , bvNm == "Clash.Sized.Internal.BitVector.fromInteger#"
    , bNm  == "Clash.Sized.Internal.BitVector.fromInteger##"
      -> let resTyInfo = extractTySizeInfo tcm ty tys
             (mskVal,val) = reifyNat n (op (BV mskBv bv) (fromInteger i) (Bit mskB b))
      in reduce (mkBitVectorLit' resTyInfo mskVal val)
      where
        op :: KnownNat n => BitVector n -> Int -> Bit -> Proxy n -> (Integer,Integer)
        -- op bv i b _ = (BitVector.unsafeMask res, BitVector.unsafeToInteger res)
        op bv i b _ = splitBV (BitVector.replaceBit# bv i b)
  "Clash.Sized.Internal.BitVector.setSlice#"
  -- :: BitVector (m + 1 + i) -> SNat m -> SNat n -> BitVector (m + 1 - n) -> BitVector (m + 1 + i)
    | mTy : _ : nTy : _ <- tys
    , Right m <- runExcept (tyNatSize tcm mTy)
    , Right n <- runExcept (tyNatSize tcm nTy)
    , [i,j] <- bitVectorLiterals' args
    -> let BV msk val = BitVector.setSlice# (toBV i) (unsafeSNat m) (unsafeSNat n) (toBV j)
           resTyInfo = extractTySizeInfo tcm ty tys
       in  reduce (mkBitVectorLit' resTyInfo msk val)
  "Clash.Sized.Internal.BitVector.slice#"
  -- :: BitVector (m + 1 + i) -> SNat m -> SNat n -> BitVector (m + 1 - n)
    | mTy : _ : nTy : _ <- tys
    , Right m <- runExcept (tyNatSize tcm mTy)
    , Right n <- runExcept (tyNatSize tcm nTy)
    , [i] <- bitVectorLiterals' args
    -> let BV msk val = BitVector.slice# (toBV i) (unsafeSNat m) (unsafeSNat n)
           resTyInfo = extractTySizeInfo tcm ty tys
       in  reduce (mkBitVectorLit' resTyInfo msk val)
  "Clash.Sized.Internal.BitVector.split#" -- :: forall n m. KnownNat n => BitVector (m + n) -> (BitVector m, BitVector n)
    | nTy : mTy : _ <- tys
    , Right n <-  runExcept (tyNatSize tcm nTy)
    , Right m <-  runExcept (tyNatSize tcm mTy)
    , [(mski,i)] <- bitVectorLiterals' args
    -> let ty' = List.foldl' ((runFreshM .) . applyTy tcm) ty tys
           (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty'
           (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc] = tyConDataCons tupTc
           bvTy : _ = tyArgs
           valM = i `shiftR` fromInteger n
           mskM = mski `shiftR` fromInteger n
           valN = i .&. mask
           mskN = mski .&. mask
           mask = bit (fromInteger n) - 1
    in reduce $
       mkApps (Data tupDc) (map Right tyArgs ++
                [ Left (mkBitVectorLit bvTy mTy m mskM valM)
                , Left (mkBitVectorLit bvTy nTy n mskN valN)])

  "Clash.Sized.Internal.BitVector.msb#" -- :: forall n. KnownNat n => BitVector n -> Bit
    | [i] <- bitVectorLiterals' args
    , Just (_, kn) <- extractKnownNat tcm tys
    -> let resTy = getResultTy tcm ty tys
           (msk,val) = reifyNat kn (op (toBV i))
       in reduce (mkBitLit resTy msk val)
    where
      op :: KnownNat n => BitVector n -> Proxy n -> (Integer,Integer)
      op u _ = (unsafeMask# res, BitVector.unsafeToInteger# res)
        where
          res = BitVector.msb# u
  "Clash.Sized.Internal.BitVector.lsb#" -- :: BitVector n -> Bit
    | [i] <- bitVectorLiterals' args
    -> let resTy = getResultTy tcm ty tys
           Bit msk val = BitVector.lsb# (toBV i)
    in reduce (mkBitLit resTy msk val)


-- Eq
  -- eq#, neq# :: KnownNat n => BitVector n -> BitVector n -> Bool
  "Clash.Sized.Internal.BitVector.eq#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2Bool BitVector.eq# ty tcm args)
    -> reduce val

  "Clash.Sized.Internal.BitVector.neq#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2Bool BitVector.neq# ty tcm args)
    -> reduce val

-- Ord
  -- lt#,ge#,gt#,le# :: KnownNat n => BitVector n -> BitVector n -> Bool
  "Clash.Sized.Internal.BitVector.lt#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2Bool BitVector.lt# ty tcm args)
    -> reduce val
  "Clash.Sized.Internal.BitVector.ge#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2Bool BitVector.ge# ty tcm args)
    -> reduce val
  "Clash.Sized.Internal.BitVector.gt#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2Bool BitVector.gt# ty tcm args)
    -> reduce val
  "Clash.Sized.Internal.BitVector.le#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2Bool BitVector.le# ty tcm args)
    -> reduce val

-- Bounded
  "Clash.Sized.Internal.BitVector.minBound#"
    | Just (nTy,len) <- extractKnownNat tcm tys
    -> reduce (mkBitVectorLit ty nTy len 0 0)
  "Clash.Sized.Internal.BitVector.maxBound#"
    | Just (litTy,mb) <- extractKnownNat tcm tys
    -> let maxB = (2 ^ mb) - 1
       in  reduce (mkBitVectorLit ty litTy mb 0 maxB)

-- Num
  "Clash.Sized.Internal.BitVector.+#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2 (BitVector.+#) ty tcm tys args)
    -> reduce val
  "Clash.Sized.Internal.BitVector.-#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2 (BitVector.-#) ty tcm tys args)
    -> reduce val
  "Clash.Sized.Internal.BitVector.*#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2 (BitVector.*#) ty tcm tys args)
    -> reduce val
  "Clash.Sized.Internal.BitVector.negate#"
    | Just (nTy, kn) <- extractKnownNat tcm tys
    , [i] <- bitVectorLiterals' args
    -> let (msk,val) = reifyNat kn (op (toBV i))
    in reduce (mkBitVectorLit ty nTy kn msk val)
    where
      op :: KnownNat n => BitVector n -> Proxy n -> (Integer,Integer)
      op u _ = splitBV (BitVector.negate# u)

-- ExtendingNum
  "Clash.Sized.Internal.BitVector.plus#" -- :: (KnownNat n, KnownNat m) => BitVector m -> BitVector n -> BitVector (Max m n + 1)
    | [(0,i),(0,j)] <- bitVectorLiterals' args
    -> let ty' = List.foldl' ((runFreshM .) . applyTy tcm) ty tys
           (_,resTy) = splitFunForallTy ty'
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
       in  reduce (mkBitVectorLit resTy resSizeTy resSizeInt 0 (i+j))

  "Clash.Sized.Internal.BitVector.minus#"
    | [(0,i),(0,j)] <- bitVectorLiterals' args
    -> let ty' = List.foldl' ((runFreshM .) . applyTy tcm) ty tys
           (_,resTy) = splitFunForallTy ty'
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
           val = reifyNat resSizeInt (runSizedF (BitVector.-#) i j)
      in  reduce (mkBitVectorLit resTy resSizeTy resSizeInt 0 val)

  "Clash.Sized.Internal.BitVector.times#"
    | [(0,i),(0,j)] <- bitVectorLiterals' args
    -> let ty' = List.foldl' ((runFreshM .) . applyTy tcm) ty tys
           (_,resTy) = splitFunForallTy ty'
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
       in  reduce (mkBitVectorLit resTy resSizeTy resSizeInt 0 (i*j))

-- Integral
  "Clash.Sized.Internal.BitVector.quot#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2 (BitVector.quot#) ty tcm tys args)
    -> reduce val
  "Clash.Sized.Internal.BitVector.rem#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2 (BitVector.rem#) ty tcm tys args)
    -> reduce val
  "Clash.Sized.Internal.BitVector.toInteger#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , [i] <- bitVectorLiterals' args
    -> let val = reifyNat kn (op (toBV i))
    in reduce (integerToIntegerLiteral val)
    where
      op :: KnownNat n => BitVector n -> Proxy n -> Integer
      op u _ = BitVector.toInteger# u

-- Bits
  "Clash.Sized.Internal.BitVector.and#"
    | Just (i,j) <- bitVectorLiterals args
    , Just (nTy, kn) <- extractKnownNat tcm tys
    -> let BV msk val = BitVector.and# (toBV i) (toBV j)
    in reduce (mkBitVectorLit ty nTy kn msk val)
  "Clash.Sized.Internal.BitVector.or#"
    | Just (i,j) <- bitVectorLiterals args
    , Just (nTy, kn) <- extractKnownNat tcm tys
    -> let BV msk val = BitVector.or# (toBV i) (toBV j)
    in reduce (mkBitVectorLit ty nTy kn msk val)
  "Clash.Sized.Internal.BitVector.xor#"
    | Just (i,j) <- bitVectorLiterals args
    , Just (nTy, kn) <- extractKnownNat tcm tys
    -> let BV msk val = BitVector.xor# (toBV i) (toBV j)
    in reduce (mkBitVectorLit ty nTy kn msk val)

  "Clash.Sized.Internal.BitVector.complement#"
    | [i] <- bitVectorLiterals' args
    , Just (nTy, kn) <- extractKnownNat tcm tys
    -> let (msk,val) = reifyNat kn (op (toBV i))
    in reduce (mkBitVectorLit ty nTy kn msk val)
    where
      op :: KnownNat n => BitVector n -> Proxy n -> (Integer,Integer)
      op u _ = splitBV $ BitVector.complement# u

  "Clash.Sized.Internal.BitVector.shiftL#"
    | Just (nTy,kn,i,j) <- bitVectorLitIntLit tcm tys args
      -> let (msk,val) = reifyNat kn (op (toBV i) (fromInteger j))
      in reduce (mkBitVectorLit ty nTy kn msk val)
      where
        op :: KnownNat n => BitVector n -> Int -> Proxy n -> (Integer,Integer)
        op u i _ = splitBV (BitVector.shiftL# u i)
  "Clash.Sized.Internal.BitVector.shiftR#"
    | Just (nTy,kn,i,j) <- bitVectorLitIntLit tcm tys args
      -> let (msk,val) = reifyNat kn (op (toBV i) (fromInteger j))
      in reduce (mkBitVectorLit ty nTy kn msk val)
      where
        op :: KnownNat n => BitVector n -> Int -> Proxy n -> (Integer,Integer)
        op u i _ = splitBV (BitVector.shiftR# u i)
  "Clash.Sized.Internal.BitVector.rotateL#"
    | Just (nTy,kn,i,j) <- bitVectorLitIntLit tcm tys args
      -> let (msk,val) = reifyNat kn (op (toBV i) (fromInteger j))
      in reduce (mkBitVectorLit ty nTy kn msk val)
      where
        op :: KnownNat n => BitVector n -> Int -> Proxy n -> (Integer,Integer)
        op u i _ = splitBV (BitVector.rotateL# u i)
  "Clash.Sized.Internal.BitVector.rotateR#"
    | Just (nTy,kn,i,j) <- bitVectorLitIntLit tcm tys args
      -> let (msk,val) = reifyNat kn (op (toBV i) (fromInteger j))
      in reduce (mkBitVectorLit ty nTy kn msk val)
      where
        op :: KnownNat n => BitVector n -> Int -> Proxy n -> (Integer,Integer)
        op u i _ = splitBV (BitVector.rotateR# u i)

-- Resize
  "Clash.Sized.Internal.BitVector.resize#" -- forall n m . KnownNat m => BitVector n -> BitVector m
    | _ : mTy : _ <- tys
    , Right km <- runExcept (tyNatSize tcm mTy)
    , [(mski,i)] <- bitVectorLiterals' args
    -> let bitsKeep = (bit (fromInteger km)) - 1
           val = i .&. bitsKeep
           msk = mski .&. bitsKeep
    in reduce (mkBitVectorLit ty mTy km msk val)

--------
-- Index
--------
-- BitPack
  "Clash.Sized.Internal.Index.pack#"
    | nTy : _ <- tys
    , Right _ <- runExcept (tyNatSize tcm nTy)
    , [i] <- indexLiterals' args
    -> let resTyInfo = extractTySizeInfo tcm ty tys
       in  reduce (mkBitVectorLit' resTyInfo 0 i)
  "Clash.Sized.Internal.Index.unpack#"
    | Just (nTy,kn) <- extractKnownNat tcm tys
    , [(0,i)] <- bitVectorLiterals' args
    -> (h,k,) <$> mkIndexLit ty nTy kn i

-- Eq
  "Clash.Sized.Internal.Index.eq#" | Just (i,j) <- indexLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i == j))
  "Clash.Sized.Internal.Index.neq#" | Just (i,j) <- indexLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i /= j))

-- Ord
  "Clash.Sized.Internal.Index.lt#"
    | Just (i,j) <- indexLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i < j))
  "Clash.Sized.Internal.Index.ge#"
    | Just (i,j) <- indexLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i >= j))
  "Clash.Sized.Internal.Index.gt#"
    | Just (i,j) <- indexLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i > j))
  "Clash.Sized.Internal.Index.le#"
    | Just (i,j) <- indexLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i <= j))

-- Bounded
  "Clash.Sized.Internal.Index.maxBound#"
    | Just (nTy,mb) <- extractKnownNat tcm tys
    -> (h,k,) <$> mkIndexLit ty nTy mb (mb - 1)

-- Num
  "Clash.Sized.Internal.Index.+#"
    | Just (nTy,kn) <- extractKnownNat tcm tys
    , [i,j] <- indexLiterals' args
    -> (h,k,) <$> mkIndexLit ty nTy kn (i + j)
  "Clash.Sized.Internal.Index.-#"
    | Just (nTy,kn) <- extractKnownNat tcm tys
    , [i,j] <- indexLiterals' args
    -> (h,k,) <$> mkIndexLit ty nTy kn (i - j)
  "Clash.Sized.Internal.Index.*#"
    | Just (nTy,kn) <- extractKnownNat tcm tys
    , [i,j] <- indexLiterals' args
    -> (h,k,) <$> mkIndexLit ty nTy kn (i * j)

-- ExtendingNum
  "Clash.Sized.Internal.Index.plus#"
    | mTy : nTy : _ <- tys
    , Right _ <- runExcept (tyNatSize tcm mTy)
    , Right _ <- runExcept (tyNatSize tcm nTy)
    , Just (i,j) <- indexLiterals args
    -> let resTyInfo = extractTySizeInfo tcm ty tys
       in  (h,k,) <$> mkIndexLit' resTyInfo (i + j)
  "Clash.Sized.Internal.Index.minus#"
    | mTy : nTy : _ <- tys
    , Right _ <- runExcept (tyNatSize tcm mTy)
    , Right _ <- runExcept (tyNatSize tcm nTy)
    , Just (i,j) <- indexLiterals args
    -> let resTyInfo = extractTySizeInfo tcm ty tys
       in  (h,k,) <$> mkIndexLit' resTyInfo (i - j)
  "Clash.Sized.Internal.Index.times#"
    | mTy : nTy : _ <- tys
    , Right _ <- runExcept (tyNatSize tcm mTy)
    , Right _ <- runExcept (tyNatSize tcm nTy)
    , Just (i,j) <- indexLiterals args
    -> let resTyInfo = extractTySizeInfo tcm ty tys
       in  (h,k,) <$> mkIndexLit' resTyInfo (i * j)

-- Integral
  "Clash.Sized.Internal.Index.quot#"
    | Just (nTy,kn) <- extractKnownNat tcm tys
    , Just (i,j) <- indexLiterals args
    -> (h,k,) <$> mkIndexLit ty nTy kn (i `quot` j)
  "Clash.Sized.Internal.Index.rem#"
    | Just (nTy,kn) <- extractKnownNat tcm tys
    , Just (i,j) <- indexLiterals args
    -> (h,k,) <$> mkIndexLit ty nTy kn (i `rem` j)
  "Clash.Sized.Internal.Index.toInteger#"
    | [PrimVal nm' _ _ [_, Lit (IntegerLiteral i)]] <- args
    , nm' == "Clash.Sized.Internal.Index.fromInteger#"
    -> reduce (integerToIntegerLiteral i)

-- Resize
  "Clash.Sized.Internal.Index.resize#"
    | Just (mTy,m) <- extractKnownNat tcm tys
    , [i] <- indexLiterals' args
    -> (h,k,) <$> mkIndexLit ty mTy m i

---------
-- Signed
---------
  "Clash.Sized.Internal.Signed.size#"
    | Just (_, kn) <- extractKnownNat tcm tys
    -> let (_,tyView -> TyConApp intTcNm _) = splitFunForallTy ty
           (Just intTc) = HashMap.lookup (nameOcc intTcNm) tcm
           [intCon] = tyConDataCons intTc
       in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral kn))])

-- BitPack
  "Clash.Sized.Internal.Signed.pack#"
    | Just (nTy, kn) <- extractKnownNat tcm tys
    , [i] <- signedLiterals' args
    -> reduce (mkBitVectorLit ty nTy kn 0 i)
  "Clash.Sized.Internal.Signed.unpack#"
    | Just (nTy, kn) <- extractKnownNat tcm tys
    , [(0,i)] <- bitVectorLiterals' args
    -> reduce (mkSignedLit ty nTy kn i)

-- Eq
  "Clash.Sized.Internal.Signed.eq#" | Just (i,j) <- signedLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i == j))
  "Clash.Sized.Internal.Signed.neq#" | Just (i,j) <- signedLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i /= j))

-- Ord
  "Clash.Sized.Internal.Signed.lt#" | Just (i,j) <- signedLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i <  j))
  "Clash.Sized.Internal.Signed.ge#" | Just (i,j) <- signedLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i >= j))
  "Clash.Sized.Internal.Signed.gt#" | Just (i,j) <- signedLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i >  j))
  "Clash.Sized.Internal.Signed.le#" | Just (i,j) <- signedLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i <= j))

-- Bounded
  "Clash.Sized.Internal.Signed.minBound#"
    | Just (litTy,mb) <- extractKnownNat tcm tys
    -> let minB = negate (2 ^ (mb - 1))
       in  reduce (mkSignedLit ty litTy mb minB)
  "Clash.Sized.Internal.Signed.maxBound#"
    | Just (litTy,mb) <- extractKnownNat tcm tys
    -> let maxB = (2 ^ (mb - 1)) - 1
       in reduce (mkSignedLit ty litTy mb maxB)

-- Num
  "Clash.Sized.Internal.Signed.+#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftSigned2 (Signed.+#) ty tcm tys args)
    -> reduce (val)
  "Clash.Sized.Internal.Signed.-#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftSigned2 (Signed.-#) ty tcm tys args)
    -> reduce (val)
  "Clash.Sized.Internal.Signed.*#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftSigned2 (Signed.*#) ty tcm tys args)
    -> reduce (val)
  "Clash.Sized.Internal.Signed.negate#"
    | Just (nTy, kn) <- extractKnownNat tcm tys
    , [i] <- signedLiterals' args
    -> let val = reifyNat kn (op (fromInteger i))
    in reduce (mkSignedLit ty nTy kn val)
    where
      op :: KnownNat n => Signed n -> Proxy n -> Integer
      op s _ = toInteger (Signed.negate# s)
  "Clash.Sized.Internal.Signed.abs#"
    | Just (nTy, kn) <- extractKnownNat tcm tys
    , [i] <- signedLiterals' args
    -> let val = reifyNat kn (op (fromInteger i))
    in reduce (mkSignedLit ty nTy kn val)
    where
      op :: KnownNat n => Signed n -> Proxy n -> Integer
      op s _ = toInteger (Signed.abs# s)

-- ExtendingNum
  "Clash.Sized.Internal.Signed.plus#"
    | Just (i,j) <- signedLiterals args
    -> let ty' = List.foldl' ((runFreshM .) . applyTy tcm) ty tys
           (_,resTy) = splitFunForallTy ty'
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
       in  reduce (mkSignedLit resTy resSizeTy resSizeInt (i+j))

  "Clash.Sized.Internal.Signed.minus#"
    | Just (i,j) <- signedLiterals args
    -> let ty' = List.foldl' ((runFreshM .) . applyTy tcm) ty tys
           (_,resTy) = splitFunForallTy ty'
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
       in  reduce (mkSignedLit resTy resSizeTy resSizeInt (i-j))

  "Clash.Sized.Internal.Signed.times#"
    | Just (i,j) <- signedLiterals args
    -> let ty' = List.foldl' ((runFreshM .) . applyTy tcm) ty tys
           (_,resTy) = splitFunForallTy ty'
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
       in  reduce (mkSignedLit resTy resSizeTy resSizeInt (i*j))

-- Integral
  "Clash.Sized.Internal.Signed.quot#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftSigned2 (Signed.quot#) ty tcm tys args)
    -> reduce val
  "Clash.Sized.Internal.Signed.rem#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftSigned2 (Signed.rem#) ty tcm tys args)
    -> reduce val
  "Clash.Sized.Internal.Signed.div#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftSigned2 (Signed.div#) ty tcm tys args)
    -> reduce val
  "Clash.Sized.Internal.Signed.mod#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftSigned2 (Signed.mod#) ty tcm tys args)
    -> reduce val
  "Clash.Sized.Internal.Signed.toInteger#"
    | [PrimVal nm' _ _ [_, Lit (IntegerLiteral i)]] <- args
    , nm' == "Clash.Sized.Internal.Signed.fromInteger#"
    -> reduce (integerToIntegerLiteral i)

-- Bits
  "Clash.Sized.Internal.Signed.and#"
    | [i,j] <- signedLiterals' args
    , Just (nTy, kn) <- extractKnownNat tcm tys
    -> reduce (mkSignedLit ty nTy kn (i .&. j))
  "Clash.Sized.Internal.Signed.or#"
    | [i,j] <- signedLiterals' args
    , Just (nTy, kn) <- extractKnownNat tcm tys
    -> reduce (mkSignedLit ty nTy kn (i .|. j))
  "Clash.Sized.Internal.Signed.xor#"
    | [i,j] <- signedLiterals' args
    , Just (nTy, kn) <- extractKnownNat tcm tys
    -> reduce (mkSignedLit ty nTy kn (i `xor` j))

  "Clash.Sized.Internal.Signed.complement#"
    | [i] <- signedLiterals' args
    , Just (nTy, kn) <- extractKnownNat tcm tys
    -> let val = reifyNat kn (op (fromInteger i))
    in reduce (mkSignedLit ty nTy kn val)
    where
      op :: KnownNat n => Signed n -> Proxy n -> Integer
      op u _ = toInteger (Signed.complement# u)

  "Clash.Sized.Internal.Signed.shiftL#"
    | Just (nTy,kn,i,j) <- signedLitIntLit tcm tys args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in reduce (mkSignedLit ty nTy kn val)
      where
        op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (Signed.shiftL# u i)
  "Clash.Sized.Internal.Signed.shiftR#"
    | Just (nTy,kn,i,j) <- signedLitIntLit tcm tys args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in reduce (mkSignedLit ty nTy kn val)
      where
        op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (Signed.shiftR# u i)
  "Clash.Sized.Internal.Signed.rotateL#"
    | Just (nTy,kn,i,j) <- signedLitIntLit tcm tys args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in reduce (mkSignedLit ty nTy kn val)
      where
        op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (Signed.rotateL# u i)
  "Clash.Sized.Internal.Signed.rotateR#"
    | Just (nTy,kn,i,j) <- signedLitIntLit tcm tys args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in reduce (mkSignedLit ty nTy kn val)
      where
        op :: KnownNat n => Signed n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (Signed.rotateR# u i)

-- Resize
  "Clash.Sized.Internal.Signed.resize#" -- forall m n. (KnownNat n, KnownNat m) => Signed n -> Signed m
    | mTy : nTy : _ <- tys
    , Right mInt <- runExcept (tyNatSize tcm mTy)
    , Right nInt <- runExcept (tyNatSize tcm nTy)
    , [i] <- signedLiterals' args
    -> let val | nInt <= mInt = extended
               | otherwise    = truncated
           extended  = i
           mask      = 1 `shiftL` fromInteger (mInt - 1)
           i'        = i `mod` mask
           truncated = if testBit i (fromInteger nInt - 1)
                          then (i' - mask)
                          else i'
       in reduce (mkSignedLit ty mTy mInt val)
  "Clash.Sized.Internal.Signed.truncateB#" -- KnownNat m => Signed (m + n) -> Signed m
    | Just (mTy, km) <- extractKnownNat tcm tys
    , [i] <- signedLiterals' args
    -> let bitsKeep = (bit (fromInteger km)) - 1
           val = i .&. bitsKeep
    in reduce (mkSignedLit ty mTy km val)

-- SaturatingNum
-- No need to manually evaluate Clash.Sized.Internal.Signed.minBoundSym#
-- It is just implemented in terms of other primitives.


-----------
-- Unsigned
-----------
  "Clash.Sized.Internal.Unsigned.size#"
    | Just (_, kn) <- extractKnownNat tcm tys
    -> let (_,ty') = splitFunForallTy ty
           (TyConApp intTcNm _) = tyView ty'
           (Just intTc) = HashMap.lookup (nameOcc intTcNm) tcm
           [intCon] = tyConDataCons intTc
       in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral kn))])

-- BitPack
  "Clash.Sized.Internal.Unsigned.pack#"
    | Just (nTy, kn) <- extractKnownNat tcm tys
    , [i] <- unsignedLiterals' args
    -> reduce (mkBitVectorLit ty nTy kn 0 i)
  "Clash.Sized.Internal.Unsigned.unpack#"
    | Just (nTy, kn) <- extractKnownNat tcm tys
    , [i] <- bitVectorLiterals' args
    -> let val = reifyNat kn (op (toBV i))
    in reduce (mkUnsignedLit ty nTy kn val)
    where
      op :: KnownNat n => BitVector n -> Proxy n -> Integer
      op u _ = toInteger (Unsigned.unpack# u)

-- Eq
  "Clash.Sized.Internal.Unsigned.eq#" | Just (i,j) <- unsignedLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i == j))
  "Clash.Sized.Internal.Unsigned.neq#" | Just (i,j) <- unsignedLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i /= j))

-- Ord
  "Clash.Sized.Internal.Unsigned.lt#" | Just (i,j) <- unsignedLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i <  j))
  "Clash.Sized.Internal.Unsigned.ge#" | Just (i,j) <- unsignedLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i >= j))
  "Clash.Sized.Internal.Unsigned.gt#" | Just (i,j) <- unsignedLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i >  j))
  "Clash.Sized.Internal.Unsigned.le#" | Just (i,j) <- unsignedLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i <= j))

-- Bounded
  "Clash.Sized.Internal.Unsigned.minBound#"
    | Just (nTy,len) <- extractKnownNat tcm tys
    -> reduce (mkUnsignedLit ty nTy len 0)
  "Clash.Sized.Internal.Unsigned.maxBound#"
    | Just (litTy,mb) <- extractKnownNat tcm tys
    -> let maxB = (2 ^ mb) - 1
       in  reduce (mkUnsignedLit ty litTy mb maxB)

-- Num
  "Clash.Sized.Internal.Unsigned.+#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.+#) ty tcm tys args)
    -> reduce val
  "Clash.Sized.Internal.Unsigned.-#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.-#) ty tcm tys args)
    -> reduce val
  "Clash.Sized.Internal.Unsigned.*#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.*#) ty tcm tys args)
    -> reduce val
  "Clash.Sized.Internal.Unsigned.negate#"
    | Just (nTy, kn) <- extractKnownNat tcm tys
    , [i] <- unsignedLiterals' args
    -> let val = reifyNat kn (op (fromInteger i))
    in reduce (mkUnsignedLit ty nTy kn val)
    where
      op :: KnownNat n => Unsigned n -> Proxy n -> Integer
      op u _ = toInteger (Unsigned.negate# u)

-- ExtendingNum
  "Clash.Sized.Internal.Unsigned.plus#" -- :: Unsigned m -> Unsigned n -> Unsigned (Max m n + 1)
    | Just (i,j) <- unsignedLiterals args
    -> let ty' = List.foldl' ((runFreshM .) . applyTy tcm) ty tys
           (_,resTy) = splitFunForallTy ty'
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
       in  reduce (mkUnsignedLit resTy resSizeTy resSizeInt (i+j))

  "Clash.Sized.Internal.Unsigned.minus#"
    | [i,j] <- unsignedLiterals' args
    -> let ty' = List.foldl' ((runFreshM .) . applyTy tcm) ty tys
           (_,resTy) = splitFunForallTy ty'
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
           val = reifyNat resSizeInt (runSizedF (Unsigned.-#) i j)
      in   reduce (mkUnsignedLit resTy resSizeTy resSizeInt val)

  "Clash.Sized.Internal.Unsigned.times#"
    | Just (i,j) <- unsignedLiterals args
    -> let ty' = List.foldl' ((runFreshM .) . applyTy tcm) ty tys
           (_,resTy) = splitFunForallTy ty'
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
       in  reduce (mkUnsignedLit resTy resSizeTy resSizeInt (i*j))

-- Integral
  "Clash.Sized.Internal.Unsigned.quot#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.quot#) ty tcm tys args)
    -> reduce val
  "Clash.Sized.Internal.Unsigned.rem#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.rem#) ty tcm tys args)
    -> reduce val
  "Clash.Sized.Internal.Unsigned.toInteger#"
    | [PrimVal nm' _ _ [_, Lit (IntegerLiteral i)]] <- args
    , nm' == "Clash.Sized.Internal.Unsigned.fromInteger#"
    -> reduce (integerToIntegerLiteral i)

-- Bits
  "Clash.Sized.Internal.Unsigned.and#"
    | Just (i,j) <- unsignedLiterals args
    , Just (nTy, kn) <- extractKnownNat tcm tys
    -> reduce (mkUnsignedLit ty nTy kn (i .&. j))
  "Clash.Sized.Internal.Unsigned.or#"
    | Just (i,j) <- unsignedLiterals args
    , Just (nTy, kn) <- extractKnownNat tcm tys
    -> reduce (mkUnsignedLit ty nTy kn (i .|. j))
  "Clash.Sized.Internal.Unsigned.xor#"
    | Just (i,j) <- unsignedLiterals args
    , Just (nTy, kn) <- extractKnownNat tcm tys
    -> reduce (mkUnsignedLit ty nTy kn (i `xor` j))

  "Clash.Sized.Internal.Unsigned.complement#"
    | [i] <- unsignedLiterals' args
    , Just (nTy, kn) <- extractKnownNat tcm tys
    -> let val = reifyNat kn (op (fromInteger i))
    in reduce (mkUnsignedLit ty nTy kn val)
    where
      op :: KnownNat n => Unsigned n -> Proxy n -> Integer
      op u _ = toInteger (Unsigned.complement# u)

  "Clash.Sized.Internal.Unsigned.shiftL#" -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
    | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm tys args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in reduce (mkUnsignedLit ty nTy kn val)
      where
        op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (Unsigned.shiftL# u i)
  "Clash.Sized.Internal.Unsigned.shiftR#" -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
    | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm tys args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in reduce (mkUnsignedLit ty nTy kn val)
      where
        op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (Unsigned.shiftR# u i)
  "Clash.Sized.Internal.Unsigned.rotateL#" -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
    | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm tys args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in reduce (mkUnsignedLit ty nTy kn val)
      where
        op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (Unsigned.rotateL# u i)
  "Clash.Sized.Internal.Unsigned.rotateR#" -- :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n
    | Just (nTy,kn,i,j) <- unsignedLitIntLit tcm tys args
      -> let val = reifyNat kn (op (fromInteger i) (fromInteger j))
      in reduce (mkUnsignedLit ty nTy kn val)
      where
        op :: KnownNat n => Unsigned n -> Int -> Proxy n -> Integer
        op u i _ = toInteger (Unsigned.rotateR# u i)

-- Resize
  "Clash.Sized.Internal.Unsigned.resize#" -- forall n m . KnownNat m => Unsigned n -> Unsigned m
    | _ : mTy : _ <- tys
    , Right km <- runExcept (tyNatSize tcm mTy)
    , [i] <- unsignedLiterals' args
    -> let bitsKeep = (bit (fromInteger km)) - 1
           val = i .&. bitsKeep
    in reduce (mkUnsignedLit ty mTy km val)

--------
-- RTree
--------
  "Clash.Sized.RTree.textract"
    | isSubj
    , [DC _ tArgs] <- args
    -> reduceWHNF (Either.lefts tArgs !! 1)

  "Clash.Sized.RTree.tsplit"
    | isSubj
    , dTy : aTy : _ <- tys
    , [DC _ tArgs] <- args
    , (tyArgs,tyView -> TyConApp tupTcNm _) <- splitFunForallTy ty
    , TyConApp treeTcNm _ <- tyView (Either.rights tyArgs !! 0)
    -> let (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc]      = tyConDataCons tupTc
       in  reduce $
           mkApps (Data tupDc)
                  [Right (mkTyConApp treeTcNm [dTy,aTy])
                  ,Right (mkTyConApp treeTcNm [dTy,aTy])
                  ,Left (Either.lefts tArgs !! 1)
                  ,Left (Either.lefts tArgs !! 2)
                  ]

  "Clash.Sized.RTree.tdfold"
    | isSubj
    , pTy : kTy : aTy : _ <- tys
    , _ : p : f : g : ts : _ <- args
    , DC _ tArgs <- ts
    , Right k' <- runExcept (tyNatSize tcm kTy)
    -> case k' of
         0 -> reduceWHNF (mkApps (valToTerm f) [Left (Either.lefts tArgs !! 1)])
         _ -> let k'ty = LitTy (NumTy (k'-1))
                  (tyArgs,_)  = splitFunForallTy ty
                  (tyArgs',_) = splitFunForallTy (Either.rights tyArgs !! 3)
                  TyConApp snatTcNm _ = tyView (Either.rights tyArgs' !! 0)
                  Just snatTc = HashMap.lookup (nameOcc snatTcNm) tcm
                  [snatDc]    = tyConDataCons snatTc
              in  reduceWHNF $
                  mkApps (valToTerm g)
                         [Right k'ty
                         ,Left (mkApps (Data snatDc)
                                       [Right k'ty
                                       ,Left (Literal (NaturalLiteral (k'-1)))])
                         ,Left (mkApps (Prim nm ty)
                                       [Right pTy
                                       ,Right k'ty
                                       ,Right aTy
                                       ,Left (Literal (NaturalLiteral (k'-1)))
                                       ,Left (valToTerm p)
                                       ,Left (valToTerm f)
                                       ,Left (valToTerm g)
                                       ,Left (Either.lefts tArgs !! 1)
                                       ])
                         ,Left (mkApps (Prim nm ty)
                                       [Right pTy
                                       ,Right k'ty
                                       ,Right aTy
                                       ,Left (Literal (NaturalLiteral (k'-1)))
                                       ,Left (valToTerm p)
                                       ,Left (valToTerm f)
                                       ,Left (valToTerm g)
                                       ,Left (Either.lefts tArgs !! 2)
                                       ])
                         ]

  "Clash.Sized.RTree.treplicate"
    | isSubj
    , let ty' = List.foldl' ((runFreshM .) . applyTy tcm) ty tys
    , (_,tyView -> TyConApp treeTcNm [lenTy,argTy]) <- splitFunForallTy ty'
    , Right len <- runExcept (tyNatSize tcm lenTy)
    -> let (Just treeTc) = HashMap.lookup (nameOcc treeTcNm) tcm
           [lrCon,brCon] = tyConDataCons treeTc
       in  reduce (mkRTree lrCon brCon argTy len (replicate (2^len) (valToTerm (last args))))

---------
-- Vector
---------
  "Clash.Sized.Vector.length"
    | isSubj
    , [nTy, _] <- tys
    , Right n <-runExcept (tyNatSize tcm nTy)
    -> let (_, tyView -> TyConApp intTcNm _) = splitFunForallTy ty
           (Just intTc) = HashMap.lookup (nameOcc intTcNm) tcm
           [intCon] = tyConDataCons intTc
       in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral (toInteger n)))])

  "Clash.Sized.Vector.maxIndex"
    | isSubj
    , [nTy, _] <- tys
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> let (_, tyView -> TyConApp intTcNm _) = splitFunForallTy ty
           (Just intTc) = HashMap.lookup (nameOcc intTcNm) tcm
           [intCon] = tyConDataCons intTc
       in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral (toInteger (n - 1))))])

-- Indexing
  "Clash.Sized.Vector.index_int"
    | isSubj
    , nTy : aTy : _  <- tys
    , _ : xs : i : _ <- args
    , DC intDc [Left (Literal (IntLiteral i'))] <- i
    -> if i' < 0
          then Nothing
          else case xs of
                 DC _ vArgs  -> case runExcept (tyNatSize tcm nTy) of
                    Right 0  -> Nothing
                    Right n' ->
                      if i' == 0
                         then reduceWHNF (Either.lefts vArgs !! 1)
                         else reduceWHNF $
                              mkApps (Prim nm ty)
                                     [Right (LitTy (NumTy (n'-1)))
                                     ,Right aTy
                                     ,Left (Literal (NaturalLiteral (n'-1)))
                                     ,Left (Either.lefts vArgs !! 2)
                                     ,Left (mkApps (Data intDc)
                                                   [Left (Literal (IntLiteral (i'-1)))])
                                     ]
                    _ -> Nothing
                 _ -> Nothing
  "Clash.Sized.Vector.head"
    | isSubj
    , [DC _ vArgs] <- args
    -> reduceWHNF (Either.lefts vArgs !! 1)
  "Clash.Sized.Vector.last"
    | isSubj
    , [DC _ vArgs] <- args
    , (Right _ : Right aTy : Right nTy : _) <- vArgs
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> if n == 0
          then reduceWHNF (Either.lefts vArgs !! 1)
          else reduceWHNF
                (mkApps (Prim nm ty) [Right (LitTy (NumTy (n-1)))
                                     ,Right aTy
                                     ,Left (Either.lefts vArgs !! 2)
                                     ])
-- - Sub-vectors
  "Clash.Sized.Vector.tail"
    | isSubj
    , [DC _ vArgs] <- args
    -> reduceWHNF (Either.lefts vArgs !! 2)
  "Clash.Sized.Vector.init"
    | isSubj
    , [DC consCon vArgs] <- args
    , (Right _ : Right aTy : Right nTy : _) <- vArgs
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> if n == 0
          then reduceWHNF (Either.lefts vArgs !! 2)
          else reduce $
               mkVecCons consCon aTy n
                  (Either.lefts vArgs !! 1)
                  (mkApps (Prim nm ty) [Right (LitTy (NumTy (n-1)))
                                       ,Right aTy
                                       ,Left (Either.lefts vArgs !! 2)])
  "Clash.Sized.Vector.select"
    | isSubj
    , iTy : sTy : nTy : fTy : aTy : _ <- tys
    , eq : f : s : n : xs : _ <- args
    , Right n' <- runExcept (tyNatSize tcm nTy)
    , Right f' <- runExcept (tyNatSize tcm fTy)
    , Right i' <- runExcept (tyNatSize tcm iTy)
    , Right s' <- runExcept (tyNatSize tcm sTy)
    , DC _ vArgs <- xs
    -> case n' of
         0 -> reduce (mkVecNil nilCon aTy)
         _ -> case f' of
          0 -> let splitAtCall =
                    mkApps (Prim "Clash.Sized.Vector.splitAt" (splitAtTy snatTcNm vecTcNm))
                           [Right sTy
                           ,Right (LitTy (NumTy (i'-s')))
                           ,Right aTy
                           ,Left (valToTerm s)
                           ,Left (valToTerm xs)
                           ]
                   fVecTy = mkTyConApp vecTcNm [sTy,aTy]
                   iVecTy = mkTyConApp vecTcNm [LitTy (NumTy (i'-s')),aTy]
                   fNm    = string2SystemName "fxs"
                   iNm    = string2SystemName "ixs"
                   fId    = Id fNm (embed fVecTy)
                   iId    = Id iNm (embed iVecTy)
                   tupPat = (DataPat (embed tupDc) (rebind [] [fId,iId]))
                   iAlt   = bind tupPat (Var iVecTy iNm)
               in  reduce $
                   mkVecCons consCon aTy n' (Either.lefts vArgs !! 1) $
                   mkApps (Prim nm ty)
                          [Right (LitTy (NumTy (i'-s')))
                          ,Right sTy
                          ,Right (LitTy (NumTy (n'-1)))
                          ,Right (LitTy (NumTy 0))
                          ,Right aTy
                          ,Left (valToTerm eq)
                          ,Left (Literal (NaturalLiteral 0))
                          ,Left (valToTerm s)
                          ,Left (Literal (NaturalLiteral (n'-1)))
                          ,Left (Case splitAtCall iVecTy [iAlt])
                          ]
          _ -> let splitAtCall =
                    mkApps (Prim "Clash.Sized.Vector.splitAt" (splitAtTy snatTcNm vecTcNm))
                           [Right fTy
                           ,Right iTy
                           ,Right aTy
                           ,Left (valToTerm f)
                           ,Left (valToTerm xs)
                           ]
                   fVecTy = mkTyConApp vecTcNm [fTy,aTy]
                   iVecTy = mkTyConApp vecTcNm [iTy,aTy]
                   fNm    = string2SystemName "fxs"
                   iNm    = string2SystemName "ixs"
                   fId    = Id fNm (embed fVecTy)
                   iId    = Id iNm (embed iVecTy)
                   tupPat = (DataPat (embed tupDc) (rebind [] [fId,iId]))
                   iAlt   = bind tupPat (Var iVecTy iNm)
               in  reduceWHNF $
                   mkApps (Prim nm ty)
                     [Right iTy
                     ,Right sTy
                     ,Right nTy
                     ,Right (LitTy (NumTy 0))
                     ,Right aTy
                     ,Left (valToTerm eq)
                     ,Left (Literal (NaturalLiteral 0))
                     ,Left (valToTerm s)
                     ,Left (valToTerm n)
                     ,Left (Case splitAtCall iVecTy [iAlt])
                     ]
    where
      (tyArgs,tyView -> TyConApp vecTcNm _) = splitFunForallTy ty
      Just vecTc          = HashMap.lookup (nameOcc vecTcNm) tcm
      [nilCon,consCon]    = tyConDataCons vecTc
      TyConApp snatTcNm _ = tyView (Either.rights tyArgs !! 1)
      tupTcNm            = ghcTyconToTyConName (tupleTyCon Boxed 2)
      (Just tupTc)       = HashMap.lookup (nameOcc tupTcNm) tcm
      [tupDc]            = tyConDataCons tupTc
-- - Splitting
  "Clash.Sized.Vector.splitAt"
    | isSubj
    , DC snatDc (Right mTy:_) <- head args
    , Right m <- runExcept (tyNatSize tcm mTy)
    -> let _:nTy:aTy:_ = tys
           -- Get the tuple data-constructor
           (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc)       = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc]            = tyConDataCons tupTc
           -- Get the vector data-constructors
           TyConApp vecTcNm _ = tyView (head tyArgs)
           Just vecTc         = HashMap.lookup (nameOcc vecTcNm) tcm
           [nilCon,consCon]   = tyConDataCons vecTc
           -- Recursive call to @splitAt@
           splitAtRec v =
            mkApps (Prim nm ty)
                   [Right (LitTy (NumTy (m-1)))
                   ,Right nTy
                   ,Right aTy
                   ,Left (mkApps (Data snatDc)
                                 [ Right (LitTy (NumTy (m-1)))
                                 , Left  (Literal (NaturalLiteral (m-1)))])
                   ,Left v
                   ]
           -- Projection either the first or second field of the recursive
           -- call to @splitAt@
           splitAtSelR v = Case (splitAtRec v) (last tyArgs)
           m1VecTy = mkTyConApp vecTcNm [LitTy (NumTy (m-1)),aTy]
           nVecTy  = mkTyConApp vecTcNm [nTy,aTy]
           lNm     = string2SystemName "l"
           rNm     = string2SystemName "r"
           lId     = Id lNm (embed m1VecTy)
           rId     = Id rNm (embed nVecTy)
           tupPat  = (DataPat (embed tupDc) (rebind [] [lId,rId]))
           lAlt    = bind tupPat (Var m1VecTy lNm)
           rAlt    = bind tupPat (Var nVecTy rNm)

       in case m of
         -- (Nil,v)
         0 -> reduce $
              mkApps (Data tupDc) $ (map Right tyArgs) ++
                [ Left (mkVecNil nilCon aTy)
                , Left (valToTerm (last args))
                ]
         -- (x:xs) <- v
         m' | DC _ vArgs <- last args
            -- (x:fst (splitAt (m-1) xs),snd (splitAt (m-1) xs))
            -> reduce $
               mkApps (Data tupDc) $ (map Right tyArgs) ++
                 [ Left (mkVecCons consCon aTy m' (Either.lefts vArgs !! 1)
                           (splitAtSelR (Either.lefts vArgs !! 2) [lAlt]))
                 , Left (splitAtSelR (Either.lefts vArgs !! 2) [rAlt])
                 ]
         -- v doesn't reduce to a data-constructor
         _  -> Nothing

  "Clash.Sized.Vector.unconcat"
    | isSubj
    , kn : snat : v : _  <- args
    , nTy : mTy : aTy :_ <- tys
    , Lit (NaturalLiteral n) <- kn
    -> let ( Either.rights -> argTys, tyView -> TyConApp vecTcNm _) =
              splitFunForallTy ty
           Just vecTc = HashMap.lookup (nameOcc vecTcNm) tcm
           [nilCon,consCon]   = tyConDataCons vecTc
           tupTcNm            = ghcTyconToTyConName (tupleTyCon Boxed 2)
           (Just tupTc)       = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc]            = tyConDataCons tupTc
           TyConApp snatTcNm _ = tyView (argTys !! 1)
           n1mTy  = mkTyConApp typeNatMul
                        [mkTyConApp typeNatSub [nTy,LitTy (NumTy 1)]
                        ,mTy]
           splitAtCall =
            mkApps (Prim "Clash.Sized.Vector.splitAt" (splitAtTy snatTcNm vecTcNm))
                   [Right mTy
                   ,Right n1mTy
                   ,Right aTy
                   ,Left (valToTerm snat)
                   ,Left (valToTerm v)
                   ]
           mVecTy   = mkTyConApp vecTcNm [mTy,aTy]
           n1mVecTy = mkTyConApp vecTcNm [n1mTy,aTy]
           asNm     = string2SystemName "as"
           bsNm     = string2SystemName "bs"
           asId     = Id asNm (embed mVecTy)
           bsId     = Id bsNm (embed n1mVecTy)
           tupPat   = (DataPat (embed tupDc) (rebind [] [asId,bsId]))
           asAlt    = bind tupPat (Var mVecTy asNm)
           bsAlt    = bind tupPat (Var n1mVecTy bsNm)

       in  case n of
         0 -> reduce (mkVecNil nilCon mVecTy)
         _ -> reduce $
              mkVecCons consCon mVecTy n
                (Case splitAtCall mVecTy [asAlt])
                (mkApps (Prim nm ty)
                    [Right (LitTy (NumTy (n-1)))
                    ,Right mTy
                    ,Right aTy
                    ,Left (Literal (NaturalLiteral (n-1)))
                    ,Left (valToTerm snat)
                    ,Left (Case splitAtCall n1mVecTy [bsAlt])])
-- Construction
-- - initialisation
  "Clash.Sized.Vector.replicate"
    | isSubj
    , let ty' = List.foldl' ((runFreshM .) . applyTy tcm) ty tys
    , let (_,resTy) = splitFunForallTy ty'
    , (TyConApp vecTcNm [lenTy,argTy]) <- tyView resTy
    , Right len <- runExcept (tyNatSize tcm lenTy)
    -> let (Just vecTc) = HashMap.lookup (nameOcc vecTcNm) tcm
           [nilCon,consCon] = tyConDataCons vecTc
       in  reduce $
           mkVec nilCon consCon argTy len
                 (replicate (fromInteger len) (valToTerm (last args)))
-- - Concatenation
  "Clash.Sized.Vector.++"
    | isSubj
    , DC dc vArgs <- head args
    , Right nTy : Right aTy : _ <- vArgs
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> case n of
         0  -> reduce (valToTerm (last args))
         n' | (_ : _ : mTy : _) <- tys
            , Right m <- runExcept (tyNatSize tcm mTy)
            -> -- x : (xs ++ ys)
               reduce $
               mkVecCons dc aTy (n' + m) (Either.lefts vArgs !! 1)
                 (mkApps (Prim nm ty) [Right (LitTy (NumTy (n'-1)))
                                      ,Right aTy
                                      ,Right mTy
                                      ,Left (Either.lefts vArgs !! 2)
                                      ,Left (valToTerm (last args))
                                      ])
         _ -> Nothing
  "Clash.Sized.Vector.concat"
    | isSubj
    , (nTy : mTy : aTy : _)  <- tys
    , (xs : _)               <- args
    , DC dc vArgs <- xs
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> case n of
        0 -> reduce (mkVecNil dc aTy)
        _ | _ : h' : t : _ <- Either.lefts  vArgs
          , (_,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
          -> reduceWHNF $
             mkApps (Prim "Clash.Sized.Vector.++" (vecAppendTy vecTcNm))
                    [Right mTy
                    ,Right aTy
                    ,Right $ mkTyConApp typeNatMul
                      [mkTyConApp typeNatSub [nTy,LitTy (NumTy 1)], mTy]
                    ,Left h'
                    ,Left $ mkApps (Prim nm ty)
                      [ Right (LitTy (NumTy (n-1)))
                      , Right mTy
                      , Right aTy
                      , Left t
                      ]
                    ]
        _ -> Nothing

-- Modifying vectors
  "Clash.Sized.Vector.replace_int"
    | isSubj
    , nTy : aTy : _  <- tys
    , _ : xs : i : a : _ <- args
    , DC intDc [Left (Literal (IntLiteral i'))] <- i
    -> if i' < 0
          then Nothing
          else case xs of
                 DC vecTcNm vArgs -> case runExcept (tyNatSize tcm nTy) of
                    Right 0  -> Nothing
                    Right n' ->
                      if i' == 0
                         then reduce (mkVecCons vecTcNm aTy n' (valToTerm a) (Either.lefts vArgs !! 2))
                         else reduce $
                              mkVecCons vecTcNm aTy n' (Either.lefts vArgs !! 1)
                                (mkApps (Prim nm ty)
                                        [Right (LitTy (NumTy (n'-1)))
                                        ,Right aTy
                                        ,Left (Literal (NaturalLiteral (n'-1)))
                                        ,Left (Either.lefts vArgs !! 2)
                                        ,Left (mkApps (Data intDc)
                                                      [Left (Literal (IntLiteral (i'-1)))])
                                        ,Left (valToTerm a)
                                        ])
                    _ -> Nothing
                 _ -> Nothing

-- - specialised permutations
  "Clash.Sized.Vector.reverse"
    | isSubj
    , nTy : aTy : _  <- tys
    , [DC vecDc vArgs] <- args
    -> case runExcept (tyNatSize tcm nTy) of
         Right 0 -> reduce (mkVecNil vecDc aTy)
         Right n
           | (_,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
           , let (Just vecTc) = HashMap.lookup (nameOcc vecTcNm) tcm
           , let [nilCon,consCon] = tyConDataCons vecTc
           -> reduceWHNF $
              mkApps (Prim "Clash.Sized.Vector.++" (vecAppendTy vecTcNm))
                [Right (LitTy (NumTy (n-1)))
                ,Right aTy
                ,Right (LitTy (NumTy 1))
                ,Left (mkApps (Prim nm ty)
                              [Right (LitTy (NumTy (n-1)))
                              ,Right aTy
                              ,Left (Either.lefts vArgs !! 2)
                              ])
                ,Left (mkVec nilCon consCon aTy 1 [Either.lefts vArgs !! 1])
                ]
         _ -> Nothing
  "Clash.Sized.Vector.transpose" -- :: KnownNat n => Vec m (Vec n a) -> Vec n (Vec m a)
    | isSubj
    , nTy : mTy : aTy : _ <- tys
    , kn : xss : _ <- args
    , (_,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
    , DC _ vArgs <- xss
    , Right n <- runExcept (tyNatSize tcm nTy)
    , Right m <- runExcept (tyNatSize tcm mTy)
    -> case m of
      0 -> let (Just vecTc)     = HashMap.lookup (nameOcc vecTcNm) tcm
               [nilCon,consCon] = tyConDataCons vecTc
           in  reduce $
               mkVec nilCon consCon (mkTyConApp vecTcNm [mTy,aTy]) n
                (replicate (fromInteger n) (mkVec nilCon consCon aTy 0 []))
      m' -> let (Just vecTc)     = HashMap.lookup (nameOcc vecTcNm) tcm
                [_,consCon] = tyConDataCons vecTc
                Just (consCoTy : _) = dataConInstArgTys consCon
                                        [mTy,aTy,LitTy (NumTy (m'-1))]
            in  reduceWHNF $
                mkApps (Prim "Clash.Sized.Vector.zipWith" (vecZipWithTy vecTcNm))
                       [ Right aTy
                       , Right (mkTyConApp vecTcNm [LitTy (NumTy (m'-1)),aTy])
                       , Right (mkTyConApp vecTcNm [mTy,aTy])
                       , Right nTy
                       , Left  (mkApps (Data consCon)
                                       [Right mTy
                                       ,Right aTy
                                       ,Right (LitTy (NumTy (m'-1)))
                                       ,Left (Prim "_CO_" consCoTy)
                                       ])
                       , Left  (Either.lefts vArgs !! 1)
                       , Left  (mkApps (Prim nm ty)
                                       [ Right nTy
                                       , Right (LitTy (NumTy (m'-1)))
                                       , Right aTy
                                       , Left  (valToTerm kn)
                                       , Left  (Either.lefts vArgs !! 2)
                                       ])
                       ]
  "Clash.Sized.Vector.rotateLeftS"
    | isSubj
    , nTy : aTy : _ : _ <- tys
    , kn : xs : d : _ <- args
    , DC dc vArgs <- xs
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> case n of
         0  -> reduce (mkVecNil dc aTy)
         n' | DC snatDc [_,Left d'] <- d
            , (h2,[],Literal (NaturalLiteral d2)) <- whnf reduceConstant gbl tcm isSubj (h,[],d')
            -> case (d2 `mod` n) of
                 0  -> reduce (valToTerm xs)
                 d3 -> let (_,tyView -> TyConApp vecTcNm _) = splitFunForallTy ty
                           (Just vecTc)     = HashMap.lookup (nameOcc vecTcNm) tcm
                           [nilCon,consCon] = tyConDataCons vecTc
                       in  reduceWHNF' h2 $
                           mkApps (Prim nm ty)
                                  [Right nTy
                                  ,Right aTy
                                  ,Right (LitTy (NumTy (d3-1)))
                                  ,Left (valToTerm kn)
                                  ,Left (mkApps (Prim "Clash.Sized.Vector.++" (vecAppendTy vecTcNm))
                                                [Right (LitTy (NumTy (n'-1)))
                                                ,Right aTy
                                                ,Right (LitTy (NumTy 1))
                                                ,Left  (Either.lefts vArgs !! 2)
                                                ,Left  (mkVec nilCon consCon aTy 1 [Either.lefts vArgs !! 1])])
                                  ,Left (mkApps (Data snatDc)
                                                [Right (LitTy (NumTy (d3-1)))
                                                ,Left  (Literal (NaturalLiteral (d3-1)))])
                                  ]
         _  -> Nothing
  "Clash.Sized.Vector.rotateRightS"
    | isSubj
    , nTy : aTy : _ : _ <- tys
    , kn : xs : d : _ <- args
    , DC dc _ <- xs
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> case n of
         0  -> reduce (mkVecNil dc aTy)
         n' | DC snatDc [_,Left d'] <- d
            , (h2,[],Literal (NaturalLiteral d2)) <- whnf reduceConstant gbl tcm isSubj (h,[],d')
            -> case (d2 `mod` n) of
                 0  -> reduce (valToTerm xs)
                 d3 -> let (_,tyView -> TyConApp vecTcNm _) = splitFunForallTy ty
                       in  reduceWHNF' h2 $
                           mkApps (Prim nm ty)
                                  [Right nTy
                                  ,Right aTy
                                  ,Right (LitTy (NumTy (d3-1)))
                                  ,Left (valToTerm kn)
                                  ,Left (mkVecCons dc aTy n
                                          (mkApps (Prim "Clash.Sized.Vector.last" (vecHeadTy vecTcNm))
                                                  [Right (LitTy (NumTy (n'-1)))
                                                  ,Right aTy
                                                  ,Left  (valToTerm xs)])
                                          (mkApps (Prim "Clash.Sized.Vector.init" (vecTailTy vecTcNm))
                                                  [Right (LitTy (NumTy (n'-1)))
                                                  ,Right aTy
                                                  ,Left (valToTerm xs)]))
                                  ,Left (mkApps (Data snatDc)
                                                [Right (LitTy (NumTy (d3-1)))
                                                ,Left  (Literal (NaturalLiteral (d3-1)))])
                                  ]
         _  -> Nothing
-- Element-wise operations
-- - mapping
  "Clash.Sized.Vector.map"
    | isSubj
    , DC dc vArgs <- args !! 1
    , aTy : bTy : nTy : _ <- tys
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> case n of
         0  -> reduce (mkVecNil dc bTy)
         n' -> reduce $
               mkVecCons dc bTy n'
                 (mkApps (valToTerm (args !! 0)) [Left (Either.lefts vArgs !! 1)])
                 (mkApps (Prim nm ty) [Right aTy
                                      ,Right bTy
                                      ,Right (LitTy (NumTy (n' - 1)))
                                      ,Left (valToTerm (args !! 0))
                                      ,Left (Either.lefts vArgs !! 2)])
  "Clash.Sized.Vector.imap"
    | isSubj
    , nTy : aTy : bTy : _ <- tys
    , (tyArgs,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
    , let (tyArgs',_) = splitFunForallTy (Either.rights tyArgs !! 1)
    , TyConApp indexTcNm _ <- tyView (Either.rights tyArgs' !! 0)
    , Right n <- runExcept (tyNatSize tcm nTy)
    , Just iLit <- mkIndexLit (Either.rights tyArgs' !! 0) nTy n 0
    -> reduceWHNF $
       mkApps (Prim "Clash.Sized.Vector.imap_go" (vecImapGoTy vecTcNm indexTcNm))
              [Right nTy
              ,Right nTy
              ,Right aTy
              ,Right bTy
              ,Left iLit
              ,Left (valToTerm (args !! 1))
              ,Left (valToTerm (args !! 2))
              ]

  "Clash.Sized.Vector.imap_go"
    | isSubj
    , nTy : mTy : aTy : bTy : _ <- tys
    , n : f : xs : _ <- args
    , DC dc vArgs <- xs
    , Right n' <- runExcept (tyNatSize tcm nTy)
    , Right m <- runExcept (tyNatSize tcm mTy)
    -> case m of
         0  -> reduce (mkVecNil dc bTy)
         m' -> let (tyArgs,_) = splitFunForallTy ty
                   TyConApp indexTcNm _ = tyView (Either.rights tyArgs !! 0)
                   Just iLit = mkIndexLit (Either.rights tyArgs !! 0) nTy n' 1
               in reduce $ mkVecCons dc bTy m'
                 (mkApps (valToTerm f) [Left (valToTerm n),Left (Either.lefts vArgs !! 1)])
                 (mkApps (Prim nm ty)
                         [Right nTy
                         ,Right (LitTy (NumTy (m'-1)))
                         ,Right aTy
                         ,Right bTy
                         ,Left (mkApps (Prim "Clash.Sized.Internal.Index.+#" (indexAddTy indexTcNm))
                                       [Right nTy
                                       ,Left (Literal (NaturalLiteral n'))
                                       ,Left (valToTerm n)
                                       ,Left iLit
                                       ])
                         ,Left (valToTerm f)
                         ,Left (Either.lefts vArgs !! 2)
                         ])

-- - Zipping
  "Clash.Sized.Vector.zipWith"
    | isSubj
    , aTy : bTy : cTy : nTy : _ <- tys
    , f : xs : ys : _   <- args
    , DC dc vArgs <- xs
    , (_,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> case n of
         0  -> reduce (mkVecNil dc cTy)
         n' -> reduce $ mkVecCons dc cTy n'
                 (mkApps (valToTerm f)
                            [Left (Either.lefts vArgs !! 1)
                            ,Left (mkApps (Prim "Clash.Sized.Vector.head" (vecHeadTy vecTcNm))
                                    [Right (LitTy (NumTy (n'-1)))
                                    ,Right bTy
                                    ,Left  (valToTerm ys)
                                    ])
                            ])
                 (mkApps (Prim nm ty) [Right aTy
                                      ,Right bTy
                                      ,Right cTy
                                      ,Right (LitTy (NumTy (n' - 1)))
                                      ,Left (valToTerm f)
                                      ,Left (Either.lefts vArgs !! 2)
                                      ,Left (mkApps (Prim "Clash.Sized.Vector.tail" (vecTailTy vecTcNm))
                                                    [Right (LitTy (NumTy (n'-1)))
                                                    ,Right bTy
                                                    ,Left (valToTerm ys)
                                                    ])])

-- Folding
  "Clash.Sized.Vector.foldr"
    | isSubj
    , aTy : bTy : nTy : _ <- tys
    , f : z : xs : _ <- args
    , DC _ vArgs <- xs
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> case n of
         0 -> reduce (valToTerm z)
         _ -> reduceWHNF $
              mkApps (valToTerm f)
                     [Left (Either.lefts vArgs !! 1)
                     ,Left (mkApps (Prim nm ty)
                                   [Right aTy
                                   ,Right bTy
                                   ,Right (LitTy (NumTy (n-1)))
                                   ,Left  (valToTerm f)
                                   ,Left  (valToTerm z)
                                   ,Left  (Either.lefts vArgs !! 2)
                                   ])
                     ]
  "Clash.Sized.Vector.fold"
    | isSubj
    , aTy : nTy : _ <- tys
    , f : vs : _ <- args
    , DC _ vArgs <- vs
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> case n of
         0 -> reduceWHNF (Either.lefts vArgs !! 1)
         _ -> let (tyArgs,_)         = splitFunForallTy ty
                  TyConApp vecTcNm _ = tyView (Either.rights tyArgs !! 1)
                  tupTcNm      = ghcTyconToTyConName (tupleTyCon Boxed 2)
                  (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
                  [tupDc]      = tyConDataCons tupTc
                  n'     = n+1
                  m      = n' `div` 2
                  n1     = n' - m
                  mTy    = LitTy (NumTy m)
                  m'ty   = LitTy (NumTy (m-1))
                  n1mTy  = LitTy (NumTy n1)
                  n1m'ty = LitTy (NumTy (n1-1))
                  splitAtCall =
                   mkApps (Prim "Clash.Sized.Vector.fold_split" (foldSplitAtTy vecTcNm))
                          [Right mTy
                          ,Right n1mTy
                          ,Right aTy
                          ,Left (Literal (NaturalLiteral m))
                          ,Left (valToTerm vs)
                          ]
                  mVecTy   = mkTyConApp vecTcNm [mTy,aTy]
                  n1mVecTy = mkTyConApp vecTcNm [n1mTy,aTy]
                  asNm     = string2SystemName "as"
                  bsNm     = string2SystemName "bs"
                  asId     = Id asNm (embed mVecTy)
                  bsId     = Id bsNm (embed n1mVecTy)
                  tupPat   = (DataPat (embed tupDc) (rebind [] [asId,bsId]))
                  asAlt    = bind tupPat (Var mVecTy asNm)
                  bsAlt    = bind tupPat (Var n1mVecTy bsNm)
              in  reduceWHNF $
                  mkApps (valToTerm f)
                         [Left (mkApps (Prim nm ty)
                                       [Right aTy
                                       ,Right m'ty
                                       ,Left (valToTerm f)
                                       ,Left (Case splitAtCall mVecTy [asAlt])
                                       ])
                         ,Left (mkApps (Prim nm ty)
                                       [Right aTy
                                       ,Right n1m'ty
                                       ,Left  (valToTerm f)
                                       ,Left  (Case splitAtCall n1mVecTy [bsAlt])
                                       ])
                         ]


  "Clash.Sized.Vector.fold_split"
    | isSubj
    , mTy : nTy : aTy : _ <- tys
    , Right m <- runExcept (tyNatSize tcm mTy)
    -> let -- Get the tuple data-constructor
           (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc)       = HashMap.lookup (nameOcc tupTcNm) tcm
           [tupDc]            = tyConDataCons tupTc
           -- Get the vector data-constructors
           TyConApp vecTcNm _ = tyView (head tyArgs)
           Just vecTc         = HashMap.lookup (nameOcc vecTcNm) tcm
           [nilCon,consCon]   = tyConDataCons vecTc
           -- Recursive call to @splitAt@
           splitAtRec v =
            mkApps (Prim nm ty)
                   [Right (LitTy (NumTy (m-1)))
                   ,Right nTy
                   ,Right aTy
                   ,Left (Literal (NaturalLiteral (m-1)))
                   ,Left v
                   ]
           -- Projection either the first or second field of the recursive
           -- call to @splitAt@
           splitAtSelR v = Case (splitAtRec v) (last tyArgs)
           m1VecTy = mkTyConApp vecTcNm [LitTy (NumTy (m-1)),aTy]
           nVecTy  = mkTyConApp vecTcNm [nTy,aTy]
           lNm     = string2SystemName "l"
           rNm     = string2SystemName "r"
           lId     = Id lNm (embed m1VecTy)
           rId     = Id rNm (embed nVecTy)
           tupPat  = (DataPat (embed tupDc) (rebind [] [lId,rId]))
           lAlt    = bind tupPat (Var m1VecTy lNm)
           rAlt    = bind tupPat (Var nVecTy rNm)
       in case m of
         -- (Nil,v)
         0 -> reduce $
              mkApps (Data tupDc) $ (map Right tyArgs) ++
                [ Left (mkVecNil nilCon aTy)
                , Left (valToTerm (last args))
                ]
         -- (x:xs) <- v
         m' | DC _ vArgs <- last args
            -- (x:fst (splitAt (m-1) xs),snd (splitAt (m-1) xs))
            -> reduce $
               mkApps (Data tupDc) $ (map Right tyArgs) ++
                 [ Left (mkVecCons consCon aTy m' (Either.lefts vArgs !! 1)
                           (splitAtSelR (Either.lefts vArgs !! 2) [lAlt]))
                 , Left (splitAtSelR (Either.lefts vArgs !! 2) [rAlt])
                 ]
         -- v doesn't reduce to a data-constructor
         _  -> Nothing
-- - Specialised folds
  "Clash.Sized.Vector.dfold"
    | isSubj
    , pTy : kTy : aTy : _ <- tys
    , _ : p : f : z : xs : _ <- args
    , DC _ vArgs <- xs
    , Right k' <- runExcept (tyNatSize tcm kTy)
    -> case k'  of
         0 -> reduce (valToTerm z)
         _ -> let (tyArgs,_)  = splitFunForallTy ty
                  (tyArgs',_) = splitFunForallTy (Either.rights tyArgs !! 2)
                  TyConApp snatTcNm _ = tyView (Either.rights tyArgs' !! 0)
                  Just snatTc = HashMap.lookup (nameOcc snatTcNm) tcm
                  [snatDc]    = tyConDataCons snatTc
                  k'ty        = LitTy (NumTy (k'-1))
              in  reduceWHNF $
                  mkApps (valToTerm f)
                         [Right k'ty
                         ,Left (mkApps (Data snatDc)
                                       [Right k'ty
                                       ,Left (Literal (NaturalLiteral (k'-1)))])
                         ,Left (Either.lefts vArgs !! 1)
                         ,Left (mkApps (Prim nm ty)
                                       [Right pTy
                                       ,Right k'ty
                                       ,Right aTy
                                       ,Left (Literal (NaturalLiteral (k'-1)))
                                       ,Left (valToTerm p)
                                       ,Left (valToTerm f)
                                       ,Left (valToTerm z)
                                       ,Left (Either.lefts vArgs !! 2)
                                       ])
                         ]
  "Clash.Sized.Vector.dtfold"
    | isSubj
    , pTy : kTy : aTy : _ <- tys
    , _ : p : f : g : xs : _ <- args
    , DC _ vArgs <- xs
    , Right k' <- runExcept (tyNatSize tcm kTy)
    -> case k' of
         0 -> reduceWHNF (mkApps (valToTerm f) [Left (Either.lefts vArgs !! 1)])
         _ -> let (tyArgs,_)  = splitFunForallTy ty
                  TyConApp vecTcNm _ = tyView (Either.rights tyArgs !! 4)
                  (tyArgs',_) = splitFunForallTy (Either.rights tyArgs !! 3)
                  TyConApp snatTcNm _ = tyView (Either.rights tyArgs' !! 0)
                  Just snatTc = HashMap.lookup (nameOcc snatTcNm) tcm
                  [snatDc]    = tyConDataCons snatTc
                  tupTcNm     = ghcTyconToTyConName (tupleTyCon Boxed 2)
                  (Just tupTc) = HashMap.lookup (nameOcc tupTcNm) tcm
                  [tupDc]     = tyConDataCons tupTc
                  k'ty        = LitTy (NumTy (k'-1))
                  k2ty        = LitTy (NumTy (2^(k'-1)))
                  splitAtCall =
                   mkApps (Prim "Clash.Sized.Vector.splitAt" (splitAtTy snatTcNm vecTcNm))
                          [Right k2ty
                          ,Right k2ty
                          ,Right aTy
                          ,Left (mkApps (Data snatDc)
                                        [Right k2ty
                                        ,Left (Literal (NaturalLiteral (2^(k'-1))))])
                          ,Left (valToTerm xs)
                          ]
                  xsSVecTy = mkTyConApp vecTcNm [k2ty,aTy]
                  xsLNm    = string2SystemName "xsL"
                  xsRNm    = string2SystemName "xsR"
                  xsLId    = Id xsLNm (embed k2ty)
                  xsRId    = Id xsRNm (embed k2ty)
                  tupPat   = (DataPat (embed tupDc) (rebind [] [xsLId,xsRId]))
                  asAlt    = bind tupPat (Var k2ty xsLNm)
                  bsAlt    = bind tupPat (Var k2ty xsRNm)
              in  reduceWHNF $
                  mkApps (valToTerm g)
                         [Right k'ty
                         ,Left (mkApps (Data snatDc)
                                       [Right k'ty
                                       ,Left (Literal (NaturalLiteral (k'-1)))])
                         ,Left (mkApps (Prim nm ty)
                                       [Right pTy
                                       ,Right k'ty
                                       ,Right aTy
                                       ,Left (Literal (NaturalLiteral (k'-1)))
                                       ,Left (valToTerm p)
                                       ,Left (valToTerm f)
                                       ,Left (valToTerm g)
                                       ,Left (Case splitAtCall xsSVecTy [asAlt])])
                         ,Left (mkApps (Prim nm ty)
                                       [Right pTy
                                       ,Right k'ty
                                       ,Right aTy
                                       ,Left (Literal (NaturalLiteral (k'-1)))
                                       ,Left (valToTerm p)
                                       ,Left (valToTerm f)
                                       ,Left (valToTerm g)
                                       ,Left (Case splitAtCall xsSVecTy [bsAlt])])
                         ]
-- Misc
  "Clash.Sized.Vector.lazyV"
    | isSubj
    , nTy : aTy : _ <- tys
    , _ : xs : _ <- args
    , (_,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> case n of
         0  -> let (Just vecTc) = HashMap.lookup (nameOcc vecTcNm) tcm
                   [nilCon,_]   = tyConDataCons vecTc
               in  reduce (mkVecNil nilCon aTy)
         n' -> let (Just vecTc) = HashMap.lookup (nameOcc vecTcNm) tcm
                   [_,consCon]  = tyConDataCons vecTc
               in  reduce $ mkVecCons consCon aTy n'
                     (mkApps (Prim "Clash.Sized.Vector.head" (vecHeadTy vecTcNm))
                             [ Right (LitTy (NumTy (n' - 1)))
                             , Right aTy
                             , Left  (valToTerm xs)
                             ])
                     (mkApps (Prim nm ty)
                             [ Right (LitTy (NumTy (n' - 1)))
                             , Right aTy
                             , Left  (Literal (NaturalLiteral (n'-1)))
                             , Left  (mkApps (Prim "Clash.Sized.Vector.tail" (vecTailTy vecTcNm))
                                             [ Right (LitTy (NumTy (n'-1)))
                                             , Right aTy
                                             , Left  (valToTerm xs)
                                             ])
                             ])
-- Traversable
  "Clash.Sized.Vector.traverse#"
    | isSubj
    , aTy : fTy : bTy : nTy : _ <- tys
    , apDict : f : xs : _ <- args
    , DC dc vArgs <- xs
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> case n of
         0 -> let (pureF,ids') = runPEM (mkSelectorCase $(curLoc) tcm (valToTerm apDict) 1 1) ids
              in  reduceWHNF' (Heap gh h' ids') $
                  mkApps pureF
                         [Right (mkTyConApp (vecTcNm) [nTy,bTy])
                         ,Left  (mkVecNil dc bTy)]
         _ -> let ((fmapF,apF),ids') = flip runPEM ids $ do
                    fDict  <- mkSelectorCase $(curLoc) tcm (valToTerm apDict) 1 0
                    fmapF' <- mkSelectorCase $(curLoc) tcm fDict 1 0
                    apF'   <- mkSelectorCase $(curLoc) tcm (valToTerm apDict) 1 2
                    return (fmapF',apF')
                  n'ty = LitTy (NumTy (n-1))
                  Just (consCoTy : _) = dataConInstArgTys dc [nTy,bTy,n'ty]
              in  reduceWHNF' (Heap gh h' ids') $
                  mkApps apF
                         [Right (mkTyConApp vecTcNm [n'ty,bTy])
                         ,Right (mkTyConApp vecTcNm [nTy,bTy])
                         ,Left (mkApps fmapF
                                       [Right bTy
                                       ,Right (mkFunTy (mkTyConApp vecTcNm [n'ty,bTy])
                                                       (mkTyConApp vecTcNm [nTy,bTy]))
                                       ,Left (mkApps (Data dc)
                                                     [Right nTy
                                                     ,Right bTy
                                                     ,Right n'ty
                                                     ,Left (Prim "_CO_" consCoTy)])
                                       ,Left (mkApps (valToTerm f)
                                                     [Left (Either.lefts vArgs !! 1)])
                                       ])
                         ,Left (mkApps (Prim nm ty)
                                       [Right aTy
                                       ,Right fTy
                                       ,Right bTy
                                       ,Right n'ty
                                       ,Left (valToTerm apDict)
                                       ,Left (valToTerm f)
                                       ,Left (Either.lefts vArgs !! 2)
                                       ])
                         ]
    where
      (tyArgs,_)         = splitFunForallTy ty
      TyConApp vecTcNm _ = tyView (Either.rights tyArgs !! 2)
      Heap gh h' ids     = h

-- BitPack
  "Clash.Sized.Vector.concatBitVector#"
    | isSubj
    , nTy : mTy : _ <- tys
    , _  : km  : v : _ <- args
    , DC _ vArgs <- v
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> case n of
         0  -> let resTyInfo = extractTySizeInfo tcm ty tys
               in  reduce (mkBitVectorLit' resTyInfo 0 0)
         n' | Right m <- runExcept (tyNatSize tcm mTy)
            , (_,tyView -> TyConApp bvTcNm _) <- splitFunForallTy ty
            -> reduceWHNF $
               mkApps (Prim "Clash.Sized.Internal.BitVector.++#" (bvAppendTy bvTcNm))
                 [ Right (mkTyConApp typeNatMul [LitTy (NumTy (n'-1)),mTy])
                 , Right mTy
                 , Left (Literal (NaturalLiteral ((n'-1)*m)))
                 , Left (Either.lefts vArgs !! 1)
                 , Left (mkApps (Prim nm ty)
                                [ Right (LitTy (NumTy (n'-1)))
                                , Right mTy
                                , Left (Literal (NaturalLiteral (n'-1)))
                                , Left (valToTerm km)
                                , Left (Either.lefts vArgs !! 2)
                                ])
                 ]
         _ -> Nothing
  "Clash.Sized.Vector.unconcatBitVector#"
    | isSubj
    , nTy : mTy : _  <- tys
    , _  : km  : bv : _ <- args
    , (_,tyView -> TyConApp vecTcNm [_,bvMTy]) <- splitFunForallTy ty
    , TyConApp bvTcNm _ <- tyView bvMTy
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> case n of
         0 ->
          let (Just vecTc) = HashMap.lookup (nameOcc vecTcNm) tcm
              [nilCon,_] = tyConDataCons vecTc
          in  reduce (mkVecNil nilCon (mkTyConApp bvTcNm [mTy]))
         n' | Right m <- runExcept (tyNatSize tcm mTy) ->
          let Just vecTc  = HashMap.lookup (nameOcc vecTcNm) tcm
              [_,consCon] = tyConDataCons vecTc
              tupTcNm     = ghcTyconToTyConName (tupleTyCon Boxed 2)
              Just tupTc  = HashMap.lookup (nameOcc tupTcNm) tcm
              [tupDc]     = tyConDataCons tupTc
              splitCall   =
                mkApps (Prim "Clash.Sized.Internal.BitVector.split#"
                             (bvSplitTy bvTcNm))
                       [ Right (mkTyConApp typeNatMul [LitTy (NumTy (n'-1)),mTy])
                       , Right mTy
                       , Left (Literal (NaturalLiteral ((n'-1)*m)))
                       , Left (valToTerm bv)
                       ]
              mBVTy       = mkTyConApp bvTcNm [mTy]
              n1BVTy      = mkTyConApp bvTcNm
                              [mkTyConApp typeNatMul
                                [LitTy (NumTy (n'-1))
                                ,mTy]]
              xNm         = string2SystemName "x"
              bvNm        = string2SystemName "bv'"
              xId         = Id xNm (embed mBVTy)
              bvId        = Id bvNm (embed n1BVTy)
              tupPat      = DataPat (embed tupDc) (rebind [] [xId,bvId])
              xAlt        = bind tupPat (Var mBVTy xNm)
              bvAlt       = bind tupPat (Var n1BVTy bvNm)

          in  reduce $ mkVecCons consCon (mkTyConApp bvTcNm [mTy]) n'
                (Case splitCall mBVTy [xAlt])
                (mkApps (Prim nm ty)
                        [ Right (LitTy (NumTy (n'-1)))
                        , Right mTy
                        , Left (Literal (NaturalLiteral (n'-1)))
                        , Left (valToTerm km)
                        , Left (Case splitCall n1BVTy [bvAlt])
                        ])
         _ -> Nothing
  _ -> Nothing
  where
    reduce e = case isX e of
      Left msg -> trace (unlines ["Warning: Not evaluating constant expression:", show nm, "Because doing so generates an XException:", msg]) Nothing
      Right e' -> Just (h,k,e')
    reduceWHNF e = let (h2,[],e') = whnf reduceConstant gbl tcm isSubj (h,[],e)
                   in  Just (h2,k,e')
    reduceWHNF' h' e = let (h2,[],e') = whnf reduceConstant gbl tcm isSubj (h',[],e)
                       in  Just (h2,k,e')

typedLiterals' :: (Value -> Maybe a) -> [Value] -> [a]
typedLiterals' typedLiteral = mapMaybe typedLiteral

doubleLiterals' :: [Value] -> [Rational]
doubleLiterals' = typedLiterals' doubleLiteral
  where
    doubleLiteral x = case x of
      Lit (DoubleLiteral i) -> Just i
      _ -> Nothing

floatLiterals' :: [Value] -> [Rational]
floatLiterals' = typedLiterals' floatLiteral
  where
    floatLiteral x = case x of
      Lit (FloatLiteral i) -> Just i
      _ -> Nothing

integerLiterals :: [Value] -> Maybe (Integer,Integer)
integerLiterals args = case integerLiterals' args of
  [i,j] -> Just (i,j)
  _ -> Nothing

integerLiterals' :: [Value] -> [Integer]
integerLiterals' = typedLiterals' integerLiteral
 where
  integerLiteral x = case x of
    Lit (IntegerLiteral i)      -> Just i
    DC dc [Left (Literal (IntLiteral i))]
      | dcTag dc == 1
      -> Just i
    DC dc [Left (Literal (ByteArrayLiteral (Vector.Vector _ _ (ByteArray.ByteArray ba))))]
      | dcTag dc == 2
      -> Just (Jp# (BN# ba))
      | dcTag dc == 3
      -> Just (Jn# (BN# ba))
    _ -> Nothing

naturalLiterals' :: [Value] -> [Integer]
naturalLiterals' = typedLiterals' naturalLiteral
 where
  naturalLiteral x = case x of
    Lit (NaturalLiteral i) -> Just i
    DC dc [Left (Literal (WordLiteral i))]
      | dcTag dc == 1
      -> Just i
    DC dc [Left (Literal (ByteArrayLiteral (Vector.Vector _ _ (ByteArray.ByteArray ba))))]
      | dcTag dc == 2
      -> Just (Jp# (BN# ba))
    _ -> Nothing

intLiterals :: [Value] -> Maybe (Integer,Integer)
intLiterals args = case args of
  [Lit (IntLiteral i), Lit (IntLiteral j)] -> Just (i,j)
  _ -> Nothing

intLiterals' :: [Value] -> [Integer]
intLiterals' = typedLiterals' intLiteral
  where
    intLiteral x = case x of
      Lit (IntLiteral i) -> Just i
      _ -> Nothing

intCLiterals :: [Value] -> Maybe (Integer,Integer)
intCLiterals args = case args of
  ([DC _ [Left (Literal (IntLiteral i))]
   ,DC _ [Left (Literal (IntLiteral j))]])
    -> Just (i,j)
  _ -> Nothing

wordLiterals :: [Value] -> Maybe (Integer,Integer)
wordLiterals args = case args of
  [Lit (WordLiteral i), Lit (WordLiteral j)] -> Just (i,j)
  _ -> Nothing
wordLiterals' :: [Value] -> [Integer]
wordLiterals' = typedLiterals' wordLiteral
  where
    wordLiteral x = case x of
      Lit (WordLiteral i) -> Just i
      _ -> Nothing

charLiterals :: [Value] -> Maybe (Char,Char)
charLiterals args = case args of
  [Lit (CharLiteral i), Lit (CharLiteral j)] -> Just (i,j)
  _ -> Nothing

charLiterals' :: [Value] -> [Char]
charLiterals' = typedLiterals' charLiteral
  where
    charLiteral x = case x of
      Lit (CharLiteral c) -> Just c
      _ -> Nothing

sizedLiterals :: Text -> [Value] -> Maybe (Integer,Integer)
sizedLiterals szCon args
  = case args of
      ([ PrimVal nm  _ _ [_, Lit (IntegerLiteral i)]
       , PrimVal nm' _ _ [_, Lit (IntegerLiteral j)]])
        | nm  == szCon
        , nm' == szCon -> Just (i,j)
      _ -> Nothing

sizedLiterals' :: Text -> [Value] -> [Integer]
sizedLiterals' szCon = typedLiterals' (sizedLiteral szCon)

sizedLiteral :: Text -> Value -> Maybe Integer
sizedLiteral szCon val = case val of
  PrimVal nm  _ _ [_, Lit (IntegerLiteral i)] | nm == szCon -> Just i
  _ -> Nothing

bitLiterals
  :: [Value]
  -> [(Integer,Integer)]
bitLiterals = typedLiterals' go
 where
  go val = case val of
    PrimVal nm _ _ [Lit (IntegerLiteral m), Lit (IntegerLiteral i)]
      | nm == "Clash.Sized.Internal.BitVector.fromInteger##"
      -> Just (m,i)
    _ -> Nothing

indexLiterals, signedLiterals, unsignedLiterals
  :: [Value] -> Maybe (Integer,Integer)
indexLiterals     = sizedLiterals "Clash.Sized.Internal.Index.fromInteger#"
signedLiterals    = sizedLiterals "Clash.Sized.Internal.Signed.fromInteger#"
unsignedLiterals  = sizedLiterals "Clash.Sized.Internal.Unsigned.fromInteger#"

bitVectorLiterals
  :: [Value] -> Maybe ((Integer,Integer),(Integer,Integer))
bitVectorLiterals args
  = case args of
      ([ PrimVal nm  _ _ [_, Lit (IntegerLiteral mi), Lit (IntegerLiteral i)]
       , PrimVal nm' _ _ [_, Lit (IntegerLiteral mj), Lit (IntegerLiteral j)]])
        | nm  == "Clash.Sized.Internal.BitVector.fromInteger#"
        , nm' == "Clash.Sized.Internal.BitVector.fromInteger#" -> Just ((mi,i),(mj,j))
      _ -> Nothing

indexLiterals', signedLiterals', unsignedLiterals'
  :: [Value] -> [Integer]
indexLiterals'     = sizedLiterals' "Clash.Sized.Internal.Index.fromInteger#"
signedLiterals'    = sizedLiterals' "Clash.Sized.Internal.Signed.fromInteger#"
unsignedLiterals'  = sizedLiterals' "Clash.Sized.Internal.Unsigned.fromInteger#"

bitVectorLiterals'
  :: [Value] -> [(Integer,Integer)]
bitVectorLiterals' = mapMaybe go
 where
  go :: Value -> Maybe (Integer,Integer)
  go val = case val of
    PrimVal nm  _ _ [_, Lit (IntegerLiteral mi), Lit (IntegerLiteral i)]
      | nm == "Clash.Sized.Internal.BitVector.fromInteger#" -> Just (mi, i)
    _ -> Nothing

toBV :: (Integer,Integer) -> BitVector n
toBV = uncurry BV

splitBV :: BitVector n -> (Integer,Integer)
splitBV (BV msk val) = (msk,val)

valArgs
  :: Value
  -> Maybe [Term]
valArgs (PrimVal _ _ _ vs) = Just (map valToTerm vs)
valArgs (DC _ args)        = Just (Either.lefts args)
valArgs _                  = Nothing


-- Tries to match literal arguments to a function like
--   (Unsigned.shiftL#  :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n)
sizedLitIntLit
  :: Text -> TyConMap -> [Type] -> [Value]
  -> Maybe (Type,Integer,Integer,Integer)
sizedLitIntLit szCon tcm tys args
  | Just (nTy,kn) <- extractKnownNat tcm tys
  , [_
    ,PrimVal nm _ _ [_,Lit (IntegerLiteral i)]
    ,valArgs -> Just [Literal (IntLiteral j)]
    ] <- args
  , nm == szCon
  = Just (nTy,kn,i,j)
  | otherwise
  = Nothing

signedLitIntLit, unsignedLitIntLit
  :: TyConMap -> [Type] -> [Value]
  -> Maybe (Type,Integer,Integer,Integer)
signedLitIntLit    = sizedLitIntLit "Clash.Sized.Internal.Signed.fromInteger#"
unsignedLitIntLit  = sizedLitIntLit "Clash.Sized.Internal.Unsigned.fromInteger#"

bitVectorLitIntLit
  :: TyConMap -> [Type] -> [Value]
  -> Maybe (Type,Integer,(Integer,Integer),Integer)
bitVectorLitIntLit tcm tys args
  | Just (nTy,kn) <- extractKnownNat tcm tys
  , [_
    ,PrimVal nm _ _ [_,Lit (IntegerLiteral m),Lit (IntegerLiteral i)]
    ,valArgs -> Just [Literal (IntLiteral j)]
    ] <- args
  , nm == "Clash.Sized.Internal.BitVector.fromInteger#"
  = Just (nTy,kn,(m,i),j)
  | otherwise
  = Nothing

-- From an argument list to function of type
--   forall n. KnownNat n => ...
-- extract (nTy,nInt)
-- where nTy is the Type of n
-- and   nInt is its value as an Integer
extractKnownNat :: HashMap.HashMap TyConOccName TyCon -> [Type] -> Maybe (Type, Integer)
extractKnownNat tcm tys = case tys of
  nTy : _ | Right nInt <- runExcept (tyNatSize tcm nTy)
    -> Just (nTy, nInt)
  _ -> Nothing

-- Construct a constant term of a sized type
mkSizedLit
  :: (Type -> Term)    -- type constructor?
  -> Type    -- result type
  -> Type    -- forall n.
  -> Integer -- KnownNat n
  -> Integer -- value
  -> Term
mkSizedLit conPrim ty nTy kn val
  = mkApps (conPrim sTy) [Right nTy,Left (Literal (NaturalLiteral kn)),Left (Literal (IntegerLiteral ( val)))]
  where
    (_,sTy) = splitFunForallTy ty

mkBitLit
  :: Type
  -- ^ Result type
  -> Integer
  -- ^ Mask
  -> Integer
  -- ^ Value
  -> Term
mkBitLit ty msk val =
  mkApps (bConPrim sTy) [Left (Literal (IntegerLiteral msk)), Left (Literal (IntegerLiteral val))]
  where
    (_,sTy) = splitFunForallTy ty

mkSignedLit, mkUnsignedLit
  :: Type    -- result type
  -> Type    -- forall n.
  -> Integer -- KnownNat n
  -> Integer -- value
  -> Term
mkSignedLit    = mkSizedLit signedConPrim
mkUnsignedLit  = mkSizedLit unsignedConPrim

mkBitVectorLit
  :: Type    -- result type
  -> Type    -- forall n.
  -> Integer -- KnownNat n
  -> Integer -- mask
  -> Integer -- value
  -> Term
mkBitVectorLit ty nTy kn mask val
  = mkApps (bvConPrim sTy)
           [Right nTy
           ,Left (Literal (NaturalLiteral kn))
           ,Left (Literal (IntegerLiteral mask))
           ,Left (Literal (IntegerLiteral val))]
  where
    (_,sTy) = splitFunForallTy ty

mkIndexLit
  :: Type    -- result type
  -> Type    -- forall n.
  -> Integer -- KnownNat n
  -> Integer -- value
  -> Maybe Term
mkIndexLit rTy nTy kn val
  | val >= 0
  , val < kn
  = Just (mkSizedLit indexConPrim rTy nTy kn val)
  | otherwise
  = Nothing

-- Construct a constant term of a sized type
mkSizedLit'
  :: (Type -> Term)    -- type constructor?
  -> (Type     -- result type
     ,Type     -- forall n.
     ,Integer) -- KnownNat n
  -> Integer -- value
  -> Term
mkSizedLit' conPrim (ty,nTy,kn) val
  = mkApps (conPrim sTy) [Right nTy,Left (Literal (NaturalLiteral kn)),Left (Literal (IntegerLiteral ( val)))]
  where
    (_,sTy) = splitFunForallTy ty

mkSignedLit', mkUnsignedLit'
  :: (Type     -- result type
     ,Type     -- forall n.
     ,Integer) -- KnownNat n
  -> Integer -- value
  -> Term
mkSignedLit'    = mkSizedLit' signedConPrim
mkUnsignedLit'  = mkSizedLit' unsignedConPrim

mkBitVectorLit'
  :: (Type     -- result type
     ,Type     -- forall n.
     ,Integer) -- KnownNat n
  -> Integer -- Mask
  -> Integer -- value
  -> Term
mkBitVectorLit' (ty,nTy,kn) mask val
  = mkApps (bvConPrim sTy)
           [Right nTy
           ,Left (Literal (NaturalLiteral kn))
           ,Left (Literal (IntegerLiteral mask))
           ,Left (Literal (IntegerLiteral val))]
  where
    (_,sTy) = splitFunForallTy ty

mkIndexLit'
  :: (Type     -- result type
     ,Type     -- forall n.
     ,Integer) -- KnownNat n
  -> Integer -- value
  -> Maybe Term
mkIndexLit' res@(_,_,kn) val
  | val >= 0
  , val < kn
  = Just (mkSizedLit' indexConPrim res val)
  | otherwise
  = Nothing


-- | Create a vector of supplied elements
mkVecCons
  :: DataCon -- ^ The Cons (:>) constructor
  -> Type    -- ^ Element type
  -> Integer -- ^ Length of the vector
  -> Term    -- ^ head of the vector
  -> Term    -- ^ tail of the vector
  -> Term
mkVecCons consCon resTy n h t =
  mkApps (Data consCon) [Right (LitTy (NumTy n))
                        ,Right resTy
                        ,Right (LitTy (NumTy (n-1)))
                        ,Left (Prim "_CO_" consCoTy)
                        ,Left h
                        ,Left t]

  where
    args = dataConInstArgTys consCon [LitTy (NumTy n),resTy,LitTy (NumTy (n-1))]
    Just (consCoTy : _) = args

-- | Create an empty vector
mkVecNil
  :: DataCon
  -- ^ The Nil constructor
  -> Type
  -- ^ The element type
  -> Term
mkVecNil nilCon resTy =
  mkApps (Data nilCon) [Right (LitTy (NumTy 0))
                       ,Right resTy
                       ,Left  (Prim "_CO_" nilCoTy)
                       ]
  where
    args = dataConInstArgTys nilCon [LitTy (NumTy 0),resTy]
    Just (nilCoTy : _ ) = args

boolToIntLiteral :: Bool -> Term
boolToIntLiteral b = if b then Literal (IntLiteral 1) else Literal (IntLiteral 0)

boolToBoolLiteral :: HashMap.HashMap TyConOccName TyCon -> Type -> Bool -> Term
boolToBoolLiteral tcm ty b =
 let (_,tyView -> TyConApp boolTcNm []) = splitFunForallTy ty
     (Just boolTc) = HashMap.lookup (nameOcc boolTcNm) tcm
     [falseDc,trueDc] = tyConDataCons boolTc
     retDc = if b then trueDc else falseDc
 in  Data retDc

charToCharLiteral :: Char -> Term
charToCharLiteral = Literal . CharLiteral

integerToIntLiteral :: Integer -> Term
integerToIntLiteral = Literal . IntLiteral . toInteger . (fromInteger :: Integer -> Int) -- for overflow behaviour

integerToWordLiteral :: Integer -> Term
integerToWordLiteral = Literal . WordLiteral . toInteger . (fromInteger :: Integer -> Word) -- for overflow behaviour

integerToIntegerLiteral :: Integer -> Term
integerToIntegerLiteral = Literal . IntegerLiteral

bConPrim :: Type -> Term
bConPrim (tyView -> TyConApp bTcNm _)
  = Prim "Clash.Sized.Internal.BitVector.fromInteger##" funTy
  where
    funTy      = foldr1 mkFunTy [integerPrimTy,integerPrimTy,mkTyConApp bTcNm []]
bConPrim _ = error $ $(curLoc) ++ "called with incorrect type"

bvConPrim :: Type -> Term
bvConPrim (tyView -> TyConApp bvTcNm _)
  = Prim "Clash.Sized.Internal.BitVector.fromInteger#" (ForAllTy (bind nTV funTy))
  where
    funTy = foldr1 mkFunTy [naturalPrimTy,integerPrimTy,integerPrimTy,mkTyConApp bvTcNm [nVar]]
    nName = string2SystemName "n"
    nVar  = VarTy typeNatKind nName
    nTV   = TyVar nName (embed typeNatKind)
bvConPrim _ = error $ $(curLoc) ++ "called with incorrect type"

indexConPrim :: Type -> Term
indexConPrim (tyView -> TyConApp indexTcNm _)
  = Prim "Clash.Sized.Internal.Index.fromInteger#" (ForAllTy (bind nTV funTy))
  where
    funTy        = foldr1 mkFunTy [naturalPrimTy,integerPrimTy,mkTyConApp indexTcNm [nVar]]
    nName      = string2SystemName "n"
    nVar       = VarTy typeNatKind nName
    nTV        = TyVar nName (embed typeNatKind)
indexConPrim _ = error $ $(curLoc) ++ "called with incorrect type"

signedConPrim :: Type -> Term
signedConPrim (tyView -> TyConApp signedTcNm _)
  = Prim "Clash.Sized.Internal.Signed.fromInteger#" (ForAllTy (bind nTV funTy))
  where
    funTy        = foldr1 mkFunTy [naturalPrimTy,integerPrimTy,mkTyConApp signedTcNm [nVar]]
    nName      = string2SystemName "n"
    nVar       = VarTy typeNatKind nName
    nTV        = TyVar nName (embed typeNatKind)
signedConPrim _ = error $ $(curLoc) ++ "called with incorrect type"

unsignedConPrim :: Type -> Term
unsignedConPrim (tyView -> TyConApp unsignedTcNm _)
  = Prim "Clash.Sized.Internal.Unsigned.fromInteger#" (ForAllTy (bind nTV funTy))
  where
    funTy        = foldr1 mkFunTy [naturalPrimTy,integerPrimTy,mkTyConApp unsignedTcNm [nVar]]
    nName        = string2SystemName "n"
    nVar         = VarTy typeNatKind nName
    nTV          = TyVar nName (embed typeNatKind)
unsignedConPrim _ = error $ $(curLoc) ++ "called with incorrect type"


-- |  Lift a binary function over 'Unsigned' values to be used as literal Evaluator
--
--
liftUnsigned2 :: KnownNat n
              => (Unsigned n -> Unsigned n -> Unsigned n)
              -> Type
              -> TyConMap
              -> [Type]
              -> [Value]
              -> (Proxy n -> Maybe Term)
liftUnsigned2 = liftSized2 unsignedLiterals' mkUnsignedLit

liftSigned2 :: KnownNat n
              => (Signed n -> Signed n -> Signed n)
              -> Type
              -> TyConMap
              -> [Type]
              -> [Value]
              -> (Proxy n -> Maybe Term)
liftSigned2 = liftSized2 signedLiterals' mkSignedLit

liftBitVector2 :: KnownNat n
              => (BitVector n -> BitVector n -> BitVector n)
              -> Type
              -> TyConMap
              -> [Type]
              -> [Value]
              -> (Proxy n -> Maybe Term)
liftBitVector2  f ty tcm tys args _p
  | Just (nTy, kn) <- extractKnownNat tcm tys
  , [i,j] <- bitVectorLiterals' args
  = let BV mask val = f (toBV i) (toBV j)
    in Just $ mkBitVectorLit ty nTy kn mask val
  | otherwise = Nothing

liftBitVector2Bool :: KnownNat n
              => (BitVector n -> BitVector n -> Bool)
              -> Type
              -> TyConMap
              -> [Value]
              -> (Proxy n -> Maybe Term)
liftBitVector2Bool  f ty tcm args _p
  | [i,j] <- bitVectorLiterals' args
  = let val = f (toBV i) (toBV j)
    in Just $ boolToBoolLiteral tcm ty val
  | otherwise = Nothing

liftSized2 :: (KnownNat n, Integral (sized n))
           => ([Value] -> [Integer])
              -- ^ literal argument extraction function
           -> (Type -> Type -> Integer -> Integer -> Term)
              -- ^ literal contruction function
           -> (sized n -> sized n -> sized n)
           -> Type
           -> TyConMap
           -> [Type]
           -> [Value]
           -> (Proxy n -> Maybe Term)
liftSized2 extractLitArgs mkLit f ty tcm tys args p
  | Just (nTy, kn) <- extractKnownNat tcm tys
  , [i,j] <- extractLitArgs args
  = let val = runSizedF f i j p
    in Just $ mkLit ty nTy kn val
  | otherwise = Nothing

-- | Helper to run a function over sized types on integers
--
-- This only works on function of type (sized n -> sized n -> sized n)
-- The resulting function must be executed with reifyNat
runSizedF
  :: (KnownNat n, Integral (sized n))
  => (sized n -> sized n -> sized n)   -- ^ function to run
  -> Integer                           -- ^ first  argument
  -> Integer                           -- ^ second argument
  -> (Proxy n -> Integer)
runSizedF f i j _ = toInteger $ f (fromInteger i) (fromInteger j)

extractTySizeInfo :: TyConMap -> Type -> [Type] -> (Type, Type, Integer)
extractTySizeInfo tcm ty tys = (resTy,resSizeTy,resSize)
  where
    ty' = List.foldl' ((runFreshM .) . applyTy tcm) ty tys
    (_,resTy) = splitFunForallTy ty'
    TyConApp _ [resSizeTy] = tyView resTy
    Right resSize = runExcept (tyNatSize tcm resSizeTy)

getResultTy
  :: TyConMap
  -> Type
  -> [Type]
  -> Type
getResultTy tcm ty tys = resTy
 where
  ty' = List.foldl' ((runFreshM .) . applyTy tcm) ty tys
  (_,resTy) = splitFunForallTy ty'

liftDDI :: (Double# -> Double# -> Int#) -> [Value] -> Maybe Term
liftDDI f args = case doubleLiterals' args of
  [i,j] -> Just $ runDDI f i j
  _     -> Nothing
liftDDD :: (Double# -> Double# -> Double#) -> [Value] -> Maybe Term
liftDDD f args = case doubleLiterals' args of
  [i,j] -> Just $ runDDD f i j
  _     -> Nothing
liftDD  :: (Double# -> Double#) -> [Value] -> Maybe Term
liftDD  f args = case doubleLiterals' args of
  [i]   -> Just $ runDD f i
  _     -> Nothing
runDDI :: (Double# -> Double# -> Int#) -> Rational -> Rational -> Term
runDDI f i j
  = let !(D# a) = fromRational i
        !(D# b) = fromRational j
        r = f a b
    in  Literal . IntLiteral . toInteger $ I# r
runDDD :: (Double# -> Double# -> Double#) -> Rational -> Rational -> Term
runDDD f i j
  = let !(D# a) = fromRational i
        !(D# b) = fromRational j
        r = f a b
    in  Literal . DoubleLiteral . toRational $ D# r
runDD :: (Double# -> Double#) -> Rational -> Term
runDD f i
  = let !(D# a) = fromRational i
        r = f a
    in  Literal . DoubleLiteral . toRational $ D# r

liftFFI :: (Float# -> Float# -> Int#) -> [Value] -> Maybe Term
liftFFI f args = case floatLiterals' args of
  [i,j] -> Just $ runFFI f i j
  _     -> Nothing
liftFFF :: (Float# -> Float# -> Float#) -> [Value] -> Maybe Term
liftFFF f args = case floatLiterals' args of
  [i,j] -> Just $ runFFF f i j
  _     -> Nothing
liftFF  :: (Float# -> Float#) -> [Value] -> Maybe Term
liftFF  f args = case floatLiterals' args of
  [i]   -> Just $ runFF f i
  _     -> Nothing
runFFI :: (Float# -> Float# -> Int#) -> Rational -> Rational -> Term
runFFI f i j
  = let !(F# a) = fromRational i
        !(F# b) = fromRational j
        r = f a b
    in  Literal . IntLiteral . toInteger $ I# r
runFFF :: (Float# -> Float# -> Float#) -> Rational -> Rational -> Term
runFFF f i j
  = let !(F# a) = fromRational i
        !(F# b) = fromRational j
        r = f a b
    in  Literal . FloatLiteral . toRational $ F# r
runFF :: (Float# -> Float#) -> Rational -> Term
runFF f i
  = let !(F# a) = fromRational i
        r = f a
    in  Literal . FloatLiteral . toRational $ F# r


vecHeadTy
  :: TyConName
  -- ^ Vec TyCon name
  -> Type
vecHeadTy vecNm =
    ForAllTy (bind nTV (
    ForAllTy (bind aTV (
    mkFunTy
      (mkTyConApp vecNm [mkTyConApp typeNatAdd
                           [VarTy typeNatKind (string2SystemName "n")
                           ,LitTy (NumTy 1)]
                        ,VarTy liftedTypeKind (string2SystemName "a")
                        ])
      (VarTy liftedTypeKind (string2SystemName "a"))))))
  where
    aTV = TyVar (string2SystemName "a") (embed liftedTypeKind)
    nTV = TyVar (string2SystemName "n") (embed typeNatKind)

vecTailTy
  :: TyConName
  -- ^ Vec TyCon name
  -> Type
vecTailTy vecNm =
    ForAllTy (bind nTV (
    ForAllTy (bind aTV (
    mkFunTy
      (mkTyConApp vecNm [mkTyConApp typeNatAdd
                           [VarTy typeNatKind (string2SystemName "n")
                           ,LitTy (NumTy 1)]
                        ,VarTy liftedTypeKind (string2SystemName "a")
                        ])
      (mkTyConApp vecNm [VarTy typeNatKind    (string2SystemName "n")
                        ,VarTy liftedTypeKind (string2SystemName "a")
                        ])))))
  where
    nTV = TyVar (string2SystemName "n") (embed typeNatKind)
    aTV = TyVar (string2SystemName "a") (embed liftedTypeKind)

splitAtTy
  :: TyConName
  -- ^ SNat TyCon name
  -> TyConName
  -- ^ Vec TyCon name
  -> Type
splitAtTy snatNm vecNm =
  ForAllTy (bind mTV (
  ForAllTy (bind nTV (
  ForAllTy (bind aTV (
  mkFunTy
    (mkTyConApp snatNm [VarTy typeNatKind (string2SystemName "m")])
    (mkFunTy
      (mkTyConApp vecNm
                  [mkTyConApp typeNatAdd
                    [VarTy typeNatKind (string2SystemName "m")
                    ,VarTy typeNatKind (string2SystemName "n")]
                  ,VarTy liftedTypeKind (string2SystemName "a")])
      (mkTyConApp tupNm
                  [mkTyConApp vecNm
                              [VarTy typeNatKind (string2SystemName "m")
                              ,VarTy liftedTypeKind (string2SystemName "a")]
                  ,mkTyConApp vecNm
                              [VarTy typeNatKind (string2SystemName "n")
                              ,VarTy liftedTypeKind (string2SystemName "a")]]))))))))
  where
    mTV   = TyVar (string2SystemName "m") (embed typeNatKind)
    nTV   = TyVar (string2SystemName "n") (embed typeNatKind)
    aTV   = TyVar (string2SystemName "a") (embed liftedTypeKind)
    tupNm = ghcTyconToTyConName (tupleTyCon Boxed 2)

foldSplitAtTy
  :: TyConName
  -- ^ Vec TyCon name
  -> Type
foldSplitAtTy vecNm =
  ForAllTy (bind mTV (
  ForAllTy (bind nTV (
  ForAllTy (bind aTV (
  mkFunTy
    naturalPrimTy
    (mkFunTy
      (mkTyConApp vecNm
                  [mkTyConApp typeNatAdd
                    [VarTy typeNatKind (string2SystemName "m")
                    ,VarTy typeNatKind (string2SystemName "n")]
                  ,VarTy liftedTypeKind (string2SystemName "a")])
      (mkTyConApp tupNm
                  [mkTyConApp vecNm
                              [VarTy typeNatKind (string2SystemName "m")
                              ,VarTy liftedTypeKind (string2SystemName "a")]
                  ,mkTyConApp vecNm
                              [VarTy typeNatKind (string2SystemName "n")
                              ,VarTy liftedTypeKind (string2SystemName "a")]]))))))))
  where
    mTV   = TyVar (string2SystemName "m") (embed typeNatKind)
    nTV   = TyVar (string2SystemName "n") (embed typeNatKind)
    aTV   = TyVar (string2SystemName "a") (embed liftedTypeKind)
    tupNm = ghcTyconToTyConName (tupleTyCon Boxed 2)

vecAppendTy
  :: TyConName
  -- ^ Vec TyCon name
  -> Type
vecAppendTy vecNm =
    ForAllTy (bind nTV (
    ForAllTy (bind aTV (
    ForAllTy (bind mTV (
    mkFunTy
      (mkTyConApp vecNm [VarTy typeNatKind    (string2SystemName "n")
                        ,VarTy liftedTypeKind (string2SystemName "a")
                        ])
      (mkFunTy
         (mkTyConApp vecNm [VarTy typeNatKind    (string2SystemName "m")
                           ,VarTy liftedTypeKind (string2SystemName "a")
                           ])
         (mkTyConApp vecNm [mkTyConApp typeNatAdd
                              [VarTy typeNatKind (string2SystemName "n")
                              ,VarTy typeNatKind (string2SystemName "m")]
                           ,VarTy liftedTypeKind (string2SystemName "a")
                           ]))))))))
  where
    nTV = TyVar (string2SystemName "n") (embed typeNatKind)
    aTV = TyVar (string2SystemName "a") (embed liftedTypeKind)
    mTV = TyVar (string2SystemName "m") (embed typeNatKind)

vecZipWithTy
  :: TyConName
  -- ^ Vec TyCon name
  -> Type
vecZipWithTy vecNm =
  ForAllTy (bind aTV (
  ForAllTy (bind bTV (
  ForAllTy (bind cTV (
  ForAllTy (bind nTV (
  mkFunTy
    (mkFunTy aTy (mkFunTy bTy cTy))
    (mkFunTy
      (mkTyConApp vecNm [nTy,aTy])
      (mkFunTy
        (mkTyConApp vecNm [nTy,bTy])
        (mkTyConApp vecNm [nTy,cTy])))))))))))
  where
    aTV = TyVar (string2SystemName "a") (embed liftedTypeKind)
    bTV = TyVar (string2SystemName "b") (embed liftedTypeKind)
    cTV = TyVar (string2SystemName "c") (embed liftedTypeKind)
    nTV = TyVar (string2SystemName "n") (embed typeNatKind)
    aTy = VarTy liftedTypeKind (string2SystemName "a")
    bTy = VarTy liftedTypeKind (string2SystemName "b")
    cTy = VarTy liftedTypeKind (string2SystemName "c")
    nTy = VarTy typeNatKind (string2SystemName "n")

vecImapGoTy
  :: TyConName
  -- ^ Vec TyCon name
  -> TyConName
  -- ^ Index TyCon name
  -> Type
vecImapGoTy vecTcNm indexTcNm =
  ForAllTy (bind nTV (
  ForAllTy (bind mTV (
  ForAllTy (bind aTV (
  ForAllTy (bind bTV (
  mkFunTy indexTy
    (mkFunTy fTy
       (mkFunTy vecATy vecBTy))))))))))
  where
    nTV = TyVar (string2SystemName "n") (embed typeNatKind)
    mTV = TyVar (string2SystemName "m") (embed typeNatKind)
    aTV = TyVar (string2SystemName "a") (embed liftedTypeKind)
    bTV = TyVar (string2SystemName "b") (embed liftedTypeKind)
    indexTy = mkTyConApp indexTcNm [nTy]
    nTy = VarTy typeNatKind (string2SystemName "n")
    mTy = VarTy typeNatKind (string2SystemName "m")
    fTy = mkFunTy indexTy (mkFunTy aTy bTy)
    aTy = VarTy liftedTypeKind (string2SystemName "a")
    bTy = VarTy liftedTypeKind (string2SystemName "b")
    vecATy = mkTyConApp vecTcNm [mTy,aTy]
    vecBTy = mkTyConApp vecTcNm [mTy,bTy]

indexAddTy
  :: TyConName
  -- ^ Index TyCon name
  -> Type
indexAddTy indexTcNm =
  ForAllTy (bind nTV (
  mkFunTy naturalPrimTy (mkFunTy indexTy (mkFunTy indexTy indexTy))))
  where
    nTV     = TyVar (string2SystemName "n") (embed typeNatKind)
    indexTy = mkTyConApp indexTcNm [VarTy typeNatKind (string2SystemName "n")]



bvAppendTy
  :: TyConName
  -- ^ BitVector TyCon Name
  -> Type
bvAppendTy bvNm =
  ForAllTy (bind mTV (
  ForAllTy (bind nTV (
  mkFunTy naturalPrimTy (mkFunTy
    (mkTyConApp bvNm [VarTy typeNatKind (string2SystemName "n")])
    (mkFunTy
      (mkTyConApp bvNm [VarTy typeNatKind (string2SystemName "m")])
      (mkTyConApp bvNm [mkTyConApp typeNatAdd
                          [VarTy typeNatKind (string2SystemName "n")
                          ,VarTy typeNatKind (string2SystemName "m")]])))))))
  where
    mTV = TyVar (string2SystemName "m") (embed typeNatKind)
    nTV = TyVar (string2SystemName "n") (embed typeNatKind)

bvSplitTy
  :: TyConName
  -- ^ BitVector TyCon Name
  -> Type
bvSplitTy bvNm =
  ForAllTy (bind nTV (
  ForAllTy (bind mTV (
  mkFunTy naturalPrimTy (mkFunTy
    (mkTyConApp bvNm [mkTyConApp typeNatAdd
                                 [VarTy typeNatKind (string2SystemName "m")
                                 ,VarTy typeNatKind (string2SystemName "n")]])
    (mkTyConApp tupNm [mkTyConApp bvNm [VarTy typeNatKind (string2SystemName "m")]
                      ,mkTyConApp bvNm [VarTy typeNatKind (string2SystemName "n")]]))))))
  where
    nTV   = TyVar (string2SystemName "n") (embed typeNatKind)
    mTV   = TyVar (string2SystemName "m") (embed typeNatKind)
    tupNm = ghcTyconToTyConName (tupleTyCon Boxed 2)

typeNatAdd :: TyConName
typeNatAdd = Name User
                  (makeName "GHC.TypeNats.+"
                            (toInteger (getKey typeNatAddTyFamNameKey)))
                  wiredInSrcSpan


typeNatMul :: TyConName
typeNatMul = Name User
                  (makeName "GHC.TypeNats.*"
                            (toInteger (getKey typeNatMulTyFamNameKey)))
                  wiredInSrcSpan

typeNatSub :: TyConName
typeNatSub = Name User
                  (makeName "GHC.TypeNats.-"
                            (toInteger (getKey typeNatSubTyFamNameKey)))
                  wiredInSrcSpan

ghcTyconToTyConName
  :: TyCon.TyCon
  -> TyConName
ghcTyconToTyConName tc =
    Name User
         (makeName n' (toInteger (getKey (TyCon.tyConUnique tc))))
         (getSrcSpan n)
  where
    n'      = fromMaybe "_INTERNAL_" (modNameM n) ++ ('.':occName)
    occName = occNameString $ nameOccName n
    n       = TyCon.tyConName tc

svoid :: (State# RealWorld -> State# RealWorld) -> IO ()
svoid m0 = IO (\s -> case m0 s of s' -> (# s', () #))
