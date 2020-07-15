{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017     , QBayLogic, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.Evaluator.Primitive
  ( ghcPrimStep
  , ghcPrimUnwind
  , isUndefinedPrimVal
  ) where

import           Control.Concurrent.Supply  (Supply,freshId)
import           Control.DeepSeq            (force)
import           Control.Exception          (ArithException(..), Exception, tryJust, evaluate)
import           Control.Monad.State.Strict (State, MonadState)
import qualified Control.Monad.State.Strict as State
import           Control.Monad.Trans.Except (runExcept)
import           Data.Bits
import           Data.Char           (chr,ord)
import qualified Data.Either         as Either
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.List           as List
import qualified Data.Primitive.ByteArray as BA
import           Data.Proxy          (Proxy)
import           Data.Reflection     (reifyNat)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Exts (IsList(..))
import           GHC.Float
import           GHC.Int
import           GHC.Integer
  (decodeDoubleInteger,encodeDoubleInteger,compareInteger,orInteger,andInteger,
   xorInteger,complementInteger,absInteger,signumInteger)
import           GHC.Integer.GMP.Internals
  (Integer (..), BigNat (..))
import           GHC.Natural
import           GHC.Prim
import           GHC.Real            (Ratio (..))
import           GHC.TypeLits        (KnownNat)
import           GHC.Types           (IO (..))
import           GHC.Word
import           System.IO.Unsafe    (unsafeDupablePerformIO)

import           BasicTypes          (Boxity (..))
import           Name                (getSrcSpan, nameOccName, occNameString)
import           PrelNames           (trueDataConKey, falseDataConKey)
import qualified TyCon
import           TysWiredIn          (tupleTyCon)
import           Unique              (getKey)

import           Clash.Class.BitPack (pack,unpack)
import           Clash.Core.DataCon  (DataCon (..))
import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import           Clash.Core.Name
  (Name (..), NameSort (..), mkUnsafeSystemName)
import           Clash.Core.Pretty   (showPpr)
import           Clash.Core.Term
  (Pat (..), PrimInfo (..), Term (..), WorkInfo (..), mkApps)
import           Clash.Core.TermInfo (piResultTys, applyTypeToArgs)
import           Clash.Core.Type
  (Type (..), ConstTy (..), LitTy (..), TypeView (..), mkFunTy, mkTyConApp,
   splitFunForallTy, tyView)
import           Clash.Core.TyCon
  (TyConMap, TyConName, tyConDataCons)
import           Clash.Core.TysPrim
import           Clash.Core.Util
  (mkRTree,mkVec,tyNatSize,dataConInstArgTys,primCo,
   undefinedTm)
import           Clash.Core.Var      (mkLocalId, mkTyVar)
import           Clash.Debug
import           Clash.GHC.GHC2Core  (modNameM)
import           Clash.Rewrite.Util  (mkSelectorCase)
import           Clash.Unique        (lookupUniqMap)
import           Clash.Util
  (MonadUnique (..), clogBase, flogBase, curLoc)
import           Clash.Normalize.PrimitiveReductions
  (typeNatMul, typeNatSub, typeNatAdd, vecLastPrim, vecInitPrim, vecHeadPrim,
   vecTailPrim, mkVecCons, mkVecNil)

import Clash.Promoted.Nat.Unsafe (unsafeSNat)
import qualified Clash.Sized.Internal.BitVector as BitVector
import qualified Clash.Sized.Internal.Signed    as Signed
import qualified Clash.Sized.Internal.Unsigned  as Unsigned
import Clash.Sized.Internal.BitVector(BitVector(..), Bit(..))
import Clash.Sized.Internal.Signed   (Signed   (..))
import Clash.Sized.Internal.Unsigned (Unsigned (..))
import Clash.XException (isX)

import {-# SOURCE #-} Clash.GHC.Evaluator

isUndefinedPrimVal :: Value -> Bool
isUndefinedPrimVal (PrimVal (PrimInfo{primName}) _ _) =
  primName == "Clash.Transformations.undefined"
isUndefinedPrimVal _ = False

-- | Evaluation of primitive operations.
ghcPrimUnwind :: PrimUnwind
ghcPrimUnwind tcm p tys vs v [] m
  | primName p `elem` [ "Clash.Sized.Internal.Index.fromInteger#"
                       , "GHC.CString.unpackCString#"
                       , "Clash.Transformations.removedArg"
                       , "GHC.Prim.MutableByteArray#"
                       , "Clash.Transformations.undefined"
                       ]
              -- The above primitives are actually values, and not operations.
  = ghcUnwind (PrimVal p tys (vs ++ [v])) m tcm
  | primName p == "Clash.Sized.Internal.BitVector.fromInteger#"
  = case (vs,v) of
    ([naturalLiteral -> Just n,mask], integerLiteral -> Just i) ->
      ghcUnwind (PrimVal p tys [Lit (NaturalLiteral n), mask, Lit (IntegerLiteral (wrapUnsigned n i))]) m tcm
    _ -> error ($(curLoc) ++ "Internal error"  ++ show (vs,v))
  | primName p == "Clash.Sized.Internal.BitVector.fromInteger##"
  = case (vs,v) of
    ([mask], integerLiteral -> Just i) ->
      ghcUnwind (PrimVal p tys [mask, Lit (IntegerLiteral (wrapUnsigned 1 i))]) m tcm
    _ -> error ($(curLoc) ++ "Internal error"  ++ show (vs,v))
  | primName p == "Clash.Sized.Internal.Signed.fromInteger#"
  = case (vs,v) of
    ([naturalLiteral -> Just n],integerLiteral -> Just i) ->
      ghcUnwind (PrimVal p tys [Lit (NaturalLiteral n), Lit (IntegerLiteral (wrapSigned n i))]) m tcm
    _ -> error ($(curLoc) ++ "Internal error"  ++ show (vs,v))
  | primName p == "Clash.Sized.Internal.Unsigned.fromInteger#"
  = case (vs,v) of
    ([naturalLiteral -> Just n],integerLiteral -> Just i) ->
      ghcUnwind (PrimVal p tys [Lit (NaturalLiteral n), Lit (IntegerLiteral (wrapUnsigned n i))]) m tcm
    _ -> error ($(curLoc) ++ "Internal error"  ++ show (vs,v))
  | isUndefinedPrimVal v
  = let tyArgs = map Right tys
        tmArgs = map (Left . valToTerm) (vs ++ [v])
    in  Just $ flip setTerm m $ undefinedTm $
          applyTypeToArgs (Prim p) tcm (primType p) (tyArgs ++ tmArgs)
  | otherwise
  = ghcPrimStep tcm (forcePrims m) p tys (vs ++ [v]) m

ghcPrimUnwind tcm p tys vs v [e] m0
  -- Primitives are usually considered undefined when one of their arguments is
  -- (unless they're unused). _Some_ primitives can still yield a result even
  -- though one of their arguments is undefined. It turns out that all primitives
  -- exhibiting this property happen to be "lazy" in their last argument. Thus,
  -- all the cases can be covered by a match on [e] and their names:
  | primName p `elem` [ "Clash.Sized.Vector.lazyV"
                       , "Clash.Sized.Vector.replicate"
                       , "Clash.Sized.Vector.replace_int"
                       , "GHC.Classes.&&"
                       , "GHC.Classes.||"
                       ]
  = if isUndefinedPrimVal v then
      let tyArgs = map Right tys
          tmArgs = map (Left . valToTerm) (vs ++ [v]) ++ [Left e]
      in  Just $ flip setTerm m0 $ undefinedTm $
            applyTypeToArgs (Prim p) tcm (primType p) (tyArgs ++ tmArgs)
    else
      let (m1,i) = newLetBinding tcm m0 e
      in  ghcPrimStep tcm (forcePrims m0) p tys (vs ++ [v,Suspend (Var i)]) m1

ghcPrimUnwind tcm p tys vs (collectValueTicks -> (v, ts)) (e:es) m
  | isUndefinedPrimVal v
  = let tyArgs = map Right tys
        tmArgs = map (Left . valToTerm) (vs ++ [v]) ++ map Left (e:es)
    in  Just $ flip setTerm m $ undefinedTm $
          applyTypeToArgs (Prim p) tcm (primType p) (tyArgs ++ tmArgs)
  | otherwise
  = Just . setTerm e $ stackPush (PrimApply p tys (vs ++ [foldr TickValue v ts]) es) m

newtype PrimEvalMonad a = PEM (State Supply a)
  deriving (Functor, Applicative, Monad, MonadState Supply)

instance MonadUnique PrimEvalMonad where
  getUniqueM = PEM $ State.state (\s -> case freshId s of (!i,!s') -> (i,s'))

runPEM :: PrimEvalMonad a -> Supply -> (a, Supply)
runPEM (PEM m) = State.runState m

ghcPrimStep :: PrimStep
ghcPrimStep tcm isSubj pInfo tys args mach = case primName pInfo of
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
    -> reduce $ catchDivByZero (integerToIntLiteral (i `quot` j))
  "GHC.Prim.remInt#" | Just (i,j) <- intLiterals args
    -> reduce $ catchDivByZero (integerToIntLiteral (i `rem` j))
  "GHC.Prim.quotRemInt#" | Just (i,j) <- intLiterals args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = lookupUniqMap tupTcNm tcm
           [tupDc] = tyConDataCons tupTc
           (q,r)   = quotRem i j
           ret     = mkApps (Data tupDc) (map Right tyArgs ++
                    [Left $ catchDivByZero (integerToIntLiteral q)
                    ,Left $ catchDivByZero (integerToIntLiteral r)])
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
           (Just tupTc) = lookupUniqMap tupTcNm tcm
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
           (Just tupTc) = lookupUniqMap tupTcNm tcm
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
    -> reduce . Literal . WordLiteral . toInteger $ (fromInteger :: Integer -> Word) i -- for overflow behavior

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
           (Just tupTc) = lookupUniqMap tupTcNm tcm
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
           (Just tupTc) = lookupUniqMap tupTcNm tcm
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
           (Just tupTc) = lookupUniqMap tupTcNm tcm
           [tupDc] = tyConDataCons tupTc
           !(W# a)  = fromInteger i
           !(W# b)  = fromInteger j
           !(# h', l #) = timesWord2# a b
       in  reduce $
           mkApps (Data tupDc) (map Right tyArgs ++
                   [ Left (Literal . WordLiteral . toInteger $ W# h')
                   , Left (Literal . WordLiteral . toInteger $ W# l)])

  "GHC.Prim.quotWord#" | Just (i,j) <- wordLiterals args
    -> reduce $ catchDivByZero (integerToWordLiteral (i `quot` j))
  "GHC.Prim.remWord#" | Just (i,j) <- wordLiterals args
    -> reduce $ catchDivByZero (integerToWordLiteral (i `rem` j))
  "GHC.Prim.quotRemWord#" | Just (i,j) <- wordLiterals args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = lookupUniqMap tupTcNm tcm
           [tupDc] = tyConDataCons tupTc
           (q,r)   = quotRem i j
           ret     = mkApps (Data tupDc) (map Right tyArgs ++
                    [Left $ catchDivByZero (integerToWordLiteral q)
                    ,Left $ catchDivByZero (integerToWordLiteral r)])
       in  reduce ret
  "GHC.Prim.quotRemWord2#" | [i,j,k'] <- wordLiterals' args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = lookupUniqMap tupTcNm tcm
           [tupDc] = tyConDataCons tupTc
           !(W# a)  = fromInteger i
           !(W# b)  = fromInteger j
           !(W# c)  = fromInteger k'
           !(# x, y #) = quotRemWord2# a b c
       in  reduce $
           mkApps (Data tupDc) (map Right tyArgs ++
                   [ Left $ catchDivByZero (Literal . WordLiteral . toInteger $ W# x)
                   , Left $ catchDivByZero (Literal . WordLiteral . toInteger $ W# y)])

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
    -> reduce . Literal . IntLiteral . toInteger $ (fromInteger :: Integer -> Int) i -- for overflow behavior

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

#if MIN_VERSION_base(4,14,0)
  "GHC.Prim.bitReverse#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . bitReverse64 . fromInteger $ i -- assume 64bits
  "GHC.Prim.bitReverse8#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . bitReverse8 . fromInteger $ i
  "GHC.Prim.bitReverse16#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . bitReverse16 . fromInteger $ i
  "GHC.Prim.bitReverse32#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . bitReverse32 . fromInteger $ i
  "GHC.Prim.bitReverse64#" | [i] <- wordLiterals' args
    -> reduce . integerToWordLiteral . toInteger . bitReverse64 . fromInteger $ i
#endif
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

#if MIN_VERSION_ghc(8,7,0)
  "GHC.Prim.asinhDouble#"  | Just r <- liftDD asinhDouble# args
    -> reduce r
  "GHC.Prim.acoshDouble#"  | Just r <- liftDD acoshDouble# args
    -> reduce r
  "GHC.Prim.atanhDouble#"  | Just r <- liftDD atanhDouble# args
    -> reduce r
#endif

  "GHC.Prim.**##" | Just r <- liftDDD (**##) args
    -> reduce r
-- decodeDouble_2Int# :: Double# -> (#Int#, Word#, Word#, Int##)
  "GHC.Prim.decodeDouble_2Int#" | [i] <- doubleLiterals' args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = lookupUniqMap tupTcNm tcm
           [tupDc] = tyConDataCons tupTc
           !(D# a) = fromRational i
           !(# p, q, r, s #) = decodeDouble_2Int# a
       in reduce $
          mkApps (Data tupDc) (map Right tyArgs ++
                   [ Left (Literal . IntLiteral  . toInteger $ I# p)
                   , Left (Literal . WordLiteral . toInteger $ W# q)
                   , Left (Literal . WordLiteral . toInteger $ W# r)
                   , Left (Literal . IntLiteral  . toInteger $ I# s)])
-- decodeDouble_Int64# :: Double# -> (# Int64#, Int# #)
  "GHC.Prim.decodeDouble_Int64#" | [i] <- doubleLiterals' args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = lookupUniqMap tupTcNm tcm
           [tupDc] = tyConDataCons tupTc
           !(D# a) = fromRational i
           !(# p, q #) = decodeDouble_Int64# a
       in reduce $
          mkApps (Data tupDc) (map Right tyArgs ++
                   [ Left (Literal . IntLiteral  . toInteger $ I64# p)
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

#if MIN_VERSION_ghc(8,7,0)
  "GHC.Prim.asinhFloat#"  | Just r <- liftFF asinhFloat# args
    -> reduce r
  "GHC.Prim.acoshFloat#"  | Just r <- liftFF acoshFloat# args
    -> reduce r
  "GHC.Prim.atanhFloat#"  | Just r <- liftFF atanhFloat# args
    -> reduce r
#endif

  "GHC.Prim.float2Double#" | [i] <- floatLiterals' args
    -> let !(F# a) = fromRational i
           r = float2Double# a
       in  reduce . Literal . DoubleLiteral . toRational $ D# r


  "GHC.Prim.newByteArray#"
    | [iV,PrimVal rwTy _ _] <- args
    , [i] <- intLiterals' [iV]
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = lookupUniqMap tupTcNm tcm
           [tupDc] = tyConDataCons tupTc
           p = primCount mach
           lit = Literal (ByteArrayLiteral (fromList (List.genericReplicate i 0)))
           mbaTy = mkFunTy intPrimTy (last tyArgs)
           newE = mkApps (Data tupDc) (map Right tyArgs ++
                    [Left (Prim rwTy)
                    ,Left (mkApps (Prim (PrimInfo "GHC.Prim.MutableByteArray#" mbaTy WorkNever Nothing))
                                  [Left (Literal . IntLiteral $ toInteger p)])
                    ])
       in Just . setTerm newE $ primInsert p lit mach

  "GHC.Prim.setByteArray#"
    | [PrimVal _mbaTy _ [baV]
      ,offV,lenV,cV
      ,PrimVal rwTy _ _
      ] <- args
    , [ba,off,len,c] <- intLiterals' [baV,offV,lenV,cV]
    -> let Just (Literal (ByteArrayLiteral ba1)) =
              primLookup (fromInteger ba) mach
           !(I# off') = fromInteger off
           !(I# len') = fromInteger len
           !(I# c')   = fromInteger c
           ba2 = unsafeDupablePerformIO $ do
                  BA.MutableByteArray mba <- BA.unsafeThawByteArray ba1
                  svoid (setByteArray# mba off' len' c')
                  BA.unsafeFreezeByteArray (BA.MutableByteArray mba)
           ba3 = Literal (ByteArrayLiteral ba2)
       in Just . setTerm (Prim rwTy) $ primUpdate (fromInteger ba) ba3 mach

  "GHC.Prim.writeWordArray#"
    | [PrimVal _mbaTy _  [baV]
      ,iV,wV
      ,PrimVal rwTy _ _
      ] <- args
    , [ba,i] <- intLiterals' [baV,iV]
    , [w] <- wordLiterals' [wV]
    -> let Just (Literal (ByteArrayLiteral ba1)) =
              primLookup (fromInteger ba) mach
           !(I# i') = fromInteger i
           !(W# w') = fromIntegral w
           ba2 = unsafeDupablePerformIO $ do
                  BA.MutableByteArray mba <- BA.unsafeThawByteArray ba1
                  svoid (writeWordArray# mba i' w')
                  BA.unsafeFreezeByteArray (BA.MutableByteArray mba)
           ba3 = Literal (ByteArrayLiteral ba2)
       in Just . setTerm (Prim rwTy) $ primUpdate (fromInteger ba) ba3 mach

  "GHC.Prim.unsafeFreezeByteArray#"
    | [PrimVal _mbaTy _ [baV]
      ,PrimVal rwTy _ _
      ] <- args
    , [ba] <-  intLiterals' [baV]
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = lookupUniqMap tupTcNm tcm
           [tupDc] = tyConDataCons tupTc
           Just ba' = primLookup (fromInteger ba) mach
       in  reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                      [Left (Prim rwTy)
                      ,Left ba'])

  "GHC.Prim.sizeofByteArray#"
    | [Lit (ByteArrayLiteral ba)] <- args
    -> reduce (Literal (IntLiteral (toInteger (BA.sizeofByteArray ba))))

  "GHC.Prim.indexWordArray#"
    | [Lit (ByteArrayLiteral (BA.ByteArray ba)),iV] <- args
    , [i] <- intLiterals' [iV]
    -> let !(I# i') = fromInteger i
           !w       = indexWordArray# ba i'
       in  reduce (Literal (WordLiteral (toInteger (W# w))))

  "GHC.Prim.getSizeofMutBigNat#"
    | [PrimVal _mbaTy _ [baV]
      ,PrimVal rwTy _ _
      ] <- args
    , [ba] <- intLiterals' [baV]
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = lookupUniqMap tupTcNm tcm
           [tupDc] = tyConDataCons tupTc
           Just (Literal (ByteArrayLiteral ba')) = primLookup (fromInteger ba) mach
           lit = Literal (IntLiteral (toInteger (BA.sizeofByteArray ba')))
       in  reduce $ mkApps (Data tupDc) (map Right tyArgs ++
                      [Left (Prim rwTy)
                      ,Left lit])

  "GHC.Prim.resizeMutableByteArray#"
    | [PrimVal mbaTy _ [baV]
      ,iV
      ,PrimVal rwTy _ _
      ] <- args
    , [ba,i] <- intLiterals' [baV,iV]
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = lookupUniqMap tupTcNm tcm
           [tupDc] = tyConDataCons tupTc
           p = primCount mach
           Just (Literal (ByteArrayLiteral ba1))
            = primLookup (fromInteger ba) mach
           !(I# i') = fromInteger i
           ba2 = unsafeDupablePerformIO $ do
                   BA.MutableByteArray mba <- BA.unsafeThawByteArray ba1
                   mba' <- IO (\s -> case resizeMutableByteArray# mba i' s of
                                 (# s', mba' #) -> (# s', BA.MutableByteArray mba' #))
                   BA.unsafeFreezeByteArray mba'
           ba3 = Literal (ByteArrayLiteral ba2)
           newE = mkApps (Data tupDc) (map Right tyArgs ++
                    [Left (Prim rwTy)
                    ,Left (mkApps (Prim mbaTy)
                                  [Left (Literal . IntLiteral $ toInteger p)])
                    ])
       in Just . setTerm newE $ primInsert p ba3 mach

  "GHC.Prim.shrinkMutableByteArray#"
    | [PrimVal _mbaTy _ [baV]
      ,lenV
      ,PrimVal rwTy _ _
      ] <- args
    , [ba,len] <- intLiterals' [baV,lenV]
    -> let Just (Literal (ByteArrayLiteral ba1)) =
              primLookup (fromInteger ba) mach
           !(I# len') = fromInteger len
           ba2 = unsafeDupablePerformIO $ do
                  BA.MutableByteArray mba <- BA.unsafeThawByteArray ba1
                  svoid (shrinkMutableByteArray# mba len')
                  BA.unsafeFreezeByteArray (BA.MutableByteArray mba)
           ba3 = Literal (ByteArrayLiteral ba2)
       in Just . setTerm (Prim rwTy) $ primUpdate (fromInteger ba) ba3 mach

  "GHC.Prim.copyByteArray#"
    | [Lit (ByteArrayLiteral (BA.ByteArray src_ba))
      ,src_offV
      ,PrimVal _mbaTy _ [dst_mbaV]
      ,dst_offV, nV
      ,PrimVal rwTy _ _
      ] <- args
    , [src_off,dst_mba,dst_off,n] <- intLiterals' [src_offV,dst_mbaV,dst_offV,nV]
    -> let Just (Literal (ByteArrayLiteral dst_ba)) =
              primLookup (fromInteger dst_mba) mach
           !(I# src_off') = fromInteger src_off
           !(I# dst_off') = fromInteger dst_off
           !(I# n')       = fromInteger n
           ba2 = unsafeDupablePerformIO $ do
                  BA.MutableByteArray dst_mba1 <- BA.unsafeThawByteArray dst_ba
                  svoid (copyByteArray# src_ba src_off' dst_mba1 dst_off' n')
                  BA.unsafeFreezeByteArray (BA.MutableByteArray dst_mba1)
           ba3 = Literal (ByteArrayLiteral ba2)
       in Just . setTerm (Prim rwTy) $ primUpdate (fromInteger dst_mba) ba3 mach

  "GHC.Prim.readWordArray#"
    | [PrimVal _mbaTy _  [baV]
      ,offV
      ,PrimVal rwTy _ _
      ] <- args
    , [ba,off] <- intLiterals' [baV,offV]
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = lookupUniqMap tupTcNm tcm
           [tupDc] = tyConDataCons tupTc
           Just (Literal (ByteArrayLiteral ba1)) =
              primLookup (fromInteger ba) mach
           !(I# off') = fromInteger off
           w = unsafeDupablePerformIO $ do
                  BA.MutableByteArray mba <- BA.unsafeThawByteArray ba1
                  IO (\s -> case readWordArray# mba off' s of
                        (# s', w' #) -> (# s',  W# w' #))
           newE = mkApps (Data tupDc) (map Right tyArgs ++
                    [Left (Prim rwTy)
                    ,Left (Literal (WordLiteral (toInteger w)))
                    ])
       in reduce newE

-- decodeFloat_Int# :: Float# -> (#Int#, Int##)
  "GHC.Prim.decodeFloat_Int#" | [i] <- floatLiterals' args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = lookupUniqMap tupTcNm tcm
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
    -> let dc = do { tc <- lookupUniqMap tcN tcm
                   ; let dcs = tyConDataCons tc
                   ; List.find ((== (i+1)) . toInteger . dcTag) dcs
                   }
       in (\e -> setTerm (Data e) mach) <$> dc


  "GHC.Classes.geInt" | Just (i,j) <- intCLiterals args
    -> reduce (boolToBoolLiteral tcm ty (i >= j))

  "GHC.Classes.&&"
    | [ lArg , rArg ] <- args
    , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
    -- evaluation of the arguments is deferred until the evaluation of the ghcPrimUnwindWith
    -- to make `&&` lazy in both arguments
    , mach1@Machine{mStack=[],mTerm=lArgWHNF} <- whnf eval tcm True (setTerm (valToTerm lArg) $ stackClear mach)
    , mach2@Machine{mStack=[],mTerm=rArgWHNF} <- whnf eval tcm True (setTerm (valToTerm rArg) $ stackClear mach1)
    -> case [ lArgWHNF, rArgWHNF ] of
         [ Data lCon, Data rCon ] ->
           Just $ mach2
             { mStack = mStack mach
             , mTerm = boolToBoolLiteral tcm ty (isTrueDC lCon && isTrueDC rCon)
             }

         [ Data lCon, _ ]
           | isTrueDC lCon -> reduce rArgWHNF
           | otherwise     -> reduce (boolToBoolLiteral tcm ty False)

         [ _, Data rCon ]
           | isTrueDC rCon -> reduce lArgWHNF
           | otherwise     -> reduce (boolToBoolLiteral tcm ty False)

         _ -> Nothing

  "GHC.Classes.||"
    | [ lArg , rArg ] <- args
    , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
    -- evaluation of the arguments is deferred until the evaluation of the ghcPrimUnwindWith
    -- to make `||` lazy in both arguments
    , mach1@Machine{mStack=[],mTerm=lArgWHNF} <- whnf eval tcm True (setTerm (valToTerm lArg) $ stackClear mach)
    , mach2@Machine{mStack=[],mTerm=rArgWHNF} <- whnf eval tcm True (setTerm (valToTerm rArg) $ stackClear mach1)
    -> case [ lArgWHNF, rArgWHNF ] of
         [ Data lCon, Data rCon ] ->
           Just $ mach2
             { mStack = mStack mach
             , mTerm = boolToBoolLiteral tcm ty (isTrueDC lCon || isTrueDC rCon)
             }

         [ Data lCon, _ ]
           | isFalseDC lCon -> reduce rArgWHNF
           | otherwise      -> reduce (boolToBoolLiteral tcm ty True)

         [ _, Data rCon ]
           | isFalseDC rCon -> reduce lArgWHNF
           | otherwise      -> reduce (boolToBoolLiteral tcm ty True)

         _ -> Nothing

  "GHC.Classes.divInt#" | Just (i,j) <- intLiterals args
    -> reduce (integerToIntLiteral (i `div` j))

  -- modInt# :: Int# -> Int# -> Int#
  "GHC.Classes.modInt#"
    | [dividend, divisor] <- intLiterals' args
    ->
      if divisor == 0 then
        let iTy = snd (splitFunForallTy ty) in
        reduce (undefinedTm iTy)
      else
        reduce (Literal (IntLiteral (dividend `mod` divisor)))

  "GHC.Classes.not"
    | [DC bCon _] <- args
    -> reduce (boolToBoolLiteral tcm ty (nameOcc (dcName bCon) == "GHC.Types.False"))

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
           (Just tupTc) = lookupUniqMap tupTcNm tcm
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
           (Just tupTc) = lookupUniqMap tupTcNm tcm
           [tupDc] = tyConDataCons tupTc
           (q,r) = quotRem i j
    in reduce $
         mkApps (Data tupDc) (map Right tyArgs ++
                [ Left $ catchDivByZero (integerToIntegerLiteral q)
                , Left $ catchDivByZero (integerToIntegerLiteral r)])

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
    -> reduce $ catchDivByZero (integerToIntegerLiteral (i `div` j))

  "GHC.Integer.Type.modInteger" | Just (i,j) <- integerLiterals args
    -> reduce $ catchDivByZero (integerToIntegerLiteral (i `mod` j))

  "GHC.Integer.Type.quotInteger" | Just (i,j) <- integerLiterals args
    -> reduce $ catchDivByZero (integerToIntegerLiteral (i `quot` j))

  "GHC.Integer.Type.remInteger" | Just (i,j) <- integerLiterals args
    -> reduce $ catchDivByZero (integerToIntegerLiteral (i `rem` j))

  "GHC.Integer.Type.divModInteger" | Just (i,j) <- integerLiterals args
    -> let (_,tyView -> TyConApp ubTupTcNm [liftedKi,_,intTy,_]) = splitFunForallTy ty
           (Just ubTupTc) = lookupUniqMap ubTupTcNm tcm
           [ubTupDc] = tyConDataCons ubTupTc
           (d,m) = divMod i j
       in  reduce $
           mkApps (Data ubTupDc) [ Right liftedKi, Right liftedKi
                                 , Right intTy,    Right intTy
                                 , Left $ catchDivByZero (Literal (IntegerLiteral d))
                                 , Left $ catchDivByZero (Literal (IntegerLiteral m))
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

  "GHC.Integer.Type.compareInteger" -- :: Integer -> Integer -> Ordering
    | [i, j] <- integerLiterals' args
    -> let -- Get the required result type (viewed as an applied type constructor name)
           (_,tyView -> TyConApp tupTcNm []) = splitFunForallTy ty
           -- Find the type constructor from the name
           (Just tupTc) = lookupUniqMap tupTcNm tcm
           -- Get the data constructors of that type
           -- The type is 'Ordering', so they are: 'LT', 'EQ', 'GT'
           [ltDc, eqDc, gtDc] = tyConDataCons tupTc
           -- Do the actual compile-time evaluation
           ordVal = compareInteger i j
    in reduce $ case ordVal of
        LT -> Data ltDc
        EQ -> Data eqDc
        GT -> Data gtDc

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

  "GHC.Integer.Type.integerToWord"
    | [i] <- integerLiterals' args
    -> reduce (integerToWordLiteral i)

  "GHC.Integer.Type.testBitInteger" -- :: Integer -> Int# -> Bool
    | [Lit (IntegerLiteral i), Lit (IntLiteral j)] <- args
    -> reduce (boolToBoolLiteral tcm ty (testBit i (fromInteger j)))

  "GHC.Natural.NatS#"
    | [Lit (WordLiteral w)] <- args
    -> reduce (Literal (NaturalLiteral w))

  "GHC.Natural.naturalToInteger"
    | [i] <- naturalLiterals' args
    -> reduce (Literal (IntegerLiteral (toInteger i)))

  "GHC.Natural.naturalFromInteger"
    | [i] <- integerLiterals' args
    ->
     let nTy = snd (splitFunForallTy ty) in
     reduce (checkNaturalRange1 nTy i id)

  -- GHC.shiftLNatural --- XXX: Fragile worker of GHC.shiflLNatural
  "GHC.Natural.$wshiftLNatural"
    | [nV,iV] <- args
    , [n] <- naturalLiterals' [nV]
    , [i] <- fromInteger <$> intLiterals' [iV]
    ->
     let nTy = snd (splitFunForallTy ty) in
     reduce (checkNaturalRange1 nTy n ((flip shiftL) i))

  "GHC.Natural.plusNatural"
    | Just (i,j) <- naturalLiterals args
    ->
     let nTy = snd (splitFunForallTy ty) in
     reduce (checkNaturalRange2 nTy i j (+))

  "GHC.Natural.timesNatural"
    | Just (i,j) <- naturalLiterals args
    ->
     let nTy = snd (splitFunForallTy ty) in
     reduce (checkNaturalRange2 nTy i j (*))

  "GHC.Natural.minusNatural"
    | Just (i,j) <- naturalLiterals args
    ->
     let nTy = snd (splitFunForallTy ty) in
     reduce (checkNaturalRange nTy [i, j] (\[i', j'] ->
                case minusNaturalMaybe i' j' of
                  Nothing -> checkNaturalRange1 nTy (-1) id
                  Just n -> naturalToNaturalLiteral n))

  "GHC.Natural.wordToNatural#"
    | [Lit (WordLiteral w)] <- args
    ->
     let nTy = snd (splitFunForallTy ty) in
     reduce (checkNaturalRange1 nTy w id)

  "GHC.Natural.gcdNatural"
    | Just (i,j) <- naturalLiterals args
    ->
     let nTy = snd (splitFunForallTy ty) in
     reduce (checkNaturalRange2 nTy i j gcd)

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

  -- Type level ^    -- XXX: Very fragile
  -- These is are specialized versions of ^_f, named by some combination of ghc and singletons.
  "Data.Singletons.TypeLits.Internal.$s^_f"            -- ghc-8.4.4, singletons-2.4.1
    | [i,j] <- naturalLiterals' args
    -> reduce (Literal (NaturalLiteral (i ^ j)))
  "Data.Singletons.TypeLits.Internal.$fSingI->^@#@$_f" -- ghc-8.6.5, singletons-2.5.1
    | [i,j] <- naturalLiterals' args
    -> reduce (Literal (NaturalLiteral (i ^ j)))
  "Data.Singletons.TypeLits.Internal.%^_f"             -- ghc-8.8.1, singletons-2.6
    | [i,j] <- naturalLiterals' args
    -> reduce (Literal (NaturalLiteral (i ^ j)))

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
            (Just charTc) = lookupUniqMap charTcNm tcm
            [charDc] = tyConDataCons charTc
        in  reduce (mkApps (Data charDc) [Left (Literal (CharLiteral c))])

  "GHC.Types.I#"
    | isSubj
    , [Lit (IntLiteral i)] <- args
    ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
            (Just intTc) = lookupUniqMap intTcNm tcm
            [intDc] = tyConDataCons intTc
        in  reduce (mkApps (Data intDc) [Left (Literal (IntLiteral i))])
  "GHC.Int.I8#"
    | isSubj
    , [Lit (IntLiteral i)] <- args
    ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
            (Just intTc) = lookupUniqMap intTcNm tcm
            [intDc] = tyConDataCons intTc
        in  reduce (mkApps (Data intDc) [Left (Literal (IntLiteral i))])
  "GHC.Int.I16#"
    | isSubj
    , [Lit (IntLiteral i)] <- args
    ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
            (Just intTc) = lookupUniqMap intTcNm tcm
            [intDc] = tyConDataCons intTc
        in  reduce (mkApps (Data intDc) [Left (Literal (IntLiteral i))])
  "GHC.Int.I32#"
    | isSubj
    , [Lit (IntLiteral i)] <- args
    ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
            (Just intTc) = lookupUniqMap intTcNm tcm
            [intDc] = tyConDataCons intTc
        in  reduce (mkApps (Data intDc) [Left (Literal (IntLiteral i))])
  "GHC.Int.I64#"
    | isSubj
    , [Lit (IntLiteral i)] <- args
    ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
            (Just intTc) = lookupUniqMap intTcNm tcm
            [intDc] = tyConDataCons intTc
        in  reduce (mkApps (Data intDc) [Left (Literal (IntLiteral i))])

  "GHC.Types.W#"
    | isSubj
    , [Lit (WordLiteral c)] <- args
    ->  let (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
            (Just wordTc) = lookupUniqMap wordTcNm tcm
            [wordDc] = tyConDataCons wordTc
        in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral c))])
  "GHC.Word.W8#"
    | isSubj
    , [Lit (WordLiteral c)] <- args
    ->  let (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
            (Just wordTc) = lookupUniqMap wordTcNm tcm
            [wordDc] = tyConDataCons wordTc
        in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral c))])
  "GHC.Word.W16#"
    | isSubj
    , [Lit (WordLiteral c)] <- args
    ->  let (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
            (Just wordTc) = lookupUniqMap wordTcNm tcm
            [wordDc] = tyConDataCons wordTc
        in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral c))])
  "GHC.Word.W32#"
    | isSubj
    , [Lit (WordLiteral c)] <- args
    ->  let (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
            (Just wordTc) = lookupUniqMap wordTcNm tcm
            [wordDc] = tyConDataCons wordTc
        in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral c))])
  "GHC.Word.W64#"
    | [Lit (WordLiteral c)] <- args
    ->  let (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
            (Just wordTc) = lookupUniqMap wordTcNm tcm
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

  "GHC.Integer.Type.$wsignumInteger" -- XXX: Not super-fragile, but still..
    | [i] <- integerLiterals' args
    -> reduce (Literal (IntLiteral (signum i)))


  "GHC.Integer.Type.signumInteger"
    | [i] <- integerLiterals' args
    -> reduce (Literal (IntegerLiteral (signumInteger i)))

  "GHC.Integer.Type.absInteger"
    | [i] <- integerLiterals' args
    -> reduce (Literal (IntegerLiteral (absInteger i)))

  "GHC.Integer.Type.bitInteger"
    | [i] <- intLiterals' args
    -> reduce (Literal (IntegerLiteral (bit (fromInteger i))))

  "GHC.Integer.Type.complementInteger"
    | [i] <- integerLiterals' args
    -> reduce (Literal (IntegerLiteral (complementInteger i)))

  "GHC.Integer.Type.orInteger"
    | [i, j] <- integerLiterals' args
    -> reduce (Literal (IntegerLiteral (orInteger i j)))

  "GHC.Integer.Type.xorInteger"
    | [i, j] <- integerLiterals' args
    -> reduce (Literal (IntegerLiteral (xorInteger i j)))

  "GHC.Integer.Type.andInteger"
    | [i, j] <- integerLiterals' args
    -> reduce (Literal (IntegerLiteral (andInteger i j)))

  "GHC.Integer.Type.doubleFromInteger"
    | [i] <- integerLiterals' args
    -> reduce (Literal (DoubleLiteral (toRational (fromInteger i :: Double))))

  "GHC.Base.eqString"
    | [PrimVal _ _ [Lit (StringLiteral s1)]
      ,PrimVal _ _ [Lit (StringLiteral s2)]
      ] <- args
    -> reduce (boolToBoolLiteral tcm ty (s1 == s2))
    | otherwise -> error (show args)


  "Clash.Class.BitPack.packDouble#" -- :: Double -> BitVector 64
    | [DC _ [Left arg]] <- args
    , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
    , mach2@Machine{mStack=[],mTerm=Literal (DoubleLiteral i)} <- whnf eval tcm True (setTerm arg $ stackClear mach)
    -> let resTyInfo = extractTySizeInfo tcm ty tys
        in Just $ mach2
             { mStack = mStack mach
             , mTerm = mkBitVectorLit' resTyInfo 0 (toInteger $ (pack :: Double -> BitVector 64) $ fromRational i)
             }

  "Clash.Class.BitPack.packFloat#" -- :: Float -> BitVector 32
    | [DC _ [Left arg]] <- args
    , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
    , mach2@Machine{mStack=[],mTerm=Literal (FloatLiteral i)} <- whnf eval tcm True (setTerm arg $ stackClear mach)
    -> let resTyInfo = extractTySizeInfo tcm ty tys
        in Just $ mach2
             { mStack = mStack mach
             , mTerm = mkBitVectorLit' resTyInfo 0 (toInteger $ (pack :: Float -> BitVector 32) $ fromRational i)
             }

  "Clash.Class.BitPack.unpackFloat#"
    | [i] <- bitVectorLiterals' args
    -> reduce (Literal (FloatLiteral (toRational $ (unpack :: BitVector 32 -> Float) (toBV i))))

  "Clash.Class.BitPack.unpackDouble#"
    | [i] <- bitVectorLiterals' args
    -> reduce (Literal (DoubleLiteral (toRational $ (unpack :: BitVector 64 -> Double) (toBV i))))

  -- expIndex#
  --   :: KnownNat m
  --   => Index m
  --   -> SNat n
  --   -> Index (n^m)
  "Clash.Class.Exp.expIndex#"
    | [b] <- indexLiterals' args
    , [(_mTy, km), (_, e)] <- extractKnownNats tcm tys
    -> reduce (mkIndexLit ty (LitTy (NumTy (km^e))) (km^e) (b^e))

  -- expSigned#
  --   :: KnownNat m
  --   => Signed m
  --   -> SNat n
  --   -> Signed (n*m)
  "Clash.Class.Exp.expSigned#"
    | [b] <- signedLiterals' args
    , [(_mTy, km), (_, e)] <- extractKnownNats tcm tys
    -> reduce (mkSignedLit ty (LitTy (NumTy (km*e))) (km*e) (b^e))

  -- expUnsigned#
  --   :: KnownNat m
  --   => Unsigned m
  --   -> SNat n
  --   -> Unsigned m
  "Clash.Class.Exp.expUnsigned#"
    | [b] <- unsignedLiterals' args
    , [(_mTy, km), (_, e)] <- extractKnownNats tcm tys
    -> reduce (mkUnsignedLit ty (LitTy (NumTy (km*e))) (km*e) (b^e))

  "Clash.Promoted.Nat.powSNat"
    | [Right a, Right b] <- map (runExcept . tyNatSize tcm) tys
    -> let c = case a of
                 2 -> 1 `shiftL` (fromInteger b)
                 _ -> a ^ b
           (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
           (Just snatTc) = lookupUniqMap snatTcNm tcm
           [snatDc] = tyConDataCons snatTc
       in  reduce $
           mkApps (Data snatDc) [ Right (LitTy (NumTy c))
                                , Left (Literal (NaturalLiteral c))]

  "Clash.Promoted.Nat.flogBaseSNat"
    | [Right a, Right b] <- map (runExcept . tyNatSize tcm) tys
    , Just c <- flogBase a b
    , let c' = toInteger c
    -> let (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
           (Just snatTc) = lookupUniqMap snatTcNm tcm
           [snatDc] = tyConDataCons snatTc
       in  reduce $
           mkApps (Data snatDc) [ Right (LitTy (NumTy c'))
                                , Left (Literal (NaturalLiteral c'))]

  "Clash.Promoted.Nat.clogBaseSNat"
    | [Right a, Right b] <- map (runExcept . tyNatSize tcm) tys
    , Just c <- clogBase a b
    , let c' = toInteger c
    -> let (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
           (Just snatTc) = lookupUniqMap snatTcNm tcm
           [snatDc] = tyConDataCons snatTc
       in  reduce $
           mkApps (Data snatDc) [ Right (LitTy (NumTy c'))
                                , Left (Literal (NaturalLiteral c'))]
    | otherwise
    -> error ("clogBaseSNat: args = " <> show args <> ", tys = " <> show tys)

  "Clash.Promoted.Nat.logBaseSNat"
    | [Right a, Right b] <- map (runExcept . tyNatSize tcm) tys
    , Just c <- flogBase a b
    , let c' = toInteger c
    -> let (_,tyView -> TyConApp snatTcNm _) = splitFunForallTy ty
           (Just snatTc) = lookupUniqMap snatTcNm tcm
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
           (Just intTc) = lookupUniqMap intTcNm tcm
           [intCon] = tyConDataCons intTc
       in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral kn))])
  "Clash.Sized.Internal.BitVector.maxIndex#"
    | Just (_, kn) <- extractKnownNat tcm tys
    -> let (_,tyView -> TyConApp intTcNm _) = splitFunForallTy ty
           (Just intTc) = lookupUniqMap intTcNm tcm
           [intCon] = tyConDataCons intTc
       in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral (kn-1)))])

-- Construction
  "Clash.Sized.Internal.BitVector.high"
    -> reduce (mkBitLit ty 0 1)
  "Clash.Sized.Internal.BitVector.low"
    -> reduce (mkBitLit ty 0 0)

  "Clash.Sized.Internal.BitVector.undefined#"
    | Just (_, kn) <- extractKnownNat tcm tys
    -> let resTyInfo = extractTySizeInfo tcm ty tys
           mask = bit (fromInteger kn) - 1
       in reduce (mkBitVectorLit' resTyInfo mask 0)

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
    | [i,j] <- bitLiterals args
    -> let Bit msk val = BitVector.and## (toBit i) (toBit j)
       in reduce (mkBitLit ty (toInteger msk) (toInteger val))
  "Clash.Sized.Internal.BitVector.or##"
    | [i,j] <- bitLiterals args
    -> let Bit msk val = BitVector.or## (toBit i) (toBit j)
       in reduce (mkBitLit ty (toInteger msk) (toInteger val))
  "Clash.Sized.Internal.BitVector.xor##"
    | [i,j] <- bitLiterals args
    -> let Bit msk val = BitVector.xor## (toBit i) (toBit j)
       in reduce (mkBitLit ty (toInteger msk) (toInteger val))

  "Clash.Sized.Internal.BitVector.complement##"
    | [i] <- bitLiterals args
    -> let Bit msk val = BitVector.complement## (toBit i)
       in reduce (mkBitLit ty (toInteger msk) (toInteger val))

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
        op u i _ = (toInteger m, toInteger v)
          where Bit m v = (BitVector.index# u i)
  "Clash.Sized.Internal.BitVector.replaceBit#" -- :: :: KnownNat n => BitVector n -> Int -> Bit -> BitVector n
    | Just (_, n) <- extractKnownNat tcm tys
    , [ _
      , PrimVal bvP _ [_, Lit (NaturalLiteral mskBv), Lit (IntegerLiteral bv)]
      , valArgs -> Just [Literal (IntLiteral i)]
      , PrimVal bP _ [Lit (WordLiteral mskB), Lit (IntegerLiteral b)]
      ] <- args
    , primName bvP == "Clash.Sized.Internal.BitVector.fromInteger#"
    , primName bP  == "Clash.Sized.Internal.BitVector.fromInteger##"
      -> let resTyInfo = extractTySizeInfo tcm ty tys
             (mskVal,val) = reifyNat n (op (BV (fromInteger mskBv) (fromInteger bv))
                                           (fromInteger i)
                                           (Bit (fromInteger mskB) (fromInteger b)))
      in reduce (mkBitVectorLit' resTyInfo mskVal val)
      where
        op :: KnownNat n => BitVector n -> Int -> Bit -> Proxy n -> (Integer,Integer)
        -- op bv i b _ = (BitVector.unsafeMask res, BitVector.unsafeToInteger res)
        op bv i b _ = splitBV (BitVector.replaceBit# bv i b)
  "Clash.Sized.Internal.BitVector.setSlice#"
  -- :: SNat (m+1+i) -> BitVector (m + 1 + i) -> SNat m -> SNat n -> BitVector (m + 1 - n) -> BitVector (m + 1 + i)
    | mTy : iTy : nTy : _ <- tys
    , Right m <- runExcept (tyNatSize tcm mTy)
    , Right iN <- runExcept (tyNatSize tcm iTy)
    , Right n <- runExcept (tyNatSize tcm nTy)
    , [i,j] <- bitVectorLiterals' args
    -> let BV msk val = BitVector.setSlice# (unsafeSNat (m+1+iN)) (toBV i) (unsafeSNat m) (unsafeSNat n) (toBV j)
           resTyInfo = extractTySizeInfo tcm ty tys
       in  reduce (mkBitVectorLit' resTyInfo (toInteger msk) (toInteger val))
  "Clash.Sized.Internal.BitVector.slice#"
  -- :: BitVector (m + 1 + i) -> SNat m -> SNat n -> BitVector (m + 1 - n)
    | mTy : _ : nTy : _ <- tys
    , Right m <- runExcept (tyNatSize tcm mTy)
    , Right n <- runExcept (tyNatSize tcm nTy)
    , [i] <- bitVectorLiterals' args
    -> let BV msk val = BitVector.slice# (toBV i) (unsafeSNat m) (unsafeSNat n)
           resTyInfo = extractTySizeInfo tcm ty tys
       in  reduce (mkBitVectorLit' resTyInfo (toInteger msk) (toInteger val))
  "Clash.Sized.Internal.BitVector.split#" -- :: forall n m. KnownNat n => BitVector (m + n) -> (BitVector m, BitVector n)
    | nTy : mTy : _ <- tys
    , Right n <-  runExcept (tyNatSize tcm nTy)
    , Right m <-  runExcept (tyNatSize tcm mTy)
    , [(mski,i)] <- bitVectorLiterals' args
    -> let ty' = piResultTys tcm ty tys
           (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty'
           (Just tupTc) = lookupUniqMap tupTcNm tcm
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
       in reduce (mkBitLit resTy (toInteger msk) (toInteger val))
    where
      op :: KnownNat n => BitVector n -> Proxy n -> (Word,Word)
      op u _ = (unsafeMask# res, BitVector.unsafeToInteger# res)
        where
          res = BitVector.msb# u
  "Clash.Sized.Internal.BitVector.lsb#" -- :: BitVector n -> Bit
    | [i] <- bitVectorLiterals' args
    -> let resTy = getResultTy tcm ty tys
           Bit msk val = BitVector.lsb# (toBV i)
    in reduce (mkBitLit resTy (toInteger msk) (toInteger val))


-- Eq
  -- eq#, neq# :: KnownNat n => BitVector n -> BitVector n -> Bool
  "Clash.Sized.Internal.BitVector.eq#"
    | nTy : _ <- tys
    , Right 0 <- runExcept (tyNatSize tcm nTy)
    -> reduce (boolToBoolLiteral tcm ty True)
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2Bool BitVector.eq# ty tcm args)
    -> reduce val

  "Clash.Sized.Internal.BitVector.neq#"
    | nTy : _ <- tys
    , Right 0 <- runExcept (tyNatSize tcm nTy)
    -> reduce (boolToBoolLiteral tcm ty False)
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2Bool BitVector.neq# ty tcm args)
    -> reduce val

-- Ord
  -- lt#,ge#,gt#,le# :: KnownNat n => BitVector n -> BitVector n -> Bool
  "Clash.Sized.Internal.BitVector.lt#"
    | nTy : _ <- tys
    , Right 0 <- runExcept (tyNatSize tcm nTy)
    -> reduce (boolToBoolLiteral tcm ty False)
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2Bool BitVector.lt# ty tcm args)
    -> reduce val
  "Clash.Sized.Internal.BitVector.ge#"
    | nTy : _ <- tys
    , Right 0 <- runExcept (tyNatSize tcm nTy)
    -> reduce (boolToBoolLiteral tcm ty True)
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2Bool BitVector.ge# ty tcm args)
    -> reduce val
  "Clash.Sized.Internal.BitVector.gt#"
    | nTy : _ <- tys
    , Right 0 <- runExcept (tyNatSize tcm nTy)
    -> reduce (boolToBoolLiteral tcm ty False)
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2Bool BitVector.gt# ty tcm args)
    -> reduce val
  "Clash.Sized.Internal.BitVector.le#"
    | nTy : _ <- tys
    , Right 0 <- runExcept (tyNatSize tcm nTy)
    -> reduce (boolToBoolLiteral tcm ty True)
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
    -> let ty' = piResultTys tcm ty tys
           (_,resTy) = splitFunForallTy ty'
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
       in  reduce (mkBitVectorLit resTy resSizeTy resSizeInt 0 (i+j))

  "Clash.Sized.Internal.BitVector.minus#"
    | [(0,i),(0,j)] <- bitVectorLiterals' args
    -> let ty' = piResultTys tcm ty tys
           (_,resTy) = splitFunForallTy ty'
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
           val = reifyNat resSizeInt (runSizedF (BitVector.-#) i j)
      in  reduce (mkBitVectorLit resTy resSizeTy resSizeInt 0 val)

  "Clash.Sized.Internal.BitVector.times#"
    | [(0,i),(0,j)] <- bitVectorLiterals' args
    -> let ty' = piResultTys tcm ty tys
           (_,resTy) = splitFunForallTy ty'
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
       in  reduce (mkBitVectorLit resTy resSizeTy resSizeInt 0 (i*j))

-- Integral
  "Clash.Sized.Internal.BitVector.quot#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2 (BitVector.quot#) ty tcm tys args)
    -> reduce $ catchDivByZero val
  "Clash.Sized.Internal.BitVector.rem#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2 (BitVector.rem#) ty tcm tys args)
    -> reduce $ catchDivByZero val
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
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2 (BitVector.and#) ty tcm tys args)
    -> reduce val
  "Clash.Sized.Internal.BitVector.or#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2 (BitVector.or#) ty tcm tys args)
    -> reduce val
  "Clash.Sized.Internal.BitVector.xor#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftBitVector2 (BitVector.xor#) ty tcm tys args)
    -> reduce val

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

-- truncateB
  "Clash.Sized.Internal.BitVector.truncateB#" -- forall a b . KnownNat a => BitVector (a + b) -> BitVector a
    | aTy  : _ <- tys
    , Right ka <- runExcept (tyNatSize tcm aTy)
    , [(mski,i)] <- bitVectorLiterals' args
    -> let bitsKeep = (bit (fromInteger ka)) - 1
           val = i .&. bitsKeep
           msk = mski .&. bitsKeep
    in reduce (mkBitVectorLit ty aTy ka msk val)

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
    -> reduce (mkIndexLit ty nTy kn i)

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
    -> reduce (mkIndexLit ty nTy mb (mb - 1))

-- Num
  "Clash.Sized.Internal.Index.+#"
    | Just (nTy,kn) <- extractKnownNat tcm tys
    , [i,j] <- indexLiterals' args
    -> reduce (mkIndexLit ty nTy kn (i + j))
  "Clash.Sized.Internal.Index.-#"
    | Just (nTy,kn) <- extractKnownNat tcm tys
    , [i,j] <- indexLiterals' args
    -> reduce (mkIndexLit ty nTy kn (i - j))
  "Clash.Sized.Internal.Index.*#"
    | Just (nTy,kn) <- extractKnownNat tcm tys
    , [i,j] <- indexLiterals' args
    -> reduce (mkIndexLit ty nTy kn (i * j))

-- ExtendingNum
  "Clash.Sized.Internal.Index.plus#"
    | mTy : nTy : _ <- tys
    , Right _ <- runExcept (tyNatSize tcm mTy)
    , Right _ <- runExcept (tyNatSize tcm nTy)
    , Just (i,j) <- indexLiterals args
    -> let resTyInfo = extractTySizeInfo tcm ty tys
       in  reduce (mkIndexLit' resTyInfo (i + j))
  "Clash.Sized.Internal.Index.minus#"
    | mTy : nTy : _ <- tys
    , Right _ <- runExcept (tyNatSize tcm mTy)
    , Right _ <- runExcept (tyNatSize tcm nTy)
    , Just (i,j) <- indexLiterals args
    -> let resTyInfo = extractTySizeInfo tcm ty tys
       in  reduce (mkIndexLit' resTyInfo (i - j))
  "Clash.Sized.Internal.Index.times#"
    | mTy : nTy : _ <- tys
    , Right _ <- runExcept (tyNatSize tcm mTy)
    , Right _ <- runExcept (tyNatSize tcm nTy)
    , Just (i,j) <- indexLiterals args
    -> let resTyInfo = extractTySizeInfo tcm ty tys
       in  reduce (mkIndexLit' resTyInfo (i * j))

-- Integral
  "Clash.Sized.Internal.Index.quot#"
    | Just (nTy,kn) <- extractKnownNat tcm tys
    , Just (i,j) <- indexLiterals args
    -> reduce $ catchDivByZero (mkIndexLit ty nTy kn (i `quot` j))
  "Clash.Sized.Internal.Index.rem#"
    | Just (nTy,kn) <- extractKnownNat tcm tys
    , Just (i,j) <- indexLiterals args
    -> reduce $ catchDivByZero (mkIndexLit ty nTy kn (i `rem` j))
  "Clash.Sized.Internal.Index.toInteger#"
    | [PrimVal p _ [_, Lit (IntegerLiteral i)]] <- args
    , primName p == "Clash.Sized.Internal.Index.fromInteger#"
    -> reduce (integerToIntegerLiteral i)

-- Resize
  "Clash.Sized.Internal.Index.resize#"
    | Just (mTy,m) <- extractKnownNat tcm tys
    , [i] <- indexLiterals' args
    -> reduce (mkIndexLit ty mTy m i)

---------
-- Signed
---------
  "Clash.Sized.Internal.Signed.size#"
    | Just (_, kn) <- extractKnownNat tcm tys
    -> let (_,tyView -> TyConApp intTcNm _) = splitFunForallTy ty
           (Just intTc) = lookupUniqMap intTcNm tcm
           [intCon] = tyConDataCons intTc
       in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral kn))])

-- BitPack
  "Clash.Sized.Internal.Signed.pack#"
    | Just (nTy, kn) <- extractKnownNat tcm tys
    , [i] <- signedLiterals' args
    -> let val = reifyNat kn (op (fromInteger i))
       in reduce (mkBitVectorLit ty nTy kn 0 val)
    where
        op :: KnownNat n => Signed n -> Proxy n -> Integer
        op s _ = toInteger (Signed.pack# s)
  "Clash.Sized.Internal.Signed.unpack#"
    | Just (nTy, kn) <- extractKnownNat tcm tys
    , [(0,i)] <- bitVectorLiterals' args
    -> let val = reifyNat kn (op (fromInteger i))
       in reduce (mkSignedLit ty nTy kn val)
    where
        op :: KnownNat n => BitVector n -> Proxy n -> Integer
        op s _ = toInteger (Signed.unpack# s)

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
    -> let ty' = piResultTys tcm ty tys
           (_,resTy) = splitFunForallTy ty'
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
       in  reduce (mkSignedLit resTy resSizeTy resSizeInt (i+j))

  "Clash.Sized.Internal.Signed.minus#"
    | Just (i,j) <- signedLiterals args
    -> let ty' = piResultTys tcm ty tys
           (_,resTy) = splitFunForallTy ty'
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
       in  reduce (mkSignedLit resTy resSizeTy resSizeInt (i-j))

  "Clash.Sized.Internal.Signed.times#"
    | Just (i,j) <- signedLiterals args
    -> let ty' = piResultTys tcm ty tys
           (_,resTy) = splitFunForallTy ty'
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
       in  reduce (mkSignedLit resTy resSizeTy resSizeInt (i*j))

-- Integral
  "Clash.Sized.Internal.Signed.quot#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftSigned2 (Signed.quot#) ty tcm tys args)
    -> reduce $ catchDivByZero val
  "Clash.Sized.Internal.Signed.rem#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftSigned2 (Signed.rem#) ty tcm tys args)
    -> reduce $ catchDivByZero val
  "Clash.Sized.Internal.Signed.div#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftSigned2 (Signed.div#) ty tcm tys args)
    -> reduce $ catchDivByZero val
  "Clash.Sized.Internal.Signed.mod#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftSigned2 (Signed.mod#) ty tcm tys args)
    -> reduce $ catchDivByZero val
  "Clash.Sized.Internal.Signed.toInteger#"
    | [PrimVal p _ [_, Lit (IntegerLiteral i)]] <- args
    , primName p == "Clash.Sized.Internal.Signed.fromInteger#"
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
           (Just intTc) = lookupUniqMap intTcNm tcm
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
    -> let ty' = piResultTys tcm ty tys
           (_,resTy) = splitFunForallTy ty'
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
       in  reduce (mkUnsignedLit resTy resSizeTy resSizeInt (i+j))

  "Clash.Sized.Internal.Unsigned.minus#"
    | [i,j] <- unsignedLiterals' args
    -> let ty' = piResultTys tcm ty tys
           (_,resTy) = splitFunForallTy ty'
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
           val = reifyNat resSizeInt (runSizedF (Unsigned.-#) i j)
      in   reduce (mkUnsignedLit resTy resSizeTy resSizeInt val)

  "Clash.Sized.Internal.Unsigned.times#"
    | Just (i,j) <- unsignedLiterals args
    -> let ty' = piResultTys tcm ty tys
           (_,resTy) = splitFunForallTy ty'
           (TyConApp _ [resSizeTy]) = tyView resTy
           Right resSizeInt = runExcept (tyNatSize tcm resSizeTy)
       in  reduce (mkUnsignedLit resTy resSizeTy resSizeInt (i*j))

-- Integral
  "Clash.Sized.Internal.Unsigned.quot#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.quot#) ty tcm tys args)
    -> reduce $ catchDivByZero val
  "Clash.Sized.Internal.Unsigned.rem#"
    | Just (_, kn) <- extractKnownNat tcm tys
    , Just val <- reifyNat kn (liftUnsigned2 (Unsigned.rem#) ty tcm tys args)
    -> reduce $ catchDivByZero val
  "Clash.Sized.Internal.Unsigned.toInteger#"
    | [PrimVal p _ [_, Lit (IntegerLiteral i)]] <- args
    , primName p == "Clash.Sized.Internal.Unsigned.fromInteger#"
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

-- Conversions
  "Clash.Sized.Internal.Unsigned.unsignedToWord"
    | isSubj
    , [a] <- unsignedLiterals' args
    -> let b = Unsigned.unsignedToWord (U (fromInteger a))
           (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
           (Just wordTc) = lookupUniqMap wordTcNm tcm
           [wordDc] = tyConDataCons wordTc
       in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral (toInteger b)))])

  "Clash.Sized.Internal.Unsigned.unsigned8toWord8"
    | isSubj
    , [a] <- unsignedLiterals' args
    -> let b = Unsigned.unsigned8toWord8 (U (fromInteger a))
           (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
           (Just wordTc) = lookupUniqMap wordTcNm tcm
           [wordDc] = tyConDataCons wordTc
       in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral (toInteger b)))])

  "Clash.Sized.Internal.Unsigned.unsigned16toWord16"
    | isSubj
    , [a] <- unsignedLiterals' args
    -> let b = Unsigned.unsigned16toWord16 (U (fromInteger a))
           (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
           (Just wordTc) = lookupUniqMap wordTcNm tcm
           [wordDc] = tyConDataCons wordTc
       in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral (toInteger b)))])

  "Clash.Sized.Internal.Unsigned.unsigned32toWord32"
    | isSubj
    , [a] <- unsignedLiterals' args
    -> let b = Unsigned.unsigned32toWord32 (U (fromInteger a))
           (_,tyView -> TyConApp wordTcNm []) = splitFunForallTy ty
           (Just wordTc) = lookupUniqMap wordTcNm tcm
           [wordDc] = tyConDataCons wordTc
       in  reduce (mkApps (Data wordDc) [Left (Literal (WordLiteral (toInteger b)))])

  "Clash.Annotations.BitRepresentation.Deriving.dontApplyInHDL"
    | isSubj
    , f : a : _ <- args
    -> reduceWHNF (mkApps (valToTerm f) [Left (valToTerm a)])

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
    -> let (Just tupTc) = lookupUniqMap tupTcNm tcm
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
                  Just snatTc = lookupUniqMap snatTcNm tcm
                  [snatDc]    = tyConDataCons snatTc
              in  reduceWHNF $
                  mkApps (valToTerm g)
                         [Right k'ty
                         ,Left (mkApps (Data snatDc)
                                       [Right k'ty
                                       ,Left (Literal (NaturalLiteral (k'-1)))])
                         ,Left (mkApps (Prim pInfo)
                                       [Right pTy
                                       ,Right k'ty
                                       ,Right aTy
                                       ,Left (Literal (NaturalLiteral (k'-1)))
                                       ,Left (valToTerm p)
                                       ,Left (valToTerm f)
                                       ,Left (valToTerm g)
                                       ,Left (Either.lefts tArgs !! 1)
                                       ])
                         ,Left (mkApps (Prim pInfo)
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
    , let ty' = piResultTys tcm ty tys
    , (_,tyView -> TyConApp treeTcNm [lenTy,argTy]) <- splitFunForallTy ty'
    , Right len <- runExcept (tyNatSize tcm lenTy)
    -> let (Just treeTc) = lookupUniqMap treeTcNm tcm
           [lrCon,brCon] = tyConDataCons treeTc
       in  reduce (mkRTree lrCon brCon argTy len (replicate (2^len) (valToTerm (last args))))

---------
-- Vector
---------
  "Clash.Sized.Vector.length" -- :: KnownNat n => Vec n a -> Int
    | isSubj
    , [nTy, _] <- tys
    , Right n <-runExcept (tyNatSize tcm nTy)
    -> let (_, tyView -> TyConApp intTcNm _) = splitFunForallTy ty
           (Just intTc) = lookupUniqMap intTcNm tcm
           [intCon] = tyConDataCons intTc
       in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral (toInteger n)))])

  "Clash.Sized.Vector.maxIndex"
    | isSubj
    , [nTy, _] <- tys
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> let (_, tyView -> TyConApp intTcNm _) = splitFunForallTy ty
           (Just intTc) = lookupUniqMap intTcNm tcm
           [intCon] = tyConDataCons intTc
       in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral (toInteger (n - 1))))])

-- Indexing
  "Clash.Sized.Vector.index_int" -- :: KnownNat n => Vec n a -> Int
    | nTy : aTy : _  <- tys
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
                              mkApps (Prim pInfo)
                                     [Right (LitTy (NumTy (n'-1)))
                                     ,Right aTy
                                     ,Left (Literal (NaturalLiteral (n'-1)))
                                     ,Left (Either.lefts vArgs !! 2)
                                     ,Left (mkApps (Data intDc)
                                                   [Left (Literal (IntLiteral (i'-1)))])
                                     ]
                    _ -> Nothing
                 _ -> Nothing
  "Clash.Sized.Vector.head" -- :: Vec (n+1) a -> a
    | isSubj
    , [DC _ vArgs] <- args
    -> reduceWHNF (Either.lefts vArgs !! 1)
  "Clash.Sized.Vector.last" -- :: Vec (n+1) a -> a
    | isSubj
    , [DC _ vArgs] <- args
    , (Right _ : Right aTy : Right nTy : _) <- vArgs
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> if n == 0
          then reduceWHNF (Either.lefts vArgs !! 1)
          else reduceWHNF
                (mkApps (Prim pInfo)
                                     [Right (LitTy (NumTy (n-1)))
                                     ,Right aTy
                                     ,Left (Either.lefts vArgs !! 2)
                                     ])
-- - Sub-vectors
  "Clash.Sized.Vector.tail" -- :: Vec (n+1) a -> Vec n a
    | isSubj
    , [DC _ vArgs] <- args
    -> reduceWHNF (Either.lefts vArgs !! 2)
  "Clash.Sized.Vector.init" -- :: Vec (n+1) a -> Vec n a
    | isSubj
    , [DC consCon vArgs] <- args
    , (Right _ : Right aTy : Right nTy : _) <- vArgs
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> if n == 0
          then reduceWHNF (Either.lefts vArgs !! 2)
          else reduce $
               mkVecCons consCon aTy n
                  (Either.lefts vArgs !! 1)
                  (mkApps (Prim pInfo)
                                       [Right (LitTy (NumTy (n-1)))
                                       ,Right aTy
                                       ,Left (Either.lefts vArgs !! 2)])
  "Clash.Sized.Vector.select" -- :: (CmpNat (i+s) (s*n) ~ GT) => SNat f -> SNat s -> SNat n -> Vec (f + i) a -> Vec n a
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
                    mkApps (splitAtPrim snatTcNm vecTcNm)
                           [Right sTy
                           ,Right (LitTy (NumTy (i'-s')))
                           ,Right aTy
                           ,Left (valToTerm s)
                           ,Left (valToTerm xs)
                           ]
                   fVecTy = mkTyConApp vecTcNm [sTy,aTy]
                   iVecTy = mkTyConApp vecTcNm [LitTy (NumTy (i'-s')),aTy]
                   -- Guaranteed no capture, so okay to use unsafe name generation
                   fNm    = mkUnsafeSystemName "fxs" 0
                   iNm    = mkUnsafeSystemName "ixs" 1
                   fId    = mkLocalId fVecTy fNm
                   iId    = mkLocalId iVecTy iNm
                   tupPat = DataPat tupDc [] [fId,iId]
                   iAlt   = (tupPat, (Var iId))
               in  reduce $
                   mkVecCons consCon aTy n' (Either.lefts vArgs !! 1) $
                   mkApps (Prim pInfo)
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
                    mkApps (splitAtPrim snatTcNm vecTcNm)
                           [Right fTy
                           ,Right iTy
                           ,Right aTy
                           ,Left (valToTerm f)
                           ,Left (valToTerm xs)
                           ]
                   fVecTy = mkTyConApp vecTcNm [fTy,aTy]
                   iVecTy = mkTyConApp vecTcNm [iTy,aTy]
                   -- Guaranteed no capture, so okay to use unsafe name generation
                   fNm    = mkUnsafeSystemName "fxs" 0
                   iNm    = mkUnsafeSystemName "ixs" 1
                   fId    = mkLocalId fVecTy fNm
                   iId    = mkLocalId iVecTy iNm
                   tupPat = DataPat tupDc [] [fId,iId]
                   iAlt   = (tupPat, (Var iId))
               in  reduceWHNF $
                   mkApps (Prim pInfo)
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
      Just vecTc          = lookupUniqMap vecTcNm tcm
      [nilCon,consCon]    = tyConDataCons vecTc
      TyConApp snatTcNm _ = tyView (Either.rights tyArgs !! 1)
      tupTcNm            = ghcTyconToTyConName (tupleTyCon Boxed 2)
      (Just tupTc)       = lookupUniqMap tupTcNm tcm
      [tupDc]            = tyConDataCons tupTc
-- - Splitting
  "Clash.Sized.Vector.splitAt" -- :: SNat m -> Vec (m + n) a -> (Vec m a, Vec n a)
    | isSubj
    , DC snatDc (Right mTy:_) <- head args
    , Right m <- runExcept (tyNatSize tcm mTy)
    -> let _:nTy:aTy:_ = tys
           -- Get the tuple data-constructor
           ty1 = piResultTys tcm ty tys
           (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty1
           (Just tupTc)       = lookupUniqMap tupTcNm tcm
           [tupDc]            = tyConDataCons tupTc
           -- Get the vector data-constructors
           TyConApp vecTcNm _ = tyView (head tyArgs)
           Just vecTc         = lookupUniqMap vecTcNm tcm
           [nilCon,consCon]   = tyConDataCons vecTc
           -- Recursive call to @splitAt@
           splitAtRec v =
            mkApps (Prim pInfo)
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
           splitAtSelR v = Case (splitAtRec v)
           m1VecTy = mkTyConApp vecTcNm [LitTy (NumTy (m-1)),aTy]
           nVecTy  = mkTyConApp vecTcNm [nTy,aTy]
           -- Guaranteed no capture, so okay to use unsafe name generation
           lNm     = mkUnsafeSystemName "l" 0
           rNm     = mkUnsafeSystemName "r" 1
           lId     = mkLocalId m1VecTy lNm
           rId     = mkLocalId nVecTy rNm
           tupPat  = DataPat tupDc [] [lId,rId]
           lAlt    = (tupPat, (Var lId))
           rAlt    = (tupPat, (Var rId))

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
                           (splitAtSelR (Either.lefts vArgs !! 2) m1VecTy [lAlt]))
                 , Left (splitAtSelR (Either.lefts vArgs !! 2) nVecTy [rAlt])
                 ]
         -- v doesn't reduce to a data-constructor
         _  -> Nothing

  "Clash.Sized.Vector.unconcat" -- :: KnownNat n => SNamt m -> Vec (n * m) a -> Vec n (Vec m a)
    | isSubj
    , kn : snat : v : _  <- args
    , nTy : mTy : aTy :_ <- tys
    , Lit (NaturalLiteral n) <- kn
    -> let ( Either.rights -> argTys, tyView -> TyConApp vecTcNm _) =
              splitFunForallTy ty
           Just vecTc = lookupUniqMap vecTcNm tcm
           [nilCon,consCon]   = tyConDataCons vecTc
           tupTcNm            = ghcTyconToTyConName (tupleTyCon Boxed 2)
           (Just tupTc)       = lookupUniqMap tupTcNm tcm
           [tupDc]            = tyConDataCons tupTc
           TyConApp snatTcNm _ = tyView (argTys !! 1)
           n1mTy  = mkTyConApp typeNatMul
                        [mkTyConApp typeNatSub [nTy,LitTy (NumTy 1)]
                        ,mTy]
           splitAtCall =
            mkApps (splitAtPrim snatTcNm vecTcNm)
                   [Right mTy
                   ,Right n1mTy
                   ,Right aTy
                   ,Left (valToTerm snat)
                   ,Left (valToTerm v)
                   ]
           mVecTy   = mkTyConApp vecTcNm [mTy,aTy]
           n1mVecTy = mkTyConApp vecTcNm [n1mTy,aTy]
           -- Guaranteed no capture, so okay to use unsafe name generation
           asNm     = mkUnsafeSystemName "as" 0
           bsNm     = mkUnsafeSystemName "bs" 1
           asId     = mkLocalId mVecTy asNm
           bsId     = mkLocalId n1mVecTy bsNm
           tupPat   = DataPat tupDc [] [asId,bsId]
           asAlt    = (tupPat, (Var asId))
           bsAlt    = (tupPat, (Var bsId))

       in  case n of
         0 -> reduce (mkVecNil nilCon mVecTy)
         _ -> reduce $
              mkVecCons consCon mVecTy n
                (Case splitAtCall mVecTy [asAlt])
                (mkApps (Prim pInfo)
                    [Right (LitTy (NumTy (n-1)))
                    ,Right mTy
                    ,Right aTy
                    ,Left (Literal (NaturalLiteral (n-1)))
                    ,Left (valToTerm snat)
                    ,Left (Case splitAtCall n1mVecTy [bsAlt])])
-- Construction
-- - initialisation
  "Clash.Sized.Vector.replicate" -- :: SNat n -> a -> Vec n a
    | isSubj
    , let ty' = piResultTys tcm ty tys
    , let (_,resTy) = splitFunForallTy ty'
    , (TyConApp vecTcNm [lenTy,argTy]) <- tyView resTy
    , Right len <- runExcept (tyNatSize tcm lenTy)
    -> let (Just vecTc) = lookupUniqMap vecTcNm tcm
           [nilCon,consCon] = tyConDataCons vecTc
       in  reduce $
           mkVec nilCon consCon argTy len
                 (replicate (fromInteger len) (valToTerm (last args)))
-- - Concatenation
  "Clash.Sized.Vector.++" -- :: Vec n a -> Vec m a -> Vec (n + m) a
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
                 (mkApps (Prim pInfo)
                                      [Right (LitTy (NumTy (n'-1)))
                                      ,Right aTy
                                      ,Right mTy
                                      ,Left (Either.lefts vArgs !! 2)
                                      ,Left (valToTerm (last args))
                                      ])
         _ -> Nothing
  "Clash.Sized.Vector.concat" -- :: Vec n (Vec m a) -> Vec (n * m) a
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
             mkApps (vecAppendPrim vecTcNm)
                    [Right mTy
                    ,Right aTy
                    ,Right $ mkTyConApp typeNatMul
                      [mkTyConApp typeNatSub [nTy,LitTy (NumTy 1)], mTy]
                    ,Left h'
                    ,Left $ mkApps (Prim pInfo)
                      [ Right (LitTy (NumTy (n-1)))
                      , Right mTy
                      , Right aTy
                      , Left t
                      ]
                    ]
        _ -> Nothing

-- Modifying vectors
  "Clash.Sized.Vector.replace_int" -- :: KnownNat n => Vec n a -> Int -> a -> Vec n a
    | nTy : aTy : _  <- tys
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
                                (mkApps (Prim pInfo)
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

  "Clash.Transformations.eqInt"
    | [ DC _ [Left (Literal (IntLiteral i))]
      , DC _ [Left (Literal (IntLiteral j))]
      ] <- args
    -> reduce (boolToBoolLiteral tcm ty (i == j))

-- - specialized permutations
  "Clash.Sized.Vector.reverse" -- :: Vec n a -> Vec n a
    | isSubj
    , nTy : aTy : _  <- tys
    , [DC vecDc vArgs] <- args
    -> case runExcept (tyNatSize tcm nTy) of
         Right 0 -> reduce (mkVecNil vecDc aTy)
         Right n
           | (_,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
           , let (Just vecTc) = lookupUniqMap vecTcNm tcm
           , let [nilCon,consCon] = tyConDataCons vecTc
           -> reduceWHNF $
              mkApps (vecAppendPrim vecTcNm)
                [Right (LitTy (NumTy (n-1)))
                ,Right aTy
                ,Right (LitTy (NumTy 1))
                ,Left (mkApps (Prim pInfo)
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
      0 -> let (Just vecTc)     = lookupUniqMap vecTcNm tcm
               [nilCon,consCon] = tyConDataCons vecTc
           in  reduce $
               mkVec nilCon consCon (mkTyConApp vecTcNm [mTy,aTy]) n
                (replicate (fromInteger n) (mkVec nilCon consCon aTy 0 []))
      m' -> let (Just vecTc)     = lookupUniqMap vecTcNm tcm
                [_,consCon] = tyConDataCons vecTc
                Just (consCoTy : _) = dataConInstArgTys consCon
                                        [mTy,aTy,LitTy (NumTy (m'-1))]
            in  reduceWHNF $
                mkApps (vecZipWithPrim vecTcNm)
                       [ Right aTy
                       , Right (mkTyConApp vecTcNm [LitTy (NumTy (m'-1)),aTy])
                       , Right (mkTyConApp vecTcNm [mTy,aTy])
                       , Right nTy
                       , Left  (mkApps (Data consCon)
                                       [Right mTy
                                       ,Right aTy
                                       ,Right (LitTy (NumTy (m'-1)))
                                       ,Left (primCo consCoTy)
                                       ])
                       , Left  (Either.lefts vArgs !! 1)
                       , Left  (mkApps (Prim pInfo)
                                       [ Right nTy
                                       , Right (LitTy (NumTy (m'-1)))
                                       , Right aTy
                                       , Left  (valToTerm kn)
                                       , Left  (Either.lefts vArgs !! 2)
                                       ])
                       ]

  "Clash.Sized.Vector.rotateLeftS" -- :: KnownNat n => Vec n a -> SNat d -> Vec n a
    | nTy : aTy : _ : _ <- tys
    , kn : xs : d : _ <- args
    , DC dc vArgs <- xs
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> case n of
         0  -> reduce (mkVecNil dc aTy)
         n' | DC snatDc [_,Left d'] <- d
            , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
            , mach2@Machine{mStack=[],mTerm=Literal (NaturalLiteral d2)} <- whnf eval tcm isSubj (setTerm d' $ stackClear mach)
            -> case (d2 `mod` n) of
                 0  -> reduce (valToTerm xs)
                 d3 -> let (_,tyView -> TyConApp vecTcNm _) = splitFunForallTy ty
                           (Just vecTc)     = lookupUniqMap vecTcNm tcm
                           [nilCon,consCon] = tyConDataCons vecTc
                       in  reduceWHNF' mach2 $
                           mkApps (Prim pInfo)
                                  [Right nTy
                                  ,Right aTy
                                  ,Right (LitTy (NumTy (d3-1)))
                                  ,Left (valToTerm kn)
                                  ,Left (mkApps (vecAppendPrim vecTcNm)
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

  "Clash.Sized.Vector.rotateRightS" -- :: KnownNat n => Vec n a -> SNat d -> Vec n a
    | isSubj
    , nTy : aTy : _ : _ <- tys
    , kn : xs : d : _ <- args
    , DC dc _ <- xs
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> case n of
         0  -> reduce (mkVecNil dc aTy)
         n' | DC snatDc [_,Left d'] <- d
            , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
            , mach2@Machine{mStack=[],mTerm=Literal (NaturalLiteral d2)} <- whnf eval tcm isSubj (setTerm d' $ stackClear mach)
            -> case (d2 `mod` n) of
                 0  -> reduce (valToTerm xs)
                 d3 -> let (_,tyView -> TyConApp vecTcNm _) = splitFunForallTy ty
                       in  reduceWHNF' mach2 $
                           mkApps (Prim pInfo)
                                  [Right nTy
                                  ,Right aTy
                                  ,Right (LitTy (NumTy (d3-1)))
                                  ,Left (valToTerm kn)
                                  ,Left (mkVecCons dc aTy n
                                          (mkApps (vecLastPrim vecTcNm)
                                                  [Right (LitTy (NumTy (n'-1)))
                                                  ,Right aTy
                                                  ,Left  (valToTerm xs)])
                                          (mkApps (vecInitPrim vecTcNm)
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
  "Clash.Sized.Vector.map" -- :: (a -> b) -> Vec n a -> Vec n b
    | isSubj
    , DC dc vArgs <- args !! 1
    , aTy : bTy : nTy : _ <- tys
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> case n of
         0  -> reduce (mkVecNil dc bTy)
         n' -> reduce $
               mkVecCons dc bTy n'
                 (mkApps (valToTerm (args !! 0)) [Left (Either.lefts vArgs !! 1)])
                 (mkApps (Prim pInfo)
                                      [Right aTy
                                      ,Right bTy
                                      ,Right (LitTy (NumTy (n' - 1)))
                                      ,Left (valToTerm (args !! 0))
                                      ,Left (Either.lefts vArgs !! 2)])
  "Clash.Sized.Vector.imap" -- :: forall n a b . KnownNat n => (Index n -> a -> b) -> Vec n a -> Vec n b
    | isSubj
    , nTy : aTy : bTy : _ <- tys
    , (tyArgs,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
    , let (tyArgs',_) = splitFunForallTy (Either.rights tyArgs !! 1)
    , TyConApp indexTcNm _ <- tyView (Either.rights tyArgs' !! 0)
    , Right n <- runExcept (tyNatSize tcm nTy)
    , let iLit = mkIndexLit (Either.rights tyArgs' !! 0) nTy n 0
    -> reduceWHNF $
       mkApps (Prim (PrimInfo "Clash.Sized.Vector.imap_go" (vecImapGoTy vecTcNm indexTcNm) WorkNever Nothing))
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
                   iLit = mkIndexLit (Either.rights tyArgs !! 0) nTy n' 1
               in reduce $ mkVecCons dc bTy m'
                 (mkApps (valToTerm f) [Left (valToTerm n),Left (Either.lefts vArgs !! 1)])
                 (mkApps (Prim pInfo)
                         [Right nTy
                         ,Right (LitTy (NumTy (m'-1)))
                         ,Right aTy
                         ,Right bTy
                         ,Left (mkApps (Prim (PrimInfo "Clash.Sized.Internal.Index.+#" (indexAddTy indexTcNm) WorkVariable Nothing))
                                       [Right nTy
                                       ,Left (Literal (NaturalLiteral n'))
                                       ,Left (valToTerm n)
                                       ,Left iLit
                                       ])
                         ,Left (valToTerm f)
                         ,Left (Either.lefts vArgs !! 2)
                         ])

  -- :: forall n a. KnownNat n => (a -> a) -> a -> Vec n a
  "Clash.Sized.Vector.iterateI"
    | isSubj
    , [nTy, aTy] <- tys
    , [_n, f, a] <- args
    , Right n <- runExcept (tyNatSize tcm nTy)
    ->
      let
        TyConApp vecTcNm _ = tyView (getResultTy tcm ty tys)
        Just vecTc = lookupUniqMap vecTcNm tcm
        [nilCon, consCon] = tyConDataCons vecTc
      in case n of
         0 -> reduce (mkVecNil nilCon aTy)
         _ -> reduce $
          mkVecCons consCon aTy n
            (valToTerm a)
            (mkApps
              (Prim pInfo)
              [ Right (LitTy (NumTy (n - 1)))
              , Right aTy
              , Left (valToTerm (Lit (NaturalLiteral (n - 1))))
              , Left (valToTerm f)
              , Left (mkApps (valToTerm f) [Left (valToTerm a)])
              ])

-- - Zipping
  "Clash.Sized.Vector.zipWith" -- :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
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
                            ,Left (mkApps (vecHeadPrim vecTcNm)
                                    [Right (LitTy (NumTy (n'-1)))
                                    ,Right bTy
                                    ,Left  (valToTerm ys)
                                    ])
                            ])
                 (mkApps (Prim pInfo)
                                      [Right aTy
                                      ,Right bTy
                                      ,Right cTy
                                      ,Right (LitTy (NumTy (n' - 1)))
                                      ,Left (valToTerm f)
                                      ,Left (Either.lefts vArgs !! 2)
                                      ,Left (mkApps (vecTailPrim vecTcNm)
                                                    [Right (LitTy (NumTy (n'-1)))
                                                    ,Right bTy
                                                    ,Left (valToTerm ys)
                                                    ])])

-- Folding
  "Clash.Sized.Vector.foldr" -- :: (a -> b -> b) -> b -> Vec n a -> b
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
                     ,Left (mkApps (Prim pInfo)
                                   [Right aTy
                                   ,Right bTy
                                   ,Right (LitTy (NumTy (n-1)))
                                   ,Left  (valToTerm f)
                                   ,Left  (valToTerm z)
                                   ,Left  (Either.lefts vArgs !! 2)
                                   ])
                     ]
  "Clash.Sized.Vector.fold" -- :: (a -> a -> a) -> Vec (n + 1) a -> a
    | isSubj
    , nTy : aTy :  _ <- tys
    , f : vs : _ <- args
    , DC _ vArgs <- vs
    , Right n <- runExcept (tyNatSize tcm nTy)
    -> case n of
         0 -> reduceWHNF (Either.lefts vArgs !! 1)
         _ -> let (tyArgs,_)         = splitFunForallTy ty
                  TyConApp vecTcNm _ = tyView (Either.rights tyArgs !! 1)
                  tupTcNm      = ghcTyconToTyConName (tupleTyCon Boxed 2)
                  (Just tupTc) = lookupUniqMap tupTcNm tcm
                  [tupDc]      = tyConDataCons tupTc
                  n'     = n+1
                  m      = n' `div` 2
                  n1     = n' - m
                  mTy    = LitTy (NumTy m)
                  m'ty   = LitTy (NumTy (m-1))
                  n1mTy  = LitTy (NumTy n1)
                  n1m'ty = LitTy (NumTy (n1-1))
                  splitAtCall =
                   mkApps (Prim (PrimInfo "Clash.Sized.Vector.fold_split" (foldSplitAtTy vecTcNm) WorkNever Nothing))
                          [Right mTy
                          ,Right n1mTy
                          ,Right aTy
                          ,Left (Literal (NaturalLiteral m))
                          ,Left (valToTerm vs)
                          ]
                  mVecTy   = mkTyConApp vecTcNm [mTy,aTy]
                  n1mVecTy = mkTyConApp vecTcNm [n1mTy,aTy]
                  -- Guaranteed no capture, so okay to use unsafe name generation
                  asNm     = mkUnsafeSystemName "as" 0
                  bsNm     = mkUnsafeSystemName "bs" 1
                  asId     = mkLocalId mVecTy asNm
                  bsId     = mkLocalId n1mVecTy bsNm
                  tupPat   = DataPat tupDc [] [asId,bsId]
                  asAlt    = (tupPat, (Var asId))
                  bsAlt    = (tupPat, (Var bsId))
              in  reduceWHNF $
                  mkApps (valToTerm f)
                         [Left (mkApps (Prim pInfo)
                                       [Right m'ty
                                       ,Right aTy
                                       ,Left (valToTerm f)
                                       ,Left (Case splitAtCall mVecTy [asAlt])
                                       ])
                         ,Left (mkApps (Prim pInfo)
                                       [Right n1m'ty
                                       ,Right aTy
                                       ,Left  (valToTerm f)
                                       ,Left  (Case splitAtCall n1mVecTy [bsAlt])
                                       ])
                         ]


  "Clash.Sized.Vector.fold_split" -- :: Natural -> Vec (m + n) a -> (Vec m a, Vec n a)
    | isSubj
    , mTy : nTy : aTy : _ <- tys
    , Right m <- runExcept (tyNatSize tcm mTy)
    -> let -- Get the tuple data-constructor
           ty1 = piResultTys tcm ty tys
           (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty1
           (Just tupTc)       = lookupUniqMap tupTcNm tcm
           [tupDc]            = tyConDataCons tupTc
           -- Get the vector data-constructors
           TyConApp vecTcNm _ = tyView (head tyArgs)
           Just vecTc         = lookupUniqMap vecTcNm tcm
           [nilCon,consCon]   = tyConDataCons vecTc
           -- Recursive call to @splitAt@
           splitAtRec v =
            mkApps (Prim pInfo)
                   [Right (LitTy (NumTy (m-1)))
                   ,Right nTy
                   ,Right aTy
                   ,Left (Literal (NaturalLiteral (m-1)))
                   ,Left v
                   ]
           -- Projection either the first or second field of the recursive
           -- call to @splitAt@
           splitAtSelR v = Case (splitAtRec v)
           m1VecTy = mkTyConApp vecTcNm [LitTy (NumTy (m-1)),aTy]
           nVecTy  = mkTyConApp vecTcNm [nTy,aTy]
           -- Guaranteed no capture, so okay to use unsafe name generation
           lNm     = mkUnsafeSystemName "l" 0
           rNm     = mkUnsafeSystemName "r" 1
           lId     = mkLocalId m1VecTy lNm
           rId     = mkLocalId nVecTy rNm
           tupPat  = DataPat tupDc [] [lId,rId]
           lAlt    = (tupPat, (Var lId))
           rAlt    = (tupPat, (Var rId))
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
                           (splitAtSelR (Either.lefts vArgs !! 2) m1VecTy [lAlt]))
                 , Left (splitAtSelR (Either.lefts vArgs !! 2) nVecTy [rAlt])
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
                  Just snatTc = lookupUniqMap snatTcNm tcm
                  [snatDc]    = tyConDataCons snatTc
                  k'ty        = LitTy (NumTy (k'-1))
              in  reduceWHNF $
                  mkApps (valToTerm f)
                         [Right k'ty
                         ,Left (mkApps (Data snatDc)
                                       [Right k'ty
                                       ,Left (Literal (NaturalLiteral (k'-1)))])
                         ,Left (Either.lefts vArgs !! 1)
                         ,Left (mkApps (Prim pInfo)
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
                  Just snatTc = lookupUniqMap snatTcNm tcm
                  [snatDc]    = tyConDataCons snatTc
                  tupTcNm     = ghcTyconToTyConName (tupleTyCon Boxed 2)
                  (Just tupTc) = lookupUniqMap tupTcNm tcm
                  [tupDc]     = tyConDataCons tupTc
                  k'ty        = LitTy (NumTy (k'-1))
                  k2ty        = LitTy (NumTy (2^(k'-1)))
                  splitAtCall =
                   mkApps (splitAtPrim snatTcNm vecTcNm)
                          [Right k2ty
                          ,Right k2ty
                          ,Right aTy
                          ,Left (mkApps (Data snatDc)
                                        [Right k2ty
                                        ,Left (Literal (NaturalLiteral (2^(k'-1))))])
                          ,Left (valToTerm xs)
                          ]
                  xsSVecTy = mkTyConApp vecTcNm [k2ty,aTy]
                  -- Guaranteed no capture, so okay to use unsafe name generation
                  xsLNm    = mkUnsafeSystemName "xsL" 0
                  xsRNm    = mkUnsafeSystemName "xsR" 1
                  xsLId    = mkLocalId k2ty xsLNm
                  xsRId    = mkLocalId k2ty xsRNm
                  tupPat   = DataPat tupDc [] [xsLId,xsRId]
                  asAlt    = (tupPat, (Var xsLId))
                  bsAlt    = (tupPat, (Var xsRId))
              in  reduceWHNF $
                  mkApps (valToTerm g)
                         [Right k'ty
                         ,Left (mkApps (Data snatDc)
                                       [Right k'ty
                                       ,Left (Literal (NaturalLiteral (k'-1)))])
                         ,Left (mkApps (Prim pInfo)
                                       [Right pTy
                                       ,Right k'ty
                                       ,Right aTy
                                       ,Left (Literal (NaturalLiteral (k'-1)))
                                       ,Left (valToTerm p)
                                       ,Left (valToTerm f)
                                       ,Left (valToTerm g)
                                       ,Left (Case splitAtCall xsSVecTy [asAlt])])
                         ,Left (mkApps (Prim pInfo)
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
         0  -> let (Just vecTc) = lookupUniqMap vecTcNm tcm
                   [nilCon,_]   = tyConDataCons vecTc
               in  reduce (mkVecNil nilCon aTy)
         n' -> let (Just vecTc) = lookupUniqMap vecTcNm tcm
                   [_,consCon]  = tyConDataCons vecTc
               in  reduce $ mkVecCons consCon aTy n'
                     (mkApps (vecHeadPrim vecTcNm)
                             [ Right (LitTy (NumTy (n' - 1)))
                             , Right aTy
                             , Left  (valToTerm xs)
                             ])
                     (mkApps (Prim pInfo)
                             [ Right (LitTy (NumTy (n' - 1)))
                             , Right aTy
                             , Left  (Literal (NaturalLiteral (n'-1)))
                             , Left  (mkApps (vecTailPrim vecTcNm)
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
         0 -> let (pureF,ids') = runPEM (mkSelectorCase $(curLoc) is0 tcm (valToTerm apDict) 1 1) ids
              in  reduceWHNF' (mach { mSupply = ids' }) $
                  mkApps pureF
                         [Right (mkTyConApp (vecTcNm) [nTy,bTy])
                         ,Left  (mkVecNil dc bTy)]
         _ -> let ((fmapF,apF),ids') = flip runPEM ids $ do
                    fDict  <- mkSelectorCase $(curLoc) is0 tcm (valToTerm apDict) 1 0
                    fmapF' <- mkSelectorCase $(curLoc) is0 tcm fDict 1 0
                    apF'   <- mkSelectorCase $(curLoc) is0 tcm (valToTerm apDict) 1 2
                    return (fmapF',apF')
                  n'ty = LitTy (NumTy (n-1))
                  Just (consCoTy : _) = dataConInstArgTys dc [nTy,bTy,n'ty]
              in  reduceWHNF' (mach { mSupply = ids' }) $
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
                                                     ,Left (primCo consCoTy)])
                                       ,Left (mkApps (valToTerm f)
                                                     [Left (Either.lefts vArgs !! 1)])
                                       ])
                         ,Left (mkApps (Prim pInfo)
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
      (ids, is0) = (mSupply mach, mScopeNames mach)

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
               mkApps (bvAppendPrim bvTcNm)
                 [ Right (mkTyConApp typeNatMul [LitTy (NumTy (n'-1)),mTy])
                 , Right mTy
                 , Left (Literal (NaturalLiteral ((n'-1)*m)))
                 , Left (Either.lefts vArgs !! 1)
                 , Left (mkApps (Prim pInfo)
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
          let (Just vecTc) = lookupUniqMap vecTcNm tcm
              [nilCon,_] = tyConDataCons vecTc
          in  reduce (mkVecNil nilCon (mkTyConApp bvTcNm [mTy]))
         n' | Right m <- runExcept (tyNatSize tcm mTy) ->
          let Just vecTc  = lookupUniqMap vecTcNm tcm
              [_,consCon] = tyConDataCons vecTc
              tupTcNm     = ghcTyconToTyConName (tupleTyCon Boxed 2)
              Just tupTc  = lookupUniqMap tupTcNm tcm
              [tupDc]     = tyConDataCons tupTc
              splitCall   =
                mkApps (bvSplitPrim bvTcNm)
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
              -- Guaranteed no capture, so okay to use unsafe name generation
              xNm         = mkUnsafeSystemName "x" 0
              bvNm        = mkUnsafeSystemName "bv'" 1
              xId         = mkLocalId mBVTy xNm
              bvId        = mkLocalId n1BVTy bvNm
              tupPat      = DataPat tupDc [] [xId,bvId]
              xAlt        = (tupPat, (Var xId))
              bvAlt       = (tupPat, (Var bvId))

          in  reduce $ mkVecCons consCon (mkTyConApp bvTcNm [mTy]) n'
                (Case splitCall mBVTy [xAlt])
                (mkApps (Prim pInfo)
                        [ Right (LitTy (NumTy (n'-1)))
                        , Right mTy
                        , Left (Literal (NaturalLiteral (n'-1)))
                        , Left (valToTerm km)
                        , Left (Case splitCall n1BVTy [bvAlt])
                        ])
         _ -> Nothing
  _ -> Nothing
  where
    ty = primType pInfo

    checkNaturalRange1 nTy i f =
      checkNaturalRange nTy [i]
        (\[i'] -> naturalToNaturalLiteral (f i'))

    checkNaturalRange2 nTy i j f =
      checkNaturalRange nTy [i, j]
        (\[i', j'] -> naturalToNaturalLiteral (f i' j'))

    -- Check given integer's range. If any of them are less than zero, give up
    -- and return an undefined type.
    checkNaturalRange
      :: Type
      -- Type of GHC.Natural.Natural ^
      -> [Integer]
      -> ([Natural] -> Term)
      -> Term
    checkNaturalRange nTy natsAsInts f =
      if any (<0) natsAsInts then
        undefinedTm nTy
      else
        f (map fromInteger natsAsInts)

    reduce :: Term -> Maybe Machine
    reduce e = case isX e of
      Left msg -> trace (unlines ["Warning: Not evaluating constant expression:", show (primName pInfo), "Because doing so generates an XException:", msg]) Nothing
      Right e' -> Just (setTerm e' mach)

    reduceWHNF e =
      let eval = Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
          mach1@Machine{mStack=[]} = whnf eval tcm isSubj (setTerm e $ stackClear mach)
      in Just $ mach1 { mStack = mStack mach }

    reduceWHNF' mach1 e =
      let eval = Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
          mach2@Machine{mStack=[]} = whnf eval tcm isSubj (setTerm e mach1)
       in Just $ mach2 { mStack = mStack mach }

    makeUndefinedIf :: Exception e => (e -> Bool) -> Term -> Term
    makeUndefinedIf wantToHandle tm =
      case unsafeDupablePerformIO $ tryJust selectException (evaluate $ force tm) of
        Right b -> b
        Left e -> trace (msg e) (undefinedTm resTy)
      where
        resTy = getResultTy tcm ty tys
        selectException e | wantToHandle e = Just e
                          | otherwise = Nothing
        msg e = unlines ["Warning: caught exception: \"" ++ show e ++ "\" while trying to evaluate: "
                        , showPpr (mkApps (Prim pInfo) (map (Left . valToTerm) args))
                        ]

    catchDivByZero = makeUndefinedIf (==DivideByZero)

-- Helper functions for literals

pairOf :: (Value -> Maybe a) -> [Value] -> Maybe (a, a)
pairOf f [x, y] = (,) <$> f x <*> f y
pairOf _ _ = Nothing

listOf :: (Value -> Maybe a) -> [Value] -> [a]
listOf = mapMaybe

wrapUnsigned :: Integer -> Integer -> Integer
wrapUnsigned n i = i `mod` sz
 where
  sz = 1 `shiftL` fromInteger n

wrapSigned :: Integer -> Integer -> Integer
wrapSigned n i = if mask == 0 then 0 else res
 where
  mask = 1 `shiftL` fromInteger (n - 1)
  res  = case divMod i mask of
           (s,i1) | even s    -> i1
                  | otherwise -> i1 - mask

doubleLiterals' :: [Value] -> [Rational]
doubleLiterals' = listOf doubleLiteral

doubleLiteral :: Value -> Maybe Rational
doubleLiteral v = case v of
  Lit (DoubleLiteral i) -> Just i
  _ -> Nothing

floatLiterals' :: [Value] -> [Rational]
floatLiterals' = listOf floatLiteral

floatLiteral :: Value -> Maybe Rational
floatLiteral v = case v of
  Lit (FloatLiteral i) -> Just i
  _ -> Nothing

integerLiterals :: [Value] -> Maybe (Integer, Integer)
integerLiterals = pairOf integerLiteral

integerLiteral :: Value -> Maybe Integer
integerLiteral v =
  case v of
    Lit (IntegerLiteral i) -> Just i
    DC dc [Left (Literal (IntLiteral i))]
      | dcTag dc == 1
      -> Just i
    DC dc [Left (Literal (ByteArrayLiteral (BA.ByteArray ba)))]
      | dcTag dc == 2
      -> Just (Jp# (BN# ba))
      | dcTag dc == 3
      -> Just (Jn# (BN# ba))
    _ -> Nothing

naturalLiterals :: [Value] -> Maybe (Integer, Integer)
naturalLiterals = pairOf naturalLiteral

naturalLiteral :: Value -> Maybe Integer
naturalLiteral v =
  case v of
    Lit (NaturalLiteral i) -> Just i
    DC dc [Left (Literal (WordLiteral i))]
      | dcTag dc == 1
      -> Just i
    DC dc [Left (Literal (ByteArrayLiteral (BA.ByteArray ba)))]
      | dcTag dc == 2
      -> Just (Jp# (BN# ba))
    _ -> Nothing

integerLiterals' :: [Value] -> [Integer]
integerLiterals' = listOf integerLiteral

naturalLiterals' :: [Value] -> [Integer]
naturalLiterals' = listOf naturalLiteral

intLiterals :: [Value] -> Maybe (Integer,Integer)
intLiterals = pairOf intLiteral

intLiterals' :: [Value] -> [Integer]
intLiterals' = listOf intLiteral

intLiteral :: Value -> Maybe Integer
intLiteral x = case x of
  Lit (IntLiteral i) -> Just i
  _ -> Nothing

intCLiteral :: Value -> Maybe Integer
intCLiteral v = case v of
  (DC _ [Left (Literal (IntLiteral i))]) -> Just i
  _ -> Nothing

intCLiterals :: [Value] -> Maybe (Integer, Integer)
intCLiterals = pairOf intCLiteral

wordLiterals :: [Value] -> Maybe (Integer,Integer)
wordLiterals = pairOf wordLiteral

wordLiterals' :: [Value] -> [Integer]
wordLiterals' = listOf wordLiteral

wordLiteral :: Value -> Maybe Integer
wordLiteral x = case x of
  Lit (WordLiteral i) -> Just i
  _ -> Nothing

charLiterals :: [Value] -> Maybe (Char,Char)
charLiterals = pairOf charLiteral

charLiterals' :: [Value] -> [Char]
charLiterals' = listOf charLiteral

charLiteral :: Value -> Maybe Char
charLiteral x = case x of
  Lit (CharLiteral c) -> Just c
  _ -> Nothing

sizedLiterals :: Text -> [Value] -> Maybe (Integer,Integer)
sizedLiterals szCon = pairOf (sizedLiteral szCon)

sizedLiterals' :: Text -> [Value] -> [Integer]
sizedLiterals' szCon = listOf (sizedLiteral szCon)

sizedLiteral :: Text -> Value -> Maybe Integer
sizedLiteral szCon val = case val of
  PrimVal p _ [_, Lit (IntegerLiteral i)]
    | primName p == szCon -> Just i
  _ -> Nothing

bitLiterals
  :: [Value]
  -> [(Integer,Integer)]
bitLiterals = map normalizeBit . mapMaybe go
 where
  normalizeBit (msk,v) = (msk .&. 1, v .&. 1)
  go val = case val of
    PrimVal p _ [Lit (WordLiteral m), Lit (IntegerLiteral i)]
      | primName p == "Clash.Sized.Internal.BitVector.fromInteger##"
      -> Just (m,i)
    _ -> Nothing

indexLiterals, signedLiterals, unsignedLiterals
  :: [Value] -> Maybe (Integer,Integer)
indexLiterals     = sizedLiterals "Clash.Sized.Internal.Index.fromInteger#"
signedLiterals    = sizedLiterals "Clash.Sized.Internal.Signed.fromInteger#"
unsignedLiterals  = sizedLiterals "Clash.Sized.Internal.Unsigned.fromInteger#"

indexLiterals', signedLiterals', unsignedLiterals'
  :: [Value] -> [Integer]
indexLiterals'     = sizedLiterals' "Clash.Sized.Internal.Index.fromInteger#"
signedLiterals'    = sizedLiterals' "Clash.Sized.Internal.Signed.fromInteger#"
unsignedLiterals'  = sizedLiterals' "Clash.Sized.Internal.Unsigned.fromInteger#"

bitVectorLiterals'
  :: [Value] -> [(Integer,Integer)]
bitVectorLiterals' = listOf bitVectorLiteral

bitVectorLiteral :: Value -> Maybe (Integer, Integer)
bitVectorLiteral val = case val of
  (PrimVal p _ [_, Lit (NaturalLiteral m), Lit (IntegerLiteral i)])
    | primName p == "Clash.Sized.Internal.BitVector.fromInteger#" -> Just (m, i)
  _ -> Nothing

toBV :: (Integer,Integer) -> BitVector n
toBV (mask,val) = BV (fromInteger mask) (fromInteger val)

splitBV :: BitVector n -> (Integer,Integer)
splitBV (BV msk val) = (toInteger msk, toInteger val)

toBit :: (Integer,Integer) -> Bit
toBit (mask,val) = Bit (fromInteger mask) (fromInteger val)

valArgs
  :: Value
  -> Maybe [Term]
valArgs v =
  case v of
    PrimVal _ _ vs -> Just (fmap valToTerm vs)
    DC _ args -> Just (Either.lefts args)
    _ -> Nothing

-- Tries to match literal arguments to a function like
--   (Unsigned.shiftL#  :: forall n. KnownNat n => Unsigned n -> Int -> Unsigned n)
sizedLitIntLit
  :: Text -> TyConMap -> [Type] -> [Value]
  -> Maybe (Type,Integer,Integer,Integer)
sizedLitIntLit szCon tcm tys args
  | Just (nTy,kn) <- extractKnownNat tcm tys
  , [_
    ,PrimVal p _ [_,Lit (IntegerLiteral i)]
    ,valArgs -> Just [Literal (IntLiteral j)]
    ] <- args
  , primName p == szCon
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
    ,PrimVal p _ [_,Lit (NaturalLiteral m),Lit (IntegerLiteral i)]
    ,valArgs -> Just [Literal (IntLiteral j)]
    ] <- args
  , primName p == "Clash.Sized.Internal.BitVector.fromInteger#"
  = Just (nTy,kn,(m,i),j)
  | otherwise
  = Nothing

-- From an argument list to function of type
--   forall n. KnownNat n => ...
-- extract (nTy,nInt)
-- where nTy is the Type of n
-- and   nInt is its value as an Integer
extractKnownNat :: TyConMap -> [Type] -> Maybe (Type, Integer)
extractKnownNat tcm tys = case tys of
  nTy : _ | Right nInt <- runExcept (tyNatSize tcm nTy)
    -> Just (nTy, nInt)
  _ -> Nothing

-- From an argument list to function of type
--   forall n m o .. . (KnownNat n, KnownNat m, KnownNat o, ..) => ...
-- extract [(nTy,nInt), (mTy,mInt), (oTy,oInt)]
-- where nTy is the Type of n
-- and   nInt is its value as an Integer
extractKnownNats :: TyConMap -> [Type] -> [(Type, Integer)]
extractKnownNats tcm =
  mapMaybe (extractKnownNat tcm . pure)

-- Construct a constant term of a sized type
mkSizedLit
  :: (Type -> Term)
  -- ^ Type constructor?
  -> Type
  -- ^ Result type
  -> Type
  -- ^ forall n.
  -> Integer
  -- ^ KnownNat n
  -> Integer
  -- ^ Value to construct
  -> Term
mkSizedLit conPrim ty nTy kn val =
  mkApps
    (conPrim sTy)
    [ Right nTy
    , Left (Literal (NaturalLiteral kn))
    , Left (Literal (IntegerLiteral val)) ]
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
  mkApps (bConPrim sTy) [ Left (Literal (WordLiteral (msk .&. 1)))
                        , Left (Literal (IntegerLiteral (val .&. 1)))]
  where
    (_,sTy) = splitFunForallTy ty

mkSignedLit, mkUnsignedLit
  :: Type
  -- Result type
  -> Type
  -- forall n.
  -> Integer
  -- KnownNat n
  -> Integer
  -- Value
  -> Term
mkSignedLit    = mkSizedLit signedConPrim
mkUnsignedLit  = mkSizedLit unsignedConPrim

mkBitVectorLit
  :: Type
  -- ^ Result type
  -> Type
  -- ^ forall n.
  -> Integer
  -- ^ KnownNat n
  -> Integer
  -- ^ mask
  -> Integer
  -- ^ Value to construct
  -> Term
mkBitVectorLit ty nTy kn mask val
  = mkApps (bvConPrim sTy)
           [Right nTy
           ,Left (Literal (NaturalLiteral kn))
           ,Left (Literal (NaturalLiteral mask))
           ,Left (Literal (IntegerLiteral val))]
  where
    (_,sTy) = splitFunForallTy ty

mkIndexLitE
  :: Type
  -- ^ Result type
  -> Type
  -- ^ forall n.
  -> Integer
  -- ^ KnownNat n
  -> Integer
  -- ^ Value to construct
  -> Either Term Term
  -- ^ Either undefined (if given value is out of bounds of given type) or term
  -- representing literal
mkIndexLitE rTy nTy kn val
  | val >= 0
  , val < kn
  = Right (mkSizedLit indexConPrim rTy nTy kn val)
  | otherwise
  = Left (undefinedTm (mkTyConApp indexTcNm [nTy]))
  where
    TyConApp indexTcNm _ = tyView (snd (splitFunForallTy rTy))

mkIndexLit
  :: Type
  -- ^ Result type
  -> Type
  -- ^ forall n.
  -> Integer
  -- ^ KnownNat n
  -> Integer
  -- ^ Value to construct
  -> Term
mkIndexLit rTy nTy kn val =
  either id id (mkIndexLitE rTy nTy kn val)

mkBitVectorLit'
  :: (Type, Type, Integer)
  -- ^ (result type, forall n., KnownNat n)
  -> Integer
  -- ^ Mask
  -> Integer
  -- ^ Value
  -> Term
mkBitVectorLit' (ty,nTy,kn) = mkBitVectorLit ty nTy kn

mkIndexLit'
  :: (Type, Type, Integer)
  -- ^ (result type, forall n., KnownNat n)
  -> Integer
  -- ^ value
  -> Term
mkIndexLit' (rTy,nTy,kn) = mkIndexLit rTy nTy kn

boolToIntLiteral :: Bool -> Term
boolToIntLiteral b = if b then Literal (IntLiteral 1) else Literal (IntLiteral 0)

boolToBoolLiteral :: TyConMap -> Type -> Bool -> Term
boolToBoolLiteral tcm ty b =
 let (_,tyView -> TyConApp boolTcNm []) = splitFunForallTy ty
     (Just boolTc) = lookupUniqMap boolTcNm tcm
     [falseDc,trueDc] = tyConDataCons boolTc
     retDc = if b then trueDc else falseDc
 in  Data retDc

charToCharLiteral :: Char -> Term
charToCharLiteral = Literal . CharLiteral

integerToIntLiteral :: Integer -> Term
integerToIntLiteral = Literal . IntLiteral . toInteger . (fromInteger :: Integer -> Int) -- for overflow behavior

integerToWordLiteral :: Integer -> Term
integerToWordLiteral = Literal . WordLiteral . toInteger . (fromInteger :: Integer -> Word) -- for overflow behavior

integerToIntegerLiteral :: Integer -> Term
integerToIntegerLiteral = Literal . IntegerLiteral

naturalToNaturalLiteral :: Natural -> Term
naturalToNaturalLiteral = Literal . NaturalLiteral . toInteger

bConPrim :: Type -> Term
bConPrim (tyView -> TyConApp bTcNm _)
  = Prim (PrimInfo "Clash.Sized.Internal.BitVector.fromInteger##" funTy WorkNever Nothing)
  where
    funTy      = foldr1 mkFunTy [wordPrimTy,integerPrimTy,mkTyConApp bTcNm []]
bConPrim _ = error $ $(curLoc) ++ "called with incorrect type"

bvConPrim :: Type -> Term
bvConPrim (tyView -> TyConApp bvTcNm _)
  = Prim (PrimInfo "Clash.Sized.Internal.BitVector.fromInteger#" (ForAllTy nTV funTy) WorkNever Nothing)
  where
    funTy = foldr1 mkFunTy [naturalPrimTy,naturalPrimTy,integerPrimTy,mkTyConApp bvTcNm [nVar]]
    nName = mkUnsafeSystemName "n" 0
    nVar  = VarTy nTV
    nTV   = mkTyVar typeNatKind nName
bvConPrim _ = error $ $(curLoc) ++ "called with incorrect type"

indexConPrim :: Type -> Term
indexConPrim (tyView -> TyConApp indexTcNm _)
  = Prim (PrimInfo "Clash.Sized.Internal.Index.fromInteger#" (ForAllTy nTV funTy) WorkNever Nothing)
  where
    funTy        = foldr1 mkFunTy [naturalPrimTy,integerPrimTy,mkTyConApp indexTcNm [nVar]]
    nName      = mkUnsafeSystemName "n" 0
    nVar       = VarTy nTV
    nTV        = mkTyVar typeNatKind nName
indexConPrim _ = error $ $(curLoc) ++ "called with incorrect type"

signedConPrim :: Type -> Term
signedConPrim (tyView -> TyConApp signedTcNm _)
  = Prim (PrimInfo "Clash.Sized.Internal.Signed.fromInteger#" (ForAllTy nTV funTy) WorkNever Nothing)
  where
    funTy        = foldr1 mkFunTy [naturalPrimTy,integerPrimTy,mkTyConApp signedTcNm [nVar]]
    nName      = mkUnsafeSystemName "n" 0
    nVar       = VarTy nTV
    nTV        = mkTyVar typeNatKind nName
signedConPrim _ = error $ $(curLoc) ++ "called with incorrect type"

unsignedConPrim :: Type -> Term
unsignedConPrim (tyView -> TyConApp unsignedTcNm _)
  = Prim (PrimInfo "Clash.Sized.Internal.Unsigned.fromInteger#" (ForAllTy nTV funTy) WorkNever Nothing)
  where
    funTy        = foldr1 mkFunTy [naturalPrimTy,integerPrimTy,mkTyConApp unsignedTcNm [nVar]]
    nName        = mkUnsafeSystemName "n" 0
    nVar         = VarTy nTV
    nTV          = mkTyVar typeNatKind nName
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
    in Just $ mkBitVectorLit ty nTy kn (toInteger mask) (toInteger val)
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
  => (sized n -> sized n -> sized n)
  -- ^ function to run
  -> Integer
  -- ^ first  argument
  -> Integer
  -- ^ second argument
  -> (Proxy n -> Integer)
runSizedF f i j _ = toInteger $ f (fromInteger i) (fromInteger j)

extractTySizeInfo :: TyConMap -> Type -> [Type] -> (Type, Type, Integer)
extractTySizeInfo tcm ty tys = (resTy,resSizeTy,resSize)
  where
    ty' = piResultTys tcm ty tys
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
  ty' = piResultTys tcm ty tys
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

splitAtPrim
  :: TyConName
  -- ^ SNat TyCon name
  -> TyConName
  -- ^ Vec TyCon name
  -> Term
splitAtPrim snatTcNm vecTcNm =
  Prim (PrimInfo "Clash.Sized.Vector.splitAt" (splitAtTy snatTcNm vecTcNm) WorkNever Nothing)

splitAtTy
  :: TyConName
  -- ^ SNat TyCon name
  -> TyConName
  -- ^ Vec TyCon name
  -> Type
splitAtTy snatNm vecNm =
  ForAllTy mTV (
  ForAllTy nTV (
  ForAllTy aTV (
  mkFunTy
    (mkTyConApp snatNm [VarTy mTV])
    (mkFunTy
      (mkTyConApp vecNm
                  [mkTyConApp typeNatAdd
                    [VarTy mTV
                    ,VarTy nTV]
                  ,VarTy aTV])
      (mkTyConApp tupNm
                  [mkTyConApp vecNm
                              [VarTy mTV
                              ,VarTy aTV]
                  ,mkTyConApp vecNm
                              [VarTy nTV
                              ,VarTy aTV]])))))
  where
    mTV   = mkTyVar typeNatKind (mkUnsafeSystemName "m" 0)
    nTV   = mkTyVar typeNatKind (mkUnsafeSystemName "n" 1)
    aTV   = mkTyVar liftedTypeKind (mkUnsafeSystemName "a" 2)
    tupNm = ghcTyconToTyConName (tupleTyCon Boxed 2)

foldSplitAtTy
  :: TyConName
  -- ^ Vec TyCon name
  -> Type
foldSplitAtTy vecNm =
  ForAllTy mTV (
  ForAllTy nTV (
  ForAllTy aTV (
  mkFunTy
    naturalPrimTy
    (mkFunTy
      (mkTyConApp vecNm
                  [mkTyConApp typeNatAdd
                    [VarTy mTV
                    ,VarTy nTV]
                  ,VarTy aTV])
      (mkTyConApp tupNm
                  [mkTyConApp vecNm
                              [VarTy mTV
                              ,VarTy aTV]
                  ,mkTyConApp vecNm
                              [VarTy nTV
                              ,VarTy aTV]])))))
  where
    mTV   = mkTyVar typeNatKind (mkUnsafeSystemName "m" 0)
    nTV   = mkTyVar typeNatKind (mkUnsafeSystemName "n" 1)
    aTV   = mkTyVar liftedTypeKind (mkUnsafeSystemName "a" 2)
    tupNm = ghcTyconToTyConName (tupleTyCon Boxed 2)

vecAppendPrim
  :: TyConName
  -- ^ Vec TyCon name
  -> Term
vecAppendPrim vecNm =
  Prim (PrimInfo "Clash.Sized.Vector.++" (vecAppendTy vecNm) WorkNever Nothing)

vecAppendTy
  :: TyConName
  -- ^ Vec TyCon name
  -> Type
vecAppendTy vecNm =
    ForAllTy nTV (
    ForAllTy aTV (
    ForAllTy mTV (
    mkFunTy
      (mkTyConApp vecNm [VarTy nTV
                        ,VarTy aTV
                        ])
      (mkFunTy
         (mkTyConApp vecNm [VarTy mTV
                           ,VarTy aTV
                           ])
         (mkTyConApp vecNm [mkTyConApp typeNatAdd
                              [VarTy nTV
                              ,VarTy mTV]
                           ,VarTy aTV
                           ])))))
  where
    nTV = mkTyVar typeNatKind (mkUnsafeSystemName "n" 0)
    aTV = mkTyVar liftedTypeKind (mkUnsafeSystemName "a" 1)
    mTV = mkTyVar typeNatKind (mkUnsafeSystemName "m" 2)

vecZipWithPrim
  :: TyConName
  -- ^ Vec TyCon name
  -> Term
vecZipWithPrim vecNm =
  Prim (PrimInfo "Clash.Sized.Vector.zipWith" (vecZipWithTy vecNm) WorkNever Nothing)

vecZipWithTy
  :: TyConName
  -- ^ Vec TyCon name
  -> Type
vecZipWithTy vecNm =
  ForAllTy aTV (
  ForAllTy bTV (
  ForAllTy cTV (
  ForAllTy nTV (
  mkFunTy
    (mkFunTy aTy (mkFunTy bTy cTy))
    (mkFunTy
      (mkTyConApp vecNm [nTy,aTy])
      (mkFunTy
        (mkTyConApp vecNm [nTy,bTy])
        (mkTyConApp vecNm [nTy,cTy])))))))
  where
    aTV = mkTyVar liftedTypeKind (mkUnsafeSystemName "a" 0)
    bTV = mkTyVar liftedTypeKind (mkUnsafeSystemName "b" 1)
    cTV = mkTyVar liftedTypeKind (mkUnsafeSystemName "c" 2)
    nTV = mkTyVar typeNatKind (mkUnsafeSystemName "n" 3)
    aTy = VarTy aTV
    bTy = VarTy bTV
    cTy = VarTy cTV
    nTy = VarTy nTV

vecImapGoTy
  :: TyConName
  -- ^ Vec TyCon name
  -> TyConName
  -- ^ Index TyCon name
  -> Type
vecImapGoTy vecTcNm indexTcNm =
  ForAllTy nTV (
  ForAllTy mTV (
  ForAllTy aTV (
  ForAllTy bTV (
  mkFunTy indexTy
    (mkFunTy fTy
       (mkFunTy vecATy vecBTy))))))
  where
    nTV = mkTyVar typeNatKind (mkUnsafeSystemName "n" 0)
    mTV = mkTyVar typeNatKind (mkUnsafeSystemName "m" 1)
    aTV = mkTyVar liftedTypeKind (mkUnsafeSystemName "a" 2)
    bTV = mkTyVar liftedTypeKind (mkUnsafeSystemName "b" 3)
    indexTy = mkTyConApp indexTcNm [nTy]
    nTy = VarTy nTV
    mTy = VarTy mTV
    fTy = mkFunTy indexTy (mkFunTy aTy bTy)
    aTy = VarTy aTV
    bTy = VarTy bTV
    vecATy = mkTyConApp vecTcNm [mTy,aTy]
    vecBTy = mkTyConApp vecTcNm [mTy,bTy]

indexAddTy
  :: TyConName
  -- ^ Index TyCon name
  -> Type
indexAddTy indexTcNm =
  ForAllTy nTV (
  mkFunTy naturalPrimTy (mkFunTy indexTy (mkFunTy indexTy indexTy)))
  where
    nTV     = mkTyVar typeNatKind (mkUnsafeSystemName "n" 0)
    indexTy = mkTyConApp indexTcNm [VarTy nTV]

bvAppendPrim
  :: TyConName
  -- ^ BitVector TyCon Name
  -> Term
bvAppendPrim bvTcNm =
  Prim (PrimInfo "Clash.Sized.Internal.BitVector.++#" (bvAppendTy bvTcNm) WorkNever Nothing)

bvAppendTy
  :: TyConName
  -- ^ BitVector TyCon Name
  -> Type
bvAppendTy bvNm =
  ForAllTy mTV (
  ForAllTy nTV (
  mkFunTy naturalPrimTy (mkFunTy
    (mkTyConApp bvNm [VarTy nTV])
    (mkFunTy
      (mkTyConApp bvNm [VarTy mTV])
      (mkTyConApp bvNm [mkTyConApp typeNatAdd
                          [VarTy nTV
                          ,VarTy mTV]])))))
  where
    mTV = mkTyVar typeNatKind (mkUnsafeSystemName "m" 0)
    nTV = mkTyVar typeNatKind (mkUnsafeSystemName "n" 1)

bvSplitPrim
  :: TyConName
  -- ^ BitVector TyCon Name
  -> Term
bvSplitPrim bvTcNm =
  Prim (PrimInfo "Clash.Sized.Internal.BitVector.split#" (bvSplitTy bvTcNm) WorkNever Nothing)

bvSplitTy
  :: TyConName
  -- ^ BitVector TyCon Name
  -> Type
bvSplitTy bvNm =
  ForAllTy nTV (
  ForAllTy mTV (
  mkFunTy naturalPrimTy (mkFunTy
    (mkTyConApp bvNm [mkTyConApp typeNatAdd
                                 [VarTy mTV
                                 ,VarTy nTV]])
    (mkTyConApp tupNm [mkTyConApp bvNm [VarTy mTV]
                      ,mkTyConApp bvNm [VarTy nTV]]))))
  where
    nTV   = mkTyVar typeNatKind (mkUnsafeSystemName "n" 0)
    mTV   = mkTyVar typeNatKind (mkUnsafeSystemName "m" 1)
    tupNm = ghcTyconToTyConName (tupleTyCon Boxed 2)

ghcTyconToTyConName
  :: TyCon.TyCon
  -> TyConName
ghcTyconToTyConName tc =
    Name User n' (getKey (TyCon.tyConUnique tc)) (getSrcSpan n)
  where
    n'      = fromMaybe "_INTERNAL_" (modNameM n) `Text.append`
              ('.' `Text.cons` Text.pack occName)
    occName = occNameString $ nameOccName n
    n       = TyCon.tyConName tc

svoid :: (State# RealWorld -> State# RealWorld) -> IO ()
svoid m0 = IO (\s -> case m0 s of s' -> (# s', () #))

isTrueDC,isFalseDC :: DataCon -> Bool
isTrueDC dc  = dcUniq dc == getKey trueDataConKey
isFalseDC dc = dcUniq dc == getKey falseDataConKey
