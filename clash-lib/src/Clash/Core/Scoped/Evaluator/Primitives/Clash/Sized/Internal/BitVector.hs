{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "Clash.Sized.Internal.BitVector" in the
  scoped evaluator: ports of
  @Clash.GHC.Evaluator.Primitives.Clash.Sized.Internal.BitVector@. See
  "Clash.Core.Scoped.Evaluator.Primitives" for how to port them.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.BitVector
  ( primitives
  ) where

import Control.Monad.Trans.Except (runExcept)
import Data.Bits
import Data.Proxy (Proxy)
import Data.Reflection (reifyNat)
import Data.Text (Text)
import Data.Text.Extra (showt)
import GHC.TypeLits (KnownNat)

import Clash.Core.Literal (Literal (..))
import qualified Clash.Core.Term as C
import Clash.Promoted.Nat.Unsafe (unsafeSNat)
import qualified Clash.Sized.Internal.BitVector as BitVector
import Clash.Sized.Internal.BitVector (Bit (..), BitVector (..))
import Clash.Util (textNameLit)
import Clash.XException (isX)

import qualified Clash.Sized.Internal.BitVector

import Clash.Core.Scoped.Evaluator.Primitives.Util
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (collectArgs, mkApps)
import Clash.Core.Scoped.Util (undefinedXPrims)

primitives :: [(Text, PrimReduction)]
primitives =
  -- A reduction that evaluates an argument with a nested machine: it
  -- continues in that machine, whose scope extends that of the primitive
  [ primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.xToBV) $ \case
      PrimStepContext{..}
        | isSubj
        , Just (nTy, kn) <- extractKnownNat te sc tys
        -- The second argument to `xToBV` is always going to be suspended.
        -- See Note [Lazy primitives]
        , [ _, (Suspend arg) ] <- args
        -> whnfArg arg $ \mach1 argWHNF ->
             let undefBitVector =
                   Just $ Next mach1
                     { mTerm = mkBitVectorLit (sink ty) (sink nTy) kn (bit (fromInteger kn)-1) 0 }
             in case isX argWHNF of
                  Left _ -> undefBitVector
                  _ -> case collectArgs argWHNF of
                    (Prim p,_) | C.primName p `elem` undefinedXPrims -> undefBitVector
                    _ -> Just $ Next mach1 { mTerm = argWHNF }
      _ -> Nothing

------------
-- BitVector
------------
-- Constructor
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.BV) $ \case
      PrimStepContext{..}
        | [Right _] <- map (runExcept . tyNatSize te sc) tys
        , Just (m,i) <- integerLiterals args
        -> let resTyInfo = extractTySizeInfo te sc ty tys
           in  reduce (mkBitVectorLit' resTyInfo m i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.Bit) $ \case
      PrimStepContext{..}
        | Just (m,i) <- integerLiterals args
        -> reduce (mkBitLit ty m i)
      _ -> Nothing


-- Initialization
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.size#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat te sc tys
        -> reduce (mkIntCLit te IntLiteral kn ty)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.maxIndex#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat te sc tys
        -> reduce (mkIntCLit te IntLiteral (kn-1) ty)
      _ -> Nothing


-- Construction
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.high) $ \case
      PrimStepContext{..}
        -> reduce (mkBitLit ty 0 1)

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.low) $ \case
      PrimStepContext{..}
        -> reduce (mkBitLit ty 0 0)

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.undefined##) $ \case
      PrimStepContext{..}
        -> reduce (mkBitLit ty 1 0)

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.undefined#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat te sc tys
        -> let resTyInfo = extractTySizeInfo te sc ty tys
               mask = bit (fromInteger kn) - 1
           in reduce (mkBitVectorLit' resTyInfo mask 0)
      _ -> Nothing


-- Eq
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.eq##) $ \case
      PrimStepContext{..} | [(0,i),(0,j)] <- bitLiterals args
        -> reduce (boolToBoolLiteral te ty (i == j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.neq##) $ \case
      PrimStepContext{..} | [(0,i),(0,j)] <- bitLiterals args
        -> reduce (boolToBoolLiteral te ty (i /= j))
      _ -> Nothing


-- Ord
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.lt##) $ \case
      PrimStepContext{..} | [(0,i),(0,j)] <- bitLiterals args
        -> reduce (boolToBoolLiteral te ty (i <  j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.ge##) $ \case
      PrimStepContext{..} | [(0,i),(0,j)] <- bitLiterals args
        -> reduce (boolToBoolLiteral te ty (i >= j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.gt##) $ \case
      PrimStepContext{..} | [(0,i),(0,j)] <- bitLiterals args
        -> reduce (boolToBoolLiteral te ty (i >  j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.le##) $ \case
      PrimStepContext{..} | [(0,i),(0,j)] <- bitLiterals args
        -> reduce (boolToBoolLiteral te ty (i <= j))
      _ -> Nothing


-- Enum
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.toEnum##) $ \case
      PrimStepContext{..}
        | [i] <- intCLiterals' args
        -> let Bit msk val = BitVector.toEnum## (fromInteger i)
           in reduce (mkBitLit ty (toInteger msk) (toInteger val))
      _ -> Nothing


-- Bits
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.and##) $ \case
      PrimStepContext{..}
        | [i,j] <- bitLiterals args
        -> let Bit msk val = BitVector.and## (toBit i) (toBit j)
           in reduce (mkBitLit ty (toInteger msk) (toInteger val))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.or##) $ \case
      PrimStepContext{..}
        | [i,j] <- bitLiterals args
        -> let Bit msk val = BitVector.or## (toBit i) (toBit j)
           in reduce (mkBitLit ty (toInteger msk) (toInteger val))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.xor##) $ \case
      PrimStepContext{..}
        | [i,j] <- bitLiterals args
        -> let Bit msk val = BitVector.xor## (toBit i) (toBit j)
           in reduce (mkBitLit ty (toInteger msk) (toInteger val))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.complement##) $ \case
      PrimStepContext{..}
        | [i] <- bitLiterals args
        -> let Bit msk val = BitVector.complement## (toBit i)
           in reduce (mkBitLit ty (toInteger msk) (toInteger val))
      _ -> Nothing


-- Pack
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.pack#) $ \case
      PrimStepContext{..}
        | [(msk,i)] <- bitLiterals args
        -> let resTyInfo = extractTySizeInfo te sc ty tys
           in  reduce (mkBitVectorLit' resTyInfo msk i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.unpack#) $ \case
      PrimStepContext{..}
        | [(msk,i)] <- bitVectorLiterals' args
        -> reduce (mkBitLit ty msk i)
      _ -> Nothing


-- Concatenation
  , primStepEntry $(textNameLit '(Clash.Sized.Internal.BitVector.++#)) $ \case
      PrimStepContext{..} -- :: KnownNat m => BitVector n -> BitVector m -> BitVector (n + m)
        | Just (_,m) <- extractKnownNat te sc tys
        , [(mski,i),(mskj,j)] <- bitVectorLiterals' args
        -> let val = i `shiftL` fromInteger m .|. j
               msk = mski `shiftL` fromInteger m .|. mskj
               resTyInfo = extractTySizeInfo te sc ty tys
           in reduce (mkBitVectorLit' resTyInfo msk val)
      _ -> Nothing


-- Reduction
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.reduceAnd#) $ \case
      PrimStepContext{..} -- :: KnownNat n => BitVector n -> Bit
        | [i] <- bitVectorLiterals' args
        , Just (_, kn) <- extractKnownNat te sc tys
        -> let resTy = getResultTy te sc ty tys
               val = reifyNat kn (op (toBV i))
           in reduce (mkBitLit resTy 0 val)
        where
          op :: KnownNat n => BitVector n -> Proxy n -> Integer
          op u _ = toInteger (BitVector.reduceAnd# u)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.reduceOr#) $ \case
      PrimStepContext{..} -- :: KnownNat n => BitVector n -> Bit
        | [i] <- bitVectorLiterals' args
        , Just (_, kn) <- extractKnownNat te sc tys
        -> let resTy = getResultTy te sc ty tys
               val = reifyNat kn (op (toBV i))
           in reduce (mkBitLit resTy 0 val)
        where
          op :: KnownNat n => BitVector n -> Proxy n -> Integer
          op u _ = toInteger (BitVector.reduceOr# u)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.reduceXor#) $ \case
      PrimStepContext{..} -- :: KnownNat n => BitVector n -> Bit
        | [i] <- bitVectorLiterals' args
        , Just (_, kn) <- extractKnownNat te sc tys
        -> let resTy = getResultTy te sc ty tys
               val = reifyNat kn (op (toBV i))
           in reduce (mkBitLit resTy 0 val)
        where
          op :: KnownNat n => BitVector n -> Proxy n -> Integer
          op u _ = toInteger (BitVector.reduceXor# u)
      _ -> Nothing



-- Indexing
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.index#) $ \case
      PrimStepContext{..} -- :: KnownNat n => BitVector n -> Int -> Bit
        | Just (_,kn,i,j) <- bitVectorLitIntLit te sc tys args
          -> let resTy = getResultTy te sc ty tys
                 (msk,val) = reifyNat kn (op (toBV i) (fromInteger j))
             in reduce (mkBitLit resTy msk val)
          where
            op :: KnownNat n => BitVector n -> Int -> Proxy n -> (Integer,Integer)
            op u i _ = (toInteger m, toInteger v)
              where Bit m v = (BitVector.index# u i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.replaceBit#) $ \case
      PrimStepContext{..} -- :: :: KnownNat n => BitVector n -> Int -> Bit -> BitVector n
        | Just (_, n) <- extractKnownNat te sc tys
        , [ _
          , PrimVal bvP _ [_, Lit (NaturalLiteral mskBv), Lit (IntegerLiteral bv)]
          , valArgs -> Just [Literal (IntLiteral i)]
          , PrimVal bP _ [Lit (WordLiteral mskB), Lit (IntegerLiteral b)]
          ] <- args
        , C.primName bvP == showt 'Clash.Sized.Internal.BitVector.fromInteger#
        , C.primName bP  == showt 'Clash.Sized.Internal.BitVector.fromInteger##
          -> let resTyInfo = extractTySizeInfo te sc ty tys
                 (mskVal,val) = reifyNat n (op (BV (fromInteger mskBv) (fromInteger bv))
                                               (fromInteger i)
                                               (Bit (fromInteger mskB) (fromInteger b)))
          in reduce (mkBitVectorLit' resTyInfo mskVal val)
          where
            op :: KnownNat n => BitVector n -> Int -> Bit -> Proxy n -> (Integer,Integer)
            -- op bv i b _ = (BitVector.unsafeMask res, BitVector.unsafeToInteger res)
            op bv i b _ = splitBV (BitVector.replaceBit# bv i b)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.setSlice#) $ \case
      PrimStepContext{..}
      -- :: SNat (m+1+i) -> BitVector (m + 1 + i) -> SNat m -> SNat n -> BitVector (m + 1 - n) -> BitVector (m + 1 + i)
        | mTy : iTy : nTy : _ <- tys
        , Right m <- runExcept (tyNatSize te sc mTy)
        , Right iN <- runExcept (tyNatSize te sc iTy)
        , Right n <- runExcept (tyNatSize te sc nTy)
        , [i,j] <- bitVectorLiterals' args
        -> let BV msk val = BitVector.setSlice# (unsafeSNat (m+1+iN)) (toBV i) (unsafeSNat m) (unsafeSNat n) (toBV j)
               resTyInfo = extractTySizeInfo te sc ty tys
           in  reduce (mkBitVectorLit' resTyInfo (toInteger msk) (toInteger val))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.slice#) $ \case
      PrimStepContext{..}
      -- :: BitVector (m + 1 + i) -> SNat m -> SNat n -> BitVector (m + 1 - n)
        | mTy : _ : nTy : _ <- tys
        , Right m <- runExcept (tyNatSize te sc mTy)
        , Right n <- runExcept (tyNatSize te sc nTy)
        , [i] <- bitVectorLiterals' args
        -> let BV msk val = BitVector.slice# (toBV i) (unsafeSNat m) (unsafeSNat n)
               resTyInfo = extractTySizeInfo te sc ty tys
           in  reduce (mkBitVectorLit' resTyInfo (toInteger msk) (toInteger val))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.split#) $ \case
      PrimStepContext{..} -- :: forall n m. KnownNat n => BitVector (m + n) -> (BitVector m, BitVector n)
        | nTy : mTy : _ <- tys
        , Right n <-  runExcept (tyNatSize te sc nTy)
        , Right m <-  runExcept (tyNatSize te sc mTy)
        , [(mski,i)] <- bitVectorLiterals' args
        -> let (tupDc, tyArgs) = resultDataCon te sc ty tys
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
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.msb#) $ \case
      PrimStepContext{..} -- :: forall n. KnownNat n => BitVector n -> Bit
        | [i] <- bitVectorLiterals' args
        , Just (_, kn) <- extractKnownNat te sc tys
        -> let resTy = getResultTy te sc ty tys
               (msk,val) = reifyNat kn (op (toBV i))
           in reduce (mkBitLit resTy (toInteger msk) (toInteger val))
        where
          op :: KnownNat n => BitVector n -> Proxy n -> (Word,Word)
          op u _ = (unsafeMask# res, BitVector.unsafeToInteger# res)
            where
              res = BitVector.msb# u
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.lsb#) $ \case
      PrimStepContext{..} -- :: BitVector n -> Bit
        | [i] <- bitVectorLiterals' args
        -> let resTy = getResultTy te sc ty tys
               Bit msk val = BitVector.lsb# (toBV i)
        in reduce (mkBitLit resTy (toInteger msk) (toInteger val))
      _ -> Nothing



-- Eq
  -- eq#, neq# :: KnownNat n => BitVector n -> BitVector n -> Bool
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.eq#) $ \case
      PrimStepContext{..}
        | nTy : _ <- tys
        , Right 0 <- runExcept (tyNatSize te sc nTy)
        -> reduce (boolToBoolLiteral te ty True)
        | Just (_, kn) <- extractKnownNat te sc tys
        , Just val <- reifyNat kn (liftBitVector2Bool BitVector.eq# ty te args)
        -> reduce val
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.neq#) $ \case
      PrimStepContext{..}
        | nTy : _ <- tys
        , Right 0 <- runExcept (tyNatSize te sc nTy)
        -> reduce (boolToBoolLiteral te ty False)
        | Just (_, kn) <- extractKnownNat te sc tys
        , Just val <- reifyNat kn (liftBitVector2Bool BitVector.neq# ty te args)
        -> reduce val
      _ -> Nothing


-- Ord
  -- lt#,ge#,gt#,le# :: KnownNat n => BitVector n -> BitVector n -> Bool
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.lt#) $ \case
      PrimStepContext{..}
        | nTy : _ <- tys
        , Right 0 <- runExcept (tyNatSize te sc nTy)
        -> reduce (boolToBoolLiteral te ty False)
        | Just (_, kn) <- extractKnownNat te sc tys
        , Just val <- reifyNat kn (liftBitVector2Bool BitVector.lt# ty te args)
        -> reduce val
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.ge#) $ \case
      PrimStepContext{..}
        | nTy : _ <- tys
        , Right 0 <- runExcept (tyNatSize te sc nTy)
        -> reduce (boolToBoolLiteral te ty True)
        | Just (_, kn) <- extractKnownNat te sc tys
        , Just val <- reifyNat kn (liftBitVector2Bool BitVector.ge# ty te args)
        -> reduce val
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.gt#) $ \case
      PrimStepContext{..}
        | nTy : _ <- tys
        , Right 0 <- runExcept (tyNatSize te sc nTy)
        -> reduce (boolToBoolLiteral te ty False)
        | Just (_, kn) <- extractKnownNat te sc tys
        , Just val <- reifyNat kn (liftBitVector2Bool BitVector.gt# ty te args)
        -> reduce val
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.le#) $ \case
      PrimStepContext{..}
        | nTy : _ <- tys
        , Right 0 <- runExcept (tyNatSize te sc nTy)
        -> reduce (boolToBoolLiteral te ty True)
        | Just (_, kn) <- extractKnownNat te sc tys
        , Just val <- reifyNat kn (liftBitVector2Bool BitVector.le# ty te args)
        -> reduce val
      _ -> Nothing


-- Enum

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.toEnum#) $ \case
      PrimStepContext{..}
        | let resTyInfo@(_,_,kn) = extractTySizeInfo te sc ty tys
        , Just val <- reifyNat kn (liftInteger2BitVector (BitVector.toEnum# . fromInteger) resTyInfo args)
        -> reduce val
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.fromEnum#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat te sc tys
        , let resTy = getResultTy te sc ty tys
        , Just val <- reifyNat kn (liftBitVector2CInt te resTy (toInteger . BitVector.fromEnum#) args)
        -> reduce val
      _ -> Nothing


-- Bounded
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.minBound#) $ \case
      PrimStepContext{..}
        | Just (nTy,len) <- extractKnownNat te sc tys
        -> reduce (mkBitVectorLit ty nTy len 0 0)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.maxBound#) $ \case
      PrimStepContext{..}
        | Just (litTy,mb) <- extractKnownNat te sc tys
        -> let maxB = (2 ^ mb) - 1
           in  reduce (mkBitVectorLit ty litTy mb 0 maxB)
      _ -> Nothing


-- Num
  , primStepEntry $(textNameLit '(Clash.Sized.Internal.BitVector.+#)) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat te sc tys
        , Just val <- reifyNat kn (liftBitVector2 (BitVector.+#) ty te sc tys args)
        -> reduce val
      _ -> Nothing

  , primStepEntry $(textNameLit '(Clash.Sized.Internal.BitVector.-#)) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat te sc tys
        , Just val <- reifyNat kn (liftBitVector2 (BitVector.-#) ty te sc tys args)
        -> reduce val
      _ -> Nothing

  , primStepEntry $(textNameLit '(Clash.Sized.Internal.BitVector.*#)) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat te sc tys
        , Just val <- reifyNat kn (liftBitVector2 (BitVector.*#) ty te sc tys args)
        -> reduce val
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.negate#) $ \case
      PrimStepContext{..}
        | Just (nTy, kn) <- extractKnownNat te sc tys
        , [i] <- bitVectorLiterals' args
        -> let (msk,val) = reifyNat kn (op (toBV i))
        in reduce (mkBitVectorLit ty nTy kn msk val)
        where
          op :: KnownNat n => BitVector n -> Proxy n -> (Integer,Integer)
          op u _ = splitBV (BitVector.negate# u)
      _ -> Nothing


-- ExtendingNum
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.plus#) $ \case
      PrimStepContext{..} -- :: (KnownNat n, KnownNat m) => BitVector m -> BitVector n -> BitVector (Max m n + 1)
        | [(0,i),(0,j)] <- bitVectorLiterals' args
        -> let (resTy, resSizeTy, resSizeInt) = extractTySizeInfo te sc ty tys
           in  reduce (mkBitVectorLit resTy resSizeTy resSizeInt 0 (i+j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.minus#) $ \case
      PrimStepContext{..}
        | [(0,i),(0,j)] <- bitVectorLiterals' args
        -> let (resTy, resSizeTy, resSizeInt) = extractTySizeInfo te sc ty tys
               val = reifyNat resSizeInt (runSizedF (BitVector.-#) i j)
          in  reduce (mkBitVectorLit resTy resSizeTy resSizeInt 0 val)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.times#) $ \case
      PrimStepContext{..}
        | [(0,i),(0,j)] <- bitVectorLiterals' args
        -> let (resTy, resSizeTy, resSizeInt) = extractTySizeInfo te sc ty tys
           in  reduce (mkBitVectorLit resTy resSizeTy resSizeInt 0 (i*j))
      _ -> Nothing


-- Integral
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.quot#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat te sc tys
        , Just val <- reifyNat kn (liftBitVector2 (BitVector.quot#) ty te sc tys args)
        -> reduce $ catchDivByZero val
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.rem#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat te sc tys
        , Just val <- reifyNat kn (liftBitVector2 (BitVector.rem#) ty te sc tys args)
        -> reduce $ catchDivByZero val
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.toInteger#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat te sc tys
        , [i] <- bitVectorLiterals' args
        -> let val = reifyNat kn (op (toBV i))
        in reduce (integerToIntegerLiteral val)
        where
          op :: KnownNat n => BitVector n -> Proxy n -> Integer
          op u _ = BitVector.toInteger# u
      _ -> Nothing


-- Bits
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.and#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat te sc tys
        , Just val <- reifyNat kn (liftBitVector2 (BitVector.and#) ty te sc tys args)
        -> reduce val
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.or#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat te sc tys
        , Just val <- reifyNat kn (liftBitVector2 (BitVector.or#) ty te sc tys args)
        -> reduce val
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.xor#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat te sc tys
        , Just val <- reifyNat kn (liftBitVector2 (BitVector.xor#) ty te sc tys args)
        -> reduce val
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.complement#) $ \case
      PrimStepContext{..}
        | [i] <- bitVectorLiterals' args
        , Just (nTy, kn) <- extractKnownNat te sc tys
        -> let (msk,val) = reifyNat kn (op (toBV i))
        in reduce (mkBitVectorLit ty nTy kn msk val)
        where
          op :: KnownNat n => BitVector n -> Proxy n -> (Integer,Integer)
          op u _ = splitBV $ BitVector.complement# u
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.shiftL#) $ \case
      PrimStepContext{..}
        | Just (nTy,kn,i,j) <- bitVectorLitIntLit te sc tys args
          -> let (msk,val) = reifyNat kn (op (toBV i) (fromInteger j))
          in reduce (mkBitVectorLit ty nTy kn msk val)
          where
            op :: KnownNat n => BitVector n -> Int -> Proxy n -> (Integer,Integer)
            op u i _ = splitBV (BitVector.shiftL# u i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.shiftR#) $ \case
      PrimStepContext{..}
        | Just (nTy,kn,i,j) <- bitVectorLitIntLit te sc tys args
          -> let (msk,val) = reifyNat kn (op (toBV i) (fromInteger j))
          in reduce (mkBitVectorLit ty nTy kn msk val)
          where
            op :: KnownNat n => BitVector n -> Int -> Proxy n -> (Integer,Integer)
            op u i _ = splitBV (BitVector.shiftR# u i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.rotateL#) $ \case
      PrimStepContext{..}
        | Just (nTy,kn,i,j) <- bitVectorLitIntLit te sc tys args
          -> let (msk,val) = reifyNat kn (op (toBV i) (fromInteger j))
          in reduce (mkBitVectorLit ty nTy kn msk val)
          where
            op :: KnownNat n => BitVector n -> Int -> Proxy n -> (Integer,Integer)
            op u i _ = splitBV (BitVector.rotateL# u i)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.rotateR#) $ \case
      PrimStepContext{..}
        | Just (nTy,kn,i,j) <- bitVectorLitIntLit te sc tys args
          -> let (msk,val) = reifyNat kn (op (toBV i) (fromInteger j))
          in reduce (mkBitVectorLit ty nTy kn msk val)
          where
            op :: KnownNat n => BitVector n -> Int -> Proxy n -> (Integer,Integer)
            op u i _ = splitBV (BitVector.rotateR# u i)
      _ -> Nothing


-- truncateB
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.truncateB#) $ \case
      PrimStepContext{..} -- forall a b . KnownNat a => BitVector (a + b) -> BitVector a
        | aTy  : _ <- tys
        , Right ka <- runExcept (tyNatSize te sc aTy)
        , [(mski,i)] <- bitVectorLiterals' args
        -> let bitsKeep = (bit (fromInteger ka)) - 1
               val = i .&. bitsKeep
               msk = mski .&. bitsKeep
        in reduce (mkBitVectorLit ty aTy ka msk val)
      _ -> Nothing

  ]
