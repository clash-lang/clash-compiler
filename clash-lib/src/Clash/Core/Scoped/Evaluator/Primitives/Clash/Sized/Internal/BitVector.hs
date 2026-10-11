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

module Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.BitVector
  ( primitives
  ) where

import Control.Monad.Trans.Except (runExcept)
import Data.Bits
import Data.Proxy (Proxy)
import Data.Reflection (reifyNat)
import Data.Text (Text)
import GHC.TypeLits (KnownNat)

import Clash.Core.Literal (Literal (..))
import qualified Clash.Core.Term as C
import qualified Clash.Sized.Internal.BitVector as BitVector
import Clash.Util (textNameLit)
import Clash.XException (isX)

import qualified Clash.Sized.Internal.BitVector

import Clash.Core.Scoped.Evaluator.Primitives.Util
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (collectArgs)
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

-- Initialization
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.size#) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat te sc tys
        -> reduce (mkIntCLit te IntLiteral kn ty)
      _ -> Nothing

-- Eq
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.eq##) $ \case
      PrimStepContext{..} | [(0,i),(0,j)] <- bitLiterals args
        -> reduce (boolToBoolLiteral te ty (i == j))
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.eq#) $ \case
      PrimStepContext{..}
        | nTy : _ <- tys
        , Right 0 <- runExcept (tyNatSize te sc nTy)
        -> reduce (boolToBoolLiteral te ty True)
        | Just (_, kn) <- extractKnownNat te sc tys
        , Just val <- reifyNat kn (liftBitVector2Bool BitVector.eq# ty te args)
        -> reduce val
      _ -> Nothing

-- Pack
  , primStepEntry $(textNameLit 'Clash.Sized.Internal.BitVector.pack#) $ \case
      PrimStepContext{..}
        | [(msk,i)] <- bitLiterals args
        -> let resTyInfo = extractTySizeInfo te sc ty tys
           in  reduce (mkBitVectorLit' resTyInfo msk i)
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
          op :: KnownNat n => BitVector.BitVector n -> Proxy n -> Integer
          op u _ = toInteger (BitVector.reduceAnd# u)
      _ -> Nothing

-- Num
  , primStepEntry $(textNameLit '(Clash.Sized.Internal.BitVector.+#)) $ \case
      PrimStepContext{..}
        | Just (_, kn) <- extractKnownNat te sc tys
        , Just val <- reifyNat kn (liftBitVector2 (BitVector.+#) ty te sc tys args)
        -> reduce val
      _ -> Nothing
  ]
