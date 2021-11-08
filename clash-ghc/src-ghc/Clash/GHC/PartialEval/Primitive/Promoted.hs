{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.Primitive.Promoted
  ( promotedPrims
  ) where

import Control.Monad.Catch (throwM)
import Data.Bits (shiftL)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Proxy (Proxy)
import Data.Reflection (reifyNat)
import Data.Text (Text)
import GHC.TypeLits (KnownNat, type (^), natVal)
import GHC.TypeLits.Extra

import Clash.Promoted.Nat (SNat)
import Clash.Promoted.Nat.Unsafe

import Clash.Core.PartialEval.Monad (Eval, EvalException(..))
import Clash.Core.PartialEval.NormalForm (Value)
import Clash.Util (clogBase, flogBase)

import Clash.GHC.PartialEval.Primitive.Info
import Clash.GHC.PartialEval.Primitive.Strategy
import Clash.GHC.PartialEval.Primitive.ToAst

promotedPrims :: HashMap Text PrimImpl
promotedPrims = HashMap.fromList
  [ ("Clash.Promoted.Nat.clogBaseSNat", primClogBaseSNat)
  , ("Clash.Promoted.Nat.flogBaseSNat", primFlogBaseSNat)
    -- This is safe, as GHC has already typechecked it for us.
  , ("Clash.Promoted.Nat.logBaseSNat", primFlogBaseSNat)
  , ("Clash.Promoted.Nat.powSNat", primPowSNat)
  , ("Clash.Promoted.Symbol.SSymbol", liftId)
  ]

-- Note: Only the type is needed to compute this function,
-- so it is lazy in all non-type term arguments.
--
primClogBaseSNat :: PrimImpl
primClogBaseSNat pr args
  | [Right baseTy, Right xTy, Left _, Left _, Left _, Left _] <- args
  = do szBase <- typeSize baseTy Nothing
       szX <- typeSize xTy Nothing
       reifyNat szBase (\pBase -> reifyNat szX (\pX -> go pBase pX))

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  go :: forall base x. (KnownNat base, KnownNat x)
     => Proxy base -> Proxy x -> Eval Value
  go pBase pX = do
    resTy <- resultType pr args

    case clogBase (natVal pBase) (natVal pX) of
      Just n ->
        let result = unsafeSNat (toInteger n)
         in toValue @(SNat (CLog base x)) result resTy

      Nothing ->
        throwM ResultUndefined

-- Note: Only the type is needed to compute this function,
-- so it is lazy in all non-type term arguments.
--
primFlogBaseSNat :: PrimImpl
primFlogBaseSNat pr args
  | [Right baseTy, Right xTy, Left _, Left _, Left _, Left _] <- args
  = do szBase <- typeSize baseTy Nothing
       szX <- typeSize xTy Nothing
       reifyNat szBase (\pBase -> reifyNat szX (\pX -> go pBase pX))

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  go :: forall base x. (KnownNat base, KnownNat x)
     => Proxy base -> Proxy x -> Eval Value
  go pBase pX = do
    resTy <- resultType pr args

    case flogBase (natVal pBase) (natVal pX) of
      Just n ->
        let result = unsafeSNat (toInteger n)
         in toValue @(SNat (FLog base x)) result resTy

      Nothing ->
        throwM ResultUndefined

primPowSNat :: PrimImpl
primPowSNat pr args
  | [Right aTy, Right bTy, Left _, Left _, Left _] <- args
  = do szA <- typeSize aTy Nothing
       szB <- typeSize bTy Nothing
       reifyNat szA (\pA -> reifyNat szB (\pB -> go pA pB))

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  go :: forall a b. (KnownNat a, KnownNat b)
     => Proxy a -> Proxy b -> Eval Value
  go pA pB = do
    resTy <- resultType pr args

    case natVal pA of
      2 -> let result = unsafeSNat (1 `shiftL` fromInteger (natVal pB))
            in toValue @(SNat (a ^ b)) result resTy

      a -> let result = unsafeSNat (a ^ natVal pB)
            in toValue @(SNat (a ^ b)) result resTy
