{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.Primitive.GhcMisc
  ( ghcPrims
  ) where

import Control.Monad.Catch (throwM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)
import GHC.Base (eqString)
import GHC.Classes
import GHC.Natural (naturalFromInteger)
import GHC.Prim
import GHC.Types

import Clash.Core.PartialEval.Monad
import Clash.Core.PartialEval.NormalForm
import Clash.Core.TyCon (tyConDataCons)
import Clash.Core.Type
import Clash.Core.TysPrim
import Clash.Unique (lookupUniqMap)

import Clash.GHC.PartialEval.Primitive.FromAst
import Clash.GHC.PartialEval.Primitive.Info
import Clash.GHC.PartialEval.Primitive.Strategy
import Clash.GHC.PartialEval.Primitive.ToAst
import Clash.GHC.PartialEval.Primitive.Unboxed

ghcPrims :: HashMap Text PrimImpl
ghcPrims = HashMap.fromList
  [ ("Control.Exception.Base.absentError", liftUndefined)
  , ("Control.Exception.Base.patError", liftUndefined)
  , ("Debug.Trace.trace", liftId)
  , ("GHC.Base.++", liftId)
  , ("GHC.Base.eqString", liftBinary eqString)
  , ("GHC.CString.unpackCString#", liftId)
  , ("GHC.CString.unpackAppendCString#", liftId)
  , ("GHC.Classes.&&", liftLazyBinary (&&) False)
  , ("GHC.Classes.divInt#", liftBinary# divInt#)
  , ("GHC.Classes.eqInt", coreUnfolding)
  , ("GHC.Classes.geInt", coreUnfolding)
  , ("GHC.Classes.gtInt", coreUnfolding)
  , ("GHC.Classes.leInt", coreUnfolding)
  , ("GHC.Classes.ltInt", coreUnfolding)
  , ("GHC.Classes.neInt", coreUnfolding)
  , ("GHC.Classes.modInt#", liftBinary# modInt#)
  , ("GHC.Classes.not", liftUnary not)
  , ("GHC.Classes.||", liftLazyBinary (||) True)
  , ("GHC.Err.error", liftUndefined)
  , ("GHC.Err.errorWithoutStackTrace", liftUndefined)
  , ("GHC.Prim.realWorld#", liftId)
  , ("GHC.Prim.void#", liftId)
  , ("GHC.Real.divZeroError", liftUndefined)
  , ("GHC.Real.overflowError", liftUndefined)
  , ("GHC.Real.ratioZeroDenominatorError", liftUndefined)
  , ("GHC.Real.underflowError", liftUndefined)
  , ("GHC.Real.$wf", primWf)
  , ("GHC.Real.$wf1", primWf1)
  , ("GHC.Real.^_f", primF)
  , ("GHC.Show.$witos'", liftId)
  , ("GHC.TypeLits.natVal", primNatValInteger)
  , ("GHC.TypeNats.natVal", primNatValNatural)
  , ("Unsafe.Coerce.unsafeEqualityProof", primUnsafeEqualityProof)
  ]

primF :: PrimImpl
primF =
  liftBinary $ \(x :: Integer) (y :: Integer) -> x ^ y

primWf :: PrimImpl
primWf =
  liftBinary $ \(x :: Integer) y ->
    let !(UInt a) = y in x ^ a

primWf1 :: PrimImpl
primWf1 =
  liftBinary $ \x y ->
    let !(UInt a) = x
        !(UInt b) = y
     in UInt (a ^ b)

primNatValInteger :: PrimImpl
primNatValInteger pr args
  | [Right n, Right _, Left _] <- args
  = do szN <- typeSize n Nothing
       resTy <- resultType pr args
       toValue szN resTy

  | otherwise
  = throwM (UnexpectedArgs pr args)

primNatValNatural :: PrimImpl
primNatValNatural pr args
  | [Right n, Right _, Left _] <- args
  = do szN <- typeSize n Nothing
       resTy <- resultType pr args
       toValue (naturalFromInteger szN) resTy

  | otherwise
  = throwM (UnexpectedArgs pr args)

primUnsafeEqualityProof :: PrimImpl
primUnsafeEqualityProof pr args
  | [Right kn, Right a, Right b] <- args
  = do resTy <- resultType pr args
       let Just tcn = fst <$> splitTyConAppM resTy
       Just tc <- lookupUniqMap tcn <$> getTyConMap
       let [dc] = tyConDataCons tc

       -- The types 'a' and 'b' appear flipped in the core output.
       let Just eqTcn = fst <$> splitTyConAppM eqPrimTy
       let eq = mkTyConApp eqTcn [kn, kn, b, a]
       VData dc [Right eq] <$> getLocalEnv

  | otherwise
  = throwM (UnexpectedArgs pr args)

liftLazyBinary :: (Bool -> Bool -> Bool) -> Bool -> PrimImpl
liftLazyBinary f dom pr args
  | [Left x, Left y] <- args
  = lazyIn x y <|> lazyIn y x <|> liftBinary f pr args

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  lazyIn x y = do
    a <- fromValueForce x
    resTy <- resultType pr args
    if a == dom
      then toValue dom resTy
      else pure y

liftBinary# :: (Int# -> Int# -> Int#) -> PrimImpl
liftBinary# f =
  liftBinary $ \x y ->
    let !(UInt (I# a)) = x
        !(UInt (I# b)) = y
     in UInt (I# (f a b))
