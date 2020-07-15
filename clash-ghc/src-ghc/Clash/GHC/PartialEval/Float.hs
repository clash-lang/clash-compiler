{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.PartialEval.Float
  ( floatPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)
import GHC.Prim
import GHC.Types hiding (Type)

import Clash.GHC.PartialEval.Internal

floatPrims :: HashMap Text PrimImpl
floatPrims = HashMap.fromList
  [ ("GHC.Prim.gtFloat#", floatComparison gtFloat#)
  , ("GHC.Prim.geFloat#", floatComparison geFloat#)
  , ("GHC.Prim.eqFloat#", floatComparison eqFloat#)
  , ("GHC.Prim.neFloat#", floatComparison neFloat#)
  , ("GHC.Prim.ltFloat#", floatComparison ltFloat#)
  , ("GHC.Prim.leFloat#", floatComparison leFloat#)
  , ("GHC.Prim.plusFloat#", liftBinary# plusFloat#)
  , ("GHC.Prim.minusFloat#", liftBinary# minusFloat#)
  , ("GHC.Prim.timesFloat#", liftBinary# timesFloat#)
  , ("GHC.Prim.divideFloat#", withExceptions (liftBinary# divideFloat#))
  , ("GHC.Prim.negateFloat#", liftUnary# negateFloat#)
  , ("GHC.Prim.fabsFloat#", liftUnary# fabsFloat#)
  , ("GHC.Prim.float2Int#", primFloat2Int)
  , ("GHC.Prim.expFloat#", liftUnary# expFloat#)
  , ("GHC.Prim.logFloat#", withExceptions (liftUnary# logFloat#))
  , ("GHC.Prim.sqrtFloat#", liftUnary# sqrtFloat#)
  , ("GHC.Prim.sinFloat#", liftUnary# sinFloat#)
  , ("GHC.Prim.cosFloat#", liftUnary# cosFloat#)
  , ("GHC.Prim.tanFloat#", liftUnary# tanFloat#)
  , ("GHC.Prim.asinFloat#", withExceptions (liftUnary# asinFloat#))
  , ("GHC.Prim.acosFloat#", withExceptions (liftUnary# acosFloat#))
  , ("GHC.Prim.atanFloat#", liftUnary# atanFloat#)
  , ("GHC.Prim.sinhFloat#", liftUnary# sinhFloat#)
  , ("GHC.Prim.coshFloat#", liftUnary# coshFloat#)
  , ("GHC.Prim.tanhFloat#", liftUnary# tanhFloat#)

#if MIN_VERSION_ghc(8,7,0)
  , ("GHC.Prim.asinhFloat#", liftUnary# asinhFloat#)
  , ("GHC.Prim.acoshFloat#", liftUnary# acoshFloat#)
  , ("GHC.Prim.atanhFloat#", liftUnary# atanhFloat#)
#endif

#if MIN_VERSION_base(4,12,0)
  , ("GHC.Float.$w$casinh1", primAsinhSpecialized)
#endif

  , ("GHC.Prim.powerFloat#", liftBinary# powerFloat#)
  , ("GHC.Prim.float2Double#", primFloat2Double)
  , ("GHC.Prim.decodeFloat_Int#", primDecodeFloat_Int)
  , ("GHC.Types.F#", liftBox)
  ]

primFloat2Int :: PrimImpl
primFloat2Int =
  liftUnary $ \x ->
    let !(UFloat (F# a)) = x in UInt (I# (float2Int# a))

#if MIN_VERSION_base(4,12,0)
primAsinhSpecialized :: PrimImpl
primAsinhSpecialized =
  liftUnary# $ \i ->
    let !(F# a) = asinh (F# i) in a
#endif

primFloat2Double :: PrimImpl
primFloat2Double =
  liftUnary $ \x ->
    let !(UFloat (F# a)) = x in UDouble (D# (float2Double# a))

primDecodeFloat_Int :: PrimImpl
primDecodeFloat_Int =
  liftUnary $ \x ->
    let !(UFloat (F# a)) = x
        !(# b, c #) = decodeFloat_Int# a
     in UTuple2 (UInt (I# b), UInt (I# c))

liftUnary# :: (Float# -> Float#) -> PrimImpl
liftUnary# f =
  liftUnary $ \x ->
    let !(UFloat (F# a)) = x in UFloat (F# (f a))

liftBinary# :: (Float# -> Float# -> Float#) -> PrimImpl
liftBinary# f =
  liftBinary $ \x y ->
    let !(UFloat (F# a)) = x
        !(UFloat (F# b)) = y
     in UFloat (F# (f a b))

floatComparison :: (Float# -> Float# -> Int#) -> PrimImpl
floatComparison f =
  liftBinary $ \x y ->
    let !(UFloat (F# a)) = x
        !(UFloat (F# b)) = y
     in UInt (I# (f a b))

