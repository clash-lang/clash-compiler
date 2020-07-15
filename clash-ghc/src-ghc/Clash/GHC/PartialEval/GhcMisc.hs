{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.GhcMisc
  ( ghcPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)
import GHC.Classes
import GHC.Natural (naturalFromInteger)
import GHC.Prim
import GHC.Types

import Clash.Core.Evaluator.Models

import Clash.GHC.PartialEval.Internal

ghcPrims :: HashMap Text PrimImpl
ghcPrims = HashMap.fromList
  [ ("_CO_", liftId)
  , ("_TY_", liftId)
  , ("GHC.Classes.&&", liftLazyBinary (&&) False)
  , ("GHC.Classes.||", liftLazyBinary (||) True)
  , ("GHC.Classes.geInt", liftBinary geInt)
  , ("GHC.Classes.divInt#", liftBinary# divInt#)
  , ("GHC.Classes.modInt#", liftBinary# modInt#)
  , ("GHC.Classes.not", liftUnary not)
  , ("GHC.CString.unpackCString#", liftId)
  , ("GHC.Prim.realWorld#", liftId)
  , ("GHC.Real.^_f", primF)
  , ("GHC.Real.$wf", primWf)
  , ("GHC.Real.$wf1", primWf1)
  , ("GHC.TypeLits.natVal", primNatValInteger)
  , ("GHC.TypeNats.natVal", primNatValNatural)
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
primNatValInteger _ p args
  | [Right n, Right _proxy, Left _x] <- args
  = do sz <- typeSize n Nothing
       resTy <- resultType p args
       toValue sz resTy

  | otherwise
  = empty

primNatValNatural :: PrimImpl
primNatValNatural _ p args
  | [Right n, Right _proxy, Left _x] <- args
  = do sz <- typeSize n Nothing
       resTy <- resultType p args
       toValue (naturalFromInteger sz) resTy

  | otherwise
  = empty

liftLazyBinary :: (Bool -> Bool -> Bool) -> Bool -> PrimImpl
liftLazyBinary f dom e p args
  | [Left x, Left y] <- args
  = lazyIn x y <|> lazyIn y x <|> liftBinary f e p args

  | otherwise
  = empty
 where
  lazyIn x y = do
    a <- fromTerm x
    resTy <- resultType p args
    if a == dom
      then toValue dom resTy
      else VThunk y <$> lift getLocalEnv

liftBinary# :: (Int# -> Int# -> Int#) -> PrimImpl
liftBinary# f =
  liftBinary $ \x y ->
    let !(UInt (I# a)) = x
        !(UInt (I# b)) = y
     in UInt (I# (f a b))

