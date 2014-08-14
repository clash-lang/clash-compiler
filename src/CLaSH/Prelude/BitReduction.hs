{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash        #-}

module CLaSH.Prelude.BitReduction where

import GHC.TypeLits                   (KnownNat, natVal)

import CLaSH.Class.BitConvert         (BitConvert (..))
import CLaSH.Sized.Internal.BitVector (Bit, reduceAnd#, reduceOr#, reduceXor#)

reduceAnd :: (BitConvert a, KnownNat (BitSize a)) => a -> Bit
reduceAnd v = reduceAnd# (natVal v') v'
  where
    v' = pack v

reduceOr :: BitConvert a => a -> Bit
reduceOr v = reduceOr# (pack v)

reduceXor :: BitConvert a => a -> Bit
reduceXor v = reduceXor# (pack v)
