{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module T3291.BitPackC (packC, maybeUnpackC, BitPackC (..)) where

import Clash.Prelude

maybeUnpackVec ::
  (KnownNat n) =>
  BitVector (n * 8) -> Maybe (Vec n (BitVector 8))
maybeUnpackVec = sequenceA . map Just . unconcatBitVector#

packC :: (BitPackC a) => a -> Vec (ByteSizeC a) (BitVector 8)
packC = fromJustX . maybeUnpackVec . packC#

fromEndianBV ::
  (KnownNat n) => BitVector (n * 8) -> BitVector (n * 8)
fromEndianBV = pack . reverse . fromJustX . maybeUnpackVec

class (KnownNat (ByteSizeC a)) => BitPackC (a :: Type) where
  type ByteSizeC (a :: Type) :: Nat

  packC# :: a -> BitVector (ByteSizeC a * 8)

instance (KnownNat n) => BitPackC (Unsigned n) where
  type ByteSizeC (Unsigned n) = 2 ^ CLog 2 (Max 1 (DivRU n 8))

  packC# val = fromEndianBV (resize (pack val))

-- Dead, but removing this triggers GHC opts (?) that make the reproduce fail to
-- reproduce.
maybeUnpackC ::
  (KnownNat n) =>
  Unsigned n -> BitVector (ByteSizeC (Unsigned n) * 8) -> Maybe (Unsigned n)
maybeUnpackC _ bv = Just (unpack (resize (fromEndianBV bv)))
