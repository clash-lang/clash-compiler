{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2021,      QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Class.BitPack.BitReduction where

import Clash.Class.BitPack.Internal   (BitPack (..))
import Clash.Sized.Internal.BitVector (Bit, reduceAnd#, reduceOr#, reduceXor#)

{- $setup
>>> :set -XDataKinds
>>> import Clash.Prelude
-}

{-# INLINE reduceAnd #-}
-- | Are all bits set to '1'?
--
-- >>> pack (-2 :: Signed 6)
-- 0b11_1110
-- >>> reduceAnd (-2 :: Signed 6)
-- 0
-- >>> pack (-1 :: Signed 6)
-- 0b11_1111
-- >>> reduceAnd (-1 :: Signed 6)
-- 1
--
-- Zero width types will evaluate to '1':
--
-- >>> reduceAnd (0 :: Unsigned 0)
-- 1
reduceAnd :: BitPack a => a -> Bit
reduceAnd v = reduceAnd# (pack v)

{-# INLINE reduceOr #-}
-- | Is there at least one bit set to '1'?
--
-- >>> pack (5 :: Signed 6)
-- 0b00_0101
-- >>> reduceOr (5 :: Signed 6)
-- 1
-- >>> pack (0 :: Signed 6)
-- 0b00_0000
-- >>> reduceOr (0 :: Signed 6)
-- 0
--
-- Zero width types will evaluate to '0':
--
-- >>> reduceOr (0 :: Unsigned 0)
-- 0
reduceOr :: BitPack a => a -> Bit
reduceOr v = reduceOr# (pack v)

{-# INLINE reduceXor #-}
-- | Is the number of bits set to '1' uneven?
--
-- >>> pack (5 :: Signed 6)
-- 0b00_0101
-- >>> reduceXor (5 :: Signed 6)
-- 0
-- >>> pack (28 :: Signed 6)
-- 0b01_1100
-- >>> reduceXor (28 :: Signed 6)
-- 1
-- >>> pack (-5 :: Signed 6)
-- 0b11_1011
-- >>> reduceXor (-5 :: Signed 6)
-- 1
--
-- Zero width types will evaluate to '0':
--
-- >>> reduceXor (0 :: Unsigned 0)
-- 0
reduceXor :: BitPack a => a -> Bit
reduceXor v = reduceXor# (pack v)
