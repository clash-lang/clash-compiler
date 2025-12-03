{-# LANGUAGE MagicHash, UnboxedTuples, CPP, PatternSynonyms #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Clash.Util.Supply
-- Copyright   :  (C) 2011-2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A fast unique identifier supply with local pooling and replay
-- support.
--
-- One often has a desire to generate a bunch of integer identifiers within
-- a single process that are unique within that process. You could use
-- UUIDs, but they can be expensive to generate; you don't want to have
-- your threads contending for a single external counter if the identifier
-- is not going to be used outside the process.
--
-- @concurrent-supply@ builds a tree-like structure which can be split; you
-- can make smaller unique supplies and then you allocate from your supplies
-- locally. Internally it pulls from a unique supply one block at a time as
-- you walk into parts of the tree that haven't been explored.
--
----------------------------------------------------------------------------
module Clash.Util.Supply
  ( Supply
  -- * Variables
  , newSupply
  , freshId
  , splitSupply
  -- * Unboxed API
  , freshId#
  , splitSupply#
  ) where

import Data.Hashable
import Data.IORef
import GHC.IO (unsafeDupablePerformIO, unsafePerformIO)

import Clash.Unique (Unique, Unique#, pattern Unique#)

infixr 5 :-
data Stream a = a :- Stream a

instance Functor Stream where
  fmap f (a :- as) = f a :- fmap f as

extract :: Stream a -> a
extract (a :- _) = a

units :: Stream ()
units = () :- units
{-# NOINLINE units #-}

data Block = Block Unique !(Stream Block)

instance Eq Block where
  Block a (Block b _ :- _) == Block c (Block d _ :- _) = a == c && b == d

instance Ord Block where
  Block a (Block b _ :- _) `compare` Block c (Block d _ :- _) = compare a c `mappend` compare b d

instance Show Block where
  showsPrec d (Block a (Block b _ :- _)) = showParen (d >= 10) $
    showString "Block " . showsPrec 10 a . showString " (Block " . showsPrec 10 b . showString " ... :- ...)"

instance Hashable Block where
  hashWithSalt s (Block a (Block b _ :- _)) = s `hashWithSalt` a `hashWithSalt` b

blockSize :: Unique
blockSize = 1024
{-# INLINE blockSize #-}

-- Minimum size to be worth splitting a supply rather than just CAS'ing twice to avoid multiple subsequent biased splits
minSplitSupplySize :: Unique
minSplitSupplySize = 32 -- based on sqrt blockSize
{-# INLINE minSplitSupplySize #-}

blockCounter :: IORef Unique
blockCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE blockCounter #-}

modifyBlock :: a -> IO Unique
modifyBlock _ = atomicModifyIORef blockCounter $ \ i -> let i' = i + blockSize in i' `seq` (i', i)
{-# NOINLINE modifyBlock #-}

gen :: a -> Block
gen x = Block (unsafeDupablePerformIO (modifyBlock x)) (gen <$> units)
{-# NOINLINE gen #-}

newBlock :: IO Block
newBlock = return $! gen ()
{-# NOINLINE newBlock #-}

splitBlock# :: Block -> (# Block, Block #)
splitBlock# (Block i (x :- xs)) = (# x, Block i xs #)
{-# INLINE splitBlock# #-}

-- | A user managed globally unique variable supply.
data Supply = Supply {-# UNPACK #-} !Unique {-# UNPACK #-} !Unique Block
  deriving (Eq,Ord,Show)

instance Hashable Supply where
  hashWithSalt s (Supply i j b) = s `hashWithSalt` i `hashWithSalt` j `hashWithSalt` b

blockSupply :: Block -> Supply
blockSupply (Block i bs) = Supply i (i + blockSize - 1) (extract bs)
{-# INLINE blockSupply #-}

-- | Grab a new supply. Any two supplies obtained with newSupply are guaranteed to return
-- disjoint sets of identifiers. Replaying the same sequence of operations on the same
-- Supply will yield the same results.
newSupply :: IO Supply
newSupply = blockSupply <$> newBlock
{-# INLINE newSupply #-}

-- | Obtain a fresh Id from a Supply.
freshId :: Supply -> (Unique, Supply)
freshId s = case freshId# s of
  (# i, s' #) -> (Unique# i, s')
{-# INLINE freshId #-}

-- | Split a supply into two supplies that will return disjoint identifiers
splitSupply :: Supply -> (Supply, Supply)
splitSupply s = case splitSupply# s of
  (# l, r #) -> (l, r)
{-# INLINE splitSupply #-}

-- | An unboxed version of freshId
freshId# :: Supply -> (# Unique#, Supply #)
freshId# (Supply i@(Unique# i#) j b)
  | i /= j = (# i#, Supply (i + 1) j b #)
  | otherwise = (# i#, blockSupply b #)
{-# INLINE freshId# #-}

-- | An unboxed version of splitSupply
splitSupply# :: Supply -> (# Supply, Supply #)
splitSupply# (Supply i k b) = case splitBlock# b of
    (# bl, br #)
      | k - i >= minSplitSupplySize
      , j <- i + div (k - i) 2 ->
        (# Supply i j bl, Supply (j + 1) k br #)
      | Block x (l :- r :- _) <- bl
      , y <- x + div blockSize 2
      , z <- x + blockSize - 1 ->
        (# Supply x (y - 1) l, Supply y z r #)
{-# INLINE splitSupply# #-}
