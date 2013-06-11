{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -O0 -fno-omit-interface-pragmas -fno-expose-all-unfoldings #-}

module CLaSH.Prelude
  ( module Exported
  , module CLaSH.Prelude
  )
where

import Control.Arrow
import Control.Applicative
import Control.Category      as Category
import Data.Bits             as Exported
import CLaSH.Class.BitVector as Exported
import CLaSH.Class.Default   as Exported
import CLaSH.Promoted.Bool   as Exported
import CLaSH.Promoted.Ord    as Exported
import CLaSH.Sized.Index     as Exported
import CLaSH.Sized.Signed    as Exported
import CLaSH.Sized.Unsigned  as Exported
import CLaSH.Sized.VectorZ   as Exported
import CLaSH.Bit             as Exported
import CLaSH.Signal          as Exported
import GHC.TypeLits          as Exported

{-# INLINABLE window #-}
window ::
  (SingI (n + 1), Default a)
  => Sync a
  -> Vec ((n + 1) + 1) (Sync a)
window x = x :> prev
  where
    prev = registerP (vcopyI def) next
    next = x +>> prev

{-# INLINABLE windowP #-}
windowP ::
  (SingI (n + 1), Default a)
  => Sync a
  -> Vec (n + 1) (Sync a)
windowP x = prev
  where
    prev = registerP (vcopyI def) next
    next = x +>> prev

{-# INLINABLE (<^>) #-}
(<^>) ::
  (Pack i, Pack o)
  => (s -> i -> (s,o))
  -> s
  -> (Packed i -> Packed o)
f <^> iS = \i -> let (s',o) = split $ f <$> s <*> (combine i)
                     s      = register iS s'
                 in split o

{-# INLINABLE registerP #-}
registerP :: Pack a => a -> Packed a -> Packed a
registerP i = split Prelude.. register i Prelude.. combine

{-# NOINLINE blockRam #-}
blockRam :: forall n m a . (SingI n, SingI m, Pack a)
         => Sing (n :: Nat)
         -> Sync (Unsigned m)
         -> Sync (Unsigned m)
         -> Sync Bool
         -> Sync a
         -> Sync a
blockRam n wr rd en din = combine $ (bram' <^> binit) (wr,rd,en,din)
  where
    binit :: (Vec n a,a)
    binit = (vcopy n (error "uninitialized ram"),error "uninitialized ram")

    bram' :: (Vec n a,a) -> (Unsigned m, Unsigned m, Bool, a)
          -> (((Vec n a),a),a)
    bram' (ram,o) (w,r,e,d) = ((ram',o'),o)
      where
        ram' | e         = vreplace ram w d
             | otherwise = ram
        o'               = ram ! r

{-# INLINABLE blockRamPow2 #-}
blockRamPow2 :: (SingI n, SingI (2^n), Pack a)
             => (Sing ((2^n) :: Nat))
             -> Sync (Unsigned n)
             -> Sync (Unsigned n)
             -> Sync Bool
             -> Sync a
             -> Sync a
blockRamPow2 = blockRam

newtype Comp a b = C { asFunction :: Sync a -> Sync b }

instance Category Comp where
  id            = C Prelude.id
  (C f) . (C g) = C (f Prelude.. g)

infixr 8 ><
(><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(f >< g) (x,y) = (f x,g y)

instance Arrow Comp where
  arr         = C Prelude.. fmap
  first (C f) = C $ combine Prelude.. (f >< Prelude.id) Prelude.. split

instance ArrowLoop Comp where
  loop (C f) = C $ simpleLoop (split Prelude.. f Prelude.. combine)
    where
      simpleLoop g b = let ~(c,d) = g (b,d)
                       in c

registerC :: a -> Comp a a
registerC = C Prelude.. register

{-# INLINABLE (^^^) #-}
(^^^) :: (s -> i -> (s,o)) -> s -> Comp i o
f ^^^ sI = C $ \i -> let (s',o) = split $ f <$> s <*> i
                         s      = register sI s'
                     in  o
