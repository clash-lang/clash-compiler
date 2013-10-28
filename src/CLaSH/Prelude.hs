{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -O0 -fno-omit-interface-pragmas #-}

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
import CLaSH.Promoted.Nats   as Exported
import CLaSH.Promoted.Ord    as Exported
import CLaSH.Sized.Index     as Exported
import CLaSH.Sized.Signed    as Exported
import CLaSH.Sized.Unsigned  as Exported
import CLaSH.Sized.Vector    as Exported
import CLaSH.Bit             as Exported
import CLaSH.Signal          as Exported
import GHC.TypeLits          as Exported

{-# INLINABLE window #-}
window ::
  (KnownNat (n + 1), Default a)
  => Signal a
  -> Vec ((n + 1) + 1) (Signal a)
window x = x :> prev
  where
    prev = registerP (vcopyI def) next
    next = x +>> prev

{-# INLINABLE windowP #-}
windowP ::
  (KnownNat (n + 1), Default a)
  => Signal a
  -> Vec (n + 1) (Signal a)
windowP x = prev
  where
    prev = registerP (vcopyI def) next
    next = x +>> prev

{-# INLINABLE (<^>) #-}
(<^>) ::
  (Pack i, Pack o)
  => (s -> i -> (s,o))
  -> s
  -> (SignalP i -> SignalP o)
f <^> iS = \i -> let (s',o) = unpack $ f <$> s <*> (pack i)
                     s      = register iS s'
                 in unpack o

{-# INLINABLE registerP #-}
registerP :: Pack a => a -> SignalP a -> SignalP a
registerP i = unpack Prelude.. register i Prelude.. pack

{-# NOINLINE blockRam #-}
blockRam :: forall n m a . (KnownNat n, KnownNat m, Pack a)
         => SNat (n :: Nat)
         -> Signal (Unsigned m)
         -> Signal (Unsigned m)
         -> Signal Bool
         -> Signal a
         -> Signal a
blockRam n wr rd en din = pack $ (bram' <^> binit) (wr,rd,en,din)
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
blockRamPow2 :: (KnownNat n, KnownNat (n^2), Pack a)
             => (SNat ((n^2) :: Nat))
             -> Signal (Unsigned n)
             -> Signal (Unsigned n)
             -> Signal Bool
             -> Signal a
             -> Signal a
blockRamPow2 = blockRam

newtype Comp a b = C { asFunction :: Signal a -> Signal b }

instance Category Comp where
  id            = C Prelude.id
  (C f) . (C g) = C (f Prelude.. g)

infixr 8 ><
(><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(f >< g) (x,y) = (f x,g y)

instance Arrow Comp where
  arr         = C Prelude.. fmap
  first (C f) = C $ pack Prelude.. (f >< Prelude.id) Prelude.. unpack

instance ArrowLoop Comp where
  loop (C f) = C $ simpleLoop (unpack Prelude.. f Prelude.. pack)
    where
      simpleLoop g b = let ~(c,d) = g (b,d)
                       in c

registerC :: a -> Comp a a
registerC = C Prelude.. register

simulateC :: Comp a b -> [a] -> [b]
simulateC f = simulate (asFunction f)

{-# INLINABLE (^^^) #-}
(^^^) :: (s -> i -> (s,o)) -> s -> Comp i o
f ^^^ sI = C $ \i -> let (s',o) = unpack $ f <$> s <*> i
                         s      = register sI s'
                     in  o
