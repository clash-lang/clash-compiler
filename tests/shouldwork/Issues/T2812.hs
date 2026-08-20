{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=20 #-}
-- | Casts originating from @ghc-typelits-knownnat@'s @SBool ~ Bool@ evidence
-- used to be dropped in the GHC-to-Clash translation, making @caseCon@ match
-- an @SFalse@/@STrue@ pattern against a @Bool@ constructor. See
-- https://github.com/clash-lang/clash-compiler/issues/2812
module T2812 where

import Clash.Prelude

import Data.Proxy
import Data.Type.Bool

data T = A

type family TF (t :: T) :: Nat where
  TF A = 32

data TFacts (t :: T) where
  TFacts :: KnownNat (TF t)  => Proxy t -> TFacts t

class    KnownT t where knownT :: TFacts t
instance KnownT A where knownT  = TFacts Proxy

type RSF t n = If (n <=? TF t) (Div (TF t) n) 1

go ::
  forall t n.
  ( KnownT t, KnownNat n, 1 <= n
  , 1 <= If (n <=? (TF t)) (Div (TF t) n) 1
  ) => BitVector n -> Vec (RSF t n) (BitVector n)
go inp | TFacts{} <- knownT @t = bitCoerce
  $ let ext :: BitVector n -> BitVector (n * RSF t n)
        ext = extend @_ @_ @(n * RSF t n - n)
     in unpack (ext inp) :: Unsigned (n * RSF t n)

topEntity ::
  HiddenClockResetEnable System =>
  Signal System (Vec (RSF A 8) (BitVector 8))
topEntity = go @A @8 <$> pure 0
