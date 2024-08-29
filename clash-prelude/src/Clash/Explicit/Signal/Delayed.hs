{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd
                  2021     , LUMI GUIDE FIETSDETECTIE B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.Signal.Delayed
  ( DSignal
    -- * Delay-annotated synchronous signals
  , delayed
  , delayedI
  , delayN
  , delayI
  , delayedFold
  , feedback
    -- * Signal \<-\> DSignal conversion
  , fromSignal
  , toSignal
    -- * List \<-\> DSignal conversion (not synthesizable)
  , dfromList
    -- ** lazy versions
  , dfromList_lazy
    -- * Experimental
  , unsafeFromSignal
  , antiDelay
  , forward
  )
where

import Prelude                    ((.), ($), (<$>), id, Num(..), Maybe(..), fmap, liftA2)

import Data.Coerce                (coerce)
import Data.Kind                  (Type)
import Data.Type.Equality         ((:~:)(Refl))
import GHC.TypeLits               (sameNat, Div, Mod, KnownNat, Nat, type (+), type (*), type (<=))
import GHC.TypeLits.Extra         (CLog)

import Clash.Magic                (clashCompileError)
import Clash.Sized.Vector
import Clash.Signal.Delayed.Internal
  (DSignal(..), dfromList, dfromList_lazy, fromSignal, toSignal,
   unsafeFromSignal, antiDelay, feedback, forward)
import qualified Clash.Signal.Delayed.Bundle as D

import Clash.Explicit.Signal
  (KnownDomain, Clock, Domain, Reset, Signal, Enable, register, delay, bundle, unbundle)
import Clash.Promoted.Nat         (SNat (..), SNatLE (..), compareSNat, snatToInteger)
import Clash.XException           (NFDataX)

{- $setup
>>> :set -XDataKinds
>>> :set -XTypeOperators
>>> import Clash.Explicit.Prelude
>>> let delay3 clk rst en = delayed clk rst en (-1 :> -1 :> -1 :> Nil)
>>> let delay2 clk rst en = (delayedI clk rst en :: Int -> DSignal System n Int -> DSignal System (n + 2) Int)
>>> let delayN2 = delayN d2
>>> let delayI2 = delayI :: KnownDomain dom => Int -> Enable dom -> Clock dom -> DSignal dom n Int -> DSignal dom (n + 2) Int
>>> let countingSignals = Clash.Prelude.repeat (dfromList [0..]) :: Vec 4 (DSignal dom 0 Int)
>>> :{
let mac :: Clock System
        -> Reset System
        -> Enable System
        -> DSignal System 0 Int -> DSignal System 0 Int
        -> DSignal System 0 Int
    mac clk rst en x y = feedback (mac' x y)
      where
        mac' :: DSignal System 0 Int -> DSignal System 0 Int
             -> DSignal System 0 Int
             -> (DSignal System 0 Int, DSignal System 1 Int)
        mac' a b acc = let acc' = a * b + acc
                       in  (acc, delayed clk rst en (singleton 0) acc')
:}

-}

-- TODO: Reimplement with dtfold
-- | Delay a 'DSignal' for @d@ periods.
--
-- @
-- delay3
--   :: KnownDomain dom
--   => Clock dom
--   -> Reset dom
--   -> Enable dom
--   -> 'DSignal' dom n Int
--   -> 'DSignal' dom (n + 3) Int
-- delay3 clk rst en = 'delayed' clk rst en (-1 ':>' -1 ':>' -1 ':>' 'Nil')
-- @
--
-- >>> sampleN 7 (delay3 systemClockGen resetGen enableGen (dfromList [0..]))
-- [-1,-1,-1,-1,1,2,3]
delayed
  :: forall dom  a n d
   . ( KnownDomain dom
     , KnownNat d
     , NFDataX a )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> Vec d a
  -- ^ Initial values
  -> DSignal dom n a
  -> DSignal dom (n + d) a
delayed clk rst en m ds = coerce (delaySignal (coerce ds))
  where
    delaySignal :: Signal dom a -> Signal dom a
    delaySignal s = case length m of
      0 -> s
      _ -> let (r',o) = shiftInAt0 (unbundle r) (singleton s)
               r      = register clk rst en m (bundle r')
           in  head o

-- | Delay a 'DSignal' for @d@ periods, where @d@ is derived from the
-- context.
--
-- @
-- delay2
--   :: KnownDomain dom
--   => Clock dom
--   -> Reset dom
--   -> Enable dom
--   -> Int
--   -> 'DSignal' dom n Int
--   -> 'DSignal' dom (n + 2) Int
-- delay2 = 'delayedI'
-- @
--
-- >>> sampleN 7 (delay2 systemClockGen resetGen enableGen (-1) (dfromList ([0..])))
-- [-1,-1,-1,1,2,3,4]
--
-- @d@ can also be specified using type application:
--
-- >>> :t delayedI @3
-- delayedI @3
--   :: ... =>
--      Clock dom
--      -> Reset dom
--      -> Enable dom
--      -> a
--      -> DSignal dom n a
--      -> DSignal dom (n + 3) a
delayedI
  :: ( KnownNat d
     , KnownDomain dom
     , NFDataX a )
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> a
  -- ^ Initial value
  -> DSignal dom n a
  -> DSignal dom (n + d) a
delayedI clk rst en dflt = delayed clk rst en (repeat dflt)

-- | Delay a 'DSignal' for @d@ cycles, the value at time 0..d-1 is /a/.
--
-- @
-- delayN2
--   :: 'KnownDomain' dom
--   => Int
--   -> 'Enable' dom
--   -> 'Clock' dom
--   -> 'DSignal' dom n Int
--   -> 'DSignal' dom (n + 2) Int
-- delayN2 = 'delayN' d2
-- @
--
-- >>> printX $ sampleN 6 (delayN2 (-1) enableGen systemClockGen (dfromList [1..]))
-- [-1,-1,1,2,3,4]
delayN
  :: forall dom a d n
   . ( KnownDomain dom
     , NFDataX a )
  => SNat d
  -> a
  -- ^ Initial value
  -> Enable dom
  -> Clock dom
  -> DSignal dom n a
  -> DSignal dom (n+d) a
delayN d dflt ena clk = coerce . go (snatToInteger d) . coerce @_ @(Signal dom a)
  where
    go 0 = id
    go i = delay clk ena dflt . go (i-1)

-- | Delay a 'DSignal' for @d@ cycles, where @d@ is derived from the context.
-- The value at time 0..d-1 is a default value.
--
-- @
-- delayI2
--   :: 'KnownDomain' dom
--   => Int
--   -> 'Enable' dom
--   -> 'Clock' dom
--   -> 'DSignal' dom n Int
--   -> 'DSignal' dom (n + 2) Int
-- delayI2 = 'delayI'
-- @
--
-- >>> sampleN 6 (delayI2 (-1) enableGen systemClockGen (dfromList [1..]))
-- [-1,-1,1,2,3,4]
--
-- You can also use type application to do the same:
--
-- >>> sampleN 6 (delayI @2 (-1) enableGen systemClockGen (dfromList [1..]))
-- [-1,-1,1,2,3,4]
delayI
  :: forall d n a dom
   . ( NFDataX a
     , KnownDomain dom
     , KnownNat d )
  => a
  -- ^ Initial value
  -> Enable dom
  -> Clock dom
  -> DSignal dom n a
  -> DSignal dom (n+d) a
delayI dflt = delayN (SNat :: SNat d) dflt

-- | Tree fold over a 'Vec' of 'DSignal's with a combinatorial function,
-- and delaying @delay@ cycles after each application.
-- Values at times 0..(delay * CLog 2 n)-1 are set to a default.
--
-- @
-- countingSignals :: Vec 4 (DSignal dom 0 Int)
-- countingSignals = repeat (dfromList [0..])
-- @
--
-- >>> printX $ sampleN 6 (delayedFold  d1 (-1) (+) enableGen systemClockGen countingSignals)
-- [-1,-2,0,4,8,12]
--
-- >>> printX $ sampleN 8 (delayedFold d2 (-1) (*) enableGen systemClockGen countingSignals)
-- [-1,-1,1,1,0,1,16,81]
delayedFold
  :: forall dom d delay n a
   . ( NFDataX a
     , KnownDomain dom
     , KnownNat delay
     , KnownNat n
     , 1 <= n )
  => SNat delay
  -- ^ Delay applied after each step
  -> a
  -- ^ Initial value
  -> (a -> a -> a)
  -- ^ Fold operation to apply
  -> Enable dom
  -> Clock dom
  -> Vec n (DSignal dom d a)
  -- ^ Vector input of size @n@
  -> DSignal dom (d + delay * CLog 2 n) a
  -- ^ Output Signal delayed by @delay * CLog 2 n@
delayedFold SNat initial f ena clk inps = case sameNat (SNat @1) (SNat @n) of
  Just Refl -> head inps
  _ -> case (modProof, strictlyPosDivRu, divMulProof) of
    (SNatLE, SNatLE, Just Refl) ->
      case sameNat (SNat @(1 + CLog 2 (n `Div` 2 + n `Mod` 2))) (SNat @(CLog 2 n)) of
        Just Refl -> delayedFold (SNat @delay) initial f ena clk newLayer
         where
          newLayer = D.unbundle $
            step @(n `Div` 2) @(n `Mod` 2) @d @delay (SNat @(n `Div` 2)) initial f ena clk (D.bundle inps)
        _ -> clashCompileError
              "delayedFold0: absurd, report this to the clash-compiler team: https://github.com/clash-lang/clash-compiler/issues"
    _ -> clashCompileError
          "delayedFold1: absurd, report this to the clash-compiler team: https://github.com/clash-lang/clash-compiler/issues"
   where
    modProof = compareSNat (SNat @(n `Mod` 2)) (SNat @1)
    strictlyPosDivRu = compareSNat (SNat @1) (SNat @(n `Div` 2 + n `Mod` 2))
    divMulProof = sameNat (SNat @n) (SNat @(2 * (n `Div` 2) + n `Mod` 2))

-- | A single layer of the pipelined fold
step :: forall (m :: Nat) (p :: Nat) (d :: Nat) (delay :: Nat) (dom :: Domain) (a :: Type).
     KnownNat p
  => KnownNat delay
  => KnownDomain dom
  => p <= 1
  => NFDataX a
  => SNat m
  -> a
  -> (a -> a -> a)
  -> Enable dom
  -> Clock dom
  -> DSignal dom d (Vec (2 * m + p) a)
  -> DSignal dom (d + delay) (Vec (m + p) a)
step SNat initial f ena clk inps =
  let
    layerCalc :: DSignal dom d (Vec (2 * m) a) -> DSignal dom d (Vec m a)
    layerCalc = fmap (map applyF . unconcatI)

    applyF :: Vec 2 a -> a
    applyF (a `Cons` b `Cons` _) = f a b
  in
    case (sameNat (SNat @p) (SNat @0), sameNat (SNat @p) (SNat @1)) of
      -- Size of the input vector is even
      (Just Refl, Nothing) ->
        delayI (repeat initial) ena clk (layerCalc inps)
      -- Size of the input vector is odd
      (Nothing, Just Refl) ->
        delayI (repeat initial) ena clk $
          liftA2
            (++)
            (singleton . head <$> inps)
            (layerCalc (tail <$> inps))
      _ -> clashCompileError
            "delayedFold step: absurd, report this to the clash-compiler team: https://github.com/clash-lang/clash-compiler/issues"
