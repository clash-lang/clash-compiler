module Clash.Verification.DSL where

import qualified Clash.Verification           as Cv
import           Clash.Verification.Internal

-- Precedences taken from:
--
--   Table 2—FL operator precedence and associativity
--
-- of
--
--   IEEE Std 1850-2010a, Annex B.1, p149

infixr 5 |&|
(|&|) :: (AssertionValue dom a, AssertionValue dom b) => a -> b -> Assertion dom
a |&| b = Cv.and a b
{-# INLINE (|&|) #-}

infixr 4 |||
(|||) :: (AssertionValue dom a, AssertionValue dom b) => a -> b -> Assertion dom
a ||| b = Cv.or a b
{-# INLINE (|||) #-}

(~>) :: (AssertionValue dom a, AssertionValue dom b) => a -> b -> Assertion dom
a ~> b = Cv.implies a b
{-# INLINE (~>) #-}
infixr 0 ~>

(|=>) :: (AssertionValue dom a, AssertionValue dom b) => a -> b -> Assertion dom
a |=> b = Cv.timplies a b
{-# INLINE (|=>) #-}
infixr 1 |=>

(|->) :: (AssertionValue dom a, AssertionValue dom b) => a -> b -> Assertion dom
a |-> b = Cv.timpliesOverlapping a b
{-# INLINE (|->) #-}
infixr 1 |->

(#|#) :: (AssertionValue dom a, AssertionValue dom b) => a -> b -> Assertion dom
a #|# b = Cv.before a b
{-# INLINE (#|#) #-}
infixr 3 #|#
