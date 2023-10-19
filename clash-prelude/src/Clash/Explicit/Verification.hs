{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
                  2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Verification primitives for Clash. Currently implements PSL (Property
Specification Language) and SVA (SystemVerilog Assertions). For a good overview
of PSL and an introduction to the concepts of property checking, read
<https://standards.ieee.org/standard/62531-2012.html>.

The verification API is currently experimental and subject to change.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Explicit.Verification
  ( -- * Types
    Assertion
  , Property
  , AssertionValue
  , RenderAs(..)

    -- * Bootstrapping functions
  , name
  , lit

    -- * Functions to build a PSL/SVA expressions
  , not
  , and
  , or
  , implies
  , next
  , nextN
  , before
  , timplies
  , timpliesOverlapping
  , always
  , never
  , eventually

  -- * Asserts
  , assert
  , cover
  , assume

  -- * Assertion checking
  , check
  , checkI

  -- * Functions to deal with assertion results
  , hideAssertion
  )
 where

import           Prelude
  (Bool, Word, (.), pure, max, concat)

import           Data.Text  (Text)
import           Data.Maybe (Maybe(Just))
import           Data.String.Interpolate (__i)

import           Clash.Annotations.Primitive
  (Primitive(InlineYamlPrimitive), HDL(..))
import           Clash.Signal.Internal (Signal, Clock, Reset)
import           Clash.XException      (errorX, hwSeqX)

import           Clash.Verification.Internal

-- | Convert a signal to a cv expression with a name hint. Clash will try its
-- best to use this name in the rendered assertion, but might run into
-- collisions. You can skip using 'name' altogether. Clash will then try its
-- best to get a readable name from context.
name :: Text -> Signal dom Bool -> Assertion dom
name nm signal = Assertion IsNotTemporal (CvPure (Just nm, signal))
{-# INLINE name #-}

-- | For using a literal (either True or False) in assertions
lit :: Bool -> Assertion dom
lit = Assertion IsNotTemporal . CvLit
{-# INLINE lit #-}

-- | Truth table for 'not':
--
-- @
-- a     | not a
-- ------------
-- True  | False
-- False | True
-- @
not :: AssertionValue dom a => a -> Assertion dom
not (toAssertionValue -> a) = Assertion (isTemporal a) (CvNot (assertion a))
{-# INLINE not #-}

-- | Truth table for 'and':
--
-- @
-- a     | b     | a `and` b
-- --------------|----------
-- False | False | False
-- False | True  | False
-- True  | False | False
-- True  | True  | True
-- @
and :: (AssertionValue dom a, AssertionValue dom b) => a -> b -> Assertion dom
and (toAssertionValue -> a) (toAssertionValue -> b) =
  Assertion
    (max (isTemporal a) (isTemporal b))
    (CvAnd (assertion a) (assertion b))
{-# INLINE and #-}

-- | Truth table for 'or':
--
-- @
-- a     | b     | a `or` b
-- --------------|---------
-- False | False | False
-- False | True  | True
-- True  | False | True
-- True  | True  | True
-- @
or :: (AssertionValue dom a, AssertionValue dom b) => a -> b -> Assertion dom
or (toAssertionValue -> a) (toAssertionValue -> b) =
  Assertion
    (max (isTemporal a) (isTemporal b))
    (CvOr (assertion a) (assertion b))
{-# INLINE or #-}

-- |
-- Truth table for 'implies':
--
-- @
-- a     | b     | a `implies` b
-- --------------|--------------
-- False | False | True
-- False | True  | True
-- True  | False | False
-- True  | True  | True
-- @
implies :: (AssertionValue dom a, AssertionValue dom b) => a -> b -> Assertion dom
implies (toAssertionValue -> Assertion aTmp a) (toAssertionValue -> Assertion bTmp b) =
  Assertion (max aTmp bTmp) (CvImplies a b)
{-# INLINE implies #-}

-- | Truth table for 'next':
--
-- @
-- a[n]  | a[n+1] | a `implies` next a
-- ---------------|-------------------
-- False | False  | True
-- False | True   | True
-- True  | False  | False
-- True  | True   | True
-- @
--
-- where a[n] represents the value of @a@ at cycle @n@ and @a[n+1]@ represents
-- the value of @a@ at cycle @n+1@. Cycle n is an arbitrary cycle.
next :: AssertionValue dom a => a -> Assertion dom
next = nextN 1
{-# INLINE next #-}

-- | Truth table for 'nextN':
--
-- @
-- a[n]  | a[n+m] | a `implies` next m a
-- ---------------|---------------------
-- False | False  | True
-- False | True   | True
-- True  | False  | False
-- True  | True   | True
-- @
--
-- where a[n] represents the value of @a@ at cycle @n@ and a[n+m] represents
-- the value of @a@ at cycle @n+m@. Cycle n is an arbitrary cycle.
nextN :: AssertionValue dom a => Word -> a -> Assertion dom
nextN n = Assertion IsTemporal . CvNext n . assertion . toAssertionValue
{-# INLINE nextN #-}

-- | Same as @a && next b@ but with a nice syntax. E.g., @a && next b@ could
-- be written as @a `before` b@. Might be read as "a happens one cycle before b".
before :: (AssertionValue dom a, AssertionValue dom b) => a -> b -> Assertion dom
before a0 b0 = Assertion IsTemporal (CvBefore a1 b1)
 where
  a1 = assertion (toAssertionValue a0)
  b1 = assertion (toAssertionValue b0)
{-# INLINE before #-}

-- | Same as @a `implies` next b@ but with a nice syntax. E.g.,
-- @a `implies` next b@ could be written as @a `timplies` b@. Might be read
-- as "a at cycle n implies b at cycle n+1".
timplies :: (AssertionValue dom a, AssertionValue dom b) => a -> b -> Assertion dom
timplies a0 b0 = Assertion IsTemporal (CvTemporalImplies 1 a1 b1)
 where
  a1 = toTemporal (toAssertionValue a0)
  b1 = toTemporal (toAssertionValue b0)
{-# INLINE timplies #-}

-- | Same as 'implies' but strictly temporal.
timpliesOverlapping
  :: (AssertionValue dom a, AssertionValue dom b)
  => a
  -> b
  -> Assertion dom
timpliesOverlapping a0 b0 =
  Assertion IsTemporal (CvTemporalImplies 0 a1 b1)
 where
  a1 = toTemporal (toAssertionValue a0)
  b1 = toTemporal (toAssertionValue b0)
{-# INLINE timpliesOverlapping #-}

-- | Specify assertion should _always_ hold
always :: AssertionValue dom a => a -> Assertion dom
always = Assertion IsTemporal . CvAlways . assertion . toAssertionValue
{-# INLINE always #-}

-- | Specify assertion should _never_ hold (not supported by SVA)
never :: AssertionValue dom a => a -> Assertion dom
never = Assertion IsTemporal . CvNever . assertion . toAssertionValue
{-# INLINE never #-}

-- | Specify assertion should _eventually_ hold
eventually :: AssertionValue dom a => a -> Assertion dom
eventually = Assertion IsTemporal . CvEventually . assertion . toAssertionValue
{-# INLINE eventually #-}

-- | Check whether given assertion always holds. Results can be collected with
-- 'check'.
assert :: AssertionValue dom a => a -> Property dom
assert = Property . CvAssert . assertion . toAssertionValue
{-# INLINE assert #-}

-- | Check whether given assertion holds for at least a single cycle. Results
-- can be collected with 'check'.
cover :: AssertionValue dom a => a -> Property dom
cover = Property . CvCover . assertion . toAssertionValue
{-# INLINE cover #-}

-- | Inform the prover that this property is true. This is the same as 'assert'
-- for simulations.
assume :: AssertionValue dom a => a -> Property dom
assume = Property . CvAssume . assertion . toAssertionValue
{-# INLINE assume #-}


-- | Print property as PSL/SVA in HDL. Clash simulation support not yet
-- implemented.
check
  :: Clock dom
  -> Reset dom
  -> Text
  -- ^ Property name (used in reports and error messages)
  -> RenderAs
  -- ^ Assertion language to use in HDL
  -> Property dom
  -> Signal dom AssertionResult
check !_clk !_rst !_propName !_renderAs !_prop =
  pure (errorX (concat [
      "Simulation for Clash.Verification not yet implemented. If you need this,"
    , " create an issue at https://github.com/clash-compiler/clash-lang/issues." ]))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE check #-}
{-# ANN check (InlineYamlPrimitive [Verilog, SystemVerilog, VHDL] [__i|
  BlackBoxHaskell:
    name: Clash.Explicit.Verification.check
    templateFunction: Clash.Primitives.Verification.checkBBF
  |]) #-}

-- | Same as 'check', but doesn't require a design to explicitly carried to
-- top-level.
checkI
  :: Clock dom
  -> Reset dom
  -> Text
  -- ^ Property name (used in reports and error messages)
  -> RenderAs
  -- ^ Assertion language to use in HDL
  -> Property dom
  -> Signal dom a
  -> Signal dom a
checkI clk rst propName renderAs prop =
  hideAssertion (check clk rst propName renderAs prop)

-- | Print assertions in HDL
hideAssertion :: Signal dom AssertionResult -> Signal dom a -> Signal dom a
hideAssertion = hwSeqX
