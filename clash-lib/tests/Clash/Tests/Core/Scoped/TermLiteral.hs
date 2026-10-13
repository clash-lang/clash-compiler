{-|
Copyright   :  (C) 2022, Google Inc.
                   2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Tests for "Clash.Core.Scoped.TermLiteral", and checks that the decoders
decode the encodings of generated values to these values.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Tests.Core.Scoped.TermLiteral where

import Data.Bifunctor (bimap)
import Data.Either (isLeft)
import Data.Proxy
import Data.Text (Text)
import Data.Text.Arbitrary ()
import Data.Typeable
import GHC.Natural (Natural)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Clash.Annotations.SynthesisAttributes (Attr)
import qualified Clash.Core.Literal as L
import Clash.Core.Pretty (showPpr)
import Clash.Core.Scoped.Pretty ()
import Clash.Core.Scoped.Syntax (S (VoidS), Term (App, Literal, Tick), TickInfo (NoDeDup))
import Clash.Core.Scoped.TermLiteral
import Clash.Promoted.Nat
import Clash.Sized.Index (Index)
import Clash.Sized.Vector (Vec)
import qualified Clash.Verification.Internal as Cv

import Clash.Tests.Core.TermLiteral.Types
import Test.Clash.TermLiteral

showTypeable :: Typeable a => Proxy a -> String
showTypeable proxy = showsPrec 0 (typeRep proxy) ""

eqTest :: (TermLiteral a, Typeable a) => Proxy a -> Assertion
eqTest proxy = showType proxy @=? showTypeable proxy

case_int :: Assertion
case_int = eqTest (Proxy @Int)

case_maybe_int :: Assertion
case_maybe_int = eqTest (Proxy @(Maybe Int))

case_maybe_maybe_int :: Assertion
case_maybe_maybe_int = eqTest (Proxy @(Maybe (Maybe Int)))

case_either_int_int :: Assertion
case_either_int_int = eqTest (Proxy @(Either Int Int))

case_either_int_maybe_int :: Assertion
case_either_int_maybe_int = eqTest (Proxy @(Either Int (Maybe Int)))

case_int_int :: Assertion
case_int_int = eqTest (Proxy @(Int, Int))

case_maybe_int_maybe_int :: Assertion
case_maybe_int_maybe_int = eqTest (Proxy @(Maybe Int, Maybe Int))

case_maybe_int_int :: Assertion
case_maybe_int_int = eqTest (Proxy @(Maybe (Int, Int)))

case_snat :: Assertion
case_snat = "SNat _" @=? showType (Proxy @(SNat 5))

case_maybe_snat :: Assertion
case_maybe_snat = "Maybe (SNat _)" @=? showType (Proxy @(Maybe (SNat 5)))

deriveTermLiteral ''NatTypeArg

case_natTypeArg :: Assertion
case_natTypeArg = "NatTypeArg _" @=? showType (Proxy @(NatTypeArg 10))

-- Regression test for #2926: 'termToData' must be transparent to ticks
-- anywhere in the term.
case_termToData_strips_ticks_in_subterm :: Assertion
case_termToData_strips_ticks_in_subterm =
  Just "hi"
    @=? either (const Nothing) Just (termToData @String term)
 where
  term :: Term 'VoidS
  term =
    App (Literal (L.IntegerLiteral 0))
        (Tick NoDeDup (Literal (L.StringLiteral "hi")))

-- * Decoding encoded values

deriveToTerm ''Bool
deriveToTerm ''Maybe
deriveToTerm ''Either
deriveToTerm ''Cv.RenderAs
deriveToTerm ''Cv.Assertion'
deriveToTerm ''Cv.Property'
deriveToTerm ''Attr

deriveArbitrary ''Cv.RenderAs
deriveArbitrary ''Cv.Assertion'
deriveArbitrary ''Cv.Property'
deriveArbitrary ''Attr

-- | Decoding the encoding of a value at type @b@ fails
failsAt
  :: forall b a
   . (TermLiteral b, Show b, ToTerm a)
  => a -> Property
failsAt x =
  counterexample (showPpr t) $
    counterexample (either showPpr show r) (isLeft r)
 where
  t = toTerm x
  r = termToData @b t

-- | The decoder decodes the encoding of a value to the value
roundTrip
  :: forall a
   . (TermLiteral a, Show a, ToTerm a)
  => a -> Property
roundTrip x =
  bimap showPpr show (termToData @a (toTerm x)) === Right (show x)

prop_roundTrip_bool :: Bool -> Property
prop_roundTrip_bool = roundTrip

prop_roundTrip_maybe :: Maybe Int -> Property
prop_roundTrip_maybe = roundTrip

prop_roundTrip_either :: Either Int Word -> Property
prop_roundTrip_either = roundTrip

prop_roundTrip_renderAs :: Cv.RenderAs -> Property
prop_roundTrip_renderAs = roundTrip

prop_roundTrip_assertion :: Cv.Assertion' Int -> Property
prop_roundTrip_assertion = roundTrip

prop_roundTrip_property :: Cv.Property' (Maybe Text, Int) -> Property
prop_roundTrip_property = roundTrip

prop_roundTrip_attr :: Attr Text -> Property
prop_roundTrip_attr = roundTrip

prop_roundTrip_int :: Int -> Property
prop_roundTrip_int = roundTrip

prop_roundTrip_word :: Word -> Property
prop_roundTrip_word = roundTrip

prop_roundTrip_char :: Char -> Property
prop_roundTrip_char = roundTrip

prop_roundTrip_integer :: Integer -> Property
prop_roundTrip_integer = roundTrip

-- | QuickCheck before 2.17 has no 'Arbitrary' instance for 'Natural'
prop_roundTrip_natural :: NonNegative Integer -> Property
prop_roundTrip_natural (NonNegative n) = roundTrip (fromInteger @Natural n)

prop_roundTrip_string :: String -> Property
prop_roundTrip_string = roundTrip

prop_roundTrip_text :: Text -> Property
prop_roundTrip_text = roundTrip

prop_roundTrip_text_byteArray :: Text -> Property
prop_roundTrip_text_byteArray x =
  bimap showPpr id (termToData @Text (textByteArrayTerm x)) === Right x

prop_roundTrip_index :: Index 10 -> Property
prop_roundTrip_index = roundTrip

prop_roundTrip_tuple :: (Int, Bool) -> Property
prop_roundTrip_tuple = roundTrip

prop_roundTrip_vec :: Vec 3 (Maybe Int) -> Property
prop_roundTrip_vec = roundTrip

-- | Decoding at the wrong type fails
prop_wrong_type :: Maybe Int -> Cv.Property' Int -> Vec 3 Int -> Property
prop_wrong_type x y z =
  failsAt @(Either Int Int) x
    .&&. failsAt @(Cv.Assertion' Int) y
    .&&. failsAt @(Vec 4 Int) z
    .&&. failsAt @Natural x

-- | The explicit leaf decoder that replaces the old @TermLiteral Term@
-- instance, for "Clash.Primitives.Verification": decoding with subterms as
-- leaves gives the subterms that decode to the values at the leaves.
prop_property_term_leaves :: Cv.Property' (Maybe Text, Int) -> Property
prop_property_term_leaves x =
  counterexample (showPpr t) $
    bimap showPpr (show . fmap (fmap decodeLeaf))
      (termToDataWith @(Cv.Property' (Maybe Text, Term 'VoidS))
        (liftTermToData# (liftTermToData# Right))
        t)
      === Right (show (fmap (fmap (Right :: Int -> Either String Int)) x))
 where
  t = toTerm x
  decodeLeaf :: Term 'VoidS -> Either String Int
  decodeLeaf = bimap showPpr id . termToData @Int

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.TermLiteral" [$(testGroupGenerator)]
