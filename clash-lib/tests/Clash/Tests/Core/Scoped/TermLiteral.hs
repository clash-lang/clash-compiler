{-|
Copyright   :  (C) 2022, Google Inc.
                   2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Tests for 'Clash.Core.Scoped.TermLiteral': the scoped twins of
"Clash.Tests.Core.TermLiteral", and checks that the old and scoped decoders
agree on generated values.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Tests.Core.Scoped.TermLiteral where

import Data.Bifunctor (bimap)
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
import Clash.Core.Scoped.Convert (fromClashTermClosed)
import Clash.Core.Scoped.Pretty ()
import Clash.Core.Scoped.Syntax (S (VoidS), Term (App, Literal, Tick), TickInfo (NoDeDup))
import Clash.Core.Scoped.TermLiteral
import qualified Clash.Core.Term as C
import qualified Clash.Core.TermLiteral as Old
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

-- * Agreement of the old and scoped decoders

deriveToCoreTerm ''Bool
deriveToCoreTerm ''Maybe
deriveToCoreTerm ''Either
deriveToCoreTerm ''Cv.RenderAs
deriveToCoreTerm ''Cv.Assertion'
deriveToCoreTerm ''Cv.Property'
deriveToCoreTerm ''Attr

deriveArbitrary ''Cv.RenderAs
deriveArbitrary ''Cv.Assertion'
deriveArbitrary ''Cv.Property'
deriveArbitrary ''Attr

-- | Decoding the encoding of a value at type @b@ with the old decoder gives
-- the same as converting it to scoped Core and decoding it with the scoped
-- decoder: the same value, or the same failing subterm.
agreeAt
  :: forall b a
   . (Old.TermLiteral b, TermLiteral b, Show b, ToCoreTerm a)
  => a -> Property
agreeAt x = agreeOn @b (toCoreTerm x)

-- | 'agreeAt' for a given term
agreeOn
  :: forall b
   . (Old.TermLiteral b, TermLiteral b, Show b)
  => C.Term -> Property
agreeOn t =
  counterexample (showPpr t) $
    bimap showPpr show (Old.termToData @b t)
      === bimap showPpr show (termToData @b (fromClashTermClosed t))

-- | Both decoders decode the encoding of a value to the value
agree
  :: forall a
   . (Old.TermLiteral a, TermLiteral a, Show a, ToCoreTerm a)
  => a -> Property
agree x =
  agreeAt @a x
    .&&. bimap showPpr show (termToData @a (fromClashTermClosed (toCoreTerm x)))
      === Right (show x)

prop_agree_bool :: Bool -> Property
prop_agree_bool = agree

prop_agree_maybe :: Maybe Int -> Property
prop_agree_maybe = agree

prop_agree_either :: Either Int Word -> Property
prop_agree_either = agree

prop_agree_renderAs :: Cv.RenderAs -> Property
prop_agree_renderAs = agree

prop_agree_assertion :: Cv.Assertion' Int -> Property
prop_agree_assertion = agree

prop_agree_property :: Cv.Property' (Maybe Text, Int) -> Property
prop_agree_property = agree

prop_agree_attr :: Attr Text -> Property
prop_agree_attr = agree

prop_agree_int :: Int -> Property
prop_agree_int = agree

prop_agree_word :: Word -> Property
prop_agree_word = agree

prop_agree_char :: Char -> Property
prop_agree_char = agree

prop_agree_integer :: Integer -> Property
prop_agree_integer = agree

-- | QuickCheck before 2.17 has no 'Arbitrary' instance for 'Natural'
prop_agree_natural :: NonNegative Integer -> Property
prop_agree_natural (NonNegative n) = agree (fromInteger @Natural n)

prop_agree_string :: String -> Property
prop_agree_string = agree

prop_agree_text :: Text -> Property
prop_agree_text = agree

prop_agree_text_byteArray :: Text -> Property
prop_agree_text_byteArray x = agreeOn @Text (textByteArrayTerm x)

prop_agree_index :: Index 10 -> Property
prop_agree_index = agree

prop_agree_tuple :: (Int, Bool) -> Property
prop_agree_tuple = agree

prop_agree_vec :: Vec 3 (Maybe Int) -> Property
prop_agree_vec = agree

-- | Decoding at the wrong type fails at the same subterm
prop_agree_wrong_type :: Maybe Int -> Cv.Property' Int -> Vec 3 Int -> Property
prop_agree_wrong_type x y z =
  agreeAt @(Either Int Int) x
    .&&. agreeAt @(Cv.Assertion' Int) y
    .&&. agreeAt @(Vec 4 Int) z
    .&&. agreeAt @Natural x

-- | The explicit leaf decoder that replaces the old @TermLiteral Term@
-- instance, for "Clash.Primitives.Verification": decoding with subterms as
-- leaves gives the same subterms as the old decoder at
-- @Property' (Maybe Text, Term)@.
prop_agree_property_term_leaves :: Cv.Property' (Maybe Text, Int) -> Property
prop_agree_property_term_leaves x =
  counterexample (showPpr t) $
    bimap showPpr (show . fmap (fmap showPpr))
      (Old.termToData @(Cv.Property' (Maybe Text, C.Term)) t)
      === bimap showPpr (show . fmap (fmap showPpr))
            (termToDataWith @(Cv.Property' (Maybe Text, Term 'VoidS))
              (liftTermToData# (liftTermToData# Right))
              (fromClashTermClosed t))
 where
  t = toCoreTerm x

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.TermLiteral" [$(testGroupGenerator)]
