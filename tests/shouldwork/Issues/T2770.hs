{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Distilled from https://github.com/gergoerdi/clash-sudoku (src/Punctuate.hs)
module T2770 where

import Clash.Prelude
import Clash.Class.Counter
import Data.Maybe
import Data.Proxy

class SymbolLength_' (s :: Maybe (Char, Symbol)) where
  type SymbolLength' s :: Nat

  symbolAt :: proxy s -> Index (SymbolLength' s) -> Char

instance SymbolLength_' Nothing where
  type SymbolLength' Nothing = 0

  symbolAt _ _ = errorX "impossible"

instance (SymbolLength_ s, KnownChar c, KnownNat (SymbolLength s)) => SymbolLength_' (Just '(c, s)) where
  type SymbolLength' (Just '(c, s)) = 1 + SymbolLength s

  symbolAt _ i
    | i == 0
    = charVal (Proxy @c)

    | otherwise
    = symbolAt (Proxy @(UnconsSymbol s)) (fromIntegral (i - 1))

type SymbolLength s = SymbolLength' (UnconsSymbol s)
type SymbolLength_ s = SymbolLength_' (UnconsSymbol s)

newtype Punctuate (rep :: Nat) (sep :: Symbol) a
  = MkPunctuate (Index rep, Either a (Index (SymbolLength sep)))
  deriving stock (Show, Generic)
  deriving newtype (NFDataX)

deriving newtype instance (SymbolLength_ sep, KnownNat (SymbolLength sep), 1 <= SymbolLength sep, KnownNat rep, 1 <= rep, Counter a) => Counter (Punctuate rep sep a)

type PunctuateGrid n m =
  Punctuate m "\r\n" (
  Punctuate n "\r\n" (
  Punctuate n " " (
  Punctuate m " "
  (Index 1))))

punctuateGrid :: SNat n -> SNat m -> PunctuateGrid n m
punctuateGrid SNat SNat =
  MkPunctuate . (0,) . Left $
  MkPunctuate . (0,) . Left $
  MkPunctuate . (0,) . Left $
  MkPunctuate . (0,) . Left $
  0

class Punctuating c a where
  punctuation :: a -> Maybe c

instance Punctuating c (Index n) where
  punctuation _ = Nothing

instance (Punctuating Char k, SymbolLength_ sep) => Punctuating Char (Punctuate rep sep k) where
  punctuation = \case
    MkPunctuate (_, Right i) -> Just $ symbolAt (Proxy @(UnconsSymbol sep)) i
    MkPunctuate (_, Left k) -> punctuation k

-- A stripped-down 'Protocols.Df.expander': consume input when not emitting
-- punctuation.
punctuate ::
  forall dom spec.
  (HiddenClockResetEnable dom, Counter spec, Punctuating Char spec, NFDataX spec) =>
  spec ->
  Signal dom (Maybe Char) ->
  Signal dom (Maybe Char, Bool)
punctuate spec0 = mealy step (spec0, punctuation spec0)
 where
  step (spec, punc) x =
    let spec' = countSucc spec
        punc' = punctuation spec'
    in case punc of
        Just sep -> ((spec', punc'), (Just sep, isNothing punc'))
        Nothing  -> ((spec', punc'), (x, False))

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Maybe Char) ->
  Signal System (Maybe Char, Bool)
topEntity clk rst en =
  withClockResetEnable clk rst en
    (punctuate (punctuateGrid (SNat @3) (SNat @3)))
