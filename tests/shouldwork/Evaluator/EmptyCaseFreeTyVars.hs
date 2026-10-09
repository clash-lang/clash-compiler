{-# LANGUAGE EmptyCase #-}

-- | Regression test for commit
-- https://github.com/clash-lang/clash-compiler/commit/32489b7a7b91f80a7fdcb4dcb3b88b366cd2dfae
-- ("Don't create EmptyCase primitive with free variables").
--
-- GHC2Core used to translate an empty case @case v of {}@ of type @a@ to a
-- primitive /whose type is/ @a@. When @a@ is a type variable bound by the
-- enclosing function (here: 'absurd''), the primitive's type keeps mentioning
-- @a@ after 'absurd'' is instantiated at @Int@, because substitution does not
-- look inside the types of primitives. Clash then fails to bring 'absurd''
-- into normal form ("Not in normal form: no Letrec", with type @Never -> a@).
-- Nowadays the empty case becomes @undefined \@a@, where
-- @undefined :: forall a. a@, so the type variable is an ordinary type
-- argument.
module EmptyCaseFreeTyVars where

import Clash.Prelude

data Never

absurd' :: Never -> a
absurd' v = case v of {}
{-# OPAQUE absurd' #-}

topEntity :: Never -> Int -> (Int, Int)
topEntity v x = (x, absurd' v)
