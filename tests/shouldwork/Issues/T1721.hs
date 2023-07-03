{-# LANGUAGE CPP #-}

module T1721 where

import Clash.Prelude

type family G a
type instance G () = Bool

type family F a

data T a = T a
data TP a = TP Bool

type instance F (T Bool) = TP Bool

f :: F (T (G ()))
  -> F (T (G ()))
f = id

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE f #-}

topEntity :: F (T (G ()))
          -> F (T (G ()))
topEntity = f
