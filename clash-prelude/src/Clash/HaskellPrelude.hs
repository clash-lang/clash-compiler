{-|
  Copyright   :  (C) 2019, QBayLogic B.V.
                 (C) 2021, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V <devops@qbaylogic.com>

"Clash.HaskellPrelude" re-exports most of the Haskell "Prelude" with the
exception of those functions that the Clash API defines to work on
'Clash.Sized.Vector.Vec' from "Clash.Sized.Vector" instead of on lists as the
Haskell Prelude does. In addition, for the 'Clash.Class.Parity.odd' and
'Clash.Class.Parity.even' functions a type class called
'Clash.Class.Parity.Parity' is available at "Clash.Class.Parity".
-}

{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions, not-home #-}

module Clash.HaskellPrelude
  (module Prelude, (&&), (||), not)
where

import Prelude hiding
  ((++), (!!), concat, concatMap, drop, even, foldl, foldl1, foldr, foldr1, head, init,
   iterate, last, length, map, odd, repeat, replicate, reverse, scanl, scanl1,
   scanr, scanr1, splitAt, tail, take, unzip, unzip3, zip, zip3, zipWith, zipWith3, undefined,
   (^), getChar, putChar, getLine, (&&), (||), not, maximum, minimum)

import qualified Prelude
import GHC.Magic (noinline)

{-
Note [use of noinline]
~~~~~~~~~~~~~~~~~~~~~~
The magic noinline function is used here to prevent GHC inlining these
functions in the simplifier. They are removed (by GHC) post-simplifier, so
they have no negative impact on Clash's normalization.

Why prevent this inlining? When GHC sees a function like

    topEntity :: Bool -> Bool -> Bool
    topEntity a b = a && b

it inlines the definition of && to become

    topEntity a b =
      case a of
        True -> case b of
                  True -> True
                  False -> False
        False -> False

which Clash will render as multiplexer(s) instead of using the and operator
available in the targeted HDL backend. While this has no impact on the quality
of the final result (EDA tools will optimize this with ease in P&R), it makes
the generated HDL (and RTL view of circuits) more obfuscated to read.
-}

infixr 3 &&

(&&) :: Bool -> Bool -> Bool
(&&) = noinline (Prelude.&&)

infixr 2 ||

(||) :: Bool -> Bool -> Bool
(||) = noinline (Prelude.||)

not :: Bool -> Bool
not = noinline Prelude.not
