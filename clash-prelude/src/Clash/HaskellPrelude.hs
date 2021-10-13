{-|
  Copyright   :  (C) 2019, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V <devops@qbaylogic.com>

"Clash.HaskellPrelude" re-exports most of the Haskell "Prelude" with the
exception of those functions that the Clash API defines to work on
'Clash.Sized.Vector.Vec' from "Clash.Sized.Vector" instead of on lists as the
Haskell Prelude does. In addition, for the 'Clash.Class.Parity.odd' and
'Clash.Class.Parity.even' functions a type class called
'Clash.Class.Parity.Parity' is available at "Clash.Class.Parity".
-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions, not-home #-}

module Clash.HaskellPrelude
  (module Prelude)
where

import Prelude hiding
  ((++), (!!), concat, concatMap, drop, even, foldl, foldl1, foldr, foldr1, head, init,
   iterate, last, length, map, odd, repeat, replicate, reverse, scanl, scanr, splitAt,
   tail, take, unzip, unzip3, zip, zip3, zipWith, zipWith3, undefined, (^),
   getChar, putChar, getLine)
