{-|
  Copyright   :  (C) 2019, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V <devops@qbaylogic.com>

"Clash.HaskellPrelude" re-exports most of the Haskell "Prelude" with the exception of
the following: (++), (!!), concat, drop, even, foldl, foldl1, foldr, foldr1, head,
init, iterate, last, length, map, odd, repeat, replicate, reverse, scanl, scanr,
splitAt, tail, take, unzip, unzip3, zip, zip3, zipWith, zipWith3.
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
