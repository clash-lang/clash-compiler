{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module CLaSH.Promoted.Ord where

import Data.Type.Bool
import GHC.TypeLits

-- | Type-level 'min' function for natural numbers
type family Min (x :: Nat) (y :: Nat) :: Nat
  where
    Min x y = If (x <=? y) x y

-- | Type-level 'max' function for natural numbers
type family Max (x :: Nat) (y :: Nat) :: Nat
  where
    Max x y = If (x <=? y) y x
