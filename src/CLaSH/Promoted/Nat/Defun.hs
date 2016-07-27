{-|
Copyright  :  (C) 2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Defunctionalised forms of 'GHC.TypeLits.Nat' operations missing from
the <http://hackage.haskell.org/package/singletons singletons> package.
-}

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module CLaSH.Promoted.Nat.Defun where

import Data.Promotion.Prelude     ((:.$))
import Data.Singletons.Prelude    (Apply, TyFun)
import GHC.Exts                   (Constraint)
import GHC.TypeLits               (Nat, KnownNat)

-- | Defunctionalised form of 'KnownNat'
data KnownNatSym (f :: TyFun Nat Constraint) :: *
type instance Apply KnownNatSym n = KnownNat n

-- | Type level equivalent of:
--
-- > dot :: (c -> d) -> (a -> b -> c) -> a -> b -> d
-- > dot = (.) . (.)
--
-- Although strictly not a `GHC.TypeLits` operation, it is handy to have, and it
-- does not currently exist in the <http://hackage.haskell.org/package/singletons singletons>
-- package.
type DotSym = (:.$) `Apply` (:.$) `Apply` (:.$)
