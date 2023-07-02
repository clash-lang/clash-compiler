{-|
Copyright  :  (C) 2015-2016, University of Twente
                  2022     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}

module Clash.Promoted.Nat.Unsafe
  (unsafeSNat)
where

import Data.Reflection    (reifyNat)
import Unsafe.Coerce      (unsafeCoerce)

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Promoted.Nat (SNat, snatProxy)

-- | I hope you know what you're doing
unsafeSNat :: Integer -> SNat k
unsafeSNat i = reifyNat i $ (\p -> unsafeCoerce (snatProxy p))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsafeSNat #-}
{-# ANN unsafeSNat hasBlackBox #-}
