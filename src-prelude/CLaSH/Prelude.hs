{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module CLaSH.Prelude
  ( module CLaSH.Class.Default
  , module CLaSH.Sized.Index
  , module CLaSH.Sized.Signed
  , module CLaSH.Sized.Unsigned
  , module CLaSH.Sized.VectorZ
  , module CLaSH.Bit
  , module CLaSH.Signal
  , module GHC.TypeLits
  , module CLaSH.Prelude
  )
where

import CLaSH.Class.Default
import CLaSH.Sized.Index
import CLaSH.Sized.Signed
import CLaSH.Sized.Unsigned
import CLaSH.Sized.VectorZ
import CLaSH.Bit
import CLaSH.Signal
import GHC.TypeLits

rememberN ::
  (SingI (n + 1), Default a)
  => Sync a
  -> Vec ((n + 1) + 1) (Sync a)
rememberN x = x :> prev
  where
    prev = registerP (vcopy def) next
    next = x :> vinit prev

rememberN1 ::
  (SingI (n + 1), Default a)
  => Sync a
  -> Vec (n + 1) (Sync a)
rememberN1 x = prev
  where
    prev = registerP (vcopy def) next
    next = x +>> prev
