{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module CLaSH.Prelude
  ( module Exported
  , module CLaSH.Prelude
  )
where

import Data.Bits            as Exported
import CLaSH.Class.Default  as Exported
import CLaSH.Promoted.Bool  as Exported
import CLaSH.Promoted.Ord   as Exported
import CLaSH.Sized.Index    as Exported
import CLaSH.Sized.Signed   as Exported
import CLaSH.Sized.Unsigned as Exported
import CLaSH.Sized.VectorZ  as Exported
import CLaSH.Bit            as Exported
import CLaSH.Signal         as Exported
import GHC.TypeLits         as Exported

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
