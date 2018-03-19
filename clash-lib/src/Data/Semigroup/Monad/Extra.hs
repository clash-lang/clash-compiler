{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Semigroup.Monad.Extra
  (module Data.Semigroup.Monad
  )
where

import Control.Monad.Fix
import Data.Semigroup.Monad

instance MonadFix f => MonadFix (Mon f) where
  mfix f = Mon (mfix (getMon . f))

