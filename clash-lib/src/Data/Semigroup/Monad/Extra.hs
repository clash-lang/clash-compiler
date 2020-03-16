{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Semigroup.Monad.Extra
  (module Data.Semigroup.Monad
  )
where

import Control.Monad.Fix
import Control.Monad.State
import Data.Semigroup.Monad

instance MonadFix f => MonadFix (Mon f) where
  mfix f = Mon (mfix (getMon . f))

instance MonadState s m => MonadState s (Mon m) where
  get = Mon get
  put = Mon . put
  state = Mon . state
