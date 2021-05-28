{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Monoid.Extra where

import Control.Monad.State (MonadState(..))
import Data.Monoid (Ap(Ap))

instance (MonadState s m) => MonadState s (Ap m) where
  get = Ap get
  put = Ap . put
  state = Ap . state

