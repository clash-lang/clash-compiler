{-# LANGUAGE RankNTypes #-}

module Control.Lens.Extra where

import Control.Lens (Lens', lens)

iso' :: (a -> b) -> (b -> a) -> Lens' a b
iso' ab ba = lens ab (const ba)
