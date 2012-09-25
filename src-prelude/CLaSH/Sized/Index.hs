{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeOperators         #-}
module CLaSH.Sized.Index where

import GHC.TypeLits

data Index :: Nat -> * where
  Z :: Index 0
  S :: Index s -> Index (s + 1)
