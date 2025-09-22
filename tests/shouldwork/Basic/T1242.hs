module T1242 where

import Clash.Prelude

class C a where
  type T a :: Nat

data Wrapper (t :: Bool)

instance C (t 'True) where
  type T (t 'True) = 16

topEntity :: Vec (T (Wrapper 'True)) Int
topEntity = undefined
