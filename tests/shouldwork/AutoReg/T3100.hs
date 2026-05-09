{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module T3100 where

import Clash.Prelude

type family Width (n :: Nat) :: Nat where
  Width n = n + 1

data Foo n = Foo (BitVector (Width n)) deriving (Generic, NFDataX)
deriveAutoReg ''Foo

topEntity ::
  SystemClockResetEnable =>
  Signal System (Foo 7) ->
  Signal System (Foo 7)
topEntity = autoReg (Foo 0)
