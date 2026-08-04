{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module T1534 where

import Clash.Prelude

type family Width a where
  Width a = BitSize a

topEntity :: BitVector (Width Bool) -> BitVector (Width (Vec 2 Bit)) -> ()
topEntity _ _ = ()
