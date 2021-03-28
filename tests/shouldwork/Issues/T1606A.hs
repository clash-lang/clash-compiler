module T1606A where

import Clash.Prelude

data CRE = CRE (Clock System) (Reset System) (Enable System)

topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Vec 2 (Signal System Bool, CRE)
topEntity = \ c r e ->
  let
    term   = CRE c r e
    vec    = term :> term :> Nil
    result = fmap (\a -> (pure True, a)) vec
  in
    result
