-- Test annotations on product types, but not their individual components.
module ProductInResult where

import Clash.Prelude hiding (assert, (++))
import Clash.Annotations.SynthesisAttributes

mac xy = mealy macT 0 xy
  where
    macT acc (x,y) = (acc',o)
      where
        acc' = acc + x * y
        o    = acc

topEntity
  :: SystemClockResetEnable
  => Signal System (Signed 9, Signed 9)
  -> Signal System (Signed 9, Signed 9) `Annotate` 'StringAttr "result" "product"
topEntity xy = bundle (s, s)
  where
    s = mac xy
