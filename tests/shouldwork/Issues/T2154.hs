module T2154 where

import Clash.Explicit.Prelude

topEntity (clk :: Clock System) rst =
  let r = unpack (pack ( errorX "T2154" :: (Index 10)
                       , 3 :: Index 10
                       )) :: Vec 2 (Index 10)
      s = register clk rst enableGen (seq r r) s
   in s
