{-# LANGUAGE TemplateHaskell, DataKinds #-}
-- | Predefined 'SNat' singleton literals in the range [0 .. 1024]
--
-- Defines:
--
-- > d0 = snat :: SNat 0
-- > d1 = snat :: SNat 1
-- > d2 = snat :: SNat 2
-- > ...
-- > d1024 = snat :: SNat 1024
--
-- You can generate more 'SNat' literals using 'decLiteralsD' from "CLaSH.Promoted.Nat.TH"
module CLaSH.Promoted.Nat.Literals where

import CLaSH.Promoted.Nat.TH

$(decLiteralsD "d" 0 1024)
