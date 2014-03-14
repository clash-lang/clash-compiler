{-# LANGUAGE TemplateHaskell, DataKinds #-}
-- | Predefined 'SNat' singleton literals
module CLaSH.Promoted.Nat.Literals where

import CLaSH.Promoted.Nat.TH

$(decLiteralsD "d" 0 1024)
