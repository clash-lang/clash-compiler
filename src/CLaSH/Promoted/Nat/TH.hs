{-# LANGUAGE TemplateHaskell #-}
module CLaSH.Promoted.Nat.TH where

import Language.Haskell.TH

import CLaSH.Promoted.Nat

-- | Create an 'SNat' constant
--
-- > $(decLiteralD "d" 1200)
--
-- >>> :t d1200
-- d1200 :: SNat 1200
decLiteralD :: String
            -> Integer
            -> Q [Dec]
decLiteralD valPrefix n =
    do let suffix  = if n < 0 then error ("Can't make negative SNat: " ++ show n) else show n
           valName = mkName $ valPrefix ++ suffix
       sig   <- sigD valName (appT (conT ''SNat) (litT (numTyLit n)))
       val   <- valD (varP valName) (normalB [| snat |]) []
       return [ sig, val ]

-- | Create an 'SNat' constants
--
-- > $(decLiteralsD "d" 1200 1202)
--
-- >>> :t d1200
-- d1200 :: SNat 1200
-- >>> :t d1201
-- d1201 :: SNat 1201
-- >>> :t d1202
-- d1202 :: SNat 1202
decLiteralsD :: String
             -> Integer
             -> Integer
             -> Q [Dec]
decLiteralsD valPrefix from to =
    fmap concat $ sequence $ [ decLiteralD valPrefix n | n <- [from..to] ]
