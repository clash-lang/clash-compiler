{-# LANGUAGE TemplateHaskell #-}
module CLaSH.Promoted.Nat.TH where

import Language.Haskell.TH

import CLaSH.Promoted.Nat

decLiteralD :: String
            -> Integer
            -> Q [Dec]
decLiteralD valPrefix n =
    do let suffix  = if n < 0 then error ("Can't make negative SNat: " ++ show n) else show n
           valName = mkName $ valPrefix ++ suffix
       sig   <- sigD valName (appT (conT ''SNat) (litT (numTyLit n)))
       val   <- valD (varP valName) (normalB [| snat |]) []
       return [ sig, val ]

decLiteralsD :: String
             -> Integer
             -> Integer
             -> Q [Dec]
decLiteralsD valPrefix from to =
    fmap concat $ sequence $ [ decLiteralD valPrefix n | n <- [from..to] ]
