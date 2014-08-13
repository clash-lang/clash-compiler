{-# LANGUAGE TemplateHaskell #-}
module CLaSH.Promoted.Nat.TH where

import Language.Haskell.TH

import CLaSH.Promoted.Nat

-- | Create an 'SNat' literal
--
-- > $(decLiteralD 1200)
--
-- >>> :t d1200
-- d1200 :: SNat 1200
decLiteralD :: Integer
            -> Q [Dec]
decLiteralD n = do
  let suffix  = if n < 0 then error ("Can't make negative SNat: " ++ show n) else show n
      valName = mkName $ 'd':suffix
  sig   <- sigD valName (appT (conT ''SNat) (litT (numTyLit n)))
  val   <- valD (varP valName) (normalB [| snat |]) []
  return [ sig, val ]

-- | Create a range of 'SNat' literals
--
-- > $(decLiteralsD 1200 1202)
--
-- >>> :t d1200
-- d1200 :: SNat 1200
-- >>> :t d1201
-- d1201 :: SNat 1201
-- >>> :t d1202
-- d1202 :: SNat 1202
decLiteralsD :: Integer
             -> Integer
             -> Q [Dec]
decLiteralsD from to =
    fmap concat $ sequence $ [ decLiteralD n | n <- [from..to] ]
