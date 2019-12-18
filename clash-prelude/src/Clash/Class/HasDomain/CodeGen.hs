{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

module Clash.Class.HasDomain.CodeGen
  ( mkTryDomainTuples
  , mkHasDomainTuples
  ) where

import           Language.Haskell.TH.Syntax
import           Clash.CPP                    (maxTupleSize)
import           Language.Haskell.TH.Compat   (mkTySynInstD)


mkTup :: [Type] -> Type
mkTup names@(length -> n) =
  foldl AppT (TupleT n) names

-- | Creates an instance of the form:
--
--  type instance TryDomain t (a, b, c, d, e) = Merge t a (b, c, d, e)
--
-- With /n/ number of variables on the LHS.
mkTryDomainTupleInstance :: Name -> Name -> Int -> Dec
mkTryDomainTupleInstance tryDomainName mergeName n =
  mkTySynInstD tryDomainName [t, tupPat] tupBody
 where
  bcde = map (VarT . mkName . ("a"++) . show) [1..n-1]
  a    = VarT (mkName "a0")
  t    = VarT (mkName "t")

  -- Merge t a (b, c, d, e)
  tupBody = ConT mergeName `AppT` t `AppT` a `AppT` (mkTup bcde)

  -- (a, b, c, d, e)
  tupPat = mkTup (a : bcde)

mkTryDomainTuples :: Name -> Name -> Q [Dec]
mkTryDomainTuples tryDomainName mergeName =
  pure (map (mkTryDomainTupleInstance tryDomainName mergeName) [3..maxTupleSize])


-- | Creates an instance of the form:
--
--  type instance HasDomain' dom (a, b, c, d, e) =
--    Merge' (HasDomain' dom a) (HasDomain' dom (b, c, d, e))
--
-- With /n/ number of variables on the LHS.
mkHasDomainTupleInstance :: Name -> Name -> Int -> Dec
mkHasDomainTupleInstance hasDomainName mergeName n =
  mkTySynInstD hasDomainName [dom, tupPat] merge
 where
  bcde = map (VarT . mkName . ("a"++) . show) [1..n-1]
  a    = VarT (mkName "a0")
  dom  = VarT (mkName "dom")

  -- Merge dom a (b, c, d, e)
  merge = ConT mergeName `AppT` dom `AppT` a `AppT` mkTup bcde

  -- (a, b, c, d, e)
  tupPat = mkTup (a : bcde)

mkHasDomainTuples :: Name -> Name -> Q [Dec]
mkHasDomainTuples hasDomainName mergeName =
  pure (map (mkHasDomainTupleInstance hasDomainName mergeName) [3..maxTupleSize])
