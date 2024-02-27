{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Sized.Vector.ToTuple.TH (vecToTupleInstance, vecToTupleInstances) where

import Clash.Sized.Vector (Vec((:>)))
import Language.Haskell.TH

appTs :: Q Type -> [Q Type] -> Q Type
appTs = foldl appT

appPsInfix :: Name -> [Q Pat] -> Q Pat
appPsInfix f = foldl1 (\l r -> uInfixP l f r)

tupT :: [Q Type] -> Q Type
tupT tyArgs = tupleT (length tyArgs) `appTs` tyArgs

vecToTupleInstances :: Integer -> Q [Dec]
vecToTupleInstances n = mapM vecToTupleInstance [3..n]

vecToTupleInstance :: Integer -> Q Dec
vecToTupleInstance n =
  instanceD
    -- No superclasses
    (pure [])

    -- Head
    (vecToTupleCon `appT` vecType)

    -- Implementation
    [ tySynInstD (tySynEqn Nothing aTypeLhs aTypeRhs)
    , funD vecToTupleFunName [clause [vecToTuplePat] (normalB vecToTupleImpl) []]
    ]

 where
  vecToTupleCon = conT (mkName "VecToTuple")
  vecType = conT ''Vec `appT` litT (numTyLit n) `appT` varT (mkName "a")

  -- associated type
  tupTypeCon = conT (mkName "TupType")
  aTypeLhs = tupTypeCon `appT` vecType
  aTypeRhs = tupT [varT (mkName "a") | _ <- [1..n]]

  -- vecToTuple
  vecToTupleFunName = mkName "vecToTuple"
  vecToTuplePat = appPsInfix '(:>) (map varP varNames ++ [wildP])
  vecToTupleImpl = tupE (map varE varNames)

  varNames = map (mkName . ('a':) . show) [1..n]
