{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.XException.TH
  ( mkShowXTupleInstances
  , mkNFDataXTupleInstances
  , mkShowXTupleInstance
  ) where

import Data.Either (isLeft)
import Data.List (intersperse)
import Language.Haskell.TH.Compat
import Language.Haskell.TH.Syntax

-- Spliced in in XException, so these names should be in scope:
isXName, hasUndefinedName, deepErrorXName, rnfXName, ensureSpineName :: Name
isXName = mkName "isX"
hasUndefinedName = mkName "hasUndefined"
deepErrorXName = mkName "deepErrorX"
rnfXName = mkName "rnfX"
ensureSpineName = mkName "ensureSpine"

showxName :: Name
showxName = mkName "ShowX"

showXFnName :: Name
showXFnName = mkName "showX"

showsPrecXName :: Name
showsPrecXName = mkName "showsPrecX"

nfdataxName :: Name
nfdataxName = mkName "NFDataX"

mkTup :: [Type] -> Type
mkTup names@(length -> n) =
  foldl AppT (TupleT n) names

-- | Creates an instance of the form:
--
--  instance (ShowX a0, ShowX a1) => ShowX (a0, a1)
--
-- With /n/ number of variables.
mkShowXTupleInstance :: Int -> Dec
mkShowXTupleInstance n =
  InstanceD Nothing constraints instanceTyp [showsPrecXDecl, showXDecl]
 where
  constraints = fmap (AppT (ConT showxName)) vars
  instanceTyp = ConT showxName `AppT` mkTup vars
  names = fmap (mkName . ('a':) . show) [0..n-1]
  vars = fmap VarT names

  x = mkName "x"
  s = mkName "s"

  showsPrecXDecl = FunD showsPrecXName
    [ Clause
        [WildP, VarP x, VarP s]
        (NormalB
          (VarE 'mappend `AppE` (VarE showXFnName `AppE` VarE x) `AppE` VarE s))
        []
    ]

  showXDecl = FunD showXFnName
    [ Clause
        [TupP (fmap VarP names)]
        (NormalB
          (VarE 'mconcat `AppE` (ListE
            ([LitE (StringL "(")]
               <> intersperse (LitE (StringL ",")) (fmap toShowX names)
               <> [LitE (StringL ")")]))))
        []
    ]
   where
    toShowX a = VarE showXFnName `AppE` VarE a

-- | Creates instances of ShowX for all tuple sizes listed.
-- See 'mkShowXTupleInstance' for more information.
mkShowXTupleInstances :: [Int] -> Q [Dec]
mkShowXTupleInstances tupSizes =
  return (fmap mkShowXTupleInstance tupSizes)

-- | Creates an instance of the form:
--
--  instance (NFDataX a0, NFDataX a1) => NFDataX (a0, a1)
--
-- With /n/ number of variables.
mkNFDataXTupleInstance :: Int -> Dec
mkNFDataXTupleInstance n =
  InstanceD
    Nothing
    constraints
    instanceTyp
    [ ensureSpineDecl
    , hasUndefinedDecl
    , deepErrorXDecl
    , rnfXDecl
    ]
 where
  constraints = map (AppT (ConT nfdataxName)) vars
  instanceTyp = ConT nfdataxName `AppT` mkTup vars
  names = map (mkName . ('a':) . show) [0..n-1]
  vars = map VarT names

  t = mkName "t"
  s = mkName "s"

  rnfXDecl = FunD rnfXName [
    Clause
      [AsP t (TildeP (TupP (map VarP names)))]
      (NormalB (
        CondE
          (VarE 'isLeft `AppE` (VarE isXName `AppE` VarE t))
          (TupE [])
          (foldl
            (\e1 e2 -> UInfixE e1 (VarE 'seq) (VarE rnfXName `AppE` e2))
            (VarE rnfXName `AppE` VarE (head names))
            (map VarE (tail names)))
      ))
      []
    ]

  hasUndefinedDecl = FunD hasUndefinedName [
    Clause
      [AsP t (TildeP (TupP (map VarP names)))]
      (NormalB (
        CondE
          (VarE 'isLeft `AppE` (VarE isXName `AppE` VarE t))
          (ConE 'True)
          (VarE 'or `AppE` ListE
            (map ((VarE hasUndefinedName `AppE`) . VarE) names))
      ))
      []
    ]

  ensureSpineDecl = FunD ensureSpineName  [
    Clause
      [TildeP (TupP (map VarP names))]
      (NormalB (mkTupE (map (AppE (VarE ensureSpineName) . VarE) names)))
      []
    ]

  deepErrorXDecl = FunD deepErrorXName [
     Clause
       [VarP s]
       (NormalB (mkTupE (replicate n (VarE deepErrorXName `AppE` VarE s))))
       []
     ]

mkNFDataXTupleInstances :: [Int] -> Q [Dec]
mkNFDataXTupleInstances tupSizes =
  pure (map mkNFDataXTupleInstance tupSizes)
