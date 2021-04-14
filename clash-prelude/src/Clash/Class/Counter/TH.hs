{-# LANGUAGE CPP #-}

module Clash.Class.Counter.TH where

import Language.Haskell.TH

counterName, countMinName, countMaxName, countSuccName, countPredName :: Name
counterName = mkName "Counter"
countMinName = mkName "countMin"
countMaxName = mkName "countMax"
countSuccName = mkName "countSuccOverflow"
countPredName = mkName "countPredOverflow"

mkTupTy :: [Type] -> Type
mkTupTy names@(length -> n) = foldl AppT (TupleT n) names

mkTup :: [Exp] -> Exp
#if MIN_VERSION_template_haskell(2,16,0)
mkTup = TupE . map Just
#else
mkTup = TupE
#endif

genTupleInstances :: Int -> Q [Dec]
genTupleInstances maxTupleSize = mapM genTupleInstance [3..maxTupleSize]

genTupleInstance :: Int -> Q Dec
genTupleInstance tupSize = do
  typeVars <- mapM (\n -> VarT <$> newName ("a" <> show n)) [0..tupSize-1]

  succOverflowBody <- genCountOverflow countSuccName tupSize
  predOverflowBody <- genCountOverflow countPredName tupSize

  let
    minBody = genCount countMinName tupSize
    maxBody = genCount countMaxName tupSize
    ctx = map (ConT counterName `AppT`) typeVars
    typ = ConT counterName `AppT` mkTupTy typeVars
    decls =
      [ FunD countMinName [minBody]
      , FunD countMaxName [maxBody]
      , FunD (mkName "countSuccOverflow") [succOverflowBody]
      , FunD (mkName "countPredOverflow") [predOverflowBody]
      ]

  pure (InstanceD Nothing ctx typ decls)

genCount :: Name -> Int -> Clause
genCount nm n = Clause [] (NormalB (mkTup (replicate n (VarE nm)))) []

genCountOverflow :: Name -> Int -> Q Clause
genCountOverflow nm tupSize = do
  varNms <- mapM (\n -> newName ("a" <> show n)) [0..tupSize-1]
  let vars = map VarE varNms

  overflowLastNm <- newName "overflowLast"
  lastNm <- newName "last"

  overflowInitNm <- newName "overflowInit"
  initNms <- mapM (\n -> newName ("a" <> show n)) [0..tupSize-2]

  let
    body =
      CondE
        (VarE overflowLastNm)
        (mkTup [VarE overflowInitNm, mkTup (map VarE (initNms <> [lastNm]))])
        (mkTup [VarE overflowLastNm, mkTup (init vars <> [VarE lastNm])])

    decs =
      [ ValD
          (TupP [VarP overflowLastNm, VarP lastNm])
          (NormalB (VarE nm `AppE` last vars))
          []

      , ValD
          (TupP [VarP overflowInitNm, TupP (map VarP initNms)])
          (NormalB (VarE nm `AppE` mkTup (init vars)))
          []
      ]

  pure (Clause [TupP (map VarP varNms)] (NormalB body) decs)
