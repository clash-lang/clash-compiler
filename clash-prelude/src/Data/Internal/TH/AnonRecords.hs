{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

A TH function for deriving 'Data.AnonRecord.AsTuple' instances.
-}
module Data.Internal.TH.AnonRecords where

import Control.Monad (replicateM)
import Language.Haskell.TH
import Prelude

-- | Derive 'Data.AnonRecord.AsTuple' implementations for tuples in the specified range.
deriveAsTuple :: Int -> Int -> DecsQ
deriveAsTuple minSize maxSize = do
  let asTuple = ConT $ mkName "AsTuple"
      tupledT = ConT $ mkName "Tupled"

      fieldT  = ConT $ mkName ":="
      prodT   = ConT $ mkName ":&:"
      fieldE  = ConE $ mkName "L"
      prodE   = ConE $ mkName ":&:"
      fieldN  =        mkName "L"
      prodN   =        mkName ":&:"

      mkRecordT [(f,a)] = AppT (AppT fieldT f) a
      mkRecordT (a:b)   = AppT (AppT prodT $ mkRecordT [a]) $ mkRecordT b
      mkRecordT []      = error "cannot construct empty record"

      mkRecordE [a]     = AppE fieldE a
      mkRecordE (a:b)   = AppE (AppE prodE $ mkRecordE [a]) $ mkRecordE b
      mkRecordE []      = error "cannot construct empty record"

      mkRecordP [a]     = ConP fieldN [] [a]
      mkRecordP (a:b)   = ConP prodN [] [mkRecordP [a], mkRecordP b]
      mkRecordP []      = error "cannot construct empty record"

  fieldNames <- replicateM maxSize (newName "f")
  typeNames  <- replicateM maxSize (newName "a")
  valNames   <- replicateM maxSize (newName "x")

  return $ flip map [minSize .. maxSize] $ \tupleNum ->
    let fields = map VarT $ take tupleNum fieldNames
        types  = map VarT $ take tupleNum  typeNames
        vals   =            take tupleNum   valNames

        tupleT' = foldl AppT (TupleT tupleNum) types
        recordT = mkRecordT $ zip fields types

        context = []
        instTy = AppT asTuple recordT

        tupleP = TupP $ map VarP vals
        tupleE = TupE $ map (Just . VarE) vals

        recordP = mkRecordP $ map VarP vals
        recordE = mkRecordE $ map VarE vals

        tupled =
          TySynInstD $
          TySynEqn
            Nothing
            (AppT tupledT recordT)
            tupleT'

        toTuple =
          FunD
            (mkName "toTuple")
            [ Clause
                [recordP]
                (NormalB tupleE)
                []
            ]

        fromTuple =
          FunD
            (mkName "fromTuple")
            [ Clause
                [tupleP]
                (NormalB recordE)
                []
            ]
     in InstanceD Nothing context instTy [tupled, toTuple, fromTuple]
