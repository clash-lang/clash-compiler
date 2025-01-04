{-|
Copyright  :  (C) 2024-2025, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Class.Finite.Internal.TH where

import           Clash.CPP             (maxTupleSize)
#if !MIN_VERSION_base(4,16,0)
import           Language.Haskell.TH.Compat (mkTySynInstD,mkTupE)
#endif
import           Control.Monad         (forM, replicateM)
#if !MIN_VERSION_base(4,20,0)
import           Data.List             (foldl')
#endif
import           Language.Haskell.TH

-- | Contruct all the tuple instances (starting at size 3) for 'Clash.Class.Finite.Internal.Finite'.
deriveFiniteTuples ::
  -- | Finite
  Name ->
#if MIN_VERSION_base(4,16,0)
  DecsQ
deriveFiniteTuples finiteName = do
  let finite = ConT finiteName
  allNames <- replicateM maxTupleSize $ newName "a"

  forM [3..maxTupleSize] $ \tupleNum -> do
    let names = take tupleNum allNames
        vs = case map VarT names of
                (z:zs) -> (z:zs)
                _ -> error "maxTupleSize < 3"
        tuple xs = foldl' AppT (TupleT $ length xs) xs

        -- Instance declaration
        context = AppT finite . VarT <$> names
        instTy = AppT finite $ tuple vs

    return $ InstanceD Nothing context instTy []
#else
  -- | ElementCount
  Name ->
  -- | elements
  Name ->
  -- | lowest
  Name ->
  -- | highest
  Name ->
  -- | before
  Name ->
  -- | after
  Name ->
  -- | ith
  Name ->
  -- | index
  Name ->
  DecsQ
deriveFiniteTuples finiteName elementCountName elementsName lowestName
  highestName beforeName afterName ithName indexName = do
    let finite = ConT finiteName
        elementCount = ConT elementCountName
        times = ConT $ mkName "*"

    allNames <- replicateM maxTupleSize $ newName "a"
    t2N <- newName "t2N"
    tN2 <- newName "tN2"
    x <- newName "x"

    forM [3..maxTupleSize] $ \tupleNum -> do
      let names = take tupleNum allNames
          (v,vs) = case map VarT names of
                      (z:zs) -> (z,zs)
                      _ -> error "maxTupleSize < 3"
          tuple xs = foldl' AppT (TupleT $ length xs) xs
          withConvContext b2N bN2 binds impl = return
            $ Clause binds (NormalB impl)
            $ ( if b2N then
                  (:) $ FunD t2N $ return
                    $ Clause
                        [ TupP [ p, TupP ps ]
                        | let (p,ps) = case map VarP names of
                                         (z:zs) -> (z,zs)
                                         _ -> error "maxTupleSize < 3"

                        ]
                        ( NormalB $ mkTupE $ map VarE names )
                        []
                else id
              )
            $ ( if bN2 then
                  (:) $ FunD tN2 $ return
                    $ Clause
                        [ TupP $ map VarP names ]
                        ( let (e,es) = case map VarE names of
                                         (z:zs) -> (z,zs)
                                         _ -> error "maxTupleSize < 3"
                          in NormalB (mkTupE [e,mkTupE es])
                        )
                        []
                else id
              )
              []

          -- Instance declaration
          context =
            [ finite `AppT` v
            , finite `AppT` tuple vs
            ]
          instTy = AppT finite $ tuple (v:vs)

          elementCountType =
            mkTySynInstD elementCountName [tuple (v:vs)]
              $ times `AppT` (elementCount `AppT` v) `AppT`
                (elementCount `AppT` foldl AppT (TupleT $ tupleNum - 1) vs)

          elements = FunD elementsName
            $ withConvContext True False []
            $ AppE (AppE (VarE '(<$>)) (VarE t2N))
            $ VarE elementsName

          lowest = FunD lowestName
            $ withConvContext True False []
            $ AppE (AppE (VarE '(<$>)) (VarE t2N))
            $ VarE lowestName

          highest = FunD highestName
            $ withConvContext True False []
            $ AppE (AppE (VarE '(<$>)) (VarE t2N))
            $ VarE highestName

          before = FunD beforeName
            $ withConvContext True True [ VarP x ]
            $ AppE (AppE (VarE '(<$>)) (VarE t2N))
            $ AppE (VarE beforeName)
            $ AppE (VarE tN2)
            $ VarE x

          after = FunD afterName
            $ withConvContext True True [ VarP x ]
            $ AppE (AppE (VarE '(<$>)) (VarE t2N))
            $ AppE (VarE afterName)
            $ AppE (VarE tN2)
            $ VarE x

          ith = FunD ithName
            $ withConvContext True False [ VarP x ]
            $ AppE (VarE t2N)
            $ AppE (VarE ithName)
            $ VarE x

          index = FunD indexName
            $ withConvContext False True [ VarP x ]
            $ AppE (VarE indexName)
            $ AppE (VarE tN2)
            $ VarE x

      return $ InstanceD Nothing context instTy
        [ elementCountType
        , elements
        , lowest
        , highest
        , before
        , after
        , ith
        , index
        ]
#endif
