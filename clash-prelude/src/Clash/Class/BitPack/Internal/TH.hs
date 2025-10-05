{-|
Copyright  :  (C) 2019-2025, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Class.BitPack.Internal.TH where

import           Clash.CPP                  (maxTupleSize)
import           Language.Haskell.TH.Compat (mkTySynInstD,mkTupE)
import           Control.Monad              (replicateM)
#if !MIN_VERSION_base(4,20,0)
import           Data.List                  (foldl')
#endif
import           GHC.TypeLits               (KnownNat)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (trueName)

-- | Contruct all the tuple (starting at size 3) instances for BitPack.
deriveBitPackTuples
  :: Name
  -- ^ BitPack
  -> Name
  -- ^ BitSize
  -> Name
  -- ^ IsProductType
  -> Name
  -- ^ IsSumType
  -> Name
  -- ^ pack
  -> Name
  -- ^ unpack
  -> DecsQ
deriveBitPackTuples bitPackName bitSizeName isProductTypeName isSumTypeName
                    packName unpackName = do
  let bitPack   = ConT bitPackName
      bitSize   = ConT bitSizeName
      isSumType = ConT isSumTypeName
      knownNat  = ConT ''KnownNat
      typeTrue  = ConT trueName
      plus      = ConT $ mkName "+"
      typeOr    = ConT $ mkName "||"

  allNames <- replicateM maxTupleSize (newName "a")
  retupName <- newName "retup"
  x <- newName "x"
  y <- newName "y"
  tup <- newName "tup"

  pure $ flip map [3..maxTupleSize] $ \tupleNum ->
    let names  = take tupleNum allNames
        (v,vs) = case map VarT names of
                    (z:zs) -> (z,zs)
                    _ -> error "maxTupleSize <= 3"
        tuple xs = foldl' AppT (TupleT $ length xs) xs

        -- Instance declaration
        context =
          [ bitPack `AppT` v
          , knownNat `AppT` (bitSize `AppT` v)
          , bitPack `AppT` tuple vs
          , knownNat `AppT` (bitSize `AppT` tuple vs)
          ]
        instTy = AppT bitPack $ tuple (v:vs)

        -- Associated type BitSize
        bitSizeType =
          mkTySynInstD bitSizeName [tuple (v:vs)]
            $ plus `AppT` (bitSize `AppT` v) `AppT`
              (bitSize `AppT` foldl AppT (TupleT $ tupleNum - 1) vs)

        -- Associated type IsProductType
        isProductTypeType =
          mkTySynInstD isProductTypeName [tuple (v:vs)] typeTrue

        -- Associated type IsSumType
        isSumTypeType =
          mkTySynInstD isSumTypeName [tuple (v:vs)]
            $ typeOr `AppT` (isSumType `AppT` v) `AppT`
              (isSumType `AppT` foldl AppT (TupleT $ tupleNum - 1) vs)

        pack =
          FunD
            packName
            [ Clause
                [VarP tup]
                (NormalB (AppE (VarE packName) (AppE (VarE retupName) (VarE tup))))
                [FunD
                    retupName
                    [ Clause
                        [ TupP $ map VarP names ]
                        ( let (e,es) = case map VarE names of
                                          (z:zs) -> (z,zs)
                                          _ -> error "maxTupleSize <= 3"
                          in NormalB (mkTupE [e,mkTupE es])
                        )
                        []
                    ]
                ]
            ]

        unpack =
          FunD
            unpackName
            [ Clause
                [ VarP x ]
                ( NormalB $
                    let (p,ps) = case map VarP names of
                                   (z:zs) -> (z,zs)
                                   _ -> error "maxTupleSize <= 3"
                    in
                    LetE
                      [ ValD
                          ( TupP [ p, VarP y ] )
                          ( NormalB $ VarE unpackName `AppE` VarE x )
                          []
                      , ValD
                          ( TupP ps )
                          ( NormalB $ VarE unpackName `AppE` VarE y )
                          []
                      ]
                      ( mkTupE $ map VarE names )
                )
                []
            ]

    in InstanceD Nothing context instTy
         [bitSizeType, isProductTypeType, isSumTypeType, pack, unpack]
