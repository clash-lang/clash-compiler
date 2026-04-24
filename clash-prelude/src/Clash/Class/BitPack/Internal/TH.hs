{-|
Copyright  :  (C) 2019-2024, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Class.BitPack.Internal.TH where

import           Clash.CPP             (maxTupleSize)
import           Language.Haskell.TH.Compat (mkTySynInstD,mkTupE)
import           Control.Monad         (replicateM)
#if !MIN_VERSION_base(4,20,0)
import           Data.List             (foldl')
#endif
import           GHC.TypeLits          (KnownNat)
import           Language.Haskell.TH

-- | Contruct all the tuple (starting at size 3) instances for BitPack.
deriveBitPackTuples
  :: Name
  -- ^ BitPack
  -> Name
  -- ^ BitSize
  -> Name
  -- ^ pack
  -> Name
  -- ^ unpack
  -> Name
  -- ^ maybeUnpack
  -> DecsQ
deriveBitPackTuples bitPackName bitSizeName packName unpackName maybeUnpackName = do
  let bitPack   = ConT bitPackName
      bitSize   = ConT bitSizeName
      knownNat  = ConT ''KnownNat
      plus      = ConT $ mkName "+"
      bitVector = ConT $ mkName "BitVector"
      justP     = ConP $ mkName "Just"
      justE     = ConE $ mkName "Just"
      nothing   = ConE $ mkName "Nothing"
      bvSplit   = VarE $ mkName "split#"

  allNames <- replicateM maxTupleSize (newName "a")
  retupName <- newName "retup"
  x <- newName "x"
  y <- newName "y"
  tup <- newName "tup"
  bvL <- newName "bvL"
  bvR <- newName "bvR"

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

        maybeUnpack =
            FunD
                maybeUnpackName
                [ Clause
                    [ VarP x ]
                    ( NormalB $
                        let (p,ps) = case map VarP names of
                                        (z:zs) -> (z,zs)
                                        _ -> error "maxTupleSize <= 3"
                        in
                        LetE
                            [ SigD bvL ( AppT bitVector ( AppT bitSize v ) )
                            , SigD
                                bvR
                                ( AppT
                                    bitVector
                                    ( AppT bitSize ( foldl AppT ( TupleT $ tupleNum - 1 ) vs ) )
                                )
                            , ValD
                                ( TupP [ VarP bvL, VarP bvR ] )
                                ( NormalB $ AppE bvSplit ( VarE x ) )
                                []
                            ]
                            ( CaseE
                                ( mkTupE $ map ( AppE ( VarE maybeUnpackName ) . VarE ) [bvL, bvR] )
                                [ Match
                                    ( TupP [ justP [] [p], justP [] [TupP ps] ] )
                                    ( NormalB $ AppE justE ( mkTupE $ map VarE names ) )
                                    []
                                , Match WildP ( NormalB $ nothing ) []
                                ]
                            )
                    )
                    []
                ]
    in InstanceD Nothing context instTy [bitSizeType, pack, unpack, maybeUnpack]
