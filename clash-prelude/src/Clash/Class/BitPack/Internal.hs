{-|
Copyright  :  (C) 2019, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Class.BitPack.Internal (deriveBitPackTuples) where

import           Clash.CPP             (maxTupleSize)
import           Language.Haskell.TH.Compat (mkTySynInstD,mkTupE)
import           Control.Monad         (replicateM)
import           Data.List             (foldl')
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
  -> DecsQ
deriveBitPackTuples bitPackName bitSizeName packName unpackName = do
  let bitPack  = ConT bitPackName
      bitSize  = ConT bitSizeName
      knownNat = ConT ''KnownNat
      plus     = ConT $ mkName "+"

  allNames <- replicateM maxTupleSize (newName "a")
  retupName <- newName "retup"
  x <- newName "x"
  y <- newName "y"
  tup <- newName "tup"

  pure $ flip map [3..maxTupleSize] $ \tupleNum ->
    let names  = take tupleNum allNames
        (v:vs) = fmap VarT names
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
                        ( let (e:es) = map VarE names
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
                    let (p:ps) = map VarP names
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

    in InstanceD Nothing context instTy [bitSizeType, pack, unpack]
