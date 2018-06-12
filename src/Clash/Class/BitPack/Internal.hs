{-# LANGUAGE TemplateHaskell #-}

module Clash.Class.BitPack.Internal where

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
  -> Name
  -- ^ append (++#)
  -> DecsQ
deriveBitPackTuples bitPackName bitSizeName packName unpackName appendName = do
  let bitPack  = ConT bitPackName
      bitSize  = ConT bitSizeName
      knownNat = ConT ''KnownNat
      plus     = ConT $ mkName "+"

  allNames <- replicateM 62 (newName "a")
  x <- newName "x"
  y <- newName "y"

  pure $ flip map [3..62] $ \tupleNum ->
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
        bitSizeTypeEq =
          TySynEqn
            [ tuple (v:vs) ]
            $ plus `AppT` (bitSize `AppT` v) `AppT`
              (bitSize `AppT` foldl AppT (TupleT $ tupleNum - 1) vs)
        bitSizeType = TySynInstD bitSizeName bitSizeTypeEq

        pack =
          FunD
            packName
            [ Clause
                [ TupP $ map VarP names ]
                ( let (e:es) = map VarE names
                  in NormalB $ AppE
                    (VarE appendName `AppE` (VarE packName `AppE` e))
                    (VarE packName `AppE` TupE es)
                )
                []
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
                      ( TupE $ map VarE names )
                )
                []
            ]

    in InstanceD Nothing context instTy [bitSizeType, pack, unpack]
