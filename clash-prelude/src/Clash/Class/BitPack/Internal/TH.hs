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

-- | Construct all the tuple (starting at size 3) instances for BitPack.
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

  allNames <- replicateM maxTupleSize (newName "a")
  retupName <- newName "retup"
  x <- newName "x"
  tup <- newName "tup"

  pure $ flip map [3..maxTupleSize] $ \tupleNum ->
    let names  = take tupleNum allNames
        tuple xs = foldl' AppT (TupleT $ length xs) xs
        types = map VarT names

        -- Use a balanced pair tree so validation logic grows logarithmically
        -- in depth. Regrouping does not change the left-to-right bit layout.
        splitAtField = tupleNum `div` 2
        (leftNames, rightNames) = splitAt splitAtField names
        (leftTypes, rightTypes) = splitAt splitAtField types

        groupType [ty] = ty
        groupType tys = tuple tys

        groupPattern [name] = VarP name
        groupPattern groupNames = TupP (map VarP groupNames)

        groupExpression [name] = VarE name
        groupExpression groupNames = mkTupE (map VarE groupNames)

        leftType = groupType leftTypes
        rightType = groupType rightTypes
        nestedType = tuple [leftType, rightType]
        nestedPattern =
          TupP [groupPattern leftNames, groupPattern rightNames]
        nestedExpression =
          mkTupE
            [ groupExpression leftNames
            , groupExpression rightNames
            ]
        flatExpression = mkTupE (map VarE names)

        bitSizeOf ty = bitSize `AppT` ty

        -- Instance declaration
        context =
          [ bitPack `AppT` leftType
          , knownNat `AppT` bitSizeOf leftType
          , bitPack `AppT` rightType
          , knownNat `AppT` bitSizeOf rightType
          ]
        instTy = AppT bitPack (tuple types)

        -- Associated type BitSize
        bitSizeType =
          mkTySynInstD bitSizeName [tuple types]
            $ plus `AppT` bitSizeOf leftType `AppT` bitSizeOf rightType

        pack =
          FunD
            packName
            [ Clause
                [VarP tup]
                (NormalB (AppE (VarE packName) (AppE (VarE retupName) (VarE tup))))
                [FunD
                    retupName
                    [ Clause
                        [TupP (map VarP names)]
                        (NormalB nestedExpression)
                        []
                    ]
                ]
            ]

        unpack =
          FunD
            unpackName
            [ Clause
                [VarP x]
                (NormalB
                  (CaseE
                    (SigE
                      (AppE (VarE unpackName) (VarE x))
                      nestedType)
                    [ Match
                        nestedPattern
                        (NormalB flatExpression)
                        []
                    ]))
                []
            ]

        maybeUnpack =
          FunD
            maybeUnpackName
            [ Clause
                [VarP x]
                (NormalB
                  (AppE
                    (AppE
                      (VarE 'fmap)
                      (LamE [nestedPattern] flatExpression))
                    (SigE
                      (AppE (VarE maybeUnpackName) (VarE x))
                      (ConT ''Maybe `AppT` nestedType))))
                []
            ]
    in InstanceD Nothing context instTy [bitSizeType, pack, unpack, maybeUnpack]
