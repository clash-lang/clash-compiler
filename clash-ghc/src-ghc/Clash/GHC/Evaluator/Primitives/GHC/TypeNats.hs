{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.GHC.Evaluator.Primitives.GHC.TypeNats
  ( primitives
  ) where

import qualified Data.Either         as Either
import           Data.Text           (Text)

import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (Term (..), mkApps)
import Clash.Core.Type (Type (..), LitTy (..), TypeView (..), splitFunForallTy, tyView)
import Clash.Core.TyCon (tyConDataCons)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Util (textNameLit)

import qualified GHC.TypeNats

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  [ ( $(textNameLit 'GHC.TypeNats.natVal)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (NaturalLiteral n), _] <- args
            -> reduce (Literal (NaturalLiteral n))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.TypeNats.someNatVal)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (NaturalLiteral n)] <- args
            -> let resTy = getResultTy tcm ty tys
                in reduce (mkSomeNat tcm n resTy)
          _ -> Nothing
    )
  , ( $(textNameLit 'GHC.TypeNats.withSomeSNat)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Lit (NaturalLiteral n) : fun : _ <- args
            , _ : funTy : _ <- Either.rights (fst (splitFunForallTy ty))
            , (tyView -> TyConApp snatTcNm _) : _ <- Either.rights (fst (splitFunForallTy funTy))
            , Just snatTc <- UniqMap.lookup snatTcNm tcm
            , [snatDc] <- tyConDataCons snatTc
            -> let nTy = LitTy (NumTy n)
                   snat = mkApps (Data snatDc) [Right nTy, Left (Literal (NaturalLiteral n))]
                   ret = mkApps (valToTerm fun) [Right nTy, Left snat]
                in reduce ret
          _ -> Nothing
    )
  ]
