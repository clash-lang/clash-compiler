{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Clash.GHC.Evaluator.Primitives.GHC.Types
  ( primitives
  ) where

import           Data.Text           (Text)

import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (Term (..), mkApps)
import Clash.Core.Type (TypeView (..), splitFunForallTy, tyView)
import Clash.Core.TyCon (tyConDataCons)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Util (textNameLit)

import qualified GHC.Types

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  [ ( $(textNameLit 'GHC.Types.I#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [Lit (IntLiteral i)] <- args
            ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
                    (Just intTc) = UniqMap.lookup intTcNm tcm
                    [intDc] = tyConDataCons intTc
                in  reduce (mkApps (Data intDc) [Left (Literal (IntLiteral i))])
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Types.W#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [Lit (WordLiteral i)] <- args
            ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
                    (Just intTc) = UniqMap.lookup intTcNm tcm
                    [intDc] = tyConDataCons intTc
                in  reduce (mkApps (Data intDc) [Left (Literal (WordLiteral i))])
          _ -> Nothing
    )
  ]
