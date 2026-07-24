{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Clash.GHC.Evaluator.Primitives.Data.Text.Show
  ( primitives
  ) where

import qualified Data.Primitive.ByteArray as BA
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.Array     as Text
import qualified Data.Text.Internal  as Text

import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (Term (..), mkApps)
import Clash.Core.Type (TypeView (..), splitFunForallTy, tyView)
import Clash.Core.TyCon (tyConDataCons)
import qualified Clash.Data.UniqMap as UniqMap

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  [ ( "Data.Text.Show.$wunpackCStringAscii#"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (StringLiteral addr)] <- args
            , Text.Text (Text.ByteArray ba) _off len <- Text.pack addr
            -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
                   (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc] = tyConDataCons tupTc
                   ret     = mkApps (Data tupDc) (map Right tyArgs ++
                            [ Left (Literal (ByteArrayLiteral (BA.ByteArray ba)))
                            , Left (Literal (IntLiteral 0))
                            , Left (Literal (IntLiteral (toInteger len)))])
                in reduce ret
          _ -> Nothing
    )
  ]
