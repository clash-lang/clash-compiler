{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Clash.GHC.Evaluator.Primitives.GHC.Num.Natural
  ( primitives
  ) where

import           Data.Text           (Text)
import           GHC.Num.Natural     (naturalSubUnsafe)

import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (Term (..))
import Clash.Core.Type (splitFunForallTy)
import Clash.Util (textNameLit)

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  [ ( $(textNameLit 'GHC.Num.Natural.naturalSubUnsafe)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | Just (i,j) <- naturalLiterals args
            ->
             let nTy = snd (splitFunForallTy ty) in
             reduce (checkNaturalRange nTy [i, j] (\[i', j'] ->
              naturalToNaturalLiteral (naturalSubUnsafe i' j')))
          _ -> Nothing
    )

  , ( "GHC.Num.Natural.$wnaturalSignum"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [i] <- naturalLiterals' args
            -> reduce (Literal (WordLiteral (signum i)))
          _ -> Nothing
    )
  ]
