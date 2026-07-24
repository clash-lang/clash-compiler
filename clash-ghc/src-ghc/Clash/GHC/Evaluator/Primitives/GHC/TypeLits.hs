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

module Clash.GHC.Evaluator.Primitives.GHC.TypeLits
  ( primitives
  ) where

import           Data.Text           (Text)

import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import Clash.Util (textNameLit)

import qualified GHC.TypeLits

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  -- XXX: Does it make sense to match on a @NaturalLiteral@ here?
  [ ( $(textNameLit 'GHC.TypeLits.natVal)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (NaturalLiteral n), _] <- args
            -> reduce (integerToIntegerLiteral n)
          _ -> Nothing
    )

  -- XXX: Does it make sense to match on a @NaturalLiteral@ here?
  , ( $(textNameLit 'GHC.TypeLits.someNatVal)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [Lit (NaturalLiteral n)] <- args
            -> let resTy = getResultTy tcm ty tys
                in reduce (mkSomeNat tcm n resTy)
          _ -> Nothing
    )
  ]
