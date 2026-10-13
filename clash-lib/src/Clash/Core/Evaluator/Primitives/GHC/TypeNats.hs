{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.TypeNats" in the scoped evaluator. See
  "Clash.Core.Evaluator.Primitives" for how to write them.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Evaluator.Primitives.GHC.TypeNats
  ( primitives
  ) where

import Data.Text (Text)

import Clash.Core.Basic (LitTy (..))
import Clash.Core.Literal (Literal (..))
import Clash.Util (textNameLit)

import qualified GHC.TypeNats

import Clash.Core.Evaluator.Primitives.Util
import Clash.Core.Evaluator.Types
import Clash.Core.Syntax
import Clash.Core.Term (mkApps)

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry $(textNameLit 'GHC.TypeNats.natVal) $ \case
      PrimStepContext{..}
        | [Lit (NaturalLiteral n), _] <- args
        -> reduce (Literal (NaturalLiteral n))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.TypeNats.someNatVal) $ \case
      PrimStepContext{..}
        | [Lit (NaturalLiteral n)] <- args
        -> let resTy = getResultTy te sc ty tys
            in reduce (mkSomeNat te n resTy)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.TypeNats.withSomeSNat) $ \case
      PrimStepContext{..}
        | Lit (NaturalLiteral n) : fun : _ <- args
        -- 'GHC.TypeNats.SNat' is a newtype over 'Natural', which Clash
        -- represents by the value it wraps, so the literal is the 'SNat'.
        -> let nTy = LitTy (NumTy n)
               snat = Literal (NaturalLiteral n)
               ret = mkApps (valToTerm fun) [Right nTy, Left snat]
            in reduce ret
      _ -> Nothing

  ]
