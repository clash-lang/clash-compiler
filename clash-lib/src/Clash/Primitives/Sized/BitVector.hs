{-|
  Copyright   :  (C) 2026 QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Blackbox implementations for functions in "Clash.Sized.BitVector".
-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Primitives.Sized.BitVector (indexBitVectorCV) where

import Clash.Netlist.BlackBox (isLiteral)
import Clash.Netlist.BlackBox.Parser (runParse)
import Clash.Netlist.BlackBox.Types
  ( BlackBoxFunction
  , BlackBoxMeta (..)
  , TemplateKind (TExpr)
  , emptyBlackBoxMeta
  )
import Clash.Netlist.Types (BlackBox (BBTemplate))
import Text.Trifecta.Result (Result (Success))

-- index# :: KnownNat n => BitVector n -> Int -> Bit
indexBitVectorCV :: BlackBoxFunction
indexBitVectorCV _isD _primName args _ty =
  pure $ case runParse template of
    Success t -> Right (emptyBlackBoxMeta{bbKind = TExpr}, BBTemplate t)
    _ -> Left "indexBitVectorCV: could not parse index template"
 where
  -- Select the template before validation: a ~LIT inside a ~IF would also
  -- require a literal argument when that branch is not selected.
  template
    | [_nTy, _kn, _bv, Left ix] <- args
    , isLiteral ix
    = "~VAR[bv][1][~LIT[2]]"
    | otherwise
    = "~VAR[bv][1][~ARG[2]]"
