{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.Base" in the evaluator. See
  "Clash.Core.Evaluator.Primitives" for how to write them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Evaluator.Primitives.GHC.Base
  ( primitives
  ) where

import Data.Text (Text)

import Clash.Core.Literal (Literal (..))
import Clash.Util (textNameLit)

import qualified GHC.Base

import Clash.Core.Evaluator.Primitives.Util
import Clash.Core.Evaluator.Types
import Clash.Core.Syntax

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry $(textNameLit 'GHC.Base.eqString) $ \case
      PrimStepContext{..}
        | [PrimVal _ _ [Lit (StringLiteral s1)]
          ,PrimVal _ _ [Lit (StringLiteral s2)]
          ] <- args
        -> reduce (boolToBoolLiteral te ty (s1 == s2))
        | otherwise -> error (show args)

  , primStepEntry $(textNameLit 'GHC.Base.quotInt) $ \case
      PrimStepContext{..}
        | [ DC intDc [Left (Literal (IntLiteral i))]
          , DC _     [Left (Literal (IntLiteral j))]
          ] <- args
        -> reduce (catchDivByZero (App (Data intDc) (Literal (IntLiteral (i `quot` j)))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Base.remInt) $ \case
      PrimStepContext{..}
        | [ DC intDc [Left (Literal (IntLiteral i))]
          , DC _     [Left (Literal (IntLiteral j))]
          ] <- args
        -> reduce (catchDivByZero (App (Data intDc) (Literal (IntLiteral (i `rem` j)))))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.Base.divInt) $ \case
      PrimStepContext{..}
        | [ DC intDc [Left (Literal (IntLiteral i))]
          , DC _     [Left (Literal (IntLiteral j))]
          ] <- args
        -> reduce (catchDivByZero (App (Data intDc) (Literal (IntLiteral (i `div` j)))))
      _ -> Nothing


  , primStepEntry $(textNameLit 'GHC.Base.modInt) $ \case
      PrimStepContext{..}
        | [ DC intDc [Left (Literal (IntLiteral i))]
          , DC _     [Left (Literal (IntLiteral j))]
          ] <- args
        -> reduce (catchDivByZero (App (Data intDc) (Literal (IntLiteral (i `mod` j)))))
      _ -> Nothing
  ]
