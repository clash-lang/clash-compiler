{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.GHC.Evaluator.Primitives.GHC.Base
  ( primitives
  ) where

import           Data.Text           (Text)

import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (Term (..))
import Clash.Util (textNameLit)

import qualified GHC.Base

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  [ primStepEntry $(textNameLit 'GHC.Base.eqString) $ \case
      PrimStepContext{..}
        | [PrimVal _ _ [Lit (StringLiteral s1)]
          ,PrimVal _ _ [Lit (StringLiteral s2)]
          ] <- args
        -> reduce (boolToBoolLiteral tcm ty (s1 == s2))
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
