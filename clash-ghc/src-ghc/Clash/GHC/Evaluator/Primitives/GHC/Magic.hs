{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.Evaluator.Primitives.GHC.Magic
  ( primitives
  ) where

import           Data.Text           (Text)

import           Clash.Core.Evaluator.Types

#if MIN_VERSION_ghc(9,10,0)
import           Clash.Core.DataCon  (DataCon (..))
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (Term (..))
import Clash.Util (textNameLit)

import qualified GHC.Magic
#endif

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  -- XXX: Does not seem to exist?
  -- 'noinlineConstraint' (if it exists) is a wired-in magic Id; not
  -- user-importable and so not TH-quotable. Match both the pre-9.14
  -- 'GHC.Magic' and the post-9.14 'GHC.Internal.Magic' module names.
  [ primStepEntry "GHC.Magic.noinlineConstraint" $ \case
      PrimStepContext{..}
        | [arg] <- args
        -> reduce (valToTerm arg)
      _ -> Nothing

  , primStepEntry "GHC.Internal.Magic.noinlineConstraint" $ \case
      PrimStepContext{..}
        | [arg] <- args
        -> reduce (valToTerm arg)
      _ -> Nothing

  -- XXX: Does not seem to exist?
  -- 'nospec' is a wired-in magic Id; not user-importable and so not
  -- TH-quotable. Match both the pre-9.14 'GHC.Magic' and the post-9.14
  -- 'GHC.Internal.Magic' module names.
  , primStepEntry "GHC.Magic.nospec" $ \case
      PrimStepContext{..}
        | [arg] <- args
        -> reduce (valToTerm arg)
      _ -> Nothing

  , primStepEntry "GHC.Internal.Magic.nospec" $ \case
      PrimStepContext{..}
        | [arg] <- args
        -> reduce (valToTerm arg)
      _ -> Nothing

#if MIN_VERSION_ghc(9,10,0)
  , primStepEntry $(textNameLit 'GHC.Magic.dataToTag#) $ \case
      PrimStepContext{..}
        | [DC dc _] <- args
        -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
      _ -> Nothing

#endif
  ]
