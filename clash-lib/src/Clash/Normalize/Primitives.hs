{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Special primitives created during the normalization process.
-}

{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Normalize.Primitives
  ( removedArg
  , undefined
  ) where

import Prelude hiding (undefined)

import qualified Data.Text.Extra as Text

import Clash.Core.Term (IsMultiPrim(..), PrimInfo(..), WorkInfo(..))
import Clash.Core.Type (undefinedTy)

-- | The removedArg primitive represents an argument which is computationally
-- irrelevant, and has been removed from the circuit (as removing it does not
-- change the behaviour of the circuit). Examples of such arguments are unused
-- arguments to blackboxes, as removing them does not affect the rendered HDL.
--
removedArg :: PrimInfo
removedArg = PrimInfo
  { primName = Text.showt 'removedArg
  , primType = undefinedTy
  , primWorkInfo = WorkNever
  , primMultiResult = SingleResult
  }

-- | The undefined primitive represents an undefined value that was identified
-- during normalization. This includes undefined results to compile-time
-- evaluation, such as division by zero.
--
undefined :: PrimInfo
undefined = PrimInfo
  { primName = Text.showt 'undefined
  , primType = undefinedTy
  , primWorkInfo = WorkNever
  , primMultiResult = SingleResult
  }
