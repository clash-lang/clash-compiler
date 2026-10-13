{-|
  Copyright   :  (C) 2021-2026, QBayLogic B.V.,
                     2022     , Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Special primitives created during the normalization process, in scoped
  Clash Core: those of "Clash.Normalize.Primitives", with the same names.
  Each has the type @forall a. a@, and is applied to the type of the term it
  stands for.
-}

{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Normalize.Scoped.Primitives
  ( removedArg
  , undefined
  , undefinedX
  ) where

import Prelude hiding (undefined)

import Data.Text (Text)
import qualified Data.Text.Extra as Text

import Clash.Core.Term (IsMultiPrim (..), WorkInfo (..))
import qualified Clash.Normalize.Primitives as C

import Clash.Core.Scoped.Convert (mkPrimInfo)
import Clash.Core.Scoped.Syntax (PrimInfo, PrimUnfolding (..))
import Clash.Core.Scoped.TysPrim (undefinedTy)

-- | An argument which is computationally irrelevant, and has been removed from
-- the circuit. See 'C.removedArg'.
removedArg :: PrimInfo
removedArg = special (Text.showt 'C.removedArg)

-- | An undefined value that was identified during normalization. See
-- 'C.undefined'.
undefined :: PrimInfo
undefined = special (Text.showt 'C.undefined)

-- | An X-exception throwing value that was identified during normalization.
-- See 'C.undefinedX'.
undefinedX :: PrimInfo
undefinedX = special (Text.showt 'C.undefinedX)

special :: Text -> PrimInfo
special nm = mkPrimInfo nm undefinedTy WorkNever SingleResult NoUnfolding
