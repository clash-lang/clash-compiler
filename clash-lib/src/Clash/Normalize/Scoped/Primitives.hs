{-|
  Copyright   :  (C) 2021-2026, QBayLogic B.V.,
                     2022     , Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Special primitives created during the normalization process. Each has the
  type @forall a. a@, and is applied to the type of the term it stands for.
-}

module Clash.Normalize.Scoped.Primitives
  ( removedArg
  , undefined
  , undefinedX
  ) where

import Prelude hiding (undefined)

import Data.Text (Text)

import Clash.Core.Basic
  (IsMultiPrim (..), WorkInfo (..), removedArgName, undefinedName, undefinedXName)

import Clash.Core.Syntax (mkPrimInfo)
import Clash.Core.Syntax (PrimInfo, PrimUnfolding (..))
import Clash.Core.TysPrim (undefinedTy)

-- | The removedArg primitive represents an argument which is computationally
-- irrelevant, and has been removed from the circuit (as removing it does not
-- change the behaviour of the circuit). Examples of such arguments are unused
-- arguments to blackboxes, as removing them does not affect the rendered HDL.
removedArg :: PrimInfo
removedArg = special removedArgName

-- | The undefined primitive represents an undefined value that was identified
-- during normalization. This includes undefined results to compile-time
-- evaluation, such as division by zero.
undefined :: PrimInfo
undefined = special undefinedName

-- | The undefinedX primitive represents an X-exception throwing value that was
-- identified during normalization.
undefinedX :: PrimInfo
undefinedX = special undefinedXName

special :: Text -> PrimInfo
special nm = mkPrimInfo nm undefinedTy WorkNever SingleResult NoUnfolding
