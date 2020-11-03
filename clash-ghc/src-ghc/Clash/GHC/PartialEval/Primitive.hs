{-|
Copyright   : (C) 2020, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Evaluation of primitive operations in the partial evaluator. This is used
by the Clash.GHC.PartialEval.Eval module to implement fully applied primitives.
-}

module Clash.GHC.PartialEval.Primitive
  ( evalPrimitive
  ) where

import Clash.Core.PartialEval.Monad
import Clash.Core.PartialEval.NormalForm
import Clash.Core.Term (Term, PrimInfo)

-- | Evaluate a primitive with the given arguments.
-- See NOTE [Evaluating primitives] for more information.
--
evalPrimitive
  :: (Term -> Eval Value)
  -- ^ Evaluation function for forcing arguments
  -> PrimInfo
  -- ^ The primitive to evaluate
  -> Args Value
  -- ^ The arguments supplied to the primitive
  -> Eval Value
  -- ^ The result of evaluating the primitive
evalPrimitive _eval pr args =
  -- TODO Implement evaluation of primitives.
  pure (VNeutral (NePrim pr args))

{-
NOTE [Evaluating primitives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the evaluator encounters a primitive operation with all arguments applied,
it will attempt to evaluate it. If this is possible, the call to the primitive
will be replaced with the result. However, it may not be possible to evaluate
a primitive if not all arguments are statically known (i.e. if an argument is
a variable with an unknown value). In this case, a neutral primitive is
returned instead.

Some primitives do not evaluate, and are deliberately preserved in the result
of the evaluator as neutral primitives. Notable examples of this are

  * GHC.CString.unpackCString#
  * Clash.Sized.Internal.BitVector.fromInteger##
  * Clash.Sized.Internal.BitVector.fromInteger#
  * Clash.Sized.Internal.Index.fromInteger#
  * Clash.Sized.Internal.Signed.fromInteger#
  * Clash.Sized.Internal.Unsigned.fromInteger#

Some primitives may throw exceptions (such as division by zero) or need to
perform IO (e.g. primitives on ByteArray#). These effects are supported by the
Eval monad, see Clash.Core.PartialEval.Monad.
-}
