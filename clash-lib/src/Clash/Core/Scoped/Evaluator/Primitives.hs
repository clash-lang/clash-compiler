{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  The reductions of primitives of the scoped evaluator
  ("Clash.Core.Scoped.Evaluator"), and a guide to porting the reductions of
  "Clash.GHC.Evaluator" (phase 6.2 of the port).

  = Where reductions live

  The reductions of the primitives of a module @M@ live in
  @Clash.Core.Scoped.Evaluator.Primitives.M@, mirroring
  @Clash.GHC.Evaluator.Primitives.M@ of @clash-ghc@. Each such module exports

  @
  primitives :: [(Text, PrimReduction)]
  @

  and 'scopedPrimReductions' combines those lists. It errors if a primitive
  has two reductions. A primitive without a reduction here is reduced by the
  reduction of Clash Core, through the adapter
  ("Clash.Core.Scoped.Evaluator.Adapter"). So porting a primitive is adding an
  entry to the list of its module: nothing else changes, and modules ported by
  different people don't share any lines. To add a module, create it, and add
  its import and its list here, sorted.

  They live in @clash-lib@, next to the machine, rather than in @clash-ghc@:
  the scoped machine doesn't go through the frontend's
  'Clash.Core.Evaluator.Types.Evaluator' record, and
  'Clash.Rewrite.Scoped.Whnf.evaluatorEnv' builds its environment from this
  module and the adapter. Once every primitive is ported, the adapter goes, and
  with it the scoped evaluator's dependency on the reductions of @clash-ghc@.

  A ported reduction doesn't declare what it uses of the machine: it gets the
  whole machine, heap included. The adapter gives every reduction of Clash
  Core the heap bindings its arguments reach, see
  "Clash.Core.Scoped.Evaluator.Adapter".

  = Porting a reduction

  "Clash.Core.Scoped.Evaluator.Primitives.Util" has the helpers of
  @Clash.GHC.Evaluator.Primitive.Util@, with their names and argument orders,
  and its @PrimStepContext@ has the fields of the current one. So most entries
  port by copying them and making mechanical replacements. The modules here
  have examples of every kind.

  1. Copy the entry, e.g.

      @
      , primStepEntry $(textNameLit 'GHC.Prim.gtChar#) $ \\case
          PrimStepContext{..} | Just (i,j) <- charLiterals args
            -> reduce (boolToIntLiteral (i > j))
          _ -> Nothing
      @

      into the list of the scoped module. Replace the imports of
      "Clash.Core.Evaluator.Types", @Clash.GHC.Evaluator.Primitive.Util@ and
      the syntax of Clash Core by

      @
      import Clash.Core.Scoped.Evaluator.Primitives.Util
      import Clash.Core.Scoped.Evaluator.Types
      import Clash.Core.Scoped.Syntax
      @

      Values, terms and types have the constructors of Clash Core, so
      patterns like @PrimVal p _ [_, Lit (IntegerLiteral i)]@ and terms like
      @mkApps (Data dc) [Left (Literal l)]@ stay as they are.

  2. Replace the type environment. Helpers that look at types take the
     t'Clash.Core.Scoped.TypeEnv.TypeEnv' @te@ and the scope @sc@ of the
     machine where the current ones take @tcm@, except for those that only
     look at the type constructor of the result:

      @
      sed -E -i \\
        -e 's\/\\btcm\\b\/te sc\/g' \\
        -e 's\/\\b(boolToBoolLiteral|mkIntCLit|mkFloatCLit|mkDoubleCLit|mkSomeNat|liftBitVector2CInt) te sc\\b\/\\1 te\/g' \\
        -e 's\/(liftBitVector2Bool .*) te sc \/\\1 te \/g' \\
        FILE
      @

      After that, @runExcept (tyNatSize te sc nTy)@ is the spine's
      'Clash.Core.Scoped.Type.tyNatSize', and @piResultTys te sc ty tys@ the
      spine's 'Clash.Core.Scoped.HasType.piResultTys'.

  3. Make the remaining changes, which the type checker points out:

      * __The result type.__ @ty@ is the type of the primitive, in the scope
        of the machine. Instead of taking the result type apart with
        @splitFunForallTy ty@, use @getResultTy te sc ty tys@ (the result
        type, instantiated with the type arguments), @mkResultCon te sc ty
        tys fields@ (e.g. an unboxed tuple), or @mkIntCLit@ (a boxed
        number). Look up type constructors with
        'Clash.Core.Scoped.TypeEnv.lookupTyCon' instead of @UniqMap.lookup@.

      * __Nested evaluation.__ Replace

          @
          , mach2\@Machine{mStack=[],mTerm=argWHNF} <- whnf eval tcm True (setTerm arg $ stackClear mach)
          -> ... Just mach2 {mStack = mStack mach, mTerm = e}
          @

          by @whnfArg arg $ \\mach2 argWHNF -> ... Just (Next mach2 {mTerm = e})@,
          or @reduceWith mach2 e@. The nested machine lives in an extension of
          the scope of the primitive, and has the stack of @mach@ again:
          continue in it, and 'Clash.Core.Scoped.Syntax.sink' values from
          before into it. Evaluate another term in that machine with
          @whnfIn@. See @xToBV@ in
          "Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.BitVector"
          and @(&&)@ in "Clash.Core.Scoped.Evaluator.Primitives.GHC.Classes".

      * __Allocation.__ 'Clash.Core.Scoped.Evaluator.newLetBinding' takes a
        continuation, which gets the machine in the extended scope and the
        variable ('Clash.Core.Scoped.Evaluator.Types.heapVarTerm'); continue
        with @reduceWith@. Fresh binders come from
        'Clash.Core.Scoped.Subst.withFreshIdBinder' and friends, not from a
        unique supply. See @zipWith@ and @splitAt@ in
        "Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Vector".

      * __The primitive heap.__ @primLookup@, @primInsert@ and @primUpdate@
        take and give scoped terms. A reduction that sets the term of the
        machine itself gives @Just . stay . setTerm e@ instead of
        @Just . setTerm e@, see @setByteArray#@ in
        "Clash.Core.Scoped.Evaluator.Primitives.GHC.Prim".

      * __Primitives in results__ are closed, like every
        'Clash.Core.Term.PrimInfo': build their types with closed Clash types,
        as the current reductions do. Some current reductions put a type of
        their result into a 'Clash.Core.Term.PrimInfo' (e.g. @newByteArray#@
        for @MutableByteArray#@), which mentions the type variables of their
        own type: give such a primitive a closed polymorphic type, and apply
        it to the type instead.

  4. For entries that only combine literals, @liftLit1@ and @liftLit2@ are
     shorter:

      @
      liftLit2 $(textNameLit 'GHC.Prim.andI#) intLiteral (\\i j -> integerToIntLiteral (i .&. j))
      @

  Registering the entry is adding it to the list of its module; from then on
  the scoped evaluator uses it instead of the adapter.
-}

module Clash.Core.Scoped.Evaluator.Primitives
  ( scopedPrimReductions
  , scopedPrimitives
  ) where

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

import Clash.Core.Scoped.Evaluator.Types (PrimReduction)

import qualified Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.BitVector as Clash.Sized.Internal.BitVector
import qualified Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.Unsigned as Clash.Sized.Internal.Unsigned
import qualified Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Vector as Clash.Sized.Vector
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Classes as GHC.Classes
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Num.Integer as GHC.Num.Integer
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Prim as GHC.Prim

-- | The ported reductions, by the name of their primitive. See the module
-- header.
scopedPrimReductions :: HashMap Text PrimReduction
scopedPrimReductions
  | null duplicates = HashMap.fromList scopedPrimitives
  | otherwise = error $
      "scopedPrimReductions: multiple reductions registered for: " <> show duplicates
 where
  duplicates =
    HashMap.keys $ HashMap.filter (> 1) $
      HashMap.fromListWith (+) [(name, 1 :: Int) | (name, _) <- scopedPrimitives]

-- | The ported reductions, in the order of their modules
scopedPrimitives :: [(Text, PrimReduction)]
scopedPrimitives = concat
  [ Clash.Sized.Internal.BitVector.primitives
  , Clash.Sized.Internal.Unsigned.primitives
  , Clash.Sized.Vector.primitives
  , GHC.Classes.primitives
  , GHC.Num.Integer.primitives
  , GHC.Prim.primitives
  ]
