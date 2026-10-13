{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  The reductions of primitives of the scoped evaluator
  ("Clash.Core.Scoped.Evaluator"), and a guide to writing them.

  = Where reductions live

  The reductions of the primitives of a module @M@ live in
  @Clash.Core.Scoped.Evaluator.Primitives.M@. Each such module exports

  @
  primitives :: [(Text, PrimReduction)]
  @

  and 'scopedPrimReductions' combines those lists. It errors if a primitive
  has two reductions. A primitive without a reduction here doesn't reduce. So
  adding a reduction is adding an entry to the list of its module: nothing
  else changes. To add a module, create it, and add its import and its list
  here, sorted.

  They live in @clash-lib@, next to the machine:
  'Clash.Rewrite.Scoped.Whnf.evaluatorEnv' builds the environment of the
  machine from this module.

  A reduction doesn't declare what it uses of the machine: it gets the
  whole machine, heap included.

  = Writing a reduction

  "Clash.Core.Scoped.Evaluator.Primitives.Util" has the helpers, and its
  @PrimStepContext@ the arguments of a reduction. The modules here have
  examples of every kind.

  1. An entry associates the name of a primitive with its reduction, e.g.

      @
      , primStepEntry $(textNameLit 'GHC.Prim.gtChar#) $ \\case
          PrimStepContext{..} | Just (i,j) <- charLiterals args
            -> reduce (boolToIntLiteral (i > j))
          _ -> Nothing
      @

      with the imports

      @
      import Clash.Core.Scoped.Evaluator.Primitives.Util
      import Clash.Core.Scoped.Evaluator.Types
      import Clash.Core.Scoped.Syntax
      @

  2. Helpers that look at types take the
     t'Clash.Core.Scoped.TypeEnv.TypeEnv' @te@ and the scope @sc@ of the
     machine, except for those that only look at the type constructor of the
     result, such as @boolToBoolLiteral@ and @mkIntCLit@, which only take
     @te@. @runExcept (tyNatSize te sc nTy)@ is the size of a type-level
     natural ('Clash.Core.Scoped.Type.tyNatSize'), and @piResultTys te sc ty
     tys@ instantiates a type ('Clash.Core.Scoped.HasType.piResultTys').

  3. Some things need care, but the type checker points them out:

      * __The result type.__ @ty@ is the type of the primitive, in the scope
        of the machine. Use @getResultTy te sc ty tys@ (the result type,
        instantiated with the type arguments), @mkResultCon te sc ty tys
        fields@ (e.g. an unboxed tuple), or @mkIntCLit@ (a boxed number).
        Look up type constructors with
        'Clash.Core.Scoped.TypeEnv.lookupTyCon'.

      * __Nested evaluation.__ Evaluate an argument with
        @whnfArg arg $ \\mach2 argWHNF -> ... Just (Next mach2 {mTerm = e})@,
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
        'Clash.Core.Scoped.Subst.withFreshIdBinder' and friends. See
        @zipWith@ and @splitAt@ in
        "Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Vector".

      * __The primitive heap.__ @primLookup@, @primInsert@ and @primUpdate@
        take and give closed terms. A reduction that sets the term of the
        machine itself gives @Just . stay . setTerm e@, see @setByteArray#@
        in "Clash.Core.Scoped.Evaluator.Primitives.GHC.Prim".

      * __Primitives in results__ are closed, like every
        'Clash.Core.Term.PrimInfo': build their types with closed Clash
        types. A primitive whose type mentions type variables of the type of
        the reduced primitive (e.g. @MutableByteArray#@ in the result of
        @newByteArray#@) gets a closed polymorphic type, and is applied to the
        type instead.

  4. For entries that only combine literals, @liftLit1@ and @liftLit2@ are
     shorter:

      @
      liftLit2 $(textNameLit 'GHC.Prim.andI#) intLiteral (\\i j -> integerToIntLiteral (i .&. j))
      @

  = Testing a reduction

  Unit tests check reductions on fixed inputs against expected results, see
  "Clash.Tests.Core.Scoped.Evaluator.Primitives" in the unit tests of
  @clash-lib@.
-}

module Clash.Core.Scoped.Evaluator.Primitives
  ( scopedPrimReductions
  , scopedPrimitives
  ) where

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

import Clash.Core.Scoped.Evaluator.Types (PrimReduction)

import qualified Clash.Core.Scoped.Evaluator.Primitives.Clash.Class.BitPack.Internal as Clash.Class.BitPack.Internal
import qualified Clash.Core.Scoped.Evaluator.Primitives.Clash.Annotations.BitRepresentation.Deriving as Clash.Annotations.BitRepresentation.Deriving
import qualified Clash.Core.Scoped.Evaluator.Primitives.Clash.Class.Exp as Clash.Class.Exp
import qualified Clash.Core.Scoped.Evaluator.Primitives.Clash.Promoted.Nat as Clash.Promoted.Nat
import qualified Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.BitVector as Clash.Sized.Internal.BitVector
import qualified Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.Index as Clash.Sized.Internal.Index
import qualified Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.Signed as Clash.Sized.Internal.Signed
import qualified Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Internal.Unsigned as Clash.Sized.Internal.Unsigned
import qualified Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.RTree as Clash.Sized.RTree
import qualified Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Vector as Clash.Sized.Vector
import qualified Clash.Core.Scoped.Evaluator.Primitives.Data.Singletons.TypeLits.Internal as Data.Singletons.TypeLits.Internal
import qualified Clash.Core.Scoped.Evaluator.Primitives.Data.Text.Show as Data.Text.Show
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Base as GHC.Base
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Classes as GHC.Classes
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Magic as GHC.Magic
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Float as GHC.Float
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Int as GHC.Int
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Internal.Float as GHC.Internal.Float
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Internal.Real as GHC.Internal.Real
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Num as GHC.Num
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Num.BigNat as GHC.Num.BigNat
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Num.Integer as GHC.Num.Integer
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Num.Natural as GHC.Num.Natural
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Prim as GHC.Prim
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.PrimopWrappers as GHC.PrimopWrappers
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Real as GHC.Real
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.TypeLits as GHC.TypeLits
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.TypeNats as GHC.TypeNats
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Types as GHC.Types
import qualified Clash.Core.Scoped.Evaluator.Primitives.GHC.Word as GHC.Word

-- | The reductions, by the name of their primitive. See the module
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

-- | The reductions, in the order of their modules
scopedPrimitives :: [(Text, PrimReduction)]
scopedPrimitives = concat
  [ Clash.Annotations.BitRepresentation.Deriving.primitives
  , Clash.Class.BitPack.Internal.primitives
  , Clash.Class.Exp.primitives
  , Clash.Promoted.Nat.primitives
  , Clash.Sized.Internal.BitVector.primitives
  , Clash.Sized.Internal.Index.primitives
  , Clash.Sized.Internal.Signed.primitives
  , Clash.Sized.Internal.Unsigned.primitives
  , Clash.Sized.RTree.primitives
  , Clash.Sized.Vector.primitives
  , Data.Singletons.TypeLits.Internal.primitives
  , Data.Text.Show.primitives
  , GHC.Base.primitives
  , GHC.Classes.primitives
  , GHC.Float.primitives
  , GHC.Int.primitives
  , GHC.Internal.Float.primitives
  , GHC.Internal.Real.primitives
  , GHC.Magic.primitives
  , GHC.Num.primitives
  , GHC.Num.BigNat.primitives
  , GHC.Num.Integer.primitives
  , GHC.Num.Natural.primitives
  , GHC.Prim.primitives
  , GHC.PrimopWrappers.primitives
  , GHC.Real.primitives
  , GHC.TypeLits.primitives
  , GHC.TypeNats.primitives
  , GHC.Types.primitives
  , GHC.Word.primitives
  ]
