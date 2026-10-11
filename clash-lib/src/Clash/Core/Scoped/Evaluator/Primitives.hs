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
-}

module Clash.Core.Scoped.Evaluator.Primitives
  ( scopedPrimReductions
  , scopedPrimitives
  ) where

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

import Clash.Core.Scoped.Evaluator.Types (PrimReduction)

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
  [ Clash.Sized.Vector.primitives
  , GHC.Classes.primitives
  , GHC.Num.Integer.primitives
  , GHC.Prim.primitives
  ]
