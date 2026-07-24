{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.GHC.Evaluator.Primitives
  ( ghcPrimStepImpls
  ) where

import qualified Data.HashMap.Strict as HashMap
import           Data.Text           (Text)

import           Clash.Core.Evaluator.Types

import qualified Clash.GHC.Evaluator.Primitives.Clash.Annotations.BitRepresentation.Deriving as Clash.Annotations.BitRepresentation.Deriving
import qualified Clash.GHC.Evaluator.Primitives.Clash.Class.BitPack.Internal as Clash.Class.BitPack.Internal
import qualified Clash.GHC.Evaluator.Primitives.Clash.Class.Exp as Clash.Class.Exp
import qualified Clash.GHC.Evaluator.Primitives.Clash.Promoted.Nat as Clash.Promoted.Nat
import qualified Clash.GHC.Evaluator.Primitives.Clash.Sized.Internal.BitVector as Clash.Sized.Internal.BitVector
import qualified Clash.GHC.Evaluator.Primitives.Clash.Sized.Internal.Index as Clash.Sized.Internal.Index
import qualified Clash.GHC.Evaluator.Primitives.Clash.Sized.Internal.Signed as Clash.Sized.Internal.Signed
import qualified Clash.GHC.Evaluator.Primitives.Clash.Sized.Internal.Unsigned as Clash.Sized.Internal.Unsigned
import qualified Clash.GHC.Evaluator.Primitives.Clash.Sized.RTree as Clash.Sized.RTree
import qualified Clash.GHC.Evaluator.Primitives.Clash.Sized.Vector as Clash.Sized.Vector
import qualified Clash.GHC.Evaluator.Primitives.Data.Singletons.TypeLits.Internal as Data.Singletons.TypeLits.Internal
import qualified Clash.GHC.Evaluator.Primitives.Data.Text.Show as Data.Text.Show
import qualified Clash.GHC.Evaluator.Primitives.GHC.Base as GHC.Base
import qualified Clash.GHC.Evaluator.Primitives.GHC.Classes as GHC.Classes
import qualified Clash.GHC.Evaluator.Primitives.GHC.Float as GHC.Float
import qualified Clash.GHC.Evaluator.Primitives.GHC.Int as GHC.Int
import qualified Clash.GHC.Evaluator.Primitives.GHC.Internal.Float as GHC.Internal.Float
import qualified Clash.GHC.Evaluator.Primitives.GHC.Internal.Real as GHC.Internal.Real
import qualified Clash.GHC.Evaluator.Primitives.GHC.Magic as GHC.Magic
import qualified Clash.GHC.Evaluator.Primitives.GHC.Num as GHC.Num
import qualified Clash.GHC.Evaluator.Primitives.GHC.Num.BigNat as GHC.Num.BigNat
import qualified Clash.GHC.Evaluator.Primitives.GHC.Num.Integer as GHC.Num.Integer
import qualified Clash.GHC.Evaluator.Primitives.GHC.Num.Natural as GHC.Num.Natural
import qualified Clash.GHC.Evaluator.Primitives.GHC.Prim as GHC.Prim
import qualified Clash.GHC.Evaluator.Primitives.GHC.PrimopWrappers as GHC.PrimopWrappers
import qualified Clash.GHC.Evaluator.Primitives.GHC.Real as GHC.Real
import qualified Clash.GHC.Evaluator.Primitives.GHC.TypeLits as GHC.TypeLits
import qualified Clash.GHC.Evaluator.Primitives.GHC.TypeNats as GHC.TypeNats
import qualified Clash.GHC.Evaluator.Primitives.GHC.Types as GHC.Types
import qualified Clash.GHC.Evaluator.Primitives.GHC.Word as GHC.Word

ghcPrimStepImpls :: HashMap.HashMap Text PrimStep
ghcPrimStepImpls = HashMap.fromList $ concat
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
