{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

#if MIN_VERSION_ghc(9,12,0)
-- We'll need to support deprecated primitives too
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif

#include "MachDeps.h"

module Clash.GHC.Evaluator.Primitives
  ( ghcPrimStepImpls
  ) where

import           Control.DeepSeq            (force)
import           Control.Exception          (ArithException(..), ErrorCall, Exception, tryJust, evaluate)
import qualified Control.Lens               as Lens
import           Control.Monad.State.Strict (State, MonadState)
import qualified Control.Monad.State.Strict as State
import           Control.Monad.Trans.Except (runExcept)
import           Data.Bits
import qualified Data.ByteString.Internal as BS
import           Data.Char           (chr,ord)
import qualified Data.Either         as Either
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.List           as List
import qualified Data.Primitive.ByteArray as BA
import           Data.Proxy          (Proxy)
import           Data.Reflection     (reifyNat)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Text.Extra     (showt)
import           GHC.Exts (IsList(..))
import           GHC.Float
import           GHC.Int
import           GHC.Integer
  (decodeDoubleInteger,encodeDoubleInteger,compareInteger,orInteger,andInteger,
   xorInteger,complementInteger,absInteger,signumInteger)
import           GHC.Num.BigNat      (bigNatEq#)
import           GHC.Num.Integer (Integer (..), integerEncodeFloat#)
import           GHC.Num.Natural     (naturalSubUnsafe)
import           GHC.Natural
import           GHC.ForeignPtr
import           GHC.Prim
import           GHC.Real            (Ratio (..))
import           GHC.TypeLits        (KnownNat)
import           GHC.Types           (IO (..))
import           GHC.Word
import           System.IO.Unsafe    (unsafeDupablePerformIO)
import           Data.Bifunctor      (first)
import qualified Data.Text.Array     as Text
import qualified Data.Text.Internal  as Text

import           GHC.Types.Basic     (Boxity (..))
import           GHC.Types.Name      (getSrcSpan, nameOccName, occNameString)
import           GHC.Builtin.Names   (trueDataConKey, falseDataConKey)
import qualified GHC.Core.TyCon      as TyCon
import           GHC.Builtin.Types   (tupleTyCon)

import           Clash.Class.BitPack (pack,unpack)
import           Clash.Core.DataCon  (DataCon (..))
import           Clash.Core.Evaluator.Types
import           Clash.Core.FreeVars (typeFreeVars)
import           Clash.Core.HasType  (piResultTys, applyTypeToArgs)
import           Clash.Core.Literal  (Literal (..))
import           Clash.Core.Name
  (Name (..), NameSort (..), mkUnsafeSystemName)
import           Clash.Core.Pretty   (showPpr)
import           Clash.Core.Subst    (extendTvSubst, mkSubst, substTy)
import           Clash.Core.Term
  (IsMultiPrim (..), Pat (..), PrimInfo (..), Term (..), WorkInfo (..), mkApps,
   PrimUnfolding(..), collectArgs)
import           Clash.Core.Type
  (Type (..), ConstTy (..), LitTy (..), TypeView (..), mkFunTy, mkTyConApp,
   normalizeType, splitFunForallTy, tyView)
import           Clash.Core.TyCon
  (TyConMap, TyConName, tyConDataCons)
import           Clash.Core.TysPrim
import           Clash.Core.Util
  (mkRTree,mkVec,tyNatSize,dataConInstArgTys,primCo, mkSelectorCase,undefinedPrims,
   undefinedXPrims)
import           Clash.Core.Var      (mkLocalId, mkTyVar)
import qualified Clash.Data.UniqMap as UniqMap
import           Clash.Debug
import           Clash.GHC.GHC2Core  (modNameM)
import           Clash.Unique        (fromGhcUnique)
import           Clash.Util
  (MonadUnique (..), clogBase, flogBase, curLoc, textNameLit)
import           Clash.Util.Supply   (Supply,freshId)
import           Clash.Normalize.PrimitiveReductions
  (typeNatMul, typeNatSub, typeNatAdd, vecLastPrim, vecInitPrim, vecHeadPrim,
   vecTailPrim, mkVecCons, mkVecNil)

import qualified Clash.Normalize.Primitives as NP
import Clash.Promoted.Nat.Unsafe (unsafeSNat)
import qualified Clash.Sized.Internal.BitVector as BitVector
import qualified Clash.Sized.Internal.Signed    as Signed
import qualified Clash.Sized.Internal.Unsigned  as Unsigned
import Clash.Sized.Internal.BitVector(BitVector(..), Bit(..))
import Clash.Sized.Internal.Signed   (Signed   (..))
import Clash.Sized.Internal.Unsigned (Unsigned (..))
import Clash.XException (isX)

import {-# SOURCE #-} Clash.GHC.Evaluator

import qualified Clash.Annotations.BitRepresentation.Deriving
import qualified Clash.Class.BitPack.Internal
import qualified Clash.Class.Exp
import qualified Clash.Promoted.Nat
import qualified Clash.Sized.Internal.BitVector
import qualified Clash.Sized.Internal.Index
import qualified Clash.Sized.Internal.Signed
import qualified Clash.Sized.Internal.Unsigned
import qualified Clash.Sized.RTree
import qualified Clash.Sized.Vector
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.CString
import qualified GHC.TypeLits
import qualified GHC.TypeNats
import qualified GHC.Types
#if MIN_VERSION_ghc(9,12,0)
import qualified GHC.Magic
#endif
import qualified GHC.Num
import qualified GHC.Num.Integer

#if MIN_VERSION_ghc_prim(0,12,0)
import qualified GHC.PrimopWrappers
#endif

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
