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

module Clash.GHC.Evaluator.Primitives.Clash.Sized.RTree
  ( primitives
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

import {-# SOURCE #-} Clash.GHC.Evaluator.Primitive
import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
--------
-- RTree
--------
  [ ( $(textNameLit 'Clash.Sized.RTree.textract)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , [DC _ tArgs] <- args
            -> reduceWHNF (Either.lefts tArgs !! 1)
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.RTree.tsplit)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , dTy : aTy : _ <- tys
            , [DC _ tArgs] <- args
            , (tyArgs,tyView -> TyConApp tupTcNm _) <- splitFunForallTy ty
            , TyConApp treeTcNm _ <- tyView (Either.rights tyArgs !! 0)
            -> let (Just tupTc) = UniqMap.lookup tupTcNm tcm
                   [tupDc]      = tyConDataCons tupTc
               in  reduce $
                   mkApps (Data tupDc)
                          [Right (mkTyConApp treeTcNm [dTy,aTy])
                          ,Right (mkTyConApp treeTcNm [dTy,aTy])
                          ,Left (Either.lefts tArgs !! 1)
                          ,Left (Either.lefts tArgs !! 2)
                          ]
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.RTree.tdfold)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , pTy : kTy : aTy : _ <- tys
            , _ : p : f : g : ts : _ <- args
            , DC _ tArgs <- ts
            , Right k' <- runExcept (tyNatSize tcm kTy)
            -> case k' of
                 0 -> reduceWHNF (mkApps (valToTerm f) [Left (Either.lefts tArgs !! 1)])
                 _ -> let k'ty = LitTy (NumTy (k'-1))
                          (tyArgs,_)  = splitFunForallTy ty
                          (tyArgs',_) = splitFunForallTy (Either.rights tyArgs !! 3)
                          TyConApp snatTcNm _ = tyView (Either.rights tyArgs' !! 0)
                          Just snatTc = UniqMap.lookup snatTcNm tcm
                          [snatDc]    = tyConDataCons snatTc
                      in  reduceWHNF $
                          mkApps (valToTerm g)
                                 [Right k'ty
                                 ,Left (mkApps (Data snatDc)
                                               [Right k'ty
                                               ,Left (Literal (NaturalLiteral (k'-1)))])
                                 ,Left (mkApps (Prim pInfo)
                                               [Right pTy
                                               ,Right k'ty
                                               ,Right aTy
                                               ,Left (Literal (NaturalLiteral (k'-1)))
                                               ,Left (valToTerm p)
                                               ,Left (valToTerm f)
                                               ,Left (valToTerm g)
                                               ,Left (Either.lefts tArgs !! 1)
                                               ])
                                 ,Left (mkApps (Prim pInfo)
                                               [Right pTy
                                               ,Right k'ty
                                               ,Right aTy
                                               ,Left (Literal (NaturalLiteral (k'-1)))
                                               ,Left (valToTerm p)
                                               ,Left (valToTerm f)
                                               ,Left (valToTerm g)
                                               ,Left (Either.lefts tArgs !! 2)
                                               ])
                                 ]
          _ -> Nothing
    )

  , ( $(textNameLit 'Clash.Sized.RTree.treplicate)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | isSubj
            , let ty' = piResultTys tcm ty tys
            , (_,tyView -> TyConApp treeTcNm [lenTy,argTy]) <- splitFunForallTy ty'
            , Right len <- runExcept (tyNatSize tcm lenTy)
            -> let (Just treeTc) = UniqMap.lookup treeTcNm tcm
                   [lrCon,brCon] = tyConDataCons treeTc
               in  reduce (mkRTree lrCon brCon argTy len (replicate (2^len) (valToTerm (last args))))
          _ -> Nothing
    )
  ]
