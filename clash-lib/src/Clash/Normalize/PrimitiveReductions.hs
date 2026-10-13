{-|
  Copyright  :  (C) 2015-2016, University of Twente,
                    2016     , Myrtle Software Ltd,
                    2021-2024, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The primitive reductions of Clash Core that its evaluator uses. The
  normalizer uses those of scoped Clash Core
  ("Clash.Normalize.Scoped.PrimitiveReductions").
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Normalize.PrimitiveReductions
  ( module Clash.Normalize.PrimitiveReductions
  , typeNatAdd
  , typeNatMul
  , typeNatSub
  ) where

import           GHC.Stack                        (HasCallStack)

import           Clash.Core.DataCon               (DataCon)
import           Clash.Core.Name (mkUnsafeSystemName)
import           Clash.Core.Term
  ( IsMultiPrim (..), PrimInfo (..), Term (..), WorkInfo (..), mkApps,
    PrimUnfolding(..) )
import           Clash.Core.Type (LitTy (..), Type (..), mkFunTy, mkTyConApp)
import           Clash.Core.TyCon (TyConName)
import           Clash.Core.TysPrim (typeNatKind, liftedTypeKind)
import           Clash.Core.Util (dataConInstArgTys, primCo)
import           Clash.Core.Var (mkTyVar)

import           Clash.Core.Scoped.TysPrim (typeNatAdd, typeNatMul, typeNatSub)

vecHeadPrim
  :: TyConName
  -- ^ Vec TyCon name
  -> Term
vecHeadPrim vecTcNm =
 -- head :: Vec (n+1) a -> a
  Prim (PrimInfo "Clash.Sized.Vector.head" (vecHeadTy vecTcNm) WorkNever SingleResult NoUnfolding)

vecLastPrim
  :: TyConName
  -- ^ Vec TyCon name
  -> Term
vecLastPrim vecTcNm =
  -- last :: Vec (n+1) a -> a
  -- has the same type signature as head, hence we're reusing its type
  -- definition here.
  Prim (PrimInfo "Clash.Sized.Vector.last" (vecHeadTy vecTcNm) WorkNever SingleResult NoUnfolding)

vecHeadTy
  :: TyConName
  -- ^ Vec TyCon name
  -> Type
vecHeadTy vecNm =
  ForAllTy nTV $
  ForAllTy aTV $
  mkFunTy
    (mkTyConApp vecNm [mkTyConApp typeNatAdd [VarTy nTV, LitTy (NumTy 1)], VarTy aTV])
    (VarTy aTV)
 where
  aTV = mkTyVar liftedTypeKind (mkUnsafeSystemName "a" 0)
  nTV = mkTyVar typeNatKind (mkUnsafeSystemName "n" 1)

vecTailPrim
  :: TyConName
  -- ^ Vec TyCon name
  -> Term
vecTailPrim vecTcNm =
  -- tail :: Vec (n + 1) a -> Vec n a
  Prim (PrimInfo "Clash.Sized.Vector.tail" (vecTailTy vecTcNm) WorkNever SingleResult NoUnfolding)

vecInitPrim
  :: TyConName
  -- ^ Vec TyCon name
  -> Term
vecInitPrim vecTcNm =
  -- init :: Vec (n + 1) a -> Vec n a
  -- has the same type signature as tail, hence we're reusing its type
  -- definition here.
  Prim (PrimInfo "Clash.Sized.Vector.init" (vecTailTy vecTcNm) WorkNever SingleResult NoUnfolding)

vecTailTy
  :: TyConName
  -- ^ Vec TyCon name
  -> Type
vecTailTy vecNm =
  ForAllTy nTV $
  ForAllTy aTV $
  mkFunTy
    (mkTyConApp vecNm [mkTyConApp typeNatAdd [VarTy nTV, LitTy (NumTy 1)], VarTy aTV])
    (mkTyConApp vecNm [VarTy nTV, VarTy aTV])
 where
  nTV = mkTyVar typeNatKind (mkUnsafeSystemName "n" 0)
  aTV = mkTyVar liftedTypeKind (mkUnsafeSystemName "a" 1)

-- | Create a vector of supplied elements
mkVecCons
  :: HasCallStack
  => DataCon
  -- ^ The Cons (:>) constructor
  -> Type
  -- ^ Element type
  -> Integer
  -- ^ Length of the vector
  -> Term
  -- ^ head of the vector
  -> Term
  -- ^ tail of the vector
  -> Term
mkVecCons consCon resTy n h t
  | n <= 0 = error "mkVecCons: n <= 0"
  | otherwise
  = case dataConInstArgTys consCon [LitTy (NumTy n), resTy, LitTy (NumTy (n-1))] of
    Just (consCoTy : _) ->
      mkApps (Data consCon) [ Right (LitTy (NumTy n))
                            , Right resTy
                            , Right (LitTy (NumTy (n-1)))
                            , Left (primCo consCoTy)
                            , Left h
                            , Left t ]
    _ -> error "mkVecCons: failed to instantiate Cons DC"

-- | Create an empty vector
mkVecNil
  :: DataCon
  -- ^ The Nil constructor
  -> Type
  -- ^ The element type
  -> Term
mkVecNil nilCon resTy = case dataConInstArgTys nilCon [LitTy (NumTy 0), resTy] of
  Just (nilCoTy : _) ->
    mkApps (Data nilCon) [ Right (LitTy (NumTy 0))
                        , Right resTy
                        , Left  (primCo nilCoTy) ]
  _ -> error "mkVecNil: failed to instantiate Nil DC"
