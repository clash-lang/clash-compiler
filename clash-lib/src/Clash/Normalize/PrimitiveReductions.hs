{-|
  Copyright  :  (C) 2015-2016, University of Twente,
                    2016     , Myrtle Software Ltd,
<<<<<<< HEAD
                    2021-2024, QBayLogic B.V.
||||||| parent of 861f35008 (Add concurrent normalization)
                    2021     , QBayLogic B.V.
=======
                    2021-2022, QBayLogic B.V.
>>>>>>> 861f35008 (Add concurrent normalization)
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of primitives

  Currently, it contains reductions for:

    * Clash.Sized.Vector.map
    * Clash.Sized.Vector.zipWith
    * Clash.Sized.Vector.traverse#
    * Clash.Sized.Vector.foldr
    * Clash.Sized.Vector.fold
    * Clash.Sized.Vector.dfold
    * Clash.Sized.Vector.(++)
    * Clash.Sized.Vector.head
    * Clash.Sized.Vector.tail
    * Clash.Sized.Vector.unconcatBitVector#
    * Clash.Sized.Vector.replicate
    * Clash.Sized.Vector.imap
    * Clash.Sized.Vector.dtfold
    * Clash.Sized.RTree.tfold
    * Clash.Sized.Vector.reverse
    * Clash.Sized.Vector.unconcat

  Partially handles:

    * Clash.Sized.Vector.transpose
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Normalize.PrimitiveReductions where

import qualified Control.Lens                     as Lens
import           Control.Lens                     ((.=))
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Maybe        (MaybeT (..))
import           Data.Bifunctor                   (second)
import           Data.List                        (mapAccumR, uncons)
import           Data.List.Extra                  (zipEqual)
#if MIN_VERSION_base(4,20,0)
import qualified Data.List.NonEmpty               as NE hiding (unzip)
import qualified Data.Functor                     as NE
#else
import qualified Data.List.NonEmpty               as NE
#endif
import qualified Data.Maybe                       as Maybe
import           Data.Semigroup                   (sconcat)
import           Data.Text.Extra                  (showt)
import           GHC.Stack                        (HasCallStack)

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Builtin.Names
  (boolTyConKey, typeNatAddTyFamNameKey, typeNatMulTyFamNameKey,
   typeNatSubTyFamNameKey)
import           GHC.Types.SrcLoc                 (wiredInSrcSpan)
#else
import           PrelNames
  (boolTyConKey, typeNatAddTyFamNameKey, typeNatMulTyFamNameKey,
   typeNatSubTyFamNameKey)
import           SrcLoc                           (wiredInSrcSpan)
#endif

import           Clash.Core.DataCon               (DataCon)
import           Clash.Core.FreeVars              (typeFreeVars)
import           Clash.Core.HasType

import           Clash.Core.Literal               (Literal (..))
import           Clash.Core.Name
  (nameOcc, Name(..), NameSort(User), mkUnsafeSystemName)
import           Clash.Core.Pretty                (showPpr)
import           Clash.Core.Subst                 (extendTvSubst, mkSubst, substTy)
import           Clash.Core.Term
  (IsMultiPrim (..), CoreContext (..), PrimInfo (..), Term (..), WorkInfo (..), Pat (..),
   collectTermIds, mkApps, PrimUnfolding(..))
import           Clash.Core.Type                  (LitTy (..), Type (..),
                                                   TypeView (..), coreView1,
                                                   mkFunTy, mkTyConApp,
                                                   normalizeType,
                                                   splitFunForallTy, tyView)
import           Clash.Core.TyCon
  (TyConMap, TyConName, tyConDataCons, tyConName)
import           Clash.Core.TysPrim
  (integerPrimTy, typeNatKind, liftedTypeKind)
import           Clash.Core.Util
  (appendToVec, extractElems, extractTElems, mkRTree,
   mkUniqInternalId, mkUniqSystemTyVar, mkVec, dataConInstArgTys, primCo)
import           Clash.Core.Var                   (mkTyVar, mkLocalId)
import           Clash.Core.VarEnv                (extendInScopeSetList)
import qualified Clash.Data.UniqMap as UniqMap
import qualified Clash.Normalize.Primitives as NP (undefined)
import {-# SOURCE #-} Clash.Normalize.Strategy
import           Clash.Normalize.Types
import           Clash.Rewrite.Types
import           Clash.Rewrite.Util
import           Clash.Unique (fromGhcUnique)
import           Clash.Util
import qualified Clash.Util.Interpolate           as I

typeNatAdd :: TyConName
typeNatAdd =
  Name User "GHC.TypeNats.+" (fromGhcUnique typeNatAddTyFamNameKey) wiredInSrcSpan

typeNatMul :: TyConName
typeNatMul =
  Name User "GHC.TypeNats.*" (fromGhcUnique typeNatMulTyFamNameKey) wiredInSrcSpan

typeNatSub :: TyConName
typeNatSub =
  Name User "GHC.TypeNats.-" (fromGhcUnique typeNatSubTyFamNameKey) wiredInSrcSpan

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

-- | Makes two case statements: the first one extract the _head_ from the given
-- vector, the latter the tail.
extractHeadTail
  :: DataCon
  -- ^ The Cons (:>) constructor
  -> Type
  -- ^ Element type
  -> Integer
  -- ^ Length of the vector, must be positive
  -> Term
  -- ^ Vector to extract head from
  -> (Term, Term)
  -- ^ (head of vector, tail of vector)
extractHeadTail consCon elTy n vec =
  case dataConInstArgTys consCon tys of
    Just [coTy, _elTy, restTy] ->
      let
        mTV = mkTyVar typeNatKind (mkUnsafeSystemName "m" 0)
        co = mkLocalId coTy (mkUnsafeSystemName "_co_" 1)
        el = mkLocalId elTy (mkUnsafeSystemName "el" 2)
        rest = mkLocalId restTy (mkUnsafeSystemName "res" 3)

        pat = DataPat consCon [mTV] [co, el, rest]
      in
        ( Case vec elTy [(pat, Var el)]
        , Case vec restTy [(pat, Var rest)] )
    _ -> error "extractHeadTail: failed to instantiate Cons DC"
 where
  tys = [(LitTy (NumTy n)), elTy, (LitTy (NumTy (n-1)))]

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

-- | Replace an application of the @Clash.Sized.Vector.reverse@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.reverse@
reduceReverse
  :: Integer
  -- ^ Length of the vector, must be positive
  -> Type
  -- ^ Element of type of the vector
  -> Term
  -- ^ The vector to reverse
  -> TransformContext
  -> NormalizeSession Term
reduceReverse n elTy vArg (TransformContext inScope0 _ctx) = do
  tcm <- Lens.view tcCache
  let ty = inferCoreTypeOf tcm vArg
  go tcm ty
 where
  go tcm (coreView1 tcm -> Just ty') = go tcm ty'
  go tcm (tyView -> TyConApp vecTcNm _)
    | Just vecTc <- UniqMap.lookup vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
    , [nilCon, consCon] <- tyConDataCons vecTc
    = do
      uniqs0 <- Lens.use uniqSupply
      let (uniqs1,(vars,elems)) = second (second sconcat . NE.unzip)
                                $ extractElems uniqs0 inScope0 consCon elTy 'V' n vArg
          lbody = mkVec nilCon consCon elTy n (reverse (NE.toList vars))
          lb    = Letrec (NE.init elems) lbody
      uniqSupply Lens..= uniqs1
      changed lb
  go _ ty = error $ $(curLoc) ++ "reduceReverse: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.zipWith@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.zipWith@
reduceZipWith
  :: PrimInfo -- ^ zipWith primitive info
  -> Integer  -- ^ Length of the vector(s)
  -> Type -- ^ Element type of the lhs of the function
  -> Type -- ^ Element type of the rhs of the function
  -> Type -- ^ Element type of the result of the function
  -> Term -- ^ The zipWith'd functions
  -> Term -- ^ The 1st vector argument
  -> Term -- ^ The 2nd vector argument
  -> TransformContext
  -> NormalizeSession Term
reduceZipWith zipWithPrimInfo n lhsElTy rhsElTy resElTy fun lhsArg rhsArg _ctx = do
  tcm <- Lens.view tcCache
  changed (go tcm (inferCoreTypeOf tcm lhsArg))
 where
  go tcm (coreView1 tcm -> Just ty) = go tcm ty
  go tcm (tyView -> TyConApp vecTcNm _)
    | (Just vecTc) <- UniqMap.lookup vecTcNm tcm
    , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
    , [nilCon, consCon] <- tyConDataCons vecTc
    = if n == 0 then
        mkVecNil nilCon resElTy
      else
        let
          (a, as) = extractHeadTail consCon lhsElTy n lhsArg
          (b, bs) = extractHeadTail consCon rhsElTy n rhsArg
          c = mkApps fun [Left a, Left b]
          cs = mkApps (Prim zipWithPrimInfo) [ Right lhsElTy
                                             , Right rhsElTy
                                             , Right resElTy
                                             , Right (LitTy (NumTy (n - 1)))
                                             , Left fun
                                             , Left as
                                             , Left bs ]
        in
          mkVecCons consCon resElTy n c cs
  go _ ty =
    error $ $(curLoc) ++ [I.i|
      reduceZipWith: argument does not have a vector type:

        #{showPpr ty}
    |]

-- | Replace an application of the @Clash.Sized.Vector.map@ primitive on vectors
-- of a known length @n@, by the fully unrolled recursive "definition" of
-- @Clash.Sized.Vector.map@
reduceMap
  :: PrimInfo -- ^ map primitive info
  -> Integer  -- ^ Length of the vector
  -> Type -- ^ Argument type of the function
  -> Type -- ^ Result type of the function
  -> Term -- ^ The map'd function
  -> Term -- ^ The map'd over vector
  -> TransformContext
  -> NormalizeSession Term
reduceMap mapPrimInfo n argElTy resElTy fun arg _ctx = do
    tcm <- Lens.view tcCache
    let ty = inferCoreTypeOf tcm arg
    changed (go tcm ty)
  where
    go tcm (coreView1 tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- UniqMap.lookup vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [nilCon,consCon] <- tyConDataCons vecTc
      = if n == 0 then
          mkVecNil nilCon argElTy
        else
          let
            nPredTy = Right (LitTy (NumTy (n - 1)))
            (a, as) = extractHeadTail consCon argElTy n arg
            b = mkApps fun [Left a]
            bs = mkApps (Prim mapPrimInfo) [ Right argElTy
                                           , Right resElTy
                                           , nPredTy
                                           , Left fun
                                           , Left as ]
          in
            mkVecCons consCon resElTy n b bs
    go _ ty =
      error $ $(curLoc) ++ [I.i|
        reduceMap: argument does not have a vector type:

          #{showPpr ty}
      |]

-- | Replace an application of the @Clash.Sized.Vector.imap@ primitive on vectors
-- of a known length @n@, by the fully unrolled recursive "definition" of
-- @Clash.Sized.Vector.imap@
reduceImap
  :: Integer  -- ^ Length of the vector, must be positive
  -> Type -- ^ Argument type of the function
  -> Type -- ^ Result type of the function
  -> Term -- ^ Lenght of the vector (as a KnownNat)
  -> Term -- ^ The imap'd function
  -> Term -- ^ The imap'd over vector
  -> TransformContext
  -> NormalizeSession Term
reduceImap n argElTy resElTy _kn fun arg (TransformContext is0 ctx) = do
    tcm <- Lens.view tcCache
    let ty = inferCoreTypeOf tcm arg
    go tcm ty
  where
    go tcm (coreView1 tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- UniqMap.lookup vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [nilCon,consCon] <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        fun1 <- constantPropagation (TransformContext is0 (AppArg Nothing:ctx)) fun
        let is1 = extendInScopeSetList is0 (collectTermIds fun1)
            (uniqs1,nTv) = mkUniqSystemTyVar (uniqs0,is1) ("n",typeNatKind)
            (uniqs2,(vars,elems)) = second (second sconcat . NE.unzip)
                                  $ uncurry extractElems uniqs1 consCon argElTy 'I' n arg
            idxTcNm = Maybe.fromMaybe (error "reduceImap: failed to create Index TC") $ do
              (Right idxTy:_,_) <- pure (splitFunForallTy (inferCoreTypeOf tcm fun))
              TyConApp nm _ <- pure (tyView idxTy)
              return nm
            -- fromInteger# :: KnownNat n => Integer -> Index n
            idxFromIntegerTy = ForAllTy nTv
                                        (foldr mkFunTy
                                               (mkTyConApp idxTcNm
                                                           [VarTy nTv])
                                               [integerPrimTy,integerPrimTy])
            idxFromInteger   = Prim (PrimInfo "Clash.Sized.Internal.Index.fromInteger#" idxFromIntegerTy WorkNever SingleResult NoUnfolding)
            idxs             = map (App (App (TyApp idxFromInteger (LitTy (NumTy n)))
                                             (Literal (IntegerLiteral (toInteger n))))
                                   . Literal . IntegerLiteral . toInteger) [0..(n-1)]
            funApps          = zipWith (\i v -> App (App fun1 i) v) idxs (NE.toList vars)
            lbody            = mkVec nilCon consCon resElTy n funApps
            lb               = Letrec (NE.init elems) lbody
        uniqSupply Lens..= uniqs2
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceImap: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.iterateI@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.iterateI@
reduceIterateI
  :: Integer
  -- ^ Length of vector
  -> Type
  -- ^ Vector's element type
  -> Type
  -- ^ Vector's type
  -> Term
  -- ^ Length of the vector (as a KnownNat)
  -> Term
  -- ^ iterateI's HO-function argument
  -> Term
  -- ^ iterateI's start value
  -> TransformContext
  -> RewriteMonad NormalizeState Term
  -- ^ Fully unrolled definition
reduceIterateI n aTy vTy _kn f0 a (TransformContext is0 ctx) = do
  tcm <- Lens.view tcCache
  f1 <- constantPropagation (TransformContext is0 (AppArg Nothing:ctx)) f0

  uniqs0 <- Lens.use uniqSupply
  let
    is1 = extendInScopeSetList is0 (collectTermIds f1)
    ((uniqs1, _is2), elementIds) =
      mapAccumR
        mkUniqInternalId
        (uniqs0, is1)
        (zip (map (("el" <>) . showt) [1..n-1]) (repeat aTy))
  uniqSupply .= uniqs1

  let
    elems = map (App f1) (a:map Var elementIds)
    vec = Maybe.fromMaybe (error "reduceIterateI: failed to create Vec DCs") $ do
      TyConApp vecTcNm _ <- pure (tyView vTy)
      vecTc <- UniqMap.lookup vecTcNm tcm
      [nilCon, consCon] <- pure (tyConDataCons vecTc)
      return (mkVec nilCon consCon aTy n (take (fromInteger n) (a:map Var elementIds)))

  -- Result:
  --   let
  --     el1 = f a
  --     el2 = f el1
  --     el3 = f el2
  --     ..
  --   in
  --     (a :> el1 :> el2 :> el3 :> ..)
  --
  changed (Letrec (zip elementIds elems) vec)

-- | Replace an application of the @Clash.Sized.Vector.traverse#@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.traverse#@
reduceTraverse
  :: Integer  -- ^ Length of the vector, must be positive
  -> Type -- ^ Element type of the argument vector
  -> Type -- ^ The type of the applicative
  -> Type -- ^ Element type of the result vector
  -> Term -- ^ The @Applicative@ dictionary
  -> Term -- ^ The function to traverse with
  -> Term -- ^ The argument vector
  -> TransformContext
  -> NormalizeSession Term
reduceTraverse n aTy fTy bTy dict fun arg (TransformContext is0 ctx) = do
    tcm <- Lens.view tcCache
    case tyView (inferCoreTypeOf tcm dict) of
      TyConApp apDictTcNm _ ->
        let ty = inferCoreTypeOf tcm arg
         in go tcm apDictTcNm ty
      t -> error ("reduceTraverse: expected a TC, but got: " <> show t)
  where
    go tcm apDictTcNm (coreView1 tcm -> Just ty') = go tcm apDictTcNm ty'
    go tcm apDictTcNm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- UniqMap.lookup vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [nilCon,consCon] <- tyConDataCons vecTc
      = fmap (Maybe.fromMaybe (error "reduceTraverse: failed to build")) $ runMaybeT $ do
          uniqs0 <- Lens.use uniqSupply
          fun1 <- lift (constantPropagation (TransformContext is0 (AppArg Nothing:ctx)) fun)
          let is1 = extendInScopeSetList is0 (collectTermIds fun1)
          apDictTc <- hoistMaybe (UniqMap.lookup apDictTcNm tcm)
          apDictCon <- hoistMaybe (Maybe.listToMaybe (tyConDataCons apDictTc))
          apDictIdTys <- hoistMaybe (dataConInstArgTys apDictCon [fTy])
          (uniqs1,apDictIds@[functorDictId,pureId,apId,_,_,_]) <- pure $
                mapAccumR mkUniqInternalId (uniqs0,is1)
                  (zipEqual ["functorDict","pure","ap","liftA2","apConstL","apConstR"]
                      apDictIdTys)

          TyConApp funcDictTcNm _ <- hoistMaybe (tyView <$> Maybe.listToMaybe apDictIdTys)
          funcDictTc <- hoistMaybe (UniqMap.lookup funcDictTcNm tcm)
          funcDictCon <- hoistMaybe (Maybe.listToMaybe (tyConDataCons funcDictTc))
          funcDictIdTys <- hoistMaybe (dataConInstArgTys funcDictCon [fTy])
          (uniqs2,funcDicIds@[fmapId,_]) <- pure $
                mapAccumR mkUniqInternalId uniqs1
                  (zipEqual ["fmap","fmapConst"] funcDictIdTys)

          let apPat    = DataPat apDictCon   [] apDictIds
              fnPat    = DataPat funcDictCon [] funcDicIds

              -- Extract the 'pure' function from the Applicative dictionary
              pureTy = coreTypeOf pureId
              pureTm = Case dict pureTy [(apPat,Var pureId)]

              -- Extract the '<*>' function from the Applicative dictionary
              apTy   = coreTypeOf apId
              apTm   = Case dict apTy [(apPat, Var apId)]

              -- Extract the Functor dictionary from the Applicative dictionary
              funcTy = coreTypeOf functorDictId
              funcTm = Case dict funcTy
                                [(apPat,Var functorDictId)]

              -- Extract the 'fmap' function from the Functor dictionary
              fmapTy = coreTypeOf fmapId
              fmapTm = Case (Var functorDictId) fmapTy
                            [(fnPat, Var fmapId)]

              (uniqs3,(vars,elems)) = second (second sconcat . NE.unzip)
                                    $ uncurry extractElems uniqs2 consCon aTy 'T' n arg

              funApps = map (fun1 `App`) (NE.toList vars)

              lbody   = mkTravVec vecTcNm nilCon consCon (Var (apDictIds!!1))
                                                        (Var (apDictIds!!2))
                                                        (Var (funcDicIds!!0))
                                                        bTy n funApps

              lb      = Letrec ([((apDictIds!!0), funcTm)
                                ,((apDictIds!!1), pureTm)
                                ,((apDictIds!!2), apTm)
                                ,((funcDicIds!!0), fmapTm)
                                ] ++ NE.init elems) lbody
          uniqSupply Lens..= uniqs3
          lift (changed lb)
    go _ _ ty = error $ $(curLoc) ++ "reduceTraverse: argument does not have a vector type: " ++ showPpr ty

-- | Create the traversable vector
--
-- e.g. for a length '2' input vector, we get
--
-- > (:>) <$> x0 <*> ((:>) <$> x1 <*> pure Nil)
mkTravVec :: TyConName -- ^ Vec tcon
          -> DataCon   -- ^ Nil con
          -> DataCon   -- ^ Cons con
          -> Term      -- ^ 'pure' term
          -> Term      -- ^ '<*>' term
          -> Term      -- ^ 'fmap' term
          -> Type      -- ^ 'b' ty
          -> Integer       -- ^ Length of the vector
          -> [Term]    -- ^ Elements of the vector
          -> Term
mkTravVec vecTc nilCon consCon pureTm apTm fmapTm bTy = go
  where
    go :: Integer -> [Term] -> Term
    go _ [] = mkApps pureTm [Right (mkTyConApp vecTc [LitTy (NumTy 0),bTy])
                            ,Left  (mkApps (Data nilCon)
                                           [Right (LitTy (NumTy 0))
                                           ,Right bTy
                                           ,Left  (primCo nilCoTy)])]

    go n (x:xs) = mkApps apTm
      [Right (mkTyConApp vecTc [LitTy (NumTy (n-1)),bTy])
      ,Right (mkTyConApp vecTc [LitTy (NumTy n),bTy])
      ,Left (mkApps fmapTm [Right bTy
                           ,Right (mkFunTy (mkTyConApp vecTc [LitTy (NumTy (n-1)),bTy])
                                           (mkTyConApp vecTc [LitTy (NumTy n),bTy]))
                           ,Left  (mkApps (Data consCon)
                                          [Right (LitTy (NumTy n))
                                          ,Right bTy
                                          ,Right (LitTy (NumTy (n-1)))
                                          ,Left  (primCo (consCoTy n))
                                          ])
                           ,Left  x])
      ,Left (go (n-1) xs)]

    nilCoTy = case dataConInstArgTys nilCon [(LitTy (NumTy 0)), bTy] of
                Just (x:_) -> x
                _ -> error "impossible"

    consCoTy n = case dataConInstArgTys consCon
                                        [(LitTy (NumTy n))
                                        ,bTy
                                        ,(LitTy (NumTy (n-1)))] of
                   Just (x:_) -> x
                   _ -> error "impossible"

-- | Replace an application of the @Clash.Sized.Vector.foldr@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.foldr@
reduceFoldr
  :: PrimInfo
  -- ^ Primitive info for foldr blackbox
  -> Integer
  -- ^ Length of the vector
  -> Type
  -- ^ Element type of the argument vector
  -> Term
  -- ^ The function to fold with
  -> Term
  -- ^ The starting value
  -> Term
  -- ^ The argument vector
  -> TransformContext
  -> NormalizeSession Term
reduceFoldr _ 0 _ _ start _ _ = changed start
reduceFoldr foldrPrimInfo n aTy fun start arg _ctx = do
    tcm <- Lens.view tcCache
    let ty = inferCoreTypeOf tcm arg
    changed (go tcm ty)
  where
    go tcm (coreView1 tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , Just vecTc <- UniqMap.lookup vecTcNm tcm
      , [_nilCon, consCon] <- tyConDataCons vecTc
      = let
          (a, as) = extractHeadTail consCon aTy n arg
          b = mkApps (Prim foldrPrimInfo) [ Right aTy
                                          , Right (inferCoreTypeOf tcm start)
                                          , Right (LitTy (NumTy (n - 1)))
                                          , Left fun
                                          , Left start
                                          , Left as ]
        in
          mkApps fun [Left a, Left b]

    go _ ty =
      error $ $(curLoc) ++ [I.i|
        reduceFoldr: argument does not have a vector type:

          #{showPpr ty}
      |]

-- | Replace an application of the @Clash.Sized.Vector.fold@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.fold@
reduceFold
  :: Integer
  -- ^ Length of the vector, must be positive
  -> Type
  -- ^ Element type of the argument vector
  -> Term
  -- ^ The function to fold with
  -> Term
  -- ^ The argument vector
  -> TransformContext
  -> NormalizeSession Term
reduceFold n aTy fun arg (TransformContext is0 ctx) = do
    tcm <- Lens.view tcCache
    let ty = inferCoreTypeOf tcm arg
    go tcm ty
  where
    go tcm (coreView1 tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- UniqMap.lookup vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [_,consCon]  <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        fun1 <- constantPropagation (TransformContext is0 (AppArg Nothing:ctx)) fun
        let is1 = extendInScopeSetList is0 (collectTermIds fun1)
            (uniqs1,(vars,elems)) = second (second sconcat . NE.unzip)
                                  $ extractElems uniqs0 is1 consCon aTy 'F' n arg
            lbody            = foldV fun1 (NE.toList vars)
            lb               = Letrec (NE.init elems) lbody
        uniqSupply Lens..= uniqs1
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceFold: argument does not have a vector type: " ++ showPpr ty

    foldV _ [a] = a
    foldV f as  = let (l,r) = splitAt (length as `div` 2) as
                      lF    = foldV f l
                      rF    = foldV f r
                  in  mkApps f [Left lF, Left rF]

-- | Replace an application of the @Clash.Sized.Vector.dfold@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.dfold@
reduceDFold
  :: Integer
  -- ^ Length of the vector
  -> Type
  -- ^ Element type of the argument vector
  -> Term
  -- ^ Length of the vector (as a KnownNat)
  -> Term
  -- ^ The motive
  -> Term
  -- ^ Function to fold with
  -> Term
  -- ^ Starting value
  -> Term
  -- ^ The vector to fold
  -> TransformContext
  -> NormalizeSession Term
reduceDFold 0 _   _   _       _   start _   _ = changed start
reduceDFold n aTy _kn _motive fun start arg (TransformContext is0 _ctx) = do
    tcm <- Lens.view tcCache
    let ty = inferCoreTypeOf tcm arg
    go tcm ty
  where
    go tcm (coreView1 tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- UniqMap.lookup vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [_,consCon]  <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let is1 = extendInScopeSetList is0 (collectTermIds fun)
            -- TODO: Should 'constantPropagation' be used on 'fun'? It seems to
            -- TOOD: be used for every other function in this module.
            (uniqs1,(vars,elems)) = second (second sconcat . NE.unzip)
                                  $ extractElems uniqs0 is1 consCon aTy 'D' n arg
            snatDc = Maybe.fromMaybe (error "reduceDFold: faild to build SNat") $ do
              (_ltv:_rubp:Right snTy:_,_) <- pure (splitFunForallTy (inferCoreTypeOf tcm fun))
              (TyConApp snatTcNm _) <- pure (tyView snTy)
              snatTc <- UniqMap.lookup snatTcNm tcm
              Maybe.listToMaybe (tyConDataCons snatTc)
            ubp k = Maybe.fromMaybe
              (error "reduceDFold: failed to extract upper bound proof") $ do
                (_ltv:Right ubpT:_,_) <- pure (splitFunForallTy (inferCoreTypeOf tcm fun))
                -- toListOf does not de-duplicate, but we know that there is only
                -- one free variable in here, thus, taking the first element is fine
                (tvN, _) <- uncons $ Lens.toListOf typeFreeVars ubpT
                let subst = extendTvSubst (mkSubst is0) tvN (LitTy (NumTy k))
                let witness = normalizeType tcm (substTy subst ubpT)
                (TyConApp tupTcNm _) <- pure (tyView witness)
                witnessTc <- UniqMap.lookup tupTcNm tcm
                Maybe.listToMaybe (tyConDataCons witnessTc)
            lbody = doFold ubp (buildSNat snatDc) (n-1) (NE.toList vars)
            lb    = Letrec (NE.init elems) lbody
        uniqSupply Lens..= uniqs1
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceDFold: argument does not have a vector type: " ++ showPpr ty

    doFold _   _    _ []     = start
    doFold ubp snDc k (x:xs) = mkApps fun
                                 [Right (LitTy (NumTy k))
                                 ,Left (Data (ubp k))
                                 ,Left (snDc k)
                                 ,Left x
                                 ,Left (doFold ubp snDc (k-1) xs)
                                 ]

-- | Replace an application of the @Clash.Sized.Vector.head@ primitive on
-- vectors of a known length @n@, by a projection of the first element of a
-- vector.
reduceHead
  :: Integer  -- ^ Length of the vector, must be positive
  -> Type -- ^ Element type of the vector
  -> Term -- ^ The argument vector
  -> TransformContext
  -> NormalizeSession Term
reduceHead n aTy vArg (TransformContext inScope _ctx) = do
    tcm <- Lens.view tcCache
    let ty = inferCoreTypeOf tcm vArg
    go tcm ty
  where
    go tcm (coreView1 tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- UniqMap.lookup vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [_,consCon]  <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let (uniqs1,(vars,elems)) = second (second sconcat . NE.unzip)
                                  $ extractElems uniqs0 inScope consCon aTy 'H' n vArg
            lb = Letrec [NE.head elems] (NE.head vars)
        uniqSupply Lens..= uniqs1
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceHead: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.tail@ primitive on
-- vectors of a known length @n@, by a projection of the tail of a
-- vector.
reduceTail
  :: Integer  -- ^ Length of the vector, must be positive
  -> Type -- ^ Element type of the vector
  -> Term -- ^ The argument vector
  -> TransformContext
  -> NormalizeSession Term
reduceTail n aTy vArg (TransformContext inScope _ctx) = do
    tcm <- Lens.view tcCache
    let ty = inferCoreTypeOf tcm vArg
    go tcm ty
  where
    go tcm (coreView1 tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- UniqMap.lookup vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [_,consCon]  <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let (uniqs1,(_,elems)) = second (second sconcat . NE.unzip)
                               $ extractElems uniqs0 inScope consCon aTy 'L' n vArg
            b@(tB,_)     = elems NE.!! 1
            lb           = Letrec [b] (Var tB)
        uniqSupply Lens..= uniqs1
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceTail: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.last@ primitive on
-- vectors of a known length @n@, by a projection of the last element of a
-- vector.
reduceLast
  :: Integer  -- ^ Length of the vector, must be positive
  -> Type -- ^ Element type of the vector
  -> Term -- ^ The argument vector
  -> TransformContext
  -> NormalizeSession Term
reduceLast n aTy vArg (TransformContext inScope _ctx) = do
    tcm <- Lens.view tcCache
    let ty = inferCoreTypeOf tcm vArg
    go tcm ty
  where
    go tcm (coreView1 tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- UniqMap.lookup vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [_,consCon]  <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let (uniqs1,(_,elems)) = second NE.unzip
                               $ extractElems uniqs0 inScope consCon aTy 'L' n vArg
            (tB,_)       = NE.head (NE.last elems)
        uniqSupply Lens..= uniqs1
        case n of
         0 -> changed (TyApp (Prim NP.undefined) aTy)
         _ -> changed (Letrec (NE.init (sconcat elems)) (Var tB))
    go _ ty = error $ $(curLoc) ++ "reduceLast: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.init@ primitive on
-- vectors of a known length @n@, by a projection of the init of a
-- vector.
reduceInit
  :: PrimInfo -- ^ Primitive info for 'init'
  -> Integer  -- ^ Length of the vector
  -> Type -- ^ Element type of the vector
  -> Term -- ^ The argument vector
  -> TransformContext
  -> NormalizeSession Term
reduceInit initPrimInfo n aTy vArg _ctx = do
  tcm <- Lens.view tcCache
  let ty = inferCoreTypeOf tcm vArg
  changed (go tcm ty)
 where
  go tcm (coreView1 tcm -> Just ty') = go tcm ty'
  go tcm (tyView -> TyConApp vecTcNm _)
    | (Just vecTc) <- UniqMap.lookup vecTcNm tcm
    , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
    , [nilCon, consCon]  <- tyConDataCons vecTc
    = if n == 0 then
        mkVecNil nilCon aTy
      else
        let
          nPredTy = Right (LitTy (NumTy (n - 1)))
          (a, as0) = extractHeadTail consCon aTy (n+1) vArg
          as1 = mkApps (Prim initPrimInfo) [nPredTy, Right aTy, Left as0]
        in
          mkVecCons consCon aTy n a as1

  go _ ty =
    error $ $(curLoc) ++ [I.i|
      reduceInit: argument does not have a vector type:

        #{showPpr ty}
    |]

-- | Replace an application of the @Clash.Sized.Vector.(++)@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.(++)@
reduceAppend
  :: Integer  -- ^ Length of the LHS arg
  -> Integer  -- ^ Lenght of the RHS arg
  -> Type -- ^ Element type of the vectors
  -> Term -- ^ The LHS argument
  -> Term -- ^ The RHS argument
  -> TransformContext
  -> NormalizeSession Term
reduceAppend 0 _ _   _    rArg _ = changed rArg
reduceAppend _ 0 _   lArg _    _ = changed lArg
reduceAppend n m aTy lArg rArg (TransformContext inScope _ctx) = do
    tcm <- Lens.view tcCache
    let ty = inferCoreTypeOf tcm lArg
    go tcm ty
  where
    go tcm (coreView1 tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- UniqMap.lookup vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [_,consCon]  <- tyConDataCons vecTc
      = do uniqs0 <- Lens.use uniqSupply
           let (uniqs1,(vars,elems)) = second (second sconcat . NE.unzip)
                                     $ extractElems uniqs0 inScope consCon aTy
                                         'C' n lArg
               lbody        = appendToVec consCon aTy rArg (n+m) (NE.toList vars)
               lb           = Letrec (NE.init elems) lbody
           uniqSupply Lens..= uniqs1
           changed lb
    go _ ty = error $ $(curLoc) ++ "reduceAppend: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.unconcat@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.unconcat@
reduceUnconcat :: PrimInfo -- ^ Unconcat primitive info
               -> Integer  -- ^ Length of the result vector
               -> Integer  -- ^ Length of the elements of the result vector
               -> Type -- ^ Element type
               -> Term -- ^ Length of the result vector (as a KnownNat)
               -> Term -- ^ SNat "Length of the elements of the result vector"
               -> Term -- ^ Argument vector
               -> TransformContext
               -> NormalizeSession Term
reduceUnconcat unconcatPrimInfo n m aTy _kn sm arg (TransformContext inScope _ctx) = do
    tcm <- Lens.view tcCache
    let ty = inferCoreTypeOf tcm arg
    go tcm ty
  where
    go tcm (coreView1 tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- UniqMap.lookup vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [nilCon,consCon] <- tyConDataCons vecTc
      , let innerVecTy = mkTyConApp vecTcNm [LitTy (NumTy m), aTy]
      = if n == 0 then
          changed (mkVecNil nilCon innerVecTy)
        else if m == 0 then do
          let
            nilVec = mkVecNil nilCon aTy
            retVec = mkVec nilCon consCon innerVecTy n (replicate (fromInteger n) nilVec)
          changed retVec
        else do
          uniqs0 <- Lens.use uniqSupply
          let
            (uniqs1,(vars,headsAndTails)) =
              second (second sconcat . NE.unzip)
                     (extractElems uniqs0 inScope consCon aTy 'U' (n*m) arg)

            -- Build a vector out of the first m elements
            mvec = mkVec nilCon consCon aTy m (NE.take (fromInteger m) vars)
            -- Get the vector representing the next ((n-1)*m) elements
            -- N.B. `extractElems (xs :: Vec 2 a)` creates:
            -- x0  = head xs
            -- xs0 = tail xs
            -- x1  = head xs0
            -- xs1 = tail xs0
            (lbs,nextVec) = case NE.splitAt ((2*fromInteger m)-1) headsAndTails of
                              (xs,y:_) -> (xs,y)
                              _ -> error "impossible"
            -- recursively call unconcat
            nextUnconcat = mkApps (Prim unconcatPrimInfo)
                                  [ Right (LitTy (NumTy (n-1)))
                                  , Right (LitTy (NumTy m))
                                  , Right aTy
                                  , Left (Literal (NaturalLiteral (n-1)))
                                  , Left sm
                                  , Left (snd nextVec)
                                  ]
            -- let (mvec,nextVec) = splitAt sm arg
            -- in Cons mvec (unconcat sm nextVec)
            lBody = mkVecCons consCon innerVecTy n mvec nextUnconcat
            lb = Letrec lbs lBody

          uniqSupply Lens..= uniqs1
          changed lb
    go _ ty = error $ $(curLoc) ++ "reduceUnconcat: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.transpose@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.transpose@
reduceTranspose :: Integer  -- ^ Length of the result vector
                -> Integer  -- ^ Length of the elements of the result vector
                -> Type -- ^ Element type
                -> Term -- ^ Lenght of the result vector (as a KnownNat)
                -> Term -- ^ Argument vector
                -> TransformContext
                -> NormalizeSession Term
reduceTranspose n 0 aTy _kn arg _ctx = do
    tcm <- Lens.view tcCache
    let ty = inferCoreTypeOf tcm arg
    go tcm ty
  where
    go tcm (coreView1 tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- UniqMap.lookup vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [nilCon,consCon] <- tyConDataCons vecTc
      = let nilVec           = mkVec nilCon consCon aTy 0 []
            innerVecTy       = mkTyConApp vecTcNm [LitTy (NumTy 0), aTy]
            retVec           = mkVec nilCon consCon innerVecTy n (replicate (fromInteger n) nilVec)
        in  changed retVec
    go _ ty = error $ $(curLoc) ++ "reduceTranspose: argument does not have a vector type: " ++ showPpr ty

reduceTranspose _ _ _ _ _ _ = error $ $(curLoc) ++ "reduceTranspose: unimplemented"

reduceReplicate :: Integer
                -> Type
                -> Type
                -> Term
                -> Term
                -> TransformContext
                -> NormalizeSession Term
reduceReplicate n aTy eTy _sn arg _ctx = do
    tcm <- Lens.view tcCache
    go tcm eTy
  where
    go tcm (coreView1 tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- UniqMap.lookup vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [nilCon,consCon] <- tyConDataCons vecTc
      = let retVec = mkVec nilCon consCon aTy n (replicate (fromInteger n) arg)
        in  changed retVec
    go _ ty = error $ $(curLoc) ++ "reduceReplicate: argument does not have a vector type: " ++ showPpr ty

-- TODO: Take a shortcut when given index is a literal. Right now, this function
-- TODO: simply creates a case statement for every element in the vector, which
-- TODO: Clash will eliminate one-by-one if the index turned out to be literal.
-- TODO: It would of course be best to not create the cases in the first place!
reduceReplace_int
  :: Integer
  -- ^ Size of vector, must be positive
  -> Type
  -- ^ Type of vector element
  -> Type
  -- ^ Type of vector
  -> Term
  -- ^ Size of vector (as a KnownNat)
  -> Term
  -- ^ Vector
  -> Term
  -- ^ Index
  -> Term
  -- ^ Element
  -> TransformContext
  -> NormalizeSession Term
reduceReplace_int n aTy vTy _kn v i newA (TransformContext is0 _ctx) = do
  tcm <- Lens.view tcCache
  go tcm vTy
 where
  -- Basically creates:
  --
  -- case eqInt i0 curI  of
  --   True -> newA
  --   _    -> oldA
  --
  -- where:
  --
  --   - curI is the index of the current element, which we statically know
  --   - i0 is the index given to replace_int
  --   - newA is the element given to replace_int as a replacement for..
  --   - oldA; an element at index curI
  --
  replace_intElement
    :: TyConMap
    -- ^ TyCon map
    -> DataCon
    -- Int datacon
    -> Type
    -- Int type
    -> Term
    -- ^ Element in vector
    -> Integer
    -- ^
    -> Term
  replace_intElement tcm iDc iTy oldA elIndex = case0
   where
    case0 = Maybe.fromMaybe (error "replace_intElement: faild to build Truce DC") $ do
      boolTc <- UniqMap.lookup (fromGhcUnique boolTyConKey) tcm
      [_,trueDc] <- pure (tyConDataCons boolTc)
      let eqInt = eqIntPrim iTy (mkTyConApp (tyConName boolTc) [])
      return (Case (mkApps eqInt [ Left i
                                 , Left (mkApps (Data iDc)
                                                [Left (Literal (IntLiteral elIndex))])
                                 ])
                   aTy
                   [ (DefaultPat, oldA)
                   , (DataPat trueDc [] [], newA)
                   ])

  -- Equality on lifted Int that returns a Bool
  eqIntPrim
    :: Type
    -> Type
    -> Term
  eqIntPrim intTy boolTy =
    Prim (PrimInfo
           "GHC.Classes.eqInt"
           (mkFunTy intTy (mkFunTy intTy boolTy))
           WorkVariable
           SingleResult
           NoUnfolding)

  go tcm (coreView1 tcm -> Just ty') = go tcm ty'
  go tcm (tyView -> TyConApp vecTcNm _)
    | (Just vecTc)     <- UniqMap.lookup vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
    , [nilCon,consCon] <- tyConDataCons vecTc
    = do
      -- Get data constructors of 'Int'
      uniqs0                   <- Lens.use uniqSupply
      let iTy                   = inferCoreTypeOf tcm i
          iDc = Maybe.fromMaybe (error "replace_intElement: faild to build Int DC") $ do
            (TyConApp iTcNm _) <- pure (tyView iTy)
            iTc <- UniqMap.lookup iTcNm tcm
            Maybe.listToMaybe (tyConDataCons iTc)

      -- Get elements from vector
          (uniqs1,(vars,elems)) = second (second sconcat . NE.unzip)
                                $ extractElems
                                    uniqs0
                                    is0
                                    consCon
                                    aTy
                                    'I'
                                    n
                                    v

      -- Replace every element with (if i == elIndex then newA else oldA)
      let replacedEls = zipWith (replace_intElement tcm iDc iTy) (NE.toList vars) [0..]
          lbody       = mkVec nilCon consCon aTy n replacedEls
          lb          = Letrec (NE.init elems) lbody
      uniqSupply Lens..= uniqs1
      changed lb
  go _ ty = error $ $(curLoc) ++ "reduceReplace_int: argument does not have "
                                ++ "a vector type: " ++ showPpr ty

-- TODO: Take a shortcut when given index is a literal. Right now, this function
-- TODO: simply creates a case statement for every element in the vector, which
-- TODO: Clash will eliminate one-by-one if the index turned out to be literal.
-- TODO: It would of course be best to not create the cases in the first place!
reduceIndex_int
  :: Integer
  -- ^ Size of vector, must be positive
  -> Type
  -- ^ Type of vector element
  -> Term
  -- ^ Size of vector (as a KnownNat)
  -> Term
  -- ^ Vector
  -> Term
  -- ^ Index
  -> TransformContext
  -> NormalizeSession Term
reduceIndex_int n aTy _kn v i (TransformContext is0 _ctx) = do
  tcm <- Lens.view tcCache
  let vTy = inferCoreTypeOf tcm v
  go tcm vTy
 where
  -- Basically creates:
  --
  -- case eqInt i0 curI of
  --   True -> curA
  --   _    -> next
  --
  -- where:
  --
  --   - curI is the index of the current element, which we statically know
  --   - i0 is the index given to index_int
  --   - curA is the element at index curI
  --   - next; the value if the current index is not equal to index argument
  --
  index_intElement
    :: TyConMap
    -- ^ TyCon map
    -> DataCon
    -- Int datacon
    -> Type
    -- Int type
    -> (Term, Integer)
    -- ^ Element in the vector, and its corresponding index
    -> Term
    -- ^ The rest
    -> Term
  index_intElement tcm iDc iTy (cur,elIndex) next = case0
   where
    case0 = Maybe.fromMaybe (error "reduceIndex_int: faild to build True DC") $ do
      boolTc <- UniqMap.lookup (fromGhcUnique boolTyConKey) tcm
      [_,trueDc] <- pure (tyConDataCons boolTc)
      let eqInt = eqIntPrim iTy (mkTyConApp (tyConName boolTc) [])
      return (Case (mkApps eqInt [ Left i
                                 , Left (mkApps (Data iDc)
                                        [Left (Literal (IntLiteral elIndex))])
                                 ])
                   aTy
                   [ (DefaultPat, next)
                   , (DataPat trueDc [] [], cur)
                   ])

  -- Equality on lifted Int that returns a Bool
  eqIntPrim
    :: Type
    -> Type
    -> Term
  eqIntPrim intTy boolTy =
    Prim ( PrimInfo
            "GHC.Classes.eqInt"
            (mkFunTy intTy (mkFunTy intTy boolTy))
            WorkVariable
            SingleResult
            NoUnfolding)

  go tcm (coreView1 tcm -> Just ty') = go tcm ty'
  go tcm (tyView -> TyConApp vecTcNm _)
    | (Just vecTc)     <- UniqMap.lookup vecTcNm tcm
    , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
    , [_nilCon,consCon] <- tyConDataCons vecTc
    = do
      -- Get data constructors of 'Int'
      uniqs0                   <- Lens.use uniqSupply
      let iTy                   = inferCoreTypeOf tcm i
          iDc = Maybe.fromMaybe (error "reduceIndex_int: faild to build Int DC") $ do
              (TyConApp iTcNm _) <- pure (tyView iTy)
              iTc <- UniqMap.lookup iTcNm tcm
              Maybe.listToMaybe (tyConDataCons iTc)

      -- Get elements from vector
          (uniqs1,(vars,elems)) = second (second sconcat . NE.unzip)
                                $ extractElems
                                    uniqs0
                                    is0
                                    consCon
                                    aTy
                                    'I'
                                    n
                                    v

      -- Build a right-biased tree of case-expressions
      let indexed = foldr (index_intElement tcm iDc iTy)
                              (TyApp (Prim NP.undefined) aTy)
                              (zip (NE.toList vars) [0..])
          lb      = Letrec (NE.init elems) indexed
      uniqSupply Lens..= uniqs1
      changed lb
  go _ ty = error $ $(curLoc) ++ "indexReplace_int: argument does not have "
                              ++ "a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.dtfold@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.dtfold@
reduceDTFold
  :: Integer  -- ^ Length of the vector
  -> Type     -- ^ Element type of the argument vector
  -> Term     -- ^ Length of the vector (as a KnownNat)
  -> Term     -- ^ The motive
  -> Term     -- ^ Function to convert elements with
  -> Term     -- ^ Function to combine branches with
  -> Term     -- ^ The vector to fold
  -> TransformContext
  -> NormalizeSession Term
reduceDTFold n aTy _kn _motive lrFun brFun arg (TransformContext inScope _ctx) = do
    tcm <- Lens.view tcCache
    let ty = inferCoreTypeOf tcm arg
    go tcm ty
  where
    go tcm (coreView1 tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- UniqMap.lookup vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [_,consCon]  <- tyConDataCons vecTc
      = do uniqs0 <- Lens.use uniqSupply
           let (uniqs1,(vars,elems)) = second (second sconcat . NE.unzip)
                                     $ extractElems uniqs0 inScope consCon aTy
                                         'T' (2^n) arg
               snatDc = Maybe.fromMaybe (error "reduceDTFold: faild to build SNat") $ do
                  (_ltv:Right snTy:_,_) <- pure (splitFunForallTy (inferCoreTypeOf tcm brFun))
                  (TyConApp snatTcNm _) <- pure (tyView snTy)
                  snatTc <- UniqMap.lookup snatTcNm tcm
                  Maybe.listToMaybe (tyConDataCons snatTc)
               lbody = doFold (buildSNat snatDc) (n-1) (NE.toList vars)
               lb = Letrec (NE.init elems) lbody
           uniqSupply Lens..= uniqs1
           changed lb
    go _ ty = error $ $(curLoc) ++ "reduceDTFold: argument does not have a vector type: " ++ showPpr ty

    doFold :: (Integer -> Term) -> Integer -> [Term] -> Term
    doFold _    _ [x] = mkApps lrFun [Left x]
    doFold snDc k xs  =
      let (xsL,xsR) = splitAt (2^k) xs
          k'        = k-1
          eL        = doFold snDc k' xsL
          eR        = doFold snDc k' xsR
      in  mkApps brFun [Right (LitTy (NumTy k))
                       ,Left  (snDc k)
                       ,Left  eL
                       ,Left  eR
                       ]

-- | Replace an application of the @Clash.Sized.RTree.tdfold@ primitive on
-- trees of a known depth @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.RTree.tdfold@
reduceTFold
  :: Integer -- ^ Depth of the tree
  -> Type    -- ^ Element type of the argument tree
  -> Term    -- ^ Depth of the tree (as a KnownNat)
  -> Term    -- ^ The motive
  -> Term    -- ^ Function to convert elements with
  -> Term    -- ^ Function to combine branches with
  -> Term    -- ^ The tree to fold
  -> TransformContext
  -> NormalizeSession Term
reduceTFold n aTy _kn _motive lrFun brFun arg (TransformContext inScope _ctx) = do
    tcm <- Lens.view tcCache
    let ty = inferCoreTypeOf tcm arg
    go tcm ty
  where
    go tcm (coreView1 tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp treeTcNm _)
      | (Just treeTc) <- UniqMap.lookup treeTcNm tcm
      , nameOcc treeTcNm == "Clash.Sized.RTree.RTree"
      , [lrCon,brCon] <- tyConDataCons treeTc
      = do uniqs0 <- Lens.use uniqSupply
           let (uniqs1,(vars,elems)) = extractTElems uniqs0 inScope lrCon brCon aTy 'T' n arg
               snatDc = Maybe.fromMaybe (error "reduceTFold: faild to build SNat") $ do
                  (_ltv:Right snTy:_,_) <- pure (splitFunForallTy (inferCoreTypeOf tcm brFun))
                  (TyConApp snatTcNm _) <- pure (tyView snTy)
                  snatTc <- UniqMap.lookup snatTcNm tcm
                  Maybe.listToMaybe (tyConDataCons snatTc)
               lbody = doFold (buildSNat snatDc) (n-1) vars
               lb = (Letrec elems lbody)
           uniqSupply Lens..= uniqs1
           changed lb
    go _ ty = error $ $(curLoc) ++ "reduceTFold: argument does not have a tree type: " ++ showPpr ty

    doFold _    _ [x] = mkApps lrFun [Left x]
    doFold snDc k xs  =
      let (xsL,xsR) = splitAt (length xs `div` 2) xs
          k'        = k-1
          eL        = doFold snDc k' xsL
          eR        = doFold snDc k' xsR
      in  mkApps brFun [Right (LitTy (NumTy k))
                       ,Left (snDc k)
                       ,Left eL
                       ,Left eR
                       ]

reduceTReplicate :: Integer -- ^ Depth of the tree
                 -> Type    -- ^ Element type
                 -> Type    -- ^ Result type
                 -> Term    -- ^ Depth of the tree (as an SNat)
                 -> Term    -- ^ Element
                 -> TransformContext
                 -> NormalizeSession Term
reduceTReplicate n aTy eTy _sn arg _ctx = do
    tcm <- Lens.view tcCache
    go tcm eTy
  where
    go tcm (coreView1 tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp treeTcNm _)
      | (Just treeTc) <- UniqMap.lookup treeTcNm tcm
      , nameOcc treeTcNm == "Clash.Sized.RTree.RTree"
      , [lrCon,brCon] <- tyConDataCons treeTc
      = let retVec = mkRTree lrCon brCon aTy n (replicate (2^n) arg)
        in  changed retVec
    go _ ty = error $ $(curLoc) ++ "reduceTReplicate: argument does not have a RTree type: " ++ showPpr ty

buildSNat :: DataCon -> Integer -> Term
buildSNat snatDc i =
  mkApps (Data snatDc)
         [Right (LitTy (NumTy i))
         ,Left (Literal (NaturalLiteral (toInteger i)))
         ]
