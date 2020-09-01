{-|
  Copyright  :  (C) 2015-2016, University of Twente,
                    2016     , Myrtle Software Ltd
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

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

  Partially handles:

    * Clash.Sized.Vector.unconcat
    * Clash.Sized.Vector.transpose
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Normalize.PrimitiveReductions where

import qualified Control.Lens                     as Lens
import           Control.Lens                     ((.=))
import           Data.List                        (mapAccumR)
import qualified Data.Maybe                       as Maybe
import           TextShow                         (showt)

import           PrelNames
  (boolTyConKey, typeNatAddTyFamNameKey, typeNatMulTyFamNameKey)
import           Unique                           (getKey)
import           SrcLoc                           (wiredInSrcSpan)

import           Clash.Core.DataCon               (DataCon)
import           Clash.Core.Literal               (Literal (..))
import           Clash.Core.Name
  (nameOcc, Name(..), NameSort(User), mkUnsafeSystemName)
import           Clash.Core.Pretty                (showPpr)
import           Clash.Core.Term
  (CoreContext (..), PrimInfo (..), Term (..), WorkInfo (..), Pat (..), AppArg (..),
   collectTermIds, mkArgApps)
import           Clash.Core.TermInfo
import           Clash.Core.Type
  (LitTy (..), Type (..), TypeView (..), mkFunTy, mkTyConApp, splitFunForallTy, tyView)
import           Clash.Core.TyCon
  (TyConMap, TyConName, tyConDataCons, tyConName)
import           Clash.Core.TysPrim
  (integerPrimTy, typeNatKind, liftedTypeKind, typeNatSubTyFamName, typeNatAddTyFamName)
import           Clash.Core.Util
  (appendToVec, extractElems, extractTElems, mkRTree,
   mkUniqInternalId, mkUniqSystemTyVar, mkVec, dataConInstArgTys,
   primCo, undefinedTm)
import           Clash.Core.Var                   (Var (..), mkTyVar, mkLocalId)
import           Clash.Core.VarEnv
  (InScopeSet, extendInScopeSetList)
import {-# SOURCE #-} Clash.Normalize.Strategy
import           Clash.Normalize.Types
import           Clash.Rewrite.Types
import           Clash.Rewrite.Util
import           Clash.Unique
import           Clash.Util
import qualified Clash.Util.Interpolate           as I

typeNatAdd :: TyConName
typeNatAdd =
  Name User "GHC.TypeNats.+" (getKey typeNatAddTyFamNameKey) wiredInSrcSpan

typeNatMul :: TyConName
typeNatMul =
  Name User "GHC.TypeNats.*" (getKey typeNatMulTyFamNameKey) wiredInSrcSpan

vecHeadPrim
  :: TyConName
  -- ^ Vec TyCon name
  -> Term
vecHeadPrim vecTcNm =
 -- head :: Vec (n+1) a -> a
  Prim (PrimInfo "Clash.Sized.Vector.head" (vecHeadTy vecTcNm) WorkNever)

vecLastPrim
  :: TyConName
  -- ^ Vec TyCon name
  -> Term
vecLastPrim vecTcNm =
  -- last :: Vec (n+1) a -> a
  -- has the same type signature as head, hence we're reusing its type
  -- definition here.
  Prim (PrimInfo "Clash.Sized.Vector.last" (vecHeadTy vecTcNm) WorkNever)

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
  Prim (PrimInfo "Clash.Sized.Vector.tail" (vecTailTy vecTcNm) WorkNever)

vecInitPrim
  :: TyConName
  -- ^ Vec TyCon name
  -> Term
vecInitPrim vecTcNm =
  -- init :: Vec (n + 1) a -> Vec n a
  -- has the same type signature as tail, hence we're reusing its type
  -- definition here.
  Prim (PrimInfo "Clash.Sized.Vector.init" (vecTailTy vecTcNm) WorkNever)

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
  -- ^ Length of the vector
  -> Term
  -- ^ Vector to extract head from
  -> (Term, Term)
  -- ^ (head of vector, tail of vector)
extractHeadTail consCon elTy n vec =
  ( Case vec elTy [(pat, Var el)]
  , Case vec restTy [(pat, Var rest)] )
 where
  tys = [LitTy (NumTy n), elTy, mkTyConApp typeNatSubTyFamName
                                           [LitTy (NumTy n),LitTy (NumTy 1)]]
  Just [coTy, _elTy, restTy] = dataConInstArgTys consCon tys

  mTV = mkTyVar typeNatKind (mkUnsafeSystemName "m" 0)
  co = mkLocalId coTy (mkUnsafeSystemName "_co_" 1)
  el = mkLocalId elTy (mkUnsafeSystemName "el" 2)
  rest = mkLocalId restTy (mkUnsafeSystemName "res" 3)

  pat = DataPat consCon [mTV] [co, el, rest]

-- Make case statement that projects the _head_ from a given vector
extractHead
  :: DataCon
  -- ^ The Cons (:>) constructor
  -> Type
  -- ^ Element type
  -> Integer
  -- ^ Length of the vector
  -> Term
  -- ^ Vector to extract head from
  -> Term
  -- ^ Head of vector
extractHead consCon elTy vLength vec =
  fst (extractHeadTail consCon elTy vLength vec)

-- Make case statement that projects the _tail_ from a given vector
extractTail
  :: DataCon
  -- ^ The Cons (:>) constructor
  -> Type
  -- ^ Element type
  -> Integer
  -- ^ Length of the vector
  -> Term
  -- ^ Vector to extract head from
  -> Term
  -- ^ Tail of vector
extractTail consCon elTy vLength vec =
  snd (extractHeadTail consCon elTy vLength vec)

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
  | otherwise =
    mkArgApps (Data consCon) [ TypeArg (LitTy (NumTy n))
                             , TypeArg resTy
                             , TypeArg (LitTy (NumTy (n-1)))
                             , TermArg (primCo consCoTy)
                             , TermArg h
                             , TermArg t ]

 where
  args = dataConInstArgTys consCon [LitTy (NumTy n), resTy, LitTy (NumTy (n-1))]
  Just (consCoTy : _) = args

-- | Create an empty vector
mkVecNil
  :: DataCon
  -- ^ The Nil constructor
  -> Type
  -- ^ The element type
  -> Term
mkVecNil nilCon resTy =
  mkArgApps (Data nilCon) [ TypeArg (LitTy (NumTy 0))
                          , TypeArg resTy
                          , TermArg (primCo nilCoTy) ]
 where
  args = dataConInstArgTys nilCon [LitTy (NumTy 0), resTy]
  Just (nilCoTy : _ ) = args

-- | Replace an application of the @Clash.Sized.Vector.reverse@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.reverse@
reduceReverse
  :: InScopeSet
  -> Integer
  -- ^ Length of the vector
  -> Type
  -- ^ Element of type of the vector
  -> Term
  -- ^ The vector to reverse
  -> NormalizeSession Term
reduceReverse inScope0 n elTy vArg = do
  tcm <- Lens.view tcCache
  let ty = termType vArg
  go tcm ty
 where
  go tcm (tyView -> TyConApp vecTcNm _)
    | Just vecTc <- lookupUniqMap vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
    , [nilCon, consCon] <- tyConDataCons vecTc
    = do
      uniqs0 <- Lens.use uniqSupply
      let (uniqs1,(vars,elems)) = second (second concat . unzip)
                                $ extractElems uniqs0 inScope0 consCon elTy 'V' n vArg
          lbody = mkVec nilCon consCon elTy n (reverse vars)
          lb    = Letrec (init elems) lbody
      uniqSupply Lens..= uniqs1
      changed lb
  go _ ty = error $ $(curLoc) ++ "reduceReverse: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.zipWith@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.zipWith@
reduceZipWith
  :: TransformContext
  -> PrimInfo -- ^ zipWith primitive info
  -> Integer  -- ^ Length of the vector(s)
  -> Type -- ^ Element type of the lhs of the function
  -> Type -- ^ Element type of the rhs of the function
  -> Type -- ^ Element type of the result of the function
  -> Term -- ^ The zipWith'd functions
  -> Term -- ^ The 1st vector argument
  -> Term -- ^ The 2nd vector argument
  -> NormalizeSession Term
reduceZipWith _ctx zipWithPrimInfo n lhsElTy rhsElTy resElTy fun lhsArg rhsArg = do
  tcm <- Lens.view tcCache
  changed (go tcm (termType lhsArg))
 where
  go tcm (tyView -> TyConApp vecTcNm _)
    | (Just vecTc) <- lookupUniqMap vecTcNm tcm
    , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
    , [nilCon, consCon] <- tyConDataCons vecTc
    = if n == 0 then
        mkVecNil nilCon resElTy
      else
        let
          (a, as) = extractHeadTail consCon lhsElTy n lhsArg
          (b, bs) = extractHeadTail consCon rhsElTy n rhsArg
          c = mkArgApps fun [TermArg a, TermArg b]
          cs = mkArgApps (Prim zipWithPrimInfo) [ TypeArg lhsElTy
                                                , TypeArg rhsElTy
                                                , TypeArg resElTy
                                                , TypeArg (LitTy (NumTy (n - 1)))
                                                , TermArg fun
                                                , TermArg as
                                                , TermArg bs ]
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
  :: TransformContext
  -> PrimInfo -- ^ map primitive info
  -> Integer  -- ^ Length of the vector
  -> Type -- ^ Argument type of the function
  -> Type -- ^ Result type of the function
  -> Term -- ^ The map'd function
  -> Term -- ^ The map'd over vector
  -> NormalizeSession Term
reduceMap _ctx mapPrimInfo n argElTy resElTy fun arg = do
    tcm <- Lens.view tcCache
    let ty = termType arg
    changed (go tcm ty)
  where
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- lookupUniqMap vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [nilCon,consCon] <- tyConDataCons vecTc
      = if n == 0 then
          mkVecNil nilCon argElTy
        else
          let
            nPredTy = LitTy (NumTy (n - 1))
            (a, as) = extractHeadTail consCon argElTy n arg
            b = mkArgApps fun [TermArg a]
            bs = mkArgApps (Prim mapPrimInfo) [ TypeArg argElTy
                                              , TypeArg resElTy
                                              , TypeArg nPredTy
                                              , TermArg fun
                                              , TermArg as ]
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
  :: TransformContext
  -> Integer  -- ^ Length of the vector
  -> Type -- ^ Argument type of the function
  -> Type -- ^ Result type of the function
  -> Term -- ^ The imap'd function
  -> Term -- ^ The imap'd over vector
  -> NormalizeSession Term
reduceImap (TransformContext is0 ctx) n argElTy resElTy fun arg = do
    tcm <- Lens.view tcCache
    let ty = termType arg
    go tcm ty
  where
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- lookupUniqMap vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [nilCon,consCon] <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        fun1 <- constantPropagation (TransformContext is0 (AppArg Nothing:ctx)) fun
        let is1 = extendInScopeSetList is0 (collectTermIds fun1)
            (uniqs1,nTv) = mkUniqSystemTyVar (uniqs0,is1) ("n",typeNatKind)
            (uniqs2,(vars,elems)) = second (second concat . unzip)
                                  $ uncurry extractElems uniqs1 consCon argElTy 'I' n arg
            (Right idxTy:_,_) = splitFunForallTy (termType fun)
            (TyConApp idxTcNm _) = tyView idxTy
            -- fromInteger# :: KnownNat n => Integer -> Index n
            idxFromIntegerTy = ForAllTy nTv
                                        (foldr mkFunTy
                                               (mkTyConApp idxTcNm
                                                           [VarTy nTv])
                                               [integerPrimTy,integerPrimTy])
            idxFromInteger   = Prim (PrimInfo "Clash.Sized.Internal.Index.fromInteger#" idxFromIntegerTy WorkNever)
            idxs             = map (App (App (TyApp idxFromInteger (LitTy (NumTy n)))
                                             (Literal (IntegerLiteral (toInteger n))))
                                   . Literal . IntegerLiteral . toInteger) [0..(n-1)]
            funApps          = zipWith (\i v -> App (App fun1 i) v) idxs vars
            lbody            = mkVec nilCon consCon resElTy n funApps
            lb               = Letrec (init elems) lbody
        uniqSupply Lens..= uniqs2
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceImap: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.iterateI@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.iterateI@
reduceIterateI
  :: TransformContext
  -> Integer
  -- ^ Length of vector
  -> Type
  -- ^ Vector's element type
  -> Type
  -- ^ Vector's type
  -> Term
  -- ^ iterateI's HO-function argument
  -> Term
  -- ^ iterateI's start value
  -> RewriteMonad NormalizeState Term
  -- ^ Fully unrolled definition
reduceIterateI (TransformContext is0 ctx) n aTy vTy f0 a = do
  tcm <- Lens.view tcCache
  f1 <- constantPropagation (TransformContext is0 (AppArg Nothing:ctx)) f0

  -- Generate uniq ids for element assignments.
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
    TyConApp vecTcNm _ = tyView vTy
    Just vecTc = lookupUniqMap vecTcNm tcm
    [nilCon, consCon] = tyConDataCons vecTc
    elems = map (App f1) (a:map Var elementIds)
    vec = mkVec nilCon consCon aTy n (take (fromInteger n) (a:map Var elementIds))

  -- Result:
  --   let
  --     el1 = f a
  --     el2 = f el1
  --     el3 = f el2
  --     ..
  --   in
  --     (a :> el1 :> el2 :> el3 :> ..)
  --
  pure (Letrec (zip elementIds elems) vec)

-- | Replace an application of the @Clash.Sized.Vector.traverse#@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.traverse#@
reduceTraverse
  :: TransformContext
  -> Integer  -- ^ Length of the vector
  -> Type -- ^ Element type of the argument vector
  -> Type -- ^ The type of the applicative
  -> Type -- ^ Element type of the result vector
  -> Term -- ^ The @Applicative@ dictionary
  -> Term -- ^ The function to traverse with
  -> Term -- ^ The argument vector
  -> NormalizeSession Term
reduceTraverse (TransformContext is0 ctx) n aTy fTy bTy dict fun arg = do
    tcm <- Lens.view tcCache
    let (TyConApp apDictTcNm _) = tyView (termType dict)
        ty = termType arg
    go tcm apDictTcNm ty
  where
    go tcm apDictTcNm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [nilCon,consCon] <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        fun1 <- constantPropagation (TransformContext is0 (AppArg Nothing:ctx)) fun
        let is1 = extendInScopeSetList is0 (collectTermIds fun1)
            (Just apDictTc)    = lookupUniqMap apDictTcNm tcm
            [apDictCon]        = tyConDataCons apDictTc
            (Just apDictIdTys) = dataConInstArgTys apDictCon [fTy]
            (uniqs1,apDictIds@[functorDictId,pureId,apId,_,_]) =
              mapAccumR mkUniqInternalId (uniqs0,is1)
                (zip ["functorDict","pure","ap","apConstL","apConstR"]
                     apDictIdTys)

            (TyConApp funcDictTcNm _) = tyView (head apDictIdTys)
            (Just funcDictTc) = lookupUniqMap funcDictTcNm tcm
            [funcDictCon] = tyConDataCons funcDictTc
            (Just funcDictIdTys) = dataConInstArgTys funcDictCon [fTy]
            (uniqs2,funcDicIds@[fmapId,_]) =
              mapAccumR mkUniqInternalId uniqs1
                (zip ["fmap","fmapConst"] funcDictIdTys)

            apPat    = DataPat apDictCon   [] apDictIds
            fnPat    = DataPat funcDictCon [] funcDicIds

            -- Extract the 'pure' function from the Applicative dictionary
            pureTy = varType pureId
            pureTm = Case dict pureTy [(apPat,Var pureId)]

            -- Extract the '<*>' function from the Applicative dictionary
            apTy   = varType apId
            apTm   = Case dict apTy [(apPat, Var apId)]

            -- Extract the Functor dictionary from the Applicative dictionary
            funcTy = varType functorDictId
            funcTm = Case dict funcTy
                               [(apPat,Var functorDictId)]

            -- Extract the 'fmap' function from the Functor dictionary
            fmapTy = varType fmapId
            fmapTm = Case (Var functorDictId) fmapTy
                          [(fnPat, Var fmapId)]

            (uniqs3,(vars,elems)) = second (second concat . unzip)
                                  $ uncurry extractElems uniqs2 consCon aTy 'T' n arg

            funApps = map (fun1 `App`) vars

            lbody   = mkTravVec vecTcNm nilCon consCon (Var (apDictIds!!1))
                                                       (Var (apDictIds!!2))
                                                       (Var (funcDicIds!!0))
                                                       bTy n funApps

            lb      = Letrec ([((apDictIds!!0), funcTm)
                              ,((apDictIds!!1), pureTm)
                              ,((apDictIds!!2), apTm)
                              ,((funcDicIds!!0), fmapTm)
                              ] ++ init elems) lbody
        uniqSupply Lens..= uniqs3
        changed lb
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
    go _ [] = mkArgApps pureTm [TypeArg (mkTyConApp vecTc [LitTy (NumTy 0),bTy])
                               ,TermArg (mkArgApps (Data nilCon)
                                           [TypeArg (LitTy (NumTy 0))
                                           ,TypeArg bTy
                                           ,TermArg  (primCo nilCoTy)])]

    go n (x:xs) = mkArgApps apTm
      [TypeArg (mkTyConApp vecTc [LitTy (NumTy (n-1)),bTy])
      ,TypeArg (mkTyConApp vecTc [LitTy (NumTy n),bTy])
      ,TermArg (mkArgApps fmapTm
                          [TypeArg bTy
                          ,TypeArg (mkFunTy (mkTyConApp vecTc [LitTy (NumTy (n-1)),bTy])
                                            (mkTyConApp vecTc [LitTy (NumTy n),bTy]))
                          ,TermArg  (mkArgApps (Data consCon)
                                               [TypeArg (LitTy (NumTy n))
                                               ,TypeArg bTy
                                               ,TypeArg (LitTy (NumTy (n-1)))
                                               ,TermArg (primCo (consCoTy n))
                                               ])
                          ,TermArg  x])
      ,TermArg (go (n-1) xs)]

    nilCoTy = head (Maybe.fromJust (dataConInstArgTys nilCon [(LitTy (NumTy 0))
                                                             ,bTy]))

    consCoTy n = head (Maybe.fromJust (dataConInstArgTys consCon
                                                         [(LitTy (NumTy n))
                                                         ,bTy
                                                         ,(LitTy (NumTy (n-1)))]))

-- | Replace an application of the @Clash.Sized.Vector.foldr@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.foldr@
reduceFoldr
  :: TransformContext
  -> PrimInfo
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
  -> NormalizeSession Term
reduceFoldr _ _ 0 _ _ start _ = changed start
reduceFoldr _ctx foldrPrimInfo n aTy fun start arg = do
    tcm <- Lens.view tcCache
    let ty = termType arg
    changed (go tcm ty)
  where
    go tcm (tyView -> TyConApp vecTcNm _)
      | nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , Just vecTc <- lookupUniqMap vecTcNm tcm
      , [_nilCon, consCon] <- tyConDataCons vecTc
      = let
          (a, as) = extractHeadTail consCon aTy n arg
          b = mkArgApps (Prim foldrPrimInfo) [ TypeArg aTy
                                             , TypeArg (termType start)
                                             , TypeArg (LitTy (NumTy (n - 1)))
                                             , TermArg fun
                                             , TermArg start
                                             , TermArg as ]
        in
          mkArgApps fun [TermArg a, TermArg b]

    go _ ty =
      error $ $(curLoc) ++ [I.i|
        reduceFoldr: argument does not have a vector type:

          #{showPpr ty}
      |]

-- | Replace an application of the @Clash.Sized.Vector.fold@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.fold@
reduceFold
  :: TransformContext
  -> Integer
  -- ^ Length of the vector
  -> Type
  -- ^ Element type of the argument vector
  -> Term
  -- ^ The function to fold with
  -> Term
  -- ^ The argument vector
  -> NormalizeSession Term
reduceFold (TransformContext is0 ctx) n aTy fun arg = do
    tcm <- Lens.view tcCache
    let ty = termType arg
    go tcm ty
  where
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [_,consCon]  <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        fun1 <- constantPropagation (TransformContext is0 (AppArg Nothing:ctx)) fun
        let is1 = extendInScopeSetList is0 (collectTermIds fun1)
            (uniqs1,(vars,elems)) = second (second concat . unzip)
                                  $ extractElems uniqs0 is1 consCon aTy 'F' n arg
            lbody            = foldV fun1 vars
            lb               = Letrec (init elems) lbody
        uniqSupply Lens..= uniqs1
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceFold: argument does not have a vector type: " ++ showPpr ty

    foldV _ [a] = a
    foldV f as  = let (l,r) = splitAt (length as `div` 2) as
                      lF    = foldV f l
                      rF    = foldV f r
                  in  mkArgApps f [TermArg lF, TermArg rF]

-- | Replace an application of the @Clash.Sized.Vector.dfold@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.dfold@
reduceDFold
  :: InScopeSet
  -> Type
  -- ^ Expected type of the result
  -> Integer
  -- ^ Length of the vector
  -> Type
  -- ^ Element type of the argument vector
  -> Term
  -- ^ Function to fold with
  -> Term
  -- ^ Starting value
  -> Term
  -- ^ The vector to fold
  -> NormalizeSession Term
reduceDFold _ oldTy 0 _ _ start _ =
  let newTy = termType start
   in if oldTy == newTy then
        changed start
      else
        changed (Cast start newTy oldTy)
reduceDFold is0 oldTy n aTy fun start arg = do
    tcm <- Lens.view tcCache
    let ty = termType arg
    go tcm ty
  where
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [_,consCon]  <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let is1 = extendInScopeSetList is0 (collectTermIds fun)
            -- TODO: Should 'constantPropagation' be used on 'fun'? It seems to
            -- TOOD: be used for every other function in this module.
            (uniqs1,(vars,elems)) = second (second concat . unzip)
                                  $ extractElems uniqs0 is1 consCon aTy 'D' n arg
            (_ltv:Right snTy:_,_) = splitFunForallTy (termType fun)
            (TyConApp snatTcNm _) = tyView snTy
            (Just snatTc)         = lookupUniqMap snatTcNm tcm
            [snatDc]              = tyConDataCons snatTc
            lbody = doFold (buildSNat snatDc) (n-1) vars
            newTy = termType lbody
            lbody1 = if newTy == oldTy then
                       lbody
                     else
                       Cast lbody newTy oldTy
            lb    = Letrec (init elems) lbody1
        uniqSupply Lens..= uniqs1
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceDFold: argument does not have a vector type: " ++ showPpr ty

    doFold _    _ []     = start
    doFold snDc k (x:xs) = mkArgApps fun [TypeArg (mkKTy k)
                                         ,TermArg (snDc k)
                                         ,TermArg x
                                         ,TermArg (doFold snDc (k-1) xs)
                                         ]

    mkKTy 0 = LitTy (NumTy 0)
    mkKTy k = mkTyConApp typeNatAddTyFamName [ mkKTy (k-1), LitTy (NumTy 1) ]

-- | Replace an application of the @Clash.Sized.Vector.head@ primitive on
-- vectors of a known length @n@, by a projection of the first element of a
-- vector.
reduceHead
  :: InScopeSet
  -> Integer  -- ^ Length of the vector
  -> Type -- ^ Element type of the vector
  -> Term -- ^ The argument vector
  -> NormalizeSession Term
reduceHead inScope n aTy vArg = do
    tcm <- Lens.view tcCache
    let ty = termType vArg
    go tcm ty
  where
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [_,consCon]  <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let (uniqs1,(vars,elems)) = second (second concat . unzip)
                                  $ extractElems uniqs0 inScope consCon aTy 'H' n vArg
            lb = Letrec [head elems] (head vars)
        uniqSupply Lens..= uniqs1
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceHead: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.tail@ primitive on
-- vectors of a known length @n@, by a projection of the tail of a
-- vector.
reduceTail
  :: InScopeSet
  -> Integer  -- ^ Length of the vector
  -> Type -- ^ Element type of the vector
  -> Term -- ^ The argument vector
  -> NormalizeSession Term
reduceTail inScope n aTy vArg = do
    tcm <- Lens.view tcCache
    let ty = termType vArg
    go tcm ty
  where
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [_,consCon]  <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let (uniqs1,(_,elems)) = second (second concat . unzip)
                               $ extractElems uniqs0 inScope consCon aTy 'L' n vArg
            b@(tB,_)     = elems !! 1
            lb           = Letrec [b] (Var tB)
        uniqSupply Lens..= uniqs1
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceTail: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.last@ primitive on
-- vectors of a known length @n@, by a projection of the last element of a
-- vector.
reduceLast
  :: InScopeSet
  -> Integer  -- ^ Length of the vector
  -> Type -- ^ Element type of the vector
  -> Term -- ^ The argument vector
  -> NormalizeSession Term
reduceLast inScope n aTy vArg = do
    tcm <- Lens.view tcCache
    let ty = termType vArg
    go tcm ty
  where
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [_,consCon]  <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let (uniqs1,(_,elems)) = second unzip
                               $ extractElems uniqs0 inScope consCon aTy 'L' n vArg
            (tB,_)       = head (last elems)
        uniqSupply Lens..= uniqs1
        case n of
         0 -> changed (undefinedTm aTy)
         _ -> changed (Letrec (init (concat elems)) (Var tB))
    go _ ty = error $ $(curLoc) ++ "reduceLast: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.init@ primitive on
-- vectors of a known length @n@, by a projection of the init of a
-- vector.
reduceInit
  :: InScopeSet
  -> PrimInfo -- ^ Primitive info for 'init'
  -> Integer  -- ^ Length of the vector
  -> Type -- ^ Element type of the vector
  -> Term -- ^ The argument vector
  -> NormalizeSession Term
reduceInit _inScope initPrimInfo n aTy vArg = do
  tcm <- Lens.view tcCache
  let ty = termType vArg
  changed (go tcm ty)
 where
  go tcm (tyView -> TyConApp vecTcNm _)
    | (Just vecTc) <- lookupUniqMap vecTcNm tcm
    , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
    , [nilCon, consCon]  <- tyConDataCons vecTc
    = if n == 0 then
        mkVecNil nilCon aTy
      else
        let
          nPredTy = TypeArg (LitTy (NumTy (n - 1)))
          (a, as0) = extractHeadTail consCon aTy (n+1) vArg
          as1 = mkArgApps (Prim initPrimInfo) [nPredTy, TypeArg aTy, TermArg as0]
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
  :: InScopeSet
  -> Integer  -- ^ Length of the LHS arg
  -> Integer  -- ^ Lenght of the RHS arg
  -> Type -- ^ Element type of the vectors
  -> Term -- ^ The LHS argument
  -> Term -- ^ The RHS argument
  -> NormalizeSession Term
reduceAppend inScope n m aTy lArg rArg = do
    tcm <- Lens.view tcCache
    let ty = termType lArg
    go tcm ty
  where
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [_,consCon]  <- tyConDataCons vecTc
      = do uniqs0 <- Lens.use uniqSupply
           let (uniqs1,(vars,elems)) = second (second concat . unzip)
                                     $ extractElems uniqs0 inScope consCon aTy
                                         'C' n lArg
               lbody        = appendToVec consCon aTy rArg (n+m) vars
               lb           = Letrec (init elems) lbody
           uniqSupply Lens..= uniqs1
           changed lb
    go _ ty = error $ $(curLoc) ++ "reduceAppend: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.unconcat@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.unconcat@
reduceUnconcat :: Integer  -- ^ Length of the result vector
               -> Integer  -- ^ Length of the elements of the result vector
               -> Type -- ^ Element type
               -> Term -- ^ Argument vector
               -> NormalizeSession Term
reduceUnconcat n 0 aTy arg = do
    tcm <- Lens.view tcCache
    let ty = termType arg
    go tcm ty
  where
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- lookupUniqMap vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [nilCon,consCon] <- tyConDataCons vecTc
      = let nilVec           = mkVec nilCon consCon aTy 0 []
            innerVecTy       = mkTyConApp vecTcNm [LitTy (NumTy 0), aTy]
            retVec           = mkVec nilCon consCon innerVecTy n (replicate (fromInteger n) nilVec)
        in  changed retVec
    go _ ty = error $ $(curLoc) ++ "reduceUnconcat: argument does not have a vector type: " ++ showPpr ty

reduceUnconcat _ _ _ _ = error $ $(curLoc) ++ "reduceUnconcat: unimplemented"

-- | Replace an application of the @Clash.Sized.Vector.transpose@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.transpose@
reduceTranspose :: Integer  -- ^ Length of the result vector
                -> Integer  -- ^ Length of the elements of the result vector
                -> Type -- ^ Element type
                -> Term -- ^ Argument vector
                -> NormalizeSession Term
reduceTranspose n 0 aTy arg = do
    tcm <- Lens.view tcCache
    let ty = termType arg
    go tcm ty
  where
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- lookupUniqMap vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [nilCon,consCon] <- tyConDataCons vecTc
      = let nilVec           = mkVec nilCon consCon aTy 0 []
            innerVecTy       = mkTyConApp vecTcNm [LitTy (NumTy 0), aTy]
            retVec           = mkVec nilCon consCon innerVecTy n (replicate (fromInteger n) nilVec)
        in  changed retVec
    go _ ty = error $ $(curLoc) ++ "reduceTranspose: argument does not have a vector type: " ++ showPpr ty

reduceTranspose _ _ _ _ = error $ $(curLoc) ++ "reduceTranspose: unimplemented"

reduceReplicate :: Integer
                -> Type
                -> Type
                -> Term
                -> NormalizeSession Term
reduceReplicate n aTy eTy arg = do
    tcm <- Lens.view tcCache
    go tcm eTy
  where
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- lookupUniqMap vecTcNm tcm
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
  :: InScopeSet
  -> Integer
  -- ^ Size of vector
  -> Type
  -- ^ Type of vector element
  -> Type
  -- ^ Type of vector
  -> Term
  -- ^ Vector
  -> Term
  -- ^ Index
  -> Term
  -- ^ Element
  -> NormalizeSession Term
reduceReplace_int is0 n aTy vTy v i newA = do
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
    (Just boolTc) = lookupUniqMap (getKey boolTyConKey) tcm
    [_,trueDc]    = tyConDataCons boolTc
    eqInt         = eqIntPrim iTy (mkTyConApp (tyConName boolTc) [])
    case0         = Case (mkArgApps eqInt
                                    [TermArg i
                                    ,TermArg (mkArgApps
                                                (Data iDc)
                                                [TermArg (Literal (IntLiteral elIndex))])
                                    ])
                         aTy
                         [(DefaultPat, oldA)
                         ,(DataPat trueDc [] [], newA)
                         ]

  -- Equality on lifted Int that returns a Bool
  eqIntPrim
    :: Type
    -> Type
    -> Term
  eqIntPrim intTy boolTy =
    Prim (PrimInfo "Clash.Transformations.eqInt" (mkFunTy intTy (mkFunTy intTy boolTy)) WorkVariable)

  go tcm (tyView -> TyConApp vecTcNm _)
    | (Just vecTc)     <- lookupUniqMap vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
    , [nilCon,consCon] <- tyConDataCons vecTc
    = do
      -- Get data constructors of 'Int'
      uniqs0                   <- Lens.use uniqSupply
      let iTy                   = termType i
          (TyConApp iTcNm _)    = tyView iTy
          (Just iTc)            = lookupUniqMap iTcNm tcm
          [iDc]                 = tyConDataCons iTc

      -- Get elements from vector
          (uniqs1,(vars,elems)) = second (second concat . unzip)
                                $ extractElems
                                    uniqs0
                                    is0
                                    consCon
                                    aTy
                                    'I'
                                    n
                                    v

      -- Replace every element with (if i == elIndex then newA else oldA)
      let replacedEls = zipWith (replace_intElement tcm iDc iTy) vars [0..]
          lbody       = mkVec nilCon consCon aTy n replacedEls
          lb          = Letrec (init elems) lbody
      uniqSupply Lens..= uniqs1
      changed lb
  go _ ty = error $ $(curLoc) ++ "reduceReplace_int: argument does not have "
                                ++ "a vector type: " ++ showPpr ty

-- TODO: Take a shortcut when given index is a literal. Right now, this function
-- TODO: simply creates a case statement for every element in the vector, which
-- TODO: Clash will eliminate one-by-one if the index turned out to be literal.
-- TODO: It would of course be best to not create the cases in the first place!
reduceIndex_int
  :: InScopeSet
  -> Integer
  -- ^ Size of vector
  -> Type
  -- ^ Type of vector element
  -> Term
  -- ^ Vector
  -> Term
  -- ^ Index
  -> NormalizeSession Term
reduceIndex_int is0 n aTy v i = do
  tcm <- Lens.view tcCache
  let vTy = termType v
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
    (Just boolTc) = lookupUniqMap (getKey boolTyConKey) tcm
    [_,trueDc]    = tyConDataCons boolTc
    eqInt         = eqIntPrim iTy (mkTyConApp (tyConName boolTc) [])
    case0         = Case (mkArgApps eqInt
                                    [TermArg i
                                    ,TermArg (mkArgApps (Data iDc)
                                             [TermArg (Literal (IntLiteral elIndex))])
                                    ])
                         aTy
                         [(DefaultPat, next)
                         ,(DataPat trueDc [] [], cur)
                         ]

  -- Equality on lifted Int that returns a Bool
  eqIntPrim
    :: Type
    -> Type
    -> Term
  eqIntPrim intTy boolTy =
    Prim (PrimInfo "Clash.Transformations.eqInt" (mkFunTy intTy (mkFunTy intTy boolTy)) WorkVariable)

  go tcm (tyView -> TyConApp vecTcNm _)
    | (Just vecTc)     <- lookupUniqMap vecTcNm tcm
    , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
    , [_nilCon,consCon] <- tyConDataCons vecTc
    = do
      -- Get data constructors of 'Int'
      uniqs0                   <- Lens.use uniqSupply
      let iTy                   = termType i
          (TyConApp iTcNm _)    = tyView iTy
          (Just iTc)            = lookupUniqMap iTcNm tcm
          [iDc]                 = tyConDataCons iTc

      -- Get elements from vector
          (uniqs1,(vars,elems)) = second (second concat . unzip)
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
                              (undefinedTm aTy)
                              (zip vars [0..])
          lb      = Letrec (init elems) indexed
      uniqSupply Lens..= uniqs1
      changed lb
  go _ ty = error $ $(curLoc) ++ "indexReplace_int: argument does not have "
                              ++ "a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.dtfold@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.dtfold@
reduceDTFold
  :: InScopeSet
  -> Integer  -- ^ Length of the vector
  -> Type     -- ^ Element type of the argument vector
  -> Term     -- ^ Function to convert elements with
  -> Term     -- ^ Function to combine branches with
  -> Term     -- ^ The vector to fold
  -> NormalizeSession Term
reduceDTFold inScope n aTy lrFun brFun arg = do
    tcm <- Lens.view tcCache
    let ty = termType arg
    go tcm ty
  where
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , nameOcc vecTcNm == "Clash.Sized.Vector.Vec"
      , [_,consCon]  <- tyConDataCons vecTc
      = do uniqs0 <- Lens.use uniqSupply
           let (uniqs1,(vars,elems)) = second (second concat . unzip)
                                     $ extractElems uniqs0 inScope consCon aTy
                                         'T' (2^n) arg
               (_ltv:Right snTy:_,_) = splitFunForallTy (termType brFun)
               (TyConApp snatTcNm _) = tyView snTy
               (Just snatTc)         = lookupUniqMap snatTcNm tcm
               [snatDc]              = tyConDataCons snatTc
               lbody = doFold (buildSNat snatDc) (n-1) vars
               lb    = Letrec (init elems) lbody
           uniqSupply Lens..= uniqs1
           changed lb
    go _ ty = error $ $(curLoc) ++ "reduceDTFold: argument does not have a vector type: " ++ showPpr ty

    doFold :: (Integer -> Term) -> Integer -> [Term] -> Term
    doFold _    _ [x] = mkArgApps lrFun [TermArg x]
    doFold snDc k xs  =
      let (xsL,xsR) = splitAt (2^k) xs
          k'        = k-1
          eL        = doFold snDc k' xsL
          eR        = doFold snDc k' xsR
      in  mkArgApps brFun [TypeArg (LitTy (NumTy k))
                          ,TermArg (snDc k)
                          ,TermArg eL
                          ,TermArg eR
                          ]

-- | Replace an application of the @Clash.Sized.RTree.tdfold@ primitive on
-- trees of a known depth @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.RTree.tdfold@
reduceTFold
  :: InScopeSet
  -> Integer -- ^ Depth of the tree
  -> Type    -- ^ Element type of the argument tree
  -> Term    -- ^ Function to convert elements with
  -> Term    -- ^ Function to combine branches with
  -> Term    -- ^ The tree to fold
  -> NormalizeSession Term
reduceTFold inScope n aTy lrFun brFun arg = do
    tcm <- Lens.view tcCache
    let ty = termType arg
    go tcm ty
  where
    go tcm (tyView -> TyConApp treeTcNm _)
      | (Just treeTc) <- lookupUniqMap treeTcNm tcm
      , nameOcc treeTcNm == "Clash.Sized.RTree.RTree"
      , [lrCon,brCon] <- tyConDataCons treeTc
      = do uniqs0 <- Lens.use uniqSupply
           let (uniqs1,(vars,elems)) = extractTElems uniqs0 inScope lrCon brCon aTy 'T' n arg
               (_ltv:Right snTy:_,_) = splitFunForallTy (termType brFun)
               (TyConApp snatTcNm _) = tyView snTy
               (Just snatTc)         = lookupUniqMap snatTcNm tcm
               [snatDc]              = tyConDataCons snatTc
               lbody = doFold (buildSNat snatDc) (n-1) vars
               lb    = Letrec elems lbody
           uniqSupply Lens..= uniqs1
           changed lb
    go _ ty = error $ $(curLoc) ++ "reduceTFold: argument does not have a tree type: " ++ showPpr ty

    doFold _    _ [x] = mkArgApps lrFun [TermArg x]
    doFold snDc k xs  =
      let (xsL,xsR) = splitAt (length xs `div` 2) xs
          k'        = k-1
          eL        = doFold snDc k' xsL
          eR        = doFold snDc k' xsR
      in  mkArgApps brFun [TypeArg (LitTy (NumTy k))
                          ,TermArg (snDc k)
                          ,TermArg eL
                          ,TermArg eR
                          ]

reduceTReplicate :: Integer -- ^ Depth of the tree
                 -> Type    -- ^ Element type
                 -> Type    -- ^ Result type
                 -> Term    -- ^ Element
                 -> NormalizeSession Term
reduceTReplicate n aTy eTy arg = do
    tcm <- Lens.view tcCache
    go tcm eTy
  where
    go tcm (tyView -> TyConApp treeTcNm _)
      | (Just treeTc) <- lookupUniqMap treeTcNm tcm
      , nameOcc treeTcNm == "Clash.Sized.RTree.RTree"
      , [lrCon,brCon] <- tyConDataCons treeTc
      = let retVec = mkRTree lrCon brCon aTy n (replicate (2^n) arg)
        in  changed retVec
    go _ ty = error $ $(curLoc) ++ "reduceTReplicate: argument does not have a RTree type: " ++ showPpr ty

buildSNat :: DataCon -> Integer -> Term
buildSNat snatDc i =
  mkArgApps (Data snatDc)
            [TypeArg (LitTy (NumTy i))
            ,TermArg (Literal (NaturalLiteral (toInteger i)))
            ]
