{-|
  Copyright  :  (C) 2015-2016, University of Twente,
                    2016     , Myrtle Software Ltd,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of primitives, on scoped Clash Core. They replace an application
  of a primitive on vectors (or trees) of a known length by its unrolled
  definition. 'Clash.Normalize.Scoped.Transformations.Reduce.reducePrim'
  decides whether to, and calls them on the saturated application.

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
    * Clash.Sized.Vector.last
    * Clash.Sized.Vector.init
    * Clash.Sized.Vector.replicate
    * Clash.Sized.Vector.replace_int
    * Clash.Sized.Vector.index_int
    * Clash.Sized.Vector.imap
    * Clash.Sized.Vector.iterateI
    * Clash.Sized.Vector.dtfold
    * Clash.Sized.RTree.tdfold
    * Clash.Sized.RTree.treplicate
    * Clash.Sized.Vector.reverse
    * Clash.Sized.Vector.unconcat

  Partially handles:

    * Clash.Sized.Vector.transpose

  Every reduction takes the context of the application as its last argument.
  The binders it creates get fresh names from the scope of the context, so
  they can't capture variables of the arguments.

  The reductions of @fold@, @imap@, @iterateI@ and @traverse#@ first run the
  'Clash.Normalize.Strategy.constantPropagation' strategy on their function
  argument.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Clash.Normalize.Scoped.PrimitiveReductions
  ( reduceZipWith
  , reduceMap
  , reduceImap
  , reduceIterateI
  , reduceTraverse
  , reduceFoldr
  , reduceFold
  , reduceDFold
  , reduceHead
  , reduceTail
  , reduceLast
  , reduceInit
  , reduceAppend
  , reduceUnconcat
  , reduceTranspose
  , reduceReplicate
  , reduceReplace_int
  , reduceIndex_int
  , reduceDTFold
  , reduceTFold
  , reduceTReplicate
  , reduceReverse
  ) where

import qualified Control.Lens as Lens
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text.Extra (showt)
import qualified GHC.Classes
import GHC.Stack (HasCallStack)

import GHC.Builtin.Names (boolTyConKey)

import Clash.Core.Basic (IsMultiPrim (..), LitTy (..), WorkInfo (..))
import Clash.Core.DataCon (DataCon)
import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (nameOcc)
import qualified Clash.Core.Name as C
import Clash.Core.Pretty (showPpr)
import Clash.Core.TyCon (TyConName, tyConDataCons, tyConName)
import Clash.Core.TysPrim (integerPrimTyConName, typeNatKindTyConName)

import qualified Clash.Data.UniqMap as UniqMap
import qualified Clash.Normalize.Scoped.Primitives as NP (undefined)
import Clash.Normalize.Types (NormalizeSession, constantPropagationStrategy)
import Clash.Rewrite.Types (extra, typeEnv)
import Clash.Sized.RTree (RTree)
import Clash.Sized.Vector (Vec)
import Clash.Unique (fromGhcUnique)

import Clash.Core.Builder (forAllTyB, funTyB, runClosedType, tyConAppB, varTyB)
import Clash.Core.DataCon (dataConInstArgTys)
import Clash.Core.HasType (inferCoreTypeOf)
import Clash.Core.Subst
  (closeType, extendTySubst, idSubst, substTy, withFreshIdBinder, withFreshIdBinders)
import Clash.Core.Syntax
import Clash.Core.Term (appendTele, mkApps, stripTicks)
import Clash.Core.Type
  ( PiTele (..), SplitFunForall (..), TypeView (..), coreView, mkFunTy, mkTyConApp
  , normalizeType, splitFunForallTy, tyView )
import Clash.Core.TypeEnv (TypeEnv, lookupTyCon, typeEnvTyConMap)
import Clash.Core.Util
  ( appendToVec, extractElems, extractTElems, internalName, mkRTree, mkVec, primCo
  , selectField, systemName )
import Clash.Rewrite.Scoped
  (Frame (AppArg), Transform (..), TransformContext, changed, pushFrame, tfScope)

-- * Helpers

-- | A natural number as a type
numTy :: Integer -> Type n
numTy = LitTy . NumTy

-- | The scope and type environment of a reduction
scopeAndTypeEnv :: TransformContext n -> NormalizeSession (Scope n, TypeEnv)
scopeAndTypeEnv ctx = (tfScope ctx,) <$> Lens.view typeEnv

-- | Run the 'Clash.Normalize.Strategy.constantPropagation' strategy on the
-- function argument of a primitive. The compiled strategy is taken from the
-- normalization state, see the documentation of
-- 'Clash.Normalize.Types._normalizationStrategy' for why it is not
-- simply imported.
constantPropagation
  :: (HasCallStack, Distinct n) => TransformContext n -> Term n -> NormalizeSession (Term n)
constantPropagation ctx fun = do
  strategy <- Lens.use (extra . constantPropagationStrategy)
  runTransform strategy (pushFrame (AppArg Nothing) ctx) fun

-- | The type constructor of @Vec@, and its @Nil@ and @Cons@ constructors, if
-- the type is a vector under 'coreView'. Errors otherwise.
withVec
  :: (HasCallStack, Distinct n)
  => String
  -- ^ Name of the reduction, for the error message
  -> TypeEnv
  -> Scope n
  -> Type n
  -> (TyConName -> DataCon -> DataCon -> r)
  -> r
withVec nm te sc ty k = withTyConDataCons nm vecTcName te (coreView te sc ty) k

-- | The type constructor at the head of a type and its two data constructors,
-- if it has the given name. Errors otherwise.
withTyConDataCons
  :: HasCallStack
  => String
  -- ^ Name of the reduction, for the error message
  -> Text
  -- ^ Name of the type constructor
  -> TypeEnv
  -> Type n
  -> (TyConName -> DataCon -> DataCon -> r)
  -> r
withTyConDataCons nm tcNm te ty k = case tyView ty of
  TyConApp tcNm' _
    | nameOcc tcNm' == tcNm
    , Just tc <- lookupTyCon te tcNm'
    , [dc1, dc2] <- tyConDataCons tc
    -> k tcNm' dc1 dc2
  _ -> error (nm <> ": argument does not have a " <> show tcNm <> " type: " <> showPpr ty)

vecTcName :: Text
vecTcName = showt ''Vec

rtreeTcName :: Text
rtreeTcName = showt ''RTree

-- | Let-bind vectors: each vector that is representable and isn't a variable
-- already. Gives the continuation the shared vectors, in the scope of the
-- let-bindings.
--
-- This makes sure the head and tail projections of 'extractHeadTail' share
-- the vector instead of each getting their own copy. Because single-step
-- reductions feed the tail projection back into the primitive, such copies
-- pile up: unrolling a vector of length @n@ would otherwise result in @n@
-- copies of the vector. That is harmless for local variables, but a global
-- binder gets inlined (and normalized) at every occurrence. See:
--
--   https://github.com/clash-lang/clash-compiler/issues/3461
--
-- Non-representable vectors are never let-bound:
-- 'Clash.Normalize.Scoped.Transformations.Inline.inlineOrLiftNonRep' would
-- inline or lift such a let-binding, undoing the sharing.
shareVecs
  :: forall n
   . Distinct n
  => TypeEnv
  -> Scope n
  -> [(Bool, Term n)]
  -- ^ Whether the vector is representable, and the vector
  -> (forall l. DExt n l => Scope l -> [Term l] -> NormalizeSession (Term l))
  -> NormalizeSession (Term n)
shareVecs te sc0 vecs0 k = go sc0 [] vecs0
 where
  go :: forall x. DExt n x => Scope x -> [Term x] -> [(Bool, Term n)] -> NormalizeSession (Term x)
  go sc acc [] = k sc (reverse acc)
  go sc acc ((isRepresentable, vec0) : vecs)
    | not isRepresentable = go sc (vec : acc) vecs
    | Var {} <- stripTicks vec = go sc (vec : acc) vecs
    | otherwise =
        withFreshIdBinder sc (internalName "vec") (inferCoreTypeOf te sc vec) $ \(sc1 :: Scope l) b ->
          withExtTrans @n @x @l $
            Let (NonRec b vec) <$> go sc1 (idBinderVar b : sink1 acc) vecs
   where
    vec = sink vec0

-- | The head and the tail of a non-empty vector, as two case-expressions: the
-- first one extracts the head of the vector, the second one its tail. Use
-- 'shareVecs' on the vector first to prevent it from being duplicated.
extractHeadTail
  :: (HasCallStack, Distinct n)
  => TypeEnv
  -> Scope n
  -> DataCon
  -- ^ The @Cons@ constructor
  -> Type n
  -- ^ Element type
  -> Integer
  -- ^ Length of the vector, must be positive
  -> Term n
  -> (Term n, Term n)
extractHeadTail te sc consCon elTy n vec = (sel 1, sel 2)
 where
  sel = selectField te sc consCon [numTy n, elTy, numTy (n - 1)] 1 ["_co_", "el", "res"] vec

-- | Create a vector of length @n@ out of its head and its tail
mkVecCons
  :: (HasCallStack, Distinct n)
  => TypeEnv -> Scope n -> DataCon -> Type n -> Integer -> Term n -> Term n -> Term n
mkVecCons te sc consCon elTy n h t
  | n <= 0 = error "mkVecCons: n <= 0"
  | otherwise = appendToVec te sc consCon elTy t n [h]

-- | Create an empty vector
mkVecNil :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> DataCon -> DataCon -> Type n -> Term n
mkVecNil te sc nilCon consCon elTy = mkVec te sc nilCon consCon elTy 0 []

-- | Let-bind the element selections of 'extractElems', in a recursive
-- let-binding
recLet :: Tele IdBinder n l -> [Term l] -> Term l -> Term n
recLet bs rhss = Let (Rec bs rhss)

-- | The first data constructor of the type constructor at the head of a type,
-- without looking through 'coreView'
firstDataConOf :: TypeEnv -> Type n -> Maybe DataCon
firstDataConOf te ty = case tyView ty of
  TyConApp tcNm _ -> lookupTyCon te tcNm >>= listToMaybe . tyConDataCons
  _ -> Nothing

-- | The @SNat@ data constructor, given the type of an argument of type @SNat
-- l@
buildSNat :: DataCon -> Integer -> Term n
buildSNat snatDc i =
  mkApps (Data snatDc) [Right (numTy i), Left (Literal (NaturalLiteral i))]

-- | The @True@ constructor and the type @Bool@
boolTrue :: HasCallStack => TypeEnv -> (DataCon, Type n)
boolTrue te = fromMaybe (error "reducePrim: failed to build the True DC") $ do
  boolTc <- UniqMap.lookup (fromGhcUnique boolTyConKey) (typeEnvTyConMap te)
  [_, trueDc] <- pure (tyConDataCons boolTc)
  pure (trueDc, mkTyConApp (tyConName boolTc) [])

-- | Equality on lifted @Int@, returning a @Bool@, applied to the index given to
-- @replace_int@ or @index_int@ and the given index, which is the statically
-- known index of an element of the vector.
eqIntWith :: HasCallStack => TypeEnv -> Type n -> Term n -> Integer -> Term n
eqIntWith te iTy i elIndex =
  mkApps (Prim eqInt) [Left i, Left (mkApps (Data iDc) [Left (Literal (IntLiteral elIndex))])]
 where
  iDc = fromMaybe (error "reducePrim: failed to build the Int DC") (firstDataConOf te iTy)
  -- The type of a primitive is closed
  iTyC = fromMaybe (error ("reducePrim: open Int type: " <> showPpr iTy)) (closeType iTy)
  eqInt = mkPrimInfo
    (showt 'GHC.Classes.eqInt)
    (mkFunTy iTyC (mkFunTy iTyC (snd (boolTrue te))))
    WorkVariable
    SingleResult
    NoUnfolding

-- * Reductions

-- | Replace an application of the @Clash.Sized.Vector.reverse@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.reverse@.
reduceReverse
  :: (HasCallStack, Distinct n)
  => Integer
  -- ^ Length of the vector
  -> Type n
  -- ^ Element type of the vector
  -> Term n
  -- ^ The vector to reverse
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceReverse n elTy vArg ctx = do
  (sc, te) <- scopeAndTypeEnv ctx
  withVec "reduceReverse" te sc (inferCoreTypeOf te sc vArg) $ \_ nilCon consCon ->
    changed $
      if n == 0 then mkVecNil te sc nilCon consCon elTy else
      extractElems te sc consCon elTy 'V' n n vArg $ \sc1 bs rhss vars _ ->
        recLet bs rhss (mkVec te sc1 nilCon consCon (sink elTy) n (reverse vars))

-- | Replace an application of the @Clash.Sized.Vector.zipWith@ primitive on
-- vectors of a known length @n@, by one step of its recursive "definition":
-- the function applied to the heads of the vectors, consed onto @zipWith@
-- applied to their tails.
reduceZipWith
  :: (HasCallStack, Distinct n)
  => PrimInfo -- ^ zipWith primitive info
  -> Integer  -- ^ Length of the vector(s)
  -> Type n -- ^ Element type of the lhs of the function
  -> Type n -- ^ Element type of the rhs of the function
  -> Type n -- ^ Element type of the result of the function
  -> Bool -- ^ Whether the 1st vector argument is representable, see 'shareVecs'
  -> Bool -- ^ Whether the 2nd vector argument is representable, see 'shareVecs'
  -> Term n -- ^ The zipWith'd functions
  -> Term n -- ^ The 1st vector argument
  -> Term n -- ^ The 2nd vector argument
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceZipWith zipWithPrimInfo n lhsElTy rhsElTy resElTy lhsRep rhsRep fun lhsArg0 rhsArg0 ctx = do
  (sc0, te) <- scopeAndTypeEnv ctx
  withVec "reduceZipWith" te sc0 (inferCoreTypeOf te sc0 lhsArg0) $ \_ nilCon consCon ->
    changed =<< shareVecs te sc0 [(lhsRep, lhsArg0), (rhsRep, rhsArg0)] (\sc -> \case
      [lhsArg, rhsArg] -> pure $
        if n == 0 then mkVecNil te sc nilCon consCon (sink resElTy) else
        let
          (a, as) = extractHeadTail te sc consCon (sink lhsElTy) n lhsArg
          (b, bs) = extractHeadTail te sc consCon (sink rhsElTy) n rhsArg
          c = mkApps (sink fun) [Left a, Left b]
          cs = mkApps (Prim zipWithPrimInfo)
                 [ Right (sink lhsElTy)
                 , Right (sink rhsElTy)
                 , Right (sink resElTy)
                 , Right (numTy (n - 1))
                 , Left (sink fun)
                 , Left as
                 , Left bs ]
        in mkVecCons te sc consCon (sink resElTy) n c cs
      _ -> error "reduceZipWith: shareVecs")

-- | Replace an application of the @Clash.Sized.Vector.map@ primitive on
-- vectors of a known length @n@, by one step of its recursive "definition":
-- the function applied to the head of the vector, consed onto @map@ applied to
-- its tail. For an empty vector, it gives an empty vector of the result
-- element type.
reduceMap
  :: (HasCallStack, Distinct n)
  => PrimInfo -- ^ map primitive info
  -> Integer  -- ^ Length of the vector
  -> Type n -- ^ Argument type of the function
  -> Type n -- ^ Result type of the function
  -> Bool -- ^ Whether the map'd over vector is representable, see 'shareVecs'
  -> Term n -- ^ The map'd function
  -> Term n -- ^ The map'd over vector
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceMap mapPrimInfo n argElTy resElTy argRep fun arg0 ctx = do
  (sc0, te) <- scopeAndTypeEnv ctx
  withVec "reduceMap" te sc0 (inferCoreTypeOf te sc0 arg0) $ \_ nilCon consCon ->
    changed =<< shareVecs te sc0 [(argRep, arg0)] (\sc -> \case
      [arg] -> pure $
        if n == 0 then mkVecNil te sc nilCon consCon (sink resElTy) else
        let
          (a, as) = extractHeadTail te sc consCon (sink argElTy) n arg
          b = mkApps (sink fun) [Left a]
          bs = mkApps (Prim mapPrimInfo)
                 [ Right (sink argElTy)
                 , Right (sink resElTy)
                 , Right (numTy (n - 1))
                 , Left (sink fun)
                 , Left as ]
        in mkVecCons te sc consCon (sink resElTy) n b bs
      _ -> error "reduceMap: shareVecs")

-- | Replace an application of the @Clash.Sized.Vector.imap@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.imap@.
reduceImap
  :: (HasCallStack, Distinct n)
  => Integer  -- ^ Length of the vector
  -> Type n -- ^ Argument type of the function
  -> Type n -- ^ Result type of the function
  -> Term n -- ^ Length of the vector (as a KnownNat)
  -> Term n -- ^ The imap'd function
  -> Term n -- ^ The imap'd over vector
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceImap n argElTy resElTy _kn fun arg ctx = do
  (sc, te) <- scopeAndTypeEnv ctx
  withVec "reduceImap" te sc (inferCoreTypeOf te sc arg) $ \_ nilCon consCon ->
    if n == 0 then changed (mkVecNil te sc nilCon consCon resElTy) else do
      fun1 <- constantPropagation ctx fun
      let
        idxTcNm = fromMaybe (error "reduceImap: failed to create Index TC") $
          case splitFunForallTy (inferCoreTypeOf te sc fun) of
            SplitFunForall (PiArg idxTy _) _ | TyConApp nm _ <- tyView idxTy -> Just nm
            _ -> Nothing
        -- fromInteger# :: KnownNat n => Integer -> Index n
        idxFromIntegerTy =
          forAllTyB 0 (C.mkUnsafeSystemName "n" 0) (tyConAppB typeNatKindTyConName [])
            (funTyB [tyConAppB integerPrimTyConName [], tyConAppB integerPrimTyConName []]
               (tyConAppB idxTcNm [varTyB 0]))
        idxFromInteger =
          mkPrimInfo "Clash.Sized.Internal.Index.fromInteger#" (runClosedType idxFromIntegerTy)
            WorkNever SingleResult NoUnfolding
      changed $ extractElems te sc consCon argElTy 'I' n n arg $ \sc1 bs rhss vars _ ->
        let
          idxs = map (App (App (TyApp (Prim idxFromInteger) (numTy n)) (Literal (IntegerLiteral n)))
                       . Literal . IntegerLiteral) [0 .. n - 1]
          funApps = zipWith (\i v -> App (App (sink fun1) i) v) idxs vars
        in recLet bs rhss (mkVec te sc1 nilCon consCon (sink resElTy) n funApps)

-- | Replace an application of the @Clash.Sized.Vector.iterateI@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.iterateI@.
reduceIterateI
  :: (HasCallStack, Distinct n)
  => Integer
  -- ^ Length of vector
  -> Type n
  -- ^ Vector's element type
  -> Type n
  -- ^ Vector's type
  -> Term n
  -- ^ Length of the vector (as a KnownNat)
  -> Term n
  -- ^ iterateI's HO-function argument
  -> Term n
  -- ^ iterateI's start value
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceIterateI n aTy vTy _kn f0 a ctx = do
  (sc, te) <- scopeAndTypeEnv ctx
  f1 <- constantPropagation ctx f0
  let
    (nilCon, consCon) = fromMaybe (error "reduceIterateI: failed to create Vec DCs") $ do
      TyConApp vecTcNm _ <- pure (tyView vTy)
      vecTc <- lookupTyCon te vecTcNm
      [nil, cons] <- pure (tyConDataCons vecTc)
      pure (nil, cons)
    elNames = [(internalName ("el" <> showt i), aTy) | i <- [1 .. n - 1]]

  -- Result:
  --   let
  --     el1 = f a
  --     el2 = f el1
  --     el3 = f el2
  --     ..
  --   in
  --     (a :> el1 :> el2 :> el3 :> ..)
  --
  changed $ withFreshIdBinders sc elNames $ \sc1 bs vars ->
    let
      elems = map (App (sink f1)) (sink a : vars)
      vec = mkVec te sc1 nilCon consCon (sink aTy) n (take (fromInteger n) (sink a : vars))
    in recLet bs (zipWith const elems vars) vec

-- | Replace an application of the @Clash.Sized.Vector.traverse#@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.traverse#@.
--
-- It let-binds the @Functor@ dictionary, @pure@ and @\<*\>@ fields of the
-- @Applicative@ dictionary, the @fmap@ field of that @Functor@ dictionary, and
-- the elements of the vector, and builds the result with 'mkTravVec'. The
-- case-expressions selecting the fields have pattern binders of their own,
-- apart from the let-binders.
reduceTraverse
  :: forall n
   . (HasCallStack, Distinct n)
  => Integer  -- ^ Length of the vector
  -> Type n -- ^ Element type of the argument vector
  -> Type n -- ^ The type of the applicative
  -> Type n -- ^ Element type of the result vector
  -> Term n -- ^ The @Applicative@ dictionary
  -> Term n -- ^ The function to traverse with
  -> Term n -- ^ The argument vector
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceTraverse n aTy fTy bTy dict fun arg ctx = do
  (sc, te) <- scopeAndTypeEnv ctx
  let dictTy = inferCoreTypeOf te sc dict
  case tyView dictTy of
    TyConApp apDictTcNm _ ->
      withVec "reduceTraverse" te sc (inferCoreTypeOf te sc arg) $ \vecTcNm nilCon consCon -> do
        fun1 <- constantPropagation ctx fun
        maybe (error "reduceTraverse: failed to build") changed $ do
          apDictTc <- lookupTyCon te apDictTcNm
          apDictCon <- listToMaybe (tyConDataCons apDictTc)
          [funcDictTy, pureTy, apTy, _, _, _] <- dataConInstArgTys te sc apDictCon [fTy]

          TyConApp funcDictTcNm _ <- pure (tyView funcDictTy)
          funcDictTc <- lookupTyCon te funcDictTcNm
          funcDictCon <- listToMaybe (tyConDataCons funcDictTc)
          [fmapTy, _] <- dataConInstArgTys te sc funcDictCon [fTy]

          let
            dictBinders =
              [ (internalName "functorDict", funcDictTy)
              , (internalName "pure", pureTy)
              , (internalName "ap", apTy)
              , (internalName "fmap", fmapTy) ]

            -- The let-bindings of the dictionary fields, followed by those of
            -- the elements of the vector
            withDicts
              :: forall l1
               . DExt n l1
              => Scope l1 -> Tele IdBinder n l1 -> [Term l1] -> Term n
            withDicts sc1 dictBs dictVars = case dictVars of
              [functorDictV, pureV, apV, fmapV] ->
                let
                  apSel = selectField te sc1 apDictCon [sink fTy] 0
                    ["functorDict", "pure", "ap", "liftA2", "apConstL", "apConstR"] (sink dict)
                  dictRhss =
                    [ apSel 0
                    , apSel 1
                    , apSel 2
                    , selectField te sc1 funcDictCon [sink fTy] 0 ["fmap", "fmapConst"] functorDictV 0 ]
                in extractAllElems te sc1 consCon (sink aTy) 'T' n (sink arg) $
                     \(sc2 :: Scope l2) elemBs elemRhss vars -> withExtTrans @n @l1 @l2 $
                       let
                         funApps = map (App (sink fun1)) vars
                         lbody = mkTravVec te sc2 vecTcNm nilCon consCon
                                   (sink pureV) (sink apV) (sink fmapV) (sink bTy) n funApps
                       in recLet (appendTele dictBs elemBs) (sink1 dictRhss <> elemRhss) lbody
              _ -> error "reduceTraverse: dictionary binders"

          pure (withFreshIdBinders sc dictBinders withDicts)
    _ -> error ("reduceTraverse: expected a TC, but got: " <> showPpr dictTy)

-- | 'extractElems' of all elements of a vector, which may be empty
extractAllElems
  :: (HasCallStack, Distinct n)
  => TypeEnv
  -> Scope n
  -> DataCon
  -- ^ The @Cons@ constructor
  -> Type n
  -- ^ Element type
  -> Char
  -- ^ Appended to the names of the binders
  -> Integer
  -- ^ Length of the vector
  -> Term n
  -> (forall l. DExt n l => Scope l -> Tele IdBinder n l -> [Term l] -> [Term l] -> r)
  -> r
extractAllElems te sc consCon elTy c n vec k
  | n == 0 = k sc TNil [] []
  | otherwise = extractElems te sc consCon elTy c n n vec (\sc1 bs rhss vars _ -> k sc1 bs rhss vars)

-- | Create the traversable vector
--
-- e.g. for a length '2' input vector, we get
--
-- > (:>) <$> x0 <*> ((:>) <$> x1 <*> pure Nil)
mkTravVec
  :: (HasCallStack, Distinct n)
  => TypeEnv
  -> Scope n
  -> TyConName -- ^ Vec tcon
  -> DataCon   -- ^ Nil con
  -> DataCon   -- ^ Cons con
  -> Term n    -- ^ 'pure' term
  -> Term n    -- ^ '<*>' term
  -> Term n    -- ^ 'fmap' term
  -> Type n    -- ^ 'b' ty
  -> Integer   -- ^ Length of the vector
  -> [Term n]  -- ^ Elements of the vector
  -> Term n
mkTravVec te sc vecTc nilCon consCon pureTm apTm fmapTm bTy = go
 where
  vecTy k = mkTyConApp vecTc [numTy k, bTy]

  go _ [] = mkApps pureTm [Right (vecTy 0), Left (mkVecNil te sc nilCon consCon bTy)]
  go n (x:xs) = mkApps apTm
    [ Right (vecTy (n - 1))
    , Right (vecTy n)
    , Left (mkApps fmapTm
        [ Right bTy
        , Right (mkFunTy (vecTy (n - 1)) (vecTy n))
        , Left (mkApps (Data consCon)
            [ Right (numTy n)
            , Right bTy
            , Right (numTy (n - 1))
            , Left (primCo (consCoTy n)) ])
        , Left x ])
    , Left (go (n - 1) xs) ]

  consCoTy n = case dataConInstArgTys te sc consCon [numTy n, bTy, numTy (n - 1)] of
    Just (x:_) -> x
    _ -> error "mkTravVec: failed to instantiate Cons DC"

-- | Replace an application of the @Clash.Sized.Vector.foldr@ primitive on
-- vectors of a known length @n@, by one step of its recursive "definition":
-- the function applied to the head of the vector and @foldr@ applied to its
-- tail. For an empty vector, it gives the starting value.
reduceFoldr
  :: (HasCallStack, Distinct n)
  => PrimInfo
  -- ^ Primitive info for foldr blackbox
  -> Integer
  -- ^ Length of the vector
  -> Type n
  -- ^ Element type of the argument vector
  -> Bool
  -- ^ Whether the argument vector is representable, see 'shareVecs'
  -> Term n
  -- ^ The function to fold with
  -> Term n
  -- ^ The starting value
  -> Term n
  -- ^ The argument vector
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceFoldr _ 0 _ _ _ start _ _ = changed start
reduceFoldr foldrPrimInfo n aTy argRep fun start arg0 ctx = do
  (sc0, te) <- scopeAndTypeEnv ctx
  withVec "reduceFoldr" te sc0 (inferCoreTypeOf te sc0 arg0) $ \_ _ consCon ->
    changed =<< shareVecs te sc0 [(argRep, arg0)] (\sc -> \case
      [arg] -> pure $
        let
          (a, as) = extractHeadTail te sc consCon (sink aTy) n arg
          b = mkApps (Prim foldrPrimInfo)
                [ Right (sink aTy)
                , Right (inferCoreTypeOf te sc (sink start))
                , Right (numTy (n - 1))
                , Left (sink fun)
                , Left (sink start)
                , Left as ]
        in mkApps (sink fun) [Left a, Left b]
      _ -> error "reduceFoldr: shareVecs")

-- | Replace an application of the @Clash.Sized.Vector.fold@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.fold@: a balanced tree of applications of the
-- function.
reduceFold
  :: (HasCallStack, Distinct n)
  => Integer
  -- ^ Length of the vector, must be positive
  -> Type n
  -- ^ Element type of the argument vector
  -> Term n
  -- ^ The function to fold with
  -> Term n
  -- ^ The argument vector
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceFold n aTy fun arg ctx = do
  (sc, te) <- scopeAndTypeEnv ctx
  withVec "reduceFold" te sc (inferCoreTypeOf te sc arg) $ \_ _ consCon -> do
    fun1 <- constantPropagation ctx fun
    changed $ extractElems te sc consCon aTy 'F' n n arg $ \_ bs rhss vars _ ->
      recLet bs rhss (foldV (sink fun1) vars)
 where
  foldV _ [a] = a
  foldV f as  = let (l,r) = splitAt (length as `div` 2) as
                    lF    = foldV f l
                    rF    = foldV f r
                in  mkApps f [Left lF, Left rF]

-- | Replace an application of the @Clash.Sized.Vector.dfold@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.dfold@. For an empty vector, it gives the starting
-- value.
--
-- The upper bound proof it passes to the function for an element is the data
-- constructor of the type of the proof argument, with the type variable that
-- the type of the function quantifies over instantiated to the index of the
-- element.
reduceDFold
  :: (HasCallStack, Distinct n)
  => Integer
  -- ^ Length of the vector
  -> Type n
  -- ^ Element type of the argument vector
  -> Term n
  -- ^ Length of the vector (as a KnownNat)
  -> Term n
  -- ^ The motive
  -> Term n
  -- ^ Function to fold with
  -> Term n
  -- ^ Starting value
  -> Term n
  -- ^ The vector to fold
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceDFold 0 _   _   _       _   start _   _ = changed start
reduceDFold n aTy _kn _motive fun start arg ctx = do
  (sc, te) <- scopeAndTypeEnv ctx
  withVec "reduceDFold" te sc (inferCoreTypeOf te sc arg) $ \_ _ consCon -> do
    let
      funTy = splitFunForallTy (inferCoreTypeOf te sc fun)
      snatDc = fromMaybe (error "reduceDFold: faild to build SNat") $ case funTy of
        SplitFunForall (PiForall _ (PiArg _ (PiArg snTy _))) _ -> firstDataConOf te snTy
        _ -> Nothing
      ubp k = fromMaybe (error "reduceDFold: failed to extract upper bound proof") $ case funTy of
        SplitFunForall (PiForall lB (PiArg ubpT _)) _ ->
          let witness = normalizeType te sc (substTy (extendTySubst (idSubst sc) lB (numTy k)) ubpT)
          in firstDataConOf te witness
        _ -> Nothing
    -- TODO: Should 'constantPropagation' be used on 'fun'? It seems to
    -- TOOD: be used for every other function in this module.
    changed $ extractElems te sc consCon aTy 'D' n n arg $ \_ bs rhss vars _ ->
      let
        doFold _ [] = sink start
        doFold k (x:xs) = mkApps (sink fun)
          [ Right (numTy k)
          , Left (Data (ubp k))
          , Left (buildSNat snatDc k)
          , Left x
          , Left (doFold (k - 1) xs) ]
      in recLet bs rhss (doFold (n - 1) vars)

-- | Replace an application of the @Clash.Sized.Vector.head@ primitive on
-- vectors of a known length @n@, by a projection of the first element of a
-- vector.
reduceHead
  :: (HasCallStack, Distinct n)
  => Integer  -- ^ Length of the vector, must be positive
  -> Type n -- ^ Element type of the vector
  -> Term n -- ^ The argument vector
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceHead n aTy vArg ctx = do
  (sc, te) <- scopeAndTypeEnv ctx
  withVec "reduceHead" te sc (inferCoreTypeOf te sc vArg) $ \_ _ consCon ->
    changed $ extractElems te sc consCon aTy 'H' n 1 vArg $ \_ bs rhss vars _ ->
      case vars of
        [v] -> recLet bs rhss v
        _ -> error "reduceHead: extractElems"

-- | Replace an application of the @Clash.Sized.Vector.tail@ primitive on
-- vectors of a known length @n@, by a projection of the tail of a vector.
reduceTail
  :: (HasCallStack, Distinct n)
  => Integer  -- ^ Length of the vector, must be positive
  -> Type n -- ^ Element type of the vector
  -> Term n -- ^ The argument vector
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceTail n aTy vArg ctx = do
  (sc, te) <- scopeAndTypeEnv ctx
  withVec "reduceTail" te sc (inferCoreTypeOf te sc vArg) $ \_ _ consCon -> do
    let
      tys = [numTy n, aTy, numTy (n - 1)]
      restTy = case dataConInstArgTys te sc consCon tys of
        Just [_, _, ty] -> ty
        _ -> error "reduceTail: failed to instantiate Cons DC"
      restRhs = selectField te sc consCon tys 1 ["_co_", "el", "rest"] vArg 2
    changed $ withFreshIdBinder sc (systemName "restL0") restTy $ \_ b ->
      recLet (TCons b TNil) [sink restRhs] (idBinderVar b)

-- | Replace an application of the @Clash.Sized.Vector.last@ primitive on
-- vectors of a known length @n@, by a projection of the last element of a
-- vector. For an empty vector, it gives @undefined@.
reduceLast
  :: (HasCallStack, Distinct n)
  => Integer  -- ^ Length of the vector, must be positive
  -> Type n -- ^ Element type of the vector
  -> Term n -- ^ The argument vector
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceLast n aTy vArg ctx = do
  (sc, te) <- scopeAndTypeEnv ctx
  withVec "reduceLast" te sc (inferCoreTypeOf te sc vArg) $ \_ _ consCon ->
    changed $
      if n == 0 then TyApp (Prim NP.undefined) aTy else
      extractElems te sc consCon aTy 'L' n n vArg $ \_ bs rhss vars _ ->
        recLet bs rhss (last vars)

-- | Replace an application of the @Clash.Sized.Vector.init@ primitive on
-- vectors of a known length @n@, by one step of its recursive "definition":
-- the head of the vector, consed onto @init@ applied to its tail.
reduceInit
  :: (HasCallStack, Distinct n)
  => PrimInfo -- ^ Primitive info for 'init'
  -> Integer  -- ^ Length of the result vector
  -> Type n -- ^ Element type of the vector
  -> Bool -- ^ Whether the argument vector is representable, see 'shareVecs'
  -> Term n -- ^ The argument vector
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceInit initPrimInfo n aTy vArgRep vArg0 ctx = do
  (sc0, te) <- scopeAndTypeEnv ctx
  withVec "reduceInit" te sc0 (inferCoreTypeOf te sc0 vArg0) $ \_ nilCon consCon ->
    changed =<< shareVecs te sc0 [(vArgRep, vArg0)] (\sc -> \case
      [vArg] -> pure $
        if n == 0 then mkVecNil te sc nilCon consCon (sink aTy) else
        let
          (a, as0) = extractHeadTail te sc consCon (sink aTy) (n + 1) vArg
          as1 = mkApps (Prim initPrimInfo) [Right (numTy (n - 1)), Right (sink aTy), Left as0]
        in mkVecCons te sc consCon (sink aTy) n a as1
      _ -> error "reduceInit: shareVecs")

-- | Replace an application of the @Clash.Sized.Vector.(++)@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.(++)@: the elements of the LHS vector, consed onto
-- the RHS vector. If either vector is empty, it gives the other one.
reduceAppend
  :: (HasCallStack, Distinct n)
  => Integer  -- ^ Length of the LHS arg
  -> Integer  -- ^ Lenght of the RHS arg
  -> Type n -- ^ Element type of the vectors
  -> Term n -- ^ The LHS argument
  -> Term n -- ^ The RHS argument
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceAppend 0 _ _   _    rArg _ = changed rArg
reduceAppend _ 0 _   lArg _    _ = changed lArg
reduceAppend n m aTy lArg rArg ctx = do
  (sc, te) <- scopeAndTypeEnv ctx
  withVec "reduceAppend" te sc (inferCoreTypeOf te sc lArg) $ \_ _ consCon ->
    changed $ extractElems te sc consCon aTy 'C' n n lArg $ \sc1 bs rhss vars _ ->
      recLet bs rhss (appendToVec te sc1 consCon (sink aTy) (sink rArg) (n + m) vars)

-- | Replace an application of the @Clash.Sized.Vector.unconcat@ primitive on
-- vectors of a known length @n@, by one step of its recursive "definition":
-- a vector of the first @m@ elements, consed onto @unconcat@ applied to the
-- vector of the remaining elements.
reduceUnconcat
  :: (HasCallStack, Distinct n)
  => PrimInfo -- ^ Unconcat primitive info
  -> Integer  -- ^ Length of the result vector
  -> Integer  -- ^ Length of the elements of the result vector
  -> Type n -- ^ Element type
  -> Term n -- ^ Length of the result vector (as a KnownNat)
  -> Term n -- ^ SNat "Length of the elements of the result vector"
  -> Term n -- ^ Argument vector
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceUnconcat unconcatPrimInfo n m aTy _kn sm arg ctx = do
  (sc, te) <- scopeAndTypeEnv ctx
  withVec "reduceUnconcat" te sc (inferCoreTypeOf te sc arg) $ \vecTcNm nilCon consCon -> do
    let innerVecTy = mkTyConApp vecTcNm [numTy m, aTy]
    changed $
      if n == 0 then mkVecNil te sc nilCon consCon innerVecTy
      else if m == 0 then
        let nilVec = mkVecNil te sc nilCon consCon aTy
        in mkVec te sc nilCon consCon innerVecTy n (replicate (fromInteger n) nilVec)
      else
        -- let (mvec,nextVec) = splitAt sm arg
        -- in Cons mvec (unconcat sm nextVec)
        extractElems te sc consCon aTy 'U' (n * m) m arg $ \sc1 bs rhss vars nextVec ->
          let
            -- A vector of the first m elements
            mvec = mkVec te sc1 nilCon consCon (sink aTy) m vars
            -- Recursively unconcat the vector of the remaining (n-1)*m
            -- elements
            nextUnconcat = mkApps (Prim unconcatPrimInfo)
                             [ Right (numTy (n - 1))
                             , Right (numTy m)
                             , Right (sink aTy)
                             , Left (Literal (NaturalLiteral (n - 1)))
                             , Left (sink sm)
                             , Left nextVec ]
          in recLet bs rhss (mkVecCons te sc1 consCon (sink innerVecTy) n mvec nextUnconcat)

-- | Replace an application of the @Clash.Sized.Vector.transpose@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.transpose@. Only handles elements of length 0.
reduceTranspose
  :: (HasCallStack, Distinct n)
  => Integer  -- ^ Length of the result vector
  -> Integer  -- ^ Length of the elements of the result vector
  -> Type n -- ^ Element type
  -> Term n -- ^ Lenght of the result vector (as a KnownNat)
  -> Term n -- ^ Argument vector
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceTranspose n 0 aTy _kn arg ctx = do
  (sc, te) <- scopeAndTypeEnv ctx
  withVec "reduceTranspose" te sc (inferCoreTypeOf te sc arg) $ \vecTcNm nilCon consCon ->
    let nilVec     = mkVecNil te sc nilCon consCon aTy
        innerVecTy = mkTyConApp vecTcNm [numTy 0, aTy]
    in changed (mkVec te sc nilCon consCon innerVecTy n (replicate (fromInteger n) nilVec))
reduceTranspose _ _ _ _ _ _ = error "reduceTranspose: unimplemented"

-- | Replace an application of the @Clash.Sized.Vector.replicate@ primitive on
-- vectors of a known length @n@, by a vector of @n@ copies of the element.
reduceReplicate
  :: (HasCallStack, Distinct n)
  => Integer -- ^ Length of the vector
  -> Type n -- ^ Element type
  -> Type n -- ^ Type of the vector
  -> Term n -- ^ Length of the vector (as an SNat)
  -> Term n -- ^ Element
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceReplicate n aTy eTy _sn arg ctx = do
  (sc, te) <- scopeAndTypeEnv ctx
  withVec "reduceReplicate" te sc eTy $ \_ nilCon consCon ->
    changed (mkVec te sc nilCon consCon aTy n (replicate (fromInteger n) arg))

-- TODO: Take a shortcut when given index is a literal. Right now, this function
-- TODO: simply creates a case statement for every element in the vector, which
-- TODO: Clash will eliminate one-by-one if the index turned out to be literal.
-- TODO: It would of course be best to not create the cases in the first place!

-- | Replace an application of the @Clash.Sized.Vector.replace_int@ primitive
-- on vectors of a known length @n@, by a vector of case-expressions that
-- select the new or the old element: for the element @oldA@ at index @curI@,
--
-- > case eqInt i curI of
-- >   True -> newA
-- >   _    -> oldA
--
-- where @i@ is the index and @newA@ the element given to @replace_int@.
--
-- TODO: Take a shortcut when the given index is a literal. Right now, this
-- function simply creates a case-expression for every element in the vector,
-- which Clash will eliminate one-by-one if the index turned out to be
-- literal. It would of course be best to not create the cases in the first
-- place!
reduceReplace_int
  :: (HasCallStack, Distinct n)
  => Integer
  -- ^ Size of vector
  -> Type n
  -- ^ Type of vector element
  -> Type n
  -- ^ Type of vector
  -> Term n
  -- ^ Size of vector (as a KnownNat)
  -> Term n
  -- ^ Vector
  -> Term n
  -- ^ Index
  -> Term n
  -- ^ Element
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceReplace_int n aTy vTy _kn v i newA ctx = do
  (sc, te) <- scopeAndTypeEnv ctx
  withVec "reduceReplace_int" te sc vTy $ \_ nilCon consCon ->
    changed $
      if n == 0 then mkVecNil te sc nilCon consCon aTy else
      extractElems te sc consCon aTy 'I' n n v $ \sc1 bs rhss vars _ ->
        let
          iTy = inferCoreTypeOf te sc i
          trueDc = fst (boolTrue te)
          -- case eqInt i curI of
          --   True -> newA
          --   _    -> oldA
          replaceElement oldA elIndex =
            Case (eqIntWith te (sink iTy) (sink i) elIndex) (sink aTy)
              [ Alt DefaultPat oldA
              , Alt (DataPat trueDc TNil TNil) (sink newA) ]
          replacedEls = zipWith replaceElement vars [0..]
        in recLet bs rhss (mkVec te sc1 nilCon consCon (sink aTy) n replacedEls)

-- TODO: Take a shortcut when given index is a literal. Right now, this function
-- TODO: simply creates a case statement for every element in the vector, which
-- TODO: Clash will eliminate one-by-one if the index turned out to be literal.
-- TODO: It would of course be best to not create the cases in the first place!

-- | Replace an application of the @Clash.Sized.Vector.index_int@ primitive on
-- vectors of a known length @n@, by a right-biased tree of case-expressions
-- that select an element: for the element @curA@ at index @curI@,
--
-- > case eqInt i curI of
-- >   True -> curA
-- >   _    -> next
--
-- where @i@ is the index given to @index_int@, and @next@ the value if the
-- current index is not equal to it. Every index is out of bounds of an empty
-- vector, which gives an undefined value.
--
-- TODO: Take a shortcut when the given index is a literal. Right now, this
-- function simply creates a case-expression for every element in the vector,
-- which Clash will eliminate one-by-one if the index turned out to be
-- literal. It would of course be best to not create the cases in the first
-- place!
reduceIndex_int
  :: (HasCallStack, Distinct n)
  => Integer
  -- ^ Size of vector
  -> Type n
  -- ^ Type of vector element
  -> Term n
  -- ^ Size of vector (as a KnownNat)
  -> Term n
  -- ^ Vector
  -> Term n
  -- ^ Index
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceIndex_int n aTy _kn v i ctx = do
  (sc, te) <- scopeAndTypeEnv ctx
  withVec "reduceIndex_int" te sc (inferCoreTypeOf te sc v) $ \_ _ consCon ->
    changed $
      -- Every index is out of bounds
      if n == 0 then TyApp (Prim NP.undefined) aTy else
      extractElems te sc consCon aTy 'I' n n v $ \_ bs rhss vars _ ->
        let
          iTy = inferCoreTypeOf te sc i
          trueDc = fst (boolTrue te)
          -- case eqInt i curI of
          --   True -> curA
          --   _    -> next
          indexElement (cur, elIndex) next =
            Case (eqIntWith te (sink iTy) (sink i) elIndex) (sink aTy)
              [ Alt DefaultPat next
              , Alt (DataPat trueDc TNil TNil) cur ]
          -- A right-biased tree of case-expressions
          indexed = foldr indexElement (TyApp (Prim NP.undefined) (sink aTy)) (zip vars [0..])
        in recLet bs rhss indexed

-- | Replace an application of the @Clash.Sized.Vector.dtfold@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.dtfold@.
reduceDTFold
  :: (HasCallStack, Distinct n)
  => Integer  -- ^ Length of the vector
  -> Type n   -- ^ Element type of the argument vector
  -> Term n   -- ^ Length of the vector (as a KnownNat)
  -> Term n   -- ^ The motive
  -> Term n   -- ^ Function to convert elements with
  -> Term n   -- ^ Function to combine branches with
  -> Term n   -- ^ The vector to fold
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceDTFold n aTy _kn _motive lrFun brFun arg ctx = do
  (sc, te) <- scopeAndTypeEnv ctx
  withVec "reduceDTFold" te sc (inferCoreTypeOf te sc arg) $ \_ _ consCon -> do
    let snatDc = snatDataConOfBranch "reduceDTFold" te (inferCoreTypeOf te sc brFun)
    changed $ extractElems te sc consCon aTy 'T' (2 ^ n) (2 ^ n) arg $ \_ bs rhss vars _ ->
      let
        doFold _ [x] = mkApps (sink lrFun) [Left x]
        doFold k xs  =
          let (xsL,xsR) = splitAt (2 ^ k) xs
              k'        = k - 1
              eL        = doFold k' xsL
              eR        = doFold k' xsR
          in  mkApps (sink brFun)
                [ Right (numTy k)
                , Left (buildSNat snatDc k)
                , Left eL
                , Left eR ]
      in recLet bs rhss (doFold (n - 1) vars)

-- | The @SNat@ data constructor, given the type of a function @forall l. SNat
-- l -> ...@, which combines the branches of a @dtfold@ or @tdfold@
snatDataConOfBranch :: HasCallStack => String -> TypeEnv -> Type n -> DataCon
snatDataConOfBranch nm te brFunTy =
  fromMaybe (error (nm <> ": faild to build SNat")) $ case splitFunForallTy brFunTy of
    SplitFunForall (PiForall _ (PiArg snTy _)) _ -> firstDataConOf te snTy
    _ -> Nothing

-- | Replace an application of the @Clash.Sized.RTree.tdfold@ primitive on
-- trees of a known depth @n@, by the fully unrolled recursive "definition" of
-- @Clash.Sized.RTree.tdfold@.
reduceTFold
  :: (HasCallStack, Distinct n)
  => Integer -- ^ Depth of the tree
  -> Type n  -- ^ Element type of the argument tree
  -> Term n  -- ^ Depth of the tree (as a KnownNat)
  -> Term n  -- ^ The motive
  -> Term n  -- ^ Function to convert elements with
  -> Term n  -- ^ Function to combine branches with
  -> Term n  -- ^ The tree to fold
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceTFold n aTy _kn _motive lrFun brFun arg ctx = do
  (sc, te) <- scopeAndTypeEnv ctx
  let argTy = coreView te sc (inferCoreTypeOf te sc arg)
  withTyConDataCons "reduceTFold" rtreeTcName te argTy $ \_ lrCon brCon -> do
    let snatDc = snatDataConOfBranch "reduceTFold" te (inferCoreTypeOf te sc brFun)
    changed $ extractTElems te sc lrCon brCon aTy 'T' n arg $ \_ bs rhss vars ->
      let
        doFold _ [x] = mkApps (sink lrFun) [Left x]
        doFold k xs  =
          let (xsL,xsR) = splitAt (length xs `div` 2) xs
              k'        = k - 1
              eL        = doFold k' xsL
              eR        = doFold k' xsR
          in  mkApps (sink brFun)
                [ Right (numTy k)
                , Left (buildSNat snatDc k)
                , Left eL
                , Left eR ]
      in recLet bs rhss (doFold (n - 1) vars)

-- | Replace an application of the @Clash.Sized.RTree.treplicate@ primitive on
-- trees of a known depth @n@, by a tree of @2^n@ copies of the element.
reduceTReplicate
  :: (HasCallStack, Distinct n)
  => Integer -- ^ Depth of the tree
  -> Type n  -- ^ Element type
  -> Type n  -- ^ Result type
  -> Term n  -- ^ Depth of the tree (as an SNat)
  -> Term n  -- ^ Element
  -> TransformContext n
  -> NormalizeSession (Term n)
reduceTReplicate n aTy eTy _sn arg ctx = do
  (sc, te) <- scopeAndTypeEnv ctx
  withTyConDataCons "reduceTReplicate" rtreeTcName te (coreView te sc eTy) $ \_ lrCon brCon ->
    changed (mkRTree te sc lrCon brCon aTy n (replicate (2 ^ n) arg))
