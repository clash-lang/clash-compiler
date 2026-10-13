{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Smart constructors and destructors for scoped Clash Core.

  = Let-bindings

  Let-bindings can't be passed around as lists in scoped Core: the right-hand
  side of a binding may only mention the binders in scope at it. The functions
  here take the bindings of a recursive let-expression
  (@'Let' ('Rec' bs xs) body@) and regroup or reorder them, giving a telescope
  of let-bindings ('Tele' t'Bind'). They reuse the binders when the order of
  the bindings doesn't change, and give them fresh names otherwise.

  = Fresh binders

  Functions that build terms with new binders, such as 'extractElems' and
  'mkSelectorCase', take the scope, and give the binders fresh names in it.
-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Scoped.Util
  ( -- * Let-bindings
    mkLets
  , listToLets
  , sccLetBindings
  , inverseTopSortLetBindings
  , inverseTopSortOrder
  , inverseTopSortOrderOf
  , LetGroup (..)
  , regroupLetBindings
    -- * Vectors and trees
  , mkVec
  , appendToVec
  , extractElems
  , mkRTree
  , extractTElems
  , selectField
    -- * Names
  , systemName
  , internalName
    -- * Data constructors
  , mkSelectorCase
  , substArgTys
    -- * Primitives
  , primCo
  , primUCo
  , primAtType
  , undefinedPrims
  , undefinedXPrims
    -- * Types
  , SplitInfo (..)
  , shouldSplit
  , splitShouldSplit
  ) where

import Control.Monad (join)
import Control.Monad.Trans.Except (runExcept)
import qualified Data.Graph as Graph
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)

import Clash.Core.Basic
  (IsMultiPrim (..), LitTy (..), WorkInfo (..), undefinedPrims, undefinedXPrims)
import Clash.Core.DataCon (DataCon)
import qualified Clash.Core.Name as C
import Clash.Core.Pretty (showPpr)
import qualified Clash.Core.TyCon as C
import Clash.Util (curLoc, indexNote)

import Clash.Core.DataCon (dataConInstArgTys, dataConInstExtTyVars)
import Clash.Core.Scoped.EqSolver (dataConInstArgTysE)
import Clash.Core.Scoped.FreeVars (freeTmVars, teleNameBinders)
import Clash.Core.Scoped.HasType (inferCoreTypeOf)
import Clash.Core.Scoped.NameMap
  ( NameBinders, appendNameBinders, emptyNameBinders, idBinderNameBinders
  , nameSetToList )
import Clash.Core.Scoped.Pretty ()
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (appendTele, mkApps, splitTele)
import Clash.Core.Scoped.Type
import Clash.Core.Scoped.TypeEnv (TypeEnv, findTyCon, lookupTyCon)
import Clash.Core.Scoped.TysPrim (typeNatKind, undefinedTy, unsafeCoerceTy)

-- * Let-bindings

-- | Wrap a term in let-expressions, the first one outermost
mkLets :: Tele Bind n l -> Term l -> Term n
mkLets TNil e = e
mkLets (TCons b bs) e = Let b (mkLets bs e)

-- | Split recursive let-bindings into strongly connected components, and
-- wrap the body in a let-expression for each
listToLets
  :: (HasCallStack, Distinct n)
  => Scope n
  -> Tele IdBinder n l
  -> [Term l]
  -- ^ Right-hand sides
  -> Term l
  -- ^ Body
  -> Term n
listToLets sc bs xs body = sccLetBindings sc bs xs body (\_ binds body' -> mkLets binds body')

-- | Split recursive let-bindings into strongly connected components: a
-- non-recursive binding for each binding that doesn't refer to itself or to
-- bindings that refer to it, and a recursive group for each cycle.
--
-- The components are in dependency order, and components that don't depend on
-- each other are in the order of the bindings, so that bindings that are
-- already in dependency order keep their order, and their binders.
sccLetBindings
  :: (HasCallStack, Distinct n)
  => Scope n
  -> Tele IdBinder n l
  -> [Term l]
  -- ^ Right-hand sides
  -> Term l
  -- ^ Body
  -> (forall o. DExt n o => Scope o -> Tele Bind n o -> Term o -> r)
  -> r
sccLetBindings sc bs xs body =
  regroupLetBindings sc bs xs body (stableSccs (letDependencies bs xs))

-- | Reorder recursive let-bindings such that bindings come after the ones
-- they refer to, unless they are part of a cycle: in the post-order of a
-- depth-first search. Bindings that are
-- already in that order keep their binders.
inverseTopSortLetBindings
  :: (HasCallStack, Distinct n)
  => Scope n
  -> Tele IdBinder n l
  -> [Term l]
  -- ^ Right-hand sides
  -> Term l
  -- ^ Body
  -> (forall o. DExt n o => Scope o -> Tele IdBinder n o -> [Term o] -> Term o -> r)
  -> r
inverseTopSortLetBindings sc bs xs body cont =
  regroupLetBindings sc bs xs body [RecGroup (inverseTopSortOrder bs xs)] $ \sc' binds body' ->
    case binds of
      TCons (Rec bs' xs') TNil -> cont sc' bs' xs' body'
      _ -> error "inverseTopSortLetBindings: expected a single recursive group"

-- | The order of 'inverseTopSortLetBindings', as the positions of the
-- bindings, without reordering them: e.g. to visit the bindings of an opened
-- let-expression in that order.
inverseTopSortOrder
  :: Tele IdBinder n l
  -> [Term l]
  -- ^ Right-hand sides
  -> [Int]
inverseTopSortOrder bs = inverseTopSortOrderRaw (teleRaws bs)

-- | The order of 'inverseTopSortLetBindings', as the positions of the
-- bindings, for let-bindings given by the names they bind and their
-- right-hand sides: e.g. those of several let-expressions opened into one
-- scope.
inverseTopSortOrderOf
  :: [TmName l]
  -- ^ The names bound by the bindings
  -> [Term l]
  -- ^ Right-hand sides
  -> [Int]
inverseTopSortOrderOf nms = inverseTopSortOrderRaw (map tmNameRaw nms)

-- | 'inverseTopSortOrder' of bindings given by the raw names they bind
inverseTopSortOrderRaw :: [Int] -> [Term l] -> [Int]
inverseTopSortOrderRaw raws xs = postorderF (Graph.dff graph) []
 where
  deps = letDependenciesRaw raws xs
  graph = Graph.buildG (0, length deps - 1) [(i, d) | (i, ds) <- zip [0..] deps, d <- ds]

  postorderF ts acc = foldr postorder acc ts
  postorder (Graph.Node a ts) acc = postorderF ts (a : acc)

-- | A group of let-bindings, by their positions in the bindings
data LetGroup
  = NonRecGroup !Int
  -- ^ A non-recursive binding
  | RecGroup [Int]
  -- ^ A recursive group, in this order
  deriving (Eq, Show)

-- | The shape of a group of let-bindings
data GroupShape
  = NonRecShape
  | RecShape !Int

-- | Regroup and reorder recursive let-bindings. The groups must contain
-- every binding once, and a binding may only refer to the bindings of its own
-- group and the groups before it.
--
-- Reuses the binders when the order of the bindings doesn't change, and gives
-- them fresh names otherwise.
regroupLetBindings
  :: forall n l r
   . (HasCallStack, Distinct n)
  => Scope n
  -> Tele IdBinder n l
  -> [Term l]
  -- ^ Right-hand sides
  -> Term l
  -- ^ Body
  -> [LetGroup]
  -> (forall o. DExt n o => Scope o -> Tele Bind n o -> Term o -> r)
  -> r
regroupLetBindings sc bs xs body groups cont
  | List.sort perm /= [0 .. k - 1] =
      error ("regroupLetBindings: not a grouping of " <> show k <> " bindings: " <> show groups)
  | perm == [0 .. k - 1] =
      -- Same order: reuse the binders
      openBind sc (Rec bs xs) body $ \sc1 bind1 body1 -> case bind1 of
        Rec bs1 xs1 -> cont sc1 (buildLetGroups bs1 shapes xs1) body1
        NonRec {} -> error "regroupLetBindings: impossible"
  | otherwise =
      -- Another order: new binders, in the new order
      withFreshIdBinders sc [(occs IntMap.! i, tys IntMap.! i) | i <- perm] $ \sco bso vars ->
        let s = substBinders (extendSubstScope sco (idSubst sc)) bs 0 (IntMap.fromList (zip perm vars))
            xsMap = IntMap.fromList (zip [0..] xs)
            xso = [substTm s (xsMap IntMap.! i) | i <- perm]
        in cont sco (buildLetGroups bso shapes xso) (substTm s body)
 where
  k = length xs
  perm = concatMap groupIdxs groups
  shapes = map groupShape groups
  occs = IntMap.fromList (zip [0..] (teleOccs bs))
  tys = IntMap.fromList (zip [0..] (teleIdBinderTypes bs))

  groupIdxs = \case
    NonRecGroup i -> [i]
    RecGroup is -> is

  groupShape = \case
    NonRecGroup _ -> NonRecShape
    RecGroup is -> RecShape (length is)

  -- Substitute the new variables for the old binders, by position
  substBinders :: Subst i o -> Tele IdBinder i l' -> Int -> IntMap (Term o) -> Subst l' o
  substBinders s TNil _ _ = s
  substBinders s (TCons b rest) i vars =
    substBinders (extendTmSubst s b (vars IntMap.! i)) rest (i + 1) vars

-- | Build groups of let-bindings, given binders and right-hand sides in the
-- order of the groups. Moves each right-hand side out of the scope of the
-- groups after it.
buildLetGroups :: HasCallStack => Tele IdBinder n l -> [GroupShape] -> [Term l] -> Tele Bind n l
buildLetGroups bs0 shapes0 xs0 = fst (go bs0 shapes0 xs0)
 where
  -- Also gives the names bound by the binders
  go :: Tele IdBinder x l -> [GroupShape] -> [Term l] -> (Tele Bind x l, NameBinders x l)
  go TNil [] [] = (TNil, emptyNameBinders)
  go (TCons b rest) (NonRecShape : shapes) (x : xs) =
    let (bindsRest, nbsRest) = go rest shapes xs
        nbs = appendNameBinders (idBinderNameBinders b) nbsRest
    in (TCons (NonRec b (unsink nbs x)) bindsRest, nbs)
  go bs (RecShape j : shapes) xs = splitTele j bs $ \grp rest ->
    let (grpXs, restXs) = splitAt j xs
        (bindsRest, nbsRest) = go rest shapes restXs
    in ( TCons (Rec grp (map (unsink nbsRest) grpXs)) bindsRest
       , appendNameBinders (teleNameBinders grp) nbsRest )
  go _ _ _ = error "buildLetGroups: the groups don't match the bindings"

  unsink :: NameBinders x l -> Term l -> Term x
  unsink nbs x = case unsinkTerm nbs x of
    Just x' -> x'
    Nothing -> error "buildLetGroups: a binding refers to a binding of a later group"

-- | The human readable names of the binders of a telescope
teleOccs :: Tele IdBinder n l -> [C.Name Term]
teleOccs TNil = []
teleOccs (TCons b bs) = idBinderOcc b : teleOccs bs

-- | For every right-hand side, the positions of the bindings it refers to
letDependencies :: Tele IdBinder n l -> [Term l] -> [[Int]]
letDependencies bs = letDependenciesRaw (teleRaws bs)

-- | 'letDependencies' of bindings given by the raw names they bind
letDependenciesRaw :: [Int] -> [Term l] -> [[Int]]
letDependenciesRaw raws = map deps
 where
  positions = IntMap.fromList (zip raws [0..])

  deps x =
    [ i
    | v <- nameSetToList (freeTmVars x)
    , Just i <- [IntMap.lookup (tmNameRaw v) positions] ]

-- | The raw names bound by a telescope
teleRaws :: Tele IdBinder n l -> [Int]
teleRaws = \case
  TNil -> []
  TCons b rest -> tmNameRaw (tmNameOf b) : teleRaws rest

-- | The strongly connected components of the bindings, given their
-- dependencies, in dependency order. Components that don't depend on each
-- other are in the order of their first binding.
stableSccs :: [[Int]] -> [LetGroup]
stableSccs deps = go initial readyInit
 where
  sccs = Graph.stronglyConnComp [(i, i, ds) | (i, ds) <- zip [0 :: Int ..] deps]

  -- Components, numbered
  comps :: IntMap LetGroup
  comps = IntMap.fromList (zip [0..] (map toGroup sccs))

  toGroup = \case
    Graph.AcyclicSCC i -> NonRecGroup i
    Graph.CyclicSCC is -> RecGroup (List.sort is)

  members = \case
    NonRecGroup i -> [i]
    RecGroup is -> is

  compOf :: IntMap Int
  compOf = IntMap.fromList [(i, c) | (c, g) <- IntMap.toList comps, i <- members g]

  depsArr :: IntMap [Int]
  depsArr = IntMap.fromList (zip [0..] deps)

  -- The other components a component depends on
  compDeps :: IntMap [Int]
  compDeps = IntMap.mapWithKey
    (\c g -> IntSet.toList (IntSet.fromList
       [ d | i <- members g, j <- depsArr IntMap.! i, let d = compOf IntMap.! j, d /= c ]))
    comps

  dependents :: IntMap [Int]
  dependents = IntMap.fromListWith (++) [(d, [c]) | (c, ds) <- IntMap.toList compDeps, d <- ds]

  key c = (minimum (members (comps IntMap.! c)), c)

  initial = IntMap.map length compDeps
  readyInit = Set.fromList [key c | (c, 0) <- IntMap.toList initial]

  go counts ready = case Set.minView ready of
    Nothing -> []
    Just ((_, c), ready1) ->
      let (counts1, ready2) =
            List.foldl' release (counts, ready1) (IntMap.findWithDefault [] c dependents)
      in comps IntMap.! c : go counts1 ready2

  release (counts, ready) d =
    let n = counts IntMap.! d - 1
        counts' = IntMap.insert d n counts
    in if n == 0 then (counts', Set.insert (key d) ready) else (counts', ready)

-- * Vectors and trees

-- | A natural number as a type
numTy :: Integer -> Type n
numTy = LitTy . NumTy

-- | The first field type of a data constructor instantiated at the given
-- types: the type of its coercion, for the constructors of @Vec@ and @RTree@
coercionTy :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> DataCon -> [Type n] -> Type n
coercionTy tenv sc dc tys = case dataConInstArgTys tenv sc dc tys of
  Just (ty:_) -> ty
  _ -> error ("coercionTy: can't instantiate " <> show dc)

-- | Create a vector of the given elements
mkVec
  :: (HasCallStack, Distinct n)
  => TypeEnv
  -> Scope n
  -> DataCon
  -- ^ The @Nil@ constructor
  -> DataCon
  -- ^ The @Cons@ constructor
  -> Type n
  -- ^ Element type
  -> Integer
  -- ^ Length of the vector
  -> [Term n]
  -- ^ Elements
  -> Term n
mkVec tenv sc nilCon consCon resTy n0 xs0 = appendVec tenv sc consCon resTy nil n0 xs0
 where
  nil = mkApps (Data nilCon)
    [ Right (numTy 0)
    , Right resTy
    , Left (primCo (coercionTy tenv sc nilCon [numTy 0, resTy])) ]

-- | Prepend the given elements to a vector
appendToVec
  :: (HasCallStack, Distinct n)
  => TypeEnv
  -> Scope n
  -> DataCon
  -- ^ The @Cons@ constructor
  -> Type n
  -- ^ Element type
  -> Term n
  -- ^ The vector
  -> Integer
  -- ^ Length of the result
  -> [Term n]
  -- ^ Elements
  -> Term n
appendToVec = appendVec

appendVec
  :: (HasCallStack, Distinct n)
  => TypeEnv -> Scope n -> DataCon -> Type n -> Term n -> Integer -> [Term n] -> Term n
appendVec tenv sc consCon resTy vec = go
 where
  go _ [] = vec
  go n (x:xs) = mkApps (Data consCon)
    [ Right (numTy n)
    , Right resTy
    , Right (numTy (n - 1))
    , Left (primCo (coercionTy tenv sc consCon [numTy n, resTy, numTy (n - 1)]))
    , Left x
    , Left (go (n - 1) xs) ]

-- | A case-expression that selects a field of the given constructor of
-- @Vec@ or @RTree@, instantiated at the given types. Its existentially
-- quantified type variable, if any, is bound with the given kind, but the
-- field types are those of the instantiation.
selectField
  :: (HasCallStack, Distinct n)
  => TypeEnv
  -> Scope n
  -> DataCon
  -> [Type n]
  -- ^ Types for the universally and existentially quantified variables
  -> Int
  -- ^ Number of existentially quantified variables
  -> [Text]
  -- ^ Names of the fields
  -> Term n
  -- ^ Subject
  -> Int
  -- ^ Field to select
  -> Term n
selectField tenv sc dc tys nExts fieldNms subj fieldI = case dataConInstArgTys tenv sc dc tys of
  Nothing -> error ("selectField: can't instantiate " <> show dc)
  Just argTys -> bindExts sc nExts $ \sc1 exts ->
    withFreshIdBinders sc1 (zip (map systemName fieldNms) (sink1 argTys)) $ \_ fields vars ->
      Case subj (indexNote "selectField" argTys fieldI)
        [Alt (DataPat dc exts fields) (indexNote "selectField" vars fieldI)]
 where
  bindExts
    :: Distinct x
    => Scope x -> Int -> (forall l. DExt x l => Scope l -> Tele TyBinder x l -> r) -> r
  bindExts scx 0 cont = cont scx TNil
  bindExts scx k cont =
    withFreshTyBinder scx (systemName "m") typeNatKind $ \scx1 b ->
      bindExts scx1 (k - 1) $ \scx2 bs -> cont scx2 (TCons b bs)

-- | A human readable name of sort 'C.System', for a fresh binder: its unique
-- is not used, see e.g. 'withFreshIdBinder'
systemName :: Text -> C.Name a
systemName nm = C.mkUnsafeSystemName nm 0

-- | A human readable name of sort 'C.Internal', for a fresh binder. Its
-- unique is not used, see e.g. 'withFreshIdBinder'.
internalName :: Text -> C.Name a
internalName nm = C.mkUnsafeInternalName nm 0

-- | Create let-bindings with case-expressions that select the first elements
-- of a vector. For the @i@-th element, it binds
-- @el<c><i>@ to the element, and @rest<c><i>@ to the vector after it, except
-- for the last element. Gives:
--
-- * the binders,
-- * their right-hand sides, in the scope of the binders, so that they form a
--   recursive let-binding,
-- * the variables of the elements, and
-- * a case-expression that selects the vector after the last element.
extractElems
  :: forall n r
   . (HasCallStack, Distinct n)
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
  -> Integer
  -- ^ Number of elements to extract, at least 1 and at most the length
  -> Term n
  -- ^ The vector
  -> (forall l. DExt n l => Scope l -> Tele IdBinder n l -> [Term l] -> [Term l] -> Term l -> r)
  -> r
extractElems tenv sc0 consCon resTy0 s maxN count vec0 cont
  | count < 1 || count > maxN =
      error ("extractElems: can't extract " <> show count <> " elements of a vector of "
        <> show maxN)
  | otherwise = go sc0 resTy0 0 vec0 cont
 where
  go
    :: Distinct x
    => Scope x -> Type x -> Integer -> Term x
    -> (forall l. DExt x l => Scope l -> Tele IdBinder x l -> [Term l] -> [Term l] -> Term l -> r)
    -> r
  go sc resTy i vec k =
    withFreshIdBinder sc (elName i) resTy $ \sc1 elB ->
      if i == count - 1 then
        k sc1 (TCons elB TNil) [sink elRhs] [idBinderVar elB] (sink restRhs)
      else
        withFreshIdBinder sc1 (restName i) (sink restTy) $ \sc2 restB ->
          go sc2 (sink resTy) (i + 1) (idBinderVar restB) $ \sc3 bs rhss vars tl ->
            k sc3
              (TCons elB (TCons restB bs))
              (sink elRhs : sink restRhs : rhss)
              (sink (idBinderVar elB) : vars)
              tl
   where
    n = maxN - i
    tys = [numTy n, resTy, numTy (n - 1)]
    restTy = case dataConInstArgTys tenv sc consCon tys of
      Just [_, _, ty] -> ty
      _ -> error "extractElems: can't instantiate Cons"
    sel = selectField tenv sc consCon tys 1 ["_co_", "el", "rest"] vec
    elRhs = sel 1
    restRhs = sel 2

  suffix i = Text.cons s (Text.pack (show i))
  elName i = systemName ("el" <> suffix i)
  restName i = systemName ("rest" <> suffix i)

-- | Create a tree of the given elements
mkRTree
  :: (HasCallStack, Distinct n)
  => TypeEnv
  -> Scope n
  -> DataCon
  -- ^ The @LR@ constructor
  -> DataCon
  -- ^ The @BR@ constructor
  -> Type n
  -- ^ Element type
  -> Integer
  -- ^ Depth of the tree
  -> [Term n]
  -- ^ Elements
  -> Term n
mkRTree tenv sc lrCon brCon resTy = go
 where
  go _ [x] = mkApps (Data lrCon)
    [ Right (numTy 0)
    , Right resTy
    , Left (primCo (coercionTy tenv sc lrCon [numTy 0, resTy]))
    , Left x ]
  go n xs =
    let (xsL, xsR) = splitAt (length xs `div` 2) xs
    in mkApps (Data brCon)
         [ Right (numTy n)
         , Right resTy
         , Right (numTy (n - 1))
         , Left (primCo (coercionTy tenv sc brCon [numTy n, resTy, numTy (n - 1)]))
         , Left (go (n - 1) xsL)
         , Left (go (n - 1) xsR) ]

-- | Create let-bindings with case-expressions that select the elements of a
-- tree. Gives the binders, their right-hand sides in
-- the scope of the binders (so that they form a recursive let-binding), and
-- the variables of the elements.
extractTElems
  :: forall n r
   . (HasCallStack, Distinct n)
  => TypeEnv
  -> Scope n
  -> DataCon
  -- ^ The @LR@ constructor
  -> DataCon
  -- ^ The @BR@ constructor
  -> Type n
  -- ^ Element type
  -> Char
  -- ^ Appended to the names of the binders
  -> Integer
  -- ^ Depth of the tree
  -> Term n
  -- ^ The tree
  -> (forall l. DExt n l => Scope l -> Tele IdBinder n l -> [Term l] -> [Term l] -> r)
  -> r
extractTElems tenv sc0 lrCon brCon resTy0 s maxN tree0 =
  go sc0 resTy0 maxN [0 .. (2 ^ (maxN + 1)) - 2] [0 .. (2 ^ maxN) - 1] tree0
 where
  go
    :: Distinct x
    => Scope x -> Type x -> Integer -> [Int] -> [Int] -> Term x
    -> (forall l. DExt x l => Scope l -> Tele IdBinder x l -> [Term l] -> [Term l] -> r)
    -> r
  go sc resTy 0 _ ks tree k = case ks of
    kIx:_ ->
      withFreshIdBinder sc (systemName ("el" <> suffix kIx)) resTy $ \sc1 elB ->
        k sc1 (TCons elB TNil)
          [sink (selectField tenv sc lrCon [numTy 0, resTy] 0 ["_co_", "el"] tree 1)]
          [idBinderVar elB]
    [] -> error "extractTElems: no name for an element"
  go sc resTy n bs ks tree k = case splitAt (length bs `div` 2) bs of
    (b0:bL, b1:bR) ->
      let tys = [numTy n, resTy, numTy (n - 1)]
          brTy = case dataConInstArgTys tenv sc brCon tys of
            Just [_, ty, _] -> ty
            _ -> error "extractTElems: can't instantiate BR"
          sel = selectField tenv sc brCon tys 1 ["_co_", "lt", "rt"] tree
          (kL, kR) = splitAt (length ks `div` 2) ks
      in withFreshIdBinder sc (systemName ("lt" <> suffix b0)) brTy $ \sc1 ltB ->
           withFreshIdBinder sc1 (systemName ("rt" <> suffix b1)) (sink brTy) $ \sc2 rtB ->
             go sc2 (sink resTy) (n - 1) bL kL (sink (idBinderVar ltB)) $ \sc3 lbs lrhss lvars ->
               go sc3 (sink resTy) (n - 1) bR kR (sink (idBinderVar rtB)) $ \sc4 rbs rrhss rvars ->
                 k sc4
                   (TCons ltB (TCons rtB (appendTele lbs rbs)))
                   (sink (sel 1) : sink (sel 2) : sink1 lrhss ++ rrhss)
                   (sink1 lvars ++ rvars)
    _ -> error "extractTElems: no names for the subtrees"

  suffix :: Int -> Text
  suffix i = Text.cons s (Text.pack (show i))

-- * Data constructors

-- | The types of the fields of a data constructor, given types for its
-- universally quantified type variables. 'Nothing' if there are more or fewer
-- types than universally quantified type variables, or if a field type
-- mentions an existentially quantified type variable.
substArgTys
  :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> DataCon -> [Type n] -> Maybe [Type n]
substArgTys tenv sc dc tys = join $
  dataConInstExtTyVars tenv sc dc tys $ \_ exts argTys ->
    traverse (unsinkType (teleNameBinders exts)) argTys

-- | Make a case-expression that selects a field of a data constructor. The
-- binders of the pattern get fresh names. Errors if
-- the type of the field mentions an existentially quantified type variable
-- that the type equations among the field types don't solve.
mkSelectorCase
  :: (HasCallStack, Distinct n)
  => String
  -- ^ Name of the caller
  -> Scope n
  -> TypeEnv
  -> Term n
  -- ^ Subject
  -> Int
  -- ^ The @n@-th data constructor, from 1
  -> Int
  -- ^ The @n@-th field, from 0
  -> Term n
mkSelectorCase caller sc tenv scrut dcI fieldI = go (inferCoreTypeOf tenv sc scrut)
 where
  go scrutTy
    | Just ty' <- coreView1 tenv sc scrutTy = go ty'
    | TyConApp tc args <- tyView scrutTy =
        case C.tyConDataCons (findTyCon tenv tc) of
          [] -> cantCreate $(curLoc) ("TyCon has no DataCons: " ++ show tc) scrutTy
          dcs
            | dcI > length dcs -> cantCreate $(curLoc) "DC index exceeds max" scrutTy
            | otherwise ->
                let dc = indexNote ($(curLoc) ++ "No DC with tag: " ++ show (dcI - 1)) dcs (dcI - 1)
                    res = dataConInstArgTysE tenv sc dc args $ \sc1 exts fieldTys ->
                      if fieldI >= length fieldTys then
                        cantCreate $(curLoc) "Field index exceed max" scrutTy
                      else
                        case unsinkType (teleNameBinders exts) (fieldTys !! fieldI) of
                          Nothing ->
                            cantCreate $(curLoc) "Field type mentions an existential" scrutTy
                          Just ty ->
                            let nms = [ internalName (if i == fieldI then "sel" else "wild")
                                      | i <- [0 .. length fieldTys - 1] ]
                            in withFreshIdBinders sc1 (zip nms fieldTys) $ \_ fields vars ->
                                 Case scrut ty [Alt (DataPat dc exts fields) (vars !! fieldI)]
                in case res of
                     Just e -> e
                     Nothing -> cantCreate $(curLoc) "Cannot instantiate dataCon" scrutTy
    | otherwise =
        cantCreate $(curLoc) ("Type of subject is not a datatype: " ++ showPpr scrutTy) scrutTy

  cantCreate :: String -> String -> Type n -> a
  cantCreate loc info scrutTy = error $
    loc ++ "Can't create selector " ++ show (caller, dcI, fieldI) ++ " for: ("
      ++ showPpr scrut ++ " :: " ++ showPpr scrutTy ++ ")\nAdditional info: " ++ info

-- * Primitives

-- | A coercion of the given type. The type is a type argument of the
-- primitive, as "Clash.GHC.Scoped.GHC2Core" makes them: the type of a
-- primitive is closed.
primCo :: Type n -> Term n
primCo = TyApp (Prim coPrimInfo)

-- | The primitive of 'primCo'
coPrimInfo :: PrimInfo
coPrimInfo = mkPrimInfo "_CO_" undefinedTy WorkNever SingleResult NoUnfolding
{-# NOINLINE coPrimInfo #-}

-- | A primitive whose type is that of a term it replaces, e.g. @errorX@ for a
-- case-expression without defined alternatives. The type of a primitive is
-- closed: if the given type mentions type variables, this is the primitive,
-- of type @forall a. a@, applied to the type, like 'primCo'. Otherwise, it is
-- the primitive with the given type.
primAtType :: PrimInfo -> Type n -> Term n
primAtType p ty = case closeType ty of
  Just closed -> Prim (withType closed)
  Nothing -> TyApp (Prim (withType undefinedTy)) ty
 where
  withType t =
    mkPrimInfo (primName p) t (primWorkInfo p) (primMultiResult p) (primUnfolding p)

-- | An unsafe coercion
primUCo :: Term n
primUCo = Prim uCoPrimInfo

-- | The primitive of 'primUCo'
uCoPrimInfo :: PrimInfo
uCoPrimInfo =
  mkPrimInfo "GHC.Prim.unsafeCoerce#" unsafeCoerceTy WorkNever SingleResult NoUnfolding
{-# NOINLINE uCoPrimInfo #-}

-- * Types

-- | How to split values of a product type, see 'shouldSplit'
data SplitInfo n = SplitInfo
  { splitConstruct :: forall x. DExt n x => Scope x -> [Term x] -> Term x
  -- ^ Creates a value of the product type out of values of the types of
  -- 'splitArgTys'
  , splitProject :: forall x. DExt n x => Scope x -> Term x -> [Term x]
  -- ^ Given a value of the product type, creates projections of it for all
  -- the types of 'splitArgTys'
  , splitArgTys :: [Type n]
  -- ^ The types of the fields of the product type
  }

-- | Whether to split values of a product type, i.e. one that contains
-- clocks, resets, enables or file handles. Only splits one level at a time.
--
-- Doesn't split product types of which the type of a field mentions an
-- existentially quantified type variable, see 'substArgTys'.
shouldSplit :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> Type n -> Maybe (SplitInfo n)
shouldSplit tenv sc ty = case tyView ty of
  -- Look through SimIO to find things like Files
  TyConApp tcNm [tyArg] | C.nameOcc tcNm == "Clash.Explicit.SimIO.SimIO" -> shouldSplit tenv sc tyArg
  _ -> shouldSplit0 tenv sc (tyView (coreView tenv sc ty))

-- | Worker of 'shouldSplit', on a 'TypeView'
shouldSplit0
  :: forall n
   . (HasCallStack, Distinct n)
  => TypeEnv -> Scope n -> TypeView n -> Maybe (SplitInfo n)
shouldSplit0 tenv sc = \case
  TyConApp tcNm tyArgs
    | Just tc <- lookupTyCon tenv tcNm
    , [dc] <- C.tyConDataCons tc
    , Just dcArgs <- substArgTys tenv sc dc tyArgs
    , let dcArgsLen = length dcArgs
    , dcArgsLen > 1
    -> if any (shouldSplitTy . tyView . coreView tenv sc) dcArgs && not (isHidden tcNm tyArgs) then
         Just SplitInfo
           { splitConstruct = \_ args ->
               mkApps (Data dc) (map Right (sink1 tyArgs) ++ map Left args)
           , splitProject = \scx subj ->
               map (mkSelectorCase ($(curLoc) ++ "splitArg") scx tenv subj 1) [0 .. dcArgsLen - 1]
           , splitArgTys = dcArgs
           }
       else
         Nothing
    | C.nameOcc tcNm == "Clash.Sized.Vector.Vec"
    , [nTy, argTy] <- tyArgs
    , Right n <- runExcept (tyNatSize tenv sc nTy)
    , n > 1
    , Just tc <- lookupTyCon tenv tcNm
    , [nil, cons] <- C.tyConDataCons tc
    -> if shouldSplitTy (tyView (coreView tenv sc argTy)) then
         Just SplitInfo
           { splitConstruct = \scx -> mkVec tenv scx nil cons (sink argTy) n
           , splitProject = \scx subj -> map (mkVecSelector scx subj) [0 .. n - 1]
           , splitArgTys = replicate (fromInteger n) argTy
           }
       else
         Nothing
  _ -> Nothing
 where
  -- Project the n'th element out of a vector
  mkVecSelector :: Distinct x => Scope x -> Term x -> Integer -> Term x
  mkVecSelector scx subj 0 = mkSelectorCase ($(curLoc) ++ "mkVecSelector") scx tenv subj 2 1
  mkVecSelector scx subj i =
    mkVecSelector scx (mkSelectorCase ($(curLoc) ++ "mkVecSelector") scx tenv subj 2 2) (i - 1)

  shouldSplitTy :: TypeView n -> Bool
  shouldSplitTy ty = isJust (shouldSplit0 tenv sc ty) || splitTy ty

  -- Hidden constructs (HiddenClock, HiddenReset, ..) don't need to be split
  -- because KnownDomain will be filtered anyway during netlist generation due
  -- to it being a zero-width type
  --
  -- TODO: This currently only handles (IP $x, KnownDomain) given that $x is any
  -- TODO: of the constructs handled in 'splitTy'. In practice this means only
  -- TODO: HiddenClock, HiddenReset, and HiddenEnable are handled. If a user were
  -- TODO: to define their own versions with -for example- the elements of the
  -- TODO: tuple swapped, 'isHidden' wouldn't recognize it. We could generalize
  -- TODO: this in the future.
  isHidden :: C.TyConName -> [Type n] -> Bool
  isHidden nm [a1, a2]
    | TyConApp a2Nm _ <- tyView a2 =
        C.nameOcc nm `elem`
          [ "GHC.Classes.(%,%)", "GHC.Internal.Classes.(%,%)"
          , "GHC.Classes.CTuple2", "GHC.Internal.Classes.CTuple2" ]
          && splitTy (tyView (stripIP a1))
          && C.nameOcc a2Nm == "Clash.Signal.Internal.KnownDomain"
  isHidden _ _ = False

  splitTy :: TypeView n -> Bool
  splitTy = \case
    TyConApp tcNm0 _ -> C.nameOcc tcNm0 `elem`
      [ "Clash.Signal.Internal.Clock"
      , "Clash.Signal.Internal.ClockN"
      , "Clash.Signal.Internal.Reset"
      , "Clash.Signal.Internal.Enable"
      , "Clash.Explicit.SimIO.File"
      , "GHC.IO.Handle.Types.Handle"
      , "GHC.Internal.IO.Handle.Types.Handle" ]
    _ -> False

-- | Split the types of the arguments of a function that 'shouldSplit' splits,
-- recursively
splitShouldSplit :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> [Type n] -> [Type n]
splitShouldSplit tenv sc = foldr go []
 where
  go ty rest = case shouldSplit tenv sc ty of
    Just info -> splitShouldSplit tenv sc (splitArgTys info) ++ rest
    Nothing -> ty : rest
