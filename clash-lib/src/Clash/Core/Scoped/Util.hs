{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Smart constructors and destructors for scoped Clash Core: ports of
  "Clash.Core.Util".

  = Let-bindings

  Clash Core passes let-bindings around as lists, which scoped Core can't:
  the right-hand side of a binding may only mention the binders in scope at
  it. The functions here take the bindings of a recursive let-expression
  (@'Let' ('Rec' bs xs) body@) and regroup or reorder them, giving a
  telescope of let-bindings ('Tele' t'Bind'). They reuse the binders when the
  order of the bindings doesn't change, and give them fresh names otherwise.

  = Fresh binders

  Functions that build terms with new binders, such as 'extractElems' and
  'mkSelectorCase', take the scope, and give the binders fresh names in it.
  Their counterparts on Clash Core take a unique supply and an in-scope set,
  or use fixed uniques.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Core.Scoped.Util
  ( -- * Let-bindings
    mkLets
  , listToLets
  , sccLetBindings
  , inverseTopSortLetBindings
  , LetGroup (..)
  , regroupLetBindings
  ) where

import qualified Data.Graph as Graph
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)

import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C

import Clash.Core.Scoped.FreeVars (freeTmVars, teleNameBinders)
import Clash.Core.Scoped.NameMap
  (NameBinders, appendNameBinders, emptyNameBinders, idBinderNameBinders, nameSetToList)
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (splitTele)

-- * Let-bindings

-- | Wrap a term in let-expressions, the first one outermost
mkLets :: Tele Bind n l -> Term l -> Term n
mkLets TNil e = e
mkLets (TCons b bs) e = Let b (mkLets bs e)

-- | Split recursive let-bindings into strongly connected components, and
-- wrap the body in a let-expression for each, see 'C.listToLets'
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
-- bindings that refer to it, and a recursive group for each cycle. See
-- 'C.sccLetBindings'.
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
-- depth-first search, see 'C.inverseTopSortLetBindings'. Bindings that are
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
  regroupLetBindings sc bs xs body [RecGroup order] $ \sc' binds body' ->
    case binds of
      TCons (Rec bs' xs') TNil -> cont sc' bs' xs' body'
      _ -> error "inverseTopSortLetBindings: expected a single recursive group"
 where
  deps = letDependencies bs xs
  graph = Graph.buildG (0, length deps - 1) [(i, d) | (i, ds) <- zip [0..] deps, d <- ds]
  order = postorderF (Graph.dff graph) []

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
teleOccs :: Tele IdBinder n l -> [C.Name C.Term]
teleOccs TNil = []
teleOccs (TCons b bs) = idBinderOcc b : teleOccs bs

-- | For every right-hand side, the positions of the bindings it refers to
letDependencies :: Tele IdBinder n l -> [Term l] -> [[Int]]
letDependencies bs = map deps
 where
  positions = IntMap.fromList (zip (raws bs) [0..])

  deps x =
    [ i
    | v <- nameSetToList (freeTmVars x)
    , Just i <- [IntMap.lookup (tmNameRaw v) positions] ]

  raws :: Tele IdBinder x y -> [Int]
  raws TNil = []
  raws (TCons b rest) = tmNameRaw (tmNameOf b) : raws rest

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
