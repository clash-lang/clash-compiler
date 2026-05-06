{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Core.VarEnv
  ( -- * Environment with variables as keys
    VarEnv
    -- ** Accessors
    -- *** Size information
  , nullVarEnv
    -- ** Indexing
  , lookupVarEnv
  , lookupVarEnv'
  , lookupVarEnvDirectly
    -- ** Construction
  , emptyVarEnv
  , unitVarEnv
  , mkVarEnv
    -- ** Modification
  , extendVarEnv
  , extendVarEnvList
  , extendVarEnvWith
  , delVarEnv
  , delVarEnvList
  , unionVarEnv
  , unionVarEnvWith
  , differenceVarEnv
    -- ** Element-wise operations
    -- *** Mapping
  , mapVarEnv
  , mapMaybeVarEnv
    -- ** Folding
  , foldlWithUniqueVarEnv'
    -- ** Working with predicates
    -- *** Searching
  , elemVarEnv
  , notElemVarEnv
    -- ** Conversions
    -- *** Lists
  , eltsVarEnv
  , toListVarEnv
  , listToVarEnv
    -- * Sets of variables
  , VarSet
    -- ** Construction
  , emptyVarSet
  , unitVarSet
    -- ** Modification
  , extendVarSet
  , delVarSetByKey
  , unionVarSet
  , differenceVarSet
    -- ** Working with predicates
  , nullVarSet
    -- *** Searching
  , elemVarSet
  , notElemVarSet
  , subsetVarSet
  , disjointVarSet
    -- ** Conversions
    -- *** Lists
  , mkVarSet
  , eltsVarSet
    -- * In-scope sets
  , InScopeSet
    -- ** Accessors
    -- *** Size information
  , emptyInScopeSet
    -- *** Indexing
  , lookupInScope
    -- ** Construction
  , mkInScopeSet
    -- ** Modification
  , extendInScopeSet
  , extendInScopeSetList
  , unionInScope
    -- ** Working with predicates
    -- *** Searching
  , elemInScopeSet
  , elemUniqInScopeSet
  , notElemInScopeSet
  , varSetInScope
    -- ** Unique generation
  , uniqAway
  , uniqAway'
    -- * Dual renaming
  , RnEnv
    -- ** Construction
  , mkRnEnv
    -- ** Renaming
  , rnTmBndr
  , rnTyBndr
  , rnTmBndrs
  , rnTyBndrs
  , rnOccLId
  , rnOccRId
  , rnOccLTy
  , rnOccRTy
  )
where

#if MIN_VERSION_ghc(9,8,4) || (MIN_VERSION_ghc(9,6,7) && !MIN_VERSION_ghc(9,8,0))
#define UNIQUE_IS_WORD64
#endif

import           Control.DeepSeq           (NFData)
import           Data.Binary               (Binary)
import           Data.Coerce               (coerce)
import qualified Data.List                 as List
import qualified Data.List.Extra           as List
import           Data.Maybe                (fromMaybe)
#ifdef UNIQUE_IS_WORD64
import           Data.Word                 (Word64)
#endif

#if MIN_VERSION_prettyprinter(1,7,0)
import           Prettyprinter
#else
import           Data.Text.Prettyprint.Doc
#endif

import           GHC.Exts                  (Any)
import           GHC.Generics              (Generic)
import           GHC.Stack                 (HasCallStack)

import           Clash.Core.Pretty         ()
import           Clash.Core.Var
import           Clash.Data.UniqMap        (UniqMap)
import qualified Clash.Data.UniqMap as UniqMap
import           Clash.Debug               (debugIsOn)
import           Clash.Unique
import           Clash.Util
import           Clash.Pretty

-- * VarEnv

-- | Map indexed by variables
type VarEnv a = UniqMap a

-- | Empty map
emptyVarEnv
  :: VarEnv a
emptyVarEnv = UniqMap.empty

-- | Environment containing a single variable-value pair
unitVarEnv
  :: Var b
  -> a
  -> VarEnv a
unitVarEnv = UniqMap.singleton

-- | Look up a value based on the variable
lookupVarEnv
  :: Var b
  -> VarEnv a
  -> Maybe a
lookupVarEnv = UniqMap.lookup

-- | Lookup a value based on the unique of a variable
lookupVarEnvDirectly
  :: Unique
  -> VarEnv a
  -> Maybe a
lookupVarEnvDirectly = UniqMap.lookup

-- | Lookup a value based on the variable
--
-- Errors out when the variable is not present
lookupVarEnv'
  :: HasCallStack
  => VarEnv a
  -> Var b
  -> a
lookupVarEnv' = flip UniqMap.find

-- | Remove a variable-value pair from the environment
delVarEnv
  :: VarEnv a
  -> Var b
  -> VarEnv a
delVarEnv = flip UniqMap.delete

-- | Remove a list of variable-value pairs from the environment
delVarEnvList
  :: VarEnv a
  -> [Var b]
  -> VarEnv a
delVarEnvList = flip UniqMap.deleteMany

-- | Add a variable-value pair to the environment; overwrites the value if the
-- variable already exists
extendVarEnv
  :: Var b
  -> a
  -> VarEnv a
  -> VarEnv a
extendVarEnv = UniqMap.insert

-- | Add a variable-value pair to the environment; if the variable already
-- exists, the two values are merged with the given function
extendVarEnvWith
  :: Var b
  -> a
  -> (a -> a -> a)
  -> VarEnv a
  -> VarEnv a
extendVarEnvWith k v f =
  UniqMap.insertWith f k v

-- | Add a list of variable-value pairs; the values of existing keys will be
-- overwritten
extendVarEnvList
  :: VarEnv a
  -> [(Var b, a)]
  -> VarEnv a
extendVarEnvList = flip UniqMap.insertMany

-- | Is the environment empty
nullVarEnv
  :: VarEnv a
  -> Bool
nullVarEnv = UniqMap.null

-- | Get the (left-biased) union of two environments
unionVarEnv
  :: VarEnv a
  -> VarEnv a
  -> VarEnv a
unionVarEnv = (<>)

-- | Get the union of two environments, mapped values existing in both
-- environments will be merged with the given function.
unionVarEnvWith
  :: (a -> a -> a)
  -> VarEnv a
  -> VarEnv a
  -> VarEnv a
unionVarEnvWith = UniqMap.unionWith

-- | Filter the first varenv to only contain keys which are not in the second varenv.
differenceVarEnv
  :: VarEnv a
  -> VarEnv a
  -> VarEnv a
differenceVarEnv = UniqMap.difference

-- | Create an environment given a list of var-value pairs
mkVarEnv
  :: [(Var a,b)]
  -> VarEnv b
mkVarEnv = UniqMap.fromList

-- | Apply a function to every element in the environment
mapVarEnv
  :: (a -> b)
  -> VarEnv a
  -> VarEnv b
mapVarEnv = fmap

-- | Apply a function to every element in the environment; values for which the
-- function returns 'Nothing' are removed from the environment
mapMaybeVarEnv
  :: (a -> Maybe b)
  -> VarEnv a
  -> VarEnv b
mapMaybeVarEnv = UniqMap.mapMaybe

-- | Strict left-fold over an environment using both the unique of the
-- the variable and the value
foldlWithUniqueVarEnv'
  :: (a -> Unique -> b -> a)
  -> a
  -> VarEnv b
  -> a
foldlWithUniqueVarEnv' = UniqMap.foldlWithUnique'

-- | Extract the elements
eltsVarEnv
  :: VarEnv a
  -> [a]
eltsVarEnv = UniqMap.elems

toListVarEnv :: VarEnv a -> [(Unique, a)]
toListVarEnv = UniqMap.toList

listToVarEnv
  :: Uniquable a
  => [(a, b)]
  -> VarEnv b
listToVarEnv = UniqMap.fromList

-- | Does the variable exist in the environment
elemVarEnv
  :: Var a
  -> VarEnv b
  -> Bool
elemVarEnv = UniqMap.elem

-- | Does the variable not exist in the environment
notElemVarEnv
  :: Var a
  -> VarEnv b
  -> Bool
notElemVarEnv = UniqMap.notElem

-- * VarSet

-- | Set of variables
type VarSet = UniqMap (Var Any)

-- | The empty set
emptyVarSet
  :: VarSet
emptyVarSet = UniqMap.empty

-- | The set of a single variable
unitVarSet
  :: Var a
  -> VarSet
unitVarSet v = UniqMap.singletonUnique (coerce v)

-- | Add a variable to the set
extendVarSet
  :: VarSet
  -> Var a
  -> VarSet
extendVarSet env v = UniqMap.insertUnique (coerce v) env

-- | Union two sets
unionVarSet
  :: VarSet
  -> VarSet
  -> VarSet
unionVarSet = (<>)

-- | Take the difference of two sets
differenceVarSet
  :: VarSet
  -> VarSet
  -> VarSet
differenceVarSet = UniqMap.difference

-- | Is the variable an element in the set
elemVarSet
  :: Var a
  -> VarSet
  -> Bool
elemVarSet v = UniqMap.elem (getUnique v)

-- | Is the variable not an element in the set
notElemVarSet
  :: Var a
  -> VarSet
  -> Bool
notElemVarSet v = UniqMap.notElem (getUnique v)

-- | Is the set of variables A a subset of the variables B
subsetVarSet
  :: VarSet
  -- ^ Set of variables A
  -> VarSet
  -- ^ Set of variables B
  -> Bool
subsetVarSet = UniqMap.submap

-- | Are the sets of variables disjoint
disjointVarSet
  :: VarSet
  -> VarSet
  -> Bool
disjointVarSet = UniqMap.disjoint

-- | Check whether a varset is empty
nullVarSet
  :: VarSet
  -> Bool
nullVarSet = UniqMap.null

-- | Look up a variable in the set, returns it if it exists
lookupVarSet
  :: Var a
  -> VarSet
  -> Maybe (Var Any)
lookupVarSet = UniqMap.lookup

-- | Remove a variable from the set based on its 'Unique'
delVarSetByKey
  :: Unique
  -> VarSet
  -> VarSet
delVarSetByKey = UniqMap.delete

-- | Create a set from a list of variables
mkVarSet
  :: [Var a]
  -> VarSet
mkVarSet xs = UniqMap.fromList $ fmap (\x -> (getUnique x, coerce x)) xs

eltsVarSet
  :: VarSet
  -> [Var Any]
eltsVarSet = UniqMap.elems

-- * InScopeSet

type Seed
#ifdef UNIQUE_IS_WORD64
  = Word64
#else
  = Int
#endif

-- | Set of variables that is in scope at some point
--
-- The 'Seed' is a kind of hash-value used to generate new uniques. It should
-- never be zero
--
-- See "Secrets of the Glasgow Haskell Compiler inliner" Section 3.2 for the
-- motivation
data InScopeSet = InScopeSet VarSet {-# UNPACK #-} !Seed
  deriving (Generic, NFData, Binary)

instance ClashPretty InScopeSet where
  clashPretty (InScopeSet s _) = clashPretty s

-- | The empty set
extendInScopeSet
  :: InScopeSet
  -> Var a
  -> InScopeSet
extendInScopeSet (InScopeSet inScope n) v =
  InScopeSet (extendVarSet inScope v) (n + 1)

-- | Add a list of variables in scope
extendInScopeSetList
  :: InScopeSet
  -> [Var a]
  -> InScopeSet
extendInScopeSetList (InScopeSet inScope n) vs =
  InScopeSet (List.foldl' extendVarSet inScope vs) (n + fromIntegral (length vs))

-- | Union two sets of in scope variables
unionInScope
  :: InScopeSet
  -> InScopeSet
  -> InScopeSet
unionInScope (InScopeSet s1 _) (InScopeSet s2 n2)
  = InScopeSet (s1 `unionVarSet` s2) n2

-- | Is the set of variables in scope
varSetInScope
  :: VarSet
  -> InScopeSet
  -> Bool
varSetInScope vars (InScopeSet s1 _)
  = vars `subsetVarSet` s1

-- | Look up a variable in the 'InScopeSet'. This gives you the canonical
-- version of the variable
lookupInScope
  :: InScopeSet
  -> Var a
  -> Maybe (Var Any)
lookupInScope (InScopeSet s _) v = lookupVarSet v s

-- | Is the variable in scope
elemInScopeSet
  :: Var a
  -> InScopeSet
  -> Bool
elemInScopeSet v (InScopeSet s _) = elemVarSet v s

-- | Check whether an element exists in the set based on the `Unique` contained
-- in that element
elemUniqInScopeSet
  :: Unique
  -> InScopeSet
  -> Bool
elemUniqInScopeSet u (InScopeSet s _) = UniqMap.elem u s

-- | Is the variable not in scope
notElemInScopeSet
  :: Var a
  -> InScopeSet
  -> Bool
notElemInScopeSet v (InScopeSet s _) = notElemVarSet v s

-- | Create a set of variables in scope
mkInScopeSet
  :: VarSet
  -> InScopeSet
mkInScopeSet is = InScopeSet is 1

-- | The empty set
emptyInScopeSet
  :: InScopeSet
emptyInScopeSet = mkInScopeSet emptyVarSet

-- | Ensure that the 'Unique' of a variable does not occur in the 'InScopeSet'
uniqAway
  :: (Uniquable a, ClashPretty a)
  => InScopeSet
  -> a
  -> a
uniqAway (InScopeSet set n) a =
  uniqAway' (`UniqMap.elem` set) n a

uniqAway'
  :: (Uniquable a, ClashPretty a)
  => (Unique -> Bool)
  -- ^ Unique in scope test
  -> Seed
  -- ^ Seed
  -> a
  -> a
uniqAway' inScopeTest n u =
  if inScopeTest (getUnique u) then
    try 1
  else
    u
 where
  origUniq = getUnique u
  try k
    | debugIsOn && k > 1000
    = pprPanic "uniqAway loop:" msg
    | inScopeTest uniq
    = try (k + 1)
    | k > 3
    = pprTraceDebug "uniqAway:" msg (setUnique u uniq)
    | otherwise
    = setUnique u uniq
    where
      msg  = fromPretty k <+> "tries" <+> clashPretty u <+> fromPretty n
      uniq = deriveUnique origUniq (n * k)

deriveUnique
  :: Unique
  -> Seed
  -> Unique
deriveUnique i delta = i + delta

-- * RnEnv

-- | Rename environment for e.g. alpha equivalence
--
-- When going under binders for e.g.
--
-- @
-- \x -> e1  `aeq` \y -> e2
-- @
--
-- We want to rename @[x -> y]@  or @[y -> x]@, but we have to pick a binder
-- that is neither free in @e1@ nor @e2@ or we risk accidental capture.
--
-- So we must maintain:
--
--   1. A renaming for the left term
--
--   2. A renaming for the right term
--
--   3. A set of in scope variables
data RnEnv
  = RnEnv
  { rn_envLTy  :: VarEnv TyVar
    -- ^ Type renaming for the left term
  , rn_envLTm  :: VarEnv Id
    -- ^ Term renaming for the left term
  , rn_envRTy  :: VarEnv TyVar
    -- ^ Type renaming for the right term
  , rn_envRTm  :: VarEnv Id
    -- ^ Term renaming for the right term
  , rn_inScope :: InScopeSet
    -- ^ In scope in left or right terms
  }

-- | Create an empty renaming environment
mkRnEnv
  :: InScopeSet -> RnEnv
mkRnEnv vars
  = RnEnv
  { rn_envLTy  = emptyVarEnv
  , rn_envLTm  = emptyVarEnv
  , rn_envRTy  = emptyVarEnv
  , rn_envRTm  = emptyVarEnv
  , rn_inScope = vars
  }

-- | Look up the renaming of an type-variable occurrence in the left term
rnOccLTy
  :: RnEnv -> TyVar -> TyVar
rnOccLTy rn v = fromMaybe v (lookupVarEnv v (rn_envLTy rn))

-- | Look up the renaming of an type-variable occurrence in the right term
rnOccRTy
  :: RnEnv -> TyVar -> TyVar
rnOccRTy rn v = fromMaybe v (lookupVarEnv v (rn_envRTy rn))

-- | Simultaneously go under the type-variable binder /bTvL/ and type-variable
-- binder /bTvR/, finds a new binder /newTvB/, and return an environment mapping
-- @[bTvL -> newB]@ and @[bTvR -> newB]@
rnTyBndr
  :: RnEnv -> TyVar -> TyVar -> RnEnv
rnTyBndr rv@(RnEnv {rn_envLTy = lenv, rn_envRTy = renv, rn_inScope = inScope}) bL bR =
  rv { rn_envLTy = extendVarEnv bL newB lenv -- See Note [Rebinding and shadowing]
     , rn_envRTy = extendVarEnv bR newB renv
     , rn_inScope = extendInScopeSet inScope newB }
 where
  -- Find a new type-binder not in scope in either term
  newB | not (bL `elemInScopeSet` inScope) = bL
       | not (bR `elemInScopeSet` inScope) = bR
       | otherwise                         = uniqAway inScope bL

{- Note [Rebinding and shadowing]
Imagine:

@
\x -> \x -> e1  `aeq` \y -> \x -> e2
@

Then inside

@
\x \y  { [x->p] [y->p]  {p} }
\x \z  { [x->q] [y->p, z->q] {p,q} }
@

i.e. if the new var is the same as the old var, the renaming is deleted by
'extendVarEnv'
-}

-- | Applies 'rnTyBndr' to several variables: the two variable lists must be of
-- equal length.
rnTyBndrs
  :: RnEnv -> [TyVar] -> [TyVar] -> RnEnv
rnTyBndrs env tvs1 tvs2 =
  List.foldl' (\s (l,r) -> rnTyBndr s l r) env (List.zipEqual tvs1 tvs2)

-- | Look up the renaming of an occurrence in the left term
rnOccLId
  :: RnEnv -> Id -> Id
rnOccLId rn v = fromMaybe v (lookupVarEnv v (rn_envLTm rn))

-- | Look up the renaming of an occurrence in the left term
rnOccRId
  :: RnEnv -> Id -> Id
rnOccRId rn v = fromMaybe v (lookupVarEnv v (rn_envRTm rn))

-- | Simultaneously go under the binder /bL/ and binder /bR/, finds a new binder
-- /newTvB/, and return an environment mapping @[bL -> newB]@ and @[bR -> newB]@
rnTmBndr
  :: RnEnv -> Id -> Id -> RnEnv
rnTmBndr rv@(RnEnv {rn_envLTm = lenv, rn_envRTm = renv, rn_inScope = inScope}) bL bR =
  rv { rn_envLTm = extendVarEnv bL newB lenv -- See Note [Rebinding and shadowing]
     , rn_envRTm = extendVarEnv bR newB renv
     , rn_inScope = extendInScopeSet inScope newB }
 where
  -- Find a new type-binder not in scope in either term
  newB | not (bL `elemInScopeSet` inScope) = bL
       | not (bR `elemInScopeSet` inScope) = bR
       | otherwise                         = uniqAway inScope bL

-- | Applies 'rnTmBndr' to several variables: the two variable lists must be of
-- equal length.
rnTmBndrs
  :: RnEnv -> [Id] -> [Id] -> RnEnv
rnTmBndrs env ids1 ids2 =
  List.foldl' (\s (l,r) -> rnTmBndr s l r) env (List.zipEqual ids1 ids2)
