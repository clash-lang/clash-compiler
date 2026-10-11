{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Sets and maps of local and global names of scoped Clash Core.

  Local names and global names live in separate namespaces: a 'NameMap' is
  keyed by local names ('TyName' or 'TmName') of one scope, a t'GlobalEnv' by
  global names ('GlobalId'). Clash's 'Clash.Core.VarEnv.VarEnv' is keyed by the
  unique of an 'Clash.Core.Var.Id', whether local or global, which makes it easy
  to look up a local variable in a map of global bindings or vice versa.

  A 'NameSet' or 'NameMap' is indexed by the scope of its keys. Move it into an
  extended scope with 'sinkNameSet' and 'sinkNameMap', and out of the scope of
  binders with 'unsinkNameSet' and 'unsinkNameMap'.

  A 'GlobalId' does not imply that there is a global binding for it: e.g. a
  t'GlobalEnv' of bindings need not have an entry for every 'GlobalId' in use.
  Lists of the elements of a t'GlobalSet' or t'GlobalEnv' are in the order of
  the uniques of the 'GlobalId's.
-}

module Clash.Core.Scoped.NameMap
  ( -- * Sets of local names
    NameSet
  , emptyNameSet
  , singletonNameSet
  , insertNameSet
  , deleteNameSet
  , memberNameSet
  , nullNameSet
  , sizeNameSet
  , unionNameSet
  , unionsNameSet
  , differenceNameSet
  , nameSetToList
  , nameSetFromList
  , sinkNameSet
  , unsinkNameSet
    -- * Maps from local names
  , NameMap
  , emptyNameMap
  , singletonNameMap
  , insertNameMap
  , insertWithNameMap
  , deleteNameMap
  , lookupNameMap
  , memberNameMap
  , nullNameMap
  , sizeNameMap
  , unionNameMap
  , unionWithNameMap
  , nameMapToList
  , nameMapFromList
  , nameMapKeys
  , nameMapElems
  , sinkNameMap
  , unsinkNameMap
    -- * The names bound between two scopes
  , NameBinders
  , emptyNameBinders
  , snocNameBinders
  , appendNameBinders
  , tyBinderNameBinders
  , idBinderNameBinders
    -- * Sets of global names
  , GlobalSet
  , emptyGlobalSet
  , singletonGlobalSet
  , insertGlobalSet
  , deleteGlobalSet
  , memberGlobalSet
  , nullGlobalSet
  , sizeGlobalSet
  , unionGlobalSet
  , unionsGlobalSet
  , differenceGlobalSet
  , filterGlobalSet
  , globalSetToList
  , globalSetFromList
    -- * Environments of global names
  , GlobalEnv
  , emptyGlobalEnv
  , singletonGlobalEnv
  , insertGlobalEnv
  , insertWithGlobalEnv
  , deleteGlobalEnv
  , adjustGlobalEnv
  , alterGlobalEnv
  , lookupGlobalEnv
  , memberGlobalEnv
  , nullGlobalEnv
  , sizeGlobalEnv
  , unionGlobalEnv
  , unionWithGlobalEnv
  , differenceGlobalEnv
  , filterGlobalEnv
  , filterWithGlobalEnv
  , mapWithGlobalEnv
  , foldrWithGlobalEnv
  , restrictGlobalEnv
  , withoutGlobalEnv
  , globalEnvToList
  , globalEnvFromList
  , globalEnvKeys
  , globalEnvKeysSet
  , globalEnvElems
  ) where

import qualified Data.List as List

import Clash.Data.UniqMap (UniqMap)
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Scoped.Internal.Names
import Clash.Core.Scoped.Syntax

-- | The name a type variable binder binds, as 'NameBinders'
tyBinderNameBinders :: TyBinder n l -> NameBinders n l
tyBinderNameBinders b = snocNameBinders emptyNameBinders (binderOf b)
 where
  binderOf :: TyBinder n l -> NameBinder n l
  binderOf = unsortTyNameBinder . tyBinderName

-- | The name a term variable binder binds, as 'NameBinders'
idBinderNameBinders :: IdBinder n l -> NameBinders n l
idBinderNameBinders b = snocNameBinders emptyNameBinders (binderOf b)
 where
  binderOf :: IdBinder n l -> NameBinder n l
  binderOf = unsortTmNameBinder . idBinderName

-- * Sets of global names

-- | A set of global names
newtype GlobalSet = GlobalSet (UniqMap GlobalId)

instance Eq GlobalSet where
  s1 == s2 = globalSetToList s1 == globalSetToList s2

instance Show GlobalSet where
  showsPrec d s = showParen (d > 10) $
    showString "globalSetFromList " . shows (globalSetToList s)

instance Semigroup GlobalSet where
  (<>) = unionGlobalSet

instance Monoid GlobalSet where
  mempty = emptyGlobalSet

-- | The empty set
emptyGlobalSet :: GlobalSet
emptyGlobalSet = GlobalSet UniqMap.empty

-- | A set of one global
singletonGlobalSet :: GlobalId -> GlobalSet
singletonGlobalSet g = GlobalSet (UniqMap.singleton (globalIdUniq g) g)

-- | Add a global
insertGlobalSet :: GlobalId -> GlobalSet -> GlobalSet
insertGlobalSet g (GlobalSet s) = GlobalSet (UniqMap.insert (globalIdUniq g) g s)

-- | Remove a global
deleteGlobalSet :: GlobalId -> GlobalSet -> GlobalSet
deleteGlobalSet g (GlobalSet s) = GlobalSet (UniqMap.delete (globalIdUniq g) s)

-- | Whether a global is in the set
memberGlobalSet :: GlobalId -> GlobalSet -> Bool
memberGlobalSet g (GlobalSet s) = UniqMap.elem (globalIdUniq g) s

-- | Whether the set is empty
nullGlobalSet :: GlobalSet -> Bool
nullGlobalSet (GlobalSet s) = UniqMap.null s

-- | The number of globals in the set
sizeGlobalSet :: GlobalSet -> Int
sizeGlobalSet (GlobalSet s) = length s

-- | The union of two sets
unionGlobalSet :: GlobalSet -> GlobalSet -> GlobalSet
unionGlobalSet (GlobalSet s1) (GlobalSet s2) = GlobalSet (UniqMap.unionWith const s1 s2)

-- | The union of a list of sets
unionsGlobalSet :: [GlobalSet] -> GlobalSet
unionsGlobalSet = List.foldl' unionGlobalSet emptyGlobalSet

-- | The globals of the first set that are not in the second
differenceGlobalSet :: GlobalSet -> GlobalSet -> GlobalSet
differenceGlobalSet (GlobalSet s1) (GlobalSet s2) = GlobalSet (UniqMap.difference s1 s2)

-- | The globals that satisfy a predicate
filterGlobalSet :: (GlobalId -> Bool) -> GlobalSet -> GlobalSet
filterGlobalSet f (GlobalSet s) = GlobalSet (UniqMap.filter f s)

-- | In the order of their uniques
globalSetToList :: GlobalSet -> [GlobalId]
globalSetToList (GlobalSet s) = UniqMap.elems s

-- | A set of the given globals
globalSetFromList :: [GlobalId] -> GlobalSet
globalSetFromList gs = GlobalSet (UniqMap.fromList [(globalIdUniq g, g) | g <- gs])

-- * Environments of global names

-- | A map from global names, e.g. of global bindings. Stores the keys, so that
-- 'globalEnvToList' can return them.
newtype GlobalEnv a = GlobalEnv (UniqMap (GlobalId, a))

instance Show a => Show (GlobalEnv a) where
  showsPrec d m = showParen (d > 10) $
    showString "globalEnvFromList " . shows (globalEnvToList m)

instance Functor GlobalEnv where
  fmap f (GlobalEnv m) = GlobalEnv (fmap (fmap f) m)

instance Foldable GlobalEnv where
  foldr f z (GlobalEnv m) = foldr (f . snd) z m
  length (GlobalEnv m) = length m
  null (GlobalEnv m) = UniqMap.null m

instance Traversable GlobalEnv where
  traverse f (GlobalEnv m) = GlobalEnv <$> traverse (traverse f) m

-- | Left-biased union
instance Semigroup (GlobalEnv a) where
  (<>) = unionGlobalEnv

instance Monoid (GlobalEnv a) where
  mempty = emptyGlobalEnv

-- | The empty environment
emptyGlobalEnv :: GlobalEnv a
emptyGlobalEnv = GlobalEnv UniqMap.empty

-- | An environment of one global
singletonGlobalEnv :: GlobalId -> a -> GlobalEnv a
singletonGlobalEnv g a = GlobalEnv (UniqMap.singleton (globalIdUniq g) (g, a))

-- | Add an entry, replacing the entry of the global if there is one
insertGlobalEnv :: GlobalId -> a -> GlobalEnv a -> GlobalEnv a
insertGlobalEnv g a (GlobalEnv m) = GlobalEnv (UniqMap.insert (globalIdUniq g) (g, a) m)

-- | @insertWithGlobalEnv f g new m@ inserts @f new old@ if @g@ is already in
-- @m@
insertWithGlobalEnv :: (a -> a -> a) -> GlobalId -> a -> GlobalEnv a -> GlobalEnv a
insertWithGlobalEnv f g a (GlobalEnv m) =
  GlobalEnv (UniqMap.insertWith (\(_, new) (gOld, old) -> (gOld, f new old)) (globalIdUniq g) (g, a) m)

-- | Remove the entry of a global
deleteGlobalEnv :: GlobalId -> GlobalEnv a -> GlobalEnv a
deleteGlobalEnv g (GlobalEnv m) = GlobalEnv (UniqMap.delete (globalIdUniq g) m)

-- | Apply a function to the entry of a global, if it has one
adjustGlobalEnv :: (a -> a) -> GlobalId -> GlobalEnv a -> GlobalEnv a
adjustGlobalEnv f g = alterGlobalEnv (fmap f) g

-- | Insert, update or delete the entry of a global
alterGlobalEnv :: (Maybe a -> Maybe a) -> GlobalId -> GlobalEnv a -> GlobalEnv a
alterGlobalEnv f g m = case f (lookupGlobalEnv g m) of
  Nothing -> deleteGlobalEnv g m
  Just a -> insertGlobalEnv g a m

-- | The entry of a global
lookupGlobalEnv :: GlobalId -> GlobalEnv a -> Maybe a
lookupGlobalEnv g (GlobalEnv m) = snd <$> UniqMap.lookup (globalIdUniq g) m

-- | Whether a global has an entry
memberGlobalEnv :: GlobalId -> GlobalEnv a -> Bool
memberGlobalEnv g (GlobalEnv m) = UniqMap.elem (globalIdUniq g) m

-- | Whether the environment is empty
nullGlobalEnv :: GlobalEnv a -> Bool
nullGlobalEnv (GlobalEnv m) = UniqMap.null m

-- | The number of entries
sizeGlobalEnv :: GlobalEnv a -> Int
sizeGlobalEnv (GlobalEnv m) = length m

-- | Left-biased union
unionGlobalEnv :: GlobalEnv a -> GlobalEnv a -> GlobalEnv a
unionGlobalEnv (GlobalEnv m1) (GlobalEnv m2) = GlobalEnv (UniqMap.unionWith const m1 m2)

-- | Union, combining the entries of globals in both environments
unionWithGlobalEnv :: (a -> a -> a) -> GlobalEnv a -> GlobalEnv a -> GlobalEnv a
unionWithGlobalEnv f (GlobalEnv m1) (GlobalEnv m2) =
  GlobalEnv (UniqMap.unionWith (\(g, a1) (_, a2) -> (g, f a1 a2)) m1 m2)

-- | The entries of the first environment whose key is not in the second
differenceGlobalEnv :: GlobalEnv a -> GlobalEnv a -> GlobalEnv a
differenceGlobalEnv (GlobalEnv m1) (GlobalEnv m2) = GlobalEnv (UniqMap.difference m1 m2)

-- | The entries that satisfy a predicate
filterGlobalEnv :: (a -> Bool) -> GlobalEnv a -> GlobalEnv a
filterGlobalEnv f = filterWithGlobalEnv (const f)

-- | The entries that satisfy a predicate on the global and the entry
filterWithGlobalEnv :: (GlobalId -> a -> Bool) -> GlobalEnv a -> GlobalEnv a
filterWithGlobalEnv f (GlobalEnv m) = GlobalEnv (UniqMap.filter (uncurry f) m)

-- | Apply a function to every entry and its global
mapWithGlobalEnv :: (GlobalId -> a -> b) -> GlobalEnv a -> GlobalEnv b
mapWithGlobalEnv f (GlobalEnv m) = GlobalEnv (fmap (\(g, a) -> (g, f g a)) m)

-- | In the order of the uniques of the keys
foldrWithGlobalEnv :: (GlobalId -> a -> b -> b) -> b -> GlobalEnv a -> b
foldrWithGlobalEnv f z (GlobalEnv m) = foldr (uncurry f) z m

-- | The entries whose key is in the set
restrictGlobalEnv :: GlobalEnv a -> GlobalSet -> GlobalEnv a
restrictGlobalEnv m s = filterWithGlobalEnv (\g _ -> memberGlobalSet g s) m

-- | The entries whose key is not in the set
withoutGlobalEnv :: GlobalEnv a -> GlobalSet -> GlobalEnv a
withoutGlobalEnv m s = filterWithGlobalEnv (\g _ -> not (memberGlobalSet g s)) m

-- | In the order of the uniques of the keys
globalEnvToList :: GlobalEnv a -> [(GlobalId, a)]
globalEnvToList (GlobalEnv m) = UniqMap.elems m

-- | An environment of the given entries. Later entries win.
globalEnvFromList :: [(GlobalId, a)] -> GlobalEnv a
globalEnvFromList gas = GlobalEnv (UniqMap.fromList [(globalIdUniq g, ga) | ga@(g, _) <- gas])

-- | In the order of their uniques
globalEnvKeys :: GlobalEnv a -> [GlobalId]
globalEnvKeys (GlobalEnv m) = map fst (UniqMap.elems m)

-- | The globals that have an entry
globalEnvKeysSet :: GlobalEnv a -> GlobalSet
globalEnvKeysSet (GlobalEnv m) = GlobalSet (fmap fst m)

-- | In the order of the uniques of their keys
globalEnvElems :: GlobalEnv a -> [a]
globalEnvElems (GlobalEnv m) = map snd (UniqMap.elems m)
