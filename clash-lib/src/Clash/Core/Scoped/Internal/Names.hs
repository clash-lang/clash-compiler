{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Sorted local names, and sets and maps of them. The public interface is
  "Clash.Core.Scoped.Syntax" and "Clash.Core.Scoped.NameMap".

  Type variables and term variables live in a single scope index @n :: S@,
  just like Clash's 'Clash.Core.VarEnv.InScopeSet' contains both. The newtypes
  'TyName' and 'TmName' keep the sorts apart. A 'TyName' can only be obtained
  from a type variable binder, a 'TmName' only from a term variable binder (see
  "Clash.Core.Scoped.Syntax"), so a type never mentions a term variable.

  In a scope, a raw name identifies at most one variable that can be referred
  to: binders in distinct scopes are distinct, and a binder that shadows a
  name of a scope that isn't distinct makes that name inaccessible (see
  'Clash.Core.Scoped.Internal.Scope.extendRenamingNameBinder'). So sets and
  maps keyed by the raw names of a scope are sound, whichever sorts they mix.

  = Trusted code

  This module is part of the trusted core of "Clash.Core.Scoped". Users must
  not import it. Its unsafe operations, the invariants they rely on, and why
  these hold:

  * __'UnsafeTyName', 'UnsafeTmName'__: only applied to the name of a type
    respectively term variable binder ('tyNameBinderName',
    'tmNameBinderName'), and in 'unsafeScopedName'.

  * __'UnsafeTyNameBinder', 'UnsafeTmNameBinder'__: the trusted modules only
    apply them to a /fresh/ binder, e.g. one 'withFresh' produced, which has
    no sort yet. Wrapping a binder that is already bound in another sort would
    give one name both sorts: e.g. a type variable that a term binder binds,
    which 'Clash.Core.Scoped.Subst.strengthenTyId' would then wrongly
    move out of its scope.

  * __'extendRenamingTyNameBinder', 'extendRenamingTmNameBinder'__: like
    'extendRenamingNameBinder', and keep the sort of the binder.

  * __'unsafeScopedName'__: only applied to raw names taken from a 'NameSet'
    or 'NameMap' of the same sort and scope. These only contain such names:
    the only ways to add a name to them take a name of that sort and scope.

  * __'sinkNameSet', 'sinkNameMap'__ ('unsafeCoerce'): sinking a set or map
    of names of @n@ into an extension @l@ of @n@ is renaming its keys by the
    injection of names of @n@ into names of @l@, i.e. keeping them as they are.

  * __'unsinkNameSet', 'unsinkNameSetBinder', 'unsinkNameMap'__ (their
    constructors at another scope): after removing the names bound between
    scopes @n@ and @l@ from a set or map of names of @l@, only names of @n@
    remain.

  * __@UnsafeNameSet@, @UnsafeNameMap@, @UnsafeNameBinders@__: the
    constructors are not exported. The functions that build sets and maps take
    the names of their sort and scope, except for the ones above.
    'emptyNameBinders' and 'snocNameBinders' only add names of binders.
    'appendNameBinders' takes the union of the names bound between scopes
    @n@ and @i@, and between @i@ and @l@, which are the names bound between
    @n@ and @l@.
-}

{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

module Clash.Core.Scoped.Internal.Names
  ( -- * Sorted names
    TyName (UnsafeTyName)
  , TmName (UnsafeTmName)
  , tyNameRaw
  , tmNameRaw
  , ScopedName (..)
    -- * Sorted binders
  , TyNameBinder (UnsafeTyNameBinder)
  , TmNameBinder (UnsafeTmNameBinder)
  , tyNameBinderName
  , tmNameBinderName
  , tyNameBinderRaw
  , tmNameBinderRaw
  , unsortTyNameBinder
  , unsortTmNameBinder
  , extendRenamingTyNameBinder
  , extendRenamingTmNameBinder
    -- * Sets of local names
  , NameSet
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
  , unsinkNameSetBinder
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
  , nameBindersRaw
  ) where

import Control.DeepSeq (NFData (..), rwhnf)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerce)

import Clash.Core.Scoped.Internal.Scope

-- * Sorted names

-- | A type variable in scope @n@
newtype TyName (n :: S) = UnsafeTyName (Name n)
  deriving (Eq, Ord, Show)
type role TyName nominal

-- | A local term variable in scope @n@
newtype TmName (n :: S) = UnsafeTmName (Name n)
  deriving (Eq, Ord, Show)
type role TmName nominal

-- | A name is a strict 'Int'
instance NFData (TyName n) where
  rnf = rwhnf

-- | A name is a strict 'Int'
instance NFData (TmName n) where
  rnf = rwhnf

instance Sinkable TyName where
  sinkabilityProof r (UnsafeTyName n) = UnsafeTyName (r n)

instance Sinkable TmName where
  sinkabilityProof r (UnsafeTmName n) = UnsafeTmName (r n)

-- | The raw name of a type variable, for printing, maps and conversion
tyNameRaw :: TyName n -> Int
tyNameRaw (UnsafeTyName n) = nameId n
{-# INLINE tyNameRaw #-}

-- | The raw name of a term variable, for printing, maps and conversion
tmNameRaw :: TmName n -> Int
tmNameRaw (UnsafeTmName n) = nameId n
{-# INLINE tmNameRaw #-}

-- * Sorted binders

-- | A binder of a type variable, that extends scope @n@ to scope @l@
newtype TyNameBinder (n :: S) (l :: S) = UnsafeTyNameBinder (NameBinder n l)
  deriving (Eq, Ord, Show)
type role TyNameBinder nominal nominal

-- | A binder of a term variable, that extends scope @n@ to scope @l@
newtype TmNameBinder (n :: S) (l :: S) = UnsafeTmNameBinder (NameBinder n l)
  deriving (Eq, Ord, Show)
type role TmNameBinder nominal nominal

-- | A binder is a strict 'Int'
instance NFData (TyNameBinder n l) where
  rnf = rwhnf

-- | A binder is a strict 'Int'
instance NFData (TmNameBinder n l) where
  rnf = rwhnf

-- | The type variable a binder binds
tyNameBinderName :: TyNameBinder n l -> TyName l
tyNameBinderName (UnsafeTyNameBinder b) = UnsafeTyName (nameOf b)
{-# INLINE tyNameBinderName #-}

-- | The term variable a binder binds
tmNameBinderName :: TmNameBinder n l -> TmName l
tmNameBinderName (UnsafeTmNameBinder b) = UnsafeTmName (nameOf b)
{-# INLINE tmNameBinderName #-}

-- | The raw name of the type variable a binder binds
tyNameBinderRaw :: TyNameBinder n l -> Int
tyNameBinderRaw (UnsafeTyNameBinder b) = nameId (nameOf b)
{-# INLINE tyNameBinderRaw #-}

-- | The raw name of the term variable a binder binds
tmNameBinderRaw :: TmNameBinder n l -> Int
tmNameBinderRaw (UnsafeTmNameBinder b) = nameId (nameOf b)
{-# INLINE tmNameBinderRaw #-}

-- | Forget the sort of a binder
unsortTyNameBinder :: TyNameBinder n l -> NameBinder n l
unsortTyNameBinder (UnsafeTyNameBinder b) = b
{-# INLINE unsortTyNameBinder #-}

-- | Forget the sort of a binder
unsortTmNameBinder :: TmNameBinder n l -> NameBinder n l
unsortTmNameBinder (UnsafeTmNameBinder b) = b
{-# INLINE unsortTmNameBinder #-}

-- | 'extendRenamingNameBinder' for a type variable binder
extendRenamingTyNameBinder
  :: (Name n -> Name n')
  -> TyNameBinder n l
  -> (forall l'. (Name l -> Name l') -> TyNameBinder n' l' -> r)
  -> r
extendRenamingTyNameBinder r (UnsafeTyNameBinder b) cont =
  extendRenamingNameBinder r b (\r' b' -> cont r' (UnsafeTyNameBinder b'))
{-# INLINE extendRenamingTyNameBinder #-}

-- | 'extendRenamingNameBinder' for a term variable binder
extendRenamingTmNameBinder
  :: (Name n -> Name n')
  -> TmNameBinder n l
  -> (forall l'. (Name l -> Name l') -> TmNameBinder n' l' -> r)
  -> r
extendRenamingTmNameBinder r (UnsafeTmNameBinder b) cont =
  extendRenamingNameBinder r b (\r' b' -> cont r' (UnsafeTmNameBinder b'))
{-# INLINE extendRenamingTmNameBinder #-}

-- | The sorts of local names: 'TyName' and 'TmName'
class ScopedName (k :: S -> Type) where
  -- | The raw name, for printing, maps and conversion
  scopedNameRaw :: k n -> Int
  -- | Trusted: see the module header
  unsafeScopedName :: Int -> k n

instance ScopedName TyName where
  scopedNameRaw = tyNameRaw
  unsafeScopedName = UnsafeTyName . UnsafeName
  {-# INLINE scopedNameRaw #-}
  {-# INLINE unsafeScopedName #-}

instance ScopedName TmName where
  scopedNameRaw = tmNameRaw
  unsafeScopedName = UnsafeTmName . UnsafeName
  {-# INLINE scopedNameRaw #-}
  {-# INLINE unsafeScopedName #-}

-- * Sets of local names

-- | A set of local names of sort @k@ ('TyName' or 'TmName') in scope @n@
newtype NameSet (k :: S -> Type) (n :: S) = UnsafeNameSet IntSet
  deriving (Eq)
type role NameSet nominal nominal

instance ScopedName k => Show (NameSet k n) where
  showsPrec d s = showParen (d > 10) $
    showString "nameSetFromList " . shows (map scopedNameRaw (nameSetToList s))

instance Semigroup (NameSet k n) where
  (<>) = unionNameSet

instance Monoid (NameSet k n) where
  mempty = emptyNameSet

-- | The empty set
emptyNameSet :: NameSet k n
emptyNameSet = UnsafeNameSet IntSet.empty

-- | A set of one name
singletonNameSet :: ScopedName k => k n -> NameSet k n
singletonNameSet k = UnsafeNameSet (IntSet.singleton (scopedNameRaw k))

-- | Add a name
insertNameSet :: ScopedName k => k n -> NameSet k n -> NameSet k n
insertNameSet k (UnsafeNameSet s) = UnsafeNameSet (IntSet.insert (scopedNameRaw k) s)

-- | Remove a name
deleteNameSet :: ScopedName k => k n -> NameSet k n -> NameSet k n
deleteNameSet k (UnsafeNameSet s) = UnsafeNameSet (IntSet.delete (scopedNameRaw k) s)

-- | Whether a name is in the set
memberNameSet :: ScopedName k => k n -> NameSet k n -> Bool
memberNameSet k (UnsafeNameSet s) = IntSet.member (scopedNameRaw k) s
{-# INLINE memberNameSet #-}

-- | Whether the set is empty
nullNameSet :: NameSet k n -> Bool
nullNameSet (UnsafeNameSet s) = IntSet.null s

-- | The number of names in the set
sizeNameSet :: NameSet k n -> Int
sizeNameSet (UnsafeNameSet s) = IntSet.size s

-- | The union of two sets
unionNameSet :: NameSet k n -> NameSet k n -> NameSet k n
unionNameSet (UnsafeNameSet s1) (UnsafeNameSet s2) = UnsafeNameSet (IntSet.union s1 s2)

-- | The union of a list of sets
unionsNameSet :: [NameSet k n] -> NameSet k n
unionsNameSet = List.foldl' unionNameSet emptyNameSet

-- | The names of the first set that are not in the second
differenceNameSet :: NameSet k n -> NameSet k n -> NameSet k n
differenceNameSet (UnsafeNameSet s1) (UnsafeNameSet s2) =
  UnsafeNameSet (IntSet.difference s1 s2)

-- | The names in the set, in ascending order of their raw names
nameSetToList :: ScopedName k => NameSet k n -> [k n]
nameSetToList (UnsafeNameSet s) = map unsafeScopedName (IntSet.toList s)

-- | A set of the given names
nameSetFromList :: ScopedName k => [k n] -> NameSet k n
nameSetFromList = UnsafeNameSet . IntSet.fromList . map scopedNameRaw

-- | Move a set of names into an extended scope. A coercion, see the module
-- header.
sinkNameSet :: DExt n l => NameSet k n -> NameSet k l
sinkNameSet = unsafeCoerce
{-# INLINE sinkNameSet #-}

-- | Remove the names bound between scopes @n@ and @l@, moving the set into
-- scope @n@. Trusted, see the module header.
unsinkNameSet :: NameBinders n l -> NameSet k l -> NameSet k n
unsinkNameSet (UnsafeNameBinders bs) (UnsafeNameSet s) =
  UnsafeNameSet (IntSet.difference s bs)

-- | Remove the name a binder binds, moving the set into the scope the binder
-- extends. Trusted, see the module header.
unsinkNameSetBinder :: NameBinder n l -> NameSet k l -> NameSet k n
unsinkNameSetBinder b (UnsafeNameSet s) = UnsafeNameSet (IntSet.delete (nameId (nameOf b)) s)
{-# INLINE unsinkNameSetBinder #-}

-- * Maps from local names

-- | A map from local names of sort @k@ ('TyName' or 'TmName') in scope @n@
newtype NameMap (k :: S -> Type) (n :: S) a = UnsafeNameMap (IntMap a)
  deriving (Functor, Foldable, Traversable)
type role NameMap nominal nominal representational

instance (ScopedName k, Show a) => Show (NameMap k n a) where
  showsPrec d m = showParen (d > 10) $
    showString "nameMapFromList "
      . shows [(scopedNameRaw k, a) | (k, a) <- nameMapToList m]

-- | The empty map
emptyNameMap :: NameMap k n a
emptyNameMap = UnsafeNameMap IntMap.empty

-- | A map of one name
singletonNameMap :: ScopedName k => k n -> a -> NameMap k n a
singletonNameMap k a = UnsafeNameMap (IntMap.singleton (scopedNameRaw k) a)

-- | Add an entry, replacing the entry of the name if there is one
insertNameMap :: ScopedName k => k n -> a -> NameMap k n a -> NameMap k n a
insertNameMap k a (UnsafeNameMap m) = UnsafeNameMap (IntMap.insert (scopedNameRaw k) a m)

-- | @insertWithNameMap f k new m@ adds @f new old@ if @k@ is already in @m@
insertWithNameMap
  :: ScopedName k => (a -> a -> a) -> k n -> a -> NameMap k n a -> NameMap k n a
insertWithNameMap f k a (UnsafeNameMap m) =
  UnsafeNameMap (IntMap.insertWith f (scopedNameRaw k) a m)

-- | Remove the entry of a name
deleteNameMap :: ScopedName k => k n -> NameMap k n a -> NameMap k n a
deleteNameMap k (UnsafeNameMap m) = UnsafeNameMap (IntMap.delete (scopedNameRaw k) m)

-- | The entry of a name
lookupNameMap :: ScopedName k => k n -> NameMap k n a -> Maybe a
lookupNameMap k (UnsafeNameMap m) = IntMap.lookup (scopedNameRaw k) m
{-# INLINE lookupNameMap #-}

-- | Whether a name has an entry
memberNameMap :: ScopedName k => k n -> NameMap k n a -> Bool
memberNameMap k (UnsafeNameMap m) = IntMap.member (scopedNameRaw k) m

-- | Whether the map is empty
nullNameMap :: NameMap k n a -> Bool
nullNameMap (UnsafeNameMap m) = IntMap.null m

-- | The number of entries
sizeNameMap :: NameMap k n a -> Int
sizeNameMap (UnsafeNameMap m) = IntMap.size m

-- | Left-biased union
unionNameMap :: NameMap k n a -> NameMap k n a -> NameMap k n a
unionNameMap (UnsafeNameMap m1) (UnsafeNameMap m2) = UnsafeNameMap (IntMap.union m1 m2)

-- | Union, combining the entries of names in both maps
unionWithNameMap :: (a -> a -> a) -> NameMap k n a -> NameMap k n a -> NameMap k n a
unionWithNameMap f (UnsafeNameMap m1) (UnsafeNameMap m2) =
  UnsafeNameMap (IntMap.unionWith f m1 m2)

-- | The entries, in ascending order of the raw names
nameMapToList :: ScopedName k => NameMap k n a -> [(k n, a)]
nameMapToList (UnsafeNameMap m) = [(unsafeScopedName k, a) | (k, a) <- IntMap.toList m]

-- | A map of the given entries. Later entries win.
nameMapFromList :: ScopedName k => [(k n, a)] -> NameMap k n a
nameMapFromList kas = UnsafeNameMap (IntMap.fromList [(scopedNameRaw k, a) | (k, a) <- kas])

-- | The names that have an entry, in ascending order of their raw names
nameMapKeys :: ScopedName k => NameMap k n a -> [k n]
nameMapKeys (UnsafeNameMap m) = map unsafeScopedName (IntMap.keys m)

-- | The entries, in ascending order of the raw names of their names
nameMapElems :: NameMap k n a -> [a]
nameMapElems (UnsafeNameMap m) = IntMap.elems m

-- | Move a map of names into an extended scope. A coercion, see the module
-- header. Note that this only moves the keys: values that are themselves in
-- scope @n@ have to be sunk separately.
sinkNameMap :: DExt n l => NameMap k n a -> NameMap k l a
sinkNameMap = unsafeCoerce
{-# INLINE sinkNameMap #-}

-- | Remove the names bound between scopes @n@ and @l@, moving the map into
-- scope @n@. Trusted, see the module header. Note that this only moves the
-- keys.
unsinkNameMap :: NameBinders n l -> NameMap k l a -> NameMap k n a
unsinkNameMap (UnsafeNameBinders bs) (UnsafeNameMap m) =
  UnsafeNameMap (IntMap.withoutKeys m bs)

-- * The names bound between two scopes

-- | The names bound between scope @n@ and its extension @l@, e.g. by a
-- pattern
newtype NameBinders (n :: S) (l :: S) = UnsafeNameBinders IntSet
type role NameBinders nominal nominal

-- | No names are bound between a scope and itself
emptyNameBinders :: NameBinders n n
emptyNameBinders = UnsafeNameBinders IntSet.empty

-- | Add a binder that extends scope @i@ to @l@
snocNameBinders :: NameBinders n i -> NameBinder i l -> NameBinders n l
snocNameBinders (UnsafeNameBinders bs) b = UnsafeNameBinders (IntSet.insert (nameId (nameOf b)) bs)
{-# INLINE snocNameBinders #-}

-- | The names bound between scopes @n@ and @i@, and between @i@ and @l@
appendNameBinders :: NameBinders n i -> NameBinders i l -> NameBinders n l
appendNameBinders (UnsafeNameBinders bs1) (UnsafeNameBinders bs2) =
  UnsafeNameBinders (IntSet.union bs1 bs2)
{-# INLINE appendNameBinders #-}

-- | The raw names of the binders
nameBindersRaw :: NameBinders n l -> IntSet
nameBindersRaw (UnsafeNameBinders bs) = bs
{-# INLINE nameBindersRaw #-}
