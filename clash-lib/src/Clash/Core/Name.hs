{-|
  Copyright   :  (C) 2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Names
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Core.Name
  ( module Clash.Core.Name
  , noSrcSpan
  )
where

import           Control.DeepSeq                        (NFData)
import           Data.Binary                            (Binary)
import           Data.Coerce                            (coerce)
import           Data.Function                          (on)
import           Data.GenericTrie.Internal              (TrieKey (..), Trie (..))
import           Data.Hashable                          (Hashable (..))
import           Data.IntMap                            (IntMap)
import qualified Data.IntMap                            as IntMap
import           Data.Text                              (Text, append)
import           GHC.BasicTypes.Extra                   ()
import           GHC.Generics                           (Generic)
import           GHC.SrcLoc.Extra                       ()
import           SrcLoc                                 (SrcSpan, noSrcSpan)

import           Clash.Unique

data Name a
  = Name
  { nameSort :: NameSort
  , nameOcc  :: !OccName
  , nameUniq :: {-# UNPACK #-} !Unique
  , nameLoc  :: !SrcSpan
  }
  deriving (Show,Generic,NFData,Binary)

instance Eq (Name a) where
  (==) = (==) `on` nameUniq
  (/=) = (/=) `on` nameUniq

instance TrieKey (Name a) where
  type TrieRep (Name a)   = IntMap
  trieLookup k (MkTrie x)       = IntMap.lookup (nameUniq k) x
  trieInsert k v (MkTrie t)     = MkTrie (IntMap.insert (nameUniq k) v t)
  trieDelete k (MkTrie t)       = MkTrie (IntMap.delete (nameUniq k) t)
  trieEmpty                     = MkTrie IntMap.empty
  trieSingleton k v             = MkTrie (IntMap.singleton (nameUniq k) v)
  trieNull (MkTrie x)           = IntMap.null x
  trieMap f (MkTrie x)          = MkTrie (IntMap.map f x)
  trieTraverse f (MkTrie x)     = fmap MkTrie (traverse f x)
  trieShowsPrec p (MkTrie x)    = showsPrec p x
  trieMapMaybeWithKey f (MkTrie x)  = MkTrie (IntMap.mapMaybeWithKey (withNameUniq f) x)
  trieFoldWithKey f z (MkTrie x)    = IntMap.foldrWithKey (withNameUniq f) z x
  trieTraverseWithKey f (MkTrie x)  = fmap MkTrie (IntMap.traverseWithKey (withNameUniq f) x)
  trieMergeWithKey f g h (MkTrie x) (MkTrie y) = MkTrie (IntMap.mergeWithKey (withNameUniq f) (coerce g) (coerce h) x y)
  {-# INLINABLE trieEmpty #-}
  {-# INLINABLE trieInsert #-}
  {-# INLINABLE trieLookup #-}
  {-# INLINABLE trieDelete #-}
  {-# INLINABLE trieSingleton #-}
  {-# INLINABLE trieFoldWithKey #-}
  {-# INLINABLE trieShowsPrec #-}
  {-# INLINABLE trieTraverse #-}
  {-# INLINABLE trieTraverseWithKey #-}
  {-# INLINABLE trieNull #-}
  {-# INLINABLE trieMap #-}
  {-# INLINABLE trieMergeWithKey #-}
  {-# INLINABLE trieMapMaybeWithKey #-}

withNameUniq
  :: (Name a -> r)
  -> (Unique -> r)
withNameUniq f k = f (Name Internal "" k noSrcSpan)
{-# INLINE withNameUniq #-}

instance Ord (Name a) where
  compare = compare `on` nameUniq

instance Hashable (Name a) where
  hashWithSalt salt nm = hashWithSalt salt (nameUniq nm)

instance Uniquable (Name a) where
  getUnique = nameUniq
  setUnique nm u = nm {nameUniq=u}

type OccName = Text

data NameSort
  = User
  | System
  | Internal
  deriving (Eq,Ord,Show,Generic,NFData,Hashable,Binary)

mkUnsafeSystemName
  :: Text
  -> Unique
  -> Name a
mkUnsafeSystemName s i = Name System s i noSrcSpan

mkUnsafeInternalName
  :: Text
  -> Unique
  -> Name a
mkUnsafeInternalName s i = Name Internal ("c$" `append` s) i noSrcSpan

appendToName :: Name a -> Text -> Name a
appendToName (Name sort nm uniq loc) s = Name Internal nm2 uniq loc
  where
    nm1 = case sort of {Internal -> nm; _ -> "c$" `append` nm}
    nm2 = nm1 `append` s
