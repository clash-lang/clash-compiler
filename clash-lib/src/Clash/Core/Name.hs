{-|
  Copyright   :  (C) 2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Names
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Core.Name
  ( module Clash.Core.Name
  , noSrcSpan
  )
where

import           Control.DeepSeq                        (NFData)
import           Data.Binary                            (Binary)
import           Data.Function                          (on)
import           Data.Hashable                          (Hashable (..))
import           Data.Text                              (Text, append)
import           GHC.BasicTypes.Extra                   ()
import           GHC.Generics                           (Generic)
import           GHC.SrcLoc.Extra                       ()
import           GHC.Types.SrcLoc
  (SrcSpan, leftmost_smallest, noSrcSpan)

import           Clash.Unique

data Name a
  = Name
  { nameSort :: NameSort
  , nameOcc  :: !OccName
  , nameUniq :: {-# UNPACK #-} !Unique
  , nameLoc  :: !SrcSpan
  }
  deriving (Show,Generic,NFData,Binary)

-- | N.B.: Equality checking only compares uniques, which only identify a name
-- within one scope. If you want structural equality, use `eqName`.
instance Eq (Name a) where
  (==) = (==) `on` nameUniq
  (/=) = (/=) `on` nameUniq

-- | N.B.: Comparison only looks at uniques, which only identify a name within
-- one scope. If you want structural comparison, use `ordName`.
instance Ord (Name a) where
  compare = compare `on` nameUniq

-- | Structural equality on 'Name's. See 'ordName'.
eqName :: Name a -> Name a -> Bool
eqName n1 n2 = ordName n1 n2 == EQ

-- | Structural comparison on 'Name's: on top of the 'Ord' instance, which only
-- compares uniques, this compares every other field too.
--
-- 'SrcSpan's are compared with 'leftmost_smallest', which treats all unhelpful
-- spans alike, matching how they are hashed in "Clash.Core.Subst".
ordName :: Name a -> Name a -> Ordering
ordName n1 n2 =
  compare (nameUniq n1) (nameUniq n2)
    <> compare (nameSort n1) (nameSort n2)
    <> compare (nameOcc n1) (nameOcc n2)
    <> leftmost_smallest (nameLoc n1) (nameLoc n2)

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

mkUnsafeName
  :: NameSort
  -> Text
  -> Unique
  -> Name a
mkUnsafeName ns s i = Name ns s i noSrcSpan

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
