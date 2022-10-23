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
import           GHC.Generics                           (Generic)
import           GHC.SrcLoc.Extra                       ()
#if MIN_VERSION_ghc(9,0,0)
import           GHC.Types.SrcLoc                       (SrcSpan, noSrcSpan)
#else
import           SrcLoc                                 (SrcSpan, noSrcSpan)
#endif

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
