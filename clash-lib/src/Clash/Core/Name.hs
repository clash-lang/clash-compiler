{-|
  Copyright   :  (C) 2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Names
-}

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Clash.Core.Name
  ( module Clash.Core.Name
  , noSrcSpan
  )
where

import           Control.DeepSeq                        (NFData)
import           Data.Binary                            (Binary)
import           Data.Function                          (on)
import           Data.Hashable                          (Hashable)
import           Data.Typeable                          (Typeable)
import           GHC.BasicTypes.Extra                   ()
import           GHC.Generics                           (Generic)
import           GHC.SrcLoc.Extra                       ()
import           Unbound.Generics.LocallyNameless       hiding
  (Name, name2String, string2Name)
import qualified Unbound.Generics.LocallyNameless       as Unbound
import qualified Unbound.Generics.LocallyNameless.Name  as Unbound
import           Unbound.Generics.LocallyNameless.TH
import           Unbound.Generics.LocallyNameless.Extra ()
import           SrcLoc                                 (SrcSpan, noSrcSpan)

data Name a
  = Name
  { nameSort :: NameSort
  , nameOcc  :: OccName a
  , nameLoc  :: !SrcSpan
  }
  deriving (Show,Generic,NFData,Hashable,Binary)

instance Eq (Name a) where
  (==) = (==) `on` nameOcc

instance Ord (Name a) where
  compare = compare `on` nameOcc

type OccName a = Unbound.Name a

data NameSort
  = User
  | System
  | Internal
  deriving (Eq,Ord,Show,Generic,NFData,Hashable,Binary)

instance Typeable a => Alpha (Name a) where
  aeq'      ctx (Name _ nm1 _) (Name _ nm2 _) = aeq'      ctx nm1 nm2
  acompare' ctx (Name _ nm1 _) (Name _ nm2 _) = acompare' ctx nm1 nm2

makeClosedAlpha ''NameSort

instance Subst b (Name a) where subst _ _ = id; substs _ = id

name2String :: Name a -> String
name2String = Unbound.name2String . nameOcc
{-# INLINE name2String #-}

name2Integer :: Name a -> Integer
name2Integer = Unbound.name2Integer . nameOcc

string2OccName :: String -> OccName a
string2OccName = Unbound.string2Name
{-# INLINE string2OccName #-}

string2SystemName :: String -> Name a
string2SystemName nm = Name System (string2OccName nm) noSrcSpan

string2InternalName :: String -> Name a
string2InternalName nm = Name Internal (string2OccName ('#':nm)) noSrcSpan

makeOccName :: String -> Integer -> OccName a
makeOccName = Unbound.makeName

makeSystemName :: String -> Integer -> Name a
makeSystemName s i = Name System (makeOccName s i) noSrcSpan

coerceName :: Name a -> Name b
coerceName nm = nm {nameOcc = go (nameOcc nm)}
  where
    go (Unbound.Fn s i) = Unbound.Fn s i
    go _                = error "Trying to coerce bound name"

appendToName :: Name a -> String -> Name a
appendToName (Name sort nm loc) s = Name Internal nm' loc
  where
    n   = Unbound.name2String nm
    n'  = case sort of {Internal -> n; _ -> '#':n}
    nm' = Unbound.makeName (n' ++ s) (Unbound.name2Integer nm)
