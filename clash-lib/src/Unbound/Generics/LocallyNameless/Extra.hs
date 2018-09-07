{-|
  Copyright   :  (C) 2015-2016, University of Twente,
                          2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unbound.Generics.LocallyNameless.Extra where

#ifndef MIN_VERSION_unbound_generics
#error MIN_VERSION_unbound_generics undefined
#endif

#if !MIN_VERSION_unbound_generics(0,2,0)
import Control.DeepSeq
#endif
import Data.Binary
import Data.Vector.Primitive
import Data.Hashable                           (Hashable(..),hash)
#if MIN_VERSION_unbound_generics(0,3,0)
import Data.Monoid
#endif
import Data.Text                               (Text)
import GHC.Real                                (Ratio)
#if MIN_VERSION_unbound_generics(0,3,0)
import Unbound.Generics.LocallyNameless.Alpha  (Alpha (..), NamePatFind (..),
                                                NthPatFind (..))
#else
import Unbound.Generics.LocallyNameless.Alpha  (Alpha (..))
#endif
import Unbound.Generics.LocallyNameless.Bind   (Bind (..))
import Unbound.Generics.LocallyNameless.Embed  (Embed (..))
import Unbound.Generics.LocallyNameless.Name   (Name (..))
#if !MIN_VERSION_unbound_generics(0,2,0)
import Unbound.Generics.LocallyNameless.Rec    (unrec)
#endif
import Unbound.Generics.LocallyNameless.Rebind (Rebind (..))
import Unbound.Generics.LocallyNameless.Rec    (Rec)
import Unbound.Generics.LocallyNameless.Subst  (Subst (..))

#if !MIN_VERSION_unbound_generics(0,2,0)
instance (NFData a, NFData b) => NFData (Bind a b)
instance NFData a => NFData (Embed a)
instance NFData (Name a)
instance (NFData a, NFData b) => NFData (Rebind a b)
instance (Alpha a, NFData a) => NFData (Rec a) where
  rnf r = seq (rnf (unrec r)) ()
#endif

instance Subst b (Ratio a) where
  subst  _ _ = id
  substs _   = id

instance Hashable (Name a) where
  hashWithSalt salt (Fn str int) = hashWithSalt salt (hashWithSalt (hash int) str)
  hashWithSalt salt (Bn i0  i1)  = hashWithSalt salt (hash i0 `hashWithSalt` i1)

instance (Hashable a, Hashable b) => Hashable (Bind a b)
instance Hashable a => Hashable (Embed a)
instance (Hashable a, Hashable b) => Hashable (Rebind a b)
instance Hashable a => Hashable (Rec a)

#if !MIN_VERSION_unbound_generics(0,2,0)
instance (Ord a) => Ord (Embed a) where
  compare (Embed a) (Embed b) = compare a b
#endif

instance (Prim a, Ord a, Show a) => Alpha (Vector a) where
  aeq' _ctx             = (==)
  fvAny' _ctx _nfn i    = pure i
  close _ctx _b         = id
  open _ctx _b          = id
  isPat _               = mempty
#if MIN_VERSION_unbound_generics(0,3,0)
  isTerm _              = All True
  nthPatFind _          = NthPatFind Left
  namePatFind _         = NamePatFind (const (Left 0))
#else
  isTerm _              = True
  nthPatFind _          = Left
  namePatFind _ _       = Left 0
#endif
  swaps' _ctx _p        = id
  freshen' _ctx i       = return (i, mempty)
  lfreshen' _ctx i cont = cont i mempty
  acompare' _ctx        = compare

instance Alpha Text where
  aeq' _ctx             = (==)
  fvAny' _ctx _nfn i    = pure i
  close _ctx _b         = id
  open _ctx _b          = id
  isPat _               = mempty
#if MIN_VERSION_unbound_generics(0,3,0)
  isTerm _              = All True
  nthPatFind _          = NthPatFind Left
  namePatFind _         = NamePatFind (const (Left 0))
#else
  isTerm _              = True
  nthPatFind _          = Left
  namePatFind _ _       = Left 0
#endif
  swaps' _ctx _p        = id
  freshen' _ctx i       = return (i, mempty)
  lfreshen' _ctx i cont = cont i mempty
  acompare' _ctx        = compare

instance Subst b Text where
  subst  _ _ = id
  substs _   = id

instance (Binary a, Binary b) => Binary (Bind a b)
instance Binary a => Binary (Embed a)
instance Binary (Name a)
instance (Binary a, Binary b) => Binary (Rebind a b)
instance Binary a => Binary (Rec a)
