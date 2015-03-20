{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unbound.Generics.LocallyNameless.Extra where

import Control.DeepSeq
import Data.Hashable                           (Hashable(..),hash)
import Data.Text                               (Text)
import GHC.Real                                (Ratio)
import Unbound.Generics.LocallyNameless.Alpha  (Alpha (..))
import Unbound.Generics.LocallyNameless.Bind   (Bind (..))
import Unbound.Generics.LocallyNameless.Embed  (Embed (..))
import Unbound.Generics.LocallyNameless.Name   (Name (..))
import Unbound.Generics.LocallyNameless.Rebind (Rebind (..))
import Unbound.Generics.LocallyNameless.Rec    (Rec,unrec)
import Unbound.Generics.LocallyNameless.Subst  (Subst (..))

instance (NFData a, NFData b) => NFData (Bind a b)
instance NFData a => NFData (Embed a)
instance NFData (Name a)
instance (NFData a, NFData b) => NFData (Rebind a b)
instance (Alpha a, NFData a) => NFData (Rec a) where
  rnf r = seq (rnf (unrec r)) ()

instance Subst b (Ratio a) where
  subst  _ _ = id
  substs _   = id

instance Hashable (Name a) where
  hashWithSalt salt (Fn str int) = hashWithSalt salt (hashWithSalt (hash int) str)
  hashWithSalt salt (Bn i0  i1)  = hashWithSalt salt (hash i0 `hashWithSalt` i1)

instance (Ord a) => Ord (Embed a) where
  compare (Embed a) (Embed b) = compare a b

instance Alpha Text where
  aeq' _ctx             = (==)
  fvAny' _ctx _nfn i    = pure i
  close _ctx _b         = id
  open _ctx _b          = id
  isPat _               = mempty
  isTerm _              = True
  nthPatFind _          = Left
  namePatFind _ _       = Left 0
  swaps' _ctx _p        = id
  freshen' _ctx i       = return (i, mempty)
  lfreshen' _ctx i cont = cont i mempty
  acompare' _ctx        = compare

instance Subst b Text where
  subst  _ _ = id
  substs _   = id
