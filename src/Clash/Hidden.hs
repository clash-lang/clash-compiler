{-|
Copyright  :  (C) 2018     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Hidden arguments
-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}

{-# LANGUAGE Trustworthy #-}

module Clash.Hidden
  ( Hidden
  , expose
  -- * OverloadedLabels
  , fromLabel
  )
where

import GHC.TypeLits
import Unsafe.Coerce

class Hidden (x :: Symbol) a | x -> a where
  hidden :: a

newtype Secret x a r = Secret (Hidden x a => r)

expose :: forall x a r . (Hidden x a => r) -> a -> r
expose k = unsafeCoerce (Secret @x @a @r k)
{-# INLINE expose #-}


fromLabel :: forall x a . Hidden x a => a
fromLabel = hidden @x
{-# INLINE fromLabel #-}
