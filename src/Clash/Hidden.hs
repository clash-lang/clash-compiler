{-|
Copyright  :  (C) 2018     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Hidden arguments
-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
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

import qualified GHC.Classes
import GHC.TypeLits
import Unsafe.Coerce

type Hidden (x :: Symbol) a = GHC.Classes.IP x a

newtype Secret x a r = Secret (Hidden x a => r)

expose :: forall x a r . (Hidden x a => r) -> a -> r
expose k = unsafeCoerce (Secret @x @a @r k)
{-# INLINE expose #-}


fromLabel :: forall x a . Hidden x a => a
fromLabel = GHC.Classes.ip @x
{-# INLINE fromLabel #-}
