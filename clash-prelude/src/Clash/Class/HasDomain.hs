{-# OPTIONS_GHC -Wno-deprecations #-}

module Clash.Class.HasDomain {-# DEPRECATED "Experimental feature multiple hidden has been removed. This module will therefore be removed in Clash 1.12." #-}
  ( WithSpecificDomain
  , WithSingleDomain

  , HasDomain
  , TryDomain
  , TryDomainResult(..)
  , DomEq
  ) where

-- Compilation is split across modules to maximize GHC parallelism
import Clash.Class.HasDomain.HasSingleDomain
import Clash.Class.HasDomain.HasSpecificDomain
