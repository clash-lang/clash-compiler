module Clash.Class.HasDomain
  ( WithSpecificDomain
  , WithSingleDomain
  ) where

-- Compilation is split across modules to maximize GHC parallelism
import Clash.Class.HasDomain.HasSingleDomain
import Clash.Class.HasDomain.HasSpecificDomain
