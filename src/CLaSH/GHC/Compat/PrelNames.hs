{-# LANGUAGE CPP #-}
module CLaSH.GHC.Compat.PrelNames
  ( tySuperKindTyConKey
  )
where

import qualified Unique
import qualified PrelNames

tySuperKindTyConKey :: Unique.Unique
#if __GLASGOW_HASKELL__ >= 706
tySuperKindTyConKey = PrelNames.superKindTyConKey
#else
tySuperKindTyConKey = PrelNames.tySuperKindTyConKey
#endif
