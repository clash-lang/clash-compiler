{-# LANGUAGE CPP #-}
module CLaSH.GHC.Compat.Outputable
  ( showPpr )
where

#if __GLASGOW_HASKELL__ >= 707
import qualified StaticFlags (unsafeGlobalDynFlags)
#elif __GLASGOW_HASKELL__ >= 706
import qualified DynFlags (tracingDynFlags)
#endif
import qualified Outputable (Outputable,showPpr)

showPpr :: (Outputable.Outputable a) => a -> String
#if __GLASGOW_HASKELL__ >= 707
showPpr = Outputable.showPpr StaticFlags.unsafeGlobalDynFlags
#elif __GLASGOW_HASKELL__ >= 706
showPpr = Outputable.showPpr DynFlags.tracingDynFlags
#else
showPpr = Outputable.showPpr
#endif
