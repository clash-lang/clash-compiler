{-# LANGUAGE CPP #-}
module CLaSH.GHC.Compat.Outputable
  ( showPpr )
where

#if __GLASGOW_HASKELL__ >= 706
import qualified DynFlags (tracingDynFlags)
import qualified Outputable (Outputable,showPpr)
#else
import qualified Outputable (Outputable,showPpr)
#endif

showPpr :: (Outputable.Outputable a) => a -> String
#if __GLASGOW_HASKELL__ >= 706
showPpr = Outputable.showPpr DynFlags.tracingDynFlags
#else
showPpr = Outputable.showPpr
#endif
