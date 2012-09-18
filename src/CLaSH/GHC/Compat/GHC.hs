{-# LANGUAGE CPP #-}
module CLaSH.GHC.Compat.GHC
  ( defaultErrorHandler
  )
where

import qualified DynFlags
import qualified Exception
import qualified GHC
import qualified MonadUtils

defaultErrorHandler ::
  (Exception.ExceptionMonad m, MonadUtils.MonadIO m)
  => m a
  -> m a
#if __GLASGOW_HASKELL__ >= 706
defaultErrorHandler = GHC.defaultErrorHandler DynFlags.defaultFatalMessager DynFlags.defaultFlushOut
#else
defaultErrorHandler = GHC.defaultErrorHandler DynFlags.defaultLogAction
#endif
