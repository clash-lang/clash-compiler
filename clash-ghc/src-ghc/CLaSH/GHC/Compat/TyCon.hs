{-# LANGUAGE CPP #-}
module CLaSH.GHC.Compat.TyCon
  (isSuperKindTyCon
  )
where

import qualified TyCon
#if __GLASGOW_HASKELL__ >= 706
import qualified Kind
#endif

isSuperKindTyCon :: TyCon.TyCon -> Bool
#if __GLASGOW_HASKELL__ >= 706
isSuperKindTyCon = Kind.isSuperKindTyCon
#else
isSuperKindTyCon = TyCon.isSuperKindTyCon
#endif
