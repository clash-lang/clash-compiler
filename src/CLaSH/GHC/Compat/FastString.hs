{-# LANGUAGE CPP #-}
module CLaSH.GHC.Compat.FastString
  ( unpackFS
  )
where

import qualified FastString

#if __GLASGOW_HASKELL__ >= 707
unpackFS :: FastString.FastBytes -> String
unpackFS = FastString.unpackFS
         . FastString.mkFastStringByteList
         . FastString.bytesFB
#else
unpackFS :: FastString.FastString -> String
unpackFS = FastString.unpackFS
#endif
