{-# LANGUAGE CPP #-}
module CLaSH.GHC.Compat.FastString
  ( unpackFS
  , unpackFB
  )
where

import qualified FastString

#if __GLASGOW_HASKELL__ == 706
unpackFS :: FastString.FastBytes -> String
unpackFS = FastString.unpackFS
         . FastString.mkFastStringByteList
         . FastString.bytesFB
#else
unpackFS :: FastString.FastString -> String
unpackFS = FastString.unpackFS

unpackFB :: FastString.FastBytes -> String
unpackFB = FastString.unpackFS
         . FastString.mkFastStringByteList
         . FastString.bytesFB
#endif
