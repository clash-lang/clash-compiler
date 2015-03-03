{-# LANGUAGE CPP #-}
module CLaSH.GHC.Compat.FastString
  ( unpackFS
  , unpackFB
  )
where

import qualified FastString

#if __GLASGOW_HASKELL__ >= 707
import           Data.ByteString
import           Data.ByteString.Char8 as Char8 (unpack)
#endif

#if __GLASGOW_HASKELL__ == 706
unpackFS :: FastString.FastBytes -> String
unpackFS = FastString.unpackFS
         . FastString.mkFastStringByteList
         . FastString.bytesFB
#endif

#if __GLASGOW_HASKELL__ >= 707
unpackFS :: FastString.FastString -> String
unpackFS = FastString.unpackFS

unpackFB :: ByteString -> String
unpackFB = Char8.unpack
#endif
