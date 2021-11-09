{-|
  Copyright   :  (C) 2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.SrcLoc.Extra where

import Data.Binary
import Data.Hashable                        (Hashable (..))
import GHC.Generics
#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.SrcLoc
  (SrcSpan (..), RealSrcLoc, RealSrcSpan, BufSpan (..), BufPos (..), UnhelpfulSpanReason (..),
   mkRealSrcLoc, mkRealSrcSpan,
   realSrcSpanStart, realSrcSpanEnd,
   srcLocFile, srcLocLine, srcLocCol,
   srcSpanFile, srcSpanStartLine, srcSpanEndLine, srcSpanStartCol, srcSpanEndCol)
import GHC.Data.FastString (FastString (..), bytesFS, mkFastStringByteList)
#else
import SrcLoc
  (SrcSpan (..), RealSrcLoc, RealSrcSpan,
   mkRealSrcLoc, mkRealSrcSpan,
   realSrcSpanStart, realSrcSpanEnd,
   srcLocFile, srcLocLine, srcLocCol,
   srcSpanFile, srcSpanStartLine, srcSpanEndLine, srcSpanStartCol, srcSpanEndCol)
import FastString                           (FastString (..), bytesFS, mkFastStringByteList)
#endif


#if MIN_VERSION_ghc(9,0,0)
deriving instance Ord SrcSpan
deriving instance Ord UnhelpfulSpanReason
#endif

deriving instance Generic SrcSpan
instance Hashable SrcSpan

instance Hashable RealSrcSpan where
  hashWithSalt salt rss =
    hashWithSalt salt (srcSpanFile rss,srcSpanStartLine rss, srcSpanEndLine rss
                      ,srcSpanStartCol rss, srcSpanEndCol rss)

instance Hashable FastString where
  hashWithSalt salt fs = hashWithSalt salt (uniq fs)

instance Binary SrcSpan
instance Binary RealSrcSpan where
  put r = put (realSrcSpanStart r, realSrcSpanEnd r)
  get = uncurry mkRealSrcSpan <$> get

instance Binary RealSrcLoc where
  put r = put (srcLocFile r, srcLocLine r, srcLocCol r)
  get = (\(file,line,col) -> mkRealSrcLoc file line col) <$> get

instance Binary FastString where
  put str = put $ bytesFS str
  get = mkFastStringByteList <$> get

#if MIN_VERSION_ghc(9,0,0)
deriving instance Generic BufPos
instance Binary BufPos
instance Hashable BufPos

deriving instance Generic UnhelpfulSpanReason
instance Binary UnhelpfulSpanReason
instance Hashable UnhelpfulSpanReason

deriving instance Generic BufSpan
instance Binary BufSpan
instance Hashable BufSpan
#endif
