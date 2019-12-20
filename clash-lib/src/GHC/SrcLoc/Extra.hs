{-|
  Copyright   :  (C) 2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.SrcLoc.Extra where

import Data.Binary
import Data.Hashable                        (Hashable (..))
import GHC.Generics
import SrcLoc
  (SrcSpan (..), RealSrcLoc, RealSrcSpan,
   mkRealSrcLoc, mkRealSrcSpan,
   realSrcSpanStart, realSrcSpanEnd,
   srcLocFile, srcLocLine, srcLocCol,
   srcSpanFile, srcSpanStartLine, srcSpanEndLine, srcSpanStartCol, srcSpanEndCol)
import FastString                           (FastString (..), bytesFS, mkFastStringByteList)

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
