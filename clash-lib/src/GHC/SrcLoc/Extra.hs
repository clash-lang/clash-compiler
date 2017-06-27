{-|
  Copyright   :  (C) 2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.SrcLoc.Extra where

import Data.Hashable                        (Hashable (..))
import GHC.Generics
import SrcLoc
  (SrcSpan (..), RealSrcSpan, srcSpanFile, srcSpanStartLine, srcSpanEndLine,
   srcSpanStartCol, srcSpanEndCol)
import FastString                           (FastString (..))
import Unbound.Generics.LocallyNameless     (Alpha (..))
import Unbound.Generics.LocallyNameless.TH

deriving instance Generic SrcSpan
instance Hashable SrcSpan

makeClosedAlpha ''SrcSpan

instance Hashable RealSrcSpan where
  hashWithSalt salt rss =
    hashWithSalt salt (srcSpanFile rss,srcSpanStartLine rss, srcSpanEndLine rss
                      ,srcSpanStartCol rss, srcSpanEndCol rss)

instance Hashable FastString where
  hashWithSalt salt fs = hashWithSalt salt (uniq fs)

