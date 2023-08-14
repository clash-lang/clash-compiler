{-|
  Copyright   :  (C) 2017, Google Inc.
                     2023, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.BasicTypes.Extra where

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.Basic
#else
import BasicTypes
#endif
import Control.DeepSeq
import Data.Binary
import GHC.Generics

#if MIN_VERSION_ghc(9,4,0)
import GHC.Types.SourceText
#endif

#if MIN_VERSION_ghc(9,8,0)
import Data.ByteString
import GHC.Data.FastString
import Unsafe.Coerce
#endif

deriving instance Generic InlineSpec
instance NFData InlineSpec
instance Binary InlineSpec

#if MIN_VERSION_ghc(9,8,0)
deriving instance Generic FastString
instance Binary FastString
instance Binary FastZString where
  put = put . fastZStringToByteString
  get = unsafeCoerce (get :: Get ByteString)
#endif

#if MIN_VERSION_ghc(9,4,0)
deriving instance Generic SourceText
#if !MIN_VERSION_ghc(9,8,0)
instance NFData SourceText
#endif
instance Binary SourceText
#endif

-- | Determine whether given 'InlineSpec' is NOINLINE or more strict (OPAQUE)
isNoInline :: InlineSpec -> Bool
isNoInline NoInline{} = True
#if MIN_VERSION_ghc(9,4,0)
isNoInline Opaque{} = True
#endif
isNoInline _ = False

-- | Determine whether given 'InlineSpec' is OPAQUE. If this function is used on
-- a GHC that does not support OPAQUE yet (<9.4), it will return 'True' if given
-- 'InlineSpec' is NOINLINE instead.
isOpaque :: InlineSpec -> Bool
#if MIN_VERSION_ghc(9,4,0)
isOpaque Opaque{} = True
#else
isOpaque NoInline{} = True
#endif
isOpaque _ = False
