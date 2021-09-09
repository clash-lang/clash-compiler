{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.Utils.Misc.Extra where

import Data.Hashable

#if MIN_VERSION_ghc(9,0,0)
import GHC.Utils.Misc (OverridingBool(..))
#else
import Util (OverridingBool(..))
#endif

instance Eq OverridingBool where
  Auto == Auto = True
  Always == Always = True
  Never == Never = True
  _ == _ = False

instance Hashable OverridingBool where
  hashWithSalt salt x =
    case x of
      Auto -> hashWithSalt salt (0 :: Int)
      Always -> hashWithSalt salt (1 :: Int)
      Never -> hashWithSalt salt (2 :: Int)
