{-|
Copyright  :  (C) 2024, Google LLC
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Tooling to safely work around @incomplete-uni-patterns@ and @incomplete-patterns@
warnings. See 'vecToTuple' for more information and examples.

Note: This module has been added to make upgrading to GHC 9.2 easier. As of GHC
      9.2, the @incomplete-uni-patterns@ has been added to the @-Wall@, making
      previously warning-free code now produce warnings.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- Purpose of this module
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- For debugging:
-- {-# OPTIONS_GHC -ddump-splices #-}

module Clash.Sized.Vector.ToTuple (VecToTuple(..)) where

import Clash.CPP
import Clash.Sized.Vector
import Clash.Sized.Vector.ToTuple.TH (vecToTupleInstances)

import Data.Tagged (Tagged(..))

#if MIN_VERSION_base(4,18,0)
import Data.Tuple (Solo(MkSolo))
#elif MIN_VERSION_base(4,16,0)
import Data.Tuple (Solo(Solo))
#endif

{- $setup
>>> :set -XMonoLocalBinds -XGADTs
>>> import Clash.Sized.Vector
-}

class VecToTuple a where
  type TupType a = r | r -> a

  -- | Given a vector with three elements:
  --
  -- >>> myVec = (1 :> 2 :> 3 :> Nil) :: Vec 3 Int
  --
  -- The following would produce a warning even though we can be sure
  -- no other pattern can ever apply:
  --
  -- >>> :{
  -- a, b, c :: Int
  -- (a :> b :> c :> Nil) = myVec
  -- :}
  --
  -- 'vecToTuple' can be used to work around the warning:
  --
  -- >>> (a, b, c) = vecToTuple myVec
  --
  -- Of course, you will still get an error if you try to match a vector of the
  -- wrong length:
  --
  -- >>> (a, b, c, d) = vecToTuple myVec
  -- ...
#if __GLASGOW_HASKELL__ > 900
  --     • Couldn't match type: (a, b, c, d)
  --                      with: (Int, Int, Int)
#else
  --     • Couldn't match type: (Int, Int, Int)
  --                             ^
  --                      with: (a, b, c, d)
#endif
  -- ...
  vecToTuple ::  a -> TupType a

instance VecToTuple (Vec 0 a) where
  type TupType (Vec 0 a) = Tagged a ()
  vecToTuple _ = Tagged ()

#if MIN_VERSION_base(4,18,0)
-- | Instead of using 'vecToTuple' for @Vec 1 _@, you could also consider using 'head'
instance VecToTuple (Vec 1 a) where
  type TupType (Vec 1 a) = Solo a
  vecToTuple (a1 :> _) = MkSolo a1
#elif MIN_VERSION_base(4,16,0)
-- | Instead of using 'vecToTuple' for @Vec 1 _@, you could also consider using 'head'
instance VecToTuple (Vec 1 a) where
  type TupType (Vec 1 a) = Solo a
  vecToTuple (a1 :> _) = Solo a1
#endif

-- | __NB__: The documentation only shows instances up to /3/-tuples. By
-- default, instances up to and including /12/-tuples will exist. If the flag
-- @large-tuples@ is set instances up to the GHC imposed limit will exist. The
-- GHC imposed limit is either 62 or 64 depending on the GHC version.
instance VecToTuple (Vec 2 a) where
  type TupType (Vec 2 a) = (a, a)
  vecToTuple (a1 :> a2 :> _) = (a1, a2)

vecToTupleInstances maxTupleSize
