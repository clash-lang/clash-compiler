{-|
Copyright  :  (C) 2023,      QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# OPTIONS_GHC "-Wno-orphans" #-}

{-# LANGUAGE CPP #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE Trustworthy #-}

module Clash.Class.IndexTuple (indexTuple) where

import Clash.Class.IndexTuple.Deriving

#if MIN_VERSION_ghc(9,0,0)
import GHC.Settings.Constants (mAX_TUPLE_SIZE)
#else
import Constants (mAX_TUPLE_SIZE)
#endif

deriveIndexTupleInstances mAX_TUPLE_SIZE
