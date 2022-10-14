{-
Copyright   : (C) 2013-2016 University of Twente,
                  2016-2017 Myrtle Software Ltd,
                  2017      QbayLogic, Google Inc.
                  2020-2022 QBayLogic
License     : BSD2 (see the file LICENSE)
Maintainer  : QbayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Clash.Core.Binding
  ( Binding(..)
  , BindingMap
  , IsPrim(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import GHC.Generics (Generic)

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.Basic (InlineSpec)
import GHC.Types.SrcLoc (SrcSpan)
#else
import BasicTypes (InlineSpec)
import SrcLoc (SrcSpan)
#endif

import Clash.Core.Var (Id)
import Clash.Core.VarEnv (VarEnv)

data IsPrim
  = IsPrim
    -- ^ The binding is the unfolding for a primitive.
  | IsFun
    -- ^ The binding is an ordinary function.
  deriving (Binary, Eq, Generic, NFData, Show)

-- A function binder in the global environment.
--
data Binding a = Binding
  { bindingId :: Id
    -- ^ The core identifier for this binding.
  , bindingLoc :: SrcSpan
    -- ^ The source location of this binding in the original source code.
  , bindingSpec :: InlineSpec
    -- ^ the inline specification for this binding, in the original source code.
  , bindingIsPrim :: IsPrim
    -- ^ Is the binding a core term corresponding to a primitive with a known
    -- implementation? If so, it can potentially be inlined despite being
    -- marked as NOINLINE in source.
  , bindingTerm :: a
    -- ^ The term representation for this binding. This is polymorphic so
    -- alternate representations can be used if more appropriate (i.e. in the
    -- evaluator this can be Value for evaluated bindings).
  , bindingRecursive :: Bool
    -- ^ Whether the binding is recursive.
    --
    -- TODO Ideally the BindingMap would store recursive and non-recursive
    -- bindings in a way similar to Let / Letrec. GHC also does this.
  } deriving (Binary, Functor, Generic, NFData, Show)

type BindingMap a = VarEnv (Binding a)
