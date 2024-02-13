{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Monad for random generation of clash-core types.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Clash.Hedgehog.Core.Monad
  ( CoreGenT
  , runCoreGenT
  , CoreGenConfig(..)
  , defaultConfig
  , canGenDataKinds
  , canGenPolyKinds
  , canGenRankNTypes
  , canGenTypeFamilies
  , canGenUndecidableInstances

    -- * Re-exports
  , Alternative(..)
  , MonadGen(..)
  , MonadReader(..)
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(..), ReaderT, runReaderT)
import Control.Monad.Trans (MonadTrans)
import Hedgehog (MonadGen(..))

-- | The CoreGenT monad keeps track of features like language extensions which
-- have an impact on what can be generated. This allows more meaningful random
-- generation, as the output of generators can be constrained to the same
-- variant of Haskell / Clash used by the caller.
--
newtype CoreGenT m a
  = CoreGenT (ReaderT CoreGenConfig m a)
  deriving newtype
    ( Alternative
    , Applicative
    , Functor
    , Monad
    , MonadFail
    , MonadGen
    , MonadIO
    , MonadReader CoreGenConfig
    , MonadTrans
    )

-- | Run a generator that generates types from @clash-lib@. This is intended
-- to transform another monad which implements 'MonadGen'.
--
runCoreGenT :: CoreGenT m a -> CoreGenConfig -> m a
runCoreGenT (CoreGenT act) = runReaderT act

-- | The configuration of Haskell / Clash which the generated source adheres
-- to. These are typically things which change what a user could potentially
-- have written in a source file, such as language extensions.
--
data CoreGenConfig = CoreGenConfig
  { allowDataKinds :: Bool
  , allowPolyKinds :: Bool
  , allowRankNTypes :: Bool
  , allowTypeFamilies :: Bool
  , allowUndecidableInstances :: Bool
  } deriving stock (Show)

-- | The default configuration matches the set of language extensions which
-- are enabled by default when running @clash@ / @clashi@. For most projects,
-- this will likely be the most representative set of options.
--
defaultConfig :: CoreGenConfig
defaultConfig = CoreGenConfig
  { allowDataKinds = True
  , allowPolyKinds = False
  , allowRankNTypes = False
  , allowTypeFamilies = True
  , allowUndecidableInstances = False
  }

canGenDataKinds :: forall m. Monad m => CoreGenT m Bool
canGenDataKinds = reader allowDataKinds

canGenPolyKinds :: forall m. Monad m => CoreGenT m Bool
canGenPolyKinds = reader allowPolyKinds

canGenRankNTypes :: forall m. Monad m => CoreGenT m Bool
canGenRankNTypes = reader allowRankNTypes

canGenTypeFamilies :: forall m. Monad m => CoreGenT m Bool
canGenTypeFamilies = reader allowTypeFamilies

canGenUndecidableInstances :: forall m. Monad m => CoreGenT m Bool
canGenUndecidableInstances = reader allowUndecidableInstances
