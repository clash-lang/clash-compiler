{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module CLaSH.Rewrite.Types where

import Control.Concurrent.Supply (Supply,freshId)
import Control.Lens              (use,(.=))
import Control.Monad.Reader      (MonadReader,ReaderT,lift)
import Control.Monad.State       (MonadState,StateT)
import Control.Monad.Writer      (MonadWriter,WriterT)
import Data.HashMap.Lazy         (HashMap)
import Data.Monoid               (Any)
import Unbound.LocallyNameless   (Fresh,FreshMT)

import CLaSH.Core.Term (Term,TmName)
import CLaSH.Core.Type (Type)
import CLaSH.Core.Var  (Id,TyVar)
import CLaSH.Driver.Types
import CLaSH.Util

data CoreContext = AppFirst
                 | AppSecond
                 | TyAppC
                 | LetBinding [Id]
                 | LetBody    [Id]
                 | LamBody    Id
                 | TyLamBody  TyVar
                 | CaseAlt    [Id]
                 | CaseScrut
                 deriving Show

data RewriteState
  = RewriteState
  { _transformCounter :: Int
  , _bindings         :: HashMap TmName (Type,Term)
  , _dictFuns         :: DFunMap
  , _classOps         :: ClassOpMap
  , _uniqSupply       :: Supply
  }

makeLenses ''RewriteState

data DebugLevel
  = DebugNone
  | DebugFinal
  | DebugApplied
  | DebugAll
  deriving (Eq,Ord)

newtype RewriteEnv = RE { _dbgLevel :: DebugLevel }

makeLenses ''RewriteEnv

type RewriteSession m = ReaderT RewriteEnv (StateT RewriteState (FreshMT m))

type RewriteMonad m = WriterT Any (RewriteSession m)

instance Monad m => MonadUnique (RewriteMonad m) where
  getUniqueM = do
    sup <- lift . lift $ use uniqSupply
    let (a,sup') = freshId sup
    lift . lift $ uniqSupply .= sup'
    return a

newtype R m a = R { runR :: RewriteMonad m a }
  deriving ( Monad
           , Functor
           , MonadReader RewriteEnv
           , MonadState  RewriteState
           , MonadWriter Any
           , MonadUnique
           , Fresh
           )

type Transform m = [CoreContext] -> Term -> m Term
type Rewrite m   = Transform (R m)
