{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module CLaSH.Rewrite.Types where

import Control.Concurrent.Supply (Supply,freshId)
import Control.Monad.Reader      (MonadTrans,MonadReader,ReaderT,lift)
import Control.Monad.State       (MonadState,StateT)
import Control.Monad.Writer      (MonadWriter,WriterT)
import Data.HashMap.Lazy         (HashMap)
import Data.Label.PureM          (gets,puts)
import Data.Monoid               (Any)
import Unbound.LocallyNameless   (Fresh,FreshMT)

import CLaSH.Core.Term (Term,TmName)
import CLaSH.Core.Type (Type)
import CLaSH.Core.Var  (Id,TyVar)
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

data RewriteState
  = RewriteState
  { _transformCounter :: Int
  , _bindings         :: HashMap TmName (Type,Term)
  , _uniqSupply       :: Supply
  }

mkLabels [''RewriteState]

data DebugLevel
  = DebugNone
  | DebugFinal
  | DebugApplied
  | DebugAll
  deriving (Eq,Ord)

newtype RewriteEnv = RE { _dbgLevel :: DebugLevel }

mkLabels [''RewriteEnv]

type RewriteSession m = ReaderT RewriteEnv (StateT RewriteState (FreshMT m))

type RewriteMonad m = WriterT Any (RewriteSession m)

instance Monad m => MonadUnique (RewriteMonad m) where
  getUniqueM = do
    sup <- lift . lift $ gets uniqSupply
    let (a,sup') = freshId sup
    lift . lift $ puts uniqSupply sup'
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

type Rewrite m = [CoreContext] -> Term -> R m Term
