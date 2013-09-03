{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module CLaSH.Rewrite.Types where

import Control.Concurrent.Supply (Supply, freshId)
import Control.Lens              (use, (.=))
import Control.Monad.Reader      (MonadReader, ReaderT, lift)
import Control.Monad.State       (MonadState, StateT)
import Control.Monad.Writer      (MonadWriter, WriterT)
import Data.HashMap.Lazy         (HashMap)
import Data.Monoid               (Any)
import Unbound.LocallyNameless   (Fresh, FreshMT)

import CLaSH.Core.Term           (Term, TmName)
import CLaSH.Core.Type           (Type)
import CLaSH.Core.Var            (Id, TyVar)
import CLaSH.Netlist.Types       (HWType)
import CLaSH.Util

-- | Context in which a term appears
data CoreContext = AppFun -- ^ Function position of an application
                 | AppArg -- ^ Argument position of an application
                 | TyAppC -- ^ Function position of a type application
                 | LetBinding [Id] -- ^ RHS of a Let-binder with the sibling LHS'
                 | LetBody    [Id] -- ^ Body of a Let-binding with the bound LHS'
                 | LamBody    Id   -- ^ Body of a lambda-term with the abstracted variable
                 | TyLamBody  TyVar -- ^ Body of a TyLambda-term with the abstracted type-variable
                 | CaseAlt    [Id] -- ^ RHS of a case-alternative with the variables bound by the pattern on the LHS
                 | CaseScrut -- ^ Subject of a case-decomposition
                 deriving Show

-- | State of a rewriting session
data RewriteState
  = RewriteState
  { _transformCounter :: Int -- ^ Number of applied transformations
  , _bindings         :: HashMap TmName (Type,Term) -- ^ Global binders
  , _uniqSupply       :: Supply -- ^ Supply of unique numbers
  , _typeTranslator   :: Type -> Maybe (Either String HWType) -- ^ Hardcode Type -> HWType translator
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
