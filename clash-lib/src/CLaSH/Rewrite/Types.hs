{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | Type and instance definitions for Rewrite modules
module CLaSH.Rewrite.Types where

import Control.Concurrent.Supply             (Supply, freshId)
import Control.Lens                          (use, (.=), (<<%=))
import Control.Monad.Reader                  (MonadReader (..))
import Control.Monad
import Control.Monad.State                   (MonadState (..))
import Control.Monad.Writer                  (MonadWriter (..))
import Data.HashMap.Strict                   (HashMap)
import Data.IntMap.Strict                    (IntMap)
import Data.Monoid                           (Any)
import Unbound.Generics.LocallyNameless      (Fresh (..))
import Unbound.Generics.LocallyNameless.Name (Name (..))

import CLaSH.Core.Term           (Term, TmName)
import CLaSH.Core.Type           (Type)
import CLaSH.Core.TyCon          (TyCon, TyConName)
import CLaSH.Core.Var            (Id, TyVar)
import CLaSH.Netlist.Types       (HWType)
import CLaSH.Util

-- | Context in which a term appears
data CoreContext
  = AppFun           -- ^ Function position of an application
  | AppArg           -- ^ Argument position of an application
  | TyAppC           -- ^ Function position of a type application
  | LetBinding [Id]  -- ^ RHS of a Let-binder with the sibling LHS'
  | LetBody    [Id]  -- ^ Body of a Let-binding with the bound LHS'
  | LamBody    Id    -- ^ Body of a lambda-term with the abstracted variable
  | TyLamBody  TyVar -- ^ Body of a TyLambda-term with the abstracted
                     -- type-variable
  | CaseAlt    [Id]  -- ^ RHS of a case-alternative with the variables bound by
                     -- the pattern on the LHS
  | CaseScrut        -- ^ Subject of a case-decomposition
  deriving (Eq,Show)

-- | State of a rewriting session
data RewriteState extra
  = RewriteState
  { _transformCounter :: {-# UNPACK #-} !Int
  -- ^ Number of applied transformations
  , _bindings         :: !(HashMap TmName (Type,Term))
  -- ^ Global binders
  , _uniqSupply       :: !Supply
  -- ^ Supply of unique numbers
  , _curFun           :: TmName -- Initially set to undefined: no strictness annotation
  -- ^ Function which is currently normalized
  , _nameCounter      :: {-# UNPACK #-} !Int
  -- ^ Used for 'Fresh'
  , _extra            :: !extra
  -- ^ Additional state
  }

makeLenses ''RewriteState

-- | Debug Message Verbosity
data DebugLevel
  = DebugNone    -- ^ Don't show debug messages
  | DebugFinal   -- ^ Show completely normalized expressions
  | DebugName    -- ^ Names of applied transformations
  | DebugApplied -- ^ Show sub-expressions after a successful rewrite
  | DebugAll     -- ^ Show all sub-expressions on which a rewrite is attempted
  deriving (Eq,Ord,Read)

-- | Read-only environment of a rewriting session
data RewriteEnv
  = RewriteEnv
  { _dbgLevel       :: DebugLevel
  -- ^ Lvl at which we print debugging messages
  , _typeTranslator :: HashMap TyConName TyCon -> Type
                    -> Maybe (Either String HWType)
  -- ^ Hardcode Type -> HWType translator
  , _tcCache        :: HashMap TyConName TyCon
  -- ^ TyCon cache
  , _tupleTcCache   :: IntMap TyConName
  -- ^ Tuple TyCon cache
  , _evaluator      :: HashMap TyConName TyCon -> Bool -> Term -> Term
  -- ^ Hardcoded evaluator (delta-reduction)}
  }

makeLenses ''RewriteEnv

-- | Monad that keeps track how many transformations have been applied and can
-- generate fresh variables and unique identifiers. In addition, it keeps track
-- if a transformation/rewrite has been successfully applied.
newtype RewriteMonad extra a = R
  { runR :: RewriteEnv -> RewriteState extra -> (a,RewriteState extra,Any) }

instance Functor (RewriteMonad extra) where
  fmap f m = R (\r s -> case runR m r s of (a,s',w) -> (f a,s',w))

instance Applicative (RewriteMonad extra) where
  pure  = return
  (<*>) = ap

instance Monad (RewriteMonad extra) where
  return a = R (\_ s -> (a, s, mempty))
  m >>= k  = R (\r s -> case runR m r s of
                          (a,s',w) -> case runR (k a) r s' of
                                        (b,s'',w') -> let w'' = mappend w w'
                                                      in seq w'' (b,s'',w''))

instance MonadState (RewriteState extra) (RewriteMonad extra) where
  get     = R (\_ s -> (s,s,mempty))
  put s   = R (\_ _ -> ((),s,mempty))
  state f = R (\_ s -> case f s of (a,s') -> (a,s',mempty))

instance Fresh (RewriteMonad extra) where
  fresh (Fn s _) = do
    n <- nameCounter <<%= (+1)
    let n' = toInteger n
    n' `seq` return (Fn s n')
  fresh nm@(Bn {}) = return nm

instance MonadUnique (RewriteMonad extra) where
  getUniqueM = do
    sup <- use uniqSupply
    let (a,sup') = freshId sup
    uniqSupply .= sup'
    a `seq` return a

instance MonadWriter Any (RewriteMonad extra) where
  writer (a,w) = R (\_ s -> (a,s,w))
  tell   w     = R (\_ s -> ((),s,w))
  listen m     = R (\r s -> case runR m r s of (a,s',w) -> ((a,w),s',w))
  pass   m     = R (\r s -> case runR m r s of ((a,f),s',w) -> (a, s', f w))

instance MonadReader RewriteEnv (RewriteMonad extra) where
   ask       = R (\r s -> (r,s,mempty))
   local f m = R (\r s -> runR m (f r) s)
   reader f  = R (\r s -> (f r,s,mempty))

-- | Monadic action that transforms a term given a certain context
type Transform m = [CoreContext] -> Term -> m Term

-- | A 'Transform' action in the context of the 'RewriteMonad'
type Rewrite extra = Transform (RewriteMonad extra)
