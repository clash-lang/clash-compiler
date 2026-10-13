{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The types that Netlist generation on scoped Clash Core
  ("Clash.Core.Scoped.Syntax") shares with its handlers and with blackbox
  functions.

  = Component contexts

  Netlist generation opens the normalized term of a component into a single
  scope: its arguments and its let-binders (see
  'Clash.Netlist.Scoped.genComponentT'). A t'CompCtx' records, for every
  local variable of that scope, its HDL identifier: the name it has in the
  HDL. Handlers that go under further binders, e.g. those of a case
  alternative, extend the context with the HDL identifiers of the variables
  they bind ('extendCompCtx').

  The identifier lives in the context, keyed by the scoped name of the
  variable ('localIdentifier'), not in the occurrence name of the variable.

  = Blackbox functions

  A blackbox function on scoped Core ('BlackBoxFunction') gets the context of
  the component, as a read-only 'BlackBoxEnv': the scope of its arguments,
  which it needs to e.g. look through type synonyms with
  'Clash.Core.Scoped.Type.coreView', the type environment, and the HDL
  identifiers of the local variables its arguments mention.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Netlist.Scoped.Types
  ( -- * Component contexts
    CompCtx
  , emptyCompCtx
  , compScope
  , compTypeEnv
  , localIdentifier
  , localIdentifierMaybe
  , extendCompCtx
  , compCtxFromList
    -- ** The view of blackbox functions
  , BlackBoxEnv
  , bbScope
  , bbTypeEnv
  , bbLocalIdentifier
    -- * Local variables
  , LocalId (..)
  , localIdVar
  , binderLocalId
  , NetlistId (..)
  , netlistId1
  , netlistTypes
  , netlistTypes1
    -- * Blackbox functions
  , BlackBoxFunction
  , SomeBlackBoxFunction (..)
  , callBlackBoxFunction
  ) where

import           Control.DeepSeq             (NFData (..), rwhnf)
import           Data.Text                   (Text)
import           GHC.Stack                   (HasCallStack)

import qualified Clash.Core.Name             as C

import           Clash.Core.Scoped.NameMap
  (NameMap, emptyNameMap, insertNameMap, lookupNameMap, sinkNameMap)
import           Clash.Core.Scoped.Syntax
import           Clash.Core.Scoped.TypeEnv   (TypeEnv)

import           Clash.Netlist.BlackBox.Types (BlackBoxMeta)
import {-# SOURCE #-} Clash.Netlist.Types    (BlackBox, Identifier, NetlistMonad)

-- * Component contexts

-- | The context of the terms of a component, in scope @n@: the HDL identifiers
-- of its local variables. See the module header.
data CompCtx (n :: S) = CompCtx
  { _ccScope :: !(Scope n)
  , _ccTypeEnv :: !TypeEnv
  , _ccNames :: !(NameMap TmName n Identifier)
  -- ^ The HDL identifiers of the local variables
  }

-- | The context of closed terms
emptyCompCtx :: TypeEnv -> CompCtx 'VoidS
emptyCompCtx tenv = CompCtx emptyScope tenv emptyNameMap

-- | The scope of a context
compScope :: CompCtx n -> Scope n
compScope = _ccScope

-- | The type environment of a context
compTypeEnv :: CompCtx n -> TypeEnv
compTypeEnv = _ccTypeEnv

-- | The HDL identifier of a local variable. Errors if the context doesn't
-- give it one: every variable of the component has one, except those bound
-- under binders that the handler that opened them didn't name.
localIdentifier :: HasCallStack => CompCtx n -> TmName n -> Identifier
localIdentifier ctx nm = case localIdentifierMaybe ctx nm of
  Just i -> i
  Nothing -> error ("localIdentifier: no HDL identifier for local variable " <> show (tmNameRaw nm))

-- | The HDL identifier of a local variable, if the context gives it one
localIdentifierMaybe :: CompCtx n -> TmName n -> Maybe Identifier
localIdentifierMaybe ctx nm = lookupNameMap nm (_ccNames ctx)

-- | Extend a context to an extended scope, e.g. one with the variables bound
-- by let-bindings, with the HDL identifiers of the given variables of that
-- scope
extendCompCtx :: DExt n l => Scope l -> [(LocalId l, Identifier)] -> CompCtx n -> CompCtx l
extendCompCtx sc vs (CompCtx _ tenv names) =
  CompCtx sc tenv (foldl (\m (v, i) -> insertNameMap (localIdName v) i m) (sinkNameMap names) vs)

-- | The context of the given local variables, with their HDL identifiers. The
-- first identifier of a variable wins.
compCtxFromList :: Scope n -> TypeEnv -> [(LocalId n, Identifier)] -> CompCtx n
compCtxFromList sc tenv vs =
  CompCtx sc tenv (foldr (\(v, i) -> insertNameMap (localIdName v) i) emptyNameMap vs)

-- * The view of blackbox functions

-- | The context of the arguments of a blackbox function: see the module
-- header. Read-only.
type BlackBoxEnv = CompCtx

-- | The scope of the arguments of a blackbox function
bbScope :: BlackBoxEnv n -> Scope n
bbScope = compScope

-- | The type environment
bbTypeEnv :: BlackBoxEnv n -> TypeEnv
bbTypeEnv = compTypeEnv

-- | The HDL identifier of a local variable, see 'localIdentifier'
bbLocalIdentifier :: HasCallStack => BlackBoxEnv n -> TmName n -> Identifier
bbLocalIdentifier = localIdentifier

-- * Local variables

-- | A local variable of scope @n@: its human readable name, type and name
data LocalId (n :: S) = LocalId
  { localIdOcc :: !(C.Name Term)
  -- ^ Human readable name. Its unique is not used.
  , localIdType :: Type n
  , localIdName :: !(TmName n)
  }
  deriving (Show)

-- | By name
instance Eq (LocalId n) where
  a == b = localIdName a == localIdName b

instance Sinkable LocalId where
  sinkabilityProof r (LocalId o ty n) = LocalId o (sinkabilityProof r ty) (sinkabilityProof r n)

-- | A local variable as a term
localIdVar :: LocalId n -> Term n
localIdVar (LocalId o ty n) = Var o ty n

-- | The variable that a binder binds
binderLocalId :: DExt n l => IdBinder n l -> LocalId l
binderLocalId b = LocalId (idBinderOcc b) (sink (idBinderType b)) (tmNameOf b)

-- | Netlist-level identifier, see 'Clash.Netlist.Types.NetlistId'
data NetlistId (n :: S)
  = NetlistId Identifier (Type n)
  -- ^ Identifier generated in the NetlistMonad, always derived from another
  -- 'NetlistId'
  | CoreId (LocalId n)
  -- ^ A local variable of the component
  | MultiId [LocalId n]
  -- ^ A split identifier (into several sub-identifiers), needed to assign
  -- expressions of types that have to be split apart (e.g. tuples of Files)
  deriving (Show)

instance Sinkable NetlistId where
  sinkabilityProof r = \case
    NetlistId i t -> NetlistId i (sinkabilityProof r t)
    CoreId i -> CoreId (sinkabilityProof r i)
    MultiId is -> MultiId (map (sinkabilityProof r) is)

-- | Eliminator for 'NetlistId', fails on 'MultiId'
netlistId1 :: HasCallStack => (Identifier -> r) -> (LocalId n -> r) -> NetlistId n -> r
netlistId1 f g = \case
  NetlistId i _ -> f i
  CoreId i -> g i
  MultiId _ -> error "netlistId1 MultiId"

-- | The type(s) of a 'NetlistId': several for a 'MultiId'
netlistTypes :: NetlistId n -> [Type n]
netlistTypes = \case
  NetlistId _ t -> [t]
  CoreId i -> [localIdType i]
  MultiId is -> map localIdType is

-- | The type of a 'NetlistId', fails on 'MultiId'
netlistTypes1 :: HasCallStack => NetlistId n -> Type n
netlistTypes1 = \case
  NetlistId _ t -> t
  CoreId i -> localIdType i
  MultiId _ -> error "netlistTypes1 MultiId"

-- * Blackbox functions

-- | Call a blackbox function
callBlackBoxFunction
  :: (HasCallStack, Distinct n)
  => BlackBoxEnv n
  -> SomeBlackBoxFunction
  -> Bool
  -> Text
  -> [Either (Term n) (Type n)]
  -> [Type n]
  -> NetlistMonad (Either String (BlackBoxMeta, BlackBox))
callBlackBoxFunction ctx (ScopedBBF f) = f ctx

-- | A blackbox function on scoped Clash Core: given the arguments and result
-- types of the primitive it should provide a blackbox for, it generates the
-- blackbox.
type BlackBoxFunction =
     forall n
   . Distinct n
  => BlackBoxEnv n
  -- ^ The context of the arguments, see the module header
  -> Bool
  -- ^ Indicates whether caller needs a declaration. If set, the function is
  -- still free to return an expression, but the caller will convert it to a
  -- declaration.
  -> Text
  -- ^ Name of primitive
  -> [Either (Term n) (Type n)]
  -- ^ Arguments
  -> [Type n]
  -- ^ Result types
  -> NetlistMonad (Either String (BlackBoxMeta, BlackBox))

-- | A blackbox function. A wrapper, because the rank-2 'BlackBoxFunction'
-- can't instantiate a type parameter (e.g. of
-- 'Clash.Primitives.Types.Primitive'), nor be the result of interpreting a
-- function with Hint.
newtype SomeBlackBoxFunction = ScopedBBF BlackBoxFunction

-- | To weak head normal form, like functions
instance NFData SomeBlackBoxFunction where
  rnf = rwhnf
