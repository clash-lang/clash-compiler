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
  they bind ('extendCompCtx', 'extendCompCtxTy').

  Clash Core put the HDL identifier of a variable in its occurrence name
  ('Clash.Netlist.Util.mkUnique'), and Netlist read it back with
  'Clash.Netlist.Id.unsafeFromCoreId'. Here, the identifier lives in the
  context, keyed by the scoped name of the variable ('localIdentifier').

  = Blackbox functions

  A blackbox function on scoped Core ('BlackBoxFunction') gets the context of
  the component, as a read-only 'BlackBoxEnv': the scope of its arguments,
  which it needs to e.g. look through type synonyms with
  'Clash.Core.Scoped.Type.coreView', the type environment, and the HDL
  identifiers of the local variables its arguments mention.

  Primitives refer to blackbox functions on Clash Core too, which
  'SomeBlackBoxFunction' tells apart from those on scoped Core.

  = The boundary with Clash Core

  During the port, Netlist handlers that are not ported yet run on Clash Core.
  'toLegacy' converts a term of a component into Clash Core for them: every
  local variable becomes a 'C.Id' whose occurrence name is its HDL identifier,
  which is what 'Clash.Netlist.Id.unsafeFromCoreId' reads. The variables of the
  context get uniques from a unique supply when they are added, and the
  binders in a converted term get them from the rest of that supply, see
  "Clash.Core.Scoped.Convert.Fresh". So a local never shares its unique with a
  global variable. TODO(Phase 7): remove the conversions.
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
  , extendCompCtxTy
    -- ** The view of blackbox functions
  , BlackBoxEnv
  , bbScope
  , bbTypeEnv
  , bbLocalIdentifier
    -- ** Clash Core counterparts of the context
  , compCtxFromList
  , legacyCompCtx
  , toLegacy
  , toLegacyArgs
  , toLegacyType
  , toLegacyTick
  , toLegacyVar
    -- * Local variables
  , LocalId (..)
  , localIdVar
  , binderLocalId
  , toLegacyLocalId
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
import           Control.Monad.State.Strict  (evalState)
import qualified Data.IntMap.Strict          as IntMap
import           Data.List                   (mapAccumL)
import           Data.Text                   (Text)
import           GHC.Stack                   (HasCallStack)

import qualified Clash.Core.Name             as C
import qualified Clash.Core.Term             as C
import qualified Clash.Core.Type             as C
import qualified Clash.Core.Var              as C
import           Clash.Core.VarEnv           (InScopeSet, emptyInScopeSet, extendInScopeSet)
import           Clash.Unique                (Unique)
import           Clash.Util.Supply           (Supply, freshId)

import           Clash.Core.Scoped.Convert.Fresh
  (BackEnv (..), emptyBackEnv, toClashFresh, toClashTickFresh, toClashTypeFresh)
import           Clash.Core.Scoped.NameMap
  (NameMap, emptyNameMap, insertNameMap, lookupNameMap, sinkNameMap)
import           Clash.Core.Scoped.Syntax
import           Clash.Core.Scoped.TypeEnv   (TypeEnv)

import qualified Clash.Netlist.BlackBox.Types as Legacy (BlackBoxFunction)
import           Clash.Netlist.BlackBox.Types (BlackBoxMeta)
import qualified Clash.Netlist.Id             as Id (toText)
import           Clash.Netlist.Types         (BlackBox, Identifier, NetlistMonad)

-- * Component contexts

-- | The context of the terms of a component, in scope @n@: the HDL identifiers
-- of its local variables. See the module header.
data CompCtx (n :: S) = CompCtx
  { _ccScope :: !(Scope n)
  , _ccTypeEnv :: !TypeEnv
  , _ccNames :: !(NameMap TmName n Identifier)
  -- ^ The HDL identifiers of the local variables
  , _ccBackEnv :: !BackEnv
  -- ^ The Clash Core counterparts of the local variables, see 'toLegacy'
  , _ccInScope :: !InScopeSet
  -- ^ The Clash Core counterparts of the local variables
  , _ccSupply :: !Supply
  -- ^ Uniques for the binders of 'toLegacy'
  }

-- | The context of closed terms. The supply gives the uniques of the Clash
-- Core counterparts of the variables, see 'toLegacy'.
emptyCompCtx :: TypeEnv -> Supply -> CompCtx 'VoidS
emptyCompCtx tenv =
  CompCtx emptyScope tenv emptyNameMap emptyBackEnv emptyInScopeSet

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

-- | Extend a context with local variables of an extended scope, e.g. those
-- bound by let-bindings, and their HDL identifiers. Their Clash Core
-- counterparts (see 'toLegacy') get the converted types of the 'LocalId's.
extendCompCtx :: DExt n l => Scope l -> [(LocalId l, Identifier)] -> CompCtx n -> CompCtx l
extendCompCtx sc vs (CompCtx _ tenv names0 env0@(BackEnv tvs ids0) is0 sup0) =
  go (sinkNameMap names0) ids0 is0 sup0 vs
 where
  go names ids is sup [] = CompCtx sc tenv names (BackEnv tvs ids) is sup
  go names ids is sup ((v, i) : rest) =
    let (u, sup') = freshId sup
        ty = evalState (toClashTypeFresh is0 env0 (localIdType v)) sup0
        cv = C.mkLocalId ty (legacyName (localIdOcc v) i u)
    in go (insertNameMap (localIdName v) i names)
          (IntMap.insert (tmNameRaw (localIdName v)) cv ids)
          (extendInScopeSet is cv) sup' rest

-- | Extend a context with type variables of an extended scope, e.g. those
-- bound by a pattern: their human readable names, kinds and names. Type
-- variables have no HDL identifiers, but their Clash Core counterparts (see
-- 'toLegacy') keep their human readable names.
extendCompCtxTy
  :: DExt n l => Scope l -> [(C.Name C.Type, Kind l, TyName l)] -> CompCtx n -> CompCtx l
extendCompCtxTy sc vs (CompCtx _ tenv names (BackEnv tvs0 ids) is0 sup0) =
  go tvs0 is0 sup0 vs
 where
  go tvs is sup [] = CompCtx sc tenv (sinkNameMap names) (BackEnv tvs ids) is sup
  go tvs is sup ((occ, k, nm) : rest) =
    let (u, sup') = freshId sup
        -- The kinds of later variables may mention earlier ones
        k' = evalState (toClashTypeFresh is (BackEnv tvs ids) k) sup0
        tv = C.mkTyVar k' occ {C.nameUniq = u}
    in go (IntMap.insert (tyNameRaw nm) tv tvs) (extendInScopeSet is tv) sup' rest

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

-- * Clash Core counterparts

-- | The context of the given local variables, with their HDL identifiers and
-- the types of their Clash Core counterparts (see 'toLegacy'), which get
-- fresh uniques from the supply
compCtxFromList
  :: Scope n
  -> TypeEnv
  -> Supply
  -> [(LocalId n, Identifier, C.Type)]
  -- ^ The variables, their HDL identifiers, and the types of their Clash Core
  -- counterparts
  -> CompCtx n
compCtxFromList sc tenv sup0 vs = legacyCompCtx sc tenv sup1 [] ids
 where
  (sup1, ids) = mapAccumL mk sup0 vs
  mk sup (v, i, ty) =
    let (u, sup') = freshId sup
    in (sup', (localIdName v, C.mkLocalId ty (legacyName (localIdOcc v) i u), i))

-- | The name of the Clash Core counterpart of a local variable: its
-- occurrence name is its HDL identifier. Keeps the sort and the source
-- location of the human readable name.
legacyName :: C.Name C.Term -> Identifier -> Unique -> C.Name C.Term
legacyName occ i u = occ {C.nameOcc = Id.toText i, C.nameUniq = u}

-- | A context for Clash Core terms whose free variables are given by their
-- scoped counterparts: e.g. the arguments of a blackbox function on Clash
-- Core, converted to scoped Core to call a blackbox function on scoped Core.
-- 'toLegacy' gives back the given Clash Core variables.
legacyCompCtx
  :: Scope n
  -> TypeEnv
  -> Supply
  -> [(TyName n, C.TyVar)]
  -> [(TmName n, C.Id, Identifier)]
  -> CompCtx n
legacyCompCtx sc tenv sup tvs ids =
  CompCtx sc tenv
    (foldr (\(nm, _, i) -> insertNameMap nm i) emptyNameMap ids)
    (BackEnv (IntMap.fromList [(tyNameRaw nm, tv) | (nm, tv) <- tvs])
             (IntMap.fromList [(tmNameRaw nm, v) | (nm, v, _) <- ids]))
    is1
    sup
 where
  is0 = foldl (\is (_, v, _) -> extendInScopeSet is v) emptyInScopeSet ids
  is1 = foldl (\is (_, tv) -> extendInScopeSet is tv) is0 tvs

-- | Convert a term of a component to Clash Core, for a handler that runs on
-- Clash Core: see the module header.
toLegacy :: HasCallStack => CompCtx n -> Term n -> C.Term
toLegacy (CompCtx _ _ _ env is sup) e = evalState (toClashFresh is env e) sup

-- | Convert the arguments of a primitive or function to Clash Core, like
-- 'toLegacy'. Their binders get distinct uniques.
toLegacyArgs :: HasCallStack => CompCtx n -> [Either (Term n) (Type n)] -> [Either C.Term C.Type]
toLegacyArgs (CompCtx _ _ _ env is sup) args =
  evalState (mapM (either (fmap Left . toClashFresh is env) (fmap Right . toClashTypeFresh is env)) args) sup

-- | Convert a type to Clash Core
toLegacyType :: CompCtx n -> Type n -> C.Type
toLegacyType (CompCtx _ _ _ env is sup) ty = evalState (toClashTypeFresh is env ty) sup

-- | Convert a tick to Clash Core
toLegacyTick :: CompCtx n -> TickInfo n -> C.TickInfo
toLegacyTick (CompCtx _ _ _ env is sup) t = evalState (toClashTickFresh is env t) sup

-- | The Clash Core counterpart of a local variable. Errors if it is not in the
-- context.
toLegacyVar :: HasCallStack => CompCtx n -> TmName n -> C.Id
toLegacyVar (CompCtx _ _ _ (BackEnv _ ids) _ _) nm =
  case IntMap.lookup (tmNameRaw nm) ids of
    Just v -> v
    Nothing -> error ("toLegacyVar: local variable not in the context: " <> show (tmNameRaw nm))

-- * Local variables

-- | A local variable of scope @n@: its human readable name, type and name
data LocalId (n :: S) = LocalId
  { localIdOcc :: !(C.Name C.Term)
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

-- | The Clash Core counterpart of a local variable. Its type is that of the
-- variable when it was added to the context.
toLegacyLocalId :: HasCallStack => CompCtx n -> LocalId n -> C.Id
toLegacyLocalId ctx i = toLegacyVar ctx (localIdName i)

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

-- | Call a blackbox function: one on Clash Core gets its arguments and result
-- types converted with 'toLegacy'
callBlackBoxFunction
  :: (HasCallStack, Distinct n)
  => BlackBoxEnv n
  -> SomeBlackBoxFunction
  -> Bool
  -> Text
  -> [Either (Term n) (Type n)]
  -> [Type n]
  -> NetlistMonad (Either String (BlackBoxMeta, BlackBox))
callBlackBoxFunction ctx bbf isD nm args resTys = case bbf of
  ScopedBBF f -> f ctx isD nm args resTys
  LegacyBBF f -> f isD nm (toLegacyArgs ctx args) (map (toLegacyType ctx) resTys)

-- | A blackbox function on scoped Clash Core: given the arguments and result
-- types of the primitive it should provide a blackbox for, it generates the
-- blackbox. See "Clash.Netlist.BlackBox.Types" for its counterpart on Clash
-- Core.
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

-- | A blackbox function on scoped Clash Core or on Clash Core. A newtype-like
-- wrapper, because the rank-2 'BlackBoxFunction' can't instantiate a type
-- parameter (e.g. of 'Clash.Primitives.Types.Primitive'), nor be the result
-- of interpreting a function with Hint.
--
-- TODO(Phase 7): remove 'LegacyBBF'.
data SomeBlackBoxFunction
  = ScopedBBF BlackBoxFunction
  | LegacyBBF Legacy.BlackBoxFunction

-- | To weak head normal form, like functions
instance NFData SomeBlackBoxFunction where
  rnf = rwhnf
