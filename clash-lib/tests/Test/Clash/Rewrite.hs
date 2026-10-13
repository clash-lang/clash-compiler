{-|
  Copyright  :  (C) 2020,2022-2026 QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utilities to write unit tests on transformations: environments to run them
  in, type constructors to test them with ('testTyConMap'), and parsers of
  terms from strings.

  'parseToTerm' parses a term of Clash Core. 'parseTestTerm' parses the same
  language into scoped Clash Core, see the section "Parsing scoped Core" and
  "Test.Clash.Scoped" for the test scopes that build its terms.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Clash.Rewrite where

import Clash.Annotations.BitRepresentation.Internal (buildCustomReprs)
import qualified Clash.Core.DataCon as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.TyCon as C
import qualified Clash.Core.Literal as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.TysPrim as C
import qualified Clash.Core.Var as C
import Clash.Core.Evaluator.Types
  ( Evaluator (..), Machine (..), heapDelete, heapLookup, setTerm )
import Clash.Core.Pretty (PrettyOptions (..), PrettyPrec, showPpr')
import Clash.Core.VarEnv (InScopeSet, emptyVarSet, emptyVarEnv, emptyInScopeSet)
import Clash.Driver.Types
  ( Binding (..), ClashEnv (..), ClashOpts (..), DebugOpts, IsPrim (IsFun),
    defClashOpts, debugSilent )
import Clash.Annotations.Primitive (HDL (VHDL))
import Clash.Backend (initBackend)
import Clash.Backend.VHDL (VHDLState)
import Clash.Netlist (runNetlistMonad)
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
  ( FilteredHWType (..), HWType (Signed), HWTypeTranslator (..), NetlistMonad
  , PreserveCase (PreserveCase), SomeBackend (..) )
import Clash.Rewrite.Types
import Clash.Core.Scoped.Builder
  ( runDataConSig, Key, TickB, TmB, TyB, appB, appTyB, caseB, castB, constTyB, dataAltB, dataB, deDupTickB
  , defaultAltB, forAllTyB, funTyB, globalB, lamB, letB, letrecB, litAltB, litTyB, literalB
  , nameModTickB, noDeDupTickB, primB, runClosedType, runNewTyConSig, runTyFamEqn
  , srcSpanTickB, tickB, tyAppB, tyConAppB, tyLamB, uniqueKey, varB, varTyB )
import qualified Clash.Core.Scoped.Convert as S
  ( mkAlgTyCon, mkDataCon, mkFunTyCon, mkGlobalId, mkNewTyConRhs, mkPrimInfo, mkPrimTyCon )
import qualified Clash.Core.Scoped.TysPrim as P
import Clash.Core.Scoped.Syntax (PrimInfo (..), PrimUnfolding (NoUnfolding))
import Clash.Core.Scoped.Evaluator.Primitives.Util (tupleTyConName)
import Clash.Core.Scoped.Sig (DataConSig (..))
import Clash.Core.Scoped.Syntax (GlobalId (..), teleLength)
import Clash.Core.Scoped.NameMap (GlobalEnv, emptyGlobalEnv)
import qualified Clash.Core.Scoped.Syntax as ST (S (VoidS), Term)
import Clash.Core.Scoped.TypeEnv (mkTypeEnv)
import qualified Clash.Core.Scoped.Type as S (TypeView (..), tyView)
import Clash.Normalize.Strategy (constantPropagation, normalization)
import Clash.Normalize.Types
import qualified Clash.Util.Interpolate as I
import Clash.Util.Supply (Supply, freshId, newSupply)
import Clash.Unique (Unique, fromGhcUnique)

import Control.Applicative ((<|>))
import qualified Control.Monad as Monad
import Control.Monad.Trans.Writer.Strict (Writer, runWriter, tell)
import Control.DeepSeq (NFData, force)
import Control.Exception (ErrorCall (..), evaluate, try)
import Data.Char (isAscii, ord)
import Data.Default
import Data.Maybe (fromMaybe)
import qualified Data.Maybe as Maybe
import Data.Monoid (Any (..))
import Data.String (IsString (..))
import GHC.Types.Basic (InlineSpec (NoUserInlinePrag))
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Extension (Extension (..), KnownExtension (..))
import Language.Haskell.Exts.Parser
  ( ParseMode (..), defaultParseMode, fromParseResult, parseExpWithMode
  , parseTypeWithMode )
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Text.Read (readMaybe)
import GHC.Stack (HasCallStack)
import GHC.Builtin.Names
  ( boolTyConKey, eqTyConKey, falseDataConKey, ordEQDataConKey, ordGTDataConKey
  , ordLTDataConKey, trueDataConKey, typeNatAddTyFamNameKey
  , typeNatCmpTyFamNameKey, typeUnconsSymbolTyFamNameKey )

import qualified Text.Show.Pretty as Pretty

import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Quote as TH

import qualified Clash.Data.UniqMap as UniqMap
import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Text as Text

type TypeMap = HashMap.HashMap Unique C.Type

lookupTM :: Unique -> TypeMap -> C.Type
lookupTM u tm = case HashMap.lookup u tm of
  Just t -> t
  Nothing ->
    error [I.i|
      Tried to lookup unique '#{u}' in typemap, but couldn't find it. This
      usually means you forgot to (explicitely) declare a variable's type.
    |]

instance Default RewriteEnv where
  def = withTyConMap (UniqMap.singletonUnique intTyCon) RewriteEnv
    { _clashEnv = ClashEnv
        { envOpts = defClashOpts { opt_debug = debugSilent }
        , envTyConMap = UniqMap.empty
        , envTupleTyCons = IntMap.empty
        , envPrimitives = HashMap.empty
        , envCustomReprs = buildCustomReprs []
        , envDomains = HashMap.empty
        }
    , _typeTranslator=testTypeTranslator
    , _evaluator=error "_evaluator: NYI"
    , _topEntities=emptyVarSet
    , _scopedTypeEnv=mkTypeEnv UniqMap.empty
    }

-- | A 'RewriteEnv' with the given type constructors, in both 'envTyConMap' and
-- '_scopedTypeEnv'. Use this rather than setting 'envTyConMap', which would
-- leave '_scopedTypeEnv' with the old ones.
withTyConMap :: C.TyConMap -> RewriteEnv -> RewriteEnv
withTyConMap tcm env = env
  { _clashEnv = (_clashEnv env) {envTyConMap = tcm}
  , _scopedTypeEnv = mkTypeEnv tcm
  }

-- | The type translator of the default 'RewriteEnv': function types are not
-- representable, all other types are, as a 64-bit signed number. Clash's own
-- translator is a lot more refined, but this suffices for transformations that
-- only look at /whether/ a type is representable, such as 'caseCase' and
-- 'nonRepANF'.
testTypeTranslator :: HWTypeTranslator
testTypeTranslator = HWTypeTranslator $ \_reprs _tenv _sc ty -> pure $ Just $
  case S.tyView ty of
    S.FunTy {} -> Left "function types are not representable"
    _ -> Right (FilteredHWType (Signed 64) [])

-- | Run a Netlist action with the given global bindings of scoped Core, and
-- the environment of 'runNetlistTest'
runScopedNetlistTest :: GlobalEnv (Binding (ST.Term 'ST.VoidS)) -> NetlistMonad a -> IO a
runScopedNetlistTest bs act =
  fst <$>
    runNetlistMonad
      (_clashEnv env) False
      bs mempty (_typeTranslator env) True be ids "" mempty
      act
 where
  env = withTyConMap testTyConMap def
  ids = Id.emptyIdentifierSet False PreserveCase VHDL
  be = SomeBackend (initBackend (envOpts (_clashEnv env)) :: VHDLState)

instance Default extra => Default (RewriteState extra) where
  def = RewriteState
    { _transformCounter=0
    , _transformAppliedCounters=mempty
    , _transformTriedCounters=mempty
    , _bindings=emptyVarEnv
    , _uniqSupply=unsafePerformIO newSupply
    , _curFun=error "_curFun: NYI"
    , _nameCounter=2
    , _workFreeBinders=emptyVarEnv
    , _scopedBindings=emptyGlobalEnv
    , _scopedWorkFreeBinders=emptyGlobalEnv
    , _hwTypeCache=mempty
    , _globalHeap=error "_globalHeap: NYI"
    , _extra=def
    }

instance Default NormalizeState where
  def = NormalizeState
    { _normalized=emptyVarEnv
    , _specialisationCache=Map.empty
    , _specialisationHistory=emptyVarEnv
    , _inlineHistory=emptyVarEnv
    , _primitiveArgs=Map.empty
    , _recursiveComponents=emptyVarEnv
    , _normalizationStrategy=error "_normalizationStrategy: Clash Core is not normalized"
    , _constantPropagationStrategy=error "_constantPropagationStrategy: Clash Core is not normalized"
    , _scopedNormalizationStrategy=normalization
    , _scopedConstantPropagationStrategy=constantPropagation
    , _workFreeAppCache=HashMap.empty
    , _scopedNormalized=emptyGlobalEnv
    , _scopedSpecialisationCache=Map.empty
    , _scopedSpecialisationHistory=emptyGlobalEnv
    , _scopedInlineHistory=emptyGlobalEnv
    , _scopedRecursiveComponents=emptyGlobalEnv
    , _scopedWorkFreeAppCache=Map.empty
    }

instance Default InScopeSet where
  def = emptyInScopeSet

-- | Set the function that is being normalized ('Clash.Rewrite.Types.curFun')
withCurFun :: GlobalId -> RewriteState extra -> RewriteState extra
withCurFun g st = st{_curFun = (g, C.noSrcSpan)}

-- | Run an action in the rewrite monad, such as a transformation applied
-- with 'Clash.Rewrite.Scoped.Apply.runRewrite'. Returns its result, the
-- final state, and whether any transformation signalled a change.
runRewriteTest
  :: RewriteEnv
  -> RewriteState extra
  -> RewriteMonad extra a
  -> IO (a, RewriteState extra, Bool)
runRewriteTest rwEnv rwState m = do
  (a, rwState1, anyChanged) <- runR m rwEnv rwState
  pure (a, rwState1, getAny anyChanged)

-- | The default 'RewriteEnv', with the given debug options instead of
-- 'debugSilent'
rewriteEnvWithDebug :: DebugOpts -> RewriteEnv
rewriteEnvWithDebug opts = def{_clashEnv = clashEnv0{envOpts = opts0{opt_debug = opts}}}
 where
  clashEnv0 = _clashEnv def
  opts0 = envOpts clashEnv0

-- | The default 'RewriteEnv', with the type constructors of 'testTyConMap'
rewriteEnvWithTyCons :: RewriteEnv
rewriteEnvWithTyCons = withTyConMap testTyConMap def

parseType :: (HasCallStack, Show l) => Type l -> C.Type
parseType = \case
  -- Parentheses: (..)
  TyParen _ t ->
    parseType t

  -- Type constructor: T
  TyCon _ (UnQual _ nm) ->
    -- TODO: We could/should build a TyConMap here
    C.ConstTy (C.TyCon (parseName nm))

  -- Type variable: a
  TyVar _ nm ->
    C.VarTy (parseTyVar nm)

  -- Universal quantification: forall a b. t
  TyForall _ (Just tvs) Nothing t ->
    foldr (C.ForAllTy . parseTyVarBind) (parseType t) tvs

  -- Type application: f a
  TyApp _ t1 t2 ->
    C.AppTy (parseType t1) (parseType t2)

  -- Unsupported type:
  t ->
    error ("parseType: " <> show t)

-- | Parse an identifier into a 'C.TyVar'. Type variables are always of kind
-- 'liftedTypeKind': there is no way to spell out anything else, and nothing in
-- these tests needs one. See 'parseNameScope' for the format of identifiers.
parseTyVar :: (HasCallStack, Show l) => Name l -> C.TyVar
parseTyVar nm0 = C.TyVar nm1 (C.nameUniq nm1) C.liftedTypeKind
 where
  nm1 = parseName nm0

-- | Parse the binder of a @forall@ into a 'C.TyVar'. See 'parseTyVar'.
parseTyVarBind :: (HasCallStack, Show l) => TyVarBind l -> C.TyVar
parseTyVarBind = \case
  UnkindedVar _ nm -> parseTyVar nm

  -- A kind annotation would have to name a kind, and 'parseTyVar' only produces
  -- 'liftedTypeKind' anyway
  b -> error ("parseTyVarBind: " <> show b)

-- | Derive a 'Unique' from a human readable name, by interpreting each of its
-- characters as a byte and concatenating those bytes. Used for identifiers that
-- don't spell out their unique, see 'parseName'.
--
-- Only ASCII names of at most four characters are supported, as a 'Unique' is
-- only guaranteed to hold 32 bits.
nameToUnique :: HasCallStack => String -> Unique
nameToUnique nm
  | null nm = error
      "nameToUnique: can't derive a unique from an empty name"
  | length nm > 4 = error [I.i|
      Can't derive a unique from '#{nm}': a 'Unique' is only guaranteed to hold
      32 bits, so names of more than four characters don't fit. Spell out the
      unique instead, e.g. '#{nm}_123'.
    |]
  | any (not . isAscii) nm = error [I.i|
      Can't derive a unique from '#{nm}': it contains non-ASCII characters.
      Spell out the unique instead, e.g. 'foobar_123'.
    |]
  | otherwise = List.foldl' (\acc c -> acc * 256 + fromIntegral (ord c)) 0 nm

-- | Parse an identifier into a Clash Name and an 'C.IdScope'. Identifiers might
-- include a unique, and might include modifiers indicating their 'C.NameSort'
-- and 'C.IdScope'. Examples:
--
--   * x_3:  User, local identifier with human readable name "x", unique "3"
--   * x_I3: Internal, local identifier with human readable name "x", unique "3"
--   * x_S3: System, local identifier with human readable name "x", unique "3"
--   * x_G3: User, global identifier with human readable name "x", unique "3"
--
--   * x:    User, local identifier with human readable name "x", unique derived from "x"
--   * x_U:  User, local identifier with human readable name "x", unique derived from "x"
--   * x_S:  System, local identifier with human readable name "x", unique derived from "x"
--   * x_G:  User, global identifier with human readable name "x", unique derived from "x"
--
-- Modifiers may be combined, in any order: 'x_SG3' and 'x_GS3' both denote a
-- System, global identifier with unique "3". Identifiers that don't spell out
-- their unique derive it from their human readable name, see 'nameToUnique'.
--
-- Identifiers default to 'C.User' and 'C.LocalId'.
--
parseNameScope :: (HasCallStack, Show l) => Name l -> (C.Name a, C.IdScope)
parseNameScope = \case
  Ident _ s -> mkName s
  Symbol _ s -> mkName s
 where
  mkName s = case go "" s of
    Just (nmSort, scope, nm, uniq) ->
      ( C.mkUnsafeName nmSort (Text.pack nm) (fromMaybe (nameToUnique nm) uniq)
      , scope )
    -- No '_'-delimited suffix at all: the whole identifier is the name
    Nothing ->
      (C.mkUnsafeName C.User (Text.pack s) (nameToUnique s), C.LocalId)

  go _seen "" = Nothing
  go seen0 ('_':s:ss) = fmap withName (parseSuffix (s:ss)) <|> cont
   where
    withName (nmSort, scope, uniq) = (nmSort, scope, reverse seen0, uniq)
    cont = go ('_':seen0) (s:ss)
  go seen (s:ss) = go (s:seen) ss

  -- Parse a suffix such as "3", "S", or "SG3": zero or more modifiers followed
  -- by an optional unique. Yields 'Nothing' if the suffix is malformed, in which
  -- case it is considered part of the human readable name.
  parseSuffix = goSuffix C.User C.LocalId
   where
    goSuffix nmSort scope = \case
      'U':ss -> goSuffix C.User scope ss
      'S':ss -> goSuffix C.System scope ss
      'I':ss -> goSuffix C.Internal scope ss
      'G':ss -> goSuffix nmSort C.GlobalId ss
      'L':ss -> goSuffix nmSort C.LocalId ss
      -- Modifiers are either followed by a unique, or end the identifier
      "" -> Just (nmSort, scope, Nothing)
      ss -> fmap ((nmSort,scope,) . Just) (readMaybe ss)

-- | Parse an identifier into a Clash Name, ignoring any scope modifier. See
-- 'parseNameScope'.
parseName :: (HasCallStack, Show l) => Name l -> C.Name a
parseName = fst . parseNameScope

-- | Parse an identifier into an 'C.Id' of the given type. See 'parseNameScope'.
parseIdWithType :: (HasCallStack, Show l) => C.Type -> Name l -> C.Id
parseIdWithType typ nm0 = C.Id nm1 (C.nameUniq nm1) typ scope
 where
  (nm1, scope) = parseNameScope nm0

-- | Parse an identifier into an 'C.Id', looking its type up in the given
-- 'TypeMap'. Fails if the identifier's type wasn't declared. See
-- 'parseIdWithType'.
parseId :: (HasCallStack, Show l) => TypeMap -> Name l -> C.Id
parseId typs nm0 = C.Id nm1 uniq (lookupTM uniq typs) scope
 where
  (nm1, scope) = parseNameScope nm0
  uniq = C.nameUniq nm1

-- | Type given to free variables, i.e. to variables that aren't bound anywhere
-- in the term and don't spell out their type. There's nothing to infer such a
-- type from, and it is irrelevant to most tests - notably, alpha equivalence
-- compares free variables by unique alone.
--
-- Note that this only applies to /references/: binders always need their type
-- declared, see 'parsePats'.
freeVarType :: C.Type
freeVarType =
  C.ConstTy (C.TyCon (C.Name C.Internal (Text.pack "FreeVar") 0 C.noSrcSpan))

-- | Parse a reference to a variable, looking its type up in the given 'TypeMap'.
-- References that aren't in it are free variables, and get 'freeVarType'.
parseVarRef :: (HasCallStack, Show l) => TypeMap -> Name l -> C.Id
parseVarRef typs nm0 = C.Id nm1 uniq typ scope
 where
  (nm1, scope) = parseNameScope nm0
  uniq = C.nameUniq nm1
  typ = fromMaybe freeVarType (HashMap.lookup uniq typs)

-- | Parse the operator of an infix application into a variable reference
parseOp :: (HasCallStack, Show l) => TypeMap -> QOp l -> C.Term
parseOp typs = \case
  -- Operator: `f` or +
  QVarOp _ (UnQual _ nm) ->
    C.Var (parseVarRef typs nm)

  -- Unsupported operator:
  o ->
    error ("parseOp: " <> show o)

-- | Parse binder patterns, as used by let bindings and lambdas. Note that every
-- binder needs an explicit type annotation, as we don't do any type inference.
-- The annotation may be spelled out in the pattern itself, or declared in an
-- enclosing let. I.e., all of these are OK:
--
--    \(x_0 :: Int) -> x_0
--
--    let
--      (x_0 :: Int) = 2
--    in
--      x_0
--
--    let
--      x_0 :: Int
--      x_0 = 2
--    in
--      \x_0 -> x_0
--
-- But this is not:
--
--    let
--      x_0 = 2
--    in
--      x_0
--
-- Binders are added to the type map, so the scope they bind can refer to them
-- without repeating their type.
parsePats
  :: forall l
   . (HasCallStack, Show l)
  => TypeMap
  -> [Pat l]
  -> (TypeMap, [C.Id])
parsePats = List.mapAccumL parsePat
 where
  parsePat :: HasCallStack => TypeMap -> Pat l -> (TypeMap, C.Id)
  parsePat typs = \case
    -- Parentheses: (...)
    PParen _ p ->
      parsePat typs p

    -- Binder with type signature: x :: t
    PatTypeSig _ (PVar _ nm) (parseType -> t) ->
      let i = parseIdWithType t nm
      in (HashMap.insert (C.varUniq i) t typs, i)

    -- Binder: x
    PVar _ nm ->
      (typs, parseId typs nm)

    -- Unsupported pattern
    p ->
      error ("parsePat: " <> show p)

-- | Parse lambda binders. Like 'parsePats', except that a binder annotated with
-- the kind @Type@ binds a /type/ variable rather than a term variable:
--
--    \\(a :: Type) (x :: a) -> x
--
-- is @/\\a. \\x. x@, a 'C.TyLam' around a 'C.Lam'. Haskell has no syntax for a
-- type lambda, and @\\ \@a -> e@ is not something @haskell-src-exts@ parses, so
-- the kind annotation is what marks one here.
--
-- Type binders are not added to the type map: it maps a term variable to its
-- type, and a reference to a type variable is parsed by 'parseType', which needs
-- no context. See 'parseTyVar'.
parseLamPats
  :: forall l
   . (HasCallStack, Show l)
  => TypeMap
  -> [Pat l]
  -> (TypeMap, [Either C.TyVar C.Id])
parseLamPats = List.mapAccumL parseLamPat
 where
  parseLamPat
    :: HasCallStack => TypeMap -> Pat l -> (TypeMap, Either C.TyVar C.Id)
  parseLamPat typs pat
    | Just nm <- typeBinder pat = (typs, Left (parseTyVar nm))
    | otherwise = fmap Right (head' (parsePats typs [pat]))
   where
    head' (typs1, [i]) = (typs1, i)
    head' _ = error "parseLamPats: impossible"

  -- A binder annotated with the kind 'Type', modulo parentheses
  typeBinder :: Pat l -> Maybe (Name l)
  typeBinder = \case
    PParen _ p -> typeBinder p
    PatTypeSig _ (PVar _ nm) (TyCon _ (UnQual _ (Ident _ "Type"))) -> Just nm
    _ -> Nothing

-- | Parse declarations (as, amongst others, used in let expressions). See
-- 'parsePats' for how binders get their type.
--
-- The type map returned includes the types of all binders declared here, so it
-- can be used to parse the body of the let these declarations belong to.
parseDecls
  :: forall l
   . (HasCallStack, Show l)
  => TypeMap
  -> [Decl l]
  -> (TypeMap, [C.LetBinding])
parseDecls typs0 decls = (typs2, zip ids (map (expToTerm typs2) rhss))
 where
  (typDecls, otherDecls) = List.partition isTypeDecl decls

  -- Types declared by separate type signatures
  insertTyp (nm, t) = HashMap.insert nm t
  typs1 = foldr insertTyp typs0 (concatMap parseTypeDecl typDecls)

  -- Binders, plus the types they declare in their patterns. Note that all
  -- right-hand sides are parsed with the /final/ type map, so bindings may refer
  -- to each other irrespective of the order they're declared in.
  (typs2, ids) = parsePats typs1 pats
  (pats, rhss) = unzip (map splitOtherDecl otherDecls)

  splitOtherDecl :: HasCallStack => Decl l -> (Pat l, Exp l)
  splitOtherDecl = \case
    PatBind _ p (UnGuardedRhs _ e) Nothing -> (p, e)
    d -> error ("splitOtherDecl: " <> show d)

  parseTypeDecl :: Decl l -> [(Unique, C.Type)]
  parseTypeDecl (TypeSig _ nms t) =
    map (\nm -> (C.nameUniq (parseName nm), parseType t)) nms
  parseTypeDecl _ = error "impossible"

  isTypeDecl :: Decl l -> Bool
  isTypeDecl (TypeSig {}) = True
  isTypeDecl _ = False

-- | Parse a haskell-src-exts expression into Clash Core.
expToTerm
  :: forall l
   . (HasCallStack, Show l)
  => TypeMap
  -> Exp l
  -> C.Term
expToTerm typs0 = \case
  -- Parentheses: (...)
  Paren _ e ->
    expToTerm typs0 e

  -- Variable reference with type signature: x :: t
  ExpTypeSig _ (Var _ (UnQual _ nm)) (parseType -> t) ->
    C.Var (parseIdWithType t nm)

  -- Type application: e @t
  App _ e1 (TypeApp _ t) ->
    C.TyApp (expToTerm typs0 e1) (parseType t)

  -- Term application: e1 e2
  App _ e1 e2 ->
    C.App (expToTerm typs0 e1) (expToTerm typs0 e2)

  -- Infix application: e1 + e2
  InfixApp _ e1 op e2 ->
    C.App (C.App (parseOp typs0 op) (expToTerm typs0 e1)) (expToTerm typs0 e2)

  -- Lambda: \x y -> e. A binder annotated @:: Type@ binds a type variable, so
  -- it becomes a 'C.TyLam': @\\(a :: Type) (x :: a) -> x@ is @/\\a. \\x. x@.
  Lambda _ pats body0 ->
    let
      (typs1, binders) = parseLamPats typs0 pats
      body1 = expToTerm typs1 body0
    in
      foldr (either C.TyLam C.Lam) body1 binders

  -- Variable reference: e
  Var _ (UnQual _ nm) ->
    C.Var (parseVarRef typs0 nm)

  -- Literal: 3
  Lit _ (Int _ i _) -> C.Literal (C.IntLiteral i)

  -- Let expression: let {e1 = .., e2 = ..} in r
  Let _ (BDecls _ decls0) body0 ->
    let
      (typs1, decls1) = parseDecls typs0 decls0
      body1 = expToTerm typs1 body0
    in
      C.Letrec decls1 body1

 -- Unsupported expression
  e -> error ("expToTerm: " <> show e)

-- | Parse mode used by 'parseToTerm'. Enables:
--
--   * @ScopedTypeVariables@, so lambda binders can spell out their type:
--     @\\(x_0 :: Int) -> x_0@, and so a type binder can spell out its kind:
--     @\\(a :: Type) -> ..@, see 'parseLamPats'.
--   * @RankNTypes@, for @forall@ in a type: @(x :: forall a. a)@.
--   * @TypeApplications@, for type application: @f \@Int@.
termParseMode :: ParseMode
termParseMode = defaultParseMode
  { extensions =
      map EnableExtension [ScopedTypeVariables, RankNTypes, TypeApplications]
        <> extensions defaultParseMode
  }

-- | Parse a string representing a Haskell expression into Clash Core. This can
-- only parse very simple expressions. In the future we should make an effort to
-- build a proper TyConMap (using LoadModules) to faithfully reproduce more
-- complex expressions.
parseToTerm :: HasCallStack => String -> C.Term
parseToTerm =
  expToTerm HashMap.empty . fromParseResult . parseExpWithMode termParseMode

-- * Parsing scoped Core
--
-- The parser of scoped Clash Core. It parses the same language as
-- 'parseToTerm', with the same identifiers (see 'parseNameScope'), but builds
-- builders ("Clash.Core.Scoped.Builder") rather than terms of Clash Core, so
-- that a test can build the term in a scope of its choice. See
-- "Test.Clash.Scoped" for how tests use it.
--
-- Compared to 'parseToTerm', it also parses:
--
--   * Type constructors and data constructors of 'zeroWidthTyConMap', by
--     their names, see 'resolveTyConName' and 'resolveDataCon':
--     @Maybe Int@, @Clash.Sized.Vector.Vec 2 Int@, @Just \@Int 1@.
--
--   * Function types @Int -> Int@, type-level numbers and strings, and the
--     kinds @Type@, @Nat@ and @Symbol@, also on type binders:
--     @\\(n :: Nat) -> ..@ and @forall (n :: Nat). ..@.
--
--   * Case-expressions, annotated with the type of their alternatives:
--     @(case m of { Just (x :: Int) -> x; Nothing -> 0 }) :: Int@. Patterns
--     are constructors applied to typed binders, integer literals, and @_@.
--     The existentially quantified type variables of a constructor are its
--     first binders, annotated with their kinds, like the type binders of a
--     lambda: @MkE (b :: Type) (y :: b)@.
--
--   * Primitives, as qualified variables annotated with their (closed) type:
--     @(GHC.Num.Integer.integerMul :: Integer -> Integer -> Integer)@. They
--     do variable work, have a single result and no unfolding. Without an
--     annotation, a primitive has type 'freeVarType'. The module @Prim@
--     qualifies primitives whose names are not qualified (@Prim.f@), and
--     @Core.prim \@"name"@ is the primitive of any name. A test can define
--     its own primitives, by name, with 'parseTestTermWith'.
--
--   * Literals annotated with their type, for the literals that aren't
--     @Int#@ literals: @(3 :: Integer)@, @(3 :: Natural)@, @(3 :: Word)@, and
--     negative literals @(-1)@.
--
--   * Binders of type variables of other kinds than the ones above, such as
--     a type variable, annotated as @Core.Kind k@:
--     @\\(k :: Type) (a :: Core.Kind k) -> ..@.
--
--   * Empty case-expressions: @(case x of {}) :: Int@.
--
--   * Equality constraints @a ~ b@, see 'liftedEqTyConName'.
--
--   * Casts and ticks, as applications of the pseudo-functions of the module
--     @Core@: @Core.cast \@T1 \@T2 e@ casts @e@ from @T1@ to @T2@,
--     @Core.deDup e@, @Core.noDeDup e@ and @Core.srcSpan e@ are ticks, and
--     @Core.prefixName \@"n" e@ (and @suffixName@, @suffixNameP@, @setName@)
--     are name modifiers.
--
--   * Non-recursive let-bindings, written with a bang:
--     @let { !(x :: Int) = 1 } in x@. Like in Haskell, where a bang binding
--     can't be recursive, its right-hand side is outside of its scope. A let
--     with several bang bindings binds them one after the other. A let
--     without bangs is recursive, as in 'parseToTerm'.
--
-- Variables refer to their binders by /key/: the unique of the identifier,
-- see 'Clash.Core.Scoped.Builder.Key'. So an identifier that a binder binds
-- again refers to the inner binder within its scope, like in Haskell:
-- @\\(x_1 :: Int) -> let { !(x_1 :: Int) = f x_1 } in x_1@ binds @x_1@ twice,
-- and the right-hand side refers to the lambda's binder.
--
-- Global variables (@f_G1@) are not scoped. Their type is their annotation,
-- which must be closed, or 'freeVarType'. Local variables that no binder
-- binds are free: the scope that the term is built in must have them (see
-- 'Test.Clash.Scoped.termIn'), or the test declares them from the term (see
-- 'Test.Clash.Scoped.withTerm', and 'testTermFree').

-- | A term of scoped Clash Core to build in the scope of a test, see
-- 'Test.Clash.Scoped.termIn'. Parse it from a string with 'parseTestTerm',
-- the quasiquoter 'termQQ', or a string literal (with @OverloadedStrings@).
data TestTerm = TestTerm
  { testTermB :: TmB
  -- ^ The term
  , testTermFree :: [ScopeDecl]
  -- ^ The free local variables it mentions, in the order in which
  -- 'Test.Clash.Scoped.withTerm' declares them: the type variables (of kind
  -- @Type@) by key, then the term variables by key, with the type of their
  -- first annotated occurrence, or 'freeVarType'.
  }

instance IsString TestTerm where
  fromString = parseTestTerm

-- | A type of scoped Clash Core to build in the scope of a test, see
-- 'TestTerm' and 'Test.Clash.Scoped.typeIn'
data TestType = TestType
  { testTypeB :: TyB
  , testTypeFree :: [ScopeDecl]
  -- ^ The free type variables it mentions, of kind @Type@, by key
  }

instance IsString TestType where
  fromString = parseTestType

-- | A term made with the builders ("Clash.Core.Scoped.Builder"), e.g. for
-- what the parser can't parse, such as primitives. The builders of a parsed
-- term are 'testTermB'. Its free variables are not known: a scope that the
-- term is built in must have them.
builtTerm :: TmB -> TestTerm
builtTerm b = TestTerm b []

-- | A type made with the builders, see 'builtTerm'
builtType :: TyB -> TestType
builtType b = TestType b []

-- | A variable of the scope of a test, see 'Test.Clash.Scoped.withTestScope'
data ScopeDecl = ScopeDecl
  { declRaw :: !(Maybe Int)
  -- ^ The raw name it must get, see 'atRaw'
  , declVar :: !DeclVar
  }

-- | A variable of the scope of a test: its key, name, and kind or type. The
-- type of a term variable is 'freeVarType' if it has none.
data DeclVar
  = DeclTyVar !Key !(C.Name C.Type) TestType
  | DeclId !Key !(C.Name C.Term) !(Maybe TestType)

-- | A type variable of kind @Type@, by its identifier (see 'parseNameScope')
tyVarD :: HasCallStack => String -> ScopeDecl
tyVarD nm = kindedTyVarD nm (parseTestType "Type")

-- | A type variable of the given kind, see 'tyVarD'
kindedTyVarD :: HasCallStack => String -> TestType -> ScopeDecl
kindedTyVarD s k = ScopeDecl Nothing (DeclTyVar (nameKey nm) nm k)
 where
  nm = parseLocalName s

-- | A term variable of the given type, by its identifier (see
-- 'parseNameScope'), e.g. @varD "x_1" "Maybe a_2"@
varD :: HasCallStack => String -> TestType -> ScopeDecl
varD s ty = ScopeDecl Nothing (DeclId (nameKey nm) nm (Just ty))
 where
  nm = parseLocalName s

-- | Give a variable a raw name above those of the variables before it,
-- rather than the next one. E.g. a variable with a raw name above the names of
-- all binders of a term, see 'Test.Clash.Scoped.sunkD'.
atRaw :: Int -> ScopeDecl -> ScopeDecl
atRaw r d = d {declRaw = Just r}

-- | The name of a local variable, by its identifier
parseLocalName :: HasCallStack => String -> C.Name a
parseLocalName s = case parseNameScope (Ident () s) of
  (nm, C.LocalId) -> nm
  (_, C.GlobalId) -> error ("parseLocalName: not a local variable: " <> s)

-- | The key of a variable with the given name
nameKey :: C.Name a -> Key
nameKey = uniqueKey . C.nameUniq

-- | A global variable of the given closed type, by its identifier (see
-- 'parseNameScope'). Equal to the global variable that a term with the same
-- identifier refers to, as global variables are equal by unique.
testGlobalId :: HasCallStack => String -> TestType -> GlobalId
testGlobalId s ty = S.mkGlobalId (parseName (Ident () s)) (runClosedType (testTypeB ty))

-- | Parse a term of scoped Core, see the section header
parseTestTerm :: HasCallStack => String -> TestTerm
parseTestTerm = parseTestTermWith mempty

-- | Data constructors and primitives that a test defines, which the terms it
-- parses with 'parseTestTermWith' mention by name
data TestDefs = TestDefs
  { testDefDataCons :: [C.DataCon]
  -- ^ Data constructors, e.g. those of a type constructor that a test
  -- defines. They take precedence over those of 'testDataCons'.
  , testDefPrims :: [PrimInfo]
  -- ^ Primitives, by their names. A type annotation on a primitive the term
  -- mentions overrides the type of its definition. A primitive that is not
  -- one of these does variable work, see the section header.
  , testDefGlobals :: [GlobalId]
  -- ^ Global variables, with their types, see 'testGlobalId'. A global
  -- variable the term mentions with a type annotation, or that is not one of
  -- these, has the type of its annotation.
  }

instance Semigroup TestDefs where
  TestDefs dcs1 ps1 gs1 <> TestDefs dcs2 ps2 gs2 =
    TestDefs (dcs1 <> dcs2) (ps1 <> ps2) (gs1 <> gs2)

instance Monoid TestDefs where
  mempty = TestDefs [] [] []

-- | Parse a term of scoped Core that may also mention the data constructors
-- and primitives that a test defines, see 'TestDefs'
parseTestTermWith :: HasCallStack => TestDefs -> String -> TestTerm
parseTestTermWith defs s = TestTerm b (freeDecls fr)
 where
  (b, fr) = runWriter (spTm emptyPEnv {_peDefs = defs} (fromParseResult (parseExpWithMode scopedParseMode s)))

-- | A quasiquoter for strings that span several lines, e.g. for
-- 'parseTestTermWith'
strQQ :: TH.QuasiQuoter
strQQ = stringQQ 'id

-- | Parse a type of scoped Core, see the section header
parseTestType :: HasCallStack => String -> TestType
parseTestType s = TestType b (freeDecls fr)
 where
  (b, fr) = runWriter (spTy emptyPEnv (fromParseResult (parseTypeWithMode scopedParseMode s)))

-- | 'parseTestTerm' as a quasiquoter, for terms that span several lines:
--
-- > [termQQ|
-- >   let
-- >     !(x :: Int) = 1
-- >   in
-- >     x
-- > |]
termQQ :: TH.QuasiQuoter
termQQ = stringQQ 'parseTestTerm

-- | 'parseTestType' as a quasiquoter
typeQQ :: TH.QuasiQuoter
typeQQ = stringQQ 'parseTestType

-- | A quasiquoter that applies the given function to the quoted string
stringQQ :: TH.Name -> TH.QuasiQuoter
stringQQ f = TH.QuasiQuoter
  { TH.quoteExp = fmap (TH.AppE (TH.VarE f)) . TH.lift
  , TH.quotePat = error "stringQQ.quotePat: NYI"
  , TH.quoteType = error "stringQQ.quoteType: NYI"
  , TH.quoteDec = error "stringQQ.quoteDec: NYI"
  }

-- | The parse mode of 'parseTestTerm': that of 'parseToTerm', with
-- @BangPatterns@ for non-recursive let-bindings, @DataKinds@ for type-level
-- literals, @KindSignatures@ for kinded type binders, and @MagicHash@ for
-- names like @Int#@
scopedParseMode :: ParseMode
scopedParseMode = termParseMode
  { extensions =
      map EnableExtension [BangPatterns, DataKinds, EmptyCase, GADTs, KindSignatures, MagicHash]
        <> extensions termParseMode
  }

-- | The keys of the variables bound around a subterm, and the types that let
-- expressions declare for binders by key, see 'parsePats'
data PEnv = PEnv
  { _peTyVars :: !IntSet.IntSet
  , _peVars :: !IntSet.IntSet
  , _peSigs :: !(IntMap.IntMap TyB)
  , _peDefs :: TestDefs
  -- ^ The data constructors and primitives of the test, see
  -- 'parseTestTermWith'
  }

emptyPEnv :: PEnv
emptyPEnv = PEnv IntSet.empty IntSet.empty IntMap.empty mempty

bindTyKey :: Key -> PEnv -> PEnv
bindTyKey k (PEnv tys tms sigs dcs) = PEnv (IntSet.insert k tys) tms sigs dcs

bindTmKey :: Key -> PEnv -> PEnv
bindTmKey k (PEnv tys tms sigs dcs) = PEnv tys (IntSet.insert k tms) sigs dcs

-- | The free variables of a parsed term: the type variables by key, and the
-- term variables by key, with the type of the first occurrence that has one
data Free = Free !(IntMap.IntMap (C.Name C.Type)) !(IntMap.IntMap (C.Name C.Term, Maybe TyB))

instance Semigroup Free where
  Free tys1 tms1 <> Free tys2 tms2 =
    Free (IntMap.union tys1 tys2) (IntMap.unionWith firstTyped tms1 tms2)
   where
    firstTyped (nm, Nothing) (_, ty) = (nm, ty)
    firstTyped v _ = v

instance Monoid Free where
  mempty = Free IntMap.empty IntMap.empty

-- | The declarations of free variables, see 'testTermFree'
freeDecls :: Free -> [ScopeDecl]
freeDecls (Free tys tms) =
  [ ScopeDecl Nothing (DeclTyVar k nm (builtType liftedTypeKindB))
  | (k, nm) <- IntMap.toAscList tys ] <>
  [ ScopeDecl Nothing (DeclId k nm (builtType <$> ty))
  | (k, (nm, ty)) <- IntMap.toAscList tms ]

type P = Writer Free

-- | The type constructor of a builtin type that is a single type constructor
constOf :: HasCallStack => C.Type -> C.ConstTy
constOf = \case
  C.ConstTy c -> c
  ty -> error ("constOf: not a type constructor: " <> show ty)

liftedTypeKindB :: TyB
liftedTypeKindB = constTyB (constOf C.liftedTypeKind)

-- | 'freeVarType' as a builder
freeVarTyB :: TyB
freeVarTyB = constTyB (constOf freeVarType)

-- | A qualified or unqualified name as a string
qNameString :: (HasCallStack, Show l) => QName l -> String
qNameString = \case
  UnQual _ nm -> nameString nm
  Qual _ (ModuleName _ m) nm -> m <> "." <> nameString nm
  q -> error ("qNameString: " <> show q)

nameString :: Name l -> String
nameString = \case
  Ident _ s -> s
  Symbol _ s -> s

-- | The type constructors that 'parseTestType' knows by name
testTyCons :: [C.TyCon]
testTyCons = UniqMap.elems zeroWidthTyConMap

-- | The data constructors that 'parseTestTerm' knows by name
testDataCons :: [C.DataCon]
testDataCons = concatMap C.tyConDataCons testTyCons

-- | Look a value up by its name: one whose name is the given string, or the
-- given string spelled for the GHC that compiled the tests (see 'ghcName'), or
-- else the only one whose name ends in the given string after a dot, e.g.
-- @Vec@ for @Clash.Sized.Vector.Vec@
lookupByOcc :: (a -> Text.Text) -> [a] -> String -> Maybe a
lookupByOcc occ xs s = case filter ((== t) . occ) xs <> filter ((== ghcName t) . occ) xs of
  x : _ -> Just x
  [] -> case filter ((== t) . Text.takeWhileEnd (/= '.') . occ) xs of
    [x] -> Just x
    _ -> Nothing
 where
  t = Text.pack s

-- | Spell a qualified name of GHC, written with the module names of GHC 9.12,
-- with the module names of the GHC that compiled the tests, so that tests can
-- name the types, constructors and primitives of GHC independently of its
-- version. GHC 9.14 defines them in @ghc-internal@, e.g. @GHC.Prim.Int#@ is
-- @GHC.Internal.Prim.Int#@ there.
ghcName :: Text.Text -> Text.Text
#if __GLASGOW_HASKELL__ >= 914
ghcName nm
  | nm `elem` clashSpelledGhcNames = nm
  | otherwise = go
    [ ("GHC.Num.Integer.", "GHC.Internal.Bignum.Integer.")
    , ("GHC.Num.Natural.", "GHC.Internal.Bignum.Natural.")
    , ("GHC.Prim.", "GHC.Internal.Prim.")
    , ("GHC.Types.", "GHC.Internal.Types.")
    , ("GHC.Classes.", "GHC.Internal.Classes.")
    , ("GHC.Tuple.", "GHC.Internal.Tuple.")
    , ("GHC.CString.", "GHC.Internal.CString.")
    , ("GHC.Magic.", "GHC.Internal.Magic.")
    ]
 where
  go [] = nm
  go ((old, new) : ps) =
    maybe (go ps) (Text.pack new <>) (Text.stripPrefix (Text.pack old) nm)
#else
ghcName = id
#endif

-- | Names in the modules of GHC that Clash spells itself, the same for every
-- version of GHC: those of the unsafe coercion primitive and of the type
-- constructor of equality constraints. 'ghcName' leaves them as they are.
clashSpelledGhcNames :: [Text.Text]
clashSpelledGhcNames = map Text.pack ["GHC.Prim.unsafeCoerce#", "GHC.Prim.~#"]

-- | The name of a type constructor: one of 'testTyCons' (see 'lookupByOcc'),
-- or else as 'parseType' names it, e.g. @Maybe_9@ is the name @Maybe@ with
-- unique 9
resolveTyConName :: HasCallStack => String -> C.TyConName
resolveTyConName s = case lookupByOcc (C.nameOcc . C.tyConName) testTyCons s of
  Just tc -> C.tyConName tc
  Nothing -> parseName (Ident () s)

-- | A data constructor of the given ones, or else of 'testDataCons', see
-- 'lookupByOcc'
resolveDataConIn :: HasCallStack => [C.DataCon] -> String -> C.DataCon
resolveDataConIn dcs s = case lookupByOcc (C.nameOcc . C.dcName) dcs s <|> lookupByOcc (C.nameOcc . C.dcName) testDataCons s of
  Just dc -> dc
  Nothing -> error ("resolveDataCon: unknown or ambiguous data constructor: " <> s)

-- | A type constructor, or one of the kinds @Type@, @Nat@ and @Symbol@
tyConB :: HasCallStack => String -> TyB
tyConB = \case
  "Type" -> liftedTypeKindB
  "Nat" -> constTyB (constOf C.typeNatKind)
  "Symbol" -> constTyB (constOf C.typeSymbolKind)
  s -> constTyB (C.TyCon (resolveTyConName s))

spTy :: (HasCallStack, Show l) => PEnv -> Type l -> P TyB
spTy env@(PEnv tvs _ _ _) = \case
  TyParen _ t -> spTy env t
  TyCon _ qn -> pure (tyConB (qNameString qn))
  TyVar _ nm0 -> do
    let nm = parseName nm0
        k = nameKey nm
    Monad.unless (IntSet.member k tvs) (tell (Free (IntMap.singleton k nm) IntMap.empty))
    pure (varTyB k)
  TyForall _ (Just tvbs) Nothing t ->
    spTyVarBinds env tvbs $ \env' bs -> do
      body <- spTy env' t
      pure (foldr (\(k, nm, ki) -> forAllTyB k nm ki) body bs)
  TyApp _ t1 t2 -> appTyB <$> spTy env t1 <*> spTy env t2
  TyFun _ t1 t2 -> (\a r -> funTyB [a] r) <$> spTy env t1 <*> spTy env t2
  TyEquals _ t1 t2 -> do
    a <- spTy env t1
    b <- spTy env t2
    pure (appTyB (appTyB (appTyB (constTyB (C.TyCon liftedEqTyConName)) liftedTypeKindB) a) b)
  TyPromoted _ (PromotedInteger _ i _) -> pure (litTyB (C.NumTy i))
  TyPromoted _ (PromotedString _ s _) -> pure (litTyB (C.SymTy s))
  t -> error ("parseTestType: " <> show t)

-- | Type variable binders, each in the scope of the ones before it
spTyVarBinds
  :: (HasCallStack, Show l)
  => PEnv -> [TyVarBind l] -> (PEnv -> [(Key, C.Name C.Type, TyB)] -> P r) -> P r
spTyVarBinds env [] k = k env []
spTyVarBinds env (tvb : tvbs) k = do
  (nm, ki) <- case tvb of
    UnkindedVar _ nm -> pure (parseName nm, liftedTypeKindB)
    KindedVar _ nm ki -> (parseName nm,) <$> spTy env ki
  let key = nameKey nm
  spTyVarBinds (bindTyKey key env) tvbs $ \env' bs -> k env' ((key, nm, ki) : bs)

spTm :: (HasCallStack, Show l) => PEnv -> Exp l -> P TmB
spTm env = \case
  Paren _ e -> spTm env e
  ExpTypeSig _ e t -> case stripParens e of
    Var _ (UnQual _ nm) -> varRef env nm (Just t)
    Var _ qn@Qual {} -> pure (primRef env qn (Just t))
    App _ (Var _ (Qual _ (ModuleName _ "Core") (Ident _ "prim"))) (TypeApp _ (TyPromoted _ (PromotedString _ nm _))) ->
      pure (primNamed env nm (Just t))
    Case _ scrut alts -> spCase env scrut alts t
    Lit _ l -> pure (literalB (typedLiteral l t))
    NegApp _ (Lit _ l@Int {}) -> pure (literalB (negateLiteral (typedLiteral l t)))
    _ -> error ("parseTestTerm: unsupported type annotation: " <> show e)
  App _ (App _ (App _ (Var _ (Qual _ (ModuleName _ "Core") (Ident _ "cast"))) (TypeApp _ t1)) (TypeApp _ t2)) e ->
    castB <$> spTm env e <*> spTy env t1 <*> spTy env t2
  App _ (App _ (Var _ (Qual _ (ModuleName _ "Core") (Ident _ op))) (TypeApp _ t)) e
    | Just nmMod <- lookup op nameMods -> do
        ty <- spTy env t
        tickB (nameModTickB nmMod ty) <$> spTm env e
  App _ (Var _ (Qual _ (ModuleName _ "Core") (Ident _ op))) e
    | Just tk <- lookup op ticks -> tickB tk <$> spTm env e
  Var _ (Qual _ (ModuleName _ "Core") nm) ->
    error ("parseTestTerm: unknown or partially applied pseudo-function Core." <> nameString nm)
  App _ (Var _ (Qual _ (ModuleName _ "Core") (Ident _ "prim"))) (TypeApp _ (TyPromoted _ (PromotedString _ nm _))) ->
    pure (primNamed env nm (Nothing :: Maybe (Type ())))
  Var _ qn@Qual {} -> pure (primRef env qn Nothing)
  NegApp _ (Lit _ (Int _ i _)) -> pure (literalB (C.IntLiteral (negate i)))
  Lit _ (String _ str _) -> pure (literalB (C.StringLiteral str))
  Lit _ (Char _ c _) -> pure (literalB (C.CharLiteral c))
  App _ e1 (TypeApp _ t) -> tyAppB <$> spTm env e1 <*> spTy env t
  App _ e1 e2 -> appB <$> spTm env e1 <*> spTm env e2
  InfixApp _ e1 op e2 -> do
    f <- case op of
      QVarOp _ (UnQual _ nm) -> varRef env nm Nothing
      QConOp _ qn -> pure (dataB (resolveDataConIn (testDefDataCons (_peDefs env)) (qNameString qn)))
      o -> error ("parseTestTerm: unsupported operator: " <> show o)
    a1 <- spTm env e1
    a2 <- spTm env e2
    pure (appB (appB f a1) a2)
  Lambda _ pats body -> spLams env pats body
  Var _ (UnQual _ nm) -> varRef env nm Nothing
  Con _ qn -> pure (dataB (resolveDataConIn (testDefDataCons (_peDefs env)) (qNameString qn)))
  Lit _ (Int _ i _) -> pure (literalB (C.IntLiteral i))
  Let _ (BDecls _ decls) body -> spLet env decls body
  e@Case {} ->
    error ("parseTestTerm: annotate a case-expression with the type of its alternatives: " <> show e)
  e -> error ("parseTestTerm: " <> show e)
 where
  stripParens = \case
    Paren _ e -> stripParens e
    e -> e

-- | The ticks that are pseudo-functions of the module @Core@
ticks :: [(String, TickB)]
ticks = [("deDup", deDupTickB), ("noDeDup", noDeDupTickB), ("srcSpan", srcSpanTickB C.noSrcSpan)]

-- | The name modifiers that are pseudo-functions of the module @Core@
nameMods :: [(String, C.NameMod)]
nameMods =
  [ ("prefixName", C.PrefixName), ("suffixName", C.SuffixName)
  , ("suffixNameP", C.SuffixNameP), ("setName", C.SetName) ]

-- | A primitive, by its qualified name, with an optional (closed) type
-- annotation. The module @Prim@ qualifies the names of primitives that are
-- not qualified: @Prim.f@ is the primitive @f@.
primRef :: (HasCallStack, Show l) => PEnv -> QName l -> Maybe (Type l) -> TmB
primRef env qn = primNamed env $ case qn of
  Qual _ (ModuleName _ "Prim") nm -> nameString nm
  _ -> qNameString qn

-- | A primitive by its name: one the test defines (see 'TestDefs'), at the
-- given (closed) type if there is one; or else one of the given type, which
-- does variable work
primNamed :: (HasCallStack, Show l) => PEnv -> String -> Maybe (Type l) -> TmB
primNamed env nm annot =
  case (List.find ((`elem` [Text.pack nm, ghcName (Text.pack nm)]) . primName) (testDefPrims (_peDefs env)), annot) of
    (Just p, Nothing) -> primB p
    (Just p, Just _) ->
      primB (S.mkPrimInfo (primName p) (runClosedType ty) (primWorkInfo p) (primMultiResult p) (primUnfolding p))
    (Nothing, _) ->
      primB (S.mkPrimInfo (ghcName (Text.pack nm)) (runClosedType ty) C.WorkVariable C.SingleResult NoUnfolding)
 where
  ty = maybe freeVarTyB (fst . runWriter . spTy emptyPEnv) annot

-- | A literal annotated with its type, see the section header
typedLiteral :: (HasCallStack, Show l) => Literal l -> Type l -> C.Literal
typedLiteral l t = case (l, stripTyParens t) of
  (Int _ i _, TyCon _ (UnQual _ (Ident _ tc))) -> case tc of
    "Int" -> C.IntLiteral i
    "Integer" -> C.IntegerLiteral i
    "Natural" -> C.NaturalLiteral i
    "Word" -> C.WordLiteral i
    _ -> err
  (String _ str _, _) -> C.StringLiteral str
  (Char _ c _, _) -> C.CharLiteral c
  _ -> err
 where
  err = error ("parseTestTerm: unsupported literal: " <> show l <> " :: " <> show t)
  stripTyParens = \case
    TyParen _ ty -> stripTyParens ty
    ty -> ty

-- | The negation of a numeric literal
negateLiteral :: HasCallStack => C.Literal -> C.Literal
negateLiteral = \case
  C.IntLiteral i -> C.IntLiteral (negate i)
  C.IntegerLiteral i -> C.IntegerLiteral (negate i)
  l -> error ("parseTestTerm: can't negate the literal " <> show l)

-- | A reference to a variable, with an optional type annotation
varRef :: (HasCallStack, Show l) => PEnv -> Name l -> Maybe (Type l) -> P TmB
varRef env@(PEnv _ tms _ defs) nm0 annot = case parseNameScope nm0 of
  (nm, C.GlobalId)
    | Nothing <- annot
    , Just g <- List.find ((== C.nameUniq nm) . C.nameUniq . globalIdName) (testDefGlobals defs)
    -> pure (globalB g)
    | otherwise ->
    -- The type of a global is closed, so its type variables are not free
    -- variables of the term
    let ty = maybe freeVarTyB (fst . runWriter . spTy env) annot
    in  pure (globalB (S.mkGlobalId nm (runClosedType ty)))
  (nm, C.LocalId)
    | IntSet.member k tms -> pure (varB k)
    | otherwise -> do
        ty <- traverse (spTy env) annot
        tell (Free IntMap.empty (IntMap.singleton k (nm, ty)))
        pure (varB k)
   where
    k = nameKey nm

-- | A binder of a term variable: its key, name and type. The type is its
-- annotation, or that of a type signature of an enclosing let-expression, see
-- 'parsePats'.
spIdBinder :: (HasCallStack, Show l) => PEnv -> Pat l -> P (Key, C.Name C.Term, TyB)
spIdBinder env@(PEnv _ _ sigs _) = \case
  PParen _ p -> spIdBinder env p
  PatTypeSig _ (PVar _ nm0) t -> do
    let nm = parseName nm0
    (nameKey nm, nm,) <$> spTy env t
  PVar _ nm0 ->
    let nm = parseName nm0
        k = nameKey nm
    in  case IntMap.lookup k sigs of
          Just ty -> pure (k, nm, ty)
          Nothing -> error [I.i|
            The binder '#{C.nameOcc nm}' has no type: you forgot to
            (explicitely) declare it, see 'parsePats'.
          |]
  p -> error ("parseTestTerm: unsupported binder: " <> show p)

-- | Lambdas. A binder annotated with a kind binds a type variable, see
-- 'parseLamPats'.
spLams :: (HasCallStack, Show l) => PEnv -> [Pat l] -> Exp l -> P TmB
spLams env [] body = spTm env body
spLams env (p : ps) body = case kindBinderPat p of
  Just (nm0, ki0) -> do
    let nm = parseName nm0
        k = nameKey nm
    ki <- spTy env ki0
    tyLamB k nm ki <$> spLams (bindTyKey k env) ps body
  Nothing -> do
    (k, nm, ty) <- spIdBinder env p
    lamB k nm ty <$> spLams (bindTmKey k env) ps body

-- | A binder annotated with a kind, which binds a type variable: its name and
-- kind. The kinds are @Type@, @Nat@, @Symbol@, and functions between them, or
-- any kind @k@ annotated as @Core.Kind k@, see 'coreKind'.
kindBinderPat :: Pat l -> Maybe (Name l, Type l)
kindBinderPat = \case
  PParen _ q -> kindBinderPat q
  PatTypeSig _ (PVar _ nm) ki | Just ki' <- coreKind ki -> Just (nm, ki')
  PatTypeSig _ (PVar _ nm) ki | isKind ki -> Just (nm, ki)
  _ -> Nothing
 where
  isKind = \case
    TyParen _ t -> isKind t
    TyCon _ (UnQual _ (Ident _ s)) -> s `elem` ["Type", "Nat", "Symbol"]
    TyFun _ t1 t2 -> isKind t1 && isKind t2
    _ -> False

-- | The kind @k@ of an annotation @Core.Kind k@, which marks the binder of a
-- type variable whose kind is not one that 'kindBinderPat' recognizes, such
-- as a type variable: @\\(k :: Type) (a :: Core.Kind k) -> ..@
coreKind :: Type l -> Maybe (Type l)
coreKind = \case
  TyParen _ t -> coreKind t
  TyApp _ (TyCon _ (Qual _ (ModuleName _ "Core") (Ident _ "Kind"))) ki -> Just ki
  _ -> Nothing

-- | A let-expression: recursive, or non-recursive if all its bindings have a
-- bang
spLet :: (HasCallStack, Show l) => PEnv -> [Decl l] -> Exp l -> P TmB
spLet env0@(PEnv tys tms sigs0 dcs) decls body = do
  sigs <- IntMap.fromList . concat <$> sequence
    [ (\ty -> [ (nameKey (parseName nm), ty) | nm <- nms ]) <$> spTy env0 t
    | TypeSig _ nms t <- decls ]
  let env = PEnv tys tms (IntMap.union sigs sigs0) dcs
      binds = [ (p, e) | PatBind _ p (UnGuardedRhs _ e) Nothing <- decls ]
      others = [ d | d <- decls, not (isTypeSig d), not (isPatBind d) ]
  Monad.unless (null others) (error ("parseTestTerm: unsupported declarations: " <> show others))
  case (all (isBang . fst) binds, any (isBang . fst) binds) of
    (True, _) | not (null binds) -> nonRec env binds
    (_, False) -> do
      bs <- mapM (spIdBinder env . fst) binds
      let env' = List.foldl' (\acc (k, _, _) -> bindTmKey k acc) env bs
      rhss <- mapM (spTm env' . snd) binds
      letrecB [ (k, nm, ty, rhs) | ((k, nm, ty), rhs) <- zip bs rhss ] <$> spTm env' body
    _ -> error "parseTestTerm: a let-expression with some, but not all bindings with a bang"
 where
  nonRec env [] = spTm env body
  nonRec env ((p, e) : rest) = do
    rhs <- spTm env e
    (k, nm, ty) <- spIdBinder env (unBang p)
    letB k nm ty rhs <$> nonRec (bindTmKey k env) rest

  isBang = \case
    PParen _ p -> isBang p
    PBangPat {} -> True
    _ -> False

  unBang = \case
    PParen _ p -> unBang p
    PBangPat _ p -> p
    p -> p

  isTypeSig = \case
    TypeSig {} -> True
    _ -> False

  isPatBind = \case
    PatBind {} -> True
    _ -> False

-- | A case-expression, with the type of its alternatives
spCase :: (HasCallStack, Show l) => PEnv -> Exp l -> [Alt l] -> Type l -> P TmB
spCase env scrut alts ty = caseB <$> spTm env scrut <*> spTy env ty <*> mapM pAlt alts
 where
  pAlt = \case
    Alt _ p (UnGuardedRhs _ rhs) Nothing -> pPat p rhs
    a -> error ("parseTestTerm: unsupported alternative: " <> show a)

  pPat p rhs = case p of
    PParen _ q -> pPat q rhs
    PWildCard _ -> defaultAltB <$> spTm env rhs
    PLit _ sign (Int _ i _) ->
      litAltB (C.IntLiteral (case sign of Negative _ -> negate i; Signless _ -> i)) <$> spTm env rhs
    PApp _ qn ps -> do
      let dc = resolveDataConIn (testDefDataCons (_peDefs env)) (qNameString qn)
          nExt = case C.dcSig dc of DataConSig _ ext _ -> teleLength ext
          (exPs, idPs) = List.splitAt nExt ps
      Monad.unless (length exPs == nExt && all (Maybe.isJust . kindBinderPat) exPs) $
        error [I.i|
          parseTestTerm: the data constructor #{C.nameOcc (C.dcName dc)} has
          #{nExt} existentially quantified type variables, so its pattern
          must start with as many binders annotated with a kind, e.g.
          (b :: Type).
        |]
      exTvs env exPs $ \env1 tvs -> do
        bs <- mapM (spIdBinder env1) idPs
        let env2 = List.foldl' (\acc (k, _, _) -> bindTmKey k acc) env1 bs
        dataAltB dc tvs bs <$> spTm env2 rhs
    _ -> error ("parseTestTerm: unsupported pattern: " <> show p)

  -- The existentially quantified type variables of a pattern, each in the
  -- scope of the ones before it
  exTvs env1 [] k = k env1 []
  exTvs env1 (q : qs) k = case kindBinderPat q of
    Just (nm0, ki0) -> do
      let nm = parseName nm0
          key = nameKey nm
      ki <- spTy env1 ki0
      exTvs (bindTyKey key env1) qs $ \env2 tvs -> k env2 ((key, nm, ki) : tvs)
    Nothing -> error "parseTestTerm: impossible"

-- | A data constructor of scoped Core: its name (see 'parseTestTerm' and
-- 'testDefDataCons'), unique, tag (1 for the first constructor of its type),
-- universally and existentially quantified type variables ('tyVarD' or
-- 'kindedTyVarD'), and the types of its fields and its result, which mention
-- them by their identifiers. Its fields are lazy.
testDataCon
  :: HasCallStack
  => String -> Unique -> Int -> [ScopeDecl] -> [ScopeDecl] -> [TestType] -> TestType
  -> C.DataCon
testDataCon nm u tag univ ext args res =
  S.mkDataCon (C.mkUnsafeName C.User (ghcName (Text.pack nm)) u) tag ty sig (map (const C.Lazy) args) []
 where
  tvs = map tvOf (univ <> ext)
  tvOf = \case
    ScopeDecl _ (DeclTyVar k n ki) -> (k, n, testTypeB ki)
    _ -> error ("testDataCon: not a type variable in the signature of " <> nm)
  ty = runClosedType
    (foldr (\(k, n, ki) -> forAllTyB k n ki) (funTyB (map testTypeB args) (testTypeB res)) tvs)
  sig = runDataConSig (map tvOf univ) (map tvOf ext) (map testTypeB args)

-- | A data type of scoped Core, with the given name (see 'resolveTyConName'),
-- type variables of kind @Type@, by their identifiers, and data constructors
testAlgTyCon :: HasCallStack => String -> [String] -> [C.DataCon] -> C.TyCon
testAlgTyCon nm tvs dcs =
  S.mkAlgTyCon tcNm (runClosedType (funTyB (map (const liftedTypeKindB) tvs) liftedTypeKindB))
    (length tvs) (C.DataTyCon dcs) False
 where
  tcNm = resolveTyConName nm

-- | The name 'parseType' produces for a type constructor whose name it
-- derives a unique from, e.g. @Int@
parseTyConName :: HasCallStack => String -> C.TyConName
parseTyConName nm = C.Name C.User (Text.pack nm) (nameToUnique nm) C.noSrcSpan

-- | The type 'parseType' produces for a type constructor whose name it derives
-- a unique from, e.g. @Int@
parseTyConTy :: HasCallStack => String -> C.Type
parseTyConTy = C.ConstTy . C.TyCon . parseTyConName

-- | The type 'parseType' produces for the type constructor @Int@
intTy :: C.Type
intTy = parseTyConTy "Int"

-- | The type constructor of 'intTy'. The default 'RewriteEnv' knows it.
intTyCon :: C.TyCon
intTyCon = S.mkPrimTyCon (parseTyConName "Int") (runClosedType typeB) 0

-- | An 'C.Id' with the given scope, name sort, human readable name, unique, and
-- type
mkId :: C.IdScope -> C.NameSort -> String -> Unique -> C.Type -> C.Id
mkId scope nmSort nm uniq typ =
  C.Id (C.mkUnsafeName nmSort (Text.pack nm) uniq) uniq typ scope

-- | A local 'C.Id'. See 'mkId'.
localId :: C.NameSort -> String -> Unique -> C.Type -> C.Id
localId = mkId C.LocalId

-- | A global 'C.Id'. See 'mkId'.
globalId :: C.NameSort -> String -> Unique -> C.Type -> C.Id
globalId = mkId C.GlobalId

-- | A local 'C.Id' of type @Int@. See 'localId'.
intId :: C.NameSort -> String -> Unique -> C.Id
intId nmSort nm uniq = localId nmSort nm uniq intTy

-- | The function type @Int -> .. -> Int@ with the given number of arguments
intFunTy :: Int -> C.Type
intFunTy n = foldr C.mkFunTy intTy (replicate n intTy)

-- * Fixtures
--
-- The data and type constructors of the tests, built with the builders of
-- "Clash.Core.Scoped.Builder". A type variable of a fixture is a 'FxTyVar',
-- whose key is its unique.

-- | A type variable of a fixture: its key, name and kind
type FxTyVar = (Key, C.Name C.Type, TyB)

-- | A type variable with the given name, unique and kind
fxTyVar :: String -> Unique -> TyB -> FxTyVar
fxTyVar nm u k = (fromIntegral u, C.mkUnsafeName C.User (Text.pack nm) u, k)

-- | A type variable of kind @Type@
fxTyVarT :: String -> Unique -> FxTyVar
fxTyVarT nm u = fxTyVar nm u typeB

-- | An occurrence of a type variable of a fixture
fxVarB :: FxTyVar -> TyB
fxVarB (k, _, _) = varTyB k

-- | A type quantified over a type variable of a fixture
fxForAllB :: FxTyVar -> TyB -> TyB
fxForAllB (k, n, ki) = forAllTyB k n ki

-- | The kinds @Type@, @Nat@ and @Symbol@
typeB, natB, symbolB :: TyB
typeB = constTyB (C.TyCon P.liftedTypeKindTyConName)
natB = constTyB (C.TyCon P.typeNatKindTyConName)
symbolB = constTyB (C.TyCon P.typeSymbolKindTyConName)

-- | @Int@
intB :: TyB
intB = tcAppB (parseTyConName "Int") []

-- | A type constructor, applied to types
tcAppB :: C.TyConName -> [TyB] -> TyB
tcAppB = tyConAppB

-- | A natural number as a type
numB :: Integer -> TyB
numB = litTyB . C.NumTy

-- | GHC's @+@ on type-level naturals
addB :: TyB -> TyB -> TyB
addB a b = tcAppB addTyConName [a, b]

-- | Unlifted equality of naturals, @a ~# b@
eqNatB :: TyB -> TyB -> TyB
eqNatB a b = tcAppB P.eqPrimTyConName [natB, natB, a, b]

-- | A data constructor with the given name, unique, tag (1 for the first
-- constructor of its type), universally and existentially quantified type
-- variables, (lazy) field types, and result type
fxDataCon
  :: String -> Unique -> C.ConTag -> [FxTyVar] -> [FxTyVar] -> [TyB] -> TyB -> C.DataCon
fxDataCon nm u tag univ ext args res =
  S.mkDataCon (C.mkUnsafeName C.User (ghcName (Text.pack nm)) u) tag ty sig (map (const C.Lazy) args) []
 where
  ty = runClosedType (foldr fxForAllB (funTyB args res) (univ <> ext))
  sig = runDataConSig univ ext args

-- | The kind @k1 -> .. -> kn -> Type@ of a type constructor with type
-- variables of kinds @k1 .. kn@
fxTyConKind :: [FxTyVar] -> TyB
fxTyConKind tvs = funTyB [k | (_, _, k) <- tvs] typeB

-- | A data type with the given name, type variables and data constructors
fxAlgTyCon :: C.TyConName -> [FxTyVar] -> [C.DataCon] -> C.TyCon
fxAlgTyCon nm tvs dcs =
  S.mkAlgTyCon nm (runClosedType (fxTyConKind tvs)) (length tvs) (C.DataTyCon dcs) False

-- | A newtype with the given name, type variables, data constructor, and the
-- type variables and representation type of its eta-reduced signature
fxNewTyCon :: C.TyConName -> [FxTyVar] -> C.DataCon -> ([FxTyVar], TyB) -> C.TyCon
fxNewTyCon nm tvs dc (etaTvs, rhs) =
  S.mkAlgTyCon nm (runClosedType (fxTyConKind tvs)) (length tvs)
    (S.mkNewTyConRhs dc (runNewTyConSig etaTvs rhs)) False

-- | A type family with the given name, kind, arity, and equations: their
-- pattern variables, left-hand sides and right-hand side
fxFunTyCon :: C.TyConName -> TyB -> Int -> [([FxTyVar], [TyB], TyB)] -> C.TyCon
fxFunTyCon nm k ar eqns =
  S.mkFunTyCon nm (runClosedType k) ar [runTyFamEqn tvs lhs rhs | (tvs, lhs, rhs) <- eqns]

-- | A primitive type constructor with the given name, kind and arity
fxPrimTyCon :: C.TyConName -> TyB -> Int -> C.TyCon
fxPrimTyCon nm k = S.mkPrimTyCon nm (runClosedType k)

-- | A type constructor name with the given human readable name and unique
mkTyConName :: String -> Unique -> C.TyConName
mkTyConName nm = C.mkUnsafeName C.User (ghcName (Text.pack nm))

-- | @Pair@, a data type with a single constructor 'pairDataCon'
pairTy :: C.Type
pairTy = parseTyConTy "Pair"

-- | @MkPair :: Int -> Int -> Pair@
pairDataCon :: C.DataCon
pairDataCon = fxDataCon "MkPair" 100 1 [] [] [intB, intB] (tcAppB (parseTyConName "Pair") [])

-- | @E@, a data type with a single constructor 'existentialDataCon'
existentialTy :: C.Type
existentialTy = parseTyConTy "E"

-- | The existentially quantified type variable @b@ of 'existentialDataCon'
existentialTyVar :: FxTyVar
existentialTyVar = fxTyVarT "b" 50

-- | @MkE :: forall b. b -> E@, a data constructor with an existentially
-- quantified type variable
existentialDataCon :: C.DataCon
existentialDataCon =
  fxDataCon "MkE" 1002 1 [] [existentialTyVar] [fxVarB existentialTyVar]
    (tcAppB (parseTyConName "E") [])

-- * Type constructors
--
-- A type constructor environment ('testTyConMap') with data types, newtypes
-- and type families, to test functions that look through them.

-- | Type variables of the type constructors
fxA, fxB :: FxTyVar
fxA = fxTyVarT "a" 3001
fxB = fxTyVarT "b" 3002

-- | The type of a type constructor
tcTy :: C.TyConName -> C.Type
tcTy = C.ConstTy . C.TyCon

-- | A type constructor applied to types
tcApp :: C.TyConName -> [C.Type] -> C.Type
tcApp = C.mkTyConApp

boolTyConName, maybeTyConName, nTyConName, pTyConName, wTyConName, fTyConName,
  maxTyConName, kTyConName, qTyConName, signalTyConName, t2TyConName,
  vTyConName :: C.TyConName
boolTyConName = mkTyConName "Bool" 3010
maybeTyConName = mkTyConName "Maybe" 3011
nTyConName = mkTyConName "N" 3012
pTyConName = mkTyConName "P" 3013
wTyConName = mkTyConName "W" 3014
fTyConName = mkTyConName "F" 3015
maxTyConName = mkTyConName "Max" 3016
kTyConName = mkTyConName "K" 3017
qTyConName = mkTyConName "Q" 3018
signalTyConName = mkTyConName "Clash.Signal.Internal.Signal" 3019
t2TyConName = mkTyConName "T2" 3024
vTyConName = mkTyConName "V" 3025

-- | @data Bool = False | True@
boolTy :: C.Type
boolTy = tcTy boolTyConName

-- | @GHC.Types.Bool@, with the uniques GHC gives it and its constructors.
-- The evaluator recognizes these constructors by their uniques, e.g. in
-- @(&&)@.
ghcBoolTyConName :: C.TyConName
ghcBoolTyConName = mkTyConName "GHC.Types.Bool" (fromGhcUnique boolTyConKey)

ghcBoolTy :: C.Type
ghcBoolTy = tcTy ghcBoolTyConName

ghcFalseDataCon, ghcTrueDataCon :: C.DataCon
ghcFalseDataCon =
  fxDataCon "GHC.Types.False" (fromGhcUnique falseDataConKey) 1 [] [] [] (tcAppB ghcBoolTyConName [])
ghcTrueDataCon =
  fxDataCon "GHC.Types.True" (fromGhcUnique trueDataConKey) 2 [] [] [] (tcAppB ghcBoolTyConName [])

-- | @data Maybe a = Nothing | Just a@
maybeTy :: C.Type -> C.Type
maybeTy a = tcApp maybeTyConName [a]

nothingDataCon, justDataCon :: C.DataCon
nothingDataCon = fxDataCon "Nothing" 3020 1 [fxA] [] [] (tcAppB maybeTyConName [fxVarB fxA])
justDataCon = fxDataCon "Just" 3021 2 [fxA] [] [fxVarB fxA] (tcAppB maybeTyConName [fxVarB fxA])

-- | The tuple type constructor @TupN@ of the given arity (at most 9), with a
-- single data constructor @MkTupN :: forall t1 .. tN. t1 -> .. -> tN -> TupN
-- t1 .. tN@, for 'Clash.Rewrite.Types.tupleTcCache'
tupleTyCon :: Int -> C.TyCon
tupleTyCon n = fxAlgTyCon nm tvs [dc]
 where
  nm = mkTyConName ("Tup" <> show n) (3500 + fromIntegral n)
  tvs = [fxTyVarT ("t" <> show i) (3600 + 10 * fromIntegral n + fromIntegral i) | i <- [1 .. n]]
  dc = fxDataCon ("MkTup" <> show n) (3700 + fromIntegral n) 1 tvs []
         (map fxVarB tvs) (tcAppB nm (map fxVarB tvs))

-- | Add the tuple type constructors of arity 2 up to the given one
-- ('tupleTyCon') to a 'RewriteEnv': to its type constructors, and to its
-- 'Clash.Rewrite.Types.tupleTcCache'
withTupleTyCons :: Int -> RewriteEnv -> RewriteEnv
withTupleTyCons maxN env =
  withTyConMap tcm env{_clashEnv = clashEnv0{envTupleTyCons = tups}}
 where
  clashEnv0 = _clashEnv env
  tcs = [(n, tupleTyCon n) | n <- [2 .. maxN]]
  tcm = foldr (\(_, tc) -> UniqMap.insert (C.tyConName tc) tc) (envTyConMap clashEnv0) tcs
  tups = IntMap.union (IntMap.fromList [(n, C.tyConName tc) | (n, tc) <- tcs])
                      (envTupleTyCons clashEnv0)

-- | @newtype N a = MkN (Maybe a)@
nTy :: C.Type -> C.Type
nTy a = tcApp nTyConName [a]

-- | @newtype P a = MkP (forall b. a -> b)@, whose representation type binds a
-- type variable
pTy :: C.Type -> C.Type
pTy a = tcApp pTyConName [a]

-- | @newtype W = MkW (Int -> Int)@
wTy :: C.Type
wTy = tcTy wTyConName

-- | A natural number as a type
numTy :: Integer -> C.Type
numTy = C.LitTy . C.NumTy

-- | GHC's @+@ on type-level naturals, which 'C.reduceTypeFamily' and
-- "Clash.Core.EqSolver" recognize by its unique
addTy :: C.Type -> C.Type -> C.Type
addTy a b = tcApp addTyConName [a, b]

addTyConName :: C.TyConName
addTyConName = mkTyConName "GHC.TypeNats.+" (fromGhcUnique typeNatAddTyFamNameKey)

cmpNatTyConName :: C.TyConName
cmpNatTyConName = mkTyConName "GHC.TypeNats.CmpNat" (fromGhcUnique typeNatCmpTyFamNameKey)

unconsSymbolTyConName :: C.TyConName
unconsSymbolTyConName =
  mkTyConName "GHC.TypeLits.UnconsSymbol" (fromGhcUnique typeUnconsSymbolTyFamNameKey)

-- | Unlifted equality, @a ~# b@, of types of kind @k@, the type of the
-- equality coercions that "Clash.Core.EqSolver" recognizes
eqTy :: C.Kind -> C.Type -> C.Type -> C.Type
eqTy k a b = foldl C.AppTy C.eqPrimTy [k, k, a, b]

-- | The name of the type constructor of 'liftedEqTy'. The parser of scoped
-- Core parses @a ~ b@ as its application to @Type@, @a@ and @b@.
liftedEqTyConName :: C.TyConName
liftedEqTyConName = C.mkUnsafeName C.User (Text.pack "~") (fromGhcUnique eqTyConKey)

-- | @data V n where VNil :: (n ~ 0) => V n; VCons :: forall m. (n ~ m + 1) =>
-- Int -> V m -> V n@, a data type with equality constraints and an
-- existentially quantified type variable, like @Vec@
vTy :: C.Type -> C.Type
vTy n = tcApp vTyConName [n]

fxN, fxM :: FxTyVar
fxN = fxTyVar "n" 3026 natB
fxM = fxTyVar "m" 3027 natB

vNilDataCon, vConsDataCon :: C.DataCon
vNilDataCon =
  fxDataCon "VNil" 3028 1 [fxN] [] [eqNatB (fxVarB fxN) (numB 0)] (tcAppB vTyConName [fxVarB fxN])
vConsDataCon =
  fxDataCon "VCons" 3029 2 [fxN] [fxM]
    [ eqNatB (fxVarB fxN) (addB (fxVarB fxM) (numB 1))
    , intB
    , tcAppB vTyConName [fxVarB fxM] ]
    (tcAppB vTyConName [fxVarB fxN])

-- | The name of @Vec@. The primitive reductions recognize vectors by this name.
vecTcNm :: C.TyConName
vecTcNm = mkTyConName "Clash.Sized.Vector.Vec" 2000

-- | @Vec n a@
vecTy :: C.Type -> C.Type -> C.Type
vecTy n a = tcApp vecTcNm [n, a]

-- | The universally quantified type variables of the constructors of @Vec@,
-- and the existentially quantified one of 'consDc'
vecN, vecA, vecM :: FxTyVar
vecN = fxTyVarT "n" 2001
vecA = fxTyVarT "a" 2002
vecM = fxTyVarT "m" 2003

-- | @Nil :: forall n a. (n ~ 0) => Vec n a@
nilDc :: C.DataCon
nilDc =
  fxDataCon "Nil" 2005 1 [vecN, vecA] []
    [eqNatB (fxVarB vecN) (numB 0)]
    (tcAppB vecTcNm [fxVarB vecN, fxVarB vecA])

-- | @Cons :: forall n a. forall m. (n ~ m + 1) => a -> Vec m a -> Vec n a@
consDc :: C.DataCon
consDc =
  fxDataCon "Cons" 2006 2 [vecN, vecA] [vecM]
    [ eqNatB (fxVarB vecN) (addB (fxVarB vecM) (numB 1))
    , fxVarB vecA
    , tcAppB vecTcNm [fxVarB vecM, fxVarB vecA] ]
    (tcAppB vecTcNm [fxVarB vecN, fxVarB vecA])

-- | The name of @RTree@. The primitive reductions recognize trees by this
-- name.
rtreeTcNm :: C.TyConName
rtreeTcNm = mkTyConName "Clash.Sized.RTree.RTree" 2010

-- | @RTree d a@
rtreeTy :: C.Type -> C.Type -> C.Type
rtreeTy d a = tcApp rtreeTcNm [d, a]

-- | The universally quantified type variables of the constructors of @RTree@,
-- and the existentially quantified one of 'brDc'
rtreeD, rtreeA, rtreeD1 :: FxTyVar
rtreeD = fxTyVarT "d" 2011
rtreeA = fxTyVarT "a" 2012
rtreeD1 = fxTyVarT "d1" 2013

-- | @LR :: forall d a. (d ~ 0) => a -> RTree d a@
lrDc :: C.DataCon
lrDc =
  fxDataCon "LR" 2014 1 [rtreeD, rtreeA] []
    [eqNatB (fxVarB rtreeD) (numB 0), fxVarB rtreeA]
    (tcAppB rtreeTcNm [fxVarB rtreeD, fxVarB rtreeA])

-- | @BR :: forall d a. forall d1. (d ~ d1 + 1) => RTree d1 a -> RTree d1 a ->
-- RTree d a@
brDc :: C.DataCon
brDc =
  fxDataCon "BR" 2015 2 [rtreeD, rtreeA] [rtreeD1]
    [ eqNatB (fxVarB rtreeD) (addB (fxVarB rtreeD1) (numB 1))
    , tcAppB rtreeTcNm [fxVarB rtreeD1, fxVarB rtreeA]
    , tcAppB rtreeTcNm [fxVarB rtreeD1, fxVarB rtreeA] ]
    (tcAppB rtreeTcNm [fxVarB rtreeD, fxVarB rtreeA])

-- | @SNat n@, with @SNat :: forall n. Natural -> SNat n@. The primitive
-- reductions find this constructor through the types of the functions that
-- @dfold@, @dtfold@ and @tdfold@ apply to @SNat@s.
snatTcNm :: C.TyConName
snatTcNm = mkTyConName "Clash.Promoted.Nat.SNat" 2030

snatTy :: C.Type -> C.Type
snatTy n = tcApp snatTcNm [n]

snatN :: FxTyVar
snatN = fxTyVar "n" 2031 natB

snatDc :: C.DataCon
snatDc =
  fxDataCon "SNat" 2032 1 [snatN] [] [tcAppB P.naturalPrimTyConName []]
    (tcAppB snatTcNm [fxVarB snatN])

-- | GHC's boxed @Int@: @data Int = I# Int#@, unlike 'intTy'. The primitive
-- reductions of @replace_int@ and @index_int@ compare the index with boxed
-- literals.
hIntTcNm :: C.TyConName
hIntTcNm = mkTyConName "GHC.Types.Int" 2033

hIntTy :: C.Type
hIntTy = tcTy hIntTcNm

iHashDc :: C.DataCon
iHashDc = fxDataCon "GHC.Types.I#" 2034 1 [] [] [tcAppB P.intPrimTyConName []] (tcAppB hIntTcNm [])

-- | A proof of an upper bound, as @dfold@ takes one: @data UBP l n = MkUBP@
ubpTcNm :: C.TyConName
ubpTcNm = mkTyConName "UBP" 2035

ubpL, ubpN :: FxTyVar
ubpL = fxTyVar "l" 2036 natB
ubpN = fxTyVar "n" 2037 natB

ubpTy :: C.Type -> C.Type -> C.Type
ubpTy l n = tcApp ubpTcNm [l, n]

ubpDc :: C.DataCon
ubpDc = fxDataCon "MkUBP" 2038 1 [ubpL, ubpN] [] [] (tcAppB ubpTcNm [fxVarB ubpL, fxVarB ubpN])

-- | The name of @Index@, as the function @imap@ takes indexes it
indexTcNm :: C.TyConName
indexTcNm = mkTyConName "Clash.Sized.Internal.Index.Index" 2039

-- | The @Functor@ and @Applicative@ dictionaries of @traverse#@:
--
-- > data Functor f = CFunctor (forall a b. (a -> b) -> f a -> f b) Int
-- > data Applicative f =
-- >   CApplicative (Functor f) (forall a. a -> f a)
-- >     (forall a b. f (a -> b) -> f a -> f b) Int Int Int
--
-- The fields of type @Int@ stand in for the methods the reduction of
-- @traverse#@ doesn't use.
functorTcNm, applicativeTcNm :: C.TyConName
functorTcNm = mkTyConName "GHC.Base.Functor" 2040
applicativeTcNm = mkTyConName "GHC.Base.Applicative" 2041

dictF, dictA, dictB :: FxTyVar
dictF = fxTyVar "f" 2042 (funTyB [typeB] typeB)
dictA = fxTyVarT "a" 2043
dictB = fxTyVarT "b" 2044

functorTy, applicativeTy :: C.Type -> C.Type
functorTy f = tcApp functorTcNm [f]
applicativeTy f = tcApp applicativeTcNm [f]

functorDc, applicativeDc :: C.DataCon
functorDc =
  fxDataCon "CFunctor" 2045 1 [dictF] []
    [ fxForAllB dictA (fxForAllB dictB
        (funTyB [funTyB [a] b, appTyB f a] (appTyB f b)))
    , intB ]
    (tcAppB functorTcNm [f])
 where
  f = fxVarB dictF
  a = fxVarB dictA
  b = fxVarB dictB
applicativeDc =
  fxDataCon "CApplicative" 2046 1 [dictF] []
    [ tcAppB functorTcNm [f]
    , fxForAllB dictA (funTyB [a] (appTyB f a))
    , fxForAllB dictA (fxForAllB dictB
        (funTyB [appTyB f (funTyB [a] b), appTyB f a] (appTyB f b)))
    , intB, intB, intB ]
    (tcAppB applicativeTcNm [f])
 where
  f = fxVarB dictF
  a = fxVarB dictA
  b = fxVarB dictB

-- | GHC's boxed pairs, by the name under which "Clash.GHC.GHC2Core" gives
-- them, and the reductions of primitives look them up
-- ('Clash.Core.Scoped.Evaluator.Primitives.Util.tupleTyConName')
ghcPairTcNm :: C.TyConName
ghcPairTcNm = tupleTyConName 2

-- | @(a, b)@
ghcPairTy :: C.Type -> C.Type -> C.Type
ghcPairTy a b = tcApp ghcPairTcNm [a, b]

-- | @(,) :: forall a b. a -> b -> (a, b)@
ghcPairDc :: C.DataCon
ghcPairDc =
  fxDataCon "GHC.Tuple.(,)" 2051 1 [pairA, pairB] [] [fxVarB pairA, fxVarB pairB]
    (tcAppB ghcPairTcNm [fxVarB pairA, fxVarB pairB])

pairA, pairB :: FxTyVar
pairA = fxTyVarT "a" 2052
pairB = fxTyVarT "b" 2053

-- | @Ordering@, which reductions of comparisons of numbers return
orderingTcNm :: C.TyConName
orderingTcNm = mkTyConName "GHC.Types.Ordering" 2060

orderingTy :: C.Type
orderingTy = tcTy orderingTcNm

ltDc, eqDc, gtDc :: C.DataCon
ltDc = fxDataCon "GHC.Types.LT" 2061 1 [] [] [] (tcAppB orderingTcNm [])
eqDc = fxDataCon "GHC.Types.EQ" 2062 2 [] [] [] (tcAppB orderingTcNm [])
gtDc = fxDataCon "GHC.Types.GT" 2063 3 [] [] [] (tcAppB orderingTcNm [])

-- | The boxed @Double@
doubleTcNm :: C.TyConName
doubleTcNm = mkTyConName "GHC.Types.Double" 2064

doubleTy :: C.Type
doubleTy = tcTy doubleTcNm

-- | @D# :: Double# -> Double@
doubleDc :: C.DataCon
doubleDc =
  fxDataCon "GHC.Types.D#" 2065 1 [] [] [tcAppB P.doublePrimTyConName []] (tcAppB doubleTcNm [])

-- | @T3@, a product of three types, like an unboxed triple
t3TyConName :: C.TyConName
t3TyConName = mkTyConName "T3" 2066

t3Ty :: C.Type -> C.Type -> C.Type -> C.Type
t3Ty a b c = tcApp t3TyConName [a, b, c]

-- | @MkT3 :: a -> b -> c -> T3 a b c@
t3DataCon :: C.DataCon
t3DataCon = fxDataCon "MkT3" 2067 1 [fxA, fxB, t3C] [] [fxVarB fxA, fxVarB fxB, fxVarB t3C]
  (tcAppB t3TyConName [fxVarB fxA, fxVarB fxB, fxVarB t3C])

t3C :: FxTyVar
t3C = fxTyVarT "c" 2068

-- | @T2@ applied to two types
t2Ty :: C.Type -> C.Type -> C.Type
t2Ty a b = tcApp t2TyConName [a, b]

-- | @MkT2 :: a -> b -> T2 a b@
t2DataCon :: C.DataCon
t2DataCon = fxDataCon "MkT2" 3033 1 [fxA, fxB] [] [fxVarB fxA, fxVarB fxB]
  (tcAppB t2TyConName [fxVarB fxA, fxVarB fxB])

-- | @SomeNat@, a type-level natural hidden behind an existential
someNatTcNm :: C.TyConName
someNatTcNm = mkTyConName "GHC.TypeNats.SomeNat" 2069

someNatTy :: C.Type
someNatTy = tcTy someNatTcNm

-- | @SomeNat :: forall n. KnownNat n => Proxy n -> SomeNat@
someNatDc :: C.DataCon
someNatDc =
  fxDataCon "GHC.TypeNats.SomeNat" 2070 1 [] [someNatN]
    [tcAppB P.naturalPrimTyConName [], tcAppB proxyTcNm [natB, fxVarB someNatN]]
    (tcAppB someNatTcNm [])

someNatN :: FxTyVar
someNatN = fxTyVar "n" 2071 natB

-- | @Proxy@, poly-kinded
proxyTcNm :: C.TyConName
proxyTcNm = mkTyConName "Data.Proxy.Proxy" 2072

kindedProxyTy :: C.Kind -> C.Type -> C.Type
kindedProxyTy k a = tcApp proxyTcNm [k, a]

-- | @Proxy :: forall k (a :: k). Proxy a@. The kind of @a@ is @Type@ here,
-- rather than @k@, because the kind of a type constructor can't mention its
-- type variables.
proxyDc :: C.DataCon
proxyDc =
  fxDataCon "Data.Proxy.Proxy" 2073 1 [proxyK, proxyA] [] []
    (tcAppB proxyTcNm [fxVarB proxyK, fxVarB proxyA])

proxyK, proxyA :: FxTyVar
proxyK = fxTyVarT "k" 2074
proxyA = fxTyVarT "a" 2075

-- | The names of the types of clock, reset and enable lines. Clash recognizes
-- them by their names.
clockTcNm, resetTcNm, enableTcNm :: C.TyConName
clockTcNm = mkTyConName "Clash.Signal.Internal.Clock" 2020
resetTcNm = mkTyConName "Clash.Signal.Internal.Reset" 2021
enableTcNm = mkTyConName "Clash.Signal.Internal.Enable" 2022

-- | The domain @System@
systemDomTy :: C.Type
systemDomTy = C.LitTy (C.SymTy "System")

-- | @Clock System@
clockTy :: C.Type
clockTy = tcApp clockTcNm [systemDomTy]

-- | @ClkPair@, a product type with a clock, which Clash splits
clkPairTcNm :: C.TyConName
clkPairTcNm = mkTyConName "ClkPair" 2024

clkPairTy :: C.Type
clkPairTy = tcTy clkPairTcNm

-- | @MkClkPair :: Clock System -> Int -> ClkPair@
clkPairDc :: C.DataCon
clkPairDc =
  fxDataCon "MkClkPair" 2025 1 [] [] [tcAppB clockTcNm [litTyB (C.SymTy "System")], intB]
    (tcAppB clkPairTcNm [])

-- | The type constructors of the types above, and those of 'C.tysPrimMap',
-- 'intTyCon' and 'pairTy'
testTyConMap :: C.TyConMap
testTyConMap = UniqMap.fromList [(C.tyConName tc, tc) | tc <- tcs]
 where
  tcs = UniqMap.elems P.tysPrimMap <>
    [ intTyCon
    , fxAlgTyCon (parseTyConName "Pair") [] [pairDataCon]
    , fxAlgTyCon boolTyConName [] [falseDc, trueDc]
    , fxAlgTyCon ghcBoolTyConName [] [ghcFalseDataCon, ghcTrueDataCon]
    , fxAlgTyCon maybeTyConName [fxA] [nothingDataCon, justDataCon]
    , fxNewTyCon nTyConName [fxA]
        (fxDataCon "MkN" 3030 1 [fxA] [] [maybeA] (tcAppB nTyConName [a]))
        ([fxA], maybeA)
    , fxNewTyCon pTyConName [fxA]
        (fxDataCon "MkP" 3031 1 [fxA] [] [pRhs] (tcAppB pTyConName [a]))
        ([fxA], pRhs)
    , fxNewTyCon wTyConName []
        (fxDataCon "MkW" 3032 1 [] [] [funTyB [intB] intB] (tcAppB wTyConName []))
        ([], funTyB [intB] intB)
    , fxFunTyCon fTyConName (funTyB [typeB] typeB) 1
        [ ([], [intB], tcAppB boolTyConName [])
        , ([fxA], [maybeA], a)
        , ([fxA], [a], tcAppB nTyConName [a]) ]
    , fxFunTyCon maxTyConName (funTyB [typeB, typeB] typeB) 2
        [ ([fxA], [a, a], a)
        , ([fxA, fxB], [a, b], tcAppB maybeTyConName [b]) ]
    , fxFunTyCon kTyConName (funTyB [typeB] typeB) 0
        [ ([], [], tcAppB maybeTyConName []) ]
    , fxFunTyCon qTyConName (funTyB [typeB] typeB) 1
        [ ([fxA], [a], pRhs) ]
    , fxAlgTyCon signalTyConName [fxA, fxB] []
    , fxFunTyCon addTyConName natNatNat 2 []
    , fxFunTyCon cmpNatTyConName natNatNat 2 []
    , fxFunTyCon unconsSymbolTyConName
        (funTyB [symbolB]
          (tcAppB maybeTyConName [tcAppB t2TyConName [tcAppB P.charPrimTyConName [], symbolB]]))
        1 []
    , fxAlgTyCon t2TyConName [fxA, fxB] [t2DataCon]
    , S.mkAlgTyCon vTyConName (runClosedType (funTyB [natB] typeB)) 1
        (C.DataTyCon [vNilDataCon, vConsDataCon]) False
    , fxAlgTyCon vecTcNm [vecN, vecA] [nilDc, consDc]
    , fxAlgTyCon rtreeTcNm [rtreeD, rtreeA] [lrDc, brDc]
    , fxAlgTyCon snatTcNm [snatN] [snatDc]
    , fxAlgTyCon hIntTcNm [] [iHashDc]
    , fxAlgTyCon ubpTcNm [ubpL, ubpN] [ubpDc]
    , fxPrimTyCon indexTcNm (funTyB [natB] typeB) 1
    , fxAlgTyCon functorTcNm [dictF] [functorDc]
    , fxAlgTyCon applicativeTcNm [dictF] [applicativeDc]
    , fxAlgTyCon ghcPairTcNm [pairA, pairB] [ghcPairDc]
    , fxPrimTyCon clockTcNm typeB 1
    , fxPrimTyCon resetTcNm typeB 1
    , fxPrimTyCon enableTcNm typeB 1
    , fxAlgTyCon clkPairTcNm [] [clkPairDc]
    , fxAlgTyCon (parseTyConName "E") [] [existentialDataCon]
    , fxAlgTyCon orderingTcNm [] [ltDc, eqDc, gtDc]
    , fxAlgTyCon doubleTcNm [] [doubleDc]
    , fxAlgTyCon t3TyConName [fxA, fxB, t3C] [t3DataCon]
    , fxAlgTyCon someNatTcNm [] [someNatDc]
    , fxAlgTyCon proxyTcNm [proxyK, proxyA] [proxyDc]
    ] <>
    -- Promoted data constructors that type families return. Their kinds don't
    -- matter here.
    [ fxPrimTyCon nm typeB 0
    | nm <-
        [ mkTyConName "GHC.Types.LT" (fromGhcUnique ordLTDataConKey)
        , mkTyConName "GHC.Types.EQ" (fromGhcUnique ordEQDataConKey)
        , mkTyConName "GHC.Types.GT" (fromGhcUnique ordGTDataConKey)
        , mkTyConName "Nothing" 3020
        , mkTyConName "Just" 3021
        , mkTyConName "MkT2" 3033
        ]
    ]

  natNatNat = funTyB [natB, natB] natB

  falseDc = fxDataCon "False" 3034 1 [] [] [] (tcAppB boolTyConName [])
  trueDc = fxDataCon "True" 3035 2 [] [] [] (tcAppB boolTyConName [])

  a = fxVarB fxA
  b = fxVarB fxB
  maybeA = tcAppB maybeTyConName [a]

  -- forall b. a -> b
  pRhs = fxForAllB fxB (funTyB [a] b)

bitVectorTyConName, unsignedTyConName, signedTyConName :: C.TyConName
bitVectorTyConName = mkTyConName "Clash.Sized.Internal.BitVector.BitVector" 3100
unsignedTyConName = mkTyConName "Clash.Sized.Internal.Unsigned.Unsigned" 3101
signedTyConName = mkTyConName "Clash.Sized.Internal.Signed.Signed" 3102

-- | @BitVector n@ and @Unsigned n@
bitVectorTy, unsignedTy :: Integer -> C.Type
bitVectorTy n = tcApp bitVectorTyConName [numTy n]
unsignedTy n = tcApp unsignedTyConName [numTy n]

zwTyConName, proxyTyConName, tProxyTyConName, exZwTyConName, streamTyConName :: C.TyConName
zwTyConName = mkTyConName "ZW" 3104
proxyTyConName = mkTyConName "Proxy" 3105
tProxyTyConName = mkTyConName "TProxy" 3106
exZwTyConName = mkTyConName "ExZW" 3107
streamTyConName = mkTyConName "Stream" 3108

-- | @data ZW = MkZW (BitVector 0) (Unsigned 0)@
zwTy :: C.Type
zwTy = tcTy zwTyConName

zwDataCon :: C.DataCon
zwDataCon =
  fxDataCon "MkZW" 3110 1 [] []
    [tcAppB bitVectorTyConName [numB 0], tcAppB unsignedTyConName [numB 0]]
    (tcAppB zwTyConName [])

-- | @data Proxy (n :: Nat) = Proxy@
proxyTy :: C.Type -> C.Type
proxyTy n = tcApp proxyTyConName [n]

proxyDataCon :: C.DataCon
proxyDataCon = fxDataCon "Proxy" 3111 1 [zwN] [] [] (tcAppB proxyTyConName [fxVarB zwN])

-- | @data TProxy (n :: Nat) = MkTProxy (Proxy n)@: a field type that mentions
-- a type variable
tProxyTy :: C.Type -> C.Type
tProxyTy n = tcApp tProxyTyConName [n]

tProxyDataCon :: C.DataCon
tProxyDataCon =
  fxDataCon "MkTProxy" 3112 1 [zwN] [] [tcAppB proxyTyConName [fxVarB zwN]]
    (tcAppB tProxyTyConName [fxVarB zwN])

-- | @data ExZW = forall b. MkExZW (BitVector 0)@: a constructor with an
-- existentially quantified type variable
exZwTy :: C.Type
exZwTy = tcTy exZwTyConName

exZwDataCon :: C.DataCon
exZwDataCon =
  fxDataCon "MkExZW" 3113 1 [] [zwB] [tcAppB bitVectorTyConName [numB 0]] (tcAppB exZwTyConName [])

-- | @data Stream a = a :> Stream a@: a recursive type, like @Signal@
streamTy :: C.Type -> C.Type
streamTy a = tcApp streamTyConName [a]

streamDataCon :: C.DataCon
streamDataCon =
  fxDataCon ":>" 3114 1 [zwA] [] [fxVarB zwA, tcAppB streamTyConName [fxVarB zwA]]
    (tcAppB streamTyConName [fxVarB zwA])

-- | Type variables of the zero-width types
zwN, zwA, zwB :: FxTyVar
zwN = fxTyVar "n" 3115 natB
zwA = fxTyVarT "a" 3116
zwB = fxTyVarT "b" 3117

-- | 'testTyConMap', with the zero-width types
zeroWidthTyConMap :: C.TyConMap
zeroWidthTyConMap = UniqMap.fromList [(C.tyConName tc, tc) | tc <- tcs]
 where
  tcs = UniqMap.elems testTyConMap <>
    [ fxAlgTyCon nm [zwN] [] | nm <- [bitVectorTyConName, unsignedTyConName, signedTyConName, indexTcNm] ] <>
    [ fxAlgTyCon zwTyConName [] [zwDataCon]
    , fxAlgTyCon proxyTyConName [zwN] [proxyDataCon]
    , fxAlgTyCon tProxyTyConName [zwN] [tProxyDataCon]
    , fxAlgTyCon exZwTyConName [] [exZwDataCon]
    , fxAlgTyCon streamTyConName [zwA] [streamDataCon]
    ]

-- | A 'C.TyVar' with the given name sort, human readable name, unique, and
-- kind
kindedTyVar :: C.NameSort -> String -> Unique -> C.Kind -> C.TyVar
kindedTyVar nmSort nm uniq =
  C.TyVar (C.mkUnsafeName nmSort (Text.pack nm) uniq) uniq

-- | A 'C.TyVar' of kind 'C.liftedTypeKind', the only kind 'parseTyVar' produces.
-- See 'kindedTyVar'.
tyVar :: C.NameSort -> String -> Unique -> C.TyVar
tyVar nmSort nm uniq = kindedTyVar nmSort nm uniq C.liftedTypeKind

-- | A global binding of the given term, of a function without an inline
-- pragma, e.g. of a term of scoped Core
mkGlobalBinding :: GlobalId -> a -> Binding a
mkGlobalBinding i t = Binding
  { bindingId = i
  , bindingLoc = C.noSrcSpan
  , bindingSpec = NoUserInlinePrag
  , bindingIsPrim = IsFun
  , bindingTerm = t
  , bindingRecursive = False
  }

-- | A reference to a local variable of type @Int@. See 'intId'.
intVar :: C.NameSort -> String -> Unique -> C.Term
intVar nmSort nm uniq = C.Var (intId nmSort nm uniq)

-- | A reference to a global variable of type @Int@. See 'globalId'.
globalIntVar :: C.NameSort -> String -> Unique -> C.Term
globalIntVar nmSort nm uniq = C.Var (globalId nmSort nm uniq intTy)

-- | A reference to a variable without a declared type. See 'freeVarType'.
freeVar :: C.NameSort -> String -> Unique -> C.Term
freeVar nmSort nm uniq = C.Var (localId nmSort nm uniq freeVarType)

-- | The first /n/ uniques a supply hands out. Tests use these to give a
-- binder the unique a transformation would otherwise pick for a new binder.
firstUniques :: Int -> Supply -> [Unique]
firstUniques n = take n . go
 where
  go s0 = let (u, s1) = freshId s0 in u : go s1

-- | An 'Evaluator' that only replaces a local variable by its heap binding.
-- Unlike Clash's evaluator, which lives in @clash-ghc@, it doesn't reduce
-- anything else, which is enough to test which bindings reach the heap.
heapLookupEvaluator :: Evaluator
heapLookupEvaluator = Evaluator
  { step = \m _tcm -> case mTerm m of
      C.Var i | Just e <- heapLookup C.LocalId i m ->
        -- Delete the binding, so 'let x = x in x' doesn't loop
        Just (setTerm e (heapDelete C.LocalId i m))
      _ -> Nothing
  , unwind = \_v _m _tcm -> Nothing
  , primStep = \_tcm _forcePrims _p _tys _vs _m -> Nothing
  , primUnwind = \_tcm _p _tys _vs _v _tms _m -> Nothing
  }

-- | Pretty print, always showing uniques (regardless of @CLASH_PPR_UNIQUES@)
-- but no types. Tests on scoping are about binders that share a name but not a
-- unique, or vice versa.
showPprU :: PrettyPrec p => p -> String
showPprU = showPpr' PrettyOptions
  { displayUniques = True
  , displayTypes = False
  , displayQualifiers = False
  , displayTicks = False
  }

-- | Assert that two terms, or two types, are alpha equivalent: 'Eq' on
-- 'C.Term' and 'C.Type' is alpha equivalence. Shows both, with uniques, if
-- they're not.
assertAlphaEq :: (HasCallStack, Eq a, PrettyPrec a) => a -> a -> Assertion
assertAlphaEq expected actual =
  assertBool
    ("Expected (up to alpha equivalence):\n" <> showPprU expected
      <> "\nbut got:\n" <> showPprU actual)
    (expected == actual)

-- | Assert that two terms are structurally equal, by comparing their 'Show'
-- output.
--
-- Note that we deliberately use neither '==' nor 'Clash.Core.Subst.eqTerm':
-- 'Eq' on 'C.Term' is alpha equivalence, and both compare names by their unique
-- alone, so they'd ignore a name's sort ('C.User', 'C.System', 'C.Internal')
-- and human readable name. 'Show' is derived everywhere, so it shows all of it.
assertStructurallyEqual :: (HasCallStack, Show a) => a -> a -> Assertion
assertStructurallyEqual expected actual =
  assertEqual "" (Pretty.ppShow expected) (Pretty.ppShow actual)

-- | Assert that forcing a value throws an 'ErrorCall' mentioning the given
-- substring
assertErrorContains
  :: (HasCallStack, NFData a, PrettyPrec a) => String -> a -> Assertion
assertErrorContains needle = assertErrorContainsIO needle . pure

-- | Assert that running an action, and forcing its result, throws an
-- 'ErrorCall' mentioning the given substring
assertErrorContainsIO
  :: (HasCallStack, NFData a, PrettyPrec a) => String -> IO a -> Assertion
assertErrorContainsIO needle action = do
  result <- try (action >>= evaluate . force)
  case result of
    Left (ErrorCall msg)
      | needle `List.isInfixOf` msg -> pure ()
      | otherwise -> assertFailure
          ("Expected an error mentioning '" <> needle <> "', but got:\n" <> msg)
    Right a -> assertFailure
      ("Expected an error mentioning '" <> needle
        <> "', but got the result:\n" <> showPprU a)

tests :: TestTree
tests = testGroup "Test.Clash.Rewrite"
  [ testGroup "parseToTerm"
      [ testCase "literal" $
          assertStructurallyEqual
            (C.Literal (C.IntLiteral 3))
            (parseToTerm "3")

      , testCase "variable" $
          assertStructurallyEqual
            (intVar C.User "x" 3)
            (parseToTerm "x_3 :: Int")

      , testCase "parentheses" $
          assertStructurallyEqual
            (intVar C.User "x" 3)
            (parseToTerm "((x_3 :: Int))")

      , testCase "application" $
          assertStructurallyEqual
            (C.App (intVar C.User "f" 0) (intVar C.User "x" 1))
            (parseToTerm "(f_0 :: Int) (x_1 :: Int)")

      , testCase "let" $
          assertStructurallyEqual
            (C.Letrec
              [ (localId C.User "x" 0 intTy, C.Literal (C.IntLiteral 5))
              , (localId C.User "x" 1 intTy, intVar C.User "x" 0)
              ]
              (intVar C.User "x" 1))
            (parseToTerm "let { x_0, x_1 :: Int; x_0 = 5; x_1 = x_0 } in x_1")

      , testCase "let with an inline type annotation" $
          assertStructurallyEqual
            (C.Letrec
              [(localId C.User "x" 0 intTy, C.Literal (C.IntLiteral 5))]
              (intVar C.User "x" 0))
            (parseToTerm "let { (x_0 :: Int) = 5 } in x_0")

      -- Bindings may refer to each other regardless of the order they're
      -- declared in, so their types have to be collected up front
      , testCase "let with a forward reference" $
          assertStructurallyEqual
            (C.Letrec
              [ (localId C.User "x" 0 intTy, intVar C.User "y" 1)
              , (localId C.User "y" 1 intTy, C.Literal (C.IntLiteral 5))
              ]
              (intVar C.User "x" 0))
            (parseToTerm "let { (x_0 :: Int) = y_1; (y_1 :: Int) = 5 } in x_0")

      , testCase "let without a type annotation" $
          assertErrorContains "forgot to (explicitely) declare"
            (parseToTerm "let { x_0 = 5 } in x_0")

      -- 0x2b == ord '+'
      , testCase "infix application" $
          assertStructurallyEqual
            (C.App
              (C.App (freeVar C.User "+" 0x2b) (intVar C.User "x" 0))
              (intVar C.User "y" 1))
            (parseToTerm "(x_0 :: Int) + (y_1 :: Int)")

      , testCase "infix application of a named function" $
          assertStructurallyEqual
            (C.App
              (C.App (freeVar C.User "add" 2) (intVar C.User "x" 0))
              (intVar C.User "y" 1))
            (parseToTerm "(x_0 :: Int) `add_2` (y_1 :: Int)")

      , testCase "free variable" $
          assertStructurallyEqual
            (C.App (freeVar C.User "f" 0) (intVar C.User "x" 1))
            (parseToTerm "f_0 (x_1 :: Int)")

      , testCase "lambda" $
          assertStructurallyEqual
            (C.Lam (localId C.User "x" 0 intTy) (intVar C.User "x" 0))
            (parseToTerm "\\(x_0 :: Int) -> x_0")

      , testCase "lambda with multiple binders" $
          assertStructurallyEqual
            (C.Lam (localId C.User "x" 0 intTy)
              (C.Lam (localId C.User "y" 1 intTy) (intVar C.User "y" 1)))
            (parseToTerm "\\(x_0 :: Int) (y_1 :: Int) -> y_1")

      , testCase "lambda with a modified binder" $
          assertStructurallyEqual
            (C.Lam (localId C.System "x" 0 intTy) (intVar C.System "x" 0))
            (parseToTerm "\\(x_S0 :: Int) -> x_S0")

      , testCase "lambda application" $
          assertStructurallyEqual
            (C.App
              (C.Lam (localId C.User "x" 0 intTy) (intVar C.User "x" 0))
              (intVar C.User "y" 1))
            (parseToTerm "(\\(x_0 :: Int) -> x_0) (y_1 :: Int)")

      , testCase "lambda binder typed by an enclosing let" $
          assertStructurallyEqual
            (C.Letrec
              [(localId C.User "x" 0 intTy, C.Literal (C.IntLiteral 5))]
              (C.Lam (localId C.User "y" 1 intTy) (intVar C.User "x" 0)))
            (parseToTerm "let { x_0, y_1 :: Int; x_0 = 5 } in \\y_1 -> x_0")

      , testCase "lambda without a type annotation" $
          assertErrorContains "forgot to (explicitely) declare"
            (parseToTerm "\\x_0 -> x_0")

      , testCase "type lambda" $
          assertStructurallyEqual
            (C.TyLam (tyVar C.User "a" 0)
              (C.Lam (localId C.User "x" 1 (C.VarTy (tyVar C.User "a" 0)))
                (C.Var (localId C.User "x" 1 (C.VarTy (tyVar C.User "a" 0))))))
            (parseToTerm "\\(a_0 :: Type) (x_1 :: a_0) -> x_1")

      , testCase "type application" $
          assertStructurallyEqual
            (C.TyApp (freeVar C.User "f" 0) intTy)
            (parseToTerm "f_0 @Int")

      , testCase "forall" $
          assertStructurallyEqual
            (C.Lam
              (localId C.User "v" 0
                (C.ForAllTy (tyVar C.User "a" 1)
                  (C.ForAllTy (tyVar C.User "b" 2)
                    (C.VarTy (tyVar C.User "b" 2)))))
              (C.Var
                (localId C.User "v" 0
                  (C.ForAllTy (tyVar C.User "a" 1)
                    (C.ForAllTy (tyVar C.User "b" 2)
                      (C.VarTy (tyVar C.User "b" 2)))))))
            (parseToTerm "\\(v_0 :: forall a_1 b_2. b_2) -> v_0")

      , testCase "type application of a type constructor" $
          let maybeInt =
                C.AppTy
                  (C.ConstTy
                    (C.TyCon
                      (C.Name C.User (Text.pack "Maybe") 9 C.noSrcSpan)))
                  intTy
          in assertStructurallyEqual
               (C.Lam
                 (localId C.User "v" 0 maybeInt)
                 (C.Var (localId C.User "v" 0 maybeInt)))
               (parseToTerm "\\(v_0 :: Maybe_9 Int) -> v_0")
      ]

  , testGroup "parseNameScope"
      [ testCase "explicit unique" $
          assertStructurallyEqual
            (intVar C.User "x" 3)
            (parseToTerm "x_3 :: Int")

      , testCase "explicit unique, user" $
          assertStructurallyEqual
            (intVar C.User "x" 3)
            (parseToTerm "x_U3 :: Int")

      , testCase "explicit unique, system" $
          assertStructurallyEqual
            (intVar C.System "x" 3)
            (parseToTerm "x_S3 :: Int")

      , testCase "explicit unique, internal" $
          assertStructurallyEqual
            (intVar C.Internal "x" 3)
            (parseToTerm "x_I3 :: Int")

      -- 0x78 == ord 'x'
      , testCase "derived unique" $
          assertStructurallyEqual
            (intVar C.User "x" 0x78)
            (parseToTerm "x :: Int")

      , testCase "derived unique, user" $
          assertStructurallyEqual
            (intVar C.User "x" 0x78)
            (parseToTerm "x_U :: Int")

      , testCase "derived unique, system" $
          assertStructurallyEqual
            (intVar C.System "x" 0x78)
            (parseToTerm "x_S :: Int")

      , testCase "derived unique, internal" $
          assertStructurallyEqual
            (intVar C.Internal "x" 0x78)
            (parseToTerm "x_I :: Int")

      , testCase "derived unique, four characters" $
          assertStructurallyEqual
            (intVar C.User "abcd" 0x61626364)
            (parseToTerm "abcd :: Int")

      , testCase "derived unique, four characters and a modifier" $
          assertStructurallyEqual
            (intVar C.System "abcd" 0x61626364)
            (parseToTerm "abcd_S :: Int")

      , testCase "name containing an underscore" $
          assertStructurallyEqual
            (intVar C.User "foo_bar" 3)
            (parseToTerm "foo_bar_3 :: Int")

      , testCase "explicit scope, local" $
          assertStructurallyEqual
            (intVar C.User "x" 3)
            (parseToTerm "x_L3 :: Int")

      , testCase "explicit scope, global" $
          assertStructurallyEqual
            (globalIntVar C.User "x" 3)
            (parseToTerm "x_G3 :: Int")

      , testCase "explicit scope, derived unique" $
          assertStructurallyEqual
            (globalIntVar C.User "x" 0x78)
            (parseToTerm "x_G :: Int")

      , testCase "explicit scope and name sort" $
          assertStructurallyEqual
            (globalIntVar C.System "x" 3)
            (parseToTerm "x_SG3 :: Int")

      , testCase "explicit scope and name sort, reversed" $
          assertStructurallyEqual
            (globalIntVar C.System "x" 3)
            (parseToTerm "x_GS3 :: Int")

      , testCase "explicit scope and name sort, derived unique" $
          assertStructurallyEqual
            (globalIntVar C.Internal "x" 0x78)
            (parseToTerm "x_IG :: Int")

      , testCase "explicit scope on a let binder" $
          assertStructurallyEqual
            (C.Letrec
              [(mkId C.GlobalId C.User "x" 0 intTy, C.Literal (C.IntLiteral 5))]
              (globalIntVar C.User "x" 0))
            (parseToTerm "let { x_G0 :: Int; x_G0 = 5 } in x_G0")

      , testCase "name too long to derive a unique from" $
          assertErrorContains "names of more than four characters don't fit"
            (parseToTerm "abcde :: Int")

      , testCase "name too long to derive a unique from, with a modifier" $
          assertErrorContains "names of more than four characters don't fit"
            (parseToTerm "abcde_S :: Int")
      ]
  ]
