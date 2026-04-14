{-|
  Copyright  :  (C) 2020,2022-2026 QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utilities to write unit tests on transformations
-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Clash.Rewrite where

import Clash.Annotations.BitRepresentation.Internal (buildCustomReprs)
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Literal as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.TysPrim as C
import qualified Clash.Core.Var as C
import Clash.Core.VarEnv (InScopeSet, emptyVarSet, emptyVarEnv, emptyInScopeSet)
import Clash.Driver.Types (ClashEnv(..), ClashOpts(..), defClashOpts, debugSilent)
import Clash.Rewrite.Types
import Clash.Rewrite.Util (runRewrite)
import Clash.Normalize.Types
import qualified Clash.Util.Interpolate as I
import Clash.Util.Supply (newSupply)
import Clash.Unique (Unique)

import Control.Applicative ((<|>))
import Control.Concurrent.MVar (newMVar)
import Control.DeepSeq (NFData, force)
import Control.Exception (ErrorCall (..), evaluate, try)
import Data.Char (isAscii, ord)
import Data.Default
import Data.Maybe (fromMaybe)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Extension (Extension (..), KnownExtension (..))
import Language.Haskell.Exts.Parser
  (ParseMode (..), defaultParseMode, fromParseResult, parseExpWithMode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Text.Read (readMaybe)
import GHC.Stack (HasCallStack)

import qualified Text.Show.Pretty as Pretty

import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Quote as TH

import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
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
  def = RewriteEnv
    { _clashEnv = ClashEnv
        { envOpts = defClashOpts { opt_debug = debugSilent }
        , envTyConMap = mempty
        , envTupleTyCons = IntMap.empty
        , envPrimitives = HashMap.empty
        , envCustomReprs = buildCustomReprs []
        , envDomains = HashMap.empty
        }
    , _typeTranslator=error "_typeTranslator: NYI"
    , _peEvaluator=error "_peEvaluator: NYI"
    , _evaluator=error "_evaluator: NYI"
    , _topEntities=emptyVarSet
    }

defRewriteState :: IO (RewriteState NormalizeState)
defRewriteState = do
  normState <- NormalizeState
    <$> newMVar emptyVarEnv
    <*> newMVar Map.empty
    <*> newMVar emptyVarEnv
    <*> newMVar emptyVarEnv
    <*> newMVar Map.empty
    <*> newMVar emptyVarEnv

  RewriteState
    <$> newMVar mempty
    <*> newMVar mempty
    <*> newMVar emptyVarEnv
    <*> newSupply
    <*> pure (error "_curFun: NYI")
    <*> newMVar 2
    <*> newMVar (error "_globalHeap: NYI")
    <*> newMVar emptyVarEnv
    <*> pure mempty
    <*> newMVar ()
    <*> pure normState

instance Default InScopeSet where
  def = emptyInScopeSet

-- | Run a single transformation given a certain context
runSingleTransformation
  :: RewriteEnv
  -- ^ Rewrite environment
  -> RewriteState extra
  -- ^ Rewrite state
  -> InScopeSet
  -- ^ Variables in scope in transformation
  -> Rewrite extra
  -- ^ Transformation to perform
  -> C.Term
  -- ^ Term to transform
  -> IO C.Term
runSingleTransformation rwEnv rwState is trans term = do
  (t, _, _) <- runR (runRewrite "" is trans term) rwEnv rwState
  pure t

-- | Run a single transformation with an empty environment and empty
-- InScopeSet. See Default instances ^ to inspect the precise definition of
-- "empty".
--
-- Note that at the time of writing (May 2020) the default environment does not
-- include a type translator, evaluator, current function, or global heap. Maps,
-- like the primitive and tycon map, are also empty. If the transformation under
-- test needs these definitions, you should add them manually.
runSingleTransformationDef :: Rewrite NormalizeState -> C.Term -> IO C.Term
runSingleTransformationDef rewrite term = do
  st <- defRewriteState
  runSingleTransformation def st def rewrite term


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

-- | See documentation of 'parseToTerm'. Example usage:
--
--     letrec = [parseToTermQQ|
--        let
--          x_0, x_1 :: Int
--          x_0 = 5
--          x_1 = 6
--        in
--          x_0
--     |]
--
-- Note that this is parsed at runtime, not at compile time. There's no good
-- technical reason for this though. We'd just need to implement a Template
-- Haskell Lift instance for Term.
--
-- For more information on the format of identifiers, see 'parseName'.
parseToTermQQ :: TH.QuasiQuoter
parseToTermQQ = TH.QuasiQuoter{
    TH.quoteExp = fmap (TH.AppE (TH.VarE 'parseToTerm)) . TH.lift
  , TH.quotePat = error "parseToTerm.quotePat: NYI"
  , TH.quoteType = error "parseToTerm.quoteType: NYI"
  , TH.quoteDec = error "parseToTerm.quoteDec: NYI"
  }

-- | The type 'parseType' produces for a type constructor whose name it derives
-- a unique from, e.g. @Int@
parseTyConTy :: HasCallStack => String -> C.Type
parseTyConTy nm =
  C.ConstTy
    (C.TyCon (C.Name C.User (Text.pack nm) (nameToUnique nm) C.noSrcSpan))

-- | The type 'parseType' produces for the type constructor @Int@
intTy :: C.Type
intTy = parseTyConTy "Int"

-- | An 'C.Id' with the given scope, name sort, human readable name, unique, and
-- type
mkId :: C.IdScope -> C.NameSort -> String -> Unique -> C.Type -> C.Id
mkId scope nmSort nm uniq typ =
  C.Id (C.mkUnsafeName nmSort (Text.pack nm) uniq) uniq typ scope

-- | A local 'C.Id'. See 'mkId'.
localId :: C.NameSort -> String -> Unique -> C.Type -> C.Id
localId = mkId C.LocalId

-- | A 'C.TyVar' with the given name sort, human readable name, and unique. Its
-- kind is 'C.liftedTypeKind', see 'parseTyVar'.
tyVar :: C.NameSort -> String -> Unique -> C.TyVar
tyVar nmSort nm uniq =
  C.TyVar (C.mkUnsafeName nmSort (Text.pack nm) uniq) uniq C.liftedTypeKind

-- | A reference to a local variable of type @Int@. See 'localId'.
intVar :: C.NameSort -> String -> Unique -> C.Term
intVar nmSort nm uniq = C.Var (localId nmSort nm uniq intTy)

-- | A reference to a global variable of type @Int@. See 'mkId'.
globalIntVar :: C.NameSort -> String -> Unique -> C.Term
globalIntVar nmSort nm uniq = C.Var (mkId C.GlobalId nmSort nm uniq intTy)

-- | A reference to a variable without a declared type. See 'freeVarType'.
freeVar :: C.NameSort -> String -> Unique -> C.Term
freeVar nmSort nm uniq = C.Var (localId nmSort nm uniq freeVarType)

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
  :: (HasCallStack, NFData a, Show a) => String -> a -> Assertion
assertErrorContains needle a = do
  parsed <- try (evaluate (force a))
  case parsed of
    Left (ErrorCall msg)
      | needle `List.isInfixOf` msg -> pure ()
      | otherwise -> assertFailure
          ("Expected an error mentioning '" <> needle <> "', but got:\n" <> msg)
    Right parsed1 -> assertFailure
      ("Expected an error mentioning '" <> needle
        <> "', but parsing succeeded:\n" <> Pretty.ppShow parsed1)

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
