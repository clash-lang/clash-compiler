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
import Control.DeepSeq (NFData, force)
import Control.Exception (ErrorCall (..), evaluate, try)
import Data.Char (isAscii, ord)
import Data.Default
import Data.Maybe (fromMaybe)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Extension (Extension (..), KnownExtension (..))
import Language.Haskell.Exts.Parser
  (ParseMode (..), defaultParseMode, fromParseResult, parseExpWithMode)
import System.IO.Unsafe (unsafePerformIO)
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

instance Default extra => Default (RewriteState extra) where
  def = RewriteState
    { _transformCounter=0
    , _transformCounters=mempty
    , _bindings=emptyVarEnv
    , _uniqSupply=unsafePerformIO newSupply
    , _curFun=error "_curFun: NYI"
    , _nameCounter=2
    , _workFreeBinders=emptyVarEnv
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
    }

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
runSingleTransformationDef :: Default extra => Rewrite extra -> C.Term -> IO C.Term
runSingleTransformationDef = runSingleTransformation def def def


parseType :: Show l => Type l -> C.Type
parseType = \case
  -- Type constructor: T
  TyCon _ (UnQual _ (Ident _ typNm)) ->
    -- TODO: We could/should build a TyConMap here
    C.ConstTy (C.TyCon (C.Name C.User (Text.pack typNm) 0 C.noSrcSpan))

  -- Unsupported type:
  t ->
    error ("parseType: " <> show t)

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
-- 'TypeMap'. See 'parseIdWithType'.
parseId :: (HasCallStack, Show l) => TypeMap -> Name l -> C.Id
parseId typs nm0 = C.Id nm1 uniq (lookupTM uniq typs) scope
 where
  (nm1, scope) = parseNameScope nm0
  uniq = C.nameUniq nm1

-- | Parse declarations (as, amongst others, used in let expressions). Note that
-- every binder needs an explicit type annotation, as we don't do any type
-- inference. Type annotations may occur anywhere though. Example, this is OK:
--
--    let
--      x_0 :: Int
--      x_0 = 2
--
--      x_1 :: Int
--      x_1 = x_0
--    in
--      x_1
--
-- But this is not:
--
--    let
--      x_0 :: Int
--      x_0 = 2
--
--      x_1 = x_0
--    in
--      x_1
--
parseDecls
  :: forall l
   . (HasCallStack, Show l)
  => TypeMap
  -> [Decl l]
  -> (TypeMap, [C.LetBinding])
parseDecls typs0 decls = (typs1, map parseOtherDecl otherDecls)
 where
  (typDecls, otherDecls) = List.partition isTypeDecl decls
  insertTyp (nm, t) = HashMap.insert nm t
  typs1 = foldr insertTyp typs0 (concatMap parseTypeDecl typDecls)

  parseOtherDecl :: HasCallStack => Decl l -> C.LetBinding
  parseOtherDecl = \case
    PatBind _ (PVar _ nm) (UnGuardedRhs _ e) Nothing ->
      (parseId typs1 nm, expToTerm typs1 e)
    e ->
      error ("parseOtherDecl: " <> show e)

  parseTypeDecl :: Decl l -> [(Unique, C.Type)]
  parseTypeDecl (TypeSig _ nms t) =
    map (\nm -> (C.nameUniq (parseName nm), parseType t)) nms
  parseTypeDecl _ = error "impossible"

  isTypeDecl :: Decl l -> Bool
  isTypeDecl (TypeSig {}) = True
  isTypeDecl _ = False

-- | Parse the patterns of a lambda into its binders. Like let bindings, binders
-- need an explicit type, either spelled out in the pattern or declared in an
-- enclosing let. I.e., both of these are OK:
--
--    \(x_0 :: Int) -> x_0
--
--    let
--      x_0 :: Int
--      x_0 = 5
--    in
--      \x_0 -> x_0
--
-- Binders are added to the type map, so a lambda's body can refer to them
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

  -- Term application: e1 e2
  App _ e1 e2 ->
    C.App (expToTerm typs0 e1) (expToTerm typs0 e2)

  -- Lambda: \x y -> e
  Lambda _ pats body0 ->
    let
      (typs1, ids) = parsePats typs0 pats
      body1 = expToTerm typs1 body0
    in
      foldr C.Lam body1 ids

  -- Variable reference: e
  Var _ (UnQual _ nm) ->
    C.Var (parseId typs0 nm)

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

-- | Parse mode used by 'parseToTerm'. Enables @ScopedTypeVariables@, so lambda
-- binders can spell out their type: @\\(x_0 :: Int) -> x_0@.
termParseMode :: ParseMode
termParseMode = defaultParseMode
  { extensions =
      EnableExtension ScopedTypeVariables : extensions defaultParseMode
  }

-- | Parse a string representing a Haskell expression into Clash Core. This can
-- only parse very simple expressions. In the future we should make an effort to
-- build a proper TyConMap (using LoadModules) to faithfully reproduce more
-- complex expressions.
parseToTerm :: String -> C.Term
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

-- | The type 'parseType' produces for the type constructor @Int@
intTy :: C.Type
intTy = C.ConstTy (C.TyCon (C.Name C.User (Text.pack "Int") 0 C.noSrcSpan))

-- | An 'C.Id' with the given scope, name sort, human readable name, unique, and
-- type
mkId :: C.IdScope -> C.NameSort -> String -> Unique -> C.Type -> C.Id
mkId scope nmSort nm uniq typ =
  C.Id (C.mkUnsafeName nmSort (Text.pack nm) uniq) uniq typ scope

-- | A local 'C.Id'. See 'mkId'.
localId :: C.NameSort -> String -> Unique -> C.Type -> C.Id
localId = mkId C.LocalId

-- | A reference to a local variable of type @Int@. See 'localId'.
intVar :: C.NameSort -> String -> Unique -> C.Term
intVar nmSort nm uniq = C.Var (localId nmSort nm uniq intTy)

-- | A reference to a global variable of type @Int@. See 'mkId'.
globalIntVar :: C.NameSort -> String -> Unique -> C.Term
globalIntVar nmSort nm uniq = C.Var (mkId C.GlobalId nmSort nm uniq intTy)

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

      , testCase "let without a type annotation" $
          assertErrorContains "forgot to (explicitely) declare"
            (parseToTerm "let { x_0 = 5 } in x_0")

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
