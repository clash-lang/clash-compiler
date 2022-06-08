{-|
  Copyright  :  (C) 2020,2022 QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utilities to write unit tests on transformations
-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Clash.Unique (emptyUniqMap)
import qualified Clash.Util.Interpolate as I

import Control.Applicative ((<|>))
import Control.Concurrent.Supply (newSupply)
import Data.Default
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser (parseExp, fromParseResult)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import GHC.Stack (HasCallStack)

import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Quote as TH

import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Text as Text

type TypeMap = HashMap.HashMap Int C.Type

lookupTM :: Int -> TypeMap -> C.Type
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
        { envOpts = defClashOpts { _opt_debug = debugSilent }
        , envTyConMap = emptyUniqMap
        , envTupleTyCons = IntMap.empty
        , envPrimitives = HashMap.empty
        , envCustomReprs = buildCustomReprs []
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

-- | Parse an identifier into a Clash Name. Identifiers must include a unique
-- and might include a modifier indicating whether its NameSort. Examples:
--
--   * x_3:  User identifier with human readable name "x", unique "3"
--   * x_I3: Internal identifier with human readable name "x", unique "3"
--   * x_S3: System identifier with human readable name "x", unique "3"
--
parseName :: Show l => Name l -> C.Name a
parseName = \case
  Ident _ s -> failOnNothing s (go "" s)
  Symbol _ s -> failOnNothing s (go "" s)
 where
  failOnNothing _ (Just (nmSort, nm, uniq)) =
    C.mkUnsafeName nmSort (Text.pack nm) uniq
  failOnNothing s Nothing = error [I.i|
    Not a valid id: #{s}. Identifiers must be of form 'foobar_123', where
    'foobar' is a human-readable (but ultimately unused) name and '123' is the
    unique. Additionally, 'I', 'U', or 'S' might be prefixed to create an
    Internal, User, or System name respectively. For example, 'foobar_S123'.
  |]

  go _seen "" = Nothing
  go seen0 ('_':s:ss)
    | 'U' <- s = fmap (C.User,seen1,) (readMaybe ss) <|> cont
    | 'S' <- s = fmap (C.System,seen1,) (readMaybe ss) <|> cont
    | 'I' <- s = fmap (C.Internal,seen1,) (readMaybe ss) <|> cont
    | otherwise = fmap (C.User,seen1,) (readMaybe (s:ss)) <|> cont
   where
    seen1 = reverse seen0
    cont = go ('_':seen0) (s:ss)
  go seen (s:ss) = go (s:seen) ss

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
    PatBind _ (PVar _ (parseName -> nm)) (UnGuardedRhs _ e) Nothing ->
      let
        uniq = C.nameUniq nm
        typ = lookupTM (C.nameUniq nm) typs1
      in
        (C.Id nm uniq typ C.LocalId, expToTerm typs1 e)
    e ->
      error ("parseOtherDecl: " <> show e)

  parseTypeDecl :: Decl l -> [(Int, C.Type)]
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

  -- Local variable reference with type signature: x :: t
  ExpTypeSig _ (Var _ (UnQual _ (parseName -> nm))) (parseType -> t) ->
    C.Var (C.Id nm (C.nameUniq nm) t C.LocalId)

  -- Term application: e1 e2
  App _ e1 e2 ->
    C.App (expToTerm typs0 e1) (expToTerm typs0 e2)

  -- Variable reference: e
  Var _ (UnQual _ (parseName -> nm)) ->
    let
     uniq = C.nameUniq nm
     typ = lookupTM (C.nameUniq nm) typs0
    in
      C.Var (C.Id nm uniq typ C.LocalId)

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

-- | Parse a string representing a Haskell expression into Clash Core. This can
-- only parse very simple expressions. In the future we should make an effort to
-- build a proper TyConMap (using LoadModules) to faithfully reproduce more
-- complex expressions.
parseToTerm :: String -> C.Term
parseToTerm = expToTerm HashMap.empty . fromParseResult . parseExp

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
