{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Helpers for the unit tests of the scoped evaluator and its reductions of
  primitives ("Clash.Core.Scoped.Evaluator.Primitives"): evaluating terms
  with the reductions, in the environment Clash uses
  ('Clash.Rewrite.Scoped.Whnf.evaluatorEnv'), and building primitives,
  literals, types and values of the sized types, as builders of scoped Core
  ("Clash.Core.Scoped.Builder").

  The parser of scoped Core ('Test.Clash.Rewrite.parseTestTerm') doesn't parse
  primitives and most literals, so the tests build their terms with these
  builders, e.g. @2 +# 3@:

  > apps (prim "GHC.Prim.+#" (binTy intPrimTy intPrimTy)) [Left (intLit 2), Left (intLit 3)]

  Binders get their keys and names from identifiers as the parser reads them
  ('binderName'): @x_7@ has key 7. So a type string inside a 'forAll' can
  refer to the variable it binds by its identifier.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Clash.Scoped.Evaluator.Primitives
  ( -- * Evaluating
    primEnv
  , eval
  , evalSubj
  , evalWith
  , evalEnvIn
  , evalOpen
  , evalOpenIn
  , closed
  , assertEvalsTo
    -- * Terms
  , binderName
  , var
  , prim
  , apps
  , lam
  , tyLam
  , letNonRec
  , caseData
  , conPrim
  , undefinedAt
    -- * Literals
  , intLit
  , wordLit
  , natLit
  , integerLit
    -- * Types
  , ty
  , numTy
  , tc
  , (~>)
  , forAll
  , natKind
  , typeKind
  , binTy
  , addTy
  , knownNatTy
  , snatTy
  , vecTy
  , rtreeTy
  , intPrimTy
  , wordPrimTy
  , integerPrimTy
  , naturalPrimTy
  , doublePrimTy
  , stringPrimTy
  , byteArrayPrimTy
  , charPrimTy
  , ghcBoolTy
    -- * Values
  , snat
  , vec
  , rtree
  , coercion
    -- * Sized types, with type constructors of their own
  , bitTcNm
  , bvTcNm
  , bitTy
  , bvTy
  , bit
  , bv
  , indexTy
  , index
    -- * Boxed numbers
  , boxedIntTy
  , boxedInt
  , sizedTypeEnv
    -- * Data types
  , monoDataCon
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)
import qualified Data.HashMap.Strict as HashMap
import Language.Haskell.Exts.Syntax (Name (Ident))
import Test.Tasty.HUnit (Assertion)

import Clash.Core.DataCon (DataCon, DcStrictness (Lazy))
import Clash.Core.Literal (Literal (..))
import qualified Clash.Core.Name as C
import Clash.Core.TyCon (TyConName)
import qualified Clash.Normalize.Primitives as NP
import Clash.Unique (Unique)

import Clash.Core.Scoped.Builder
  ( Key, TmB, TyB, appB, appTyB, caseB, dataAltB, dataB, forAllTyB, lamB, letB
  , literalB, primB, runClosedTerm, runClosedType, tyAppB, tyLamB, varB, varTyB )
import Clash.Core.Scoped.Convert (mkDataCon)
import Clash.Core.Scoped.Evaluator.Primitives.Util (bConPrim, bvConPrim, indexConPrim)
import Clash.Core.Scoped.Evaluator.Types (Env (..), PrimReduction, PrimReductions (..))
import Clash.Core.Scoped.NameMap (emptyGlobalEnv)
import Clash.Core.Scoped.Sig (DataConSig (..))
import Clash.Core.Scoped.Syntax (Distinct, S (VoidS), Tele (TNil), Term (..), emptyScope)
import Clash.Core.Scoped.TypeEnv (TypeEnv)
import Clash.Core.Scoped.Util (primCo)
import Clash.Rewrite.Scoped.Whnf (evaluatorEnv)

import Test.Clash.Rewrite
  ( consDc, funTyB, iHashDc, indexTcNm, lrDc, brDc, nameKey, nilDc, parseName
  , parseTestType, snatDc, testTypeB, tyConB )
import qualified Test.Clash.Rewrite as R
import Test.Clash.Scoped
  ( TestScope (..), assertTermAlphaEq, builtTerm, builtType, closedType, termIn, testPrim
  , testTypeEnv, varD, withTestScope )
import Test.Clash.Scoped.Evaluator (whnfIn)

-- * Evaluating

-- | The environment of the evaluator with the given types and reductions of
-- primitives, and nothing for the other primitives
primEnv :: TypeEnv -> [(Text, PrimReduction)] -> Env
primEnv te rs = Env te (PrimReductions (HashMap.fromList rs) Nothing)

-- | Evaluate a closed term to WHNF with the types of the tests, in the
-- environment of Clash
eval :: HasCallStack => TmB -> Term 'VoidS
eval = evalWith testTypeEnv False

-- | 'eval', as the subject of a case-expression
evalSubj :: HasCallStack => TmB -> Term 'VoidS
evalSubj = evalWith testTypeEnv True

-- | 'eval' with the given types, as the subject of a case-expression or not
evalWith :: HasCallStack => TypeEnv -> Bool -> TmB -> Term 'VoidS
evalWith te = evalEnvIn (evaluatorEnv te)

-- | Evaluate a closed term to WHNF in the given environment, as the subject of
-- a case-expression or not
evalEnvIn :: HasCallStack => Env -> Bool -> TmB -> Term 'VoidS
evalEnvIn env isSubj b = whnfIn env emptyGlobalEnv isSubj emptyScope (closed b)

-- | Evaluate a term with free variables, with the given identifiers and
-- types, to WHNF with the types of the tests, as the subject of a
-- case-expression or not. The continuation gets the scope of the variables
-- and the result.
evalOpen
  :: HasCallStack
  => [(String, TyB)]
  -> Bool
  -> TmB
  -> (forall n. Distinct n => TestScope n -> Term n -> r)
  -> r
evalOpen = evalOpenIn (evaluatorEnv testTypeEnv)

-- | 'evalOpen' in the given environment
evalOpenIn
  :: HasCallStack
  => Env
  -> [(String, TyB)]
  -> Bool
  -> TmB
  -> (forall n. Distinct n => TestScope n -> Term n -> r)
  -> r
evalOpenIn env fvs isSubj b k =
  withTestScope [varD s (builtType t) | (s, t) <- fvs] $ \ts ->
    k ts (whnfIn env emptyGlobalEnv isSubj (testScope ts) (termIn ts (builtTerm b)))

-- | Build a closed term
closed :: HasCallStack => TmB -> Term 'VoidS
closed = runClosedTerm

-- | Assert that a closed term evaluates ('eval') to the given term, up to
-- alpha-equivalence
assertEvalsTo :: HasCallStack => TmB -> TmB -> Assertion
assertEvalsTo expected t = assertTermAlphaEq (closed expected) (eval t)

-- * Terms

-- | The key and name of a binder, from an identifier as the parser reads it
-- (see 'Test.Clash.Rewrite.parseNameScope'): @x_7@ has name @x@ and key 7
binderName :: HasCallStack => String -> (Key, C.Name a)
binderName s = (nameKey nm, nm)
 where
  nm = parseName (Ident () s)

-- | A variable, by its identifier: a variable of the scope of the test, or
-- one that a binder around it binds
var :: HasCallStack => String -> TmB
var = varB . fst . binderName

-- | A primitive with the given name and closed type
prim :: HasCallStack => Text -> TyB -> TmB
prim nm t = primB (testPrim nm (builtType t))

-- | Apply a term to terms ('Left') and types ('Right')
apps :: TmB -> [Either TmB TyB] -> TmB
apps = foldl (\f -> either (appB f) (tyAppB f))

-- | A lambda, binding the variable with the given identifier and type
lam :: HasCallStack => String -> TyB -> (TmB -> TmB) -> TmB
lam s t body = lamB k nm t (body (varB k))
 where
  (k, nm) = binderName s

-- | A type lambda, binding the type variable with the given identifier and
-- kind
tyLam :: HasCallStack => String -> TyB -> (TyB -> TmB) -> TmB
tyLam s k body = tyLamB key nm k (body (varTyB key))
 where
  (key, nm) = binderName s

-- | A non-recursive let-binding of the variable with the given identifier
-- and type
letNonRec :: HasCallStack => String -> TyB -> TmB -> (TmB -> TmB) -> TmB
letNonRec s t rhs body = letB k nm t rhs (body (varB k))
 where
  (k, nm) = binderName s

-- | A case-expression with one alternative, of a data constructor without
-- existentially quantified type variables: the subject, the type of the
-- alternative, the constructor, the identifiers and types of the binders of
-- its fields, and the right-hand side
caseData :: HasCallStack => TmB -> TyB -> DataCon -> [(String, TyB)] -> ([TmB] -> TmB) -> TmB
caseData subj altTy dc fields rhs =
  caseB subj altTy [dataAltB dc [] [(k, nm, t) | ((k, nm), t) <- bs] (rhs [varB k | ((k, _), _) <- bs])]
 where
  bs = [(binderName s, t) | (s, t) <- fields]

-- | A primitive of the evaluator, such as 'bvConPrim', as a builder
conPrim :: HasCallStack => Term 'VoidS -> TmB
conPrim = \case
  Prim p -> primB p
  _ -> error "conPrim: not a primitive"

-- | 'Clash.Normalize.Primitives.undefined' at a type, as reductions give it
undefinedAt :: TyB -> TmB
undefinedAt = tyAppB (prim (Text.pack (show 'NP.undefined)) (forAll "a_1" typeKind id))

-- * Literals

intLit, wordLit, natLit, integerLit :: Integer -> TmB
intLit = literalB . IntLiteral
wordLit = literalB . WordLiteral
natLit = literalB . NaturalLiteral
integerLit = literalB . IntegerLiteral

-- * Types

-- | A type, see 'Test.Clash.Rewrite.parseTestType'. It may refer to type
-- variables that a 'forAll' around it binds.
ty :: HasCallStack => String -> TyB
ty = testTypeB . parseTestType

-- | A type-level number
numTy :: Integer -> TyB
numTy = ty . show

-- | A type constructor, by its name and unique, applied to types
tc :: TyConName -> [TyB] -> TyB
tc nm = foldl appTyB (tyConB (Text.unpack (C.nameOcc nm) <> "_" <> show (C.nameUniq nm)))

-- | The function type
(~>) :: TyB -> TyB -> TyB
(~>) = funTyB
infixr 5 ~>

-- | A @forall@, binding the type variable with the given identifier and kind
forAll :: HasCallStack => String -> TyB -> (TyB -> TyB) -> TyB
forAll s k body = forAllTyB key nm k (body (varTyB key))
 where
  (key, nm) = binderName s

natKind, typeKind :: TyB
natKind = ty "Nat"
typeKind = ty "Type"

-- | The type of a binary operation
binTy :: TyB -> TyB -> TyB
binTy a r = a ~> a ~> r

-- | GHC's @+@ on type-level naturals, 'Test.Clash.Rewrite.addTy'
addTy :: TyB -> TyB -> TyB
addTy a b = tc R.addTyConName [a, b]

knownNatTy, snatTy :: TyB -> TyB
knownNatTy n = ty "GHC.TypeNats.KnownNat_2050" `appTyB` n
snatTy n = tc R.snatTcNm [n]

vecTy, rtreeTy :: TyB -> TyB -> TyB
vecTy n a = tc R.vecTcNm [n, a]
rtreeTy d a = tc R.rtreeTcNm [d, a]

intPrimTy, wordPrimTy, integerPrimTy, naturalPrimTy, doublePrimTy, stringPrimTy
  , byteArrayPrimTy, charPrimTy, ghcBoolTy :: TyB
intPrimTy = ty "GHC.Prim.Int#"
wordPrimTy = ty "GHC.Prim.Word#"
integerPrimTy = ty "GHC.Num.Integer.Integer"
naturalPrimTy = ty "GHC.Num.Natural.Natural"
doublePrimTy = ty "GHC.Prim.Double#"
stringPrimTy = ty "GHC.Prim.Addr#"
byteArrayPrimTy = ty "GHC.Prim.ByteArray#"
charPrimTy = ty "GHC.Prim.Char#"
ghcBoolTy = ty "GHC.Types.Bool"

-- * Values

-- | @SNat n@
snat :: Integer -> TmB
snat n = apps (dataB snatDc) [Right (numTy n), Left (natLit n)]

-- | The coercion argument of a constructor such as @Cons@. Results are
-- compared without the types of coercions, see
-- 'Test.Clash.Scoped.Evaluator.dropCoercionTypes'.
coercion :: TmB
coercion = case primCo (closedType "Int") :: Term 'VoidS of
  TyApp (Prim p) _ -> tyAppB (primB p) (ty "Int")
  _ -> error "coercion: not an application of a primitive"

-- | A vector of the given element type
vec :: TyB -> [TmB] -> TmB
vec a xs0 = go (toInteger (length xs0)) xs0
 where
  go _ [] = apps (dataB nilDc) [Right (numTy 0), Right a, Left coercion]
  go n (x : xs) = apps (dataB consDc)
    [Right (numTy n), Right a, Right (numTy (n - 1)), Left coercion, Left x, Left (go (n - 1) xs)]

-- | A tree of the given depth and element type
rtree :: Integer -> TyB -> [TmB] -> TmB
rtree d0 a = go d0
 where
  go _ [x] = apps (dataB lrDc) [Right (numTy 0), Right a, Left coercion, Left x]
  go d xs =
    let (l, r) = splitAt (length xs `div` 2) xs
    in apps (dataB brDc)
         [Right (numTy d), Right a, Right (numTy (d - 1)), Left coercion, Left (go (d - 1) l), Left (go (d - 1) r)]

-- * Sized types, with type constructors of their own

bitTcNm, bvTcNm :: TyConName
bitTcNm = R.mkTyConName "Clash.Sized.Internal.BitVector.Bit" 4000
bvTcNm = R.mkTyConName "Clash.Sized.Internal.BitVector.BitVector" 4001

bitTy :: TyB
bitTy = tc bitTcNm []

bvTy :: TyB -> TyB
bvTy n = tc bvTcNm [n]

indexTy :: TyB -> TyB
indexTy n = tc indexTcNm [n]

-- | A 'Clash.Sized.Internal.BitVector.Bit' with the given mask and value
bit :: Integer -> Integer -> TmB
bit msk val = apps (conPrim (bConPrim bitTcNm)) [Left (wordLit msk), Left (integerLit val)]

-- | A 'Clash.Sized.Internal.BitVector.BitVector' with the given size, mask
-- and value
bv :: Integer -> Integer -> Integer -> TmB
bv n msk val =
  apps (conPrim (bvConPrim bvTcNm))
    [Right (numTy n), Left (natLit n), Left (natLit msk), Left (integerLit val)]

-- | An 'Clash.Sized.Internal.Index.Index' with the given size and value
index :: Integer -> Integer -> TmB
index n val =
  apps (conPrim (indexConPrim indexTcNm)) [Right (numTy n), Left (natLit n), Left (integerLit val)]

-- * Boxed numbers

-- | GHC's boxed 'Int', 'Test.Clash.Rewrite.hIntTy', unlike the @Int@ of the
-- parser
boxedIntTy :: TyB
boxedIntTy = tc R.hIntTcNm []

-- | An 'Int'
boxedInt :: Integer -> TmB
boxedInt = appB (dataB iHashDc) . intLit

-- | The types of the tests, which have 'boxedIntTy'
sizedTypeEnv :: TypeEnv
sizedTypeEnv = testTypeEnv

-- * Data types

-- | A data constructor without type variables: its name, unique, the closed
-- types of its fields, and its result type
monoDataCon :: HasCallStack => String -> Unique -> [TyB] -> TyB -> DataCon
monoDataCon nm u fields res =
  mkDataCon (C.mkUnsafeName C.User (Text.pack nm) u) 1 (runClosedType (foldr (~>) res fields))
    (DataConSig TNil TNil (map runClosedType fields)) (map (const Lazy) fields) []
