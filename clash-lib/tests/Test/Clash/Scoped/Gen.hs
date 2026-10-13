{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  QuickCheck generators of scoped Clash Core ("Clash.Core.Scoped"), to test
  its trusted core and the functions on it.

  A generated term is built in a test scope ('Test.Clash.Scoped.TestScope')
  with random free variables, the type variables before the term variables. It
  only mentions variables in scope, every occurrence carries the type of its
  binder, and the types of occurrences and binders only mention type variables
  in scope at them. The term need not be well-typed, but kinds are plausible: a
  type is generated at a kind, and a type variable is only used at its kind.

  Binders get fresh names, just above the watermark of the scope at them, as
  'Clash.Core.Scoped.Subst.withFreshIdBinder' gives them. Names clash where the
  generators of "Test.Clash.Scoped.Gen.Clash" made Clash Core shadow:

  * 'Sunk' terms are generated in a scope, and sunk into a larger one, whose
    new variables have the names of the first binders of the term;

  * 'Test.Clash.Scoped.forAllScrambled' renames binders to names of
    variables in scope, of either sort, and of other binders;

  * the global variables ('globals') have small uniques, which are the raw
    names of the first free variables and binders too (D5).

  The signatures of data constructors, globals and primitives are fixed, see
  'genCons', 'globals' and 'prims'.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Clash.Scoped.Gen
  ( -- * Signatures
    GenCon (..)
  , genCons
  , fDataCon
  , dataCons
  , typeEnv
  , tyConMap
  , globals
  , prims
    -- * Scopes
  , SomeScope (..)
  , genTestScope
  , ExtScope (..)
  , genMoreVars
  , showTestScope
    -- * Generating
  , Ctx (..)
  , ctxOf
  , genKind
  , genType
  , genTerm
  , genAlt
    -- * Inputs of properties
  , termSize
  , InScope (..)
  , genInScope
  , shrinkInScope
  , Sunk (..)
  , genSunk
  , genClosedSunk
  , shrinkSunk
  , Redex (..)
  , genRedex
  , shrinkRedex
  , KnownCon (..)
  , genKnownCon
  , shrinkKnownCon
  , DataConInst (..)
  , genDataConInst
  , genSameRhsCase
  , genPerturbed
    -- * Shrinking
  , shrinkTerm
    -- * Comparing
  , rawEqTm
  , renameOccs
    -- * Free variables, by an independent oracle
  , refFreeTyVars
  , refFreeTmVars
  , refFreeTyVarsOfType
  , refFreeTmOccurrences
  , refGlobals
    -- * Showing
  , showT
  , showTy
  ) where

import Control.Monad (replicateM)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Data.Text (Text)
import Test.Tasty.QuickCheck

import Clash.Annotations.SynthesisAttributes (Attr (..))
import Clash.Core.DataCon (DataCon, DcStrictness (Lazy))
import Clash.Core.Literal (Literal (IntLiteral))
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C (NameMod (..))
import Clash.Core.TyCon (AlgTyConRhs (DataTyCon), TyCon, TyConMap, TyConName, tyConName)
import qualified Clash.Core.Type as C (ConstTy (TyCon), LitTy (..), Type)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Unique (Unique)
import GHC.Types.SrcLoc (noSrcSpan)

import Clash.Core.Scoped.Alpha (aeqTy)
import Clash.Core.Scoped.Builder
  (Key, TyB, appTyB, constTyB, forAllTyB, litTyB, runClosedType, runDataConSig, varTyB)
import Clash.Core.Scoped.Convert (mkAlgTyCon, mkDataCon, mkPrimTyCon)
import Clash.Core.Scoped.FreeVars (bindNameBinders, patNameBinders)
import Clash.Core.Scoped.NameMap (idBinderNameBinders, tyBinderNameBinders)
import Clash.Core.Scoped.Subst
  ( openAlt, openBind, openIdBinder, openTyBinder, patVars, teleIdVars, unsinkTerm
  , withFreshIdBinder, withFreshIdBinders, withFreshTyBinder)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Type (mkFunTy)
import Clash.Core.Scoped.TypeEnv (TypeEnv, mkTypeEnv)
import qualified Clash.Core.Scoped.TysPrim as P

import Test.Clash.Rewrite
  ( existentialDataCon, funTyB, intTyCon, justDataCon, maybeTyConName
  , nothingDataCon, pairDataCon, parseTyConName, showPprU, testGlobalId)
import Test.Clash.Scoped (TestScope (..), testPrim)

-- * Signatures

-- | A type constructor
tc :: TyConName -> Type n
tc = ConstTy . C.TyCon

-- | @Type -> Type@
arrKind :: Kind n
arrKind = mkFunTy P.liftedTypeKind P.liftedTypeKind

-- | The names of the type constructors of the generated types, besides
-- @Int@, @Pair@, @E@ and @Maybe@ of "Test.Clash.Rewrite": @GF, GK :: Type ->
-- Type@, @GN :: Type@, @Eith :: Type -> Type -> Type@, @Sgn :: Nat -> Type@,
-- @Lbl :: Symbol -> Type@ and @GP :: forall k. k -> Type@. The parser of
-- "Test.Clash.Rewrite" gives these names to these identifiers.
fTcNm, kTcNm, nTcNm, eitherTcNm, signedTcNm, lblTcNm, pTcNm :: TyConName
fTcNm = parseTyConName "GF"
kTcNm = parseTyConName "GK"
nTcNm = parseTyConName "GN"
eitherTcNm = parseTyConName "Eith"
signedTcNm = parseTyConName "Sgn"
lblTcNm = parseTyConName "Lbl"
pTcNm = parseTyConName "GP"

intTy, pairTy, existentialTy, maybeTy :: Type n
intTy = tc (tyConName intTyCon)
pairTy = tc (parseTyConName "Pair")
existentialTy = tc (parseTyConName "E")
maybeTy = tc maybeTyConName

-- | A data constructor of a generated term, with its signature written out,
-- as the specification that the functions that instantiate it are checked
-- against
data GenCon = GenCon
  { gcDataCon :: DataCon
  , gcUnivKinds :: [Kind 'VoidS]
  -- ^ The kinds of its universally quantified type variables
  , gcExtKinds :: forall n. [Type n] -> [Kind n]
  -- ^ The kinds of its existentially quantified type variables, given types
  -- for the universally quantified ones
  , gcArgTys :: forall n. [Type n] -> [Type n] -> [Type n]
  -- ^ The types of its fields, given types for the universally and the
  -- existentially quantified type variables
  }

-- | A data constructor with tag 1, lazy fields, and the given name, unique,
-- universally and existentially quantified type variables, field types and
-- result type
mkGenDataCon
  :: Text -> Unique -> [(Key, C.Name C.Type, TyB)] -> [(Key, C.Name C.Type, TyB)]
  -> [TyB] -> TyB -> DataCon
mkGenDataCon nm u univ ext args res =
  mkDataCon (C.mkUnsafeName C.User nm u) 1 (runClosedType ty) (runDataConSig univ ext args)
    (map (const Lazy) args) []
 where
  ty = foldr (\(k, n, ki) -> forAllTyB k n ki) (foldr funTyB res args) (univ <> ext)

-- | A type variable of a signature, with the given key, name and kind
sigTv :: Key -> Text -> TyB -> (Key, C.Name C.Type, TyB)
sigTv k nm ki = (k, C.mkUnsafeName C.User nm (fromIntegral k), ki)

typeB, natB :: TyB
typeB = constTyB (case P.liftedTypeKind @'VoidS of ConstTy c -> c; _ -> error "typeB")
natB = constTyB (case P.typeNatKind @'VoidS of ConstTy c -> c; _ -> error "natB")

-- | @MkF :: forall a. forall b. a -> b -> GF a@, with an existential type
-- variable @b@
fDataCon :: DataCon
fDataCon =
  mkGenDataCon "MkF" 1003 [sigTv 60 "a" typeB] [sigTv 61 "b" typeB] [varTyB 60, varTyB 61]
    (appTyB (constTyB (C.TyCon fTcNm)) (varTyB 60))

-- | @MkK :: forall (k :: Type). forall (x :: k). GP k x -> Sgn 8 -> GK k@:
-- the kind of the existential type variable mentions the universal one
kDataCon :: DataCon
kDataCon =
  mkGenDataCon "MkK" 1004 [sigTv 62 "k" typeB] [sigTv 63 "x" (varTyB 62)]
    [ appTyB (appTyB (constTyB (C.TyCon pTcNm)) (varTyB 62)) (varTyB 63)
    , appTyB (constTyB (C.TyCon signedTcNm)) (litTyB (C.NumTy 8)) ]
    (appTyB (constTyB (C.TyCon kTcNm)) (varTyB 62))

-- | @MkN :: forall (n :: Nat). Sgn n -> GN@
nDataCon :: DataCon
nDataCon =
  mkGenDataCon "MkN" 1007 [] [sigTv 66 "n" natB]
    [appTyB (constTyB (C.TyCon signedTcNm)) (varTyB 66)] (constTyB (C.TyCon nTcNm))

-- | The data constructors that generated terms use: @MkPair@, @MkE@ (with an
-- existential type variable), @Just@ and @Nothing@ of "Test.Clash.Rewrite",
-- and 'fDataCon', 'kDataCon' and 'nDataCon'
genCons :: [GenCon]
genCons =
  [ GenCon pairDataCon [] (const []) (\_ _ -> [intTy, intTy])
  , GenCon existentialDataCon [] (const [P.liftedTypeKind]) (\_ ext -> ext)
  , GenCon fDataCon [P.liftedTypeKind] (const [P.liftedTypeKind]) (<>)
  , GenCon kDataCon [P.liftedTypeKind] id
      (\univ ext -> case (univ, ext) of
        ([k], [x]) -> [AppTy (AppTy (tc pTcNm) k) x, signed8]
        _ -> error "kDataCon: wrong number of types")
  , GenCon justDataCon [P.liftedTypeKind] (const []) const
  , GenCon nothingDataCon [P.liftedTypeKind] (const []) (\_ _ -> [])
  , GenCon nDataCon [] (const [P.typeNatKind]) (\_ ext -> map (AppTy (tc signedTcNm)) ext)
  ]
 where
  signed8 = AppTy (tc signedTcNm) (LitTy (C.NumTy 8))

dataCons :: [DataCon]
dataCons = map gcDataCon genCons

-- | The type constructors of the generated types
tyConMap :: TyConMap
tyConMap = UniqMap.fromList [(tyConName t, t) | t <- tcs]
 where
  tcs :: [TyCon]
  tcs =
    [ intTyCon
    , alg (parseTyConName "Pair") P.liftedTypeKind 0 [pairDataCon]
    , alg (parseTyConName "E") P.liftedTypeKind 0 [existentialDataCon]
    , alg fTcNm arrKind 1 [fDataCon]
    , alg kTcNm arrKind 1 [kDataCon]
    , alg maybeTyConName arrKind 1 [justDataCon, nothingDataCon]
    , alg nTcNm P.liftedTypeKind 0 [nDataCon]
    , mkPrimTyCon eitherTcNm (mkFunTy P.liftedTypeKind arrKind) 2
    , mkPrimTyCon signedTcNm (mkFunTy P.typeNatKind P.liftedTypeKind) 1
    , mkPrimTyCon lblTcNm (mkFunTy P.typeSymbolKind P.liftedTypeKind) 1
    , mkPrimTyCon pTcNm pKind 2
    ]

  alg nm k ar dcs = mkAlgTyCon nm k ar (DataTyCon dcs) False

  -- forall k. k -> Type
  pKind = runClosedType (forAllTyB 69 (C.mkUnsafeName C.User "k" 69) typeB (funTyB (varTyB 69) typeB))

typeEnv :: TypeEnv
typeEnv = mkTypeEnv tyConMap

-- | The global variables that generated terms use. Their uniques are small, so
-- that they are the raw names of local variables too (D5).
globals :: [GlobalId]
globals =
  [ testGlobalId "f_G2" "Int -> Int"
  , testGlobalId "g_G4" "forall a. a -> a"
  , testGlobalId "h_G6" "Pair"
  ]

-- | The primitives that generated terms use
prims :: [PrimInfo]
prims =
  [ testPrim "Test.plus" "Int -> Int -> Int"
  , testPrim "Test.undefined" "forall a. a"
  ]

-- * Scopes

-- | A test scope
data SomeScope = forall n. Distinct n => SomeScope (TestScope n)

instance Show SomeScope where
  show (SomeScope ts) = showTestScope ts

-- | An extension of a test scope
data ExtScope n = forall l. DExt n l => ExtScope (TestScope l)

-- | A scope with up to 3 type variables and up to 4 term variables, with the
-- names 1, 2, ...
genTestScope :: Gen SomeScope
genTestScope = do
  nTvs <- choose (0, 3)
  nIds <- choose (0, 4)
  ExtScope ts <- genVars nTvs nIds emptyTestScope
  pure (SomeScope ts)
 where
  emptyTestScope = TestScope emptyScope IntMap.empty IntMap.empty

-- | Extend a scope with up to 2 type variables and 1 to 4 term variables. Their
-- names are just above the watermark of the scope: those of the first binders
-- of a term generated in the scope.
genMoreVars :: Distinct n => TestScope n -> Gen (ExtScope n)
genMoreVars ts = do
  nTvs <- choose (0, 2)
  nIds <- choose (1, 4)
  genVars nTvs nIds ts

-- | Extend a scope with the given numbers of type variables and term variables
genVars :: forall n. Distinct n => Int -> Int -> TestScope n -> Gen (ExtScope n)
genVars nTvs nIds ts@(TestScope sc tys tms)
  | nTvs > 0 = do
      k <- genKind (ctxOf ts)
      nm <- elements ["a", "b", "c"]
      withFreshTyBinder sc (occName nm) k $ \(sc1 :: Scope i) b -> do
        let v = tyBinderVar b
        ExtScope (ts2 :: TestScope l) <-
          genVars (nTvs - 1) nIds (TestScope sc1 (IntMap.insert (tyRaw v) v (sink1 tys)) (sink1 tms))
        pure (withExtTrans @n @i @l (ExtScope ts2))
  | nIds > 0 = do
      ty <- genType (ctxOf ts) P.liftedTypeKind 2
      nm <- elements ["x", "y", "z"]
      withFreshIdBinder sc (occName nm) ty $ \(sc1 :: Scope i) b -> do
        let v = idBinderVar b
        ExtScope (ts2 :: TestScope l) <-
          genVars 0 (nIds - 1) (TestScope sc1 (sink1 tys) (IntMap.insert (tmRaw v) v (sink1 tms)))
        pure (withExtTrans @n @i @l (ExtScope ts2))
  | otherwise = pure (ExtScope ts)
 where
  tyRaw = \case
    VarTy _ _ n -> tyNameRaw n
    _ -> error "genVars: not a type variable"
  tmRaw = \case
    Var _ _ n -> tmNameRaw n
    _ -> error "genVars: not a variable"

-- | A human readable name. Its unique is not used.
occName :: Text -> C.Name a
occName nm = C.mkUnsafeSystemName nm 0

-- | Show the variables of a test scope, with their raw names and kinds or
-- types
showTestScope :: TestScope n -> String
showTestScope (TestScope _ tys tms) =
  "free type variables: " <> List.intercalate ", " [showTy v <> " :: " <> showTy k | v@(VarTy _ k _) <- IntMap.elems tys] <>
  "\nfree term variables: " <> List.intercalate ", " [showT v <> " :: " <> showTy t | v@(Var _ t _) <- IntMap.elems tms]

-- * Generating

-- | The variables in scope where a term is generated
data Ctx n = Ctx
  { ctxScope :: Scope n
  , ctxTyVars :: [Type n]
  -- ^ Type variables in scope, innermost first
  , ctxIds :: [Term n]
  -- ^ Term variables in scope, innermost first
  }

-- | The context of the variables of a test scope
ctxOf :: TestScope n -> Ctx n
ctxOf (TestScope sc tys tms) = Ctx sc (reverse (IntMap.elems tys)) (reverse (IntMap.elems tms))

-- | The context in the scope of the given type variables and term variables
bindCtx :: DExt n l => Scope l -> [Type l] -> [Term l] -> Ctx n -> Ctx l
bindCtx sc tvs ids (Ctx _ tvs0 ids0) = Ctx sc (tvs <> map sink tvs0) (ids <> map sink ids0)

-- | A kind: mostly @Type@, sometimes @Type -> Type@, @Nat@, @Symbol@, or a
-- type variable of kind @Type@
genKind :: Ctx n -> Gen (Kind n)
genKind ctx = frequency $
  [ (8, pure P.liftedTypeKind)
  , (2, pure arrKind)
  , (2, pure P.typeNatKind)
  , (1, pure P.typeSymbolKind)
  ] <>
  [(2, elements kvs) | not (null kvs)]
 where
  kvs = [v | v@(VarTy _ k _) <- ctxTyVars ctx, aeqTy k P.liftedTypeKind]

-- | A type of the given kind, of at most the given depth
genType :: Distinct n => Ctx n -> Kind n -> Int -> Gen (Type n)
genType ctx k d
  | d <= 0 || null nodes = frequency leaves
  | otherwise = frequency (leaves <> nodes)
 where
  vars = [v | v@(VarTy _ k' _) <- ctxTyVars ctx, aeqTy k' k]
  varLeaf = [(4, elements vars) | not (null vars)]
  sub = genType ctx P.liftedTypeKind (d - 1)

  (leaves, nodes)
    | aeqTy k P.liftedTypeKind =
        ( varLeaf <> [(3, elements [intTy, pairTy, existentialTy])]
        , [ (3, mkFunTy <$> sub <*> sub)
          , (2, genForAll)
          , (2, AppTy <$> genType ctx arrKind (d - 1) <*> sub)
          , (1, AnnType <$> genAttrs <*> sub)
          , (1, AppTy (tc signedTcNm) <$> genType ctx P.typeNatKind (d - 1))
          , (1, AppTy (tc lblTcNm) <$> genType ctx P.typeSymbolKind (d - 1))
          , (1, genP)
          ] )
    | aeqTy k arrKind =
        ( varLeaf <> [(2, elements [tc fTcNm, maybeTy])]
        , [(2, AppTy (tc eitherTcNm) <$> sub)] )
    | aeqTy k P.typeNatKind =
        (varLeaf <> [(3, LitTy . C.NumTy <$> choose (0, 16))], [])
    | aeqTy k P.typeSymbolKind =
        (varLeaf <> [(3, LitTy . C.SymTy <$> elements ["a", "b"])], [])
    | null vars =
        -- No type of this kind. Plausible is good enough.
        ([(1, pure intTy)], [])
    | otherwise = (varLeaf, [])

  genForAll = do
    ki <- genKind ctx
    nm <- elements ["a", "b", "k"]
    withFreshTyBinder (ctxScope ctx) (occName nm) ki $ \sc b ->
      ForAllTy b <$> genType (bindCtx sc [tyBinderVar b] [] ctx) P.liftedTypeKind (d - 1)

  -- @P k x@ for some kind @k@ and type @x :: k@: a kind that has types
  genP = do
    kv <- elements $
      [P.liftedTypeKind, arrKind, P.typeNatKind, P.typeSymbolKind] <>
      [ki | VarTy _ ki _ <- ctxTyVars ctx]
    x <- genType ctx kv (d - 1)
    pure (AppTy (AppTy (tc pTcNm) kv) x)

genAttrs :: Gen [Attr Text]
genAttrs = do
  n <- choose (1, 2)
  replicateM n (elements [BoolAttr "keep" True, IntegerAttr "depth" 3, StringAttr "chip" "x", Attr "dont_touch"])

-- | Split a size over a subterm and the given number of other subterms
splitSize :: Int -> Int -> Gen (Int, [Int])
splitSize n k = do
  cuts <- List.sort <$> replicateM k (choose (0, n))
  pure (case cuts of
    [] -> (n, [])
    c:_ -> (c, zipWith (-) (drop 1 cuts <> [n]) cuts))

-- | Split a size over two subterms
splitSize2 :: Int -> Gen (Int, Int)
splitSize2 n = do
  n1 <- choose (0, n)
  pure (n1, n - n1)

-- | A term with about the given number of nodes
genTerm :: Distinct n => Ctx n -> Int -> Gen (Term n)
genTerm ctx n
  | n <= 0 = leaf
  | otherwise = frequency
      [ (1, leaf)
      , (3, genLam)
      , (2, genTyLam)
      , (3, genApp)
      , (2, TyApp <$> genTerm ctx (n - 1) <*> (genKind ctx >>= \k -> genType ctx k 2))
      , (2, genNonRec)
      , (1, genRec)
      , (2, genCase)
      , (1, genCast)
      , (1, genTick)
      ]
 where
  sc = ctxScope ctx

  leaf = frequency $
    [(6, elements (ctxIds ctx)) | not (null (ctxIds ctx))] <>
    [ (2, GlobalVar <$> elements globals)
    , (1, Literal . IntLiteral <$> choose (0, 3))
    , (1, Data <$> elements dataCons)
    , (1, Prim <$> elements prims)
    ]

  genIdTy = genType ctx P.liftedTypeKind 2

  genLam = do
    ty <- genIdTy
    nm <- elements ["x", "y", "z"]
    withFreshIdBinder sc (occName nm) ty $ \sc' b ->
      Lam b <$> genTerm (bindCtx sc' [] [idBinderVar b] ctx) (n - 1)

  genTyLam = do
    k <- genKind ctx
    nm <- elements ["a", "b", "k"]
    withFreshTyBinder sc (occName nm) k $ \sc' b ->
      TyLam b <$> genTerm (bindCtx sc' [tyBinderVar b] [] ctx) (n - 1)

  genApp = do
    (n1, n2) <- splitSize2 (n - 1)
    App <$> genTerm ctx n1 <*> genTerm ctx n2

  genNonRec = do
    (n1, n2) <- splitSize2 (n - 1)
    x <- genTerm ctx n1
    ty <- genIdTy
    nm <- elements ["x", "y", "z"]
    withFreshIdBinder sc (occName nm) ty $ \sc' b ->
      Let (NonRec b x) <$> genTerm (bindCtx sc' [] [idBinderVar b] ctx) n2

  genRec = do
    k <- choose (1, 3)
    tys <- replicateM k genIdTy
    nms <- replicateM k (elements ["x", "y", "z"])
    (nBody, ns) <- splitSize (n - 1) k
    withFreshIdBinders sc (zip (map occName nms) tys) $ \sc' bs vars -> do
      let ctx' = bindCtx sc' [] vars ctx
      xs <- mapM (genTerm ctx') ns
      Let (Rec bs xs) <$> genTerm ctx' nBody

  genCase = do
    k <- choose (1, 3)
    (nSubj, ns) <- splitSize (n - 1) k
    subj <- genTerm ctx nSubj
    ty <- genIdTy
    Case subj ty <$> mapM (genAlt ctx) ns

  genCast = do
    e <- genTerm ctx (n - 1)
    Cast e <$> genIdTy <*> genIdTy

  genTick = do
    t <- frequency
      [ (1, pure (SrcSpan noSrcSpan))
      , (2, NameMod <$> elements [C.PrefixName, C.SuffixName, C.SetName]
                    <*> genType ctx P.typeSymbolKind 1)
      , (1, pure DeDup)
      , (1, pure NoDeDup)
      , (2, Attributes <$> genType ctx P.liftedTypeKind 1 <*> genTerm ctx (n `div` 3))
      ]
    Tick t <$> genTerm ctx (n - 1)

-- | A case alternative
genAlt :: Distinct n => Ctx n -> Int -> Gen (Alt n)
genAlt ctx n = frequency
  [ (5, genDataAlt ctx n)
  , (1, Alt . LitPat . IntLiteral <$> choose (0, 3) <*> genTerm ctx n)
  , (1, Alt DefaultPat <$> genTerm ctx n)
  ]

-- | A case alternative with a pattern that matches a data constructor, see
-- 'withGenPat'
genDataAlt :: Distinct n => Ctx n -> Int -> Gen (Alt n)
genDataAlt ctx n = do
  gc <- elements genCons
  univTys <- mapM (\k -> genType ctx (sinkClosed k) 1) (gcUnivKinds gc)
  withGenPat ctx gc univTys $ \ctx' p -> Alt p <$> genTerm ctx' n

-- | A pattern that matches a data constructor, given types for its
-- universally quantified type variables, with fresh binders for its
-- existentially quantified type variables and its fields, built from the
-- signature of the constructor ('GenCon') rather than with the functions
-- under test. Gives the context in the scope of the pattern too.
withGenPat
  :: Distinct n
  => Ctx n -> GenCon -> [Type n]
  -> (forall l. DExt n l => Ctx l -> Pat n l -> Gen r)
  -> Gen r
withGenPat ctx gc univTys k = do
  extNms <- replicateM (length extKinds) (elements ["e", "f"])
  case (extKinds, extNms) of
    ([], _) -> fields TNil []
    ([ki], [nm]) ->
      withFreshTyBinder sc (occName nm) ki $ \sc1 b ->
        let ctx1 = bindCtx sc1 [tyBinderVar b] [] ctx
        in  fieldsIn ctx1 (gcArgTys gc (map sink univTys) [tyBinderVar b]) $ \ctx2 ids ->
              k ctx2 (DataPat (gcDataCon gc) (TCons b TNil) ids)
    _ -> error "withGenPat: more than one existential type variable"
 where
  sc = ctxScope ctx
  extKinds = gcExtKinds gc univTys
  fields tvs _ = fieldsIn ctx (gcArgTys gc univTys []) $ \ctx2 ids ->
    k ctx2 (DataPat (gcDataCon gc) tvs ids)

  fieldsIn
    :: Distinct i
    => Ctx i -> [Type i]
    -> (forall l. DExt i l => Ctx l -> Tele IdBinder i l -> Gen r)
    -> Gen r
  fieldsIn ctxI tys cont = do
    nms <- replicateM (length tys) (elements ["x", "y", "z"])
    withFreshIdBinders (ctxScope ctxI) (zip (map occName nms) tys) $ \scL ids vars ->
      cont (bindCtx scL [] vars ctxI) ids

-- * Inputs of properties

-- | The number of nodes of the terms that the inputs below have, given the
-- QuickCheck size: up to about 33
termSize :: Int -> Int
termSize s = s `div` 3

-- | A term in the scope of its free variables
data InScope = forall n. Distinct n => InScope (TestScope n) (Term n)

instance Show InScope where
  show (InScope ts tm) = showTestScope ts <> "\nterm:\n" <> showT tm

genInScope :: Gen InScope
genInScope = do
  SomeScope ts <- genTestScope
  sized $ \s -> InScope ts <$> genTerm (ctxOf ts) (termSize s)

shrinkInScope :: InScope -> [InScope]
shrinkInScope (InScope ts tm) = InScope ts <$> shrinkTerm tm

-- | A term in the first scope, to be 'sink'ed into the second one, where the
-- new variables have the names of its first binders (see 'genMoreVars')
data Sunk = forall n l. (Distinct n, DExt n l) => Sunk (TestScope n) (TestScope l) (Term n)

instance Show Sunk where
  show (Sunk ts0 ts1 tm) =
    showTestScope ts0 <> "\nsunk into the scope of:\n" <> showTestScope ts1 <>
      "\nterm:\n" <> showT tm

genSunk :: Gen Sunk
genSunk = do
  SomeScope ts0 <- genTestScope
  ExtScope ts1 <- genMoreVars ts0
  sized $ \s -> Sunk ts0 ts1 <$> genTerm (ctxOf ts0) (termSize s)

-- | A closed term, sunk into a scope, see 'Sunk'
genClosedSunk :: Gen Sunk
genClosedSunk = do
  ExtScope ts1 <- genMoreVars ts0
  sized $ \s -> Sunk ts0 ts1 <$> genTerm (ctxOf ts0) (termSize s)
 where
  ts0 = TestScope emptyScope IntMap.empty IntMap.empty

shrinkSunk :: Sunk -> [Sunk]
shrinkSunk (Sunk ts0 ts1 tm) = Sunk ts0 ts1 <$> shrinkTerm tm

-- | A beta-redex: a lambda or type lambda in the first scope, as in 'Sunk',
-- and an argument in the second one
data Redex
  = forall n l. (Distinct n, DExt n l) => TmRedex (TestScope n) (TestScope l) (Term n) (Term l)
  -- ^ @(\x -> body) arg@
  | forall n l. (Distinct n, DExt n l) => TyRedex (TestScope n) (TestScope l) (Term n) (Type l)
  -- ^ @(/\a -> body) \@ty@

instance Show Redex where
  show = \case
    TmRedex ts0 ts1 fun arg -> common ts0 ts1 fun <> "\nargument:\n" <> showT arg
    TyRedex ts0 ts1 fun ty -> common ts0 ts1 fun <> "\nargument:\n" <> showTy ty
   where
    common :: TestScope x -> TestScope y -> Term x -> String
    common ts0 ts1 fun =
      showTestScope ts0 <> "\nsunk into the scope of:\n" <> showTestScope ts1 <>
        "\nfunction:\n" <> showT fun

genRedex :: Gen Redex
genRedex = do
  SomeScope ts0 <- genTestScope
  ExtScope ts1 <- genMoreVars ts0
  let ctx0 = ctxOf ts0
      sc0 = testScope ts0
      ctx1 = ctxOf ts1
  sized $ \s -> frequency
    [ (2, do ty <- genType ctx0 P.liftedTypeKind 2
             fun <- withFreshIdBinder sc0 (occName "x") ty $ \sc b ->
               Lam b <$> genTerm (bindCtx sc [] [idBinderVar b] ctx0) (termSize s)
             TmRedex ts0 ts1 fun <$> genTerm ctx1 (termSize s `div` 3))
      -- A body that mentions a global, which has the raw name of the binder
      -- when 'globals' clash with it
    , (1, do ty <- genType ctx0 P.liftedTypeKind 2
             g <- elements globals
             fun <- withFreshIdBinder sc0 (occName "x") ty $ \sc b -> do
               body <- genTerm (bindCtx sc [] [idBinderVar b] ctx0) (termSize s)
               pure (Lam b (App body (GlobalVar g)))
             TmRedex ts0 ts1 fun <$> genTerm ctx1 (termSize s `div` 3))
    , (1, do k <- genKind ctx0
             (fun, kL) <- withFreshTyBinder sc0 (occName "a") k $ \sc b -> do
               body <- genTerm (bindCtx sc [tyBinderVar b] [] ctx0) (termSize s)
               pure (TyLam b body, k)
             TyRedex ts0 ts1 fun <$> genType ctx1 (sink kL) 2)
    ]

shrinkRedex :: Redex -> [Redex]
shrinkRedex = \case
  TmRedex ts0 ts1 fun arg ->
    [TmRedex ts0 ts1 (Lam b body') arg | Lam b body <- [fun], body' <- shrinkTerm body] <>
    [TmRedex ts0 ts1 fun arg' | arg' <- shrinkTerm arg]
  TyRedex ts0 ts1 fun ty ->
    [TyRedex ts0 ts1 (TyLam b body') ty | TyLam b body <- [fun], body' <- shrinkTerm body]

-- | A case alternative that matches a data constructor, in the first scope, as
-- in 'Sunk', and types for its existentials and terms for its fields in the
-- second one: the alternative of case-of-known-constructor
data KnownCon = forall n l. (Distinct n, DExt n l) => KnownCon (TestScope n) (TestScope l) (Alt n) [Type l] [Term l]

instance Show KnownCon where
  show (KnownCon ts0 ts1 alt tys tms) =
    showTestScope ts0 <> "\nsunk into the scope of:\n" <> showTestScope ts1 <>
      "\nalternative:\n" <> showT (Case (Literal (IntLiteral 0)) intTy [alt]) <>
      "\ntypes:\n" <> unlines (map showTy tys) <> "terms:\n" <> unlines (map showT tms)

genKnownCon :: Gen KnownCon
genKnownCon = do
  SomeScope ts0 <- genTestScope
  ExtScope ts1 <- genMoreVars ts0
  let ctx0 = ctxOf ts0
      ctx1 = ctxOf ts1
  sized $ \s -> do
    gc <- elements genCons
    univTys <- mapM (\k -> genType ctx0 (sinkClosed k) 1) (gcUnivKinds gc)
    alt <- withGenPat ctx0 gc univTys $ \ctx' p -> Alt p <$> genTerm ctx' (termSize s)
    let extKinds = gcExtKinds gc (map sink univTys)
    tys <- mapM (\k -> genType ctx1 k 2) extKinds
    let nFields = length (gcArgTys gc univTys (map (const intTy) extKinds))
    tms <- replicateM nFields (genTerm ctx1 (termSize s `div` 4))
    pure (KnownCon ts0 ts1 alt tys tms)

shrinkKnownCon :: KnownCon -> [KnownCon]
shrinkKnownCon (KnownCon ts0 ts1 (Alt p rhs) tys tms) =
  [KnownCon ts0 ts1 (Alt p rhs') tys tms | rhs' <- shrinkTerm rhs] <>
  [ KnownCon ts0 ts1 (Alt p rhs) tys (pre <> [tm'] <> post)
  | (pre, tm : post) <- splits tms, tm' <- shrinkTerm tm ]

-- | Types to instantiate a data constructor with, in a scope: types for its
-- universally quantified type variables, and for its existentially quantified
-- ones, of their kinds. Sometimes there is one type too few or too many.
data DataConInst = forall n. Distinct n => DataConInst (TestScope n) GenCon [Type n] [Type n]

instance Show DataConInst where
  show (DataConInst ts gc univTys extTys) =
    showTestScope ts <> "\ndata constructor: " <> showPprU (gcDataCon gc) <>
      "\nuniversal types: " <> List.intercalate ", " (map showTy univTys) <>
      "\nexistential types: " <> List.intercalate ", " (map showTy extTys)

genDataConInst :: Gen DataConInst
genDataConInst = do
  SomeScope ts <- genTestScope
  gc <- elements genCons
  let ctx = ctxOf ts
  univTys <- mapM (\k -> genType ctx (sinkClosed k) 2) (gcUnivKinds gc)
  extTys <- mapM (\k -> genType ctx k 2) (gcExtKinds gc univTys)
  frequency
    [ (8, pure (DataConInst ts gc univTys extTys))
    , (1, pure (DataConInst ts gc univTys (drop 1 extTys)))
    , (1, DataConInst ts gc univTys . (extTys <>) . pure <$> genType ctx P.liftedTypeKind 1)
    ]

-- | A case-expression whose alternatives mostly have the same right-hand side
-- (sunk into the scope of their patterns), and sometimes one of their own
genSameRhsCase :: Gen InScope
genSameRhsCase = do
  SomeScope ts <- genTestScope
  let ctx = ctxOf ts
  sized $ \s -> do
    subj <- genTerm ctx 1
    rhs <- genTerm ctx (termSize s `div` 2)
    k <- choose (1, 3)
    alts <- replicateM k $ do
      same <- frequency [(3, pure True), (1, pure False)]
      frequency
        [ (5, do gc <- elements genCons
                 univTys <- mapM (\ki -> genType ctx (sinkClosed ki) 1) (gcUnivKinds gc)
                 withGenPat ctx gc univTys $ \ctx' p ->
                   Alt p <$> (if same then pure (sink rhs) else genTerm ctx' 1))
        , (1, do l <- IntLiteral <$> choose (0, 3)
                 Alt (LitPat l) <$> (if same then pure rhs else genTerm ctx 1))
        , (1, Alt DefaultPat <$> (if same then pure rhs else genTerm ctx 1))
        ]
    pure (InScope ts (Case subj intTy alts))

-- | A term like the given one, but changed in one place: a variable occurrence
-- replaced by another one in scope, a literal or type argument changed, a case
-- alternative dropped, or a subterm replaced by a leaf. The result may still
-- be alpha-equivalent to the input.
genPerturbed :: Distinct n => TestScope n -> Term n -> Gen (Term n)
genPerturbed ts tm0 = case edits (ctxOf ts) tm0 of
  [] -> pure tm0
  es -> do
    i <- choose (0, length es - 1)
    es !! i
 where
  -- One generator per position in the term, of the term changed there
  edits :: Distinct x => Ctx x -> Term x -> [Gen (Term x)]
  edits ctx tm = here <> inside
   where
    sc = ctxScope ctx
    here = genTerm ctx 0 : case tm of
      Var {} | not (null (ctxIds ctx)) -> [elements (ctxIds ctx)]
      Literal (IntLiteral n) -> [pure (Literal (IntLiteral (n + 1)))]
      TyApp e _ -> [TyApp e <$> genType ctx P.liftedTypeKind 1]
      Case e ty alts@(_:_:_) -> [pure (Case e ty (drop 1 alts))]
      _ -> []

    inside = case tm of
      Lam b e -> openIdBinder sc b e $ \sc' b' e' ->
        fmap (Lam b') <$> edits (bindCtx sc' [] [idBinderVar b'] ctx) e'
      TyLam b e -> openTyBinder sc b e $ \sc' b' e' ->
        fmap (TyLam b') <$> edits (bindCtx sc' [tyBinderVar b'] [] ctx) e'
      App e1 e2 -> (fmap (`App` e2) <$> edits ctx e1) <> (fmap (App e1) <$> edits ctx e2)
      TyApp e ty -> fmap (`TyApp` ty) <$> edits ctx e
      Let bind e -> openBind sc bind e $ \sc' bind' e' -> case bind' of
        NonRec b x ->
          (fmap (\x' -> Let (NonRec b x') e') <$> edits ctx x) <>
            (fmap (Let bind') <$> edits (bindCtx sc' [] [idBinderVar b] ctx) e')
        Rec bs xs ->
          let ctx' = bindCtx sc' [] (teleIdVars bs) ctx
          in  (fmap (Let bind') <$> edits ctx' e') <>
                [ fmap (\x' -> Let (Rec bs (pre <> [x'] <> post)) e') g
                | (pre, x : post) <- splits xs
                , g <- edits ctx' x ]
      Case e ty alts ->
        (fmap (\e' -> Case e' ty alts) <$> edits ctx e) <>
          [ fmap (\a' -> Case e ty (pre <> [a'] <> post)) g
          | (pre, alt : post) <- splits alts
          , g <- openAlt sc alt $ \sc' p rhs ->
                   fmap (Alt p) <$> edits (patCtx sc' p ctx) rhs ]
      Cast e t1 t2 -> fmap (\e' -> Cast e' t1 t2) <$> edits ctx e
      Tick t e -> fmap (Tick t) <$> edits ctx e
      _ -> []

  patCtx :: DExt x l => Scope l -> Pat x l -> Ctx x -> Ctx l
  patCtx sc p ctx = case p of
    DataPat {} -> let (tvs, ids) = patVars p in bindCtx sc tvs ids ctx
    LitPat _ -> ctx
    DefaultPat -> ctx

-- * Shrinking

-- | Shrink a term: replace it by a literal or one of its subterms in its
-- scope, drop case alternatives, simplify types, or shrink a subterm
shrinkTerm :: Term n -> [Term n]
shrinkTerm tm = [lit | not (isLit tm)] <> children <> inner
 where
  lit = Literal (IntLiteral 0)
  isLit = \case
    Literal {} -> True
    _ -> False

  children = case tm of
    Lam b e -> maybe [] pure (unsinkTerm (idBinderNameBinders b) e)
    TyLam b e -> maybe [] pure (unsinkTerm (tyBinderNameBinders b) e)
    App e1 e2 -> [e1, e2]
    TyApp e _ -> [e]
    Let bind@(NonRec _ x) e -> x : maybe [] pure (unsinkTerm (bindNameBinders bind) e)
    Let bind e -> maybe [] pure (unsinkTerm (bindNameBinders bind) e)
    Case e _ alts -> e : [e' | Alt p a <- alts, Just e' <- [unsinkTerm (patNameBinders p) a]]
    Cast e _ _ -> [e]
    Tick _ e -> [e]
    _ -> []

  inner = case tm of
    Lam b e -> Lam b <$> shrinkTerm e
    TyLam b e -> TyLam b <$> shrinkTerm e
    App e1 e2 -> [App e1' e2 | e1' <- shrinkTerm e1] <> [App e1 e2' | e2' <- shrinkTerm e2]
    TyApp e ty -> [TyApp e intTy | not (aeqTy ty intTy)] <> [TyApp e' ty | e' <- shrinkTerm e]
    Let (NonRec b x) e ->
      [Let (NonRec b x') e | x' <- shrinkTerm x] <> [Let (NonRec b x) e' | e' <- shrinkTerm e]
    Let (Rec bs xs) e ->
      [Let (Rec bs xs) e' | e' <- shrinkTerm e] <>
      [ Let (Rec bs (pre <> [x'] <> post)) e
      | (pre, x : post) <- splits xs, x' <- shrinkTerm x ]
    Case e ty alts ->
      [Case e intTy alts | not (aeqTy ty intTy)] <>
      [Case e ty (pre <> post) | (pre, _ : post) <- splits alts, not (null (pre <> post))] <>
      [Case e' ty alts | e' <- shrinkTerm e] <>
      [ Case e ty (pre <> [Alt p a'] <> post)
      | (pre, Alt p a : post) <- splits alts, a' <- shrinkTerm a ]
    Cast e t1 t2 -> [Cast e' t1 t2 | e' <- shrinkTerm e]
    Tick t e -> Tick t <$> shrinkTerm e
    _ -> []

-- | All ways to split a list
splits :: [a] -> [([a], [a])]
splits xs = [splitAt i xs | i <- [0 .. length xs - 1]]

-- * Comparing

-- | Whether two terms are the same, with the same raw names, for the parts
-- that 'Clash.Core.Scoped.Alpha.aeqTm' compares: the human readable names of
-- binders and variables, the types of variable occurrences, and the result
-- types of case-expressions don't matter. After
-- 'Clash.Core.Scoped.Subst.freshenDistinct', which names binders by their
-- position, two terms in the same scope are the same if and only if they are
-- alpha-equivalent: an oracle for 'Clash.Core.Scoped.Alpha.aeqTm'.
rawEqTm :: Term n -> Term m -> Bool
rawEqTm = goTm
 where
  goTy :: Type x -> Type y -> Bool
  goTy (VarTy _ _ n1) (VarTy _ _ n2) = tyNameRaw n1 == tyNameRaw n2
  goTy (ConstTy c1) (ConstTy c2) = c1 == c2
  goTy (ForAllTy b1 t1) (ForAllTy b2 t2) = goTyB b1 b2 && goTy t1 t2
  goTy (AppTy s1 t1) (AppTy s2 t2) = goTy s1 s2 && goTy t1 t2
  goTy (LitTy l1) (LitTy l2) = l1 == l2
  goTy (AnnType a1 t1) (AnnType a2 t2) = a1 == a2 && goTy t1 t2
  goTy _ _ = False

  goTyB :: TyBinder x x' -> TyBinder y y' -> Bool
  goTyB b1 b2 =
    tyNameRaw (tyNameOf b1) == tyNameRaw (tyNameOf b2) && goTy (tyBinderKind b1) (tyBinderKind b2)

  goIdB :: IdBinder x x' -> IdBinder y y' -> Bool
  goIdB b1 b2 =
    tmNameRaw (tmNameOf b1) == tmNameRaw (tmNameOf b2) && goTy (idBinderType b1) (idBinderType b2)

  goTele :: (forall a a' c c'. b a a' -> b c c' -> Bool) -> Tele b x x' -> Tele b y y' -> Bool
  goTele _ TNil TNil = True
  goTele f (TCons b1 bs1) (TCons b2 bs2) = f b1 b2 && goTele f bs1 bs2
  goTele _ _ _ = False

  goTm :: Term x -> Term y -> Bool
  goTm (Var _ _ n1) (Var _ _ n2) = tmNameRaw n1 == tmNameRaw n2
  goTm (GlobalVar g1) (GlobalVar g2) = g1 == g2
  goTm (Data dc1) (Data dc2) = dc1 == dc2
  goTm (Literal l1) (Literal l2) = l1 == l2
  goTm (Prim p1) (Prim p2) = primName p1 == primName p2
  goTm (Lam b1 e1) (Lam b2 e2) = goIdB b1 b2 && goTm e1 e2
  goTm (TyLam b1 e1) (TyLam b2 e2) = goTyB b1 b2 && goTm e1 e2
  goTm (App l1 r1) (App l2 r2) = goTm l1 l2 && goTm r1 r2
  goTm (TyApp l1 r1) (TyApp l2 r2) = goTm l1 l2 && goTy r1 r2
  goTm (Let (NonRec b1 x1) e1) (Let (NonRec b2 x2) e2) = goIdB b1 b2 && goTm x1 x2 && goTm e1 e2
  goTm (Let (Rec bs1 xs1) e1) (Let (Rec bs2 xs2) e2) =
    goTele goIdB bs1 bs2 && length xs1 == length xs2 && and (zipWith goTm (e1 : xs1) (e2 : xs2))
  goTm (Case e1 _ as1) (Case e2 _ as2) =
    goTm e1 e2 && length as1 == length as2 && and (zipWith goAlt as1 as2)
  goTm (Cast e1 l1 r1) (Cast e2 l2 r2) = goTm e1 e2 && goTy l1 l2 && goTy r1 r2
  goTm (Tick t1 e1) (Tick t2 e2) = goTick t1 t2 && goTm e1 e2
  goTm _ _ = False

  goAlt :: Alt x -> Alt y -> Bool
  goAlt (Alt p1 e1) (Alt p2 e2) = goPat p1 p2 && goTm e1 e2

  goPat :: Pat x x' -> Pat y y' -> Bool
  goPat (DataPat dc1 tvs1 ids1) (DataPat dc2 tvs2 ids2) =
    dc1 == dc2 && goTele goTyB tvs1 tvs2 && goTele goIdB ids1 ids2
  goPat (LitPat l1) (LitPat l2) = l1 == l2
  goPat DefaultPat DefaultPat = True
  goPat _ _ = False

  goTick :: TickInfo x -> TickInfo y -> Bool
  goTick (SrcSpan s1) (SrcSpan s2) = s1 == s2
  goTick (NameMod m1 t1) (NameMod m2 t2) = m1 == m2 && goTy t1 t2
  goTick DeDup DeDup = True
  goTick NoDeDup NoDeDup = True
  goTick (Attributes t1 e1) (Attributes t2 e2) = goTy t1 t2 && goTm e1 e2
  goTick _ _ = False

-- | Add a million to the uniques of the human readable names of all
-- variables, bound and free. Changes neither the raw names nor the meaning of
-- a term.
renameOccs :: Term n -> Term n
renameOccs = goTm
 where
  ren :: C.Name a -> C.Name a
  ren nm = nm {C.nameUniq = C.nameUniq nm + 1000000}

  goTy :: Type x -> Type x
  goTy = \case
    VarTy occ k n -> VarTy (ren occ) (goTy k) n
    ForAllTy b t -> ForAllTy (goTyB b) (goTy t)
    AppTy s t -> AppTy (goTy s) (goTy t)
    AnnType a t -> AnnType a (goTy t)
    t -> t

  goTyB :: TyBinder x x' -> TyBinder x x'
  goTyB (TyBinder occ k n) = TyBinder (ren occ) (goTy k) n

  goIdB :: IdBinder x x' -> IdBinder x x'
  goIdB (IdBinder occ ty n) = IdBinder (ren occ) (goTy ty) n

  goTele :: (forall a a'. b a a' -> b a a') -> Tele b x x' -> Tele b x x'
  goTele _ TNil = TNil
  goTele f (TCons b bs) = TCons (f b) (goTele f bs)

  goTm :: Term x -> Term x
  goTm = \case
    Var occ ty n -> Var (ren occ) (goTy ty) n
    Lam b e -> Lam (goIdB b) (goTm e)
    TyLam b e -> TyLam (goTyB b) (goTm e)
    App l r -> App (goTm l) (goTm r)
    TyApp e t -> TyApp (goTm e) (goTy t)
    Let (NonRec b x) e -> Let (NonRec (goIdB b) (goTm x)) (goTm e)
    Let (Rec bs xs) e -> Let (Rec (goTele goIdB bs) (map goTm xs)) (goTm e)
    Case e ty alts -> Case (goTm e) (goTy ty) (map goAlt alts)
    Cast e l r -> Cast (goTm e) (goTy l) (goTy r)
    Tick t e -> Tick (goTick t) (goTm e)
    e -> e

  goAlt :: Alt x -> Alt x
  goAlt (Alt p e) = case p of
    DataPat dc tvs ids -> Alt (DataPat dc (goTele goTyB tvs) (goTele goIdB ids)) (goTm e)
    _ -> Alt p (goTm e)

  goTick :: TickInfo x -> TickInfo x
  goTick = \case
    NameMod m t -> NameMod m (goTy t)
    Attributes t e -> Attributes (goTy t) (goTm e)
    t -> t

-- * Free variables, by an independent oracle
--
-- Simple traversals that collect the raw names of variable occurrences that
-- no enclosing binder of the same sort binds: the specification of the free
-- variables of "Clash.Core.Scoped.FreeVars", for terms in which no occurrence
-- refers to a variable that a binder of the other sort made inaccessible, as
-- the generated and the scrambled terms are.

-- | The raw names of the free type variables of a term, including those of
-- the types of its variable occurrences and binders, and of the kinds of its
-- type variables, in ascending order
refFreeTyVars :: Term n -> [Int]
refFreeTyVars = IntSet.toAscList . fst . refFree

-- | The raw names of the free term variables of a term, in ascending order
refFreeTmVars :: Term n -> [Int]
refFreeTmVars = IntSet.toAscList . snd . refFree

-- | The raw names of the free type variables of a type, see 'refFreeTyVars'
refFreeTyVarsOfType :: Type n -> [Int]
refFreeTyVarsOfType = IntSet.toAscList . refTy IntSet.empty

-- | The raw names of the free term variables of a term, with the number of
-- their occurrences, in ascending order
refFreeTmOccurrences :: Term n -> [(Int, Int)]
refFreeTmOccurrences =
  IntMap.toAscList . IntMap.fromListWith (+) . map (\k -> (k, 1)) . refTmOccs IntSet.empty

-- | The uniques of the global variables a term refers to, in ascending order
refGlobals :: Term n -> [Unique]
refGlobals = List.sort . List.nub . go
 where
  go :: Term x -> [Unique]
  go = \case
    GlobalVar g -> [globalIdUniq g]
    Lam _ e -> go e
    TyLam _ e -> go e
    App e1 e2 -> go e1 <> go e2
    TyApp e _ -> go e
    Let (NonRec _ x) e -> go x <> go e
    Let (Rec _ xs) e -> concatMap go (e : xs)
    Case e _ alts -> go e <> concat [go a | Alt _ a <- alts]
    Cast e _ _ -> go e
    Tick (Attributes _ x) e -> go x <> go e
    Tick _ e -> go e
    _ -> []

refFree :: Term n -> (IntSet.IntSet, IntSet.IntSet)
refFree = refTm IntSet.empty IntSet.empty

refTy :: IntSet.IntSet -> Type n -> IntSet.IntSet
refTy bound = \case
  VarTy _ k n -> free (tyNameRaw n) <> refTy bound k
  ForAllTy b t -> refTy bound (tyBinderKind b) <> refTy (IntSet.insert (tyNameRaw (tyNameOf b)) bound) t
  AppTy t1 t2 -> refTy bound t1 <> refTy bound t2
  AnnType _ t -> refTy bound t
  ConstTy {} -> IntSet.empty
  LitTy {} -> IntSet.empty
 where
  free k = if IntSet.member k bound then IntSet.empty else IntSet.singleton k

-- | The free type variables and term variables of a term, given the raw
-- names of the bound type variables and term variables
refTm :: IntSet.IntSet -> IntSet.IntSet -> Term n -> (IntSet.IntSet, IntSet.IntSet)
refTm btys btms = \case
  Var _ ty n -> (refTy btys ty, if IntSet.member (tmNameRaw n) btms then IntSet.empty else IntSet.singleton (tmNameRaw n))
  Lam b e -> tyOf (idBinderType b) <> refTm btys (bindTm b) e
  TyLam b e -> tyOf (tyBinderKind b) <> refTm (bindTy b) btms e
  App e1 e2 -> go e1 <> go e2
  TyApp e ty -> go e <> tyOf ty
  Let (NonRec b x) e -> tyOf (idBinderType b) <> go x <> refTm btys (bindTm b) e
  Let (Rec bs xs) e -> idTele btms bs (\btms' -> foldMap (refTm btys btms') (e : xs))
  Case e ty alts -> go e <> tyOf ty <> foldMap alt alts
  Cast e t1 t2 -> go e <> tyOf t1 <> tyOf t2
  Tick (NameMod _ ty) e -> tyOf ty <> go e
  Tick (Attributes ty x) e -> tyOf ty <> go x <> go e
  Tick _ e -> go e
  _ -> (IntSet.empty, IntSet.empty)
 where
  go :: Term x -> (IntSet.IntSet, IntSet.IntSet)
  go = refTm btys btms
  tyOf :: Type x -> (IntSet.IntSet, IntSet.IntSet)
  tyOf ty = (refTy btys ty, IntSet.empty)
  bindTm :: IdBinder x y -> IntSet.IntSet
  bindTm b = IntSet.insert (tmNameRaw (tmNameOf b)) btms
  bindTy :: TyBinder x y -> IntSet.IntSet
  bindTy b = IntSet.insert (tyNameRaw (tyNameOf b)) btys

  -- The types of the binders of a telescope are in the scope of the binders
  -- before them, which bind term variables only
  idTele
    :: IntSet.IntSet -> Tele IdBinder x y
    -> (IntSet.IntSet -> (IntSet.IntSet, IntSet.IntSet)) -> (IntSet.IntSet, IntSet.IntSet)
  idTele bt TNil k = k bt
  idTele bt (TCons b bs) k =
    (refTy btys (idBinderType b), IntSet.empty) <> idTele (IntSet.insert (tmNameRaw (tmNameOf b)) bt) bs k

  alt :: Alt x -> (IntSet.IntSet, IntSet.IntSet)
  alt (Alt p e) = case p of
    DataPat _ tvs ids -> tyTele btys tvs (\btys' ->
      let (fTys, (fTms, btms')) = idTeleIn btys' btms ids
      in  (fTys, IntSet.empty) <> (IntSet.empty, fTms) <> refTm btys' btms' e)
    LitPat _ -> go e
    DefaultPat -> go e

  tyTele
    :: IntSet.IntSet -> Tele TyBinder x y
    -> (IntSet.IntSet -> (IntSet.IntSet, IntSet.IntSet)) -> (IntSet.IntSet, IntSet.IntSet)
  tyTele bt TNil k = k bt
  tyTele bt (TCons b bs) k =
    (refTy bt (tyBinderKind b), IntSet.empty) <> tyTele (IntSet.insert (tyNameRaw (tyNameOf b)) bt) bs k

  -- The free type variables of the types of the fields, and the bound term
  -- variables after them
  idTeleIn
    :: IntSet.IntSet -> IntSet.IntSet -> Tele IdBinder x y
    -> (IntSet.IntSet, (IntSet.IntSet, IntSet.IntSet))
  idTeleIn _ bt TNil = (IntSet.empty, (IntSet.empty, bt))
  idTeleIn bty bt (TCons b bs) =
    let (fTys, rest) = idTeleIn bty (IntSet.insert (tmNameRaw (tmNameOf b)) bt) bs
    in  (refTy bty (idBinderType b) <> fTys, rest)

-- | The raw names of the occurrences of free term variables, given the bound
-- ones
refTmOccs :: IntSet.IntSet -> Term n -> [Int]
refTmOccs bound = \case
  Var _ _ n -> [tmNameRaw n | not (IntSet.member (tmNameRaw n) bound)]
  Lam b e -> refTmOccs (bind b bound) e
  TyLam _ e -> go e
  App e1 e2 -> go e1 <> go e2
  TyApp e _ -> go e
  Let (NonRec b x) e -> go x <> refTmOccs (bind b bound) e
  Let (Rec bs xs) e -> let bound' = tele bs bound in concatMap (refTmOccs bound') (e : xs)
  Case e _ alts -> go e <> concat [refTmOccs (patB p) a | Alt p a <- alts]
  Cast e _ _ -> go e
  Tick (Attributes _ x) e -> go x <> go e
  Tick _ e -> go e
  _ -> []
 where
  go :: Term x -> [Int]
  go = refTmOccs bound
  bind :: IdBinder x y -> IntSet.IntSet -> IntSet.IntSet
  bind b = IntSet.insert (tmNameRaw (tmNameOf b))
  tele :: Tele IdBinder x y -> IntSet.IntSet -> IntSet.IntSet
  tele TNil s = s
  tele (TCons b bs) s = tele bs (bind b s)
  patB :: Pat x y -> IntSet.IntSet
  patB = \case
    DataPat _ _ ids -> tele ids bound
    _ -> bound

-- * Showing

-- | Pretty print a term, with raw names
showT :: Term n -> String
showT = showPprU

-- | Pretty print a type, with raw names
showTy :: Type n -> String
showTy = showPprU
