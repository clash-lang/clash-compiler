{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the free variables of scoped Clash Core. Ports of the tests in
  "Clash.Tests.Core.FreeVars", and tests of occurrence checks.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Core.Scoped.FreeVars (tests) where

import qualified Control.Lens as Lens
import qualified Data.List as List
import Data.Maybe (isJust)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Clash.Core.FreeVars as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.TysPrim as C
import qualified Clash.Core.Var as C

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.FreeVars
import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Subst
  (strengthenTyPat, unsinkAlt, unsinkIdBinder, unsinkTerm, unsinkTyBinder, unsinkType)
import Clash.Core.Scoped.Syntax

import Test.Clash.Rewrite
  ( globalId, intId, intTy, kindedTyVar, localId, pairDataCon, pairTy
  , parseToTermQQ, tyVar)
import Test.Clash.Scoped
  ( abstractFreeVars, extendFreeVars, extendSunk, forAllScrambled, nameSetRaws, sinkInto
  , withFreeVars, withFreeVarsOf, withTests)
import Test.Clash.Scoped.Gen
  ( FreeVars (..), InScope (..), Redex (..), Sunk (..), ctxOf, genFreeVars, genInScope
  , genRedex, genSameRhsCase, genSunk, genType, patBindersFreeIn, runG, shrinkInScope
  , shrinkRedex, shrinkSunk, showTerm, showType)

-- | The uniques of the free local term variables of a Clash term, as raw
-- names, in ascending order
clashFreeIds :: C.Term -> [Int]
clashFreeIds =
  List.sort . List.nub . map (fromIntegral . C.varUniq) . Lens.toListOf C.freeLocalIds

-- | The uniques of the free type variables of a Clash term, as raw names, in
-- ascending order
clashFreeTyVars :: C.Term -> [Int]
clashFreeTyVars =
  List.sort . List.nub . map (fromIntegral . C.varUniq) . Lens.toListOf C.termFreeTyVars

-- | The free variables of a converted term are those of the Clash term, as
-- the free variables of the conversion environment keep their uniques
freeVarsAgree :: C.Term -> Term n -> Property
freeVarsAgree tm tm1 =
  counterexample "free term variables" (nameSetRaws (freeTmVars tm1) === clashFreeIds tm) .&&.
    counterexample "free type variables" (nameSetRaws (freeTyVars tm1) === clashFreeTyVars tm)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.FreeVars"
  [ -- @\f -> g f@, where the local @f@ and the global @g@ have the same unique
    testCase "local and global variables are kept apart" $ do
      let fL = intId C.User "f" 20
          fG = globalId C.User "f" 20 intTy
          tm = fromClashTermClosed (C.Lam fL (C.App (C.Var fG) (C.Var fL)))
      globalSetToList (termGlobalIds tm) @?= [mkGlobalId fG]
      assertBool "no free local variables" (nullNameSet (freeTmVars tm))

    -- In @forall k. b@ with @b :: k@, the @k@ in the kind of the free @b@ is
    -- not the @k@ bound by the @forall@, so it's free.
    -- https://github.com/clash-lang/clash-compiler/issues/398,
    -- https://github.com/clash-lang/clash-compiler/pull/446
  , testCase "free type variables close over kinds (#446)" $ do
      let k = tyVar C.User "k" 1
          b = kindedTyVar C.User "b" 2 (C.VarTy k)
          ty = C.ForAllTy k (C.VarTy b)
      withConvEnv [k, b] [] $ \env ->
        nameSetRaws (freeTyVarsOfType (fromClashType env ty)) @?= [1, 2]

    -- The term level equivalent: in @/\k -> a@ with @a :: k@, the @k@ in the
    -- type of the free @a@ is free.
  , testCase "free type variables of a term close over types (#446)" $ do
      let k = tyVar C.User "k" 1
          a = localId C.User "a" 2 (C.VarTy k)
          tm = C.TyLam k (C.Var a)
      withConvEnv [k] [a] $ \env -> do
        let tm1 = fromClashTerm env tm
        nameSetRaws (freeTyVars tm1) @?= [1]
        nameSetRaws (freeTmVars tm1) @?= [2]

    -- In Clash Core, the binder of a type lambda hides a free term variable
    -- with the same unique, as both live in one in-scope set. In scoped Core,
    -- the binder is renamed, and the term variable stays free.
  , testCase "a type variable binder doesn't hide a term variable" $ do
      let a = tyVar C.User "a" 20
          x = intId C.User "x" 20
          tm = C.TyLam a (C.Var x)
      withConvEnv [] [x] $ \env -> do
        let tm1 = fromClashTerm env tm
        nameSetRaws (freeTmVars tm1) @?= [20]
        assertBool "no free type variables" (nullNameSet (freeTyVars tm1))

  , testCase "freeVars splits type and term variables" $ do
      let tm = [parseToTermQQ| \(y :: Int) -> f_10 y (x_11 :: a_12) |]
      withFreeVarsOf [tm] $ \env -> do
        let (tvs, ids) = freeVars (fromClashTerm env tm)
        nameSetRaws tvs @?= [12]
        nameSetRaws ids @?= [10, 11]

  , testCase "occurrence checks look at the types of occurrences" $ do
      -- The type variable only occurs in the type of the free variable x
      let a = tyVar C.User "a" 1
          x = localId C.User "x" 2 (C.VarTy a)
      withConvEnv [a] [x] $ \env -> case fromClashTerm env (C.Var x) of
        tm@(Var _ (VarTy _ _ an) xn) -> do
          assertBool "a occurs" (tyNameOccursIn an tm)
          assertBool "x occurs" (tmNameOccursIn xn tm)
        _ -> assertFailure "unexpected shape"

  , testCase "occurrence checks account for shadowing binders" $ do
      -- Convert @\y -> y@ in the empty scope, and sink it into the scope of
      -- 'sunkVar' and @x@, where its binder has the same name as @x@
      let lam = fromClashTermClosed [parseToTermQQ| \(y_1 :: Int) -> y_1 |]
          x = intId C.User "x" 1
      extendSunk emptyConvEnv [] [x] $ \env -> case fromClashTerm env (C.Var x) of
        Var _ _ xn -> do
          let tm = sink lam
          assertBool "x does not occur" (not (tmNameOccursIn xn tm))
          assertBool "no free variables" (nullNameSet (freeTmVars tm))
        _ -> assertFailure "unexpected shape"

  , testCase "patBindersOccurIn" $ do
      -- @\s -> case s of MkPair a b -> a@
      let s = localId C.User "s" 1 pairTy
          a = intId C.User "a" 2
          b = intId C.User "b" 3
          tm = C.Lam s (C.Case (C.Var s) intTy [(C.DataPat pairDataCon [] [a, b], C.Var a)])
      case fromClashTermClosed tm of
        Lam _ (Case _ _ [Alt p e]) -> do
          assertBool "a occurs" (patBindersOccurIn p e)
          case p of
            DataPat _ TNil (TCons ba (TCons _ TNil)) ->
              assertBool "only a occurs"
                (nameSetRaws (freeTmVars e) == [tmNameRaw (tmNameOf ba)])
            _ -> assertFailure "unexpected pattern"
        _ -> assertFailure "unexpected shape"

  , withTests 500 $ testGroup "properties"
      [ testProperty "free variables agree with Clash, per namespace" $
          forAllShrink genInScope shrinkInScope $ \(InScope fvs tm) ->
            withFreeVars fvs $ \env -> freeVarsAgree tm (fromClashTerm env tm)

      , testProperty "free variables of sunk terms agree with Clash" $
          forAllShrink genSunk shrinkSunk $ \(Sunk fvs0 fvs1 tm) ->
            withFreeVars fvs0 $ \env0 -> do
              let tm0 = fromClashTerm env0 tm
              extendFreeVars env0 fvs1 $ \env1 ->
                freeVarsAgree tm (sinkInto (convScope env1) tm0)

      , testProperty "globals agree with Clash" $
          forAllShrink genInScope shrinkInScope $ \(InScope fvs tm) ->
            withFreeVars fvs $ \env ->
              map globalIdUniq (globalSetToList (termGlobalIds (fromClashTerm env tm)))
                === List.sort (List.nub (map C.varUniq (Lens.toListOf C.globalIds tm)))

      , testProperty "occurrence checks agree with free variables" $
          forAllShrink genInScope shrinkInScope $ \(InScope fvs@(FreeVars tvs ids) tm) ->
            withFreeVars fvs $ \env -> do
              let tm1 = fromClashTerm env tm
              conjoin $
                [ counterexample (show (tmNameRaw n)) $
                    tmNameOccursIn n tm1 === memberNameSet n (freeTmVars tm1)
                | Var _ _ n <- map (fromClashTerm env . C.Var) ids ] <>
                [ counterexample (show (tyNameRaw n)) $
                    tyNameOccursIn n tm1 === memberNameSet n (freeTyVars tm1)
                | VarTy _ _ n <- map (fromClashType env . C.VarTy) tvs ]

        -- Moving a term out of the scope of a binder succeeds if and only if
        -- the binder doesn't occur in it
      , testProperty "unsinking a binder agrees with Clash's free variables" $
          forAllShrink genRedex shrinkRedex $ \redex -> case redex of
            TmRedex fvs0 _ x body _ -> withFreeVars fvs0 $ \env ->
              case fromClashTerm env (C.Lam x body) of
                Lam b e -> isJust (unsinkIdBinder b e)
                  === notElem (fromIntegral (C.varUniq x)) (clashFreeIds body)
                _ -> property False
            TyRedex fvs0 _ a body _ -> withFreeVars fvs0 $ \env ->
              case fromClashTerm env (C.TyLam a body) of
                TyLam b e -> isJust (unsinkTyBinder b e)
                  === notElem (fromIntegral (C.varUniq a)) (clashFreeTyVars body)
                _ -> property False

        -- With scrambled binders, binders have the names of variables in
        -- scope, and shadow each other
      , testProperty "unsinking agrees with free variables at every binder" $
          forAllShrink genSunk shrinkSunk $ \(Sunk fvs0 fvs1 tm) ->
            withFreeVars fvs0 $ \env0 -> do
              let tm0 = fromClashTerm env0 tm
              extendFreeVars env0 fvs1 $ \env1 -> do
                let sc = convScope env1
                forAllScrambled sc (sinkInto sc tm0) $ \tm1 ->
                  conjoin (freeVarsAgree tm tm1 : unsinkChecks tm1)

        -- Abstracting over the free variables of the scope it is converted in
        -- gives terms without free variables
      , testProperty "hasFreeVars agrees with free variables" $
          forAllShrink genSunk shrinkSunk $ \(Sunk fvs0 fvs1 tm) -> forAll arbitrary $ \abstract ->
            withFreeVars fvs0 $ \env0 -> do
              let tm0
                    | abstract = sinkClosed (fromClashTermClosed (abstractFreeVars fvs0 tm))
                    | otherwise = fromClashTerm env0 tm
              extendFreeVars env0 fvs1 $ \env1 -> do
                let sc = convScope env1
                forAllScrambled sc (sinkInto sc tm0) $ \tm1 -> do
                  let free = not (nullNameSet (freeTmVars tm1) && nullNameSet (freeTyVars tm1))
                  cover 20 (not free) "no free variables" $
                    cover 20 free "free variables" $
                      hasFreeVars tm1 === free

      , testProperty "hasFreeVarsType agrees with free variables" $
          forAllBlind genFreeVars $ \fvs ->
            forAllShow (runG 30 (genType (ctxOf fvs) C.liftedTypeKind 3)) showType $ \ty ->
              forAll arbitrary $ \abstract ->
                withFreeVars fvs $ \env -> do
                  let ty1
                        | abstract = fromClashTypeClosed (convScope env) (foldr C.ForAllTy ty (fvTyVars fvs))
                        | otherwise = fromClashType env ty
                      free = not (nullNameSet (freeTyVarsOfType ty1))
                  cover 20 (not free) "no free variables" $
                    cover 10 free "free variables" $
                      hasFreeVarsType ty1 === free

      , testProperty "unsinking an alternative agrees with Clash's free variables" $
          forAllShrink genSameRhsCase shrinkInScope $ \(InScope fvs tm) ->
            withFreeVars fvs $ \env -> case (tm, fromClashTerm env tm) of
              (C.Case _ _ alts, Case _ _ alts1) ->
                conjoin
                  [ counterexample (showTerm rhs) $
                      isJust (unsinkAlt p e) === not (patBindersFreeIn pat rhs)
                  | ((pat, rhs), Alt p e) <- zip alts alts1 ]
              _ -> property False
      ]
  ]

-- | At every binder in a term, and in its types, check that moving a value out
-- of its scope succeeds if and only if the variables it binds are not among the
-- free variables of the value
unsinkChecks :: Term n -> [Property]
unsinkChecks = goTm
 where
  agree :: String -> Bool -> [Int] -> [Int] -> Property
  agree what ok bound free =
    counterexample (what <> ": binds " <> show bound <> ", free " <> show free) $
      ok === not (any (`elem` free) bound)

  goTy :: Type x -> [Property]
  goTy = \case
    ForAllTy b t ->
      agree "forall" (isJust (unsinkType (tyBinderNameBinders b) t))
        [tyNameRaw (tyNameOf b)] (nameSetRaws (freeTyVarsOfType t))
      : goTy (tyBinderKind b) <> goTy t
    VarTy _ k _ -> goTy k
    AppTy t1 t2 -> goTy t1 <> goTy t2
    AnnType _ t -> goTy t
    _ -> []

  goTm :: Term x -> [Property]
  goTm = \case
    Var _ ty _ -> goTy ty
    Lam b e ->
      agree "lambda" (isJust (unsinkIdBinder b e)) [tmNameRaw (tmNameOf b)] (nameSetRaws (freeTmVars e))
      : goTy (idBinderType b) <> goTm e
    TyLam b e ->
      agree "type lambda" (isJust (unsinkTyBinder b e)) [tyNameRaw (tyNameOf b)] (nameSetRaws (freeTyVars e))
      : goTy (tyBinderKind b) <> goTm e
    App e1 e2 -> goTm e1 <> goTm e2
    TyApp e ty -> goTm e <> goTy ty
    Let bs e ->
      agree "let" (isJust (unsinkTerm (bindNameBinders bs) e)) (bindRaws bs) (nameSetRaws (freeTmVars e))
      : (case bs of
           NonRec b x -> goTy (idBinderType b) <> goTm x
           Rec _ xs -> concatMap goTm xs)
      <> goTm e
    Case e ty alts -> goTm e <> goTy ty <> concatMap goAlt alts
    Cast e t1 t2 -> goTm e <> goTy t1 <> goTy t2
    Tick (Attributes ty x) e -> goTy ty <> goTm x <> goTm e
    Tick (NameMod _ ty) e -> goTy ty <> goTm e
    Tick _ e -> goTm e
    _ -> []

  goAlt :: Alt x -> [Property]
  goAlt (Alt p e) = case p of
    DataPat _ tvs ids ->
      let tvRaws = teleRaws tyNameRaw tyNameOf tvs
          idRaws = teleRaws tmNameRaw tmNameOf ids
          freeTvs = nameSetRaws (freeTyVars e)
      in  agree "pattern" (isJust (unsinkAlt p e)) (tvRaws <> idRaws)
            (freeTvs <> nameSetRaws (freeTmVars e))
          -- The types of the variable occurrences in the alternative
          : [ agree "pattern, type" (isJust (strengthenTyPat p ty)) tvRaws
                (nameSetRaws (freeTyVarsOfType ty))
            | ty <- occTypes e ]
          <> goTm e
    _ -> goTm e

  bindRaws :: Bind x y -> [Int]
  bindRaws = \case
    NonRec b _ -> [tmNameRaw (tmNameOf b)]
    Rec bs _ -> teleRaws tmNameRaw tmNameOf bs

  teleRaws :: (forall z. k z -> Int) -> (forall x' y'. b x' y' -> k y') -> Tele b x y -> [Int]
  teleRaws _ _ TNil = []
  teleRaws raw nm (TCons b bs) = raw (nm b) : teleRaws raw nm bs

  -- The types of the variable occurrences that are not under binders
  occTypes :: Term x -> [Type x]
  occTypes = \case
    Var _ ty _ -> [ty]
    App e1 e2 -> occTypes e1 <> occTypes e2
    TyApp e _ -> occTypes e
    Cast e _ _ -> occTypes e
    Tick _ e -> occTypes e
    Case e _ _ -> occTypes e
    _ -> []
