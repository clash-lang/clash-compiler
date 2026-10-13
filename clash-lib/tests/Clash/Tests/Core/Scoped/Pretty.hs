{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the pretty-printer of scoped Clash Core: it prints the same text
  as printing the conversion to Clash Core, and annotates the same places.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.Pretty (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter (SimpleDocStream, alterAnnotationsS, defaultLayoutOptions, layoutPretty)
#else
import Data.Text.Prettyprint.Doc (SimpleDocStream, alterAnnotationsS, defaultLayoutOptions, layoutPretty)
#endif
import Data.Text (Text)

import qualified Clash.Core.Name as C
import qualified Clash.Core.Var as C
import Clash.Core.Pretty
  (ClashAnnotation (..), ClashDoc, PrettyOptions (..), SubtermPos (..), ppr, showPpr,
   showPpr')
import qualified Clash.Core.Term as C
import qualified Clash.Core.TyCon as C
import qualified Clash.Data.UniqMap as UniqMap
import qualified Data.List as List
import qualified Clash.Core.Type as C
import qualified Clash.Core.TysPrim as C

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.Pretty ()
import Clash.Core.Scoped.Syntax

import Test.Clash.Rewrite (globalId, intTy, tyVar)
import Test.Clash.Scoped (extendFreeVars, forAllScrambled, sinkInto, withFreeVars, withTests)
import Test.Clash.Scoped.Gen
  (InScope (..), Sunk (..), ctxOf, genFreeVars, genInScope, genSunk, genType, runG,
   shrinkInScope, shrinkSunk, showType, tyConMap)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Pretty"
  [ testCase "a global variable prints like its variable of Clash Core" $ do
      let a = tyVar C.User "a" 4302
          ty = C.ForAllTy a (C.mkFunTy (C.VarTy a) intTy)
          g = mkGlobalId (C.varName (globalId C.User "f" 4301 ty)) (fromClashTypeClosed emptyScope ty)
      showPpr g @?= showPpr (globalIdLegacy g)

  , testCase "a data type prints its kind and data constructors" $
      showPpr' noUniques maybeTyCon @?= List.intercalate "\n"
        [ "data Maybe :: Type -> Type"
        , "Just :: forall (a :: Type). a -> Maybe a"
        , "Nothing :: forall (a :: Type). Maybe a" ]

    -- The uniques of the pattern variables of the equations are their raw
    -- names
  , testCase "a type family prints its kind and equations" $ do
      let a = tyVar C.User "a" 7001
          maybeNm = C.tyConName maybeTyCon
          gTc = mkFunTyConFromClash 3001 (C.mkUnsafeName C.User "G" 3001)
                  (C.mkFunTy C.liftedTypeKind C.liftedTypeKind) 1
                  [ ([C.mkTyConApp maybeNm [C.VarTy a]], C.VarTy a)
                  , ([C.VarTy a], C.mkTyConApp maybeNm [C.VarTy a]) ]
      -- The uniques of the wired-in kinds depend on the version of GHC
      take 1 (lines (showPpr' noUniques gTc)) @?= ["type family G :: Type -> Type"]
      drop 1 (lines (showPpr' noQualifiers gTc)) @?=
        [ "type instance G[3001] (Maybe[2001] a[1]) = a[1]"
        , "type instance G[3001] a[1] = Maybe[2001] a[1]" ]

  , withTests 500 $ testGroup "properties"
      [ testProperty "terms print like their conversion to Clash Core" $
          forAllShrink genInScope shrinkInScope $ \(InScope fvs tm) ->
            withFreeVars fvs $ \env ->
              samePrinting (fromClashTerm env tm)

        -- Binders that shadow variables in scope, and each other
      , testProperty "terms with scrambled binders print like their conversion" $
          forAllShrink genSunk shrinkSunk $ \(Sunk fvs0 fvs1 tm) ->
            withFreeVars fvs0 $ \env0 -> do
              let tm0 = fromClashTerm env0 tm
              extendFreeVars env0 fvs1 $ \env1 -> do
                let sc = convScope env1
                forAllScrambled sc (sinkInto sc tm0) samePrinting

      , testProperty "types print like their conversion to Clash Core" $
          forAllBlind genFreeVars $ \fvs ->
            forAllShow (runG 30 (genType (ctxOf fvs) C.liftedTypeKind 4)) showType $ \ty ->
              withFreeVars fvs $ \env -> do
                let ty1 = fromClashType env ty
                    ty2 = toClashType ty1
                conjoin
                  [ counterexample (showOptions opts) (showPpr' opts ty1 === showPpr' opts ty2)
                  | opts <- allOptions ]
      ]
  ]

-- | The text, under every combination of options, and the annotations of a
-- term are those of its conversion to Clash Core
samePrinting :: Term n -> Property
samePrinting tm =
  let tm1 = toClashTerm tm
  in conjoin
       [ counterexample (showOptions opts) (showPpr' opts tm === showPpr' opts tm1)
       | opts <- allOptions ]
     .&&. counterexample "annotations" (annotations (ppr tm) === annotations (ppr tm1))

noUniques, noQualifiers :: PrettyOptions
noUniques = PrettyOptions False True False True
noQualifiers = PrettyOptions True True False True

-- | @Maybe@, see "Test.Clash.Scoped.Gen"
maybeTyCon :: C.TyCon
maybeTyCon =
  case [tc | tc <- UniqMap.elems tyConMap, C.nameOcc (C.tyConName tc) == "Maybe"] of
    tc : _ -> tc
    [] -> error "maybeTyCon: not in Test.Clash.Scoped.Gen.tyConMap"

allOptions :: [PrettyOptions]
allOptions =
  [ PrettyOptions u t q k
  | u <- [True, False], t <- [True, False], q <- [True, False], k <- [True, False] ]

showOptions :: PrettyOptions -> String
showOptions (PrettyOptions u t q k) =
  "PrettyOptions " <> unwords (map show [u, t, q, k])

-- | The laid out document, with the annotations of a subterm or context as the
-- kind of subterm. The bodies of ticks have no context in Clash Core.
annotations :: ClashDoc -> SimpleDocStream Ann
annotations = alterAnnotationsS ann . layoutPretty defaultLayoutOptions
 where
  ann = \case
    AnnSyntax s -> Just (Syntax (show s))
    AnnContext c -> Just $ case c of
      C.AppFun -> Subterm "AppFun" Nothing
      C.AppArg a -> Subterm "AppArg" a
      C.TyAppC -> Subterm "TyAppFun" Nothing
      C.LetBinding {} -> Subterm "LetRhs" Nothing
      C.LetBody {} -> Subterm "LetBody" Nothing
      C.LamBody {} -> Subterm "LamBody" Nothing
      C.TyLamBody {} -> Subterm "TyLamBody" Nothing
      C.CaseAlt {} -> Subterm "CaseAlt" Nothing
      C.CaseScrut -> Subterm "CaseScrut" Nothing
      C.CastBody -> Subterm "CastBody" Nothing
      C.TickC {} -> Subterm "TickBody" Nothing
    AnnSubterm p -> case p of
      PosAppFun -> Just (Subterm "AppFun" Nothing)
      PosAppArg a -> Just (Subterm "AppArg" a)
      PosTyAppFun -> Just (Subterm "TyAppFun" Nothing)
      PosLetRhs _ -> Just (Subterm "LetRhs" Nothing)
      PosLetBody -> Just (Subterm "LetBody" Nothing)
      PosLamBody -> Just (Subterm "LamBody" Nothing)
      PosTyLamBody -> Just (Subterm "TyLamBody" Nothing)
      PosCaseScrut -> Just (Subterm "CaseScrut" Nothing)
      PosCaseAlt _ -> Just (Subterm "CaseAlt" Nothing)
      PosCastBody -> Just (Subterm "CastBody" Nothing)
      PosTickBody -> Nothing

data Ann
  = Syntax String
  | Subterm String (Maybe (Text, Int, Int))
  deriving (Eq, Show)
