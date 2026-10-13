{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for the pretty-printer of scoped Clash Core: what it prints for
  global variables and type constructors, and that the names it prints for
  local variables only depend on their human readable names and raw names.
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
import Clash.Core.Pretty
  (ClashAnnotation (..), ClashDoc, PrettyOptions (..), SubtermPos (..), ppr', showPpr,
   showPpr')
import qualified Clash.Core.TyCon as C
import qualified Clash.Data.UniqMap as UniqMap
import qualified Data.List as List

import Clash.Core.Scoped.Builder (runTyFamEqn, tyConAppB, varTyB)
import Clash.Core.Scoped.Convert (mkFunTyCon, mkGlobalId)
import Clash.Core.Scoped.Pretty ()
import Clash.Core.Scoped.Syntax

import Test.Clash.Rewrite (liftedTypeKindB)
import Test.Clash.Scoped (closedType, forAllScrambled, testScope, withTests)
import Test.Clash.Scoped.Gen
  (InScope (..), Sunk (..), genInScope, genSunk, renameOccs, shrinkInScope, shrinkSunk,
   tyConMap)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Pretty"
  [ testCase "a global variable prints its name and type" $ do
      let g = mkGlobalId (C.mkUnsafeName C.User "f" 4301) (closedType "forall a. a -> Int")
      showPpr g @?= "f :: forall (a :: Type). a -> Int"

  , testCase "a data type prints its kind and data constructors" $
      showPpr' noUniques maybeTyCon @?= List.intercalate "\n"
        [ "data Maybe :: Type -> Type"
        , "Just :: forall (a :: Type). a -> Maybe a"
        , "Nothing :: forall (a :: Type). Maybe a" ]

    -- The uniques of the pattern variables of the equations are their raw
    -- names
  , testCase "a type family prints its kind and equations" $ do
      let maybeNm = C.tyConName maybeTyCon
          a = C.mkUnsafeName C.User "a" 7001
          eqn lhs rhs = runTyFamEqn [(7001, a, liftedTypeKindB)] [lhs] rhs
          gTc = mkFunTyCon (C.mkUnsafeName C.User "G" 3001) (closedType "Type -> Type") 1
                  [ eqn (tyConAppB maybeNm [varTyB 7001]) (varTyB 7001)
                  , eqn (varTyB 7001) (tyConAppB maybeNm [varTyB 7001]) ]
      -- The uniques of the wired-in kinds depend on the version of GHC
      take 1 (lines (showPpr' noUniques gTc)) @?= ["type family G :: Type -> Type"]
      drop 1 (lines (showPpr' noQualifiers gTc)) @?=
        [ "type instance G[3001] (Maybe[3011] a[1]) = a[1]"
        , "type instance G[3001] a[1] = Maybe[3011] a[1]" ]

  , withTests 500 $ testGroup "properties"
      [ -- Binders that shadow variables in scope, and each other, print with
        -- the same names
        testProperty "with uniques hidden, terms print regardless of their raw names" $
          forAllShrink genSunk shrinkSunk $ \(Sunk _ ts1 tm) ->
            let sc = testScope ts1
                tm0 = sink tm
            in  forAllScrambled sc tm0 $ \tm1 ->
                  conjoin
                    [ counterexample (showOptions opts) (showPpr' opts tm0 === showPpr' opts tm1)
                    | opts <- allOptions, not (displayUniques opts) ]
                  .&&. counterexample "annotations"
                         (annotations (ppr' hidden tm0) === annotations (ppr' hidden tm1))

      , testProperty "terms print regardless of the uniques of human readable names" $
          forAllShrink genInScope shrinkInScope $ \(InScope _ tm) ->
            conjoin
              [ counterexample (showOptions opts) (showPpr' opts tm === showPpr' opts (renameOccs tm))
              | opts <- allOptions ]
      ]
  ]

-- | All options, except that uniques are hidden
hidden :: PrettyOptions
hidden = PrettyOptions False True True True

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

-- | The laid out document, with the annotations of a subterm as the kind of
-- subterm
annotations :: ClashDoc -> SimpleDocStream Ann
annotations = alterAnnotationsS ann . layoutPretty defaultLayoutOptions
 where
  ann = \case
    AnnSyntax s -> Just (Syntax (show s))
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
      PosTickBody -> Just (Subterm "TickBody" Nothing)
    _ -> Nothing

data Ann
  = Syntax String
  | Subterm String (Maybe (Text, Int, Int))
  deriving (Eq, Show)
