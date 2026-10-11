{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Compares the ported reductions of primitives of the scoped evaluator
  ("Clash.Core.Scoped.Evaluator.Primitives") with the reductions of Clash
  Core (@Clash.GHC.Evaluator@), through the adapter, on calls generated from
  the types of the primitives ("Test.Clash.Scoped.Evaluator.genPrimCall").

  The types of the primitives come from the designs: every primitive that
  occurs in the bindings of a design, with the type constructors of that
  design. A ported primitive that occurs in none of the designs, or whose
  arguments can't be generated, is reported, but not compared: test it with
  unit tests, with @clash-differential whnf --check-prims@, or with opaque
  arguments ('poOpen').

  Every call is evaluated both as the subject of a case-expression and not,
  because many reductions only reduce as a subject.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prims
  ( PrimsOptions (..)
  , defPrimsOptions
  , comparePrims
  ) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, forM_, replicateM)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO (hFlush, stdout)
import System.Timeout (timeout)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Text.Printf (printf)

import Clash.Core.Pretty (showPpr)
import qualified Clash.Core.Term as C
import qualified Clash.Core.TyCon as C
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Driver.Types (Binding (..), BindingMap)

import Clash.GHC.Evaluator (evaluator)

import Clash.Core.Scoped.Evaluator.Primitives (scopedPrimitives)
import Clash.Core.Scoped.Evaluator.Types (PrimReduction)
import Clash.Core.Scoped.TypeEnv (TypeEnv, mkTypeEnv)

import Test.Clash.Scoped.Evaluator
  ( PrimOutcome (..), adapterEnv, comparePrimCall, genOpenPrimCall, genPrimCall
  , onlyPortedEnv, outcomeKey, scopedWhnf )

-- | The options of the prims command
data PrimsOptions = PrimsOptions
  { poCount :: Int
  -- ^ Number of generated calls per primitive
  , poSeed :: Int
  , poShow :: Int
  -- ^ Number of differences to show per primitive
  , poPrim :: Maybe String
  -- ^ Only compare the primitives whose name contains this
  , poOpen :: Bool
  -- ^ Generate opaque values for the arguments it can't generate otherwise,
  -- see 'genOpenPrimCall'
  }

defPrimsOptions :: PrimsOptions
defPrimsOptions = PrimsOptions 100 42 3 Nothing False

-- | The result of comparing a primitive
data Result
  = NotFound
  -- ^ It doesn't occur in the designs
  | CantGenerate String
  -- ^ The arguments of its type can't be generated
  | Compared [(Bool, C.Term, Either String PrimOutcome)]
  -- ^ The calls, whether they were evaluated as a subject, and the outcomes

-- | Compare the ported reductions with the adapter, see the module header.
-- Gives whether they all agree.
comparePrims :: PrimsOptions -> [(FilePath, BindingMap, C.TyConMap)] -> IO Bool
comparePrims opts designs = do
  let found = HashMap.fromListWith (\_ old -> old)
        [ (C.primName p, (p, tcm))
        | (_, bm, tcm) <- reverse designs, b <- UniqMap.elems bm, p <- prims (bindingTerm b) ]
      ported = [ (nm, r) | (nm, r) <- scopedPrimitives
                         , maybe True (`List.isInfixOf` Text.unpack nm) (poPrim opts) ]
  printf "%d primitives in %d designs, %d ported reductions\n"
    (HashMap.size found) (length designs) (length ported)
  results <- forM ported $ \(nm, r) -> case HashMap.lookup nm found of
    Nothing -> pure (nm, NotFound)
    Just (p, tcm) ->
      let genCall
            | poOpen opts = Just <$> genOpenPrimCall tcm p
            | otherwise = genPrimCall tcm p
          gen = replicateM (poCount opts) genCall
      in case sequence (unGen gen (mkQCGen (poSeed opts)) 30) of
           Nothing -> pure (nm, CantGenerate (showPpr (C.primType p)))
           Just calls -> do
             let te = mkTypeEnv tcm
             outcomes <- forM [ (s, c) | c <- calls, s <- [False, True] ] $ \(isSubj, c) -> do
               o <- try (timeout 10000000 (evaluate (forceOutcome
                      (comparePrimCall te evaluator mempty nm r isSubj c))))
               (,,) isSubj c <$> case o of
                 Left (ex :: SomeException) -> Left <$> failure te nm r isSubj c ex
                 Right Nothing -> pure (Left "timeout")
                 Right (Just o') -> pure (Right o')
             pure (nm, Compared outcomes)
  report opts results
  pure (and [ all (agrees . third) os | (_, Compared os) <- results ])
 where
  third (_, _, x) = x
  agrees = \case
    Right Agree -> True
    Right BothStuck -> True
    Left e -> bothFailPrefix `List.isPrefixOf` e
    _ -> False

  forceOutcome o = case o of
    Differ a b -> length a `seq` length b `seq` o
    OnlyPorted a -> length a `seq` o
    OnlyReference a -> length a `seq` o
    _ -> o

-- | Why the comparison of a call failed with an exception. If the ported
-- reduction and the reference both fail on their own, with the same
-- exception, they agree: e.g. the reduction of Clash Core of
-- @Clash.Sized.Internal.Signed.minBound#@ fails on @Signed 0@ too.
failure
  :: TypeEnv -> Text -> PrimReduction -> Bool -> C.Term -> SomeException -> IO String
failure te nm r isSubj c ex = do
  ported <- run (onlyPortedEnv te evaluator nm r)
  reference <- run (adapterEnv te evaluator)
  pure $ case (ported, reference) of
    (Just p, Just q) | p == q -> bothFailPrefix <> p
    _ -> firstLine ex
 where
  -- The first line of the exception evaluating the call on one side throws
  run env = do
    res <- try (timeout 10000000 (evaluate (length (showPpr (scopedWhnf env mempty isSubj c)))))
    pure $ case res of
      Left (e :: SomeException) -> Just (firstLine e)
      Right _ -> Nothing
  firstLine :: SomeException -> String
  firstLine = takeWhile (/= '\n') . show

-- | The prefix of the failure of a call on which the ported reduction and the
-- reference fail alike
bothFailPrefix :: String
bothFailPrefix = "both fail: "

-- | The primitives in a term
prims :: C.Term -> [C.PrimInfo]
prims = go []
 where
  go acc = \case
    C.Prim p -> p : acc
    C.Lam _ e -> go acc e
    C.TyLam _ e -> go acc e
    C.App e1 e2 -> go (go acc e1) e2
    C.TyApp e _ -> go acc e
    C.Let (C.NonRec _ x) e -> go (go acc x) e
    C.Let (C.Rec xs) e -> List.foldl' go (go acc e) (map snd xs)
    C.Case e _ alts -> List.foldl' go (go acc e) (map snd alts)
    C.Cast e _ _ -> go acc e
    C.Tick _ e -> go acc e
    _ -> acc

report :: PrimsOptions -> [(Text, Result)] -> IO ()
report opts results = do
  forM_ results $ \(nm, res) -> case res of
    NotFound -> printf "  %-60s not in the designs\n" (Text.unpack nm)
    CantGenerate ty ->
      printf "  %-60s can't generate arguments of: %s\n" (Text.unpack nm)
        (take 200 (unwords (words ty)))
    Compared os -> do
      let counts = Map.fromListWith (+) [ (key o, 1 :: Int) | (_, _, o) <- os ]
      printf "  %-60s %s\n" (Text.unpack nm)
        (List.intercalate ", " [ k <> ": " <> show n | (k, n) <- Map.toList counts ])
      forM_ (take (poShow opts) [ (s, c, o) | (s, c, o) <- os, not (agree o) ]) $ \(s, c, o) ->
        printf "    -- %s%s\n      call:      %s\n%s" (key o)
          (if s then " (as a subject)" else "") (oneLine (showPpr c)) (details o)
  hFlush stdout
 where
  key = \case
    Left e
      | bothFailPrefix `List.isPrefixOf` e -> e
      | otherwise -> "failed: " <> e
    Right o -> outcomeKey o
  agree = \case
    Right Agree -> True
    Right BothStuck -> True
    Left e -> bothFailPrefix `List.isPrefixOf` e
    _ -> False
  oneLine = take 1500 . unwords . words
  details = \case
    Right (Differ a b) -> "      ported:    " <> oneLine a <> "\n      reference: " <> oneLine b <> "\n"
    Right (OnlyPorted a) -> "      ported:    " <> oneLine a <> "\n"
    Right (OnlyReference b) -> "      reference: " <> oneLine b <> "\n"
    Left e -> "      " <> e <> "\n"
    _ -> ""
