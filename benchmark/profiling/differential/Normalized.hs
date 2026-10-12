{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Compare the results of two whole normalizations of a design: the normalized
  bindings that @clash-profile-normalization-run@ writes when
  @CLASH_NORMALIZED_OUT@ is set (@\<design\>.norm.bin@), e.g. those of the
  normalizer of Clash Core at one commit and those of the normalizer of scoped
  Clash Core at another.

  = Matching globals

  The globals of GHC2Core have the same uniques in both runs, but those that
  normalization creates (specializations, lifted functions) get uniques from
  the unique supply, in the order in which they are created, and often share
  their names. So globals are matched by where they are used: starting from
  the top entity, the globals that two matched bindings refer to are matched
  by the order in which they occur, after normalizing the known differences
  away. A binding of the second run is compared
  with the globals it refers to renamed to their matches in the first run, up
  to alpha-equivalence and the known differences.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Normalized
  ( NormOptions (..)
  , compareNormalized
  ) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as B
import Data.Binary (decode)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.IO (hFlush, stdout)

import qualified Clash.Core.Name as C
import Clash.Core.Pretty (showPpr)
import qualified Clash.Core.Subst as C (aeqType)
import qualified Clash.Core.Type as C (Type)
import qualified Clash.Core.Var as C
import Clash.Unique (Unique)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Driver.Types (Binding (..), BindingMap)

import Clash.Core.Scoped.Convert (fromClashTermClosed)
import Clash.Core.Scoped.NameMap (globalEnvFromList)
import Clash.Core.Scoped.Subst (substGlobals)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (walkTerm)


import SerialiseInstances (NormalizationInputs)
import Test.Clash.Differential

-- | Options of the @normalized@ command
data NormOptions = NormOptions
  { noShow :: Int
  -- ^ How many differences to print per design
  , noLines :: Int
  , noWidth :: Int
  }

-- | The outcome of comparing one pair of matched bindings
data Result
  = REqual
  | RKnown [String]
  | RDifferent C.Id Mismatch
  | RMissing C.Id String
  -- ^ A global without a binding in one of the runs
  | RTypeDifferent C.Id C.Type C.Type
  -- ^ The binders of a binding have different types

-- | Compare the normalized bindings of a design in two runs. Gives whether
-- they agree up to the known differences.
compareNormalized
  :: NormOptions
  -> [KnownDifference]
  -> FilePath
  -- ^ The normalization inputs of the design (@\<design\>.bin@)
  -> FilePath
  -- ^ The normalized bindings of the first run
  -> FilePath
  -- ^ The normalized bindings of the second run
  -> IO Bool
compareNormalized opts kds design fileA fileB = do
  (_, _, _, _, _, _, top, _) <- decode <$> B.readFile design :: IO NormalizationInputs
  bmA <- decode <$> B.readFile fileA :: IO BindingMap
  bmB <- decode <$> B.readFile fileB :: IO BindingMap
  results <- go bmA bmB [(C.varUniq top, top, top)] Set.empty Map.empty []
  let equal = length [() | REqual <- results]
      compared = length [() | r <- results, not (isTypeDiff r)]
      isTypeDiff = \case
        RTypeDifferent {} -> True
        _ -> False
      known = Map.fromListWith (+) [(nm, 1 :: Int) | RKnown nms <- results, nm <- nms]
      nKnown = length [() | RKnown _ <- results]
      different = [(i, m) | RDifferent i m <- results]
      missing = [(i, why) | RMissing i why <- results]
      tyDiffs = [(i, t1, t2) | RTypeDifferent i t1 t2 <- results]
  forM_ (take (noShow opts) different) $ \(i, m) ->
    putStr (renderMismatch (noLines opts) (noWidth opts) i m)
  forM_ (take (noShow opts) missing) $ \(i, why) ->
    putStrLn ("--- " <> show (C.nameOcc (C.varName i)) <> ": " <> why)
  forM_ (take (noShow opts) tyDiffs) $ \(i, t1, t2) ->
    putStrLn ("--- " <> show (C.nameOcc (C.varName i)) <> ": the types of the binders differ:\n"
              <> showPpr t1 <> "\n" <> showPpr t2)
  putStrLn $ design <> ": " <> show compared <> " bindings compared, "
    <> show equal <> " equal, " <> show nKnown <> " known-different"
    <> (if Map.null known then "" else " (" <> List.intercalate ", "
          [nm <> ": " <> show k | (nm, k) <- Map.toList known] <> ")")
    <> ", " <> show (length different) <> " different, "
    <> show (length missing) <> " missing, "
    <> show (length tyDiffs) <> " with binders of different types"
    <> "; " <> show (length bmA) <> " and " <> show (length bmB)
    <> " normalized bindings"
  hFlush stdout
  pure (null different && null missing && null tyDiffs)
 where
  -- A work list of matched globals: their unique in the first run, and their
  -- variables in both runs. Globals of the second run are renamed to their
  -- matches in the first run.
  go
    :: BindingMap -> BindingMap -> [(Unique, C.Id, C.Id)] -> Set.Set Unique
    -> Map Unique C.Id -> [Result] -> IO [Result]
  go _ _ [] _ _ acc = pure (reverse acc)
  go bmA bmB ((u, a, b) : rest) seen ren acc
    | u `Set.member` seen = go bmA bmB rest seen ren acc
    | otherwise = case (UniqMap.lookup a bmA, UniqMap.lookup b bmB) of
        (Just ba, Just bb) -> do
          let tA = fromClashTermClosed (bindingTerm ba)
              tB = fromClashTermClosed (bindingTerm bb)
              gsA = globalsOf tA
              gsB = globalsOf tB
              -- Match the globals by the order in which they occur, after
              -- normalizing the known differences away, e.g. the order of
              -- let-bindings
              ka = globalsOf (applyKnownDifferences kds emptyScope tA)
              kb = globalsOf (applyKnownDifferences kds emptyScope tB)
              pairs
                | length ka == length kb = zip ka kb
                | length gsA == length gsB = zip gsA gsB
                | otherwise = []
              ren' = List.foldl'
                       (\m (ga, gb) -> Map.insertWith (\_ old -> old) (globalIdUniq gb) (globalIdVar ga) m)
                       ren pairs
              tB' = substGlobals
                      (globalEnvFromList
                         [ (gb, GlobalVar (mkGlobalId a'))
                         | gb <- List.nub gsB
                         , Just a' <- [Map.lookup (globalIdUniq gb) ren'] ])
                      tB
          r <- attempt "comparing" (evaluate (compareResults kds emptyScope tA tB'))
          let res = case r of
                Left m -> RDifferent a m
                Right Equal -> REqual
                Right (KnownDifferent nms) -> RKnown nms
                Right (Different m) -> RDifferent a (withLetBinders m)
              -- The names of the let-binders of the bodies of both, that the
              -- other doesn't have, as 'firstDiff' doesn't look into
              -- let-expressions with different numbers of bindings
              withLetBinders m = case m of
                Mismatch d c s ->
                  let bsA = letBinders (applyKnownDifferences kds emptyScope tA)
                      bsB = letBinders (applyKnownDifferences kds emptyScope tB')
                      onlyA = bsA List.\\ bsB
                      onlyB = bsB List.\\ bsA
                      note = "let-binders only in the first run: " <> show onlyA
                          <> "; only in the second run: " <> show onlyB
                  in if null onlyA && null onlyB then m
                     else Mismatch (fmap (\x -> x {diffPath = diffPath x <> [note]}) d) c s
                _ -> m
              new = [ (globalIdUniq ga, globalIdVar ga, globalIdVar gb)
                    | (ga, gb) <- pairs
                    , not (globalIdUniq ga `Set.member` seen) ]
              -- The types of the binders, which normalization gives the
              -- binders it normalizes
              tyRes
                | C.aeqType (C.varType (bindingId ba)) (C.varType (bindingId bb)) = []
                | otherwise =
                    [RTypeDifferent a (C.varType (bindingId ba)) (C.varType (bindingId bb))]
          go bmA bmB (rest ++ new) (Set.insert u seen) ren' (tyRes ++ res : acc)
        (Nothing, Nothing) -> go bmA bmB rest (Set.insert u seen) ren acc
        (Nothing, Just _) ->
          go bmA bmB rest (Set.insert u seen) ren (RMissing a "no binding in the first run" : acc)
        (Just _, Nothing) ->
          go bmA bmB rest (Set.insert u seen) ren (RMissing a "no binding in the second run" : acc)

  -- The occurrence names and types of the let-binders under the lambdas of a
  -- term, as text
  letBinders :: Term n -> [String]
  letBinders = \case
    Lam _ e -> letBinders e
    TyLam _ e -> letBinders e
    Tick _ e -> letBinders e
    Let (NonRec b _) e -> showBinder b : letBinders e
    Let (Rec bs _) e -> teleBinders bs <> letBinders e
    _ -> []

  teleBinders :: Tele IdBinder x y -> [String]
  teleBinders = \case
    TNil -> []
    TCons b rest -> showBinder b : teleBinders rest

  showBinder :: IdBinder x y -> String
  showBinder b = show (C.nameOcc (idBinderOcc b))

  globalsOf :: Term n -> [GlobalId]
  globalsOf = walkTerm (\case GlobalVar g -> Just g; _ -> Nothing)
