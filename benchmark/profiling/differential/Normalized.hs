{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Compare the results of two whole normalizations of a design: the normalized
  bindings that @clash-profile-normalization-run@ writes when
  @CLASH_NORMALIZED_OUT@ is set (@\<design\>.norm.bin@), e.g. those of the
  normalizer at two commits.

  = Matching globals

  The globals of GHC2Core have the same uniques in both runs, but those that
  normalization creates (specializations, lifted functions) get uniques from
  the unique supply, in the order in which they are created, and often share
  their names. So globals are matched by where they are used: starting from
  the top entity, the globals that two matched bindings refer to are matched
  by the order in which they occur. A binding of the second run is compared
  with the globals it refers to renamed to their matches in the first run, up
  to alpha-equivalence.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Normalized
  ( NormOptions (..)
  , compareNormalized
  ) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.IO (hFlush, stdout)

import qualified Clash.Core.Name as C
import Clash.Core.Pretty (showPpr)
import Clash.Driver.Types (Binding (..), GlobalBindings)
import Clash.Unique (Unique)

import Clash.Core.Alpha (aeqTy)
import Clash.Core.NameMap (globalEnvFromList, lookupGlobalEnv)
import Clash.Core.Subst (substGlobals)
import Clash.Core.Syntax
import Clash.Core.Term (walkTerm)

import Compare
import SerialiseInstances (NormalizationInputs, readInputs)

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
  | RDifferent GlobalId Mismatch
  | RMissing GlobalId String
  -- ^ A global without a binding in one of the runs
  | RTypeDifferent GlobalId (Type 'VoidS) (Type 'VoidS)
  -- ^ The binders of a binding have different types

-- | Compare the normalized bindings of a design in two runs. Gives whether
-- they agree.
compareNormalized
  :: NormOptions
  -> FilePath
  -- ^ The normalization inputs of the design (@\<design\>.bin@)
  -> FilePath
  -- ^ The normalized bindings of the first run
  -> FilePath
  -- ^ The normalized bindings of the second run
  -> IO Bool
compareNormalized opts design fileA fileB = do
  (_, _, _, _, _, _, top, _) <- readInputs design :: IO NormalizationInputs
  bmA <- readInputs fileA :: IO GlobalBindings
  bmB <- readInputs fileB :: IO GlobalBindings
  results <- go bmA bmB [(top, top)] Set.empty Map.empty []
  let equal = length [() | REqual <- results]
      compared = length [() | r <- results, not (isTypeDiff r)]
      isTypeDiff = \case
        RTypeDifferent {} -> True
        _ -> False
      different = [(i, m) | RDifferent i m <- results]
      missing = [(i, why) | RMissing i why <- results]
      tyDiffs = [(i, t1, t2) | RTypeDifferent i t1 t2 <- results]
  forM_ (take (noShow opts) different) $ \(i, m) ->
    putStr (renderMismatch ("first run", "second run") (noLines opts) (noWidth opts) (nameOf i) m)
  forM_ (take (noShow opts) missing) $ \(i, why) ->
    putStrLn ("--- " <> nameOf i <> ": " <> why)
  forM_ (take (noShow opts) tyDiffs) $ \(i, t1, t2) ->
    putStrLn ("--- " <> nameOf i <> ": the types of the binders differ:\n"
              <> showPpr t1 <> "\n" <> showPpr t2)
  putStrLn $ design <> ": " <> show compared <> " bindings compared, "
    <> show equal <> " equal, "
    <> show (length different) <> " different, "
    <> show (length missing) <> " missing, "
    <> show (length tyDiffs) <> " with binders of different types"
    <> "; " <> show (length bmA) <> " and " <> show (length bmB)
    <> " normalized bindings"
  hFlush stdout
  pure (null different && null missing && null tyDiffs)
 where
  nameOf = Text.unpack . C.nameOcc . globalIdName

  -- A work list of matched globals, of the first and the second run. Globals
  -- of the second run are renamed to their matches in the first run.
  go
    :: GlobalBindings -> GlobalBindings -> [(GlobalId, GlobalId)] -> Set.Set Unique
    -> Map Unique GlobalId -> [Result] -> IO [Result]
  go _ _ [] _ _ acc = pure (reverse acc)
  go bmA bmB ((a, b) : rest) seen ren acc
    | globalIdUniq a `Set.member` seen = go bmA bmB rest seen ren acc
    | otherwise = case (lookupGlobalEnv a bmA, lookupGlobalEnv b bmB) of
        (Just ba, Just bb) -> do
          let tA = bindingTerm ba
              tB = bindingTerm bb
              gsA = globalsOf tA
              gsB = globalsOf tB
              -- Match the globals by the order in which they occur
              pairs
                | length gsA == length gsB = zip gsA gsB
                | otherwise = []
              ren' = List.foldl'
                       (\m (ga, gb) -> Map.insertWith (\_ old -> old) (globalIdUniq gb) ga m)
                       ren pairs
              tB' = substGlobals
                      (globalEnvFromList
                         [ (gb, GlobalVar a')
                         | gb <- List.nub gsB
                         , Just a' <- [Map.lookup (globalIdUniq gb) ren'] ])
                      tB
          r <- attempt "comparing" (evaluate (compareTerms tA tB'))
          let res = case r of
                Left m -> RDifferent a m
                Right Equal -> REqual
                Right (Different m) -> RDifferent a (withLetBinders m)
              -- The names of the let-binders of the bodies of both, that the
              -- other doesn't have, as 'firstDiff' doesn't look into
              -- let-expressions with different numbers of bindings
              withLetBinders m = case m of
                Mismatch d c s ->
                  let bsA = letBinders tA
                      bsB = letBinders tB'
                      onlyA = bsA List.\\ bsB
                      onlyB = bsB List.\\ bsA
                      note = "let-binders only in the first run: " <> show onlyA
                          <> "; only in the second run: " <> show onlyB
                  in if null onlyA && null onlyB then m
                     else Mismatch (fmap (\x -> x {diffPath = diffPath x <> [note]}) d) c s
                _ -> m
              new = [ (ga, gb)
                    | (ga, gb) <- pairs
                    , not (globalIdUniq ga `Set.member` seen) ]
              -- The types of the binders, which normalization gives the
              -- binders it normalizes
              tyA = globalIdType (bindingId ba)
              tyB = globalIdType (bindingId bb)
              tyRes
                | aeqTy tyA tyB = []
                | otherwise = [RTypeDifferent a tyA tyB]
          go bmA bmB (rest ++ new) (Set.insert (globalIdUniq a) seen) ren' (tyRes ++ res : acc)
        (Nothing, Nothing) -> go bmA bmB rest (Set.insert (globalIdUniq a) seen) ren acc
        (Nothing, Just _) ->
          go bmA bmB rest (Set.insert (globalIdUniq a) seen) ren
            (RMissing a "no binding in the first run" : acc)
        (Just _, Nothing) ->
          go bmA bmB rest (Set.insert (globalIdUniq a) seen) ren
            (RMissing a "no binding in the second run" : acc)

  -- The occurrence names of the let-binders under the lambdas of a term, as
  -- text
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
