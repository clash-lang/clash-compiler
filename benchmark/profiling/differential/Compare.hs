{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Compare two closed terms up to alpha-equivalence ('aeqTm'), and report
  where they first differ ('firstDiff').
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compare
  ( -- * Comparing
    Outcome (..)
  , Mismatch (..)
  , compareTerms
  , attempt
    -- * Differences
  , Diff (..)
  , firstDiff
    -- * Reporting
  , renderMismatch
  , truncateText
  ) where

import Control.Exception
  (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List

import Clash.Core.Pretty (showPpr)
import Clash.Core.Scoped.Alpha (aeqTm)
import Clash.Core.Scoped.Syntax

-- * Comparing

-- | The outcome of comparing two terms
data Outcome
  = Equal
  | Different Mismatch

-- | Why two terms are different
data Mismatch
  = Mismatch
      { mmDiff :: Maybe Diff
      -- ^ The first difference. 'Nothing' if 'firstDiff' doesn't find one,
      -- which is a bug in 'firstDiff'.
      , mmFirst :: Term 'VoidS
      , mmSecond :: Term 'VoidS
      }
  | Crash
      { crashStage :: String
      -- ^ Where it crashed
      , crashMessage :: String
      }

-- | Compare two closed terms up to alpha-equivalence
compareTerms :: Term 'VoidS -> Term 'VoidS -> Outcome
compareTerms a b
  | aeqTm a b = Equal
  | otherwise = Different (Mismatch (firstDiff a b) a b)

-- | Run an action, turning a synchronous exception into a 'Crash'
attempt :: String -> IO a -> IO (Either Mismatch a)
attempt stage act = try act >>= \case
  Right a -> pure (Right a)
  Left (e :: SomeException)
    | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
    | otherwise -> pure (Left (Crash stage (show e)))

-- * Differences

-- | Where two terms first differ: the path to the differing subterms (or
-- types), and both of them, pretty printed
data Diff = Diff
  { diffPath :: [String]
  , diffFirst :: String
  , diffSecond :: String
  }

-- | The first place where two terms are not alpha-equivalent, in the order
-- in which 'aeqTm' compares them. Agrees with 'aeqTm': it finds a difference
-- if and only if the terms are not alpha-equivalent.
firstDiff :: Term n -> Term n -> Maybe Diff
firstDiff = diffTm emptyLv []

-- | The level of the next binder, and the levels of the bound names of either
-- side, as in "Clash.Core.Scoped.Alpha"
data Lv = Lv !Int !(IntMap Int) !(IntMap Int)

emptyLv :: Lv
emptyLv = Lv 0 IntMap.empty IntMap.empty

under :: Int -> Int -> Lv -> Lv
under k1 k2 (Lv l m1 m2) = Lv (l + 1) (IntMap.insert k1 l m1) (IntMap.insert k2 l m2)

sameName :: Lv -> Int -> Int -> Bool
sameName (Lv _ m1 m2) n1 n2 = case (IntMap.lookup n1 m1, IntMap.lookup n2 m2) of
  (Just l1, Just l2) -> l1 == l2
  (Nothing, Nothing) -> n1 == n2
  _ -> False

hereTm :: [String] -> Term x -> Term y -> Maybe Diff
hereTm path t1 t2 = Just (Diff (reverse path) (showPpr t1) (showPpr t2))

hereTy :: [String] -> Type x -> Type y -> Maybe Diff
hereTy path t1 t2 = Just (Diff (reverse path) (showPpr t1) (showPpr t2))

firstJust :: [Maybe a] -> Maybe a
firstJust = foldr (\m r -> maybe r Just m) Nothing

-- | The first difference of two lists of equal length, with an index in the
-- path
diffList :: String -> [String] -> (a -> b -> [String] -> Maybe Diff) -> [a] -> [b] -> Maybe Diff
diffList step path f xs ys =
  firstJust [f x y ((step <> "[" <> show i <> "]") : path) | (i, x, y) <- zip3 [(0 :: Int)..] xs ys]

diffTy :: Lv -> [String] -> Type x -> Type y -> Maybe Diff
diffTy lv path t1 t2 = case (t1, t2) of
  (VarTy _ _ n1, VarTy _ _ n2) | sameName lv (tyNameRaw n1) (tyNameRaw n2) -> Nothing
  (ConstTy c1, ConstTy c2) | c1 == c2 -> Nothing
  (ForAllTy b1 s1, ForAllTy b2 s2) -> firstJust
    [ diffTy lv ("ForAllTy.kind" : path) (tyBinderKind b1) (tyBinderKind b2)
    , diffTy (underTy b1 b2 lv) ("ForAllTy.body" : path) s1 s2 ]
  (AppTy f1 a1, AppTy f2 a2) -> firstJust
    [ diffTy lv ("AppTy.fun" : path) f1 f2
    , diffTy lv ("AppTy.arg" : path) a1 a2 ]
  (LitTy l1, LitTy l2) | l1 == l2 -> Nothing
  (AnnType a1 s1, AnnType a2 s2) | a1 == a2 -> diffTy lv ("AnnType.type" : path) s1 s2
  _ -> hereTy path t1 t2

underTy :: TyBinder x x' -> TyBinder y y' -> Lv -> Lv
underTy b1 b2 = under (tyNameRaw (tyNameOf b1)) (tyNameRaw (tyNameOf b2))

underId :: IdBinder x x' -> IdBinder y y' -> Lv -> Lv
underId b1 b2 = under (tmNameRaw (tmNameOf b1)) (tmNameRaw (tmNameOf b2))

diffTm :: Lv -> [String] -> Term x -> Term y -> Maybe Diff
diffTm lv path t1 t2 = case (t1, t2) of
  (Var _ _ n1, Var _ _ n2) | sameName lv (tmNameRaw n1) (tmNameRaw n2) -> Nothing
  (GlobalVar g1, GlobalVar g2) | g1 == g2 -> Nothing
  (Data dc1, Data dc2) | dc1 == dc2 -> Nothing
  (Literal l1, Literal l2) | l1 == l2 -> Nothing
  (Prim p1, Prim p2) | primName p1 == primName p2 -> Nothing
  (Lam b1 e1, Lam b2 e2) -> firstJust
    [ diffTy lv ("Lam.type" : path) (idBinderType b1) (idBinderType b2)
    , diffTm (underId b1 b2 lv) ("Lam.body" : path) e1 e2 ]
  (TyLam b1 e1, TyLam b2 e2) -> firstJust
    [ diffTy lv ("TyLam.kind" : path) (tyBinderKind b1) (tyBinderKind b2)
    , diffTm (underTy b1 b2 lv) ("TyLam.body" : path) e1 e2 ]
  (App f1 a1, App f2 a2) -> firstJust
    [ diffTm lv ("App.fun" : path) f1 f2
    , diffTm lv ("App.arg" : path) a1 a2 ]
  (TyApp e1 s1, TyApp e2 s2) -> firstJust
    [ diffTm lv ("TyApp.fun" : path) e1 e2
    , diffTy lv ("TyApp.type" : path) s1 s2 ]
  (Let (NonRec b1 x1) e1, Let (NonRec b2 x2) e2) -> firstJust
    [ diffTy lv ("Let.type" : path) (idBinderType b1) (idBinderType b2)
    , diffTm lv ("Let.rhs" : path) x1 x2
    , diffTm (underId b1 b2 lv) ("Let.body" : path) e1 e2 ]
  (Let (Rec bs1 xs1) e1, Let (Rec bs2 xs2) e2)
    | teleLength bs1 == teleLength bs2 && length xs1 == length xs2 ->
        diffIdTele lv ("Letrec" : path) bs1 bs2 $ \lv1 -> firstJust
          [ diffList "Letrec.rhs" path (\x1 x2 p -> diffTm lv1 p x1 x2) xs1 xs2
          , diffTm lv1 ("Letrec.body" : path) e1 e2 ]
  (Case e1 _ as1, Case e2 _ as2) | length as1 == length as2 -> firstJust
    [ diffTm lv ("Case.subject" : path) e1 e2
    , diffList "Case.alt" path
        (\a1 a2 p -> diffAlt lv a1 a2 p (hereTm ("Alt.pattern" : p) t1 t2)) as1 as2 ]
  (Cast e1 f1 to1, Cast e2 f2 to2) -> firstJust
    [ diffTm lv ("Cast.body" : path) e1 e2
    , diffTy lv ("Cast.from" : path) f1 f2
    , diffTy lv ("Cast.to" : path) to1 to2 ]
  (Tick i1 e1, Tick i2 e2) -> firstJust
    [ diffTick lv ("Tick.info" : path) i1 i2
    , diffTm lv ("Tick.body" : path) e1 e2 ]
  _ -> hereTm path t1 t2

-- | Compare the types of two telescopes of term binders, and continue under
-- them. They have the same length.
diffIdTele
  :: Lv -> [String] -> Tele IdBinder x x' -> Tele IdBinder y y'
  -> (Lv -> Maybe Diff) -> Maybe Diff
diffIdTele lv path = go lv (0 :: Int)
 where
  go :: Lv -> Int -> Tele IdBinder a a' -> Tele IdBinder b b' -> (Lv -> Maybe Diff) -> Maybe Diff
  go l i (TCons b1 bs1) (TCons b2 bs2) k = firstJust
    [ diffTy l (("type[" <> show i <> "]") : path) (idBinderType b1) (idBinderType b2)
    , go (underId b1 b2 l) (i + 1) bs1 bs2 k ]
  go l _ _ _ k = k l

-- | Like 'diffIdTele', for type binders
diffTyTele
  :: Lv -> [String] -> Tele TyBinder x x' -> Tele TyBinder y y'
  -> (Lv -> Maybe Diff) -> Maybe Diff
diffTyTele lv path = go lv (0 :: Int)
 where
  go :: Lv -> Int -> Tele TyBinder a a' -> Tele TyBinder b b' -> (Lv -> Maybe Diff) -> Maybe Diff
  go l i (TCons b1 bs1) (TCons b2 bs2) k = firstJust
    [ diffTy l (("kind[" <> show i <> "]") : path) (tyBinderKind b1) (tyBinderKind b2)
    , go (underTy b1 b2 l) (i + 1) bs1 bs2 k ]
  go l _ _ _ k = k l

-- | Compare two alternatives. If their patterns differ, the result is the
-- last argument.
diffAlt :: Lv -> Alt x -> Alt y -> [String] -> Maybe Diff -> Maybe Diff
diffAlt lv (Alt p1 e1) (Alt p2 e2) path patternsDiffer = case (p1, p2) of
  (DataPat dc1 tvs1 ids1, DataPat dc2 tvs2 ids2)
    | dc1 == dc2
    , teleLength tvs1 == teleLength tvs2
    , teleLength ids1 == teleLength ids2 ->
        diffTyTele lv ("DataPat.tyvars" : path) tvs1 tvs2 $ \lv1 ->
          diffIdTele lv1 ("DataPat.ids" : path) ids1 ids2 $ \lv2 ->
            diffTm lv2 ("Alt.body" : path) e1 e2
  (LitPat l1, LitPat l2) | l1 == l2 -> diffTm lv ("Alt.body" : path) e1 e2
  (DefaultPat, DefaultPat) -> diffTm lv ("Alt.body" : path) e1 e2
  _ -> patternsDiffer

diffTick :: Lv -> [String] -> TickInfo x -> TickInfo y -> Maybe Diff
diffTick lv path i1 i2 = case (i1, i2) of
  (SrcSpan s1, SrcSpan s2) | s1 == s2 -> Nothing
  (NameMod m1 t1, NameMod m2 t2) | m1 == m2 -> diffTy lv ("NameMod.type" : path) t1 t2
  (DeDup, DeDup) -> Nothing
  (NoDeDup, NoDeDup) -> Nothing
  (Attributes t1 e1, Attributes t2 e2) -> firstJust
    [ diffTy lv ("Attributes.type" : path) t1 t2
    , diffTm lv ("Attributes.term" : path) e1 e2 ]
  _ -> Just (Diff (reverse path) (showPpr i1) (showPpr i2))

-- * Reporting

-- | At most @l@ lines of at most @w@ characters
truncateText :: Int -> Int -> String -> String
truncateText l w s = List.intercalate "\n" (map line (take l ls) <> more)
 where
  ls = lines s
  line x = case splitAt w x of
    (x', []) -> x'
    (x', _) -> x' <> "..."
  more = ["... (" <> show (length ls - l) <> " more lines)" | not (null (drop l ls))]

indent :: String -> String
indent = List.intercalate "\n" . map ("    " <>) . lines

-- | A mismatch of the binding with the given name, with the terms truncated
-- to @l@ lines of @w@ characters. The labels are those of the two terms.
renderMismatch :: (String, String) -> Int -> Int -> String -> Mismatch -> String
renderMismatch (labelA, labelB) l w nm m = unlines $
  ("--- " <> nm) : case m of
    Crash stage msg ->
      ["crashed in " <> stage <> ":", indent (truncateText l w msg)]
    Mismatch d a b ->
      (case d of
         Nothing -> ["no differing subterm found (a bug in firstDiff)"]
         Just (Diff path da db) ->
           [ "first difference at: " <> (if null path then "the root" else List.intercalate " / " path)
           , "  " <> labelA <> ":"
           , indent (truncateText l w da)
           , "  " <> labelB <> ":"
           , indent (truncateText l w db) ]) <>
      [ labelA <> ":"
      , indent (truncateText l w (showPpr a))
      , labelB <> ":"
      , indent (truncateText l w (showPpr b)) ]
