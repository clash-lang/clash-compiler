{-# LANGUAGE GADTs, RankNTypes, DataKinds, ScopedTypeVariables #-}
-- EXPECT: rigid type variable
-- | Compare the right-hand sides of case alternatives without moving them out
-- of the scopes of their patterns (the bug in `caseOneAlt`)
module N3CompareAlts where

import Clash.Core.Alpha
import Clash.Core.Syntax

commonBody :: [Alt n] -> Maybe (Term n)
commonBody (Alt _ e1 : alts) | all (\(Alt _ e) -> aeqTm e1 e) alts = Just e1
commonBody _ = Nothing
