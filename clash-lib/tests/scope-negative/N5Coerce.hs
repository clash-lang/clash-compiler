{-# LANGUAGE GADTs, RankNTypes, DataKinds, ScopedTypeVariables #-}
-- EXPECT: arising from a use of ‘coerce’
-- | Roles: move a term into another scope with 'coerce'
module N5Coerce where

import Data.Coerce (coerce)

import Clash.Core.Syntax

escape :: Term n -> Term l
escape = coerce
