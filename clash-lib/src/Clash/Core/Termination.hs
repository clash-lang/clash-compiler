{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Clash.Core.Termination
  ( RecInfo
  , mkRecInfo
  , isRecursive
  , recursiveGroup
  ) where

import Control.DeepSeq (NFData)
import Control.Lens.Fold
import Data.Graph (SCC(..))
import qualified Data.Graph as Graph
import qualified Data.List as List
import GHC.Generics (Generic)

import Clash.Core.FreeVars
import Clash.Core.Var
import Clash.Core.VarEnv
import Clash.Driver.Types

-- Quick lookup for whether a binding is recursive or non-recursive. If a
-- binding is non-recursive, we can assume that it terminates and skip
-- analysing it.
--
data RecInfo = RecInfo
  { recBindings    :: [VarSet]
    -- ^ Recursive bindings, organized into groups of strongly connected
    -- components.
  , nonRecBindings :: VarSet
    -- ^ Non-recursive bindings
  } deriving (Generic, NFData)

instance Show RecInfo where
  show (RecInfo rs ns) =
    "recursive groups:\n" <> show (fmap eltsVarSet rs)
      <> "\n\nnon-recursive:\n" <> show (eltsVarSet ns)

instance Semigroup RecInfo where
  {-# INLINE (<>) #-}
  RecInfo rX nX <> RecInfo rY nY =
    RecInfo (rX <> rY) (nX <> nY)

instance Monoid RecInfo where
  {-# INLINE mempty #-}
  mempty = RecInfo mempty mempty

  {-# INLINE mappend #-}
  mappend = (<>)

-- | Given a map of top-level bindings, identify which terms are recursive and
-- organize them into groups of mutually recursive bindings. For example,
-- calling mkRecInfo on a BindingMap with the definitions
--
--   f []     = []
--   f (x:xs) = g x : h xs
--
--   g x = x + 1
--
--   h []     = []
--   h (x:xs) = x : f xs
--
--   i []     = []
--   i (x:xs) = x * 2 : i xs
--
-- would identify [f, g] and [i] as recursive groups, and g as non-recursive.
--
mkRecInfo :: BindingMap -> RecInfo
mkRecInfo =
  mconcat . fmap asInfo . dependencies
 where
  -- Convert a SCC to RecInfo
  asInfo = \case
    AcyclicSCC x -> RecInfo [] (unitVarSet $ bindingId x)
    CyclicSCC xs -> RecInfo [mkVarSet $ fmap bindingId xs] emptyVarSet

  -- Get the SCCs of the dependency graph of free variables.
  dependencies =
    Graph.stronglyConnComp . eltsVarEnv . fmap go
   where
    go x = let fvs = bindingTerm x ^.. freeIds
            in (x, bindingId x, fvs)

-- | Check if a global binder is recursive. To be conservative, binders which
-- are not included in the RecInfo are assumed to be recursive.
--
isRecursive :: Id -> RecInfo -> Bool
isRecursive i
  | isGlobalId i = not . elemVarSet i . nonRecBindings
  | otherwise = error ("isRecursive: " <> show i <> " is not a global Id")

-- | Return the recursive group that a global binder belongs to. If the
-- binder is non-recursive or not included in the RecInfo, Nothing is returned.
--
recursiveGroup :: Id -> RecInfo -> Maybe VarSet
recursiveGroup i = List.find (elemVarSet i) . recBindings

