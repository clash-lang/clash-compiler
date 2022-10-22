{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Core.InScopeSet where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import GHC.Exts (Any)
import GHC.Generics (Generic)

import Prelude hiding (elem, notElem)

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter ((<+>))
#else
import Data.Text.Prettyprint.Doc ((<+>))
#endif

import Clash.Core.Pretty () -- instance ClashPretty (Var a)
import Clash.Core.Var (Var)
import Clash.Core.VarSet (VarSet)
import qualified Clash.Core.VarSet as VarSet
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Debug (debugIsOn)
import Clash.Pretty (ClashPretty(..), fromPretty)
import Clash.Unique (Unique, Uniquable(..))
import Clash.Util (pprPanic, pprTraceDebug)

-- | Set of variables that is in scope at some point
--
-- The 'Int' is a kind of hash-value used to generate new uniques. It should
-- never be zero
--
-- See "Secrets of the Glasgow Haskell Compiler inliner" Section 3.2 for the
-- motivation
data InScopeSet = InScopeSet VarSet {-# UNPACK #-} !Int
  deriving stock Generic
  deriving anyclass (NFData, Binary)

instance Semigroup InScopeSet where
  InScopeSet s1 _ <> InScopeSet s2 n2 =
    InScopeSet (s1 <> s2) n2

instance Monoid InScopeSet where
  mempty = fromVarSet mempty

instance ClashPretty InScopeSet where
  clashPretty (InScopeSet s _) = clashPretty s

singleton :: Var a -> InScopeSet
singleton = fromVarSet . VarSet.singleton

-- | The empty set
insert :: Var a -> InScopeSet -> InScopeSet
insert v (InScopeSet inScope n) =
  InScopeSet (VarSet.insert v inScope) (n + 1)

-- | Add a list of variables in scope
insertMany :: [Var a] -> InScopeSet -> InScopeSet
insertMany vs (InScopeSet inScope n) =
  InScopeSet (foldr VarSet.insert inScope vs) (n + length vs)

-- | Is the set of variables in scope
varSetInScope :: VarSet -> InScopeSet -> Bool
varSetInScope vars (InScopeSet s1 _)
  = vars `VarSet.subset` s1

-- | Look up a variable in the 'InScopeSet'. This gives you the canonical
-- version of the variable
lookup :: Var a -> InScopeSet -> Maybe (Var Any)
lookup v (InScopeSet s _) = VarSet.lookup v s

-- | Is the variable in scope
{-# SPECIALIZE elem :: Var a -> InScopeSet -> Bool #-}
elem :: Uniquable a => a -> InScopeSet -> Bool
elem v (InScopeSet s _) = VarSet.elem v s

-- | Is the variable not in scope
{-# SPECIALIZE notElem :: Var a -> InScopeSet -> Bool #-}
notElem :: Uniquable a => a -> InScopeSet -> Bool
notElem v (InScopeSet s _) = VarSet.notElem v s

-- | Create a set of variables in scope
fromVarSet :: VarSet -> InScopeSet
fromVarSet is = InScopeSet is 1

-- | Ensure that the 'Unique' of a variable does not occur in the 'InScopeSet'
uniqAway
  :: (Uniquable a, ClashPretty a)
  => InScopeSet
  -> a
  -> a
uniqAway (InScopeSet set n) a =
  uniqAway' (`UniqMap.elem` set) n a

uniqAway'
  :: (Uniquable a, ClashPretty a)
  => (Unique -> Bool)
  -- ^ Unique in scope test
  -> Int
  -- ^ Seed
  -> a
  -> a
uniqAway' inScopeTest n u =
  if inScopeTest (getUnique u) then
    try 1
  else
    u
 where
  origUniq = getUnique u
  try k
    | debugIsOn && k > 1000
    = pprPanic "uniqAway loop:" msg
    | inScopeTest uniq
    = try (k + 1)
    | k > 3
    = pprTraceDebug "uniqAway:" msg (setUnique u uniq)
    | otherwise
    = setUnique u uniq
    where
      msg  = fromPretty k <+> "tries" <+> clashPretty u <+> fromPretty n
      uniq = deriveUnique origUniq (n * k)

deriveUnique
  :: Unique
  -> Int
  -> Unique
deriveUnique i delta = i + delta
