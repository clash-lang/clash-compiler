module Clash.Core.VarSet
  ( VarSet
  , singleton
  , insert
  , subset
  , fromList
  , toList

  , UniqMap.empty
  , UniqMap.null
  , UniqMap.lookup
  , UniqMap.find
  , UniqMap.elem
  , UniqMap.notElem
  , UniqMap.delete
  , UniqMap.deleteMany
  , UniqMap.difference
  , UniqMap.disjoint
  ) where

import           Data.Coerce (coerce)
import           GHC.Exts (Any)

import           Clash.Core.Var (Var)
import           Clash.Data.UniqMap (UniqMap)
import qualified Clash.Data.UniqMap as UniqMap

-- | Set of variables
type VarSet = UniqMap (Var Any)

-- | The set of a single variable
singleton :: Var a -> VarSet
singleton = UniqMap.singletonUnique . coerce

-- | Add a variable to the set
insert :: Var a -> VarSet -> VarSet
insert v = UniqMap.insertUnique (coerce v)

-- | Is the set of variables A a subset of the variables B
subset :: VarSet -> VarSet -> Bool
subset = UniqMap.submap

-- | Create a set from a list of variables
fromList :: [Var a] -> VarSet
fromList = UniqMap.fromList . fmap (\x -> (x, coerce x))

toList :: VarSet -> [Var Any]
toList = UniqMap.elems
