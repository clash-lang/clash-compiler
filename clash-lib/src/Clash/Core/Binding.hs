{-# LANGUAGE DeriveAnyClass #-}

module Clash.Core.Binding where

import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import qualified Data.List as List
import GHC.Generics (Generic)

import SrcLoc (SrcSpan)

import Clash.Core.Name
import Clash.Core.Var
import Clash.Core.VarEnv
import Clash.Unique

data Inlining
  = Default
  | Inline
  | NoInline
  deriving (Eq, Show, Generic, Binary, NFData)

data Binding a = Binding
  { bindingId :: Id
    -- ^ The identifier of the binding.
  , bindingLoc :: SrcSpan
    -- ^ The span of source code that this binding refers to.
  , bindingInline :: Inlining
    -- ^ Whether the binding can be inlined or not. 
  , bindingBody :: a
    -- ^ The body of the binding. This is typically a term.
  , bindingRecs :: VarSet
    -- ^ Set of identifiers that recursively call this binding. For
    -- non-recursive bindings this is empty, and for self-recursive bindings
    -- this contains only the binder's own identifier.
  } deriving (Functor, Generic, Binary, NFData)

instance Uniquable (Binding a) where
  getUnique = getUnique . bindingId

  setUnique b i =
    b { bindingId = setUnique (bindingId b) i }

isNonRecursive :: Binding a -> Bool
isNonRecursive = nullVarSet . bindingRecs

isRecursive :: Binding a -> Bool
isRecursive = not . isNonRecursive

-- | A binding map contains multiple bindings, indexed by variable.
--
newtype BindingMap a
  = BindingMap { getBindings :: VarEnv (Binding a) }
  deriving (Functor, Generic, Binary, NFData)

instance Semigroup (BindingMap a) where
  BindingMap xs <> BindingMap ys =
    BindingMap (unionVarEnv xs ys)

instance Monoid (BindingMap a) where
  mempty = BindingMap emptyVarEnv
  mappend = (<>)

instance Show (BindingMap a) where
  show = List.intercalate "\n" . fmap go . eltsBindingMap
   where
    go :: Binding a -> String
    go b = mconcat
      [ show (nameOcc (varName (bindingId b)))
      , "["
      , show (getUnique (bindingId b))
      , "]: "
      , show (eltsUniqSet (bindingRecs b))
      ]

elemBindingMap :: (Uniquable i) => i -> BindingMap a -> Bool
elemBindingMap (getUnique -> i) = elemUniqMapDirectly i . getBindings

lookupBinding :: (Uniquable i) => i -> BindingMap a -> Maybe (Binding a)
lookupBinding (getUnique -> i) = lookupVarEnvDirectly i . getBindings

-- | Insert a binding into the binding map. The id of the inserted binding is
-- added to the bindingRecs of any id that appears in its bindingRecs.
--
insertBinding :: Binding a -> BindingMap a -> BindingMap a
insertBinding b =
  BindingMap . extendVarEnv (bindingId b) b . fmap updateRecs . getBindings
 where
  updateRecs x
    | bindingId x `elemVarSet` bindingRecs b =
        x { bindingRecs = extendVarSet (bindingRecs x) (bindingId b) }
    | otherwise = x

-- | Delete a binding from the binding map. The id of the deleted binding is
-- also deleted from the bindingRecs of all bindings.
--
deleteBinding :: (Uniquable i) => i -> BindingMap a -> BindingMap a
deleteBinding (getUnique -> i) =
  BindingMap . mapMaybeVarEnv go . getBindings
 where
  go x
    | getUnique (bindingId x) == i = Nothing
    | otherwise = Just (x { bindingRecs = delVarSetByKey i (bindingRecs x) })

-- | Update a binding in the binding map, ensuring that the recursive group of
-- the binding is also updated (propogating any changes to bindingRecs).
--
-- TODO It may be desirable to replace this with a more efficient version that
-- only does one traversal of the BindingMap.
--
updateBinding :: Binding a -> BindingMap a -> BindingMap a
updateBinding b = insertBinding b . deleteBinding b

-- | Given an identifier, return the bindings which form a recursive group
-- containing that identifier.
--
recursiveGroup :: Id -> BindingMap a -> BindingMap a
recursiveGroup i =
  BindingMap . filterUniqMap (elemVarSet i . bindingRecs) . getBindings

filterBindings :: (a -> Bool) -> BindingMap a -> BindingMap a
filterBindings f =
  BindingMap . filterUniqMap (f . bindingBody) . getBindings

keysBindingMap :: BindingMap a -> [Unique]
keysBindingMap = keysUniqMap . getBindings

eltsBindingMap :: BindingMap a -> [Binding a]
eltsBindingMap = eltsVarEnv . getBindings

