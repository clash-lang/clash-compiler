{-|
Copyright  :  (C) 2023,      QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE TemplateHaskell #-}

module Clash.Class.IndexTuple.Deriving
  (IndexTuple(..), deriveIndexTupleInstances) where

import Control.Monad.Extra (concatMapM)
import Language.Haskell.TH

class IndexTuple a where
  -- | This function can be used to bind variables to numbers indexing
  -- successive elements in a list-like structure. If the type of the indices
  -- can be derived from context, it is enough to provide a tuple with the
  -- variables like:
  --
  -- @
  -- (a, b) = indexTuple
  -- (a, b, c, d, e, f) = indexTuple
  -- @
  --
  -- and so on. @a@ will be 0, @b@ 1, @c@ 2, etcetera. All variables will have
  -- the same type, and specifying the type of any one of the variables is
  -- enough. The type needs to be an instance of `Num`.
  --
  -- If a type needs to be specified here, you can use
  --
  -- @
  -- (a :: Int, b, c) = indexTuple
  -- @
  --
  -- or
  --
  -- @
  -- a :: Int
  -- (a, b, c) = indexTuple
  -- @
  --
  -- depending on preference.
  indexTuple :: a

-- Derive instance for /n/-tuple
deriveIndexTupleInstance :: Int -> DecsQ
deriveIndexTupleInstance n = do
  let e (i :: Int) = varT $ mkName $ "e" ++ show i
      e0  = e 0
      eqT i = [t| $(e i) ~ $e0 |]
      instCxt0 = appT (tupleT n) [t| Num $e0 |]
      instCxt1 = foldl appT instCxt0 $ map eqT [1 .. n - 1]
      instType  = foldl appT (tupleT n) $ map e [0 .. n - 1]
      funRHS = tupE $ map (litE . integerL) [0 .. toInteger n - 1]
  [d| instance $instCxt1 => IndexTuple $instType where
        indexTuple = $funRHS
   |]

-- Derive instances for up to and including to /n/-tuples
deriveIndexTupleInstances :: Int -> DecsQ
deriveIndexTupleInstances n = concatMapM deriveIndexTupleInstance [1..n]
