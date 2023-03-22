{-|
Copyright  :  (C) 2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Helpers to make 'Clash.XException.XException' explicit in the type system.
Using these helpers can help programmers account for 'Clash.XException.XException's
properly in blackbox models or tests. Note that none of these operations can be
translated to HDL.

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Clash.XException.MaybeX
  ( MaybeX(IsX, IsDefined)

    -- * Construction
  , toMaybeX
  , hasXToMaybeX

    -- * Deconstruction
  , fromMaybeX

    -- * Operations
  , andX
  , orX
  , maybeX
  ) where

import Prelude

import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Exception (throw)

import Clash.XException (XException(..), isX, hasX)

-- | Structure helping programmers to deal with 'Clash.XException.XException'
-- values. For safety reasons it can't be constructed directly, but should be
-- constructed using either 'pure' or 'toMaybeX'. After construction, it can be
-- deconstructed using either 'IsX' or 'IsDefined'.
data MaybeX a
  = IsX_ String
  -- ^ Upon construction, @a@ evaluated to 'Clash.XException.XException'
  | IsDefined_ !a
  -- ^ Upon construction, @a@ evaluated to a non-bottom WHNF

instance Show a => Show (MaybeX a) where
  showsPrec d = \case
    IsX_ msg     -> showParen (d > 10) $ showString "IsX "       . showsPrec 11 msg
    IsDefined_ a -> showParen (d > 10) $ showString "IsDefined " . showsPrec 11 a

-- | Upon construction, @a@ evaluated to 'Clash.XException.XException'
pattern IsX :: forall a. String -> MaybeX a
pattern IsX msg <- IsX_ msg

-- | Upon construction, @a@ evaluated to a non-bottom WHNF
pattern IsDefined :: forall a. a -> MaybeX a
pattern IsDefined a <- IsDefined_ a
{-# COMPLETE IsX, IsDefined #-}

-- | Note that 'fmap' is X-strict in its argument. That is, if its input is 'IsX',
-- its output will be too.
instance Functor MaybeX where
  fmap _f (IsX_ msg) = IsX_ msg
  fmap f  (IsDefined_ a) = pure (f a)

-- | Note that '<*>' and 'liftA2' are X-strict in their arguments. That is, if
-- any of their inputs are 'IsX', their outputs will be too.
instance Applicative MaybeX where
  pure = either IsX_ IsDefined_ . isX

  liftA2 f (IsDefined_ a) (IsDefined_ b) = pure (f a b)
  liftA2 _ (IsX_ msg)     _              = IsX_ msg
  liftA2 _ _              (IsX_ msg)     = IsX_ msg

-- | Construct a 'MaybeX' value. If @a@ evaluates to 'Clash.XException.XException',
-- this function will return 'IsX'. Otherwise, it will return 'IsDefined'.
toMaybeX :: a -> MaybeX a
toMaybeX = pure

-- | Construct a 'MaybeX' value. If 'hasX' evaluates to 'Left', this function
-- will return 'IsX'. Otherwise, it will return 'IsDefined'.
hasXToMaybeX :: NFData a => a -> MaybeX a
hasXToMaybeX = either IsX_ IsDefined_ . hasX

-- | Deconstruct 'MaybeX' into an @a@ - the opposite of 'toMaybeX'. Be careful
-- when using this function, because it might return an 'Clash.XException.XException'
-- if the argument was 'IsX'.
fromMaybeX :: MaybeX a -> a
fromMaybeX = maybeX (throw . XException) id

-- | Map functions over both constructors.
maybeX :: (String -> b) -> (a -> b) -> MaybeX a -> b
maybeX f _ (IsX_ msg)     = f msg
maybeX _ g (IsDefined_ a) = g a

-- | Implements '&&' accounting for X
--
-- +-------------------------+-------------------------+-------------------------+-------------------------+
-- |                         | &#x2003;__@X@__&#x2003; | &#x2003;__@1@__&#x2003; | &#x2003;__@0@__&#x2003; |
-- +-------------------------+-------------------------+-------------------------+-------------------------+
-- | &#x2003;__@X@__&#x2003; | &#x2003;@X@&#x2003;     | &#x2003;@X@&#x2003;     | &#x2003;@0@&#x2003;     |
-- +-------------------------+-------------------------+-------------------------+-------------------------+
-- | &#x2003;__@1@__&#x2003; | &#x2003;@X@&#x2003;     | &#x2003;@1@&#x2003;     | &#x2003;@0@&#x2003;     |
-- +-------------------------+-------------------------+-------------------------+-------------------------+
-- | &#x2003;__@0@__&#x2003; | &#x2003;@0@&#x2003;     | &#x2003;@0@&#x2003;     | &#x2003;@0@&#x2003;     |
-- +-------------------------+-------------------------+-------------------------+-------------------------+

-- (This is not part of the Haddock, a more readable version of the table
-- above)
--    | X | 1 | 0
-- ---|---|---|---
--  X | X | X | 0
--  1 | X | 1 | 0
--  0 | 0 | 0 | 0
andX :: MaybeX Bool -> MaybeX Bool -> MaybeX Bool
andX (IsDefined_ False) _                  = IsDefined_ False
andX _                  (IsDefined_ False) = IsDefined_ False
andX (IsDefined_ True)  (IsDefined_ True)  = IsDefined_ True
andX (IsX_ msg)         _                  = IsX_ msg
andX _                  (IsX_ msg)         = IsX_ msg
infixr 3 `andX`

-- | Implements '||' accounting for X
--
-- +-------------------------+-------------------------+-------------------------+-------------------------+
-- |                         | &#x2003;__@X@__&#x2003; | &#x2003;__@1@__&#x2003; | &#x2003;__@0@__&#x2003; |
-- +-------------------------+-------------------------+-------------------------+-------------------------+
-- | &#x2003;__@X@__&#x2003; | &#x2003;X&#x2003;       | &#x2003;1&#x2003;       | &#x2003;X&#x2003;       |
-- +-------------------------+-------------------------+-------------------------+-------------------------+
-- | &#x2003;__@1@__&#x2003; | &#x2003;1&#x2003;       | &#x2003;1&#x2003;       | &#x2003;1&#x2003;       |
-- +-------------------------+-------------------------+-------------------------+-------------------------+
-- | &#x2003;__@0@__&#x2003; | &#x2003;X&#x2003;       | &#x2003;1&#x2003;       | &#x2003;0&#x2003;       |
-- +-------------------------+-------------------------+-------------------------+-------------------------+

-- (This is not part of the Haddock, a more readable version of the table
-- above)
--    | X | 1 | 0
-- ---|---|---|---
--  X | X | 1 | X
--  1 | 1 | 1 | 1
--  0 | X | 1 | 0
orX :: MaybeX Bool -> MaybeX Bool -> MaybeX Bool
orX (IsDefined_ True)  _                  = IsDefined_ True
orX _                  (IsDefined_ True)  = IsDefined_ True
orX (IsDefined_ False) (IsDefined_ False) = IsDefined_ False
orX (IsX_ msg)         _                  = IsX_ msg
orX _                  (IsX_ msg)         = IsX_ msg
infixr 2 `orX`
