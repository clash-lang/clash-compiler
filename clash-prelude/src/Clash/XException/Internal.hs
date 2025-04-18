{-|
Copyright  :  (C) 2016,      University of Twente,
                  2017,      Google Inc.,
                  2017-2019, Myrtle Software Ltd,
                  2017-2025, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

'XException': An exception for uninitialized values

>>> show (errorX "No value here" :: Integer, 4 :: Int)
"(*** Exception: X: No value here
CallStack (from HasCallStack):
...
>>> showX (errorX "No value here" :: Integer, 4 :: Int)
"(undefined,4)"
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK not-home #-}

module Clash.XException.Internal
  ( XException(..)
    -- * Printing 'XException's as @undefined@
  , showsX, showsPrecXWith
  , showXWith

    -- * Internals
  , GShowX(..), GDeepErrorX(..), GHasUndefined(..), GEnsureSpine(..)
  , GNFDataX(..), Zero, One, ShowType(..), RnfArgs(..), NFDataX1(..)
  , showListX__, genericShowsPrecX
  )
where

import           Prelude             hiding (undefined)

import {-# SOURCE #-} Clash.XException
import           Control.Exception
  (Exception,  catch, evaluate)
import           Data.Either         (isLeft)
import           GHC.Exts
  (Char (C#), Double (D#), Float (F#), Int (I#), Word (W#))
import           GHC.Generics
import           GHC.Show            (appPrec)
import           GHC.Stack           (HasCallStack)
import           System.IO.Unsafe    (unsafeDupablePerformIO)

-- $setup
-- >>> import Clash.Prelude

-- | An exception representing an \"uninitialized\" value.
newtype XException = XException String

instance Show XException where
  show (XException s) = s

instance Exception XException

-- | Like 'shows', but values that normally throw an 'XException' are
-- converted to @undefined@, instead of error'ing out with an exception.
showsX :: ShowX a => a -> ShowS
showsX = showsPrecX 0

showListX__ :: (a -> ShowS) -> [a] -> ShowS
showListX__ showx = showXWith go
  where
    go []     s = "[]" ++ s
    go (x:xs) s = '[' : showx x (showl xs)
      where
        showl []     = ']':s
        showl (y:ys) = ',' : showx y (showl ys)

genericShowsPrecX :: (Generic a, GShowX (Rep a)) => Int -> a -> ShowS
genericShowsPrecX n = gshowsPrecX Pref n . from

showXWith :: (a -> ShowS) -> a -> ShowS
showXWith f x =
  unsafeDupablePerformIO $
    catch
      (f <$> evaluate x)
      (\(XException _) -> return (showString "undefined"))

-- | Use when you want to create a 'ShowX' instance where:
--
-- - There is no 'Generic' instance for your data type
-- - The 'Generic' derived ShowX method would traverse into the (hidden)
--   implementation details of your data type, and you just want to show the
--   entire value as @undefined@.
--
-- Can be used like:
--
-- > data T = ...
-- >
-- > instance Show T where ...
-- >
-- > instance ShowX T where
-- >   showsPrecX = showsPrecXWith showsPrec
showsPrecXWith :: (Int -> a -> ShowS) -> Int -> a -> ShowS
showsPrecXWith f n = showXWith (f n)

class GShowX f where
  gshowsPrecX :: ShowType -> Int -> f a -> ShowS
  isNullary   :: f a -> Bool
  isNullary = error "generic showX (isNullary): unnecessary case"

data ShowType = Rec        -- Record
              | Tup        -- Tuple
              | Pref       -- Prefix
              | Inf String -- Infix

instance GShowX U1 where
  gshowsPrecX _ _ U1 = id
  isNullary _ = True

instance (ShowX c) => GShowX (K1 i c) where
  gshowsPrecX _ n (K1 a) = showsPrecX n a
  isNullary _ = False

instance (GShowX a, Constructor c) => GShowX (M1 C c a) where
  gshowsPrecX _ n c@(M1 x) =
    case fixity of
      Prefix ->
        showParen (n > appPrec && not (isNullary x))
          ( (if conIsTuple c then id else showString (conName c))
          . (if isNullary x || conIsTuple c then id else showString " ")
          . showBraces t (gshowsPrecX t appPrec x))
      Infix _ m -> showParen (n > m) (showBraces t (gshowsPrecX t m x))
      where fixity = conFixity c
            t = if conIsRecord c then Rec else
                  case conIsTuple c of
                    True -> Tup
                    False -> case fixity of
                                Prefix    -> Pref
                                Infix _ _ -> Inf (show (conName c))
            showBraces :: ShowType -> ShowS -> ShowS
            showBraces Rec     p = showChar '{' . p . showChar '}'
            showBraces Tup     p = showChar '(' . p . showChar ')'
            showBraces Pref    p = p
            showBraces (Inf _) p = p

            conIsTuple :: C1 c f p -> Bool
            conIsTuple y = tupleName (conName y) where
              tupleName ('(':',':_) = True
              tupleName _           = False

instance (Selector s, GShowX a) => GShowX (M1 S s a) where
  gshowsPrecX t n s@(M1 x) | selName s == "" =   gshowsPrecX t n x
                           | otherwise       =   showString (selName s)
                                               . showString " = "
                                               . gshowsPrecX t 0 x
  isNullary (M1 x) = isNullary x

instance (GShowX a) => GShowX (M1 D d a) where
  gshowsPrecX t = showsPrecXWith go
    where go n (M1 x) = gshowsPrecX t n x

instance (GShowX a, GShowX b) => GShowX (a :+: b) where
  gshowsPrecX t n (L1 x) = gshowsPrecX t n x
  gshowsPrecX t n (R1 x) = gshowsPrecX t n x

instance (GShowX a, GShowX b) => GShowX (a :*: b) where
  gshowsPrecX t@Rec     n (a :*: b) =
    gshowsPrecX t n     a . showString ", " . gshowsPrecX t n     b
  gshowsPrecX t@(Inf s) n (a :*: b) =
    gshowsPrecX t n     a . showString s    . gshowsPrecX t n     b
  gshowsPrecX t@Tup     n (a :*: b) =
    gshowsPrecX t n     a . showChar ','    . gshowsPrecX t n     b
  gshowsPrecX t@Pref    n (a :*: b) =
    gshowsPrecX t (n+1) a . showChar ' '    . gshowsPrecX t (n+1) b

  -- If we have a product then it is not a nullary constructor
  isNullary _ = False

-- Unboxed types
instance GShowX UChar where
  gshowsPrecX _ _ (UChar c)   = showsPrec 0 (C# c) . showChar '#'
instance GShowX UDouble where
  gshowsPrecX _ _ (UDouble d) = showsPrec 0 (D# d) . showString "##"
instance GShowX UFloat where
  gshowsPrecX _ _ (UFloat f)  = showsPrec 0 (F# f) . showChar '#'
instance GShowX UInt where
  gshowsPrecX _ _ (UInt i)    = showsPrec 0 (I# i) . showChar '#'
instance GShowX UWord where
  gshowsPrecX _ _ (UWord w)   = showsPrec 0 (W# w) . showString "##"

-- | Hidden internal type-class. Adds a generic implementation for the \"NFData\"
-- part of 'NFDataX'
class GNFDataX arity f where
  grnfX :: RnfArgs arity a -> f a -> ()

instance GNFDataX arity V1 where
  grnfX _ x = case x of {}

data Zero
data One

data RnfArgs arity a where
  RnfArgs0 :: RnfArgs Zero a
  RnfArgs1  :: (a -> ()) -> RnfArgs One a

instance GNFDataX arity U1 where
  grnfX _ u = if isLeft (isX u) then () else case u of U1 -> ()

instance NFDataX a => GNFDataX arity (K1 i a) where
  grnfX _ = rnfX . unK1
  {-# INLINEABLE grnfX #-}

instance GNFDataX arity a => GNFDataX arity (M1 i c a) where
  grnfX args a =
    -- Check for X needed to handle edge-case "data Void"
    if isLeft (isX a) then
      ()
    else
      grnfX args (unM1 a)
  {-# INLINEABLE grnfX #-}

instance (GNFDataX arity a, GNFDataX arity b) => GNFDataX arity (a :*: b) where
  grnfX args xy@(~(x :*: y)) =
    if isLeft (isX xy) then
      ()
    else
      grnfX args x `seq` grnfX args y
  {-# INLINEABLE grnfX #-}

instance (GNFDataX arity a, GNFDataX arity b) => GNFDataX arity (a :+: b) where
  grnfX args lrx =
    if isLeft (isX lrx) then
      ()
    else
      case lrx of
        L1 x -> grnfX args x
        R1 x -> grnfX args x
  {-# INLINEABLE grnfX #-}

instance GNFDataX One Par1 where
  grnfX (RnfArgs1 r) = r . unPar1

instance NFDataX1 f => GNFDataX One (Rec1 f) where
  grnfX (RnfArgs1 r) = liftRnfX r . unRec1

instance (NFDataX1 f, GNFDataX One g) => GNFDataX One (f :.: g) where
  grnfX args = liftRnfX (grnfX args) . unComp1

class GEnsureSpine f where
  gEnsureSpine :: f a -> f a

instance GEnsureSpine U1 where
  gEnsureSpine _u = U1

instance NFDataX a => GEnsureSpine (K1 i a) where
  gEnsureSpine = K1 . ensureSpine . unK1
  {-# INLINEABLE gEnsureSpine #-}

instance GEnsureSpine a => GEnsureSpine (M1 i c a) where
  gEnsureSpine a = M1 (gEnsureSpine (unM1 a))
  {-# INLINEABLE gEnsureSpine #-}

instance (GEnsureSpine a, GEnsureSpine b) => GEnsureSpine (a :*: b) where
  gEnsureSpine ~(x :*: y) = gEnsureSpine x :*: gEnsureSpine y
  {-# INLINEABLE gEnsureSpine #-}

instance (GEnsureSpine a, GEnsureSpine b) => GEnsureSpine (a :+: b) where
  gEnsureSpine lrx =
    case lrx of
      (L1 x) -> L1 (gEnsureSpine x)
      (R1 x) -> R1 (gEnsureSpine x)
  {-# INLINEABLE gEnsureSpine #-}

instance GEnsureSpine V1 where
  gEnsureSpine _ = error "Unreachable code?"

-- | A class of functors that can be fully evaluated, according to semantics
-- of NFDataX.
class NFDataX1 f where
  -- | 'liftRnfX' should reduce its argument to normal form (that is, fully
  -- evaluate all sub-components), given an argument to reduce @a@ arguments,
  -- and then return @()@.
  --
  -- See 'rnfX' for the generic deriving.
  liftRnfX :: (a -> ()) -> f a -> ()

  default liftRnfX :: (Generic1 f, GNFDataX One (Rep1 f)) => (a -> ()) -> f a -> ()
  liftRnfX r = grnfX (RnfArgs1 r) . from1


class GHasUndefined f where
  gHasUndefined :: f a -> Bool

instance GHasUndefined U1 where
  gHasUndefined u = if isLeft (isX u) then True else case u of U1 -> False

instance NFDataX a => GHasUndefined (K1 i a) where
  gHasUndefined = hasUndefined . unK1
  {-# INLINEABLE gHasUndefined #-}

instance GHasUndefined a => GHasUndefined (M1 i c a) where
  gHasUndefined a =
    -- Check for X needed to handle edge-case "data Void"
    if isLeft (isX a) then
      True
    else
      gHasUndefined (unM1 a)
  {-# INLINEABLE gHasUndefined #-}

instance (GHasUndefined a, GHasUndefined b) => GHasUndefined (a :*: b) where
  gHasUndefined xy@(~(x :*: y)) =
    if isLeft (isX xy) then
      True
    else
      gHasUndefined x || gHasUndefined y
  {-# INLINEABLE gHasUndefined #-}

instance (GHasUndefined a, GHasUndefined b) => GHasUndefined (a :+: b) where
  gHasUndefined lrx =
    if isLeft (isX lrx) then
      True
    else
      case lrx of
        L1 x -> gHasUndefined x
        R1 x -> gHasUndefined x
  {-# INLINEABLE gHasUndefined #-}

instance GHasUndefined V1 where
  gHasUndefined _ = error "Unreachable code?"

class GDeepErrorX f where
  gDeepErrorX :: HasCallStack => String -> f a

instance GDeepErrorX V1 where
  gDeepErrorX = errorX

instance GDeepErrorX U1 where
  gDeepErrorX = const U1

instance (GDeepErrorX a) => GDeepErrorX (M1 m d a) where
  gDeepErrorX e = M1 (gDeepErrorX e)

instance (GDeepErrorX f, GDeepErrorX g) => GDeepErrorX (f :*: g) where
  gDeepErrorX e = gDeepErrorX e :*: gDeepErrorX e

instance NFDataX c => GDeepErrorX (K1 i c) where
  gDeepErrorX e = K1 (deepErrorX e)

instance GDeepErrorX (f :+: g) where
  gDeepErrorX = errorX
