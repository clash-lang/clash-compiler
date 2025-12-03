{-|
Copyright  :  (C) 2016,      University of Twente,
                  2017,      QBayLogic, Google Inc.
                  2017-2019, Myrtle Software Ltd,
                  2021-2025, QBayLogic B.V.
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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE Trustworthy #-}

module Clash.XException
  ( -- * 'XException': An exception for uninitialized values
    XException(..), errorX, isX, hasX, maybeIsX, maybeHasX, fromJustX, undefined,
    xToErrorCtx, xToError
    -- * Printing 'XException's as @undefined@
  , ShowX (..), showsX, printX, showsPrecXWith
    -- * Strict evaluation
  , seqX, seqErrorX, forceX, deepseqX, rwhnfX, defaultSeqX, hwSeqX
    -- * Structured undefined / deep evaluation with undefined values
  , NFDataX (rnfX, deepErrorX, hasUndefined, ensureSpine)
  )
where

import           Prelude             hiding (undefined)

import           Clash.Annotations.Primitive (hasBlackBox)
import           Clash.CPP           (maxTupleSize, fSuperStrict)
import           Clash.XException.Internal
import           Clash.XException.TH
import           Control.Exception
  (ErrorCall (..), Handler(..), catch, catches, evaluate, throw)
import           Control.DeepSeq     (NFData, rnf)
import           Data.Complex        (Complex)
import           Data.Either         (isLeft)
import           Data.Foldable       (toList)
import           Data.Functor.Compose (Compose)
import           Data.Functor.Const  (Const)
import           Data.Functor.Identity (Identity)
import           Data.Functor.Product (Product)
import           Data.Functor.Sum    (Sum)
import           Data.Int            (Int8, Int16, Int32, Int64)
import qualified Data.List.Infinite  as Inf
import           Data.List.Infinite  (Infinite (..))
import           Data.List.NonEmpty  (NonEmpty)
import           Data.Ord            (Down (Down))
import           Data.Proxy          (Proxy)
import           Data.Ratio          (Ratio, numerator, denominator)
import qualified Data.Semigroup      as SG
import qualified Data.Monoid         as M
import           Data.Sequence       (Seq(Empty, (:<|)))
import           Data.Word           (Word8, Word16, Word32, Word64)
import           Foreign.C.Types     (CUShort)
import           GHC.Generics
import           GHC.Natural         (Natural)
import           GHC.Stack
  (HasCallStack, callStack, prettyCallStack, withFrozenCallStack)
import           Numeric.Half        (Half)
import           System.IO.Unsafe    (unsafeDupablePerformIO)

-- $setup
-- >>> :m -Prelude
-- >>> import Clash.Prelude
-- >>> import Clash.Class.BitPack (pack)
-- >>> import Clash.Sized.Vector (Vec)
-- >>> import Clash.Sized.RTree (RTree)
-- >>> :set -fplugin GHC.TypeLits.Normalise
-- >>> :set -fplugin GHC.TypeLits.KnownNat.Solver


-- | Either 'seqX' or 'deepseqX' depending on the value of the cabal flag
-- '-fsuper-strict'. If enabled, 'defaultSeqX' will be 'deepseqX', otherwise
-- 'seqX'. Flag defaults to /false/ and thus 'seqX'.
defaultSeqX :: NFDataX a => a -> b -> b
defaultSeqX = if fSuperStrict then deepseqX else seqX
{-# INLINE defaultSeqX #-}
infixr 0 `defaultSeqX`

-- | Like 'error', but throwing an 'XException' instead of an 'ErrorCall'
--
-- The 'ShowX' methods print these error-values as @undefined@; instead of error'ing
-- out with an exception.
errorX :: HasCallStack => String -> a
errorX msg = throw (XException ("X: " ++ msg ++ "\n" ++ prettyCallStack callStack))
{-# NOINLINE errorX #-}
{-# ANN errorX hasBlackBox #-}

-- | Convert 'XException' to 'ErrorCall'
--
-- This is useful when tracking the source of 'XException' that gets eaten up by
-- 'Clash.Classes.BitPack.pack' inside of your circuit; since
-- 'Clash.Classes.BitPack.pack' translates 'XException' into undefined bits.
--
-- So for example if you have some large function f:
--
-- > f a b = ... pack a ... pack b ...
--
-- Where it is basically an error if either /a/ or /b/ ever throws an 'XException',
-- and so you want that to be reported the moment /a/ or /b/ is used, instead of
-- it being thrown when evaluating the result of /f/, then do:
--
-- > {-# LANGUAGE ViewPatterns #-}
-- > f (xToErrorCtx "a is X" -> a) (xToErrorCtx "b is X" -> b) = ...
--
-- Where we pass an extra string, for context, to know which argument evaluated
-- to an 'XException'. We can also use BangPatterns to report the potential
-- 'XException' being thrown by /a/ or /b/ even earlier, i.e. when /f/ is applied:
--
-- > {-# LANGUAGE ViewPatterns, BangPatterns #-}
-- > f (xToErrorCtx "a is X" -> !a) (xToErrorCtx "b is X" -> !b) = ...
--
-- __NB__: Fully synthesizable, so doesn't have to be removed before synthesis
--
-- === __Example__
--
-- >>> :set -XViewPatterns -XDataKinds
-- >>> import Clash.Sized.BitVector
-- >>> import GHC.Stack
-- >>> :{
-- let h, h' :: Bit -> BitVector 8 -> BitVector 8
--     h (xToErrorCtx "a is X" -> a) (xToErrorCtx "b is X" -> b) = slice d7 d0 (pack a ++# b)
--     h' a b = slice d7 d0 (pack a ++# b)
-- :}
--
-- >>> h' (errorX "QQ") 3
-- 0b0000_0011
-- >>> h (errorX "QQ") 3
-- *** Exception: a is X
-- X: QQ
-- CallStack (from HasCallStack):
--   errorX, called at ...
-- <BLANKLINE>
xToErrorCtx :: String -> a -> a
xToErrorCtx ctx a = unsafeDupablePerformIO
  (catch (evaluate a >> return a)
         (\(XException msg) ->
           throw (ErrorCall (unlines [ctx,msg]))))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE xToErrorCtx #-}

-- | Convert 'XException' to 'ErrorCall'
--
-- This is useful when tracking the source of 'XException' that gets eaten up by
-- 'Clash.Classes.BitPack.pack' inside of your circuit; since
-- 'Clash.Classes.BitPack.pack' translates 'XException' into undefined bits.
--
-- So for example if you have some large function f:
--
-- > f a b = ... pack a ... pack b ...
--
-- Where it is basically an error if either /a/ or /b/ ever throws an 'XException',
-- and so you want that to be reported the moment /a/ or /b/ is used, instead of
-- it being thrown when evaluating the result of /f/, then do:
--
-- > {-# LANGUAGE ViewPatterns #-}
-- > f (xToError -> a) (xToError -> b) = ...
--
-- Unlike 'xToErrorCtx', where we have an extra String argument to distinguish
-- one call to 'xToError' to the other, 'xToError' will use the 'GHC.CallStack'
-- mechanism to aid the user in distinguishing different call to 'xToError'.
-- We can also use BangPatterns to report the potential 'XException' being
-- thrown by /a/ or /b/ even earlier, i.e. when /f/ is applied:
--
-- > {-# LANGUAGE ViewPatterns, BangPatterns #-}
-- > f (xToError -> !a) (xToError -> !b) = ...
--
-- __NB__: Fully synthesizable, so doesn't have to be removed before synthesis
--
-- === __Example__
--
-- >>> :set -XViewPatterns -XDataKinds
-- >>> import Clash.Sized.BitVector
-- >>> import GHC.Stack
-- >>> :{
-- let f, g, h, h' :: HasCallStack => Bit -> BitVector 8 -> BitVector 8
--     f = g
--     g = h
--     h (xToError -> a) (xToError -> b) = slice d7 d0 (pack a ++# b)
--     h' a b = slice d7 d0 (pack a ++# b)
-- :}
--
-- >>> h' (errorX "QQ") 3
-- 0b0000_0011
-- >>> f (errorX "QQ") 3
-- *** Exception: CallStack (from HasCallStack):
--   xToError, called at ...
--   h, called at ...
--   g, called at ...
--   f, called at ...
-- X: QQ
-- CallStack (from HasCallStack):
--   errorX, called at ...
-- <BLANKLINE>
xToError :: HasCallStack => a -> a
xToError = xToErrorCtx (prettyCallStack callStack)
{-# INLINE xToError #-}

-- | Like 'seq', however, whereas 'seq' will always do:
--
-- > seq  _|_              b = _|_
--
-- 'seqX' will do:
--
-- > seqX (XException msg) b = b
-- > seqX _|_              b = _|_
seqX :: a -> b -> b
seqX a b = unsafeDupablePerformIO
  (catch (evaluate a >> return b) (\(XException _) -> return b))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE seqX #-}
{-# ANN seqX hasBlackBox #-}
infixr 0 `seqX`

-- | Like 'seqX', but will also catch ErrorCall exceptions which are thrown.
-- This should be used with care.
--
-- > seqErrorX (ErrorCall msg)  b = b
-- > seqErrorX (XException msg) b = b
-- > seqErrorX _|_              b = _|_
seqErrorX :: a -> b -> b
seqErrorX a b = unsafeDupablePerformIO
  ((evaluate a >> return b) `catches`
     [ Handler (\(XException _) -> return b)
     , Handler (\(ErrorCall _) -> return b)
     ])
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE seqErrorX #-}
{-# ANN seqErrorX hasBlackBox #-}
infixr 0 `seqErrorX`

-- | Like 'seqX' in simulation, but will force its first argument to be rendered
-- in HDL. This is useful for components that need to be rendered in hardware,
-- but otherwise have no meaning in simulation. An example of such a component
-- would be an ILA: a component monitoring an internal signal of a design. The
-- output of such a component (typically a unit) can be passed as the first
-- argument to 'hwSeqX' to ensure the ILA ends up in the generated HDL.
--
-- __NB__: The result of 'hwSeqX' must (indirectly) be used at the very top of
-- a design. If it's not, Clash will remove it like it does for any other unused
-- circuit parts.
--
-- __NB__: Make sure the blackbox for the component with zero-width results
-- uses 'Clash.Netlist.BlackBox.Types.RenderVoid'
hwSeqX :: a -> b -> b
hwSeqX = seqX
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE hwSeqX #-}
{-# ANN hwSeqX hasBlackBox #-}
infixr 0 `hwSeqX`

-- | Evaluate a value with given function, returning 'Nothing' if it throws
-- 'XException'. Note that non-'XException' errors take precedence over 'XException'
-- ones
--
-- > maybeX hasX 42                    = Just 42
-- > maybeX hasX (XException msg)      = Nothing
-- > maybeX hasX (3, XException msg)   = Nothing
-- > maybeX hasX (XException msg, _|_) = _|_
-- > maybeX hasX (_|_, XException msg) = _|_
-- > maybeX hasX (3, _|_)              = _|_
-- > maybeX hasX _|_                   = _|_
-- >
-- > maybeX isX 42                  = Just 42
-- > maybeX isX (XException msg)    = Nothing
-- > maybeX isX (3, XException msg) = Just (3, XException msg)
-- > maybeX isX (3, _|_)            = Just (3, _|_)
-- > maybeX isX _|_                 = _|_
--
maybeX :: (a -> Either String a) -> a -> Maybe a
maybeX f a = either (const Nothing) Just (f a)

-- | Fully evaluate a value, returning 'Nothing' if it throws 'XException'. Note
-- that non-'XException' errors take precedence over 'XException' ones.
--
-- > maybeHasX 42                    = Just 42
-- > maybeHasX (XException msg)      = Nothing
-- > maybeHasX (3, XException msg)   = Nothing
-- > maybeHasX (XException msg, _|_) = _|_
-- > maybeHasX (_|_, XException msg) = _|_
-- > maybeHasX (3, _|_)              = _|_
-- > maybeHasX _|_                   = _|_
--
maybeHasX :: (NFData a, NFDataX a) => a -> Maybe a
maybeHasX = maybeX hasX

-- | Evaluate a value to WHNF, returning 'Nothing' if it throws 'XException'.
--
-- > maybeIsX 42                  = Just 42
-- > maybeIsX (XException msg)    = Nothing
-- > maybeIsX (3, XException msg) = Just (3, XException msg)
-- > maybeIsX (3, _|_)            = Just (3, _|_)
-- > maybeIsX _|_                 = _|_
maybeIsX :: a -> Maybe a
maybeIsX = maybeX isX

-- | Fully evaluate a value, returning @'Left' msg@ if it throws 'XException'.
-- If you want to determine if a value contains undefined parts, use
-- 'hasUndefined' instead.
--
-- > hasX 42                    = Right 42
-- > hasX (XException msg)      = Left msg
-- > hasX (3, XException msg)   = Left msg
-- > hasX (XException msg, _|_) = _|_
-- > hasX (_|_, XException msg) = _|_
-- > hasX (3, _|_)              = _|_
-- > hasX _|_                   = _|_
--
-- If a data structure contains multiple 'XException's, the "first" message is
-- picked according to the implementation of 'rnfX'.
hasX :: (NFData a, NFDataX a) => a -> Either String a
hasX a =
  -- TODO: Whenever 'a' contains an 'XException', we need to reevaluate the
  --       structure using 'rnfX' to make sure it didn't also contain another
  --       error call. We could prevent the two traversals by making 'hasX' a
  --       type class method. Also see: https://github.com/clash-lang/clash-compiler/issues/2450.
  unsafeDupablePerformIO
    (catch
      (evaluate (rnf a) >> return (Right a))
      (\(XException msg) -> evaluate (rnfX a) >> return (Left msg)))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE hasX #-}

-- | Evaluate a value to WHNF, returning @'Left' msg@ if is a 'XException'.
--
-- > isX 42                  = Right 42
-- > isX (XException msg)    = Left msg
-- > isX (3, XException msg) = Right (3, XException msg)
-- > isX (3, _|_)            = Right (3, _|_)
-- > isX _|_                 = _|_
isX :: a -> Either String a
isX a =
  unsafeDupablePerformIO
    (catch
      (evaluate a >> return (Right a))
      (\(XException msg) -> return (Left msg)))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE isX #-}

-- | Like the 'Show' class, but values that normally throw an 'XException' are
-- converted to @undefined@, instead of error'ing out with an exception.
--
-- >>> show (errorX "No value here" :: Integer, 4 :: Int)
-- "(*** Exception: X: No value here
-- CallStack (from HasCallStack):
-- ...
-- >>> showX (errorX "No value here" :: Integer, 4 :: Int)
-- "(undefined,4)"
--
-- Can be derived using 'GHC.Generics':
--
-- > {-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- >
-- > import Clash.Prelude
-- > import GHC.Generics
-- >
-- > data T = MkTA Int | MkTB Bool
-- >   deriving (Show,Generic,ShowX)
class ShowX a where
  -- | Like 'showsPrec', but values that normally throw an 'XException' are
  -- converted to @undefined@, instead of error'ing out with an exception.
  showsPrecX :: Int -> a -> ShowS

  -- | Like 'show', but values that normally throw an 'XException' are
  -- converted to @undefined@, instead of error'ing out with an exception.
  showX :: a -> String
  showX x = showsX x ""

  -- | Like 'showList', but values that normally throw an 'XException' are
  -- converted to @undefined@, instead of error'ing out with an exception.
  showListX :: [a] -> ShowS
  showListX ls s = showListX__ showsX ls s

  default showsPrecX :: (Generic a, GShowX (Rep a)) => Int -> a -> ShowS
  showsPrecX = genericShowsPrecX

-- | Like 'print', but values that normally throw an 'XException' are
-- converted to @undefined@, instead of error'ing out with an exception
printX :: ShowX a => a -> IO ()
printX x = putStrLn $ showX x

instance ShowX ()
-- | @since 1.8.2
instance ShowX (Proxy a)
instance ShowX a => ShowX (Identity a)
instance ShowX a => ShowX (Const a b)
instance (ShowX (f a), ShowX (g a)) => ShowX (Product f g a)
instance (ShowX (f a), ShowX (g a)) => ShowX (Sum f g a)
instance (ShowX (f (g a))) => ShowX (Compose f g a)

instance {-# OVERLAPPABLE #-} ShowX a => ShowX [a] where
  showsPrecX _ = showListX

instance ShowX Char where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Bool

instance ShowX Double where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX a => ShowX (Down a) where
  showsPrecX = showsPrecXWith showsPrecX

instance (ShowX a, ShowX b) => ShowX (Either a b)

instance ShowX Float where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Int where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Int8 where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Int16 where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Int32 where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Int64 where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Integer where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Natural where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Ordering

instance ShowX a => ShowX (Seq a) where
  showsPrecX _ = showListX . toList

instance ShowX Word where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Word8 where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Word16 where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Word32 where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Word64 where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX a => ShowX (Maybe a)

instance ShowX a => ShowX (Ratio a) where
  showsPrecX = showsPrecXWith showsPrecX

instance ShowX a => ShowX (Complex a)

instance {-# OVERLAPPING #-} ShowX String where
  showsPrecX = showsPrecXWith showsPrec


-- | a variant of 'deepseqX' that is useful in some circumstances:
--
-- > forceX x = x `deepseqX` x
forceX :: NFDataX a => a -> a
forceX x = x `deepseqX` x
{-# INLINE forceX #-}

-- | 'deepseqX': fully evaluates the first argument, before returning the
-- second. Does not propagate 'XException's.
deepseqX :: NFDataX a => a -> b -> b
deepseqX a b = rnfX a `seq` b
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE deepseqX #-}
{-# ANN deepseqX hasBlackBox #-}
infixr 0 `deepseqX`

-- | Reduce to weak head normal form
--
-- Equivalent to @\\x -> 'seqX' x ()@.
--
-- Useful for defining 'NFDataX.rnfX' for types for which NF=WHNF holds.
rwhnfX :: a -> ()
rwhnfX = (`seqX` ())
{-# INLINE rwhnfX #-}

-- | Class that houses functions dealing with /undefined/ values in Clash. See
-- 'deepErrorX' and 'rnfX'.
class NFDataX a where
  -- | Create a value where all the elements have an 'errorX',
  -- but the spine is defined.
  deepErrorX :: HasCallStack => String -> a

  default deepErrorX :: (HasCallStack, Generic a, GDeepErrorX (Rep a)) => String -> a
  deepErrorX = withFrozenCallStack $ to . gDeepErrorX

  -- | Determines whether any of parts of a given construct contain undefined
  -- parts. Note that a negative answer does not mean its bit representation
  -- is fully defined. For example:
  --
  -- >>> m = Nothing :: Maybe Bool
  -- >>> hasUndefined m
  -- False
  -- >>> pack m
  -- 0b0.
  -- >>> hasUndefined (pack m)
  -- True
  --
  hasUndefined :: a -> Bool

  default hasUndefined :: (Generic a, GHasUndefined (Rep a)) => a -> Bool
  hasUndefined = gHasUndefined . from

  -- | Create a value where at the very least the spine is defined. For example:
  --
  -- >>> spined = ensureSpine (errorX "?" :: (Int, Int))
  -- >>> case spined of (_, _) -> 'a'
  -- 'a'
  -- >>> fmap (const 'b') (ensureSpine undefined :: Vec 3 Int)
  -- 'b' :> 'b' :> 'b' :> Nil
  -- >>> fmap (const 'c') (ensureSpine undefined :: RTree 2 Int)
  -- <<'c','c'>,<'c','c'>>
  --
  -- For users familiar with 'Clash.Sized.Vector.lazyV': this is the generalized
  -- version of it.
  ensureSpine :: a -> a

  default ensureSpine :: (Generic a, GEnsureSpine (Rep a)) => a -> a
  ensureSpine = to . gEnsureSpine . from

  -- | Evaluate a value to NF. As opposed to 'NFData's
  -- 'rnf', it does not bubble up 'XException's.
  rnfX :: a -> ()

  default rnfX :: (Generic a, GNFDataX Zero (Rep a)) => a -> ()
  rnfX = grnfX RnfArgs0 . from

instance NFDataX ()

instance NFDataX b => NFDataX (a -> b) where
  deepErrorX = pure . deepErrorX
  rnfX = rwhnfX
  hasUndefined = error "hasUndefined on Undefined (a -> b): Not Yet Implemented"
  ensureSpine = id

instance NFDataX a => NFDataX (Down a) where
  deepErrorX = Down . deepErrorX
  rnfX d@(~(Down x)) = if isLeft (isX d) then () else rnfX x
  hasUndefined d@(~(Down x))= if isLeft (isX d) then True else hasUndefined x
  ensureSpine ~(Down x) = Down (ensureSpine x)

instance NFDataX a => NFDataX (Infinite a) where
  deepErrorX msg = Inf.repeat (deepErrorX msg)
  rnfX d@(~(x :< xs)) =
    if isLeft (isX d) then
      ()
    else
      rnfX x `seq` rnfX xs
  hasUndefined d@(~(x :< xs)) =
    if isLeft (isX d) then
      True
    else
      hasUndefined x || hasUndefined xs

  ensureSpine ~(x :< xs) = ensureSpine x :< ensureSpine xs

instance NFDataX Bool
instance NFDataX Ordering
instance NFDataX a => NFDataX [a]
instance NFDataX a => NFDataX (NonEmpty a)
instance (NFDataX a, NFDataX b) => NFDataX (Either a b)
instance NFDataX a => NFDataX (Maybe a)
-- | @since 1.8.2
instance NFDataX (Proxy a)
instance NFDataX a => NFDataX (Identity a)
instance NFDataX a => NFDataX (Const a b)
instance (NFDataX (f a), NFDataX (g a)) => NFDataX (Product f g a)
instance (NFDataX (f a), NFDataX (g a)) => NFDataX (Sum f g a)
instance (NFDataX (f (g a))) => NFDataX (Compose f g a)

instance NFDataX Char where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

instance NFDataX Double where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

instance NFDataX Float where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

instance NFDataX Int where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

instance NFDataX Int8 where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

instance NFDataX Int16 where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

instance NFDataX Int32 where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

instance NFDataX Int64 where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

instance NFDataX Integer where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

instance NFDataX Natural where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

instance NFDataX Word where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

instance NFDataX Word8 where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

instance NFDataX Word16 where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

instance NFDataX Word32 where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

instance NFDataX Word64 where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

instance NFDataX CUShort where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

instance NFDataX Half where
  deepErrorX = errorX
  rnfX = rwhnfX
  hasUndefined = isLeft . isX
  ensureSpine = id

instance NFDataX a => NFDataX (Seq a) where
  deepErrorX = errorX
  rnfX s =
    if isLeft (isX s) then () else go s
   where
    go Empty = ()
    go (x :<| xs) = rnfX x `seq` go xs
  ensureSpine = id

  hasUndefined s =
    if isLeft (isX s) then True else go s
   where
    go Empty = False
    go (x :<| xs) = hasUndefined x || hasUndefined xs

instance NFDataX a => NFDataX (Ratio a) where
  deepErrorX = errorX
  rnfX r = rnfX (numerator r) `seq` rnfX (denominator r)
  hasUndefined r = isLeft (isX (numerator r)) || isLeft (isX (denominator r))
  ensureSpine = id

instance NFDataX a => NFDataX (Complex a) where
  deepErrorX = errorX

instance (NFDataX a, NFDataX b) => NFDataX (SG.Arg a b)
instance NFDataX (SG.All)
instance NFDataX (SG.Any)
instance NFDataX a => NFDataX (SG.Dual a)
instance NFDataX a => NFDataX (SG.Endo a)
instance NFDataX a => NFDataX (SG.First a)
instance NFDataX a => NFDataX (SG.Last a)
instance NFDataX a => NFDataX (SG.Max a)
instance NFDataX a => NFDataX (SG.Min a)
instance NFDataX a => NFDataX (SG.Product a)
instance NFDataX a => NFDataX (SG.Sum a)
instance NFDataX a => NFDataX (M.First a)
instance NFDataX a => NFDataX (M.Last a)

-- | __NB__: The documentation only shows instances up to /3/-tuples. By
-- default, instances up to and including /12/-tuples will exist. If the flag
-- @large-tuples@ is set instances up to the GHC imposed limit will exist. The
-- GHC imposed limit is either 62 or 64 depending on the GHC version.
mkShowXTupleInstances [2..maxTupleSize]

-- | __NB__: The documentation only shows instances up to /3/-tuples. By
-- default, instances up to and including /12/-tuples will exist. If the flag
-- @large-tuples@ is set instances up to the GHC imposed limit will exist. The
-- GHC imposed limit is either 62 or 64 depending on the GHC version.
mkNFDataXTupleInstances [2..maxTupleSize]

-- | Call to 'errorX' with default string
undefined :: HasCallStack => a
undefined = errorX "undefined"

-- | Same as 'Data.Maybe.fromJust', but returns a bottom/undefined value that
-- other Clash constructs are aware of.
fromJustX :: (HasCallStack, NFDataX a) => Maybe a -> a
fromJustX Nothing = deepErrorX "fromJustX: Nothing"
fromJustX (Just a) = a
