{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Assortment of utility function used in the Clash library
-}

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TupleSections        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Clash.Util
  ( module Clash.Util
  , module X
  , makeLenses
  , SrcSpan
  , noSrcSpan
  , HasCallStack
  )
where

import Control.Applicative            as X (Applicative,(<$>),(<*>),pure)
import Control.Arrow                  as X ((***),(&&&),first,second)
import qualified Control.Exception    as Exception
import Control.Monad                  as X ((<=<),(>=>))
import Control.Monad.State            (MonadState,State,StateT,runState)
import qualified Control.Monad.State  as State
import Data.Function                  as X (on)
import Data.Hashable                  (Hashable)
import Data.HashMap.Lazy              (HashMap)
import qualified Data.HashMap.Lazy    as HashMapL
import Data.Maybe                     (fromMaybe)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Data.Version                   (Version)
import Control.Lens
import Debug.Trace                    (trace)
import GHC.Base                       (Int(..),isTrue#,(==#),(+#))
import GHC.Integer.Logarithms         (integerLogBase#)
import GHC.Stack                      (HasCallStack, callStack, prettyCallStack)
import qualified Language.Haskell.TH  as TH

import SrcLoc                         (SrcSpan, noSrcSpan)
import Clash.Unique

#ifdef CABAL
import qualified Paths_clash_lib      (version)
#endif

data ClashException = ClashException SrcSpan String (Maybe String)

instance Show ClashException where
  show (ClashException _ s eM) = s ++ "\n" ++ maybe "" id eM

instance Exception.Exception ClashException

assertPanic
  :: String -> Int -> a
assertPanic file ln = Exception.throw
  (ClashException noSrcSpan ("ASSERT failed! file " ++ file ++ ", line " ++ show ln) Nothing)

assertPprPanic
  :: HasCallStack => String -> Int -> Doc ann -> a
assertPprPanic _file _line msg = pprPanic "ASSERT failed!" doc
 where
  doc = sep [ msg, callStackDoc ]

pprPanic
  :: String -> Doc ann -> a
pprPanic heading prettyMsg = Exception.throw
  (ClashException noSrcSpan (renderString (layoutPretty defaultLayoutOptions doc)) Nothing)
 where
  doc = sep [pretty heading, nest 2 prettyMsg]

callStackDoc
  :: HasCallStack => Doc ann
callStackDoc =
  "Call stack:" <+> hang 4
    (vcat (map pretty (lines (prettyCallStack callStack))))

warnPprTrace
  :: Bool -> String -> Int -> Doc ann -> a -> a
warnPprTrace _     _ _ _ x | not debugIsOn = x
warnPprTrace False _ _ _ x = x
warnPprTrace True  file ln msg x =
  pprDebugAndThen trace heading msg x
 where
  heading = hsep ["WARNING: file", pretty file <> comma, "line", pretty ln]

pprTrace
  :: String -> Doc ann -> a -> a
pprTrace str = pprDebugAndThen trace (pretty str)

pprTraceDebug
  :: String -> Doc ann -> a -> a
pprTraceDebug str doc x
  | debugIsOn = pprDebugAndThen trace (pretty str) doc x
  | otherwise = x

pprDebugAndThen
  :: (String -> a) -> Doc ann -> Doc ann -> a
pprDebugAndThen cont heading prettyMsg =
  cont (renderString (layoutPretty defaultLayoutOptions doc))
 where
  doc = sep [heading, nest 2 prettyMsg]

-- | A class that can generate unique numbers
class MonadUnique m where
  -- | Get a new unique
  getUniqueM :: m Int

instance Monad m => MonadUnique (StateT Int m) where
  getUniqueM = do
    supply <- State.get
    State.modify (+1)
    return supply

-- | Create a TH expression that returns the a formatted string containing the
-- name of the module 'curLoc' is spliced into, and the line where it was spliced.
curLoc :: TH.Q TH.Exp
curLoc = do
  (TH.Loc _ _ modName (startPosL,_) _) <- TH.location
  TH.litE (TH.StringL $ modName ++ "(" ++ show startPosL ++ "): ")

-- | Cache the result of a monadic action
makeCached :: (MonadState s m, Hashable k, Eq k)
           => k -- ^ The key the action is associated with
           -> Lens' s (HashMap k v) -- ^ The Lens to the HashMap that is the cache
           -> m v -- ^ The action to cache
           -> m v
makeCached key l create = do
  cache <- use l
  case HashMapL.lookup key cache of
    Just value -> return value
    Nothing -> do
      value <- create
      l %= HashMapL.insert key value
      return value

-- | Cache the result of a monadic action using a 'UniqMap'
makeCachedU
  :: (MonadState s m, Uniquable k)
  => k
  -- ^ Key the action is associated with
  -> Lens' s (UniqMap v)
  -- ^ Lens to the cache
  -> m v
  -- ^ Action to cache
  -> m v
makeCachedU key l create = do
  cache <- use l
  case lookupUniqMap key cache of
    Just value -> return value
    Nothing -> do
      value <- create
      l %= extendUniqMap key value
      return value

-- | Run a State-action using the State that is stored in a higher-layer Monad
liftState :: (MonadState s m)
          => Lens' s s' -- ^ Lens to the State in the higher-layer monad
          -> State s' a -- ^ The State-action to perform
          -> m a
liftState l m = do
  s <- use l
  let (a,s') = runState m s
  l .= s'
  return a

-- | Functorial version of 'Control.Arrow.first'
firstM :: Functor f
       => (a -> f c)
       -> (a, b)
       -> f (c, b)
firstM f (x,y) = (,y) <$> f x

-- | Functorial version of 'Control.Arrow.second'
secondM :: Functor f
        => (b -> f c)
        -> (a, b)
        -> f (a, c)
secondM f (x,y) = (x,) <$> f y

combineM :: (Applicative f)
         => (a -> f b)
         -> (c -> f d)
         -> (a,c)
         -> f (b,d)
combineM f g (x,y) = (,) <$> f x <*> g y

-- | Performs trace when first argument evaluates to 'True'
traceIf :: Bool -> String -> a -> a
traceIf True  msg = trace msg
traceIf False _   = id

-- | Monadic version of 'Data.List.partition'
partitionM :: Monad m
           => (a -> m Bool)
           -> [a]
           -> m ([a], [a])
partitionM _ []     = return ([], [])
partitionM p (x:xs) = do
  test      <- p x
  (ys, ys') <- partitionM p xs
  return $ if test then (x:ys, ys') else (ys, x:ys')

-- | Monadic version of 'Data.List.mapAccumL'
mapAccumLM :: (Monad m)
           => (acc -> x -> m (acc,y))
           -> acc
           -> [x]
           -> m (acc,[y])
mapAccumLM _ acc [] = return (acc,[])
mapAccumLM f acc (x:xs) = do
  (acc',y) <- f acc x
  (acc'',ys) <- mapAccumLM f acc' xs
  return (acc'',y:ys)

-- | if-then-else as a function on an argument
ifThenElse :: (a -> Bool)
           -> (a -> b)
           -> (a -> b)
           -> a
           -> b
ifThenElse t f g a = if t a then f a else g a

infixr 5 <:>
-- | Applicative version of 'GHC.Types.(:)'
(<:>) :: Applicative f
      => f a
      -> f [a]
      -> f [a]
x <:> xs = (:) <$> x <*> xs

-- | Safe indexing, returns a 'Nothing' if the index does not exist
indexMaybe :: [a]
           -> Int
           -> Maybe a
indexMaybe [] _     = Nothing
indexMaybe (x:_)  0 = Just x
indexMaybe (_:xs) n = indexMaybe xs (n-1)

-- | Unsafe indexing, return a custom error message when indexing fails
indexNote :: String
          -> [a]
          -> Int
          -> a
indexNote note = \xs i -> fromMaybe (error note) (indexMaybe xs i)

-- | Split the second list at the length of the first list
splitAtList :: [b] -> [a] -> ([a], [a])
splitAtList [] xs         = ([], xs)
splitAtList _ xs@[]       = (xs, xs)
splitAtList (_:xs) (y:ys) = (y:ys', ys'')
    where
      (ys', ys'') = splitAtList xs ys

clashLibVersion :: Version
#ifdef CABAL
clashLibVersion = Paths_clash_lib.version
#else
clashLibVersion = error "development version"
#endif

-- | \x y -> floor (logBase x y), x > 1 && y > 0
flogBase :: Integer -> Integer -> Maybe Int
flogBase x y | x > 1 && y > 0 = Just (I# (integerLogBase# x y))
flogBase _ _ = Nothing

-- | \x y -> ceiling (logBase x y), x > 1 && y > 0
clogBase :: Integer -> Integer -> Maybe Int
clogBase x y | x > 1 && y > 0 =
  case y of
    1 -> Just 0
    _ -> let z1 = integerLogBase# x y
             z2 = integerLogBase# x (y-1)
         in  if isTrue# (z1 ==# z2)
                then Just (I# (z1 +# 1#))
                else Just (I# z1)
clogBase _ _ = Nothing

-- | Determine whether two lists are of equal length
equalLength
  :: [a] -> [b] -> Bool
equalLength [] [] = True
equalLength (_:as) (_:bs) = equalLength as bs
equalLength _ _ = False

-- | Determine whether two lists are not of equal length
neLength
  :: [a] -> [b] -> Bool
neLength [] [] = False
neLength (_:as) (_:bs) = neLength as bs
neLength _ _ = True

-- | Zip two lists of equal length
--
-- NB Errors out for a DEBUG compiler when the two lists are not of equal length
zipEqual
  :: [a] -> [b] -> [(a,b)]
#if !defined(DEBUG)
zipEqual = zip
#else
zipEqual [] [] = []
zipEqual (a:as) (b:bs) = (a,b) : zipEqual as bs
zipEqual _ _ = error "zipEqual"
#endif

-- | Is this a DEBUG compiler?
debugIsOn
  :: Bool
#if defined(DEBUG)
debugIsOn = True
#else
debugIsOn = False
#endif
