{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Assortment of utility function used in the Clash library
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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

import Control.Arrow                  as X ((***),(&&&),first,second)
import qualified Control.Exception    as Exception
import Control.Lens
import Control.Monad                  as X ((<=<),(>=>))
import Control.Monad.State            (MonadState,StateT)
import qualified Control.Monad.State  as State
import Data.Function                  as X (on)
import Data.Hashable                  (Hashable)
import Data.HashMap.Lazy              (HashMap)
import qualified Data.HashMap.Lazy    as HashMapL
import qualified Data.List.Extra      as List
import Data.Maybe                     (fromMaybe, listToMaybe, catMaybes)
import Data.Map.Ordered               (OMap)
import qualified Data.Map.Ordered     as OMap

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
import Prettyprinter.Render.String
#else
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
#endif

import Data.Time.Clock                (UTCTime)
import qualified Data.Time.Clock      as Clock
import qualified Data.Time.Format     as Clock
import Data.Typeable                  (Typeable)
import Data.Version                   (Version)
import GHC.Base                       (Int(..),isTrue#,(==#),(+#))
import GHC.Integer.Logarithms         (integerLogBase#)
import qualified GHC.LanguageExtensions.Type as LangExt
import GHC.Stack                      (HasCallStack, callStack, prettyCallStack)
import Type.Reflection                (tyConPackage, typeRepTyCon, typeOf)
import qualified Language.Haskell.TH  as TH

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.SrcLoc               (SrcSpan, noSrcSpan)
#else
import SrcLoc                         (SrcSpan, noSrcSpan)
#endif

import Clash.Debug
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
  :: HasCallStack
  => Bool
  -- ^ Trigger warning?
  -> String
  -- ^ File name
  -> Int
  -- ^ Line number
  -> Doc ann
  -- ^ Message
  -> a
  -- ^ Pass value (like trace)
  -> a
warnPprTrace _     _ _ _ x | not debugIsOn = x
warnPprTrace False _ _ _ x = x
warnPprTrace True  file ln msg x =
  pprDebugAndThen trace (vcat [heading0, heading1]) msg x
 where
  heading0 = hsep ["WARNING: file", pretty file <> comma, "line", pretty ln]
  heading1 = "WARNING CALLSTACK:" <> line <> pretty (prettyCallStack callStack)

pprTrace
  :: String -> Doc ann -> a -> a
pprTrace str = pprDebugAndThen trace (pretty str)

pprTraceDebug
  :: String -> Doc ann -> a -> a
pprTraceDebug str doc x
  | debugIsOn = pprTrace str doc x
  | otherwise = x

pprDebugAndThen
  :: (String -> a) -> Doc ann -> Doc ann -> a
pprDebugAndThen cont heading prettyMsg =
  cont (renderString (layoutPretty defaultLayoutOptions doc))
 where
  doc = sep [heading, nest 2 prettyMsg]

-- | A class that can generate unique numbers
class Monad m => MonadUnique m where
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

-- | Cache the result of a monadic action using a 'OMap'
makeCachedO
  :: (MonadState s m, Uniquable k)
  => k
  -- ^ Key the action is associated with
  -> Lens' s (OMap Unique v)
  -- ^ Lens to the cache
  -> m v
  -- ^ Action to cache
  -> m v
makeCachedO key l create = do
  cache <- use l
  case OMap.lookup (getUnique key) cache of
    Just value -> return value
    Nothing -> do
      value <- create
      l %= (flip (OMap.|>)) (getUnique key, value)
      return value

combineM :: (Applicative f)
         => (a -> f b)
         -> (c -> f d)
         -> (a,c)
         -> f (b,d)
combineM f g (x,y) = (,) <$> f x <*> g y

-- | Same as 'indexNote' with last two arguments swapped
indexNote'
  :: HasCallStack
  => String
  -- ^ Error message to display
  -> Int
  -- ^ Index /n/
  -> [a]
  -- ^ List to index
  -> a
  -- ^ Error or element /n/
indexNote' = flip . indexNote

-- | Unsafe indexing, return a custom error message when indexing fails
indexNote
  :: HasCallStack
  => String
  -- ^ Error message to display
  -> [a]
  -- ^ List to index
  -> Int
  -- ^ Index /n/
  -> a
  -- ^ Error or element /n/
indexNote note = \xs i -> fromMaybe (error note) (List.indexMaybe xs i)

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

-- | Get the package id of the type of a value
-- >>> pkgIdFromTypeable (undefined :: TopEntity)
-- "clash-prelude-0.99.3-64904d90747cb49e17166bbc86fec8678918e4ead3847193a395b258e680373c"
pkgIdFromTypeable :: Typeable a => a -> String
pkgIdFromTypeable = tyConPackage . typeRepTyCon . typeOf

reportTimeDiff :: UTCTime -> UTCTime -> String
reportTimeDiff end start
  | diff >= Clock.nominalDay = show days <> "d" <> Clock.formatTime Clock.defaultTimeLocale fmt
    (Clock.UTCTime (toEnum 0) (fromRational (toRational hms)))
  | otherwise = Clock.formatTime Clock.defaultTimeLocale fmt
    (Clock.UTCTime (toEnum 0) (fromRational (toRational diff)))
 where
  diff = Clock.diffUTCTime end start
  (days,hms) = divMod @Integer (floor diff) (floor Clock.nominalDay)
  fmt  | diff >= 3600
       = "%-Hh%-Mm%-Ss"
       | diff >= 60
       = "%-Mm%-Ss"
       | otherwise
       = "%-S%03Qs"

-- | Left-biased choice on maybes
orElse :: Maybe a -> Maybe a -> Maybe a
orElse x@(Just _) _y = x
orElse _x y = y

-- | Left-biased choice on maybes
orElses :: [Maybe a] -> Maybe a
orElses = listToMaybe . catMaybes

-- These language extensions are used for
--  * the interactive session inside clashi
--  * compiling files with clash
--  * running output tests with runghc
--  * compiling (local) Template/Blackbox functions with Hint
wantedLanguageExtensions :: [LangExt.Extension]
wantedLanguageExtensions =
  [ LangExt.BinaryLiterals
  , LangExt.ConstraintKinds
  , LangExt.DataKinds
  , LangExt.DeriveAnyClass
  , LangExt.DeriveGeneric
  , LangExt.DeriveLift
  , LangExt.DerivingStrategies
  , LangExt.ExplicitForAll
  , LangExt.ExplicitNamespaces
  , LangExt.FlexibleContexts
  , LangExt.FlexibleInstances
  , LangExt.KindSignatures
  , LangExt.MagicHash
  , LangExt.MonoLocalBinds
  , LangExt.QuasiQuotes
  , LangExt.ScopedTypeVariables
  , LangExt.TemplateHaskell
  , LangExt.TemplateHaskellQuotes
  , LangExt.TypeApplications
  , LangExt.TypeFamilies
  , LangExt.TypeOperators
#if !MIN_VERSION_ghc(8,6,0)
  , LangExt.TypeInType
#endif
  ]

unwantedLanguageExtensions :: [LangExt.Extension]
unwantedLanguageExtensions =
  [ LangExt.ImplicitPrelude
#if MIN_VERSION_ghc(8,6,0)
  , LangExt.StarIsType
#endif
  , LangExt.Strict
  , LangExt.StrictData
  ]
