{-|
Copyright  :  (C) 2019     , Myrtle Software Ltd,
                  2023     , QBayLogic B.V.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.CPP
 ( maxTupleSize
 , haddockOnly

 -- ** Cabal flags
 , fSuperStrict
 , fStrictMapSignal
 ) where

#ifndef MAX_TUPLE_SIZE
#ifdef LARGE_TUPLES

#if MIN_VERSION_ghc(9,0,0)
import GHC.Settings.Constants (mAX_TUPLE_SIZE)
#else
import Constants (mAX_TUPLE_SIZE)
#endif
#define MAX_TUPLE_SIZE (fromIntegral mAX_TUPLE_SIZE)

#else
#ifdef HADDOCK_ONLY
#define MAX_TUPLE_SIZE 3
#else
#define MAX_TUPLE_SIZE 12
#endif
#endif
#endif

maxTupleSize :: Num a => a
maxTupleSize = MAX_TUPLE_SIZE

haddockOnly :: Bool
#ifdef HADDOCK_ONLY
haddockOnly = True
#else
haddockOnly = False
#endif

-- | Whether clash-prelude was compiled with -fsuper-strict
fSuperStrict :: Bool
#ifdef CLASH_SUPER_STRICT
fSuperStrict = True
#else
fSuperStrict = False
#endif
{-# INLINE fSuperStrict #-}

-- | Whether clash-prelude was compiled with -fstrict-mapSignal
fStrictMapSignal :: Bool
#ifdef CLASH_STRICT_MAPSIGNAL
fStrictMapSignal = True
#else
fStrictMapSignal = False
#endif
{-# INLINE fStrictMapSignal #-}
