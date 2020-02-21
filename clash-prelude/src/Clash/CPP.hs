{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}

{-# OPTIONS_HADDOCK hide #-}

#ifndef MAX_TUPLE_SIZE
#ifdef LARGE_TUPLES
#define MAX_TUPLE_SIZE 62
#else
#define MAX_TUPLE_SIZE 12
#endif
#endif

module Clash.CPP
 ( maxTupleSize

 -- ** Cabal flags
 , fSuperStrict
 , fStrictMapSignal
 ) where

maxTupleSize :: Num a => a
maxTupleSize = MAX_TUPLE_SIZE

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
