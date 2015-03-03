#ifndef HSVERSIONS_H
#define HSVERSIONS_H

/* Global variables may not work in other Haskell implementations,
 * but we need them currently! so the conditional on GLASGOW won't do. */
#if defined(__GLASGOW_HASKELL__) || !defined(__GLASGOW_HASKELL__)
#define GLOBAL_VAR(name,value,ty)  \
{-# NOINLINE name #-};             \
name :: IORef (ty);                \
name = Util.global (value);

#define GLOBAL_VAR_M(name,value,ty) \
{-# NOINLINE name #-};              \
name :: IORef (ty);                 \
name = Util.globalM (value);
#endif

#define ASSERT(e)      if debugIsOn && not (e) then (assertPanic __FILE__ __LINE__) else
#define ASSERT2(e,msg) if debugIsOn && not (e) then (assertPprPanic __FILE__ __LINE__ (msg)) else
#define WARN( e, msg ) (warnPprTrace (e) __FILE__ __LINE__ (msg)) $

-- Examples:   Assuming   flagSet :: String -> m Bool
--
--    do { c   <- getChar; MASSERT( isUpper c ); ... }
--    do { c   <- getChar; MASSERT2( isUpper c, text "Bad" ); ... }
--    do { str <- getStr;  ASSERTM( flagSet str ); .. }
--    do { str <- getStr;  ASSERTM2( flagSet str, text "Bad" ); .. }
--    do { str <- getStr;  WARNM2( flagSet str, text "Flag is set" ); .. }
#define MASSERT(e)      ASSERT(e) return ()
#define MASSERT2(e,msg) ASSERT2(e,msg) return ()
#define ASSERTM(e)      do { bool <- e; MASSERT(bool) }
#define ASSERTM2(e,msg) do { bool <- e; MASSERT2(bool,msg) }
#define WARNM2(e,msg)   do { bool <- e; WARN(bool, msg) return () }

-- Useful for declaring arguments to be strict
#define STRICT1(f) f a | a `seq` False = undefined
#define STRICT2(f) f a b | a `seq` b `seq` False = undefined
#define STRICT3(f) f a b c | a `seq` b `seq` c `seq` False = undefined
#define STRICT4(f) f a b c d | a `seq` b `seq` c `seq` d `seq` False = undefined
#define STRICT5(f) f a b c d e | a `seq` b `seq` c `seq` d `seq` e `seq` False = undefined
#define STRICT6(f) f a b c d e f | a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` False = undefined

#endif /* HsVersions.h */

