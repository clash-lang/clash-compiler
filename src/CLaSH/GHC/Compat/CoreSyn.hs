{-# LANGUAGE CPP #-}
module CLaSH.GHC.Compat.CoreSyn
  (dfunArgExprs)
where

import qualified CoreSyn

#if __GLASGOW_HASKELL__ >= 706
dfunArgExprs :: [CoreSyn.DFunArg e] -> [e]
dfunArgExprs [] = []
dfunArgExprs (CoreSyn.DFunPolyArg e:es) = e : dfunArgExprs es
dfunArgExprs (CoreSyn.DFunLamArg _:es)  = undefined : dfunArgExprs es
#else
dfunArgExprs :: [e] -> [e]
dfunArgExprs = id
#endif
