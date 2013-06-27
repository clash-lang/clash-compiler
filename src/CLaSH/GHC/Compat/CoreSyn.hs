{-# LANGUAGE CPP #-}
module CLaSH.GHC.Compat.CoreSyn
  (dfunArgExprs)
where

#if __GLASGOW_HASKELL__ < 707
import Data.List (mapAccumL)

import CoreSyn    (CoreExpr,DFunArg(..),Expr(Var))
import FastString (mkFastString)
import Id         (mkLocalId)
import MkCore     (mkCoreLams)
import Name       (mkSystemVarName)
import TcType     (tcSplitForAllTys,tcView)
import TypeRep    (Type(FunTy))
import UniqSupply (UniqSupply,takeUniqFromSupply)
#else
import Type       (Type)
import UniqSupply (UniqSupply)
#endif

#if __GLASGOW_HASKELL__ < 707
dfunArgExprs :: UniqSupply -> Type -> [DFunArg CoreExpr] -> ([CoreExpr],UniqSupply)
dfunArgExprs uniqSupply dfunty es = (map dfunArgToE es,uniqSupply')
  where
    (tvs,dfunty') = tcSplitForAllTys dfunty
    dfunArgTys    = splitDfunArgs dfunty'

    splitDfunArgs ty | Just ty' <- tcView ty = splitDfunArgs ty'
    splitDfunArgs (FunTy arg ty)             = arg : splitDfunArgs ty
    splitDfunArgs _                          = []

    (uniqSupply',dfunArgIds) = mapAccumL mkId uniqSupply dfunArgTys
    dfunVars                 = tvs ++ dfunArgIds

    dfunArgToE (DFunPolyArg e) = e
    dfunArgToE (DFunLamArg i)  = mkCoreLams dfunVars (Var (dfunVars !! i))

    mkId us t  = (us',id_)
      where
        (u,us') = takeUniqFromSupply us
        n       = mkSystemVarName u (mkFastString "x")
        id_     = mkLocalId n t
#else
dfunArgExprs :: UniqSupply -> Type -> [e] -> ([e],UniqSupply)
dfunArgExprs us _ es = (es,us)
#endif
