{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Smart constructor and destructor functions for CoreHW
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Core.Util where

import Control.Monad.Trans.Except              (Except, throwE)
import qualified Data.HashMap.Strict           as HMS
import qualified Data.HashMap.Lazy             as HashMap
import Data.HashMap.Lazy                       (HashMap)
import qualified Data.HashSet                  as HashSet
import Data.Maybe                              (fromJust, mapMaybe)
import Unbound.Generics.LocallyNameless
  (Fresh, bind, embed, rebind, unbind, unembed, unrebind, unrec)
import Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import Clash.Core.DataCon                      (DataCon, dcType, dataConInstArgTys)
import Clash.Core.Literal                      (literalType)
import Clash.Core.Name
  (Name (..), name2String, string2SystemName)
import Clash.Core.Pretty                       (showDoc)
import Clash.Core.Term
  (LetBinding, Pat (..), Term (..), TmName, TmOccName)
import Clash.Core.Type
  (Kind, LitTy (..), TyName, TyOccName, Type (..), TypeView (..), applyTy,
   coreView, isFunTy, isPolyFunCoreTy, mkFunTy, splitFunTy, tyView)
import Clash.Core.TyCon
  (TyCon (..), TyConOccName, tyConDataCons)
import Clash.Core.TysPrim                      (typeNatKind)
import Clash.Core.Var                          (Id, TyVar, Var (..), varType)
import Clash.Util

-- | Type environment/context
type Gamma = HashMap TmOccName Type
-- | Kind environment/context
type Delta = HashMap TyOccName Kind

-- | Determine the type of a term
termType :: Fresh m
         => HashMap TyConOccName TyCon
         -> Term
         -> m Type
termType m e = case e of
  Var t _        -> return t
  Data dc        -> return $ dcType dc
  Literal l      -> return $ literalType l
  Prim _ t       -> return t
  Lam b          -> do (v,e') <- unbind b
                       mkFunTy (unembed $ varType v) <$> termType m e'
  TyLam b        -> do (tv,e') <- unbind b
                       ForAllTy <$> bind tv <$> termType m e'
  App _ _        -> case collectArgs e of
                      (fun, args) -> termType m fun >>=
                                     (flip (applyTypeToArgs m) args)
  TyApp e' ty    -> termType m e' >>= (\f -> applyTy m f ty)
  Letrec b       -> do (_,e') <- unbind b
                       termType m e'
  Case _ ty _    -> return ty
  Cast _ _ ty2   -> return ty2

-- | Split a (Type)Application in the applied term and it arguments
collectArgs :: Term
            -> (Term, [Either Term Type])
collectArgs = go []
  where
    go args (App e1 e2) = go (Left e2:args) e1
    go args (TyApp e t) = go (Right t:args) e
    go args e           = (e, args)

-- | Split a (Type)Abstraction in the bound variables and the abstracted term
collectBndrs :: Fresh m
             => Term
             -> m ([Either Id TyVar], Term)
collectBndrs = go []
  where
    go bs (Lam b) = do
      (v,e') <- unbind b
      go (Left v:bs) e'
    go bs (TyLam b) = do
      (tv,e') <- unbind b
      go (Right tv:bs) e'
    go bs e' = return (reverse bs,e')

-- | Get the result type of a polymorphic function given a list of arguments
applyTypeToArgs :: Fresh m
                => HashMap TyConOccName TyCon
                -> Type
                -> [Either Term Type]
                -> m Type
applyTypeToArgs _ opTy []              = return opTy
applyTypeToArgs m opTy (Right ty:args) = applyTy m opTy ty >>=
                                          (flip (applyTypeToArgs m) args)
applyTypeToArgs m opTy (Left e:args)   = case splitFunTy m opTy of
  Just (_,resTy) -> applyTypeToArgs m resTy args
  Nothing        -> error $
                    concat [ $(curLoc)
                           , "applyTypeToArgs splitFunTy: not a funTy:\n"
                           , "opTy: "
                           , showDoc opTy
                           , "\nTerm: "
                           , showDoc e
                           , "\nOtherArgs: "
                           , unlines (map (either showDoc showDoc) args)
                           ]

-- | Get the list of term-binders out of a DataType pattern
patIds :: Pat -> ([TyVar],[Id])
patIds (DataPat _ ids) = unrebind ids
patIds _               = ([],[])

-- | Make a type variable
mkTyVar :: Kind
        -> TyName
        -> TyVar
mkTyVar tyKind tyName = TyVar tyName (embed tyKind)

-- | Make a term variable
mkId :: Type
     -> TmName
     -> Id
mkId tmType tmName = Id tmName (embed tmType)

-- | Abstract a term over a list of term and type variables
mkAbstraction :: Term
              -> [Either Id TyVar]
              -> Term
mkAbstraction = foldr (either (Lam `dot` bind) (TyLam `dot` bind))

-- | Abstract a term over a list of term variables
mkTyLams :: Term
         -> [TyVar]
         -> Term
mkTyLams tm = mkAbstraction tm . map Right

-- | Abstract a term over a list of type variables
mkLams :: Term
       -> [Id]
       -> Term
mkLams tm = mkAbstraction tm . map Left

-- | Apply a list of types and terms to a term
mkApps :: Term
       -> [Either Term Type]
       -> Term
mkApps = foldl (\e a -> either (App e) (TyApp e) a)

-- | Apply a list of terms to a term
mkTmApps :: Term
         -> [Term]
         -> Term
mkTmApps = foldl App

-- | Apply a list of types to a term
mkTyApps :: Term
         -> [Type]
         -> Term
mkTyApps = foldl TyApp

-- | Does a term have a function type?
isFun :: Fresh m
      => HashMap TyConOccName TyCon
      -> Term
      -> m Bool
isFun m t = fmap (isFunTy m) $ (termType m) t

-- | Does a term have a function or polymorphic type?
isPolyFun :: Fresh m
          => HashMap TyConOccName TyCon
          -> Term
          -> m Bool
isPolyFun m t = isPolyFunCoreTy m <$> termType m t

-- | Is a term a term-abstraction?
isLam :: Term
      -> Bool
isLam (Lam _) = True
isLam _       = False

-- | Is a term a recursive let-binding?
isLet :: Term
      -> Bool
isLet (Letrec _) = True
isLet _          = False

-- | Is a term a variable reference?
isVar :: Term
      -> Bool
isVar (Var _ _) = True
isVar _         = False

-- | Is a term a datatype constructor?
isCon :: Term
      -> Bool
isCon (Data _) = True
isCon _        = False

-- | Is a term a primitive?
isPrim :: Term
       -> Bool
isPrim (Prim _ _) = True
isPrim _          = False

-- | Make variable reference out of term variable
idToVar :: Id
        -> Term
idToVar (Id nm tyE) = Var (unembed tyE) nm
idToVar tv          = error $ $(curLoc) ++ "idToVar: tyVar: " ++ showDoc tv

-- | Make a term variable out of a variable reference
varToId :: Term
        -> Id
varToId (Var ty nm) = Id nm (embed ty)
varToId e           = error $ $(curLoc) ++ "varToId: not a var: " ++ showDoc e

termSize :: Term
         -> Word
termSize (Var _ _)   = 1
termSize (Data _)    = 1
termSize (Literal _) = 1
termSize (Prim _ _)  = 1
termSize (Lam b)     = let (_,e) = unsafeUnbind b
                       in  termSize e + 1
termSize (TyLam b)   = let (_,e) = unsafeUnbind b
                       in  termSize e
termSize (App e1 e2) = termSize e1 + termSize e2
termSize (TyApp e _) = termSize e
termSize (Cast e _ _)= termSize e
termSize (Letrec b)  = let (bndrsR,body) = unsafeUnbind b
                           bndrSzs       = map (termSize . unembed . snd) (unrec bndrsR)
                           bodySz        = termSize body
                       in sum (bodySz:bndrSzs)
termSize (Case subj _ alts) = let subjSz = termSize subj
                                  altSzs = map (termSize . snd . unsafeUnbind) alts
                              in  sum (subjSz:altSzs)

-- | Create a vector of supplied elements
mkVec :: DataCon -- ^ The Nil constructor
      -> DataCon -- ^ The Cons (:>) constructor
      -> Type    -- ^ Element type
      -> Integer -- ^ Length of the vector
      -> [Term]  -- ^ Elements to put in the vector
      -> Term
mkVec nilCon consCon resTy = go
  where
    go _ [] = mkApps (Data nilCon) [Right (LitTy (NumTy 0))
                                   ,Right resTy
                                   ,Left  (Prim "_CO_" nilCoTy)
                                   ]

    go n (x:xs) = mkApps (Data consCon) [Right (LitTy (NumTy n))
                                        ,Right resTy
                                        ,Right (LitTy (NumTy (n-1)))
                                        ,Left (Prim "_CO_" (consCoTy n))
                                        ,Left x
                                        ,Left (go (n-1) xs)]

    nilCoTy    = head (fromJust $! dataConInstArgTys nilCon  [(LitTy (NumTy 0))
                                                             ,resTy])
    consCoTy n = head (fromJust $! dataConInstArgTys consCon
                                                     [(LitTy (NumTy n))
                                                     ,resTy
                                                     ,(LitTy (NumTy (n-1)))])

-- | Append elements to the supplied vector
appendToVec :: DataCon -- ^ The Cons (:>) constructor
            -> Type    -- ^ Element type
            -> Term    -- ^ The vector to append the elements to
            -> Integer -- ^ Length of the vector
            -> [Term]  -- ^ Elements to append
            -> Term
appendToVec consCon resTy vec = go
  where
    go _ []     = vec
    go n (x:xs) = mkApps (Data consCon) [Right (LitTy (NumTy n))
                                        ,Right resTy
                                        ,Right (LitTy (NumTy (n-1)))
                                        ,Left (Prim "_CO_" (consCoTy n))
                                        ,Left x
                                        ,Left (go (n-1) xs)]

    consCoTy n = head (fromJust $! dataConInstArgTys consCon
                                                   [(LitTy (NumTy n))
                                                   ,resTy
                                                   ,(LitTy (NumTy (n-1)))])

-- | Create let-bindings with case-statements that select elements out of a
-- vector. Returns both the variables to which element-selections are bound
-- and the let-bindings
extractElems :: DataCon -- ^ The Cons (:>) constructor
             -> Type    -- ^ The element type
             -> Char    -- ^ Char to append to the bound variable names
             -> Integer -- ^ Length of the vector
             -> Term    -- ^ The vector
             -> [(Term,[LetBinding])]
extractElems consCon resTy s maxN = go maxN
  where
    go :: Integer -> Term -> [(Term,[LetBinding])]
    go 0 _ = []
    go n e = (elVar
             ,[(Id elBNm (embed resTy) ,embed lhs)
              ,(Id restBNm (embed restTy),embed rhs)
              ]
             ) :
             go (n-1) (Var restTy restBNm)

      where
        elBNm     = string2SystemName ("el" ++ s:show (maxN-n))
        restBNm   = string2SystemName ("rest" ++ s:show (maxN-n))
        elVar     = Var resTy elBNm
        pat       = DataPat (embed consCon) (rebind [mTV] [co,el,rest])
        elPatNm   = string2SystemName "el"
        restPatNm = string2SystemName "rest"
        lhs       = Case e resTy  [bind pat (Var resTy  elPatNm)]
        rhs       = Case e restTy [bind pat (Var restTy restPatNm)]

        mName = string2SystemName "m"
        mTV   = TyVar mName (embed typeNatKind)
        tys   = [(LitTy (NumTy n)),resTy,(LitTy (NumTy (n-1)))]
        (Just idTys) = dataConInstArgTys consCon tys
        [co,el,rest] = zipWith Id [string2SystemName "_co_",elPatNm, restPatNm]
                                  (map embed idTys)
        restTy = last (fromJust (dataConInstArgTys consCon tys))


-- | Create let-bindings with case-statements that select elements out of a
-- tree. Returns both the variables to which element-selections are bound
-- and the let-bindings
extractTElems :: DataCon -- ^ The 'LR' constructor
              -> DataCon -- ^ The 'BR' constructor
              -> Type    -- ^ The element type
              -> Char    -- ^ Char to append to the bound variable names
              -> Integer -- ^ Depth of the tree
              -> Term    -- ^ The tree
              -> ([Term],[LetBinding])
extractTElems lrCon brCon resTy s maxN = go maxN [0..(2^(maxN+1))-2] [0..(2^maxN - 1)]
  where
    go :: Integer -> [Int] -> [Int] -> Term -> ([Term],[LetBinding])
    go 0 _ ks e = ([elVar],[(Id elBNm (embed resTy), embed lhs)])
      where
        elBNm   = string2SystemName ("el" ++ s:show (head ks))
        elVar   = Var resTy elBNm
        pat     = DataPat (embed lrCon) (rebind [] [co,el])
        elPatNm = string2SystemName "el"
        lhs     = Case e resTy [bind pat (Var resTy elPatNm)]

        tys          = [LitTy (NumTy 0),resTy]
        (Just idTys) = dataConInstArgTys lrCon tys
        [co,el]      = zipWith Id [string2SystemName "_co_",elPatNm]
                                  (map embed idTys)

    go n bs ks e = (lVars ++ rVars,(Id ltBNm (embed brTy),embed ltLhs):
                                   (Id rtBNm (embed brTy),embed rtLhs):
                                   (lBinds ++ rBinds))
      where
        ltBNm = string2SystemName ("lt" ++ s:show b0)
        rtBNm = string2SystemName ("rt" ++ s:show b1)
        ltVar = Var brTy ltBNm
        rtVar = Var brTy rtBNm
        pat   = DataPat (embed brCon) (rebind [mTV] [co,lt,rt])
        ltPatNm = string2SystemName "lt"
        rtPatNm = string2SystemName "rt"
        ltLhs   = Case e brTy [bind pat (Var brTy ltPatNm)]
        rtLhs   = Case e brTy [bind pat (Var brTy rtPatNm)]

        mName = string2SystemName "m"
        mTV = TyVar mName (embed typeNatKind)
        tys = [LitTy (NumTy n),resTy,LitTy (NumTy (n-1))]
        (Just idTys) = dataConInstArgTys brCon tys
        [co,lt,rt] = zipWith Id [string2SystemName "_co_",ltPatNm,rtPatNm]
                                (map embed idTys)
        brTy = last idTys
        (kL,kR) = splitAt (length ks `div` 2) ks
        (b0:bL,b1:bR) = splitAt (length bs `div` 2) bs

        (lVars,lBinds) = go (n-1) bL kL ltVar
        (rVars,rBinds) = go (n-1) bR kR rtVar

-- | Create a vector of supplied elements
mkRTree :: DataCon -- ^ The LR constructor
        -> DataCon -- ^ The BR constructor
        -> Type    -- ^ Element type
        -> Integer -- ^ Depth of the tree
        -> [Term]  -- ^ Elements to put in the tree
        -> Term
mkRTree lrCon brCon resTy = go
  where
    go _ [x] = mkApps (Data lrCon) [Right (LitTy (NumTy 0))
                                    ,Right resTy
                                    ,Left  (Prim "_CO_" lrCoTy)
                                    ,Left  x
                                    ]

    go n xs =
      let (xsL,xsR) = splitAt (length xs `div` 2) xs
      in  mkApps (Data brCon) [Right (LitTy (NumTy n))
                              ,Right resTy
                              ,Right (LitTy (NumTy (n-1)))
                              ,Left (Prim "_CO_" (brCoTy n))
                              ,Left (go (n-1) xsL)
                              ,Left (go (n-1) xsR)]

    lrCoTy   = head (fromJust $! dataConInstArgTys lrCon  [(LitTy (NumTy 0))
                                                         ,resTy])
    brCoTy n = head (fromJust $! dataConInstArgTys brCon
                                                   [(LitTy (NumTy n))
                                                   ,resTy
                                                   ,(LitTy (NumTy (n-1)))])

-- | Determine whether a type is isomorphic to "Clash.Signal.Internal.Signal'"
--
-- It is i.e.:
--
--   * Signal' clk a
--   * (Signal' clk a, Signal' clk b)
--   * Vec n (Signal' clk a)
--   * data Wrap = W (Signal clk' Int)
--   * etc.
--
-- This also include BiSignals, i.e.:
--
--   * BiSignalIn High System Int
--   * etc.
--
isSignalType :: HashMap TyConOccName TyCon -> Type -> Bool
isSignalType tcm ty = go HashSet.empty ty
  where
    go tcSeen (tyView -> TyConApp tcNm args) = case name2String tcNm of
      "Clash.Signal.Internal.Signal"      -> True
      "Clash.Signal.BiSignal.BiSignalIn"  -> True
      "Clash.Signal.Internal.BiSignalOut" -> True
      _ | tcNm `HashSet.member` tcSeen    -> False -- Do not follow rec types
        | otherwise -> case HashMap.lookup (nameOcc tcNm) tcm of
            Just tc -> let dcs         = tyConDataCons tc
                           dcInsArgTys = concat
                                       $ mapMaybe (`dataConInstArgTys` args) dcs
                           tcSeen'     = HashSet.insert tcNm tcSeen
                       in  any (go tcSeen') dcInsArgTys
            Nothing -> traceIf True ($(curLoc) ++ "isSignalType: " ++ show tcNm
                                     ++ " not found.") False

    go _ _ = False

isClockOrReset
  :: HashMap TyConOccName TyCon
  -> Type
  -> Bool
isClockOrReset m (coreView m -> Just ty) = isClockOrReset m ty
isClockOrReset _ (tyView -> TyConApp tcNm _) = case name2String tcNm of
  "Clash.Signal.Internal.Clock" -> True
  "Clash.Signal.Internal.Reset" -> True
  _ -> False
isClockOrReset _ _ = False

tyNatSize :: HMS.HashMap TyConOccName TyCon
          -> Type
          -> Except String Integer
tyNatSize m (coreView m -> Just ty) = tyNatSize m ty
tyNatSize _ (LitTy (NumTy i))       = return i
tyNatSize _ ty = throwE $ $(curLoc) ++ "Cannot reduce an integer: " ++ show ty
