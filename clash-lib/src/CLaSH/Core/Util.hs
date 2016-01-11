{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Smart constructor and destructor functions for CoreHW
module CLaSH.Core.Util where

import Control.Monad.Trans.Except              (Except, throwE)
import qualified Data.HashMap.Strict           as HMS
import qualified Data.HashMap.Lazy             as HashMap
import Data.HashMap.Lazy                       (HashMap)
import qualified Data.HashSet                  as HashSet
import Data.Maybe                              (fromJust, mapMaybe)
import Unbound.Generics.LocallyNameless        (Fresh, bind, embed, rebind,
                                                string2Name, unbind, unembed,
                                                unrebind, unrec)
import Unbound.Generics.LocallyNameless.Name   (name2String)
import Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import CLaSH.Core.DataCon                      (DataCon, dcType, dataConInstArgTys)
import CLaSH.Core.Literal                      (literalType)
import CLaSH.Core.Pretty                       (showDoc)
import CLaSH.Core.Term                         (LetBinding, Pat (..), Term (..),
                                                TmName)
import CLaSH.Core.Type                         (Kind, LitTy (..), TyName,
                                                Type (..), TypeView (..), applyTy,
                                                findFunSubst, isFunTy,
                                                isPolyFunCoreTy, mkFunTy,
                                                splitFunTy, tyView)
import CLaSH.Core.TyCon                        (TyCon (..), TyConName,
                                                tyConDataCons)
import CLaSH.Core.TysPrim                      (typeNatKind)
import CLaSH.Core.Var                          (Id, TyVar, Var (..), varType)
import CLaSH.Util

-- | Type environment/context
type Gamma = HashMap TmName Type
-- | Kind environment/context
type Delta = HashMap TyName Kind

-- | Determine the type of a term
termType :: (Functor m, Fresh m)
         => HashMap TyConName TyCon
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
                => HashMap TyConName TyCon
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
patIds :: Pat -> [Id]
patIds (DataPat _ ids) = snd $ unrebind ids
patIds _               = []

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
isFun :: (Functor m, Fresh m)
      => HashMap TyConName TyCon
      -> Term
      -> m Bool
isFun m t = fmap (isFunTy m) $ (termType m) t

-- | Does a term have a function or polymorphic type?
isPolyFun :: (Functor m, Fresh m)
          => HashMap TyConName TyCon
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
         -> Int
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
      -> Int     -- ^ Length of the vector
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
            -> Int     -- ^ Length of the vector
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
             -> Int     -- ^ Length of the vector
             -> Term    -- ^ The vector
             -> [(Term,[LetBinding])]
extractElems consCon resTy s maxN = go maxN
  where
    go :: Int -> Term -> [(Term,[LetBinding])]
    go 0 _ = []
    go n e = (elVar
             ,[(Id elBNm (embed resTy) ,embed lhs)
              ,(Id restBNm (embed restTy),embed rhs)
              ]
             ) :
             go (n-1) (Var restTy restBNm)

      where
        elBNm     = string2Name ("el" ++ s:show (maxN-n))
        restBNm   = string2Name ("rest" ++ s:show (maxN-n))
        elVar     = Var resTy elBNm
        pat       = DataPat (embed consCon) (rebind [mTV] [co,el,rest])
        elPatNm   = string2Name "el"
        restPatNm = string2Name "rest"
        lhs       = Case e resTy  [bind pat (Var resTy  elPatNm)]
        rhs       = Case e restTy [bind pat (Var restTy restPatNm)]

        mName = string2Name "m"
        mTV   = TyVar mName (embed typeNatKind)
        tys   = [(LitTy (NumTy n)),resTy,(LitTy (NumTy (n-1)))]
        (Just idTys) = dataConInstArgTys consCon tys
        [co,el,rest] = zipWith Id [string2Name "_co_",elPatNm, restPatNm]
                                  (map embed idTys)
        restTy = last (fromJust (dataConInstArgTys consCon tys))

-- | Determine whether a type is isomorphic to "CLaSH.Signal.Internal.Signal'"
--
-- It is i.e.:
--
--   * Signal' clk a
--   * (Signal' clk a, Signal' clk b)
--   * Vec n (Signal' clk a)
--   * data Wrap = W (Signal clk' Int)
--   * etc.
isSignalType :: HashMap TyConName TyCon -> Type -> Bool
isSignalType tcm ty = go HashSet.empty ty
  where
    go tcSeen (tyView -> TyConApp tcNm args) = case name2String tcNm of
      "CLaSH.Signal.Internal.Signal'"  -> True
      _ | tcNm `HashSet.member` tcSeen -> False -- Do not follow rec types
        | otherwise -> case HashMap.lookup tcNm tcm of
            Just tc -> let dcs         = tyConDataCons tc
                           dcInsArgTys = concat
                                       $ mapMaybe (`dataConInstArgTys` args) dcs
                           tcSeen'     = HashSet.insert tcNm tcSeen
                       in  any (go tcSeen') dcInsArgTys
            Nothing -> traceIf True ($(curLoc) ++ "isSignalType: " ++ show tcNm
                                     ++ " not found.") False

    go _ _ = False

tyNatSize :: HMS.HashMap TyConName TyCon
          -> Type
          -> Except String Int
tyNatSize _ (LitTy (NumTy i)) = return i
tyNatSize m ty@(tyView -> TyConApp tc [ty1,ty2]) = case name2String tc of
  "GHC.TypeLits.+" -> (+) <$> tyNatSize m ty1 <*> tyNatSize m ty2
  "GHC.TypeLits.*" -> (*) <$> tyNatSize m ty1 <*> tyNatSize m ty2
  "GHC.TypeLits.^" -> (^) <$> tyNatSize m ty1 <*> tyNatSize m ty2
  "GHC.TypeLits.-" -> (-) <$> tyNatSize m ty1 <*> tyNatSize m ty2
  "CLaSH.Promoted.Ord.Max" -> max <$> tyNatSize m ty1 <*> tyNatSize m ty2
  "CLaSH.Promoted.Ord.Min" -> min <$> tyNatSize m ty1 <*> tyNatSize m ty2
  "GHC.TypeLits.Extra.CLog" -> do
    i1' <- tyNatSize m ty1
    i2' <- tyNatSize m ty2
    if (i1' > 1 && i2' > 0)
       then return (ceiling (logBase (fromIntegral i1' :: Double)
                                     (fromIntegral i2' :: Double)))
       else throwE $ $(curLoc) ++ "Can't convert: " ++ show ty
  "GHC.TypeLits.Extra.GCD" -> gcd <$> tyNatSize m ty1 <*> tyNatSize m ty2
  _ -> throwE $ $(curLoc) ++ "Can't convert tyNatOp: " ++ show tc
-- TODO: Remove this conversion
-- The current problem is that type-functions are not reduced by the GHC -> Core
-- transformation process, and so end up here. Once a fix has been found for
-- this problem remove this dirty hack.
tyNatSize tcm ty@(tyView -> TyConApp tc tys) = do
  case tcm HMS.! tc of
    FunTyCon {tyConSubst = tcSubst} -> case findFunSubst tcSubst tys of
      Just ty' -> tyNatSize tcm ty'
      _ -> throwE $ $(curLoc) ++ "Can't convert tyNat: " ++ show ty
    _ -> throwE $ $(curLoc) ++ "Can't convert tyNat: " ++ show ty

tyNatSize _ t = throwE $ $(curLoc) ++ "Can't convert tyNat: " ++ show t
