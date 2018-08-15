{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Smart constructor and destructor functions for CoreHW
-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Core.Util where

import           Control.Concurrent.Supply     (Supply, freshId)
import qualified Control.Lens                  as Lens
import Control.Monad.Trans.Except              (Except, throwE)
import           Data.Coerce                   (coerce)
import qualified Data.IntSet                   as IntSet
import qualified Data.HashSet                  as HashSet
import Data.List                               (foldl', mapAccumR)
import Data.Maybe                              (fromJust, mapMaybe)
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc     (line)
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif

import Clash.Core.DataCon
  (DataCon, dcType, dataConInstArgTys)
import Clash.Core.FreeVars                     (termFreeVars, tyFVsOfTypes)
import Clash.Core.Literal                      (literalType)
import Clash.Core.Name
  (Name (..), OccName, mkUnsafeInternalName, mkUnsafeSystemName)
import Clash.Core.Pretty                       (ppr, showDoc)
import Clash.Core.Subst
  (extendTvSubst, mkSubst, mkTvSubst, substTy)
import Clash.Core.Term
  (LetBinding, Pat (..), Term (..))
import Clash.Core.Type
  (Kind, LitTy (..), Type (..), TypeView (..),
   coreView, isFunTy, isPolyFunCoreTy, mkFunTy, splitFunTy, tyView)
import Clash.Core.TyCon
  (TyConMap, tyConDataCons)
import Clash.Core.TysPrim                      (typeNatKind)
import Clash.Core.Var
  (Id, TyVar, Var (..), mkId, mkTyVar)
import Clash.Core.VarEnv
  (InScopeSet, VarEnv, emptyVarEnv, extendInScopeSet, extendVarEnv,
   lookupVarEnv, mkInScopeSet, uniqAway)
import Clash.Unique
import Clash.Util

-- | Type environment/context
type Gamma = VarEnv Type
-- | Kind environment/context
type Delta = VarEnv Kind

-- | Determine the type of a term
termType
  :: TyConMap
  -> Term
  -> Type
termType m e = case e of
  Var t          -> varType t
  Data dc        -> dcType dc
  Literal l      -> literalType l
  Prim _ t       -> t
  Lam v e'       -> mkFunTy (varType v) (termType m e')
  TyLam tv e'    -> ForAllTy tv (termType m e')
  App _ _        -> case collectArgs e of
                      (fun, args) -> applyTypeToArgs e m (termType m fun) args
  TyApp _ _      -> case collectArgs e of
                      (fun, args) -> applyTypeToArgs e m (termType m fun) args
  Letrec _ e'    -> termType m e'
  Case _ ty _    -> ty
  Cast _ _ ty2   -> ty2

-- | Split a (Type)Application in the applied term and it arguments
collectArgs :: Term
            -> (Term, [Either Term Type])
collectArgs = go []
  where
    go args (App e1 e2) = go (Left e2:args) e1
    go args (TyApp e t) = go (Right t:args) e
    go args e           = (e, args)

-- | Split a (Type)Abstraction in the bound variables and the abstracted term
collectBndrs :: Term
             -> ([Either Id TyVar], Term)
collectBndrs = go []
  where
    go bs (Lam v e')    = go (Left v:bs) e'
    go bs (TyLam tv e') = go (Right tv:bs) e'
    go bs e'            = (reverse bs,e')

-- | Get the result type of a polymorphic function given a list of arguments
applyTypeToArgs
  :: Term
  -> TyConMap
  -> Type
  -> [Either Term Type]
  -> Type
applyTypeToArgs e m opTy args = go opTy args
 where
  go opTy' []               = opTy'
  go opTy' (Right ty:args') = goTyArgs opTy' [ty] args'
  go opTy' (Left _:args')   = case splitFunTy m opTy' of
    Just (_,resTy) -> go resTy args'
    _ -> error $ unlines ["applyTypeToArgs:"
                         ,"Expression: " ++ showDoc (ppr e)
                         ,"Type: " ++ showDoc (ppr opTy)
                         ,"Args: " ++ unlines (map (either (showDoc.ppr) (showDoc.ppr)) args)
                         ]

  goTyArgs opTy' revTys (Right ty:args') = goTyArgs opTy' (ty:revTys) args'
  goTyArgs opTy' revTys args'            = go (piResultTys m opTy' (reverse revTys)) args'

-- | Like 'piResultTyMaybe', but errors out when a type application is not
-- valid.
--
-- Do not iterate 'piResultTy', because it's inefficient to substitute one
-- variable at a time; instead use 'piResultTys'
piResultTy
  :: TyConMap
  -> Type
  -> Type
  -> Type
piResultTy m ty arg = case piResultTyMaybe m ty arg of
  Just res -> res
  Nothing  -> pprPanic "piResultTy" (ppr ty <> line <> ppr arg)

-- | Like 'piResultTys' but for a single argument.
--
-- Do not iterate 'piResultTyMaybe', because it's inefficient to substitute one
-- variable at a time; instead use 'piResultTys'
piResultTyMaybe
  :: TyConMap
  -> Type
  -> Type
  -> Maybe Type
piResultTyMaybe m ty arg
  | Just ty' <- coreView m ty
  = piResultTyMaybe m ty' arg
  | FunTy _ res <- tyView ty
  = Just res
  | ForAllTy tv res <- ty
  = let emptySubst = mkSubst (mkInScopeSet (tyFVsOfTypes [arg,res]))
    in  Just (substTy (extendTvSubst emptySubst tv arg) res)
  | otherwise
  = Nothing

-- | @(piResultTys f_ty [ty1, ..., tyn])@ give sthe type of @(f ty1 .. tyn)@
-- where @f :: f_ty@
--
-- 'piResultTys' is interesting because:
--
--    1. 'f_ty' may have more foralls than there are args
--    2. Less obviously, it may have fewer foralls
--
-- Fore case 2. think of:
--
--   piResultTys (forall a . a) [forall b.b, Int]
--
-- This really can happen, such as situations involving 'undefined's type:
--
--   undefined :: forall a. a
--
--   undefined (forall b. b -> b) Int
--
-- This term should have the type @(Int -> Int)@, but notice that there are
-- more type args than foralls in 'undefined's type.
--
-- For efficiency reasons, when there are no foralls, we simply drop arrows from
-- a function type/kind.
piResultTys
  :: TyConMap
  -> Type
  -> [Type]
  -> Type
piResultTys _ ty [] = ty
piResultTys m ty origArgs@(arg:args)
  | Just ty' <- coreView m ty
  = piResultTys m ty' origArgs
  | FunTy _ res <- tyView ty
  = piResultTys m res args
  | ForAllTy tv res <- ty
  = go (extendVarEnv tv arg emptyVarEnv) res args
  | otherwise
  = pprPanic "piResultTys1" (ppr ty <> line <> ppr origArgs)
 where
  inScope = mkInScopeSet (tyFVsOfTypes (ty:origArgs))

  go env ty' [] = substTy (mkTvSubst inScope env) ty'
  go env ty' allArgs@(arg':args')
    | Just ty'' <- coreView m ty'
    = go env ty'' allArgs
    | FunTy _ res <- tyView ty'
    = go env res args'
    | ForAllTy tv res <- ty'
    = go (extendVarEnv tv arg' env) res args'
    | VarTy tv <- ty'
    , Just ty'' <- lookupVarEnv tv env
      -- Deals with (piResultTys  (forall a.a) [forall b.b, Int])
    = piResultTys m ty'' allArgs
    | otherwise
    = pprPanic "piResultTys2" (ppr ty' <> line <> ppr origArgs <> line <> ppr allArgs)

-- | Get the list of term-binders out of a DataType pattern
patIds :: Pat -> ([TyVar],[Id])
patIds (DataPat _  tvs ids) = (tvs,ids)
patIds _                    = ([],[])

patVars :: Pat -> [Var a]
patVars (DataPat _ tvs ids) = coerce tvs ++ coerce ids
patVars _ = []

-- | Abstract a term over a list of term and type variables
mkAbstraction :: Term
              -> [Either Id TyVar]
              -> Term
mkAbstraction = foldr (either Lam TyLam)

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
mkApps = foldl' (\e a -> either (App e) (TyApp e) a)

-- | Apply a list of terms to a term
mkTmApps :: Term
         -> [Term]
         -> Term
mkTmApps = foldl' App

-- | Apply a list of types to a term
mkTyApps :: Term
         -> [Type]
         -> Term
mkTyApps = foldl' TyApp

-- | Does a term have a function type?
isFun :: TyConMap
      -> Term
      -> Bool
isFun m t = isFunTy m (termType m t)

-- | Does a term have a function or polymorphic type?
isPolyFun :: TyConMap
          -> Term
          -> Bool
isPolyFun m t = isPolyFunCoreTy m (termType m t)

-- | Is a term a term-abstraction?
isLam :: Term
      -> Bool
isLam (Lam {}) = True
isLam _        = False

-- | Is a term a recursive let-binding?
isLet :: Term
      -> Bool
isLet (Letrec {}) = True
isLet _           = False

-- | Is a term a variable reference?
isVar :: Term
      -> Bool
isVar (Var {}) = True
isVar _        = False

-- | Is a term a datatype constructor?
isCon :: Term
      -> Bool
isCon (Data {}) = True
isCon _         = False

-- | Is a term a primitive?
isPrim :: Term
       -> Bool
isPrim (Prim {}) = True
isPrim _         = False

-- | Make variable reference out of term variable
idToVar :: Id
        -> Term
idToVar i@(Id {}) = Var i
idToVar tv        = error $ $(curLoc) ++ "idToVar: tyVar: " ++ showDoc (ppr tv)

-- | Make a term variable out of a variable reference
varToId :: Term
        -> Id
varToId (Var i) = i
varToId e       = error $ $(curLoc) ++ "varToId: not a var: " ++ showDoc (ppr e)

termSize :: Term
         -> Word
termSize (Var {})     = 1
termSize (Data {})    = 1
termSize (Literal {}) = 1
termSize (Prim {})    = 1
termSize (Lam _ e)    = termSize e + 1
termSize (TyLam _ e)  = termSize e
termSize (App e1 e2)  = termSize e1 + termSize e2
termSize (TyApp e _)  = termSize e
termSize (Cast e _ _) = termSize e
termSize (Letrec bndrs e) = sum (bodySz:bndrSzs)
 where
  bndrSzs = map (termSize . snd) bndrs
  bodySz  = termSize e
termSize (Case subj _ alts) = sum (subjSz:altSzs)
 where
  subjSz = termSize subj
  altSzs = map (termSize . snd) alts

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


availableUniques
  :: Term
  -> [Unique]
availableUniques t = [ n | n <- [0..] , n `IntSet.notMember` avoid ]
 where
  avoid = Lens.foldMapOf termFreeVars (\a i -> IntSet.insert (varUniq a) i) t
            IntSet.empty

-- | Create let-bindings with case-statements that select elements out of a
-- vector. Returns both the variables to which element-selections are bound
-- and the let-bindings
extractElems
  :: Supply
  -- ^ Unique supply
  -> InScopeSet
  -- ^ (Superset of) in scope variables
  -> DataCon
  -- ^ The Cons (:>) constructor
  -> Type
  -- ^ The element type
  -> Char
  -- ^ Char to append to the bound variable names
  -> Integer
  -- ^ Length of the vector
  -> Term
  -- ^ The vector
  -> (Supply, [(Term,[LetBinding])])
extractElems supply inScope consCon resTy s maxN vec =
  first fst (go maxN (supply,inScope) vec)
 where
  go :: Integer -> (Supply,InScopeSet) -> Term
     -> ((Supply,InScopeSet),[(Term,[LetBinding])])
  go 0 uniqs _ = (uniqs,[])
  go n uniqs0 e =
    (uniqs3,(elNVar,[(elNId, lhs),(restNId, rhs)]):restVs)
   where
    tys = [(LitTy (NumTy n)),resTy,(LitTy (NumTy (n-1)))]
    (Just idTys) = dataConInstArgTys consCon tys
    restTy       = last idTys

    (uniqs1,mTV) = mkUniqSystemTyVar uniqs0 ("m",typeNatKind)
    (uniqs2,[elNId,restNId,co,el,rest]) =
      mapAccumR mkUniqSystemId uniqs1 $ zip
        ["el" `T.append` (s `T.cons` T.pack (show (maxN-n)))
        ,"rest" `T.append` (s `T.cons` T.pack (show (maxN-n)))
        ,"_co_"
        ,"el"
        ,"rest"
        ]
        (resTy:restTy:idTys)

    elNVar    = Var elNId
    pat       = DataPat consCon [mTV] [co,el,rest]
    lhs       = Case e resTy  [(pat,Var el)]
    rhs       = Case e restTy [(pat,Var rest)]

    (uniqs3,restVs) = go (n-1) uniqs2 (Var restNId)

-- | Create let-bindings with case-statements that select elements out of a
-- tree. Returns both the variables to which element-selections are bound
-- and the let-bindings
extractTElems
  :: Supply
  -- ^ Unique supply
  -> InScopeSet
  -- ^ (Superset of) in scope variables
  -> DataCon
  -- ^ The 'LR' constructor
  -> DataCon
  -- ^ The 'BR' constructor
  -> Type
  -- ^ The element type
  -> Char
  -- ^ Char to append to the bound variable names
  -> Integer
  -- ^ Depth of the tree
  -> Term
  -- ^ The tree
  -> (Supply,([Term],[LetBinding]))
extractTElems supply inScope lrCon brCon resTy s maxN tree =
  first fst (go maxN [0..(2^(maxN+1))-2] [0..(2^maxN - 1)] (supply,inScope) tree)
 where
  go :: Integer
     -> [Int]
     -> [Int]
     -> (Supply,InScopeSet)
     -> Term
     -> ((Supply,InScopeSet),([Term],[LetBinding]))
  go 0 _ ks uniqs0 e = (uniqs1,([elNVar],[(elNId, rhs)]))
   where
    tys          = [LitTy (NumTy 0),resTy]
    (Just idTys) = dataConInstArgTys lrCon tys

    (uniqs1,[elNId,co,el]) =
      mapAccumR mkUniqSystemId uniqs0 $ zip
        [ "el" `T.append` (s `T.cons` T.pack (show (head ks)))
        , "_co_"
        , "el"
        ]
        (resTy:idTys)
    elNVar = Var elNId
    pat    = DataPat lrCon [] [co,el]
    rhs    = Case e resTy [(pat,Var el)]

  go n bs ks uniqs0 e =
    (uniqs4
    ,(lVars ++ rVars,(ltNId, ltRhs):
                     (rtNId, rtRhs):
                     (lBinds ++ rBinds)))
   where
    tys = [LitTy (NumTy n),resTy,LitTy (NumTy (n-1))]
    (Just idTys) = dataConInstArgTys brCon tys

    (uniqs1,mTV) = mkUniqSystemTyVar uniqs0 ("m",typeNatKind)
    (b0:bL,b1:bR) = splitAt (length bs `div` 2) bs
    brTy = last idTys
    (uniqs2,[ltNId,rtNId,co,lt,rt]) =
      mapAccumR mkUniqSystemId uniqs1 $ zip
        ["lt" `T.append` (s `T.cons` T.pack (show b0))
        ,"rt" `T.append` (s `T.cons` T.pack (show b1))
        ,"_co_"
        ,"lt"
        ,"rt"
        ]
        (brTy:brTy:idTys)
    ltVar = Var ltNId
    rtVar = Var rtNId
    pat   = DataPat brCon [mTV] [co,lt,rt]
    ltRhs = Case e brTy [(pat,Var lt)]
    rtRhs = Case e brTy [(pat,Var rt)]

    (kL,kR) = splitAt (length ks `div` 2) ks
    (uniqs3,(lVars,lBinds)) = go (n-1) bL kL uniqs2 ltVar
    (uniqs4,(rVars,rBinds)) = go (n-1) bR kR uniqs3 rtVar

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
isSignalType :: TyConMap -> Type -> Bool
isSignalType tcm ty = go HashSet.empty ty
  where
    go tcSeen (tyView -> TyConApp tcNm args) = case nameOcc tcNm of
      "Clash.Signal.Internal.Signal"      -> True
      "Clash.Signal.BiSignal.BiSignalIn"  -> True
      "Clash.Signal.Internal.BiSignalOut" -> True
      _ | tcNm `HashSet.member` tcSeen    -> False -- Do not follow rec types
        | otherwise -> case lookupUniqMap tcNm tcm of
            Just tc -> let dcs         = tyConDataCons tc
                           dcInsArgTys = concat
                                       $ mapMaybe (`dataConInstArgTys` args) dcs
                           tcSeen'     = HashSet.insert tcNm tcSeen
                       in  any (go tcSeen') dcInsArgTys
            Nothing -> traceIf True ($(curLoc) ++ "isSignalType: " ++ show tcNm
                                     ++ " not found.") False

    go _ _ = False

isClockOrReset
  :: TyConMap
  -> Type
  -> Bool
isClockOrReset m (coreView m -> Just ty) = isClockOrReset m ty
isClockOrReset _ (tyView -> TyConApp tcNm _) = case nameOcc tcNm of
  "Clash.Signal.Internal.Clock" -> True
  "Clash.Signal.Internal.Reset" -> True
  _ -> False
isClockOrReset _ _ = False

tyNatSize :: TyConMap
          -> Type
          -> Except String Integer
tyNatSize m (coreView m -> Just ty) = tyNatSize m ty
tyNatSize _ (LitTy (NumTy i))       = return i
tyNatSize _ ty = throwE $ $(curLoc) ++ "Cannot reduce to an integer:\n" ++ showDoc (ppr ty)


mkUniqSystemTyVar
  :: (Supply, InScopeSet)
  -> (OccName, Kind)
  -> ((Supply, InScopeSet), TyVar)
mkUniqSystemTyVar (supply,inScope) (nm, ki) =
  ((supply',extendInScopeSet inScope v'), v')
 where
  (u,supply') = freshId supply
  v           = mkTyVar ki (mkUnsafeSystemName nm u)
  v'          = uniqAway inScope v

mkUniqSystemId
  :: (Supply, InScopeSet)
  -> (OccName, Type)
  -> ((Supply,InScopeSet), Id)
mkUniqSystemId (supply,inScope) (nm, ty) =
  ((supply',extendInScopeSet inScope v'), v')
 where
  (u,supply') = freshId supply
  v           = mkId ty (mkUnsafeSystemName nm u)
  v'          = uniqAway inScope v

mkUniqInternalId
  :: (Supply, InScopeSet)
  -> (OccName, Type)
  -> ((Supply,InScopeSet), Id)
mkUniqInternalId (supply,inScope) (nm, ty) =
  ((supply',extendInScopeSet inScope v'), v')
 where
  (u,supply') = freshId supply
  v           = mkId ty (mkUnsafeInternalName nm u)
  v'          = uniqAway inScope v
