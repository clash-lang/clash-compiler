{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2021-2024, QBayLogic B.V.,
                     2022     , Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Smart constructor and destructor functions for CoreHW
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Util where

import Control.Exception.Base (patError)
#if MIN_VERSION_base(4,16,0)
import GHC.Prim.Panic (absentError)
#else
import Control.Exception.Base (absentError)
#endif
import Control.Monad.Trans.Except              (Except, throwE, runExcept)
import Data.Bifunctor                          (first, second)
import qualified Data.HashSet                  as HashSet
import qualified Data.Graph                    as Graph
import Data.List                               (mapAccumR, uncons)
import Data.List.Extra                         (zipEqual)
import Data.List.NonEmpty                      (NonEmpty (..), toList)
import Data.Maybe
  (fromMaybe, isJust, mapMaybe, catMaybes)
import qualified Data.Set                      as Set
import qualified Data.Set.Lens                 as Lens
import qualified Data.Text                     as T
import           Data.Text.Extra               (showt)
import           GHC.Real
  (divZeroError, overflowError, ratioZeroDenominatorError, underflowError)
import           GHC.Stack                     (HasCallStack)

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Builtin.Names       (ipClassKey)
#else
import           PrelNames               (ipClassKey)
#endif

import Clash.Core.DataCon
import Clash.Core.EqSolver
import Clash.Core.FreeVars               (freeLocalIds)
import Clash.Core.HasFreeVars
import Clash.Core.HasType
import Clash.Core.Name
  (Name (..), OccName, mkUnsafeInternalName, mkUnsafeSystemName)
import Clash.Core.Pretty                 (showPpr)
import Clash.Core.Subst
import Clash.Core.Term
import Clash.Core.TyCon                  (TyConMap, tyConDataCons)
import Clash.Core.Type
import Clash.Core.TysPrim                (liftedTypeKind, typeNatKind)
import Clash.Core.Var                    (Id, Var(..), mkLocalId, mkTyVar)
import Clash.Core.VarEnv
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Debug                       (traceIf)
import Clash.Unique                      (fromGhcUnique)
import Clash.Util
import Clash.Util.Supply                 (Supply, freshId)

import {-# SOURCE #-} qualified Clash.Normalize.Primitives as Primitives
import Clash.XException (errorX)

-- | Rebuild a let expression / let expressions by taking the SCCs of a list
-- of bindings and remaking Let (NonRec ...) ... and Let (Rec ...) ...
--
listToLets :: [LetBinding] -> Term -> Term
listToLets xs body = foldr go body (sccLetBindings xs)
 where
  go (Graph.AcyclicSCC (i, x)) acc = Let (NonRec i x) acc
  go (Graph.CyclicSCC binds) acc = Let (Rec binds) acc

-- | The type @forall a . a@
undefinedTy ::Type
undefinedTy =
  let aNm = mkUnsafeSystemName "a" 0
      aTv = (TyVar aNm 0 liftedTypeKind)
  in  ForAllTy aTv (VarTy aTv)

-- | The type @forall a. forall b. a -> b@
unsafeCoerceTy :: Type
unsafeCoerceTy =
  let aNm = mkUnsafeSystemName "a" 0
      aTv = TyVar aNm 0 liftedTypeKind
      bNm = mkUnsafeSystemName "b" 1
      bTv = TyVar bNm 1 liftedTypeKind
  in ForAllTy aTv (ForAllTy bTv (mkFunTy (VarTy aTv) (VarTy bTv)))

-- | Create a vector of supplied elements
mkVec :: DataCon -- ^ The Nil constructor
      -> DataCon -- ^ The (:>) constructor
      -> Type    -- ^ Element type
      -> Integer -- ^ Length of the vector
      -> [Term]  -- ^ Elements to put in the vector
      -> Term
mkVec nilCon consCon resTy = go
  where
    go _ [] = mkApps (Data nilCon) [Right (LitTy (NumTy 0))
                                   ,Right resTy
                                   ,Left  (primCo nilCoTy)
                                   ]

    go n (x:xs) = mkApps (Data consCon) [Right (LitTy (NumTy n))
                                        ,Right resTy
                                        ,Right (LitTy (NumTy (n-1)))
                                        ,Left (primCo (consCoTy n))
                                        ,Left x
                                        ,Left (go (n-1) xs)]

    nilCoTy    = case dataConInstArgTys nilCon [(LitTy (NumTy 0)) ,resTy] of
                   Just (x:_) -> x
                   _ -> error "impossible"
    consCoTy n = case dataConInstArgTys consCon
                                        [(LitTy (NumTy n))
                                        ,resTy
                                        ,(LitTy (NumTy (n-1)))] of
                   Just (x:_) -> x
                   _ -> error "impossible"

-- | Append elements to the supplied vector
appendToVec :: DataCon -- ^ The (:>) constructor
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
                                        ,Left (primCo (consCoTy n))
                                        ,Left x
                                        ,Left (go (n-1) xs)]

    consCoTy n = case dataConInstArgTys consCon
                                        [(LitTy (NumTy n))
                                        ,resTy
                                        ,(LitTy (NumTy (n-1)))] of
                   Just (x:_) -> x
                   _ -> error "impossible"

-- | Create let-bindings with case-statements that select elements out of a
-- vector. Returns both the variables to which element-selections are bound
-- and the let-bindings
extractElems
  :: HasCallStack
  => Supply
  -- ^ Unique supply
  -> InScopeSet
  -- ^ (Superset of) in scope variables
  -> DataCon
  -- ^ The (:>) constructor
  -> Type
  -- ^ The element type
  -> Char
  -- ^ Char to append to the bound variable names
  -> Integer
  -- ^ Length of the vector
  -> Term
  -- ^ The vector
  -> (Supply, NonEmpty (Term,NonEmpty (Id, Term)))
extractElems supply inScope consCon resTy s maxN vec =
  if maxN >= 1 then
    first fst (go maxN (supply,inScope) vec)
  else
    error "extractElems must be called with positive number"
 where
  go :: Integer -> (Supply,InScopeSet) -> Term
     -> ((Supply,InScopeSet),NonEmpty (Term, NonEmpty (Id, Term)))
  go n uniqs0 e = fromMaybe (error "extractElems: failed to project elements") $ do
    let tys = [(LitTy (NumTy n)),resTy,(LitTy (NumTy (n-1)))]
    idTys <- dataConInstArgTys consCon tys
    let restTy = last idTys

    let (uniqs1,mTV) = mkUniqSystemTyVar uniqs0 ("m",typeNatKind)
    (uniqs2,[elNId,restNId,co,el,rest]) <- pure $
      mapAccumR mkUniqSystemId uniqs1 $ zipEqual
        ["el" `T.append` (s `T.cons` T.pack (show (maxN-n)))
        ,"rest" `T.append` (s `T.cons` T.pack (show (maxN-n)))
        ,"_co_"
        ,"el"
        ,"rest"
        ]
        (resTy:restTy:idTys)

    let elNVar    = Var elNId
        pat       = DataPat consCon [mTV] [co,el,rest]
        lhs       = Case e resTy  [(pat,Var el)]
        rhs       = Case e restTy [(pat,Var rest)]

    let (uniqs3,restVs) = if n < 2 then (uniqs2,[]) else second toList (go (n-1) uniqs2 (Var restNId))

    return (uniqs3,(elNVar,(elNId, lhs) :| [(restNId, rhs)]) :| restVs)

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
  -> (Supply,([Term],[(Id, Term)]))
extractTElems supply inScope lrCon brCon resTy s maxN tree =
  first fst (go maxN [0..(2^(maxN+1))-2] [0..(2^maxN - 1)] (supply,inScope) tree)
 where
  go :: Integer
     -> [Int]
     -> [Int]
     -> (Supply,InScopeSet)
     -> Term
     -> ((Supply,InScopeSet),([Term],[(Id, Term)]))
  go 0 _ ks uniqs0 e = fromMaybe (error "extractTElems: failed to project elements") $ do
    let tys = [LitTy (NumTy 0),resTy]
    idTys <- dataConInstArgTys lrCon tys
    (k,_) <- uncons ks

    (uniqs1,[elNId,co,el]) <- pure $
      mapAccumR mkUniqSystemId uniqs0 $ zipEqual
        [ "el" `T.append` (s `T.cons` T.pack (show k))
        , "_co_"
        , "el"
        ]
        (resTy:idTys)
    let elNVar = Var elNId
        pat    = DataPat lrCon [] [co,el]
        rhs    = Case e resTy [(pat,Var el)]
    return (uniqs1,([elNVar],[(elNId, rhs)]))

  go n bs ks uniqs0 e = fromMaybe (error "extractTElems: failed to project elements") $ do
    let tys = [LitTy (NumTy n),resTy,LitTy (NumTy (n-1))]
    idTys <- dataConInstArgTys brCon tys

    let (uniqs1,mTV) = mkUniqSystemTyVar uniqs0 ("m",typeNatKind)
    (b0:bL,b1:bR) <- pure (splitAt (length bs `div` 2) bs)
    let brTy = last idTys
    (uniqs2,[ltNId,rtNId,co,lt,rt]) <- pure $
      mapAccumR mkUniqSystemId uniqs1 $ zipEqual
        ["lt" `T.append` (s `T.cons` T.pack (show b0))
        ,"rt" `T.append` (s `T.cons` T.pack (show b1))
        ,"_co_"
        ,"lt"
        ,"rt"
        ]
        (brTy:brTy:idTys)
    let ltVar = Var ltNId
        rtVar = Var rtNId
        pat   = DataPat brCon [mTV] [co,lt,rt]
        ltRhs = Case e brTy [(pat,Var lt)]
        rtRhs = Case e brTy [(pat,Var rt)]

        (kL,kR) = splitAt (length ks `div` 2) ks
        (uniqs3,(lVars,lBinds)) = go (n-1) bL kL uniqs2 ltVar
        (uniqs4,(rVars,rBinds)) = go (n-1) bR kR uniqs3 rtVar

    return ( uniqs4
           , ( lVars ++ rVars
             , (ltNId, ltRhs):(rtNId, rtRhs): (lBinds ++ rBinds)
             )
           )

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
                                    ,Left  (primCo lrCoTy)
                                    ,Left  x
                                    ]

    go n xs =
      let (xsL,xsR) = splitAt (length xs `div` 2) xs
      in  mkApps (Data brCon) [Right (LitTy (NumTy n))
                              ,Right resTy
                              ,Right (LitTy (NumTy (n-1)))
                              ,Left (primCo (brCoTy n))
                              ,Left (go (n-1) xsL)
                              ,Left (go (n-1) xsR)]

    lrCoTy   = case dataConInstArgTys lrCon [(LitTy (NumTy 0)) ,resTy] of
                 Just (x:_) -> x
                 _ -> error "impossible"
    brCoTy n = case dataConInstArgTys brCon
                                      [(LitTy (NumTy n))
                                      ,resTy
                                      ,(LitTy (NumTy (n-1)))] of
                 Just (x:_) -> x
                 _ -> error "impossible"

-- | Determine whether a type is isomorphic to "Clash.Signal.Internal.Signal"
--
-- It is i.e.:
--
--   * Signal clk a
--   * (Signal clk a, Signal clk b)
--   * Vec n (Signal clk a)
--   * data Wrap = W (Signal clk' Int)
--   * etc.
--
-- This also includes BiSignals, i.e.:
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
      "Clash.Signal.BiSignal.BiSignalOut" -> True
      _ | tcNm `HashSet.member` tcSeen    -> False -- Do not follow rec types
        | otherwise -> case UniqMap.lookup tcNm tcm of
            Just tc -> let dcs         = tyConDataCons tc
                           dcInsArgTys = concat
                                       $ mapMaybe (`dataConInstArgTys` args) dcs
                           tcSeen'     = HashSet.insert tcNm tcSeen
                       in  any (go tcSeen') dcInsArgTys
            Nothing -> traceIf True ($(curLoc) ++ "isSignalType: " ++ show tcNm
                                     ++ " not found.") False

    go _ _ = False

-- | Determines whether given type is an (alias of en) Enable line.
isEnable
  :: TyConMap
  -> Type
  -> Bool
isEnable m ty0
  | TyConApp (nameOcc -> "Clash.Signal.Internal.Enable") _ <- tyView ty0 = True
  | Just ty1 <- coreView1 m ty0 = isEnable m ty1
isEnable _ _ = False

-- | Determines whether given type is an (alias of en) Clock or Reset line
isClockOrReset
  :: TyConMap
  -> Type
  -> Bool
isClockOrReset m (coreView1 m -> Just ty)    = isClockOrReset m ty
isClockOrReset _ (tyView -> TyConApp tcNm _) = case nameOcc tcNm of
  "Clash.Signal.Internal.Clock" -> True
  "Clash.Signal.Internal.ClockN" -> True
  "Clash.Signal.Internal.Reset" -> True
  _ -> False
isClockOrReset _ _ = False

tyNatSize :: TyConMap
          -> Type
          -> Except String Integer
tyNatSize m (coreView1 m -> Just ty) = tyNatSize m ty
tyNatSize _ (LitTy (NumTy i))        = return i
tyNatSize _ ty = throwE $ $(curLoc) ++ "Cannot reduce to an integer:\n" ++ showPpr ty


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
  v           = mkLocalId ty (mkUnsafeSystemName nm u)
  v'          = uniqAway inScope v

mkUniqInternalId
  :: (Supply, InScopeSet)
  -> (OccName, Type)
  -> ((Supply,InScopeSet), Id)
mkUniqInternalId (supply,inScope) (nm, ty) =
  ((supply',extendInScopeSet inScope v'), v')
 where
  (u,supply') = freshId supply
  v           = mkLocalId ty (mkUnsafeInternalName nm u)
  v'          = uniqAway inScope v


-- | Same as @dataConInstArgTys@, but it tries to compute existentials too,
-- hence the extra argument @TyConMap@. WARNING: It will return the types
-- of non-existentials only
dataConInstArgTysE
  :: HasCallStack
  => InScopeSet
  -> TyConMap
  -> DataCon
  -> [Type]
  -> Maybe [Type]
dataConInstArgTysE is0 tcm (MkData { dcArgTys, dcExtTyVars, dcUnivTyVars }) inst_tys = do
  -- TODO: Check if all existentials were solved (they should be, or the wouldn't have
  -- TODO: been solved in the caseElemExistentials transformation)
  let is1   = extendInScopeSetList is0 dcExtTyVars
      is2   = unionInScope is1 (mkInScopeSet (freeVarsOf inst_tys))
      subst = extendTvSubstList (mkSubst is2) (zipEqual dcUnivTyVars inst_tys)
  go
    (substGlobalsInExistentials is0 dcExtTyVars (zipEqual dcUnivTyVars inst_tys))
    (map (substTy subst) dcArgTys)

 where
  exts = mkVarSet dcExtTyVars
  go
    :: [TyVar]
    -- ^ Existentials
    -> [Type]
    -- ^ Type arguments
    -> Maybe [Type]
    -- ^ Maybe ([type of non-existential])
  go exts0 args0 =
    let eqs = catMaybes (map (typeEq tcm) args0) in
    case solveNonAbsurds tcm exts eqs of
      [] ->
        Just args0
      sols ->
        go exts1 args1
        where
          exts1 = substInExistentialsList is0 exts0 sols
          is2   = extendInScopeSetList is0 exts1
          subst = extendTvSubstList (mkSubst is2) sols
          args1 = map (substTy subst) args0


-- | Given a DataCon and a list of types, the type variables of the DataCon
-- type are substituted for the list of types. The argument types are returned.
--
-- The list of types should be equal to the number of type variables, otherwise
-- @Nothing@ is returned.
dataConInstArgTys :: DataCon -> [Type] -> Maybe [Type]
dataConInstArgTys (MkData { dcArgTys, dcUnivTyVars, dcExtTyVars }) inst_tys =
  -- TODO: Check if inst_tys do not contain any free variables on call sites. If
  -- TODO: they do, this function is unsafe to use.
  let tyvars = dcUnivTyVars ++ dcExtTyVars in
  if length tyvars == length inst_tys then
    Just (map (substTyWith tyvars inst_tys) dcArgTys)
  else
    Nothing

-- | Make a coercion
primCo
  :: Type
  -> Term
primCo ty = Prim (PrimInfo "_CO_" ty WorkNever SingleResult NoUnfolding)

-- | Make an unsafe coercion
primUCo :: Term
primUCo =
  Prim PrimInfo { primName        = "GHC.Prim.unsafeCoerce#"
                , primType        = unsafeCoerceTy
                , primWorkInfo    = WorkNever
                , primMultiResult = SingleResult
                , primUnfolding = NoUnfolding
                }

undefinedPrims :: [T.Text]
undefinedPrims = fmap showt
  [ 'Primitives.undefined
  , 'patError
  , 'error
  , 'errorWithoutStackTrace
  , 'undefined
  , 'absentError
  , 'divZeroError
  , 'overflowError
  , 'ratioZeroDenominatorError
  , 'underflowError
  ]

undefinedXPrims :: [T.Text]
undefinedXPrims = fmap showt
  [ 'Primitives.undefinedX
  , 'errorX
  ]

substArgTys
  :: DataCon
  -> [Type]
  -> [Type]
substArgTys dc args =
  let univTVs = dcUnivTyVars dc
      extTVs  = dcExtTyVars dc
      argsFVs = freeVarsOf args
      is      = mkInScopeSet (argsFVs `unionVarSet` mkVarSet extTVs)
      -- See Note [The substitution invariant]
      subst   = extendTvSubstList (mkSubst is) (univTVs `zipEqual` args)
  in  map (substTy subst) (dcArgTys dc)

-- | Try to reduce an arbitrary type to a literal type (Symbol or Nat),
-- and subsequently extract its String representation
tyLitShow
  :: TyConMap
  -> Type
  -> Except String String
tyLitShow m (coreView1 m -> Just ty) = tyLitShow m ty
tyLitShow _ (LitTy (SymTy s))        = return s
tyLitShow _ (LitTy (NumTy s))        = return (show s)
tyLitShow _ ty = throwE $ $(curLoc) ++ "Cannot reduce to a string:\n" ++ showPpr ty

-- | Helper existential for 'shouldSplit', contains a function that:
--
-- 1. given a term of a type that should be split,
-- 2. creates projections of that term for all the constructor arguments
data Projections where
  Projections :: (forall m . MonadUnique m => InScopeSet -> Term -> m [Term])
              -> Projections

-- | Determine whether we should split away types from a product type, i.e.
-- clocks should always be separate arguments, and not part of a product.
shouldSplit
  :: TyConMap
  -> Type
  -- ^ Type to examine
  -> Maybe ([Term] -> Term, Projections, [Type])
  -- ^ If we want to split values of the given type then we have /Just/:
  --
  -- 1. The (type-applied) data-constructor which, when applied to values of
  --    the types in 3., creates a value of the examined type
  --
  -- 2. Function that give a term of the type we need to split, creates projections
  --    of that term for all the types in 3.
  --
  -- 3. The arguments types of the product we are trying to split.
  --
  -- Note that we only split one level at a time (although we check all the way
  -- down), e.g. given /(Int, (Clock, Bool))/ we return:
  --
  -- > Just ( (,) @Int @(Clock, Bool)
  -- >      , \s -> [case s of (a,b) -> a, case s of (a,b) -> b]
  -- >      , [Int, (Clock, Bool)])
  --
  -- An outer loop is required to subsequently split the /(Clock, Bool)/ tuple.
shouldSplit tcm (tyView ->  TyConApp (nameOcc -> "Clash.Explicit.SimIO.SimIO") [tyArg]) =
  -- We also look through `SimIO` to find things like Files
  shouldSplit tcm tyArg
shouldSplit tcm ty = shouldSplit0 tcm (tyView (coreView tcm ty))

-- | Worker of 'shouldSplit', works on 'TypeView' instead of 'Type'
shouldSplit0
  :: TyConMap
  -> TypeView
  -> Maybe ([Term] -> Term, Projections, [Type])
shouldSplit0 tcm (TyConApp tcNm tyArgs)
  | Just tc <- UniqMap.lookup tcNm tcm
  , [dc] <- tyConDataCons tc
  , let dcArgs = substArgTys dc tyArgs
  , let dcArgsLen = length dcArgs
  , dcArgsLen > 1
  , let dcArgVs = map (tyView . coreView tcm) dcArgs
  = if any shouldSplitTy dcArgVs && not (isHidden tcNm tyArgs) then
      Just ( mkApps (Data dc) . (map Right tyArgs ++) . map Left
           , Projections
             (\is0 subj -> mapM (mkSelectorCase ($(curLoc) ++ "splitArg") is0 tcm subj 1)
                                [0..dcArgsLen - 1])
           , dcArgs
           )
    else
      Nothing
  | "Clash.Sized.Vector.Vec" <- nameOcc tcNm
  , [nTy,argTy] <- tyArgs
  , Right n <- runExcept (tyNatSize tcm nTy)
  , n > 1
  , Just tc <- UniqMap.lookup tcNm tcm
  , [nil,cons] <- tyConDataCons tc
  = if shouldSplitTy (tyView (coreView tcm argTy)) then
      Just ( mkVec nil cons argTy n
           , Projections (\is0 subj -> mapM (mkVecSelector is0 subj) [0..n-1])
           , replicate (fromInteger n) argTy)
    else
      Nothing
 where
  -- Project the n'th value out of a vector
  --
  -- >>> mkVecSelector subj 0
  -- case subj of x :> xs -> x
  --
  -- >>> mkVecSelector subj 2
  -- case (case (case subj of x :> xs -> xs) of x :> xs -> xs) of x :> xs -> x
  mkVecSelector :: forall m . MonadUnique m => InScopeSet -> Term -> Integer -> m Term
  mkVecSelector is0 subj 0 =
    mkSelectorCase ($(curLoc) ++ "mkVecSelector") is0 tcm subj 2 1

  mkVecSelector is0 subj !n = do
    subj1 <- mkSelectorCase ($(curLoc) ++ "mkVecSelector") is0 tcm subj 2 2
    mkVecSelector is0 subj1 (n-1)

  shouldSplitTy :: TypeView -> Bool
  shouldSplitTy ty = isJust (shouldSplit0 tcm ty) || splitTy ty

  -- Hidden constructs (HiddenClock, HiddenReset, ..) don't need to be split
  -- because KnownDomain will be filtered anyway during netlist generation due
  -- to it being a zero-width type
  --
  -- TODO: This currently only handles (IP $x, KnownDomain) given that $x is any
  -- TODO: of the constructs handled in 'splitTy'. In practice this means only
  -- TODO: HiddenClock, HiddenReset, and HiddenEnable are handled. If a user were
  -- TODO: to define their own versions with -for example- the elements of the
  -- TODO: tuple swapped, 'isHidden' wouldn't recognize it. We could generalize
  -- TODO: this in the future.
  --
  isHidden :: Name a -> [Type] -> Bool
  isHidden nm [a1, a2] | TyConApp a2Nm _ <- tyView a2 =
       nameOcc nm `elem` ["GHC.Classes.(%,%)", "GHC.Classes.CTuple2"]
    && splitTy (tyView (stripIP a1))
    && nameOcc a2Nm == "Clash.Signal.Internal.KnownDomain"
  isHidden _ _ = False

  splitTy (TyConApp tcNm0 _)
    = nameOcc tcNm0 `elem` [ "Clash.Signal.Internal.Clock"
                           , "Clash.Signal.Internal.ClockN"
                           , "Clash.Signal.Internal.Reset"
                           , "Clash.Signal.Internal.Enable"
                           -- iverilog doesn't like it when we put file handles
                           -- in a bitvector, so we need to make sure Clash
                           -- splits them off
                           , "Clash.Explicit.SimIO.File"
                           , "GHC.IO.Handle.Types.Handle"
                           ]
  splitTy _ = False

shouldSplit0 _ _ = Nothing

-- | Potentially split apart a list of function argument types. e.g. given:
--
-- > [Int,(Clock,(Reset,Bool)),Char]
--
-- we return
--
-- > [Int,Clock,Reset,Bool,Char]
--
-- But we would leave
--
-- > [Int, (Bool,Int), Char]
--
-- unchanged.
splitShouldSplit
  :: TyConMap
  -> [Type]
  -> [Type]
splitShouldSplit tcm = foldr go []
 where
  go ty rest = case shouldSplit tcm ty of
    Just (_,_,tys) -> splitShouldSplit tcm tys ++ rest
    Nothing        -> ty : rest

-- | Strip implicit parameter wrappers (IP)
stripIP :: Type -> Type
stripIP t@(tyView -> TyConApp tcNm [_a1, a2]) =
  if nameUniq tcNm == fromGhcUnique ipClassKey then a2 else t
stripIP t = t

-- | Do an inverse topological sorting of the let-bindings in a let-expression
inverseTopSortLetBindings
  :: HasCallStack
  => [(Id, Term)]
  -> [(Id, Term)]
inverseTopSortLetBindings bndrs0 =
  let (graph,nodeMap,_) =
        Graph.graphFromEdges
          (map (\(i,e) -> let fvs = fmap varUniq
                                    (Set.elems (Lens.setOf freeLocalIds e) )
                          in  ((i,e),varUniq i,fvs)) bndrs0)
      nodes  = postOrd graph
      bndrs1 = map ((\(x,_,_) -> x) . nodeMap) nodes
   in bndrs1
 where
  postOrd :: Graph.Graph -> [Graph.Vertex]
  postOrd g = postorderF (Graph.dff g) []

  postorderF :: Graph.Forest a -> [a] -> [a]
  postorderF ts = foldr (.) id (map postorder ts)

  postorder :: Graph.Tree a -> [a] -> [a]
  postorder (Graph.Node a ts) = postorderF ts . (a :)
{-# SCC inverseTopSortLetBindings #-}

-- | Group let-bindings into cyclic groups and acyclic individual bindings
sccLetBindings
  :: HasCallStack
  => [(Id, Term)]
  -> [Graph.SCC (Id, Term)]
sccLetBindings =
  Graph.stronglyConnComp .
  (map (\(i,e) -> let fvs = fmap varUniq
                            (Set.elems (Lens.setOf freeLocalIds e) )
                  in  ((i,e),varUniq i,fvs)))
{-# SCC sccLetBindings #-}

-- | Make a case-decomposition that extracts a field out of a (Sum-of-)Product type
mkSelectorCase
  :: HasCallStack
  => MonadUnique m
  => String -- ^ Name of the caller of this function
  -> InScopeSet
  -> TyConMap -- ^ TyCon cache
  -> Term -- ^ Subject of the case-composition
  -> Int -- ^ n'th DataCon
  -> Int -- ^ n'th field
  -> m Term
mkSelectorCase caller inScope tcm scrut dcI fieldI = go (inferCoreTypeOf tcm scrut)
  where
    go (coreView1 tcm -> Just ty') = go ty'
    go scrutTy@(tyView -> TyConApp tc args) =
      case tyConDataCons (UniqMap.find tc tcm) of
        [] -> cantCreate $(curLoc) ("TyCon has no DataCons: " ++ show tc ++ " " ++ showPpr tc) scrutTy
        dcs | dcI > length dcs -> cantCreate $(curLoc) "DC index exceeds max" scrutTy
            | otherwise -> do
          let dc = indexNote ($(curLoc) ++ "No DC with tag: " ++ show (dcI-1)) dcs (dcI-1)
          let fieldTys =
                fromMaybe (cantCreate $(curLoc) "Cannot instantiate dataCon" scrutTy)
                          (dataConInstArgTysE inScope tcm dc args)
          if fieldI >= length fieldTys
            then cantCreate $(curLoc) "Field index exceed max" scrutTy
            else do
              wildBndrs <- mapM (mkWildValBinder inScope) fieldTys
              let ty = indexNote ($(curLoc) ++ "No DC field#: " ++ show fieldI) fieldTys fieldI
              selBndr <- mkInternalVar inScope "sel" ty
              let bndrs  = take fieldI wildBndrs ++ [selBndr] ++ drop (fieldI+1) wildBndrs
                  pat    = DataPat dc (dcExtTyVars dc) bndrs
                  retVal = Case scrut ty [ (pat, Var selBndr) ]
              return retVal
    go scrutTy = cantCreate $(curLoc) ("Type of subject is not a datatype: " ++ showPpr scrutTy) scrutTy

    cantCreate :: String -> String -> Type -> a
    cantCreate loc info scrutTy = error $ loc ++ "Can't create selector " ++ show (caller,dcI,fieldI) ++ " for: (" ++ showPpr scrut ++ " :: " ++ showPpr scrutTy ++ ")\nAdditional info: " ++ info

-- | Make a binder that should not be referenced
mkWildValBinder
  :: (MonadUnique m)
  => InScopeSet
  -> Type
  -> m Id
mkWildValBinder is = mkInternalVar is "wild"

-- | Make a new, unique, identifier
mkInternalVar
  :: (MonadUnique m)
  => InScopeSet
  -> OccName
  -- ^ Name of the identifier
  -> KindOrType
  -> m Id
mkInternalVar inScope name ty = do
  i <- getUniqueM
  let nm = mkUnsafeInternalName name i
  return (uniqAway inScope (mkLocalId ty nm))
