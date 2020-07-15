{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Smart constructor and destructor functions for CoreHW
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Util where

import           Control.Concurrent.Supply     (Supply, freshId)
import qualified Control.Lens                  as Lens
import Control.Monad.Trans.Except              (Except, throwE)
import qualified Data.HashSet                  as HashSet
import qualified Data.Graph                    as Graph
import Data.List                               (foldl', mapAccumR)
import Data.List.Extra                         (zipEqual)
import Data.Maybe
  (fromJust, isJust, mapMaybe, catMaybes)
import qualified Data.Set                      as Set
import qualified Data.Set.Lens                 as Lens
import qualified Data.Text                     as T
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif

import           PrelNames               (ipClassKey)
import           Unique                  (getKey)

import Clash.Core.DataCon
import Clash.Core.EqSolver
import Clash.Core.FreeVars               (tyFVsOfTypes, typeFreeVars, freeLocalIds)
import Clash.Core.Name
  (Name (..), OccName, mkUnsafeInternalName, mkUnsafeSystemName)
import Clash.Core.Pretty                 (showPpr)
import Clash.Core.Subst
import Clash.Core.Term
import Clash.Core.TyCon                  (TyConMap, tyConDataCons)
import Clash.Core.Type
import Clash.Core.TysPrim                (typeNatKind)
import Clash.Core.Var                    (Id, Var(..), mkLocalId, mkTyVar)
import Clash.Core.VarEnv
import Clash.Debug                       (traceIf)
import Clash.Unique
import Clash.Util

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
                                   ,Left  (primCo nilCoTy)
                                   ]

    go n (x:xs) = mkApps (Data consCon) [Right (LitTy (NumTy n))
                                        ,Right resTy
                                        ,Right (LitTy (NumTy (n-1)))
                                        ,Left (primCo (consCoTy n))
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
                                        ,Left (primCo (consCoTy n))
                                        ,Left x
                                        ,Left (go (n-1) xs)]

    consCoTy n = head (fromJust $! dataConInstArgTys consCon
                                                   [(LitTy (NumTy n))
                                                   ,resTy
                                                   ,(LitTy (NumTy (n-1)))])

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

    lrCoTy   = head (fromJust $! dataConInstArgTys lrCon  [(LitTy (NumTy 0))
                                                         ,resTy])
    brCoTy n = head (fromJust $! dataConInstArgTys brCon
                                                   [(LitTy (NumTy n))
                                                   ,resTy
                                                   ,(LitTy (NumTy (n-1)))])

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
      is2   = unionInScope is1 (mkInScopeSet (tyFVsOfTypes inst_tys))
      subst = extendTvSubstList (mkSubst is2) (zip dcUnivTyVars inst_tys)
  go
    (substGlobalsInExistentials is0 dcExtTyVars (zip dcUnivTyVars inst_tys))
    (map (substTy subst) dcArgTys)

 where
  go
    :: [TyVar]
    -- ^ Existentials
    -> [Type]
    -- ^ Type arguments
    -> Maybe [Type]
    -- ^ Maybe ([type of non-existential])
  go exts0 args0 =
    let eqs = catMaybes (map (typeEq tcm) args0) in
    case solveNonAbsurds tcm eqs of
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
primCo ty = Prim (PrimInfo "_CO_" ty WorkNever Nothing)

-- | Make an undefined term
undefinedTm
  :: Type
  -> Term
undefinedTm = TyApp (Prim (PrimInfo "Clash.Transformations.undefined" undefinedTy WorkNever Nothing))

substArgTys
  :: DataCon
  -> [Type]
  -> [Type]
substArgTys dc args =
  let univTVs = dcUnivTyVars dc
      extTVs  = dcExtTyVars dc
      argsFVs = foldl' unionVarSet emptyVarSet
                  (map (Lens.foldMapOf typeFreeVars unitVarSet) args)
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

-- | Determine whether we should split away types from a product type, i.e.
-- clocks should always be separate arguments, and not part of a product.
shouldSplit
  :: TyConMap
  -> Type
  -- ^ Type to examine
  -> Maybe (Term,[Type])
  -- ^ If we want to split values of the given type then we have /Just/:
  --
  -- 1. The (type-applied) data-constructor which, when applied to values of
  --    the types in 2., creates a value of the examined type
  --
  -- 2. The arguments types of the product we are trying to split.
  --
  -- Note that we only split one level at a time (although we check all the way
  -- down), e.g. given /(Int, (Clock, Bool))/ we return:
  --
  -- > Just ((,) @Int @(Clock, Bool), [Int, (Clock, Bool)])
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
  -> Maybe (Term,[Type])
shouldSplit0 tcm (TyConApp tcNm tyArgs)
  | Just tc <- lookupUniqMap tcNm tcm
  , [dc] <- tyConDataCons tc
  , let dcArgs  = substArgTys dc tyArgs
  , let dcArgVs = map (tyView . coreView tcm) dcArgs
  = if any shouldSplitTy dcArgVs && not (isHidden tcNm tyArgs) then
      Just (mkApps (Data dc) (map Right tyArgs), dcArgs)
    else
      Nothing
 where
  shouldSplitTy :: TypeView -> Bool
  shouldSplitTy ty = isJust (shouldSplit0 tcm ty) || splitTy ty

  -- Hidden constructs (HiddenClock, HiddenReset, ..) don't need to be split
  -- because KnownDomain will be filtered anyway during netlist generation due
  -- to it being a zero-width type
  --
  -- TODO: This currently only handles (IP $x, KnownDomain) given that $x is any
  -- TODO: of the constructs handled in 'splitTy'. In practise this means only
  -- TODO: HiddenClock, HiddenReset, and HiddenEnable are handled. If a user were
  -- TODO: to define their own versions with -for example- the elements of the
  -- TODO: tuple swapped, 'isHidden' wouldn't recognize it. We could generalize
  -- TODO: this in the future.
  --
  isHidden :: Name a -> [Type] -> Bool
  isHidden nm [a1, a2] | TyConApp a2Nm _ <- tyView a2 =
       nameOcc nm == "GHC.Classes.(%,%)"
    && splitTy (tyView (stripIP a1))
    && nameOcc a2Nm == "Clash.Signal.Internal.KnownDomain"
  isHidden _ _ = False

  -- Currently we're only interested in splitting of Clock, Reset, and Enable
  splitTy (TyConApp tcNm0 _)
    = nameOcc tcNm0 `elem` [ "Clash.Signal.Internal.Clock"
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
    Just (_,tys) -> splitShouldSplit tcm tys ++ rest
    Nothing      -> ty : rest

-- | Strip implicit parameter wrappers (IP)
stripIP :: Type -> Type
stripIP t@(tyView -> TyConApp tcNm [_a1, a2]) =
  if nameUniq tcNm == getKey ipClassKey then a2 else t
stripIP t = t

-- | Do an inverse topological sorting of the let-bindings in a let-expression
inverseTopSortLetBindings
  :: HasCallStack
  => Term
  -> Term
inverseTopSortLetBindings (Letrec bndrs0 res) =
  let (graph,nodeMap,_) =
        Graph.graphFromEdges
          (map (\(i,e) -> let fvs = fmap varUniq
                                    (Set.elems (Lens.setOf freeLocalIds e) )
                          in  ((i,e),varUniq i,fvs)) bndrs0)
      nodes  = postOrd graph
      bndrs1 = map ((\(x,_,_) -> x) . nodeMap) nodes
  in  Letrec bndrs1 res
 where
  postOrd :: Graph.Graph -> [Graph.Vertex]
  postOrd g = postorderF (Graph.dff g) []

  postorderF :: Graph.Forest a -> [a] -> [a]
  postorderF ts = foldr (.) id (map postorder ts)

  postorder :: Graph.Tree a -> [a] -> [a]
  postorder (Graph.Node a ts) = postorderF ts . (a :)

inverseTopSortLetBindings e = e
{-# SCC inverseTopSortLetBindings #-}

-- | Group let-bindings into cyclic groups and acyclic individual bindings
sccLetBindings
  :: HasCallStack
  => [LetBinding]
  -> [Graph.SCC LetBinding]
sccLetBindings =
  Graph.stronglyConnComp .
  (map (\(i,e) -> let fvs = fmap varUniq
                            (Set.elems (Lens.setOf freeLocalIds e) )
                  in  ((i,e),varUniq i,fvs)))
{-# SCC sccLetBindings #-}
