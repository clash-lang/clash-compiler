{-|
  Copyright  :  (C) 2015-2016, University of Twente,
                    2016     , Myrtle Software Ltd
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Reductions of primitives

  Currently, it contains reductions for:

    * Clash.Sized.Vector.map
    * Clash.Sized.Vector.zipWith
    * Clash.Sized.Vector.traverse#
    * Clash.Sized.Vector.foldr
    * Clash.Sized.Vector.fold
    * Clash.Sized.Vector.dfold
    * Clash.Sized.Vector.(++)
    * Clash.Sized.Vector.head
    * Clash.Sized.Vector.tail
    * Clash.Sized.Vector.unconcatBitVector#
    * Clash.Sized.Vector.replicate
    * Clash.Sized.Vector.imap
    * Clash.Sized.Vector.dtfold
    * Clash.Sized.RTree.tfold

  Partially handles:

    * Clash.Sized.Vector.unconcat
    * Clash.Sized.Vector.transpose
-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Normalize.PrimitiveReductions where

import qualified Control.Lens                     as Lens
import           Data.List                        (mapAccumR)
import qualified Data.Maybe                       as Maybe

import           Clash.Core.DataCon               (DataCon, dataConInstArgTys)
import           Clash.Core.Literal               (Literal (..))
import           Clash.Core.Pretty                (showPpr)
import           Clash.Core.Term                  (Term (..), Pat (..))
import           Clash.Core.Type                  (LitTy (..), Type (..),
                                                   TypeView (..), coreView,
                                                   mkFunTy, mkTyConApp,
                                                   splitFunForallTy, tyView,
                                                   undefinedTy)
import           Clash.Core.TyCon                 (TyConName, tyConDataCons)
import           Clash.Core.TysPrim               (integerPrimTy, typeNatKind)
import           Clash.Core.Util
  (appendToVec, extractElems, extractTElems, idToVar, mkApps, mkRTree,
   mkUniqInternalId, mkUniqSystemTyVar, mkVec, termType)
import           Clash.Core.Var                   (Var (..))
import           Clash.Core.VarEnv
  (InScopeSet, extendInScopeSetList)

import           Clash.Normalize.Types
import           Clash.Rewrite.Types
import           Clash.Rewrite.Util
import           Clash.Unique
import           Clash.Util

-- | Replace an application of the @Clash.Sized.Vector.zipWith@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.zipWith@
reduceZipWith
  :: InScopeSet
  -> Integer  -- ^ Length of the vector(s)
  -> Type -- ^ Type of the lhs of the function
  -> Type -- ^ Type of the rhs of the function
  -> Type -- ^ Type of the result of the function
  -> Term -- ^ The zipWith'd functions
  -> Term -- ^ The 1st vector argument
  -> Term -- ^ The 2nd vector argument
  -> NormalizeSession Term
reduceZipWith inScope0 n lhsElTy rhsElTy resElTy fun lhsArg rhsArg = do
    tcm <- Lens.view tcCache
    let ty = termType tcm lhsArg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , [nilCon,consCon] <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let (uniqs1,(varsL,elemsL)) = second (second concat . unzip)
                                    $ extractElems uniqs0 inScope0 consCon lhsElTy 'L' n lhsArg
            inScope1 = extendInScopeSetList inScope0 (map fst elemsL)
            (uniqs2,(varsR,elemsR)) = second (second concat . unzip)
                                    $ extractElems uniqs1 inScope1 consCon rhsElTy 'R' n rhsArg
            funApps          = zipWith (\l r -> mkApps fun [Left l,Left r]) varsL varsR
            lbody            = mkVec nilCon consCon resElTy n funApps
            lb               = Letrec (init elemsL ++ init elemsR) lbody
        uniqSupply Lens..= uniqs2
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceZipWith: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.map@ primitive on vectors
-- of a known length @n@, by the fully unrolled recursive "definition" of
-- @Clash.Sized.Vector.map@
reduceMap
  :: InScopeSet
  -> Integer  -- ^ Length of the vector
  -> Type -- ^ Argument type of the function
  -> Type -- ^ Result type of the function
  -> Term -- ^ The map'd function
  -> Term -- ^ The map'd over vector
  -> NormalizeSession Term
reduceMap inScope n argElTy resElTy fun arg = do
    tcm <- Lens.view tcCache
    let ty = termType tcm arg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- lookupUniqMap vecTcNm tcm
      , [nilCon,consCon] <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let (uniqs1,(vars,elems)) = second (second concat . unzip)
                                  $ extractElems uniqs0 inScope consCon argElTy 'A' n arg
            funApps          = map (fun `App`) vars
            lbody            = mkVec nilCon consCon resElTy n funApps
            lb               = Letrec (init elems) lbody
        uniqSupply Lens..= uniqs1
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceMap: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.imap@ primitive on vectors
-- of a known length @n@, by the fully unrolled recursive "definition" of
-- @Clash.Sized.Vector.imap@
reduceImap
  :: InScopeSet
  -> Integer  -- ^ Length of the vector
  -> Type -- ^ Argument type of the function
  -> Type -- ^ Result type of the function
  -> Term -- ^ The imap'd function
  -> Term -- ^ The imap'd over vector
  -> NormalizeSession Term
reduceImap inScope n argElTy resElTy fun arg = do
    tcm <- Lens.view tcCache
    let ty = termType tcm arg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- lookupUniqMap vecTcNm tcm
      , [nilCon,consCon] <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let (uniqs1,nTv)     = mkUniqSystemTyVar (uniqs0,inScope) ("n",typeNatKind)
            (uniqs2,(vars,elems)) = second (second concat . unzip)
                                  $ uncurry extractElems uniqs1 consCon argElTy 'I' n arg
            (Right idxTy:_,_) = splitFunForallTy (termType tcm fun)
            (TyConApp idxTcNm _) = tyView idxTy
            -- fromInteger# :: KnownNat n => Integer -> Index n
            idxFromIntegerTy = ForAllTy nTv
                                        (foldr mkFunTy
                                               (mkTyConApp idxTcNm
                                                           [VarTy nTv])
                                               [integerPrimTy,integerPrimTy])
            idxFromInteger   = Prim "Clash.Sized.Internal.Index.fromInteger#"
                                    idxFromIntegerTy
            idxs             = map (App (App (TyApp idxFromInteger (LitTy (NumTy n)))
                                             (Literal (IntegerLiteral (toInteger n))))
                                   . Literal . IntegerLiteral . toInteger) [0..(n-1)]

            funApps          = zipWith (\i v -> App (App fun i) v) idxs vars
            lbody            = mkVec nilCon consCon resElTy n funApps
            lb               = Letrec (init elems) lbody
        uniqSupply Lens..= uniqs2
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceImap: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.traverse#@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.traverse#@
reduceTraverse
  :: InScopeSet
  -> Integer  -- ^ Length of the vector
  -> Type -- ^ Element type of the argument vector
  -> Type -- ^ The type of the applicative
  -> Type -- ^ Element type of the result vector
  -> Term -- ^ The @Applicative@ dictionary
  -> Term -- ^ The function to traverse with
  -> Term -- ^ The argument vector
  -> NormalizeSession Term
reduceTraverse inScope n aTy fTy bTy dict fun arg = do
    tcm <- Lens.view tcCache
    let (TyConApp apDictTcNm _) = tyView (termType tcm dict)
        ty = termType tcm arg
    go tcm apDictTcNm ty
  where
    go tcm apDictTcNm (coreView tcm -> Just ty') = go tcm apDictTcNm ty'
    go tcm apDictTcNm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , [nilCon,consCon] <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let (Just apDictTc)    = lookupUniqMap apDictTcNm tcm
            [apDictCon]        = tyConDataCons apDictTc
            (Just apDictIdTys) = dataConInstArgTys apDictCon [fTy]
            (uniqs1,apDictIds@[functorDictId,pureId,apId,_,_]) =
              mapAccumR mkUniqInternalId (uniqs0,inScope)
                (zip ["functorDict","pure","ap","apConstL","apConstR"]
                     apDictIdTys)

            (TyConApp funcDictTcNm _) = tyView (head apDictIdTys)
            (Just funcDictTc) = lookupUniqMap funcDictTcNm tcm
            [funcDictCon] = tyConDataCons funcDictTc
            (Just funcDictIdTys) = dataConInstArgTys funcDictCon [fTy]
            (uniqs2,funcDicIds@[fmapId,_]) =
              mapAccumR mkUniqInternalId uniqs1
                (zip ["fmap","fmapConst"] funcDictIdTys)

            apPat    = DataPat apDictCon   [] apDictIds
            fnPat    = DataPat funcDictCon [] funcDicIds

            -- Extract the 'pure' function from the Applicative dictionary
            pureTy = varType pureId
            pureTm = Case dict pureTy [(apPat,Var pureId)]

            -- Extract the '<*>' function from the Applicative dictionary
            apTy   = varType apId
            apTm   = Case dict apTy [(apPat, Var apId)]

            -- Extract the Functor dictionary from the Applicative dictionary
            funcTy = varType functorDictId
            funcTm = Case dict funcTy
                               [(apPat,Var functorDictId)]

            -- Extract the 'fmap' function from the Functor dictionary
            fmapTy = varType fmapId
            fmapTm = Case (Var functorDictId) fmapTy
                          [(fnPat, Var fmapId)]

            (uniqs3,(vars,elems)) = second (second concat . unzip)
                                  $ uncurry extractElems uniqs2 consCon aTy 'T' n arg

            funApps = map (fun `App`) vars

            lbody   = mkTravVec vecTcNm nilCon consCon (Var (apDictIds!!1))
                                                       (Var (apDictIds!!2))
                                                       (Var (funcDicIds!!0))
                                                       bTy n funApps

            lb      = Letrec ([((apDictIds!!0), funcTm)
                              ,((apDictIds!!1), pureTm)
                              ,((apDictIds!!2), apTm)
                              ,((funcDicIds!!0), fmapTm)
                              ] ++ init elems) lbody
        uniqSupply Lens..= uniqs3
        changed lb
    go _ _ ty = error $ $(curLoc) ++ "reduceTraverse: argument does not have a vector type: " ++ showPpr ty

-- | Create the traversable vector
--
-- e.g. for a length '2' input vector, we get
--
-- > (:>) <$> x0 <*> ((:>) <$> x1 <*> pure Nil)
mkTravVec :: TyConName -- ^ Vec tcon
          -> DataCon   -- ^ Nil con
          -> DataCon   -- ^ Cons con
          -> Term      -- ^ 'pure' term
          -> Term      -- ^ '<*>' term
          -> Term      -- ^ 'fmap' term
          -> Type      -- ^ 'b' ty
          -> Integer       -- ^ Length of the vector
          -> [Term]    -- ^ Elements of the vector
          -> Term
mkTravVec vecTc nilCon consCon pureTm apTm fmapTm bTy = go
  where
    go :: Integer -> [Term] -> Term
    go _ [] = mkApps pureTm [Right (mkTyConApp vecTc [LitTy (NumTy 0),bTy])
                            ,Left  (mkApps (Data nilCon)
                                           [Right (LitTy (NumTy 0))
                                           ,Right bTy
                                           ,Left  (Prim "_CO_" nilCoTy)])]

    go n (x:xs) = mkApps apTm
      [Right (mkTyConApp vecTc [LitTy (NumTy (n-1)),bTy])
      ,Right (mkTyConApp vecTc [LitTy (NumTy n),bTy])
      ,Left (mkApps fmapTm [Right bTy
                           ,Right (mkFunTy (mkTyConApp vecTc [LitTy (NumTy (n-1)),bTy])
                                           (mkTyConApp vecTc [LitTy (NumTy n),bTy]))
                           ,Left  (mkApps (Data consCon)
                                          [Right (LitTy (NumTy n))
                                          ,Right bTy
                                          ,Right (LitTy (NumTy (n-1)))
                                          ,Left  (Prim "_CO_" (consCoTy n))
                                          ])
                           ,Left  x])
      ,Left (go (n-1) xs)]

    nilCoTy = head (Maybe.fromJust (dataConInstArgTys nilCon [(LitTy (NumTy 0))
                                                             ,bTy]))

    consCoTy n = head (Maybe.fromJust (dataConInstArgTys consCon
                                                         [(LitTy (NumTy n))
                                                         ,bTy
                                                         ,(LitTy (NumTy (n-1)))]))

-- | Replace an application of the @Clash.Sized.Vector.foldr@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.foldr@
reduceFoldr
  :: InScopeSet
  -> Integer
  -- ^ Length of the vector
  -> Type
  -- ^ Element type of the argument vector
  -> Term
  -- ^ The function to fold with
  -> Term
  -- ^ The starting value
  -> Term
  -- ^ The argument vector
  -> NormalizeSession Term
reduceFoldr _ 0 _ _ start _ = changed start
reduceFoldr inScope n aTy fun start arg = do
    tcm <- Lens.view tcCache
    let ty = termType tcm arg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , [_,consCon] <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let (uniqs1,(vars,elems)) = second (second concat . unzip)
                                  $ extractElems uniqs0 inScope consCon aTy 'G' n arg
            lbody            = foldr (\l r -> mkApps fun [Left l,Left r]) start vars
            lb               = Letrec (init elems) lbody
        uniqSupply Lens..= uniqs1
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceFoldr: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.fold@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.fold@
reduceFold
  :: InScopeSet
  -> Integer
  -- ^ Length of the vector
  -> Type
  -- ^ Element type of the argument vector
  -> Term
  -- ^ The function to fold with
  -> Term
  -- ^ The argument vector
  -> NormalizeSession Term
reduceFold inScope n aTy fun arg = do
    tcm <- Lens.view tcCache
    let ty = termType tcm arg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , [_,consCon]  <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let (uniqs1,(vars,elems)) = second (second concat . unzip)
                                  $ extractElems uniqs0 inScope consCon aTy 'F' n arg
            lbody            = foldV vars
            lb               = Letrec (init elems) lbody
        uniqSupply Lens..= uniqs1
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceFold: argument does not have a vector type: " ++ showPpr ty

    foldV [a] = a
    foldV as  = let (l,r) = splitAt (length as `div` 2) as
                    lF    = foldV l
                    rF    = foldV r
                in  mkApps fun [Left lF, Left rF]

-- | Replace an application of the @Clash.Sized.Vector.dfold@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.dfold@
reduceDFold
  :: InScopeSet
  -> Integer
  -- ^ Length of the vector
  -> Type
  -- ^ Element type of the argument vector
  -> Term
  -- ^ Function to fold with
  -> Term
  -- ^ Starting value
  -> Term
  -- ^ The vector to fold
  -> NormalizeSession Term
reduceDFold _ 0 _ _ start _ = changed start
reduceDFold inScope n aTy fun start arg = do
    tcm <- Lens.view tcCache
    let ty = termType tcm arg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , [_,consCon]  <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let (uniqs1,(vars,elems)) = second (second concat . unzip)
                                  $ extractElems uniqs0 inScope consCon aTy 'D' n arg
            (_ltv:Right snTy:_,_) = splitFunForallTy (termType tcm fun)
            (TyConApp snatTcNm _) = tyView snTy
            (Just snatTc)         = lookupUniqMap snatTcNm tcm
            [snatDc]              = tyConDataCons snatTc
            lbody = doFold (buildSNat snatDc) (n-1) vars
            lb    = Letrec (init elems) lbody
        uniqSupply Lens..= uniqs1
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceDFold: argument does not have a vector type: " ++ showPpr ty

    doFold _    _ []     = start
    doFold snDc k (x:xs) = mkApps fun
                                 [Right (LitTy (NumTy k))
                                 ,Left (snDc k)
                                 ,Left x
                                 ,Left (doFold snDc (k-1) xs)
                                 ]

-- | Replace an application of the @Clash.Sized.Vector.head@ primitive on
-- vectors of a known length @n@, by a projection of the first element of a
-- vector.
reduceHead
  :: InScopeSet
  -> Integer  -- ^ Length of the vector
  -> Type -- ^ Element type of the vector
  -> Term -- ^ The argument vector
  -> NormalizeSession Term
reduceHead inScope n aTy vArg = do
    tcm <- Lens.view tcCache
    let ty = termType tcm vArg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , [_,consCon]  <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let (uniqs1,(vars,elems)) = second (second concat . unzip)
                                  $ extractElems uniqs0 inScope consCon aTy 'H' n vArg
            lb = Letrec [head elems] (head vars)
        uniqSupply Lens..= uniqs1
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceHead: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.tail@ primitive on
-- vectors of a known length @n@, by a projection of the tail of a
-- vector.
reduceTail
  :: InScopeSet
  -> Integer  -- ^ Length of the vector
  -> Type -- ^ Element type of the vector
  -> Term -- ^ The argument vector
  -> NormalizeSession Term
reduceTail inScope n aTy vArg = do
    tcm <- Lens.view tcCache
    let ty = termType tcm vArg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , [_,consCon]  <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let (uniqs1,(_,elems)) = second (second concat . unzip)
                               $ extractElems uniqs0 inScope consCon aTy 'L' n vArg
            b@(tB,_)     = elems !! 1
            lb           = Letrec [b] (Var tB)
        uniqSupply Lens..= uniqs1
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceTail: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.last@ primitive on
-- vectors of a known length @n@, by a projection of the last element of a
-- vector.
reduceLast
  :: InScopeSet
  -> Integer  -- ^ Length of the vector
  -> Type -- ^ Element type of the vector
  -> Term -- ^ The argument vector
  -> NormalizeSession Term
reduceLast inScope n aTy vArg = do
    tcm <- Lens.view tcCache
    let ty = termType tcm vArg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , [_,consCon]  <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let (uniqs1,(_,elems)) = second unzip
                               $ extractElems uniqs0 inScope consCon aTy 'L' n vArg
            (tB,_)       = head (last elems)
        uniqSupply Lens..= uniqs1
        case n of
         0 -> changed (mkApps (Prim "Clash.Transformations.undefined" undefinedTy) [Right aTy])
         _ -> changed (Letrec (init (concat elems)) (Var tB))
    go _ ty = error $ $(curLoc) ++ "reduceLast: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.init@ primitive on
-- vectors of a known length @n@, by a projection of the init of a
-- vector.
reduceInit
  :: InScopeSet
  -> Integer  -- ^ Length of the vector
  -> Type -- ^ Element type of the vector
  -> Term -- ^ The argument vector
  -> NormalizeSession Term
reduceInit inScope n aTy vArg = do
    tcm <- Lens.view tcCache
    let ty = termType tcm vArg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , [nilCon,consCon]  <- tyConDataCons vecTc
      = do
        uniqs0 <- Lens.use uniqSupply
        let (uniqs1,(_,elems)) = second unzip
                               $ extractElems uniqs0 inScope consCon aTy 'L' n vArg
        uniqSupply Lens..= uniqs1
        case n of
         0 -> changed (mkApps (Prim "Clash.Transformations.undefined" undefinedTy) [Right aTy])
         1 -> changed (mkVec nilCon consCon aTy 0 [])
         _ -> let el = init elems
                  iv = mkVec nilCon consCon aTy (n-1) (map (idToVar . fst . head) el)
                  lb = init (concat el)
              in  changed (Letrec lb iv)

    go _ ty = error $ $(curLoc) ++ "reduceInit: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.(++)@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.(++)@
reduceAppend
  :: InScopeSet
  -> Integer  -- ^ Length of the LHS arg
  -> Integer  -- ^ Lenght of the RHS arg
  -> Type -- ^ Element type of the vectors
  -> Term -- ^ The LHS argument
  -> Term -- ^ The RHS argument
  -> NormalizeSession Term
reduceAppend inScope n m aTy lArg rArg = do
    tcm <- Lens.view tcCache
    let ty = termType tcm lArg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , [_,consCon]  <- tyConDataCons vecTc
      = do uniqs0 <- Lens.use uniqSupply
           let (uniqs1,(vars,elems)) = second (second concat . unzip)
                                     $ extractElems uniqs0 inScope consCon aTy
                                         'C' n lArg
               lbody        = appendToVec consCon aTy rArg (n+m) vars
               lb           = Letrec (init elems) lbody
           uniqSupply Lens..= uniqs1
           changed lb
    go _ ty = error $ $(curLoc) ++ "reduceAppend: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.unconcat@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.unconcat@
reduceUnconcat :: Integer  -- ^ Length of the result vector
               -> Integer  -- ^ Length of the elements of the result vector
               -> Type -- ^ Element type
               -> Term -- ^ Argument vector
               -> NormalizeSession Term
reduceUnconcat n 0 aTy arg = do
    tcm <- Lens.view tcCache
    let ty = termType tcm arg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- lookupUniqMap vecTcNm tcm
      , [nilCon,consCon] <- tyConDataCons vecTc
      = let nilVec           = mkVec nilCon consCon aTy 0 []
            innerVecTy       = mkTyConApp vecTcNm [LitTy (NumTy 0), aTy]
            retVec           = mkVec nilCon consCon innerVecTy n (replicate (fromInteger n) nilVec)
        in  changed retVec
    go _ ty = error $ $(curLoc) ++ "reduceUnconcat: argument does not have a vector type: " ++ showPpr ty

reduceUnconcat _ _ _ _ = error $ $(curLoc) ++ "reduceUnconcat: unimplemented"

-- | Replace an application of the @Clash.Sized.Vector.transpose@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.transpose@
reduceTranspose :: Integer  -- ^ Length of the result vector
                -> Integer  -- ^ Length of the elements of the result vector
                -> Type -- ^ Element type
                -> Term -- ^ Argument vector
                -> NormalizeSession Term
reduceTranspose n 0 aTy arg = do
    tcm <- Lens.view tcCache
    let ty = termType tcm arg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- lookupUniqMap vecTcNm tcm
      , [nilCon,consCon] <- tyConDataCons vecTc
      = let nilVec           = mkVec nilCon consCon aTy 0 []
            innerVecTy       = mkTyConApp vecTcNm [LitTy (NumTy 0), aTy]
            retVec           = mkVec nilCon consCon innerVecTy n (replicate (fromInteger n) nilVec)
        in  changed retVec
    go _ ty = error $ $(curLoc) ++ "reduceTranspose: argument does not have a vector type: " ++ showPpr ty

reduceTranspose _ _ _ _ = error $ $(curLoc) ++ "reduceTranspose: unimplemented"

reduceReplicate :: Integer
                -> Type
                -> Type
                -> Term
                -> NormalizeSession Term
reduceReplicate n aTy eTy arg = do
    tcm <- Lens.view tcCache
    go tcm eTy
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- lookupUniqMap vecTcNm tcm
      , [nilCon,consCon] <- tyConDataCons vecTc
      = let retVec = mkVec nilCon consCon aTy n (replicate (fromInteger n) arg)
        in  changed retVec
    go _ ty = error $ $(curLoc) ++ "reduceReplicate: argument does not have a vector type: " ++ showPpr ty

-- | Replace an application of the @Clash.Sized.Vector.dtfold@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.Vector.dtfold@
reduceDTFold
  :: InScopeSet
  -> Integer  -- ^ Length of the vector
  -> Type     -- ^ Element type of the argument vector
  -> Term     -- ^ Function to convert elements with
  -> Term     -- ^ Function to combine branches with
  -> Term     -- ^ The vector to fold
  -> NormalizeSession Term
reduceDTFold inScope n aTy lrFun brFun arg = do
    tcm <- Lens.view tcCache
    let ty = termType tcm arg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- lookupUniqMap vecTcNm tcm
      , [_,consCon]  <- tyConDataCons vecTc
      = do uniqs0 <- Lens.use uniqSupply
           let (uniqs1,(vars,elems)) = second (second concat . unzip)
                                     $ extractElems uniqs0 inScope consCon aTy
                                         'T' (2^n) arg
               (_ltv:Right snTy:_,_) = splitFunForallTy (termType tcm brFun)
               (TyConApp snatTcNm _) = tyView snTy
               (Just snatTc)         = lookupUniqMap snatTcNm tcm
               [snatDc]              = tyConDataCons snatTc
               lbody = doFold (buildSNat snatDc) (n-1) vars
               lb    = Letrec (init elems) lbody
           uniqSupply Lens..= uniqs1
           changed lb
    go _ ty = error $ $(curLoc) ++ "reduceDTFold: argument does not have a vector type: " ++ showPpr ty

    doFold :: (Integer -> Term) -> Integer -> [Term] -> Term
    doFold _    _ [x] = mkApps lrFun [Left x]
    doFold snDc k xs  =
      let (xsL,xsR) = splitAt (2^k) xs
          k'        = k-1
          eL        = doFold snDc k' xsL
          eR        = doFold snDc k' xsR
      in  mkApps brFun [Right (LitTy (NumTy k))
                       ,Left  (snDc k)
                       ,Left  eL
                       ,Left  eR
                       ]

-- | Replace an application of the @Clash.Sized.RTree.tdfold@ primitive on
-- trees of a known depth @n@, by the fully unrolled recursive "definition"
-- of @Clash.Sized.RTree.tdfold@
reduceTFold
  :: InScopeSet
  -> Integer -- ^ Depth of the tree
  -> Type    -- ^ Element type of the argument tree
  -> Term    -- ^ Function to convert elements with
  -> Term    -- ^ Function to combine branches with
  -> Term    -- ^ The tree to fold
  -> NormalizeSession Term
reduceTFold inScope n aTy lrFun brFun arg = do
    tcm <- Lens.view tcCache
    let ty = termType tcm arg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp treeTcNm _)
      | (Just treeTc) <- lookupUniqMap treeTcNm tcm
      , [lrCon,brCon] <- tyConDataCons treeTc
      = do uniqs0 <- Lens.use uniqSupply
           let (uniqs1,(vars,elems)) = extractTElems uniqs0 inScope lrCon brCon aTy 'T' n arg
               (_ltv:Right snTy:_,_) = splitFunForallTy (termType tcm brFun)
               (TyConApp snatTcNm _) = tyView snTy
               (Just snatTc)         = lookupUniqMap snatTcNm tcm
               [snatDc]              = tyConDataCons snatTc
               lbody = doFold (buildSNat snatDc) (n-1) vars
               lb    = Letrec elems lbody
           uniqSupply Lens..= uniqs1
           changed lb
    go _ ty = error $ $(curLoc) ++ "reduceTFold: argument does not have a tree type: " ++ showPpr ty

    doFold _    _ [x] = mkApps lrFun [Left x]
    doFold snDc k xs  =
      let (xsL,xsR) = splitAt (length xs `div` 2) xs
          k'        = k-1
          eL        = doFold snDc k' xsL
          eR        = doFold snDc k' xsR
      in  mkApps brFun [Right (LitTy (NumTy k))
                       ,Left (snDc k)
                       ,Left eL
                       ,Left eR
                       ]

reduceTReplicate :: Integer -- ^ Depth of the tree
                 -> Type    -- ^ Element type
                 -> Type    -- ^ Result type
                 -> Term    -- ^ Element
                 -> NormalizeSession Term
reduceTReplicate n aTy eTy arg = do
    tcm <- Lens.view tcCache
    go tcm eTy
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp treeTcNm _)
      | (Just treeTc) <- lookupUniqMap treeTcNm tcm
      , [lrCon,brCon] <- tyConDataCons treeTc
      = let retVec = mkRTree lrCon brCon aTy n (replicate (2^n) arg)
        in  changed retVec
    go _ ty = error $ $(curLoc) ++ "reduceTReplicate: argument does not have a vector type: " ++ showPpr ty

buildSNat :: DataCon -> Integer -> Term
buildSNat snatDc i =
  mkApps (Data snatDc)
         [Right (LitTy (NumTy i))
#if MIN_VERSION_ghc(8,2,0)
         ,Left (Literal (NaturalLiteral (toInteger i)))
#else
         ,Left (Literal (IntegerLiteral (toInteger i)))
#endif
         ]
