{-|
  Copyright  :  (C) 2015-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Reductions of primitives

  Currently, it contains reductions for:

    * CLaSH.Sized.Vector.map
    * CLaSH.Sized.Vector.zipWith
    * CLaSH.Sized.Vector.traverse#
    * CLaSH.Sized.Vector.foldr
    * CLaSH.Sized.Vector.fold
    * CLaSH.Sized.Vector.dfold
    * CLaSH.Sized.Vector.(++)
    * CLaSH.Sized.Vector.head
    * CLaSH.Sized.Vector.tail
    * CLaSH.Sized.Vector.unconcatBitVector#
    * CLaSH.Sized.Vector.replicate
    * CLaSH.Sized.Vector.imap
    * CLaSH.Sized.Vector.dtfold
    * CLaSH.Sized.RTree.tfold

  Partially handles:

    * CLaSH.Sized.Vector.unconcat
    * CLaSH.Sized.Vector.transpose
-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module CLaSH.Normalize.PrimitiveReductions where

import qualified Control.Lens                     as Lens
import qualified Data.HashMap.Lazy                as HashMap
import qualified Data.Maybe                       as Maybe
import           Unbound.Generics.LocallyNameless (bind, embed, rec, rebind,
                                                   string2Name)

import           CLaSH.Core.DataCon               (DataCon, dataConInstArgTys)
import           CLaSH.Core.Literal               (Literal (..))
import           CLaSH.Core.Pretty                (showDoc)
import           CLaSH.Core.Term                  (Term (..), Pat (..))
import           CLaSH.Core.Type                  (LitTy (..), Type (..),
                                                   TypeView (..), coreView,
                                                   mkFunTy, mkTyConApp,
                                                   splitFunForallTy, tyView,
                                                   undefinedTy)
import           CLaSH.Core.TyCon                 (TyConName, tyConDataCons)
import           CLaSH.Core.TysPrim               (integerPrimTy, typeNatKind)
import           CLaSH.Core.Util                  (appendToVec, extractElems,
                                                   extractTElems, idToVar,
                                                   mkApps, mkRTree, mkVec,
                                                   termType)
import           CLaSH.Core.Var                   (Var (..))

import           CLaSH.Normalize.Types
import           CLaSH.Rewrite.Types
import           CLaSH.Rewrite.Util
import           CLaSH.Util

-- | Replace an application of the @CLaSH.Sized.Vector.zipWith@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @CLaSH.Sized.Vector.zipWith@
reduceZipWith :: Integer  -- ^ Length of the vector(s)
              -> Type -- ^ Type of the lhs of the function
              -> Type -- ^ Type of the rhs of the function
              -> Type -- ^ Type of the result of the function
              -> Term -- ^ The zipWith'd functions
              -> Term -- ^ The 1st vector argument
              -> Term -- ^ The 2nd vector argument
              -> NormalizeSession Term
reduceZipWith n lhsElTy rhsElTy resElTy fun lhsArg rhsArg = do
    tcm <- Lens.view tcCache
    ty  <- termType tcm lhsArg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- HashMap.lookup vecTcNm tcm
      , [nilCon,consCon] <- tyConDataCons vecTc
      = let (varsL,elemsL)   = second concat . unzip
                             $ extractElems consCon lhsElTy 'L' n lhsArg
            (varsR,elemsR)   = second concat . unzip
                             $ extractElems consCon rhsElTy 'R' n rhsArg
            funApps          = zipWith (\l r -> mkApps fun [Left l,Left r]) varsL varsR
            lbody            = mkVec nilCon consCon resElTy n funApps
            lb               = Letrec (bind (rec (init elemsL ++ init elemsR)) lbody)
        in  changed lb
    go _ ty = error $ $(curLoc) ++ "reduceZipWith: argument does not have a vector type: " ++ showDoc ty

-- | Replace an application of the @CLaSH.Sized.Vector.map@ primitive on vectors
-- of a known length @n@, by the fully unrolled recursive "definition" of
-- @CLaSH.Sized.Vector.map@
reduceMap :: Integer  -- ^ Length of the vector
          -> Type -- ^ Argument type of the function
          -> Type -- ^ Result type of the function
          -> Term -- ^ The map'd function
          -> Term -- ^ The map'd over vector
          -> NormalizeSession Term
reduceMap n argElTy resElTy fun arg = do
    tcm <- Lens.view tcCache
    ty  <- termType tcm arg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- HashMap.lookup vecTcNm tcm
      , [nilCon,consCon] <- tyConDataCons vecTc
      = let (vars,elems)     = second concat . unzip
                             $ extractElems consCon argElTy 'A' n arg
            funApps          = map (fun `App`) vars
            lbody            = mkVec nilCon consCon resElTy n funApps
            lb               = Letrec (bind (rec (init elems)) lbody)
        in  changed lb
    go _ ty = error $ $(curLoc) ++ "reduceMap: argument does not have a vector type: " ++ showDoc ty

-- | Replace an application of the @CLaSH.Sized.Vector.imap@ primitive on vectors
-- of a known length @n@, by the fully unrolled recursive "definition" of
-- @CLaSH.Sized.Vector.imap@
reduceImap :: Integer  -- ^ Length of the vector
           -> Type -- ^ Argument type of the function
           -> Type -- ^ Result type of the function
           -> Term -- ^ The imap'd function
           -> Term -- ^ The imap'd over vector
           -> NormalizeSession Term
reduceImap n argElTy resElTy fun arg = do
    tcm <- Lens.view tcCache
    ty  <- termType tcm arg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- HashMap.lookup vecTcNm tcm
      , [nilCon,consCon] <- tyConDataCons vecTc
      = do
        let (vars,elems)     = second concat . unzip
                             $ extractElems consCon argElTy 'I' n arg
        (Right idxTy:_,_) <- splitFunForallTy <$> termType tcm fun
        let (TyConApp idxTcNm _) = tyView idxTy
            nTv              = string2Name "n"
            -- fromInteger# :: KnownNat n => Integer -> Index n
            idxFromIntegerTy = ForAllTy (bind (TyVar nTv (embed typeNatKind))
                                         (foldr mkFunTy
                                                (mkTyConApp idxTcNm
                                                            [VarTy typeNatKind nTv])
                                                [integerPrimTy,integerPrimTy]))
            idxFromInteger   = Prim "CLaSH.Sized.Internal.Index.fromInteger#"
                                    idxFromIntegerTy
            idxs             = map (App (App (TyApp idxFromInteger (LitTy (NumTy n)))
                                             (Literal (IntegerLiteral (toInteger n))))
                                   . Literal . IntegerLiteral . toInteger) [0..(n-1)]

            funApps          = zipWith (\i v -> App (App fun i) v) idxs vars
            lbody            = mkVec nilCon consCon resElTy n funApps
            lb               = Letrec (bind (rec (init elems)) lbody)
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceImap: argument does not have a vector type: " ++ showDoc ty

-- | Replace an application of the @CLaSH.Sized.Vector.traverse#@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @CLaSH.Sized.Vector.traverse#@
reduceTraverse :: Integer  -- ^ Length of the vector
               -> Type -- ^ Element type of the argument vector
               -> Type -- ^ The type of the applicative
               -> Type -- ^ Element type of the result vector
               -> Term -- ^ The @Applicative@ dictionary
               -> Term -- ^ The function to traverse with
               -> Term -- ^ The argument vector
               -> NormalizeSession Term
reduceTraverse n aTy fTy bTy dict fun arg = do
    tcm <- Lens.view tcCache
    (TyConApp apDictTcNm _) <- tyView <$> termType tcm dict
    ty <- termType tcm arg
    go tcm apDictTcNm ty
  where
    go tcm apDictTcNm (coreView tcm -> Just ty') = go tcm apDictTcNm ty'
    go tcm apDictTcNm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- HashMap.lookup vecTcNm tcm
      , [nilCon,consCon] <- tyConDataCons vecTc
      = let (Just apDictTc)    = HashMap.lookup apDictTcNm tcm
            [apDictCon]        = tyConDataCons apDictTc
            (Just apDictIdTys) = dataConInstArgTys apDictCon [fTy]
            apDictIds          = zipWith Id (map string2Name ["functorDict"
                                                             ,"pure"
                                                             ,"ap"
                                                             ,"apConstL"
                                                             ,"apConstR"])
                                            (map embed apDictIdTys)

            (TyConApp funcDictTcNm _) = tyView (head apDictIdTys)
            (Just funcDictTc) = HashMap.lookup funcDictTcNm tcm
            [funcDictCon] = tyConDataCons funcDictTc
            (Just funcDictIdTys) = dataConInstArgTys funcDictCon [fTy]
            funcDicIds    = zipWith Id (map string2Name ["fmap","fmapConst"])
                                       (map embed funcDictIdTys)

            apPat    = DataPat (embed apDictCon) (rebind [] apDictIds)
            fnPat    = DataPat (embed funcDictCon) (rebind [] funcDicIds)

            -- Extract the 'pure' function from the Applicative dictionary
            pureTy = apDictIdTys!!1
            pureTm = Case dict pureTy [bind apPat (Var pureTy (string2Name "pure"))]

            -- Extract the '<*>' function from the Applicative dictionary
            apTy   = apDictIdTys!!2
            apTm   = Case dict apTy [bind apPat (Var apTy (string2Name "ap"))]

            -- Extract the Functor dictionary from the Applicative dictionary
            funcTy = (head apDictIdTys)
            funcTm = Case dict funcTy
                               [bind apPat (Var funcTy (string2Name "functorDict"))]

            -- Extract the 'fmap' function from the Functor dictionary
            fmapTy = (head funcDictIdTys)
            fmapTm = Case (Var funcTy (string2Name "functorDict")) fmapTy
                          [bind fnPat (Var fmapTy (string2Name "fmap"))]

            (vars,elems) = second concat . unzip
                         $ extractElems consCon aTy 'T' n arg

            funApps = map (fun `App`) vars

            lbody   = mkTravVec vecTcNm nilCon consCon (idToVar (apDictIds!!1))
                                                       (idToVar (apDictIds!!2))
                                                       (idToVar (funcDicIds!!0))
                                                       bTy n funApps

            lb      = Letrec (bind (rec ([((apDictIds!!0),embed funcTm)
                                         ,((apDictIds!!1),embed pureTm)
                                         ,((apDictIds!!2),embed apTm)
                                         ,((funcDicIds!!0),embed fmapTm)
                                         ] ++ init elems)) lbody)
          in  changed lb
    go _ _ ty = error $ $(curLoc) ++ "reduceTraverse: argument does not have a vector type: " ++ showDoc ty

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

-- | Replace an application of the @CLaSH.Sized.Vector.foldr@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @CLaSH.Sized.Vector.foldr@
reduceFoldr :: Integer  -- ^ Length of the vector
            -> Type -- ^ Element type of the argument vector
            -> Term -- ^ The function to fold with
            -> Term -- ^ The starting value
            -> Term -- ^ The argument vector
            -> NormalizeSession Term
reduceFoldr n aTy fun start arg = do
    tcm <- Lens.view tcCache
    ty  <- termType tcm arg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- HashMap.lookup vecTcNm tcm
      , [_,consCon] <- tyConDataCons vecTc
      = let (vars,elems)     = second concat . unzip
                             $ extractElems consCon aTy 'G' n arg
            lbody            = foldr (\l r -> mkApps fun [Left l,Left r]) start vars
            lb               = Letrec (bind (rec (init elems)) lbody)
        in  changed lb
    go _ ty = error $ $(curLoc) ++ "reduceFoldr: argument does not have a vector type: " ++ showDoc ty

-- | Replace an application of the @CLaSH.Sized.Vector.fold@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @CLaSH.Sized.Vector.fold@
reduceFold :: Integer  -- ^ Length of the vector
           -> Type -- ^ Element type of the argument vector
           -> Term -- ^ The function to fold with
           -> Term -- ^ The argument vector
           -> NormalizeSession Term
reduceFold n aTy fun arg = do
    tcm <- Lens.view tcCache
    ty  <- termType tcm arg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- HashMap.lookup vecTcNm tcm
      , [_,consCon]  <- tyConDataCons vecTc
      = let (vars,elems)     = second concat . unzip
                             $ extractElems consCon aTy 'F' n arg
            lbody            = foldV vars
            lb               = Letrec (bind (rec (init elems)) lbody)
        in  changed lb
    go _ ty = error $ $(curLoc) ++ "reduceFold: argument does not have a vector type: " ++ showDoc ty

    foldV [a] = a
    foldV as  = let (l,r) = splitAt (length as `div` 2) as
                    lF    = foldV l
                    rF    = foldV r
                in  mkApps fun [Left lF, Left rF]

-- | Replace an application of the @CLaSH.Sized.Vector.dfold@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @CLaSH.Sized.Vector.dfold@
reduceDFold :: Integer  -- ^ Length of the vector
            -> Type -- ^ Element type of the argument vector
            -> Term -- ^ Function to fold with
            -> Term -- ^ Starting value
            -> Term -- ^ The vector to fold
            -> NormalizeSession Term
reduceDFold n aTy fun start arg = do
    tcm <- Lens.view tcCache
    ty  <- termType tcm arg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- HashMap.lookup vecTcNm tcm
      , [_,consCon]  <- tyConDataCons vecTc
      = do
        let  (vars,elems)     = second concat . unzip
                         $ extractElems consCon aTy 'D' n arg
        (_ltv:Right snTy:_,_) <- splitFunForallTy <$> termType tcm fun
        let (TyConApp snatTcNm _) = tyView snTy
            (Just snatTc)         = HashMap.lookup snatTcNm tcm
            [snatDc]              = tyConDataCons snatTc
            lbody = doFold (buildSNat snatDc) (n-1) vars
            lb    = Letrec (bind (rec (init elems)) lbody)
        changed lb
    go _ ty = error $ $(curLoc) ++ "reduceDFold: argument does not have a vector type: " ++ showDoc ty

    doFold _    _ []     = start
    doFold snDc k (x:xs) = mkApps fun
                                 [Right (LitTy (NumTy k))
                                 ,Left (snDc k)
                                 ,Left x
                                 ,Left (doFold snDc (k-1) xs)
                                 ]

-- | Replace an application of the @CLaSH.Sized.Vector.head@ primitive on
-- vectors of a known length @n@, by a projection of the first element of a
-- vector.
reduceHead :: Integer  -- ^ Length of the vector
           -> Type -- ^ Element type of the vector
           -> Term -- ^ The argument vector
           -> NormalizeSession Term
reduceHead n aTy vArg = do
    tcm <- Lens.view tcCache
    ty  <- termType tcm vArg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- HashMap.lookup vecTcNm tcm
      , [_,consCon]  <- tyConDataCons vecTc
      = let (vars,elems)  = second concat . unzip
                          $ extractElems consCon aTy 'H' n vArg
            lb = Letrec (bind (rec [head elems]) (head vars))
        in  changed lb
    go _ ty = error $ $(curLoc) ++ "reduceHead: argument does not have a vector type: " ++ showDoc ty

-- | Replace an application of the @CLaSH.Sized.Vector.tail@ primitive on
-- vectors of a known length @n@, by a projection of the tail of a
-- vector.
reduceTail :: Integer  -- ^ Length of the vector
           -> Type -- ^ Element type of the vector
           -> Term -- ^ The argument vector
           -> NormalizeSession Term
reduceTail n aTy vArg = do
    tcm <- Lens.view tcCache
    ty  <- termType tcm vArg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- HashMap.lookup vecTcNm tcm
      , [_,consCon]  <- tyConDataCons vecTc
      = let (_,elems)    = second concat . unzip
                         $ extractElems consCon aTy 'L' n vArg
            b@(tB,_)     = elems !! 1
            lb           = Letrec (bind (rec [b]) (idToVar tB))
        in  changed lb
    go _ ty = error $ $(curLoc) ++ "reduceTail: argument does not have a vector type: " ++ showDoc ty

-- | Replace an application of the @CLaSH.Sized.Vector.last@ primitive on
-- vectors of a known length @n@, by a projection of the last element of a
-- vector.
reduceLast :: Integer  -- ^ Length of the vector
           -> Type -- ^ Element type of the vector
           -> Term -- ^ The argument vector
           -> NormalizeSession Term
reduceLast n aTy vArg = do
    tcm <- Lens.view tcCache
    ty  <- termType tcm vArg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- HashMap.lookup vecTcNm tcm
      , [_,consCon]  <- tyConDataCons vecTc
      = let (_,elems)    = unzip
                         $ extractElems consCon aTy 'L' n vArg
            (tB,_)       = head (last elems)
        in case n of
            0 -> changed (mkApps (Prim "CLaSH.Transformations.undefined" undefinedTy) [Right aTy])
            _ -> changed (Letrec (bind (rec (init (concat elems))) (idToVar tB)))
    go _ ty = error $ $(curLoc) ++ "reduceLast: argument does not have a vector type: " ++ showDoc ty

-- | Replace an application of the @CLaSH.Sized.Vector.init@ primitive on
-- vectors of a known length @n@, by a projection of the init of a
-- vector.
reduceInit :: Integer  -- ^ Length of the vector
           -> Type -- ^ Element type of the vector
           -> Term -- ^ The argument vector
           -> NormalizeSession Term
reduceInit n aTy vArg = do
    tcm <- Lens.view tcCache
    ty  <- termType tcm vArg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- HashMap.lookup vecTcNm tcm
      , [nilCon,consCon]  <- tyConDataCons vecTc
      = let (_,elems)    = unzip
                         $ extractElems consCon aTy 'L' n vArg
        in case n of
            0 -> changed (mkApps (Prim "CLaSH.Transformations.undefined" undefinedTy) [Right aTy])
            1 -> changed (mkVec nilCon consCon aTy 0 [])
            _ -> let el = init elems
                     iv = mkVec nilCon consCon aTy (n-1) (map (idToVar . fst . head) el)
                     lb = rec (init (concat el))
                 in  changed (Letrec (bind lb iv))

    go _ ty = error $ $(curLoc) ++ "reduceInit: argument does not have a vector type: " ++ showDoc ty

-- | Replace an application of the @CLaSH.Sized.Vector.(++)@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @CLaSH.Sized.Vector.(++)@
reduceAppend :: Integer  -- ^ Length of the LHS arg
             -> Integer  -- ^ Lenght of the RHS arg
             -> Type -- ^ Element type of the vectors
             -> Term -- ^ The LHS argument
             -> Term -- ^ The RHS argument
             -> NormalizeSession Term
reduceAppend n m aTy lArg rArg = do
    tcm <- Lens.view tcCache
    ty  <- termType tcm lArg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- HashMap.lookup vecTcNm tcm
      , [_,consCon]  <- tyConDataCons vecTc
      = let (vars,elems) = second concat . unzip
                         $ extractElems consCon aTy 'C' n lArg
            lbody        = appendToVec consCon aTy rArg (n+m) vars
            lb           = Letrec (bind (rec (init elems)) lbody)
        in  changed lb
    go _ ty = error $ $(curLoc) ++ "reduceAppend: argument does not have a vector type: " ++ showDoc ty

-- | Replace an application of the @CLaSH.Sized.Vector.unconcat@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @CLaSH.Sized.Vector.unconcat@
reduceUnconcat :: Integer  -- ^ Length of the result vector
               -> Integer  -- ^ Length of the elements of the result vector
               -> Type -- ^ Element type
               -> Term -- ^ Argument vector
               -> NormalizeSession Term
reduceUnconcat n 0 aTy arg = do
    tcm <- Lens.view tcCache
    ty  <- termType tcm arg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- HashMap.lookup vecTcNm tcm
      , [nilCon,consCon] <- tyConDataCons vecTc
      = let nilVec           = mkVec nilCon consCon aTy 0 []
            innerVecTy       = mkTyConApp vecTcNm [LitTy (NumTy 0), aTy]
            retVec           = mkVec nilCon consCon innerVecTy n (replicate (fromInteger n) nilVec)
        in  changed retVec
    go _ ty = error $ $(curLoc) ++ "reduceUnconcat: argument does not have a vector type: " ++ showDoc ty

reduceUnconcat _ _ _ _ = error $ $(curLoc) ++ "reduceUnconcat: unimplemented"

-- | Replace an application of the @CLaSH.Sized.Vector.transpose@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @CLaSH.Sized.Vector.transpose@
reduceTranspose :: Integer  -- ^ Length of the result vector
                -> Integer  -- ^ Length of the elements of the result vector
                -> Type -- ^ Element type
                -> Term -- ^ Argument vector
                -> NormalizeSession Term
reduceTranspose n 0 aTy arg = do
    tcm <- Lens.view tcCache
    ty  <- termType tcm arg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc)     <- HashMap.lookup vecTcNm tcm
      , [nilCon,consCon] <- tyConDataCons vecTc
      = let nilVec           = mkVec nilCon consCon aTy 0 []
            innerVecTy       = mkTyConApp vecTcNm [LitTy (NumTy 0), aTy]
            retVec           = mkVec nilCon consCon innerVecTy n (replicate (fromInteger n) nilVec)
        in  changed retVec
    go _ ty = error $ $(curLoc) ++ "reduceTranspose: argument does not have a vector type: " ++ showDoc ty

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
      | (Just vecTc)     <- HashMap.lookup vecTcNm tcm
      , [nilCon,consCon] <- tyConDataCons vecTc
      = let retVec = mkVec nilCon consCon aTy n (replicate (fromInteger n) arg)
        in  changed retVec
    go _ ty = error $ $(curLoc) ++ "reduceReplicate: argument does not have a vector type: " ++ showDoc ty

-- | Replace an application of the @CLaSH.Sized.Vector.dtfold@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @CLaSH.Sized.Vector.dtfold@
reduceDTFold :: Integer  -- ^ Length of the vector
             -> Type     -- ^ Element type of the argument vector
             -> Term     -- ^ Function to convert elements with
             -> Term     -- ^ Function to combine branches with
             -> Term     -- ^ The vector to fold
             -> NormalizeSession Term
reduceDTFold n aTy lrFun brFun arg = do
    tcm <- Lens.view tcCache
    ty  <- termType tcm arg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp vecTcNm _)
      | (Just vecTc) <- HashMap.lookup vecTcNm tcm
      , [_,consCon]  <- tyConDataCons vecTc
      = do let (vars,elems) = second concat . unzip
                            $ extractElems consCon aTy 'T' (2^n) arg
           (_ltv:Right snTy:_,_) <- splitFunForallTy <$> termType tcm brFun
           let (TyConApp snatTcNm _) = tyView snTy
               (Just snatTc)         = HashMap.lookup snatTcNm tcm
               [snatDc]              = tyConDataCons snatTc
               lbody = doFold (buildSNat snatDc) (n-1) vars
               lb    = Letrec (bind (rec (init elems)) lbody)
           changed lb
    go _ ty = error $ $(curLoc) ++ "reduceDTFold: argument does not have a vector type: " ++ showDoc ty

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

-- | Replace an application of the @CLaSH.Sized.RTree.tdfold@ primitive on
-- trees of a known depth @n@, by the fully unrolled recursive "definition"
-- of @CLaSH.Sized.RTree.tdfold@
reduceTFold :: Integer -- ^ Depth of the tree
            -> Type    -- ^ Element type of the argument tree
            -> Term    -- ^ Function to convert elements with
            -> Term    -- ^ Function to combine branches with
            -> Term    -- ^ The tree to fold
            -> NormalizeSession Term
reduceTFold n aTy lrFun brFun arg = do
    tcm <- Lens.view tcCache
    ty  <- termType tcm arg
    go tcm ty
  where
    go tcm (coreView tcm -> Just ty') = go tcm ty'
    go tcm (tyView -> TyConApp treeTcNm _)
      | (Just treeTc) <- HashMap.lookup treeTcNm tcm
      , [lrCon,brCon] <- tyConDataCons treeTc
      = do let (vars,elems)     = extractTElems lrCon brCon aTy 'T' n arg
           (_ltv:Right snTy:_,_) <- splitFunForallTy <$> termType tcm brFun
           let (TyConApp snatTcNm _) = tyView snTy
               (Just snatTc)         = HashMap.lookup snatTcNm tcm
               [snatDc]              = tyConDataCons snatTc
               lbody = doFold (buildSNat snatDc) (n-1) vars
               lb    = Letrec (bind (rec elems) lbody)
           changed lb
    go _ ty = error $ $(curLoc) ++ "reduceTFold: argument does not have a tree type: " ++ showDoc ty

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
      | (Just treeTc) <- HashMap.lookup treeTcNm tcm
      , [lrCon,brCon] <- tyConDataCons treeTc
      = let retVec = mkRTree lrCon brCon aTy n (replicate (2^n) arg)
        in  changed retVec
    go _ ty = error $ $(curLoc) ++ "reduceTReplicate: argument does not have a vector type: " ++ showDoc ty

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
