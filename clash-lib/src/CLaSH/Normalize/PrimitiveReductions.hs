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

  Partially handles:

    * CLaSH.Sized.Vector.unconcat
    * CLaSH.Sized.Vector.transpose
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module CLaSH.Normalize.PrimitiveReductions where

import qualified Control.Lens                     as Lens
import qualified Data.HashMap.Lazy                as HashMap
import qualified Data.Maybe                       as Maybe
import           Data.Text                        (pack)
import           Unbound.Generics.LocallyNameless (bind, embed, rec, rebind,
                                                   string2Name, name2String)

import           CLaSH.Core.DataCon               (DataCon, dataConInstArgTys,
                                                   dcName, dcType)
import           CLaSH.Core.Literal               (Literal (..))
import           CLaSH.Core.Term                  (Term (..), Pat (..))
import           CLaSH.Core.Type                  (LitTy (..), Type (..),
                                                   TypeView (..), coreView,
                                                   mkFunTy, mkTyConApp,
                                                   splitFunForallTy, tyView)
import           CLaSH.Core.TyCon                 (TyConName, tyConDataCons)
import           CLaSH.Core.TysPrim               (integerPrimTy, typeNatKind)
import           CLaSH.Core.Util                  (appendToVec, extractElems,
                                                   idToVar, mkApps, mkVec,
                                                   termType)
import           CLaSH.Core.Var                   (Var (..))

import           CLaSH.Normalize.Types
import           CLaSH.Rewrite.Types
import           CLaSH.Rewrite.Util
import           CLaSH.Util

-- | Replace an application of the @CLaSH.Sized.Vector.zipWith@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @CLaSH.Sized.Vector.zipWith@
reduceZipWith :: Int  -- ^ Length of the vector(s)
              -> Type -- ^ Type of the lhs of the function
              -> Type -- ^ Type of the rhs of the function
              -> Type -- ^ Type of the result of the function
              -> Term -- ^ The zipWith'd functions
              -> Term -- ^ The 1st vector argument
              -> Term -- ^ The 2nd vector argument
              -> NormalizeSession Term
reduceZipWith n lhsElTy rhsElTy resElTy fun lhsArg rhsArg = do
  tcm <- Lens.view tcCache
  (TyConApp vecTcNm _) <- coreView tcm <$> termType tcm lhsArg
  let (Just vecTc)     = HashMap.lookup vecTcNm tcm
      [nilCon,consCon] = tyConDataCons vecTc
      (varsL,elemsL)   = second concat . unzip
                       $ extractElems consCon lhsElTy 'L' n lhsArg
      (varsR,elemsR)   = second concat . unzip
                       $ extractElems consCon rhsElTy 'R' n rhsArg
      funApps          = zipWith (\l r -> mkApps fun [Left l,Left r]) varsL varsR
      lbody            = mkVec nilCon consCon resElTy n funApps
      lb               = Letrec (bind (rec (init elemsL ++ init elemsR)) lbody)
  changed lb

-- | Replace an application of the @CLaSH.Sized.Vector.map@ primitive on vectors
-- of a known length @n@, by the fully unrolled recursive "definition" of
-- @CLaSH.Sized.Vector.map@
reduceMap :: Int  -- ^ Length of the vector
          -> Type -- ^ Argument type of the function
          -> Type -- ^ Result type of the function
          -> Term -- ^ The map'd function
          -> Term -- ^ The map'd over vector
          -> NormalizeSession Term
reduceMap n argElTy resElTy fun arg = do
  tcm <- Lens.view tcCache
  (TyConApp vecTcNm _) <- coreView tcm <$> termType tcm arg
  let (Just vecTc)     = HashMap.lookup vecTcNm tcm
      [nilCon,consCon] = tyConDataCons vecTc
      (vars,elems)     = second concat . unzip
                       $ extractElems consCon argElTy 'A' n arg
      funApps          = map (fun `App`) vars
      lbody            = mkVec nilCon consCon resElTy n funApps
      lb               = Letrec (bind (rec (init elems)) lbody)
  changed lb

-- | Replace an application of the @CLaSH.Sized.Vector.imap@ primitive on vectors
-- of a known length @n@, by the fully unrolled recursive "definition" of
-- @CLaSH.Sized.Vector.map@
reduceImap :: Int  -- ^ Length of the vector
           -> Type -- ^ Argument type of the function
           -> Type -- ^ Result type of the function
           -> Term -- ^ The imap'd function
           -> Term -- ^ The imap'd over vector
           -> NormalizeSession Term
reduceImap n argElTy resElTy fun arg = do
  tcm <- Lens.view tcCache
  (TyConApp vecTcNm _) <- coreView tcm <$> termType tcm arg
  let (Just vecTc)     = HashMap.lookup vecTcNm tcm
      [nilCon,consCon] = tyConDataCons vecTc
      (vars,elems)     = second concat . unzip
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

-- | Replace an application of the @CLaSH.Sized.Vector.traverse#@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @CLaSH.Sized.Vector.traverse#@
reduceTraverse :: Int  -- ^ Length of the vector
               -> Type -- ^ Element type of the argument vector
               -> Type -- ^ The type of the applicative
               -> Type -- ^ Element type of the result vector
               -> Term -- ^ The @Applicative@ dictionary
               -> Term -- ^ The function to traverse with
               -> Term -- ^ The argument vector
               -> NormalizeSession Term
reduceTraverse n aTy fTy bTy dict fun arg = do
  tcm <- Lens.view tcCache
  (TyConApp vecTcNm    _) <- coreView tcm <$> termType tcm arg
  (TyConApp apDictTcNm _) <- coreView tcm <$> termType tcm dict
  let (Just apDictTc)    = HashMap.lookup apDictTcNm tcm
      [apDictCon]        = tyConDataCons apDictTc
      (Just apDictIdTys) = dataConInstArgTys apDictCon [fTy]
      apDictIds          = zipWith Id (map string2Name ["functorDict"
                                                       ,"pure"
                                                       ,"ap"
                                                       ,"apConstL"
                                                       ,"apConstR"])
                                      (map embed apDictIdTys)

      (TyConApp funcDictTcNm _) = coreView tcm (head apDictIdTys)
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

      (Just vecTc)     = HashMap.lookup vecTcNm tcm
      [nilCon,consCon] = tyConDataCons vecTc
      (vars,elems)     = second concat . unzip
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
  changed lb

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
          -> Int       -- ^ Length of the vector
          -> [Term]    -- ^ Elements of the vector
          -> Term
mkTravVec vecTc nilCon consCon pureTm apTm fmapTm bTy = go
  where
    go :: Int -> [Term] -> Term
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
reduceFoldr :: Int  -- ^ Length of the vector
            -> Type -- ^ Element type of the argument vector
            -> Type -- ^ Type of the starting element
            -> Term -- ^ The function to fold with
            -> Term -- ^ The starting value
            -> Term -- ^ The argument vector
            -> NormalizeSession Term
reduceFoldr n aTy _bTy fun start arg = do
  tcm <- Lens.view tcCache
  (TyConApp vecTcNm _) <- coreView tcm <$> termType tcm arg
  let (Just vecTc)     = HashMap.lookup vecTcNm tcm
      [_,consCon]      = tyConDataCons vecTc
      (vars,elems)     = second concat . unzip
                       $ extractElems consCon aTy 'G' n arg
      lbody            = foldr (\l r -> mkApps fun [Left l,Left r]) start vars
      lb               = Letrec (bind (rec (init elems)) lbody)
  changed lb

-- | Replace an application of the @CLaSH.Sized.Vector.fold@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @CLaSH.Sized.Vector.fold@
reduceFold :: Int  -- ^ Length of the vector
           -> Type -- ^ Element type of the argument vector
           -> Term -- ^ The function to fold with
           -> Term -- ^ The argument vector
           -> NormalizeSession Term
reduceFold n aTy fun arg = do
    tcm <- Lens.view tcCache
    (TyConApp vecTcNm _) <- coreView tcm <$> termType tcm arg
    let (Just vecTc)     = HashMap.lookup vecTcNm tcm
        [_,consCon]      = tyConDataCons vecTc
        (vars,elems)     = second concat . unzip
                         $ extractElems consCon aTy 'F' n arg
        lbody            = foldV vars
        lb               = Letrec (bind (rec (init elems)) lbody)
    changed lb
  where
    foldV [a] = a
    foldV as  = let (l,r) = splitAt (length as `div` 2) as
                    lF    = foldV l
                    rF    = foldV r
                in  mkApps fun [Left lF, Left rF]

-- | Replace an application of the @CLaSH.Sized.Vector.dfold@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @CLaSH.Sized.Vector.dfold@
reduceDFold :: Int  -- ^ Length of the vector
            -> Type -- ^ Element type of the argument vector
            -> Term -- ^ Function to fold with
            -> Term -- ^ Starting value
            -> Term -- ^ The vector to fold
            -> NormalizeSession Term
reduceDFold n aTy fun start arg = do
    tcm <- Lens.view tcCache
    (TyConApp vecTcNm _) <- coreView tcm <$> termType tcm arg
    let (Just vecTc)     = HashMap.lookup vecTcNm tcm
        [_,consCon]      = tyConDataCons vecTc
        (vars,elems)     = second concat . unzip
                         $ extractElems consCon aTy 'D' n arg
    (_ltv:Right snTy:_,_) <- splitFunForallTy <$> termType tcm fun
    let (TyConApp snatTcNm _) = coreView tcm snTy
        (Just snatTc)         = HashMap.lookup snatTcNm tcm
        [snatDc]              = tyConDataCons snatTc

        ([_nTv,_kn,Right pTy],_) = splitFunForallTy (dcType snatDc)
        (TyConApp proxyTcNm _)   = coreView tcm pTy
        (Just proxyTc)           = HashMap.lookup proxyTcNm tcm
        [proxyDc]                = tyConDataCons proxyTc

        buildSNat i = mkApps (Prim (pack (name2String (dcName snatDc)))
                                   (dcType snatDc))
                             [Right (LitTy (NumTy i))
                             ,Left (Literal (IntegerLiteral (toInteger i)))
                             ,Left (mkApps (Data proxyDc)
                                           [Right typeNatKind
                                           ,Right (LitTy (NumTy i))])
                             ]
        lbody = doFold buildSNat (n-1) vars
        lb    = Letrec (bind (rec (init elems)) lbody)
    changed lb
  where
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
reduceHead :: Int  -- ^ Length of the vector
           -> Type -- ^ Element type of the vector
           -> Term -- ^ The argument vector
           -> NormalizeSession Term
reduceHead n aTy vArg = do
  tcm <- Lens.view tcCache
  (TyConApp vecTcNm _) <- coreView tcm <$> termType tcm vArg
  let (Just vecTc)  = HashMap.lookup vecTcNm tcm
      [_,consCon]   = tyConDataCons vecTc
      (vars,elems)  = second concat . unzip
                    $ extractElems consCon aTy 'H' n vArg
      lb = Letrec (bind (rec [head elems]) (head vars))
  changed lb

-- | Replace an application of the @CLaSH.Sized.Vector.tail@ primitive on
-- vectors of a known length @n@, by a projection of the tail of a
-- vector.
reduceTail :: Int  -- ^ Length of the vector
           -> Type -- ^ Element type of the vector
           -> Term -- ^ The argument vector
           -> NormalizeSession Term
reduceTail n aTy vArg = do
  tcm <- Lens.view tcCache
  (TyConApp vecTcNm _) <- coreView tcm <$> termType tcm vArg
  let (Just vecTc) = HashMap.lookup vecTcNm tcm
      [_,consCon]  = tyConDataCons vecTc
      (_,elems)    = second concat . unzip
                   $ extractElems consCon aTy 'L' n vArg
      b@(tB,_)     = elems !! 1
      lb           = Letrec (bind (rec [b]) (idToVar tB))
  changed lb

-- | Replace an application of the @CLaSH.Sized.Vector.(++)@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @CLaSH.Sized.Vector.(++)@
reduceAppend :: Int  -- ^ Length of the LHS arg
             -> Int  -- ^ Lenght of the RHS arg
             -> Type -- ^ Element type of the vectors
             -> Term -- ^ The LHS argument
             -> Term -- ^ The RHS argument
             -> NormalizeSession Term
reduceAppend n m aTy lArg rArg = do
  tcm <- Lens.view tcCache
  (TyConApp vecTcNm _) <- coreView tcm <$> termType tcm lArg
  let (Just vecTc) = HashMap.lookup vecTcNm tcm
      [_,consCon]  = tyConDataCons vecTc
      (vars,elems) = second concat . unzip
                   $ extractElems consCon aTy 'C' n lArg
      lbody        = appendToVec consCon aTy rArg (n+m) vars
      lb           = Letrec (bind (rec (init elems)) lbody)
  changed lb

-- | Replace an application of the @CLaSH.Sized.Vector.unconcat@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @CLaSH.Sized.Vector.unconcat@
reduceUnconcat :: Int  -- ^ Length of the result vector
               -> Int  -- ^ Length of the elements of the result vector
               -> Type -- ^ Element type
               -> Term -- ^ Argument vector
               -> NormalizeSession Term
reduceUnconcat n 0 aTy arg = do
  tcm <- Lens.view tcCache
  (TyConApp vecTcNm _) <- coreView tcm <$> termType tcm arg
  let (Just vecTc)     = HashMap.lookup vecTcNm tcm
      [nilCon,consCon] = tyConDataCons vecTc
      nilVec           = mkVec nilCon consCon aTy 0 []
      innerVecTy       = mkTyConApp vecTcNm [LitTy (NumTy 0), aTy]
      retVec           = mkVec nilCon consCon innerVecTy n (replicate n nilVec)
  changed retVec

reduceUnconcat _ _ _ _ = error $ $(curLoc) ++ "reduceUnconcat: unimplemented"

-- | Replace an application of the @CLaSH.Sized.Vector.transpose@ primitive on
-- vectors of a known length @n@, by the fully unrolled recursive "definition"
-- of @CLaSH.Sized.Vector.transpose@
reduceTranspose :: Int  -- ^ Length of the result vector
                -> Int  -- ^ Length of the elements of the result vector
                -> Type -- ^ Element type
                -> Term -- ^ Argument vector
                -> NormalizeSession Term
reduceTranspose n 0 aTy arg = do
  tcm <- Lens.view tcCache
  (TyConApp vecTcNm _) <- coreView tcm <$> termType tcm arg
  let (Just vecTc)     = HashMap.lookup vecTcNm tcm
      [nilCon,consCon] = tyConDataCons vecTc
      nilVec           = mkVec nilCon consCon aTy 0 []
      innerVecTy       = mkTyConApp vecTcNm [LitTy (NumTy 0), aTy]
      retVec           = mkVec nilCon consCon innerVecTy n (replicate n nilVec)
  changed retVec

reduceTranspose _ _ _ _ = error $ $(curLoc) ++ "reduceTranspose: unimplemented"

reduceReplicate :: Int
                -> Type
                -> Type
                -> Term
                -> NormalizeSession Term
reduceReplicate n aTy eTy arg = do
  tcm <- Lens.view tcCache
  let (TyConApp vecTcNm _) = coreView tcm eTy
      (Just vecTc) = HashMap.lookup vecTcNm tcm
      [nilCon,consCon] = tyConDataCons vecTc
      retVec = mkVec nilCon consCon aTy n (replicate n arg)
  changed retVec
