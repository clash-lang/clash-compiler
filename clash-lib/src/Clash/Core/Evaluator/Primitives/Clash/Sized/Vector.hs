{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "Clash.Sized.Vector" in the
  evaluator. See "Clash.Core.Evaluator.Primitives" for how to write
  them.

  Most reductions only need the names of the type constructors in the type of
  their primitive (e.g. that of @Vec@). They take them from its declared type
  ('primArgResTys'), rather than from its instantiated type 'ty'.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Evaluator.Primitives.Clash.Sized.Vector
  ( primitives
  ) where

import Control.Monad.Trans.Except (runExcept)
import Data.Bits (bit, shiftR, (.&.))
import qualified Data.Either as Either
import Data.Text (Text)
import Data.Text.Extra (showt)

import Clash.Core.Basic (LitTy (..), WorkInfo (..))
import Clash.Core.DataCon (DataCon)
import Clash.Core.Literal (Literal (..))
import qualified Clash.Core.Name as C
import Clash.Core.TyCon (TyConName, tyConDataCons)
import Clash.Core.TysPrim (typeNatMul, typeNatSub)
import qualified Clash.Normalize.Primitives as NP
import Clash.Util (curLoc, textNameLit)

import qualified Clash.Sized.Internal.Index
import qualified Clash.Sized.Vector

import Clash.Core.DataCon (dataConInstArgTys)
import Clash.Core.Evaluator (newLetBinding, whnf)
import Clash.Core.Evaluator.Primitives.Util
import Clash.Core.Evaluator.Types
import Clash.Core.HasType (piResultTy, piResultTys)
import Clash.Core.Subst (withFreshIdBinder, withFreshIdBinders)
import Clash.Core.Syntax
import Clash.Core.Term (mkApps)
import Clash.Core.Type
  ( PiTele (..), SplitFunForall (..), TypeView (..), mkFunTy, mkTyConApp, normalizeType
  , splitFunForallTy, tyView )
import Clash.Core.TypeEnv (TypeEnv, lookupTyCon)
import Clash.Core.Util (mkSelectorCase, primCo)

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry $(textNameLit 'Clash.Sized.Vector.length) $ \case
      PrimStepContext{..} -- :: KnownNat n => Vec n a -> Int
        | isSubj
        , [nTy, _] <- tys
        , Right n <- runExcept (tyNatSize te sc nTy)
        -> reduce (mkIntCLit te IntLiteral n ty)
      _ -> Nothing

  -- XXX: Not a thing anymore?
  , primStepEntry "Clash.Sized.Vector.maxIndex" $ \case
      PrimStepContext{..}
        | isSubj
        , [nTy, _] <- tys
        , Right n <- runExcept (tyNatSize te sc nTy)
        -> reduce (mkIntCLit te IntLiteral (n - 1) ty)
      _ -> Nothing

-- Indexing
  -- XXX: Not exported
  , primStepEntry "Clash.Sized.Vector.index_int" $ \case
      PrimStepContext{..} -- :: KnownNat n => Vec n a -> Int -> a
        | nTy : aTy : _ <- tys
        , _ : xs : i : _ <- args
        , DC intDc [Left (Literal (IntLiteral i'))] <- i
        , i' >= 0
        , DC _ vArgs <- xs
        , Right n' <- runExcept (tyNatSize te sc nTy)
        , n' /= 0
        -> if i' == 0
             then reduceWHNF (Either.lefts vArgs !! 1)
             else reduceWHNF $
                  mkApps (Prim pInfo)
                    [ Right (numTy (n' - 1))
                    , Right aTy
                    , Left (natLit (n' - 1))
                    , Left (Either.lefts vArgs !! 2)
                    , Left (mkApps (Data intDc) [Left (Literal (IntLiteral (i' - 1)))]) ]
      _ -> Nothing

  -- A reduction that continues with the WHNF of a field
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.head) $ \case
      PrimStepContext{..} -- :: Vec (n+1) a -> a
        | isSubj
        , [DC _ vArgs] <- args
        -> reduceWHNF (Either.lefts vArgs !! 1)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.last) $ \case
      PrimStepContext{..} -- :: Vec (n+1) a -> a
        | isSubj
        , [DC _ vArgs] <- args
        , Right _ : Right aTy : Right nTy : _ <- vArgs
        , Right n <- runExcept (tyNatSize te sc nTy)
        -> if n == 0
             then reduceWHNF (Either.lefts vArgs !! 1)
             else reduceWHNF $
                  mkApps (Prim pInfo)
                    [Right (numTy (n - 1)), Right aTy, Left (Either.lefts vArgs !! 2)]
      _ -> Nothing

-- - Sub-vectors
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.tail) $ \case
      PrimStepContext{..} -- :: Vec (n+1) a -> Vec n a
        | isSubj
        , [DC _ vArgs] <- args
        -> reduceWHNF (Either.lefts vArgs !! 2)
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.init) $ \case
      PrimStepContext{..} -- :: Vec (n+1) a -> Vec n a
        | isSubj
        , [DC consCon vArgs] <- args
        , Right _ : Right aTy : Right nTy : _ <- vArgs
        , Right n <- runExcept (tyNatSize te sc nTy)
        -> if n == 0
             then reduceWHNF (Either.lefts vArgs !! 2)
             else reduce $
                  mkVecCons te sc consCon aTy n
                    (Either.lefts vArgs !! 1)
                    (mkApps (Prim pInfo)
                       [Right (numTy (n - 1)), Right aTy, Left (Either.lefts vArgs !! 2)])
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.select) $ \case
      PrimStepContext{..}
        -- :: (CmpNat (i+s) (s*n) ~ GT) => SNat f -> SNat s -> SNat n -> Vec (f + i) a -> Vec n a
        | isSubj
        , iTy : sTy : nTy : fTy : aTy : _ <- tys
        , eq : f : s : n : xs : _ <- args
        , Right n' <- runExcept (tyNatSize te sc nTy)
        , Right f' <- runExcept (tyNatSize te sc fTy)
        , Right i' <- runExcept (tyNatSize te sc iTy)
        , Right s' <- runExcept (tyNatSize te sc sTy)
        , DC _ vArgs <- xs
        , (_ : snatTy : _, resTy) <- primArgResTys pInfo
        , Just vecTcNm <- tyConNameOf resTy
        , Just snatTcNm <- tyConNameOf snatTy
        , Just (nilCon, consCon) <- vecDataCons te vecTcNm
        , Just tupDc <- pairDataCon te
        -> let vecTy len = mkTyConApp vecTcNm [len, aTy]
               -- The second part of @splitAt@ at the given lengths
               dropCall fstTy sndTy snat =
                 selectPair sc tupDc ("fxs", vecTy fstTy) ("ixs", vecTy sndTy) False $
                   mkApps (splitAtPrim snatTcNm vecTcNm)
                     [ Right fstTy, Right sndTy, Right aTy
                     , Left (valToTerm snat), Left (valToTerm xs) ]
           in case n' of
                0 -> reduce (mkVecNil te sc nilCon aTy)
                _ -> case f' of
                  0 -> let isTy = numTy (i' - s')
                       in reduce $
                          mkVecCons te sc consCon aTy n' (Either.lefts vArgs !! 1) $
                            mkApps (Prim pInfo)
                              [ Right isTy
                              , Right sTy
                              , Right (numTy (n' - 1))
                              , Right (numTy 0)
                              , Right aTy
                              , Left (valToTerm eq)
                              , Left (natLit 0)
                              , Left (valToTerm s)
                              , Left (natLit (n' - 1))
                              , Left (dropCall sTy isTy s) ]
                  _ -> reduceWHNF $
                       mkApps (Prim pInfo)
                         [ Right iTy
                         , Right sTy
                         , Right nTy
                         , Right (numTy 0)
                         , Right aTy
                         , Left (valToTerm eq)
                         , Left (natLit 0)
                         , Left (valToTerm s)
                         , Left (valToTerm n)
                         , Left (dropCall fTy iTy f) ]
      _ -> Nothing

-- - Splitting
  -- A reduction that let-binds a recursive call, and selects its fields with
  -- case-expressions whose patterns bind fresh variables
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.splitAt) $ \case
      PrimStepContext{..} -- :: SNat m -> Vec (m + n) a -> (Vec m a, Vec n a)
        | isSubj
        , DC snatDc (Right mTy : _) : _ <- args
        , Right mN <- runExcept (tyNatSize te sc mTy)
        , _ : nTy : aTy : _ <- tys
        , TyConApp tupTcNm tyArgs@(tyArg : _) <- tyView (getResultTy te sc ty tys)
        , Just tupTc <- lookupTyCon te tupTcNm
        , [tupDc] <- tyConDataCons tupTc
        , TyConApp vecTcNm _ <- tyView tyArg
        , Just vecTc <- lookupTyCon te vecTcNm
        , [nilCon, consCon] <- tyConDataCons vecTc
        -> let vecTy :: Type x -> Type x -> Type x
               vecTy len el = mkTyConApp vecTcNm [len, el]
               m1Lit :: Type x
               m1Lit = numTy (mN - 1)
               splitAtRec v = mkApps (Prim pInfo)
                 [ Right m1Lit, Right nTy, Right aTy
                 , Left (mkApps (Data snatDc)
                           [Right m1Lit, Left (Literal (NaturalLiteral (mN - 1)))])
                 , Left v ]
           in case mN of
                -- (Nil, v)
                0 -> reduce $ mkApps (Data tupDc) $ map Right tyArgs ++
                       [ Left (mkVecNil te sc nilCon aTy)
                       , Left (valToTerm (last args)) ]
                -- (x : fst (splitAt (m-1) xs), snd (splitAt (m-1) xs))
                _ | DC _ vArgs <- last args -> case Either.lefts vArgs of
                      _ : x : xs : _ ->
                        newLetBinding evalEnv mach (splitAtRec xs) $ \m1 recV ->
                          let sc1 = mScope m1
                              aTy1 = sink aTy
                              nTy1 = sink nTy
                              -- A field of the recursive call
                              sel first =
                                withFreshIdBinder sc1 (C.mkUnsafeSystemName "l" 0) (vecTy m1Lit aTy1) $ \sc2 lB ->
                                  withFreshIdBinder sc2 (C.mkUnsafeSystemName "r" 1) (vecTy (sink nTy1) (sink aTy1)) $ \_ rB ->
                                    Case (heapVarTerm recV)
                                      (if first then vecTy m1Lit aTy1 else vecTy nTy1 aTy1)
                                      [Alt (DataPat tupDc TNil (TCons lB (TCons rB TNil)))
                                           (if first then sink (idBinderVar lB) else idBinderVar rB)]
                          in reduceWith m1 $ mkApps (Data tupDc) $ map (Right . sink) tyArgs ++
                               [ Left (mkVecCons te sc1 consCon aTy1 mN (sink x) (sel True))
                               , Left (sel False) ]
                      _ ->
                        -- The vector reduces to Nil and not Cons, which only
                        -- happens when 'n' would be negative. See
                        -- https://github.com/clash-lang/clash-compiler/issues/2831
                        reduce (TyApp (Prim NP.undefined) (getResultTy te sc ty tys))
                  -- The vector doesn't reduce to a data constructor
                  | otherwise -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.unconcat) $ \case
      PrimStepContext{..} -- :: KnownNat n => SNat m -> Vec (n * m) a -> Vec n (Vec m a)
        | isSubj
        , kn : snat : v : _ <- args
        , nTy : mTy : aTy : _ <- tys
        , Lit (NaturalLiteral n) <- kn
        , (_ : snatTy : _, resTy) <- primArgResTys pInfo
        , Just vecTcNm <- tyConNameOf resTy
        , Just snatTcNm <- tyConNameOf snatTy
        , Just (nilCon, consCon) <- vecDataCons te vecTcNm
        , Just tupDc <- pairDataCon te
        -> let n1mTy = mkTyConApp typeNatMul [mkTyConApp typeNatSub [nTy, numTy 1], mTy]
               mVecTy = mkTyConApp vecTcNm [mTy, aTy]
               n1mVecTy = mkTyConApp vecTcNm [n1mTy, aTy]
               splitAtCall =
                 mkApps (splitAtPrim snatTcNm vecTcNm)
                   [ Right mTy, Right n1mTy, Right aTy
                   , Left (valToTerm snat), Left (valToTerm v) ]
               sel first = selectPair sc tupDc ("as", mVecTy) ("bs", n1mVecTy) first splitAtCall
           in case n of
                0 -> reduce (mkVecNil te sc nilCon mVecTy)
                _ -> reduce $
                     mkVecCons te sc consCon mVecTy n (sel True)
                       (mkApps (Prim pInfo)
                          [ Right (numTy (n - 1))
                          , Right mTy
                          , Right aTy
                          , Left (natLit (n - 1))
                          , Left (valToTerm snat)
                          , Left (sel False) ])
      _ -> Nothing

-- Construction
-- - initialisation
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.replicate) $ \case
      PrimStepContext{..} -- :: SNat n -> a -> Vec n a
        | isSubj
        , TyConApp vecTcNm [lenTy,argTy] <- tyView (getResultTy te sc ty tys)
        , Right len <- runExcept (tyNatSize te sc lenTy)
        , Just vecTc <- lookupTyCon te vecTcNm
        , [nilCon,consCon] <- tyConDataCons vecTc
        -> reduce $
           mkVec te sc nilCon consCon argTy len
                 (replicate (fromInteger len) (valToTerm (last args)))
      _ -> Nothing

-- - Concatenation
  , primStepEntry $(textNameLit '(Clash.Sized.Vector.++)) $ \case
      PrimStepContext{..} -- :: Vec n a -> Vec m a -> Vec (n + m) a
        | isSubj
        , DC dc vArgs : _ <- args
        , Right nTy : Right aTy : _ <- vArgs
        , Right n <- runExcept (tyNatSize te sc nTy)
        -> case n of
             0 -> reduce (valToTerm (last args))
             n' | _ : _ : mTy : _ <- tys
                , Right m <- runExcept (tyNatSize te sc mTy)
                -> -- x : (xs ++ ys)
                   reduce $
                   mkVecCons te sc dc aTy (n' + m) (Either.lefts vArgs !! 1)
                     (mkApps (Prim pInfo)
                        [ Right (numTy (n' - 1))
                        , Right aTy
                        , Right mTy
                        , Left (Either.lefts vArgs !! 2)
                        , Left (valToTerm (last args)) ])
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.concat) $ \case
      PrimStepContext{..} -- :: Vec n (Vec m a) -> Vec (n * m) a
        | isSubj
        , nTy : mTy : aTy : _ <- tys
        , xs : _ <- args
        , DC dc vArgs <- xs
        , Right n <- runExcept (tyNatSize te sc nTy)
        -> case n of
             0 -> reduce (mkVecNil te sc dc aTy)
             _ | _ : h' : t : _ <- Either.lefts vArgs
               , Just vecTcNm <- tyConNameOf (snd (primArgResTys pInfo))
               -> reduceWHNF $
                  mkApps (vecAppendPrim vecTcNm)
                    [ Right mTy
                    , Right aTy
                    , Right (mkTyConApp typeNatMul [mkTyConApp typeNatSub [nTy, numTy 1], mTy])
                    , Left h'
                    , Left (mkApps (Prim pInfo)
                              [Right (numTy (n - 1)), Right mTy, Right aTy, Left t]) ]
             _ -> Nothing
      _ -> Nothing

-- Modifying vectors
  , primStepEntry "Clash.Sized.Vector.replace_int" $ \case
      PrimStepContext{..} -- :: KnownNat n => Vec n a -> Int -> a -> Vec n a
        | nTy : aTy : _ <- tys
        , _ : xs : i : a : _ <- args
        , DC intDc [Left (Literal (IntLiteral i'))] <- i
        , i' >= 0
        , DC consCon vArgs <- xs
        , Right n' <- runExcept (tyNatSize te sc nTy)
        , n' /= 0
        -> if i' == 0
             then reduce (mkVecCons te sc consCon aTy n' (valToTerm a) (Either.lefts vArgs !! 2))
             else reduce $
                  mkVecCons te sc consCon aTy n' (Either.lefts vArgs !! 1)
                    (mkApps (Prim pInfo)
                       [ Right (numTy (n' - 1))
                       , Right aTy
                       , Left (natLit (n' - 1))
                       , Left (Either.lefts vArgs !! 2)
                       , Left (mkApps (Data intDc) [Left (Literal (IntLiteral (i' - 1)))])
                       , Left (valToTerm a) ])
      _ -> Nothing

-- - specialized permutations
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.reverse) $ \case
      PrimStepContext{..} -- :: Vec n a -> Vec n a
        | isSubj
        , nTy : aTy : _ <- tys
        , [DC vecDc vArgs] <- args
        -> case runExcept (tyNatSize te sc nTy) of
             Right 0 -> reduce (mkVecNil te sc vecDc aTy)
             Right n
               | Just vecTcNm <- tyConNameOf (snd (primArgResTys pInfo))
               , Just (nilCon, consCon) <- vecDataCons te vecTcNm
               -> reduceWHNF $
                  mkApps (vecAppendPrim vecTcNm)
                    [ Right (numTy (n - 1))
                    , Right aTy
                    , Right (numTy 1)
                    , Left (mkApps (Prim pInfo)
                              [Right (numTy (n - 1)), Right aTy, Left (Either.lefts vArgs !! 2)])
                    , Left (mkVec te sc nilCon consCon aTy 1 [Either.lefts vArgs !! 1]) ]
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.transpose) $ \case
      PrimStepContext{..} -- :: KnownNat n => Vec m (Vec n a) -> Vec n (Vec m a)
        | isSubj
        , nTy : mTy : aTy : _ <- tys
        , kn : xss : _ <- args
        , Just vecTcNm <- tyConNameOf (snd (primArgResTys pInfo))
        , DC _ vArgs <- xss
        , Right n <- runExcept (tyNatSize te sc nTy)
        , Right m <- runExcept (tyNatSize te sc mTy)
        , Just (nilCon, consCon) <- vecDataCons te vecTcNm
        -> case m of
             0 -> reduce $
                  mkVec te sc nilCon consCon (mkTyConApp vecTcNm [mTy, aTy]) n
                    (replicate (fromInteger n) (mkVec te sc nilCon consCon aTy 0 []))
             m' | Just (consCoTy : _) <- dataConInstArgTys te sc consCon [mTy, aTy, numTy (m' - 1)]
                -> reduceWHNF $
                   mkApps (vecZipWithPrim vecTcNm)
                     [ Right aTy
                     , Right (mkTyConApp vecTcNm [numTy (m' - 1), aTy])
                     , Right (mkTyConApp vecTcNm [mTy, aTy])
                     , Right nTy
                     , Left (mkApps (Data consCon)
                               [Right mTy, Right aTy, Right (numTy (m' - 1)), Left (primCo consCoTy)])
                     , Left (Either.lefts vArgs !! 1)
                     , Left (mkApps (Prim pInfo)
                               [ Right nTy
                               , Right (numTy (m' - 1))
                               , Right aTy
                               , Left (valToTerm kn)
                               , Left (Either.lefts vArgs !! 2) ]) ]
             _ -> Nothing
      _ -> Nothing

  -- A reduction that evaluates an argument with a nested machine, and
  -- continues with the WHNF of a term in that machine
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.rotateLeftS) $ \case
      PrimStepContext{..} -- :: KnownNat n => Vec n a -> SNat d -> Vec n a
        | nTy : aTy : _ : _ <- tys
        , kn : xs : d : _ <- args
        , DC dc vArgs <- xs
        , Right n <- runExcept (tyNatSize te sc nTy)
        -> case n of
             0 -> reduce (mkVecNil te sc dc aTy)
             n' | DC snatDc [_, Left d'] <- d
                -> whnf evalEnv isSubj mach {mStack = [], mTerm = d'} $ \mach2 ->
                     case mTerm mach2 of
                       Literal (NaturalLiteral d2) -> case d2 `mod` n of
                         0 -> reduce (valToTerm xs)
                         d3 | Just vecTcNm <- tyConNameOf (snd (primArgResTys pInfo))
                            , Just (nilCon, consCon) <- vecDataCons te vecTcNm
                            -> reduceWHNF' mach2 $ sink $
                               mkApps (Prim pInfo)
                                 [ Right nTy
                                 , Right aTy
                                 , Right (numTy (d3 - 1))
                                 , Left (valToTerm kn)
                                 , Left (mkApps (vecAppendPrim vecTcNm)
                                           [ Right (numTy (n' - 1))
                                           , Right aTy
                                           , Right (numTy 1)
                                           , Left (Either.lefts vArgs !! 2)
                                           , Left (mkVec te sc nilCon consCon aTy 1
                                                     [Either.lefts vArgs !! 1]) ])
                                 , Left (snatTerm snatDc (d3 - 1)) ]
                            | otherwise -> Nothing
                       _ -> Nothing
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.rotateRightS) $ \case
      PrimStepContext{..} -- :: KnownNat n => Vec n a -> SNat d -> Vec n a
        | isSubj
        , nTy : aTy : _ : _ <- tys
        , kn : xs : d : _ <- args
        , DC dc _ <- xs
        , Right n <- runExcept (tyNatSize te sc nTy)
        -> case n of
             0 -> reduce (mkVecNil te sc dc aTy)
             n' | DC snatDc [_, Left d'] <- d
                -> whnf evalEnv isSubj mach {mStack = [], mTerm = d'} $ \mach2 ->
                     case mTerm mach2 of
                       Literal (NaturalLiteral d2) -> case d2 `mod` n of
                         0 -> reduce (valToTerm xs)
                         d3 | Just vecTcNm <- tyConNameOf (snd (primArgResTys pInfo))
                            -> reduceWHNF' mach2 $ sink $
                               mkApps (Prim pInfo)
                                 [ Right nTy
                                 , Right aTy
                                 , Right (numTy (d3 - 1))
                                 , Left (valToTerm kn)
                                 , Left (mkVecCons te sc dc aTy n
                                           (mkApps ((vecLastPrim vecTcNm))
                                              [Right (numTy (n' - 1)), Right aTy, Left (valToTerm xs)])
                                           (mkApps ((vecInitPrim vecTcNm))
                                              [Right (numTy (n' - 1)), Right aTy, Left (valToTerm xs)]))
                                 , Left (snatTerm snatDc (d3 - 1)) ]
                            | otherwise -> Nothing
                       _ -> Nothing
             _ -> Nothing
      _ -> Nothing

-- Element-wise operations
-- - mapping
  -- A reduction that embeds an argument in its result. Its arguments are in
  -- the scope of the machine, so they can be embedded as they are.
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.map) $ \case
      PrimStepContext{..} -- :: (a -> b) -> Vec n a -> Vec n b
        | isSubj
        , DC dc vArgs <- args !! 1
        , aTy : bTy : nTy : _ <- tys
        , Right n <- runExcept (tyNatSize te sc nTy)
        -> case n of
             0  -> reduce (mkVecNil te sc dc bTy)
             n' -> reduce $
                   mkVecCons te sc dc bTy n'
                     (mkApps (valToTerm (args !! 0)) [Left (Either.lefts vArgs !! 1)])
                     (mkApps (Prim pInfo)
                                          [Right aTy
                                          ,Right bTy
                                          ,Right (LitTy (NumTy (n' - 1)))
                                          ,Left (valToTerm (args !! 0))
                                          ,Left (Either.lefts vArgs !! 2)])
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.imap) $ \case
      PrimStepContext{..}
        -- :: forall n a b . KnownNat n => (Index n -> a -> b) -> Vec n a -> Vec n b
        | isSubj
        , nTy : aTy : bTy : _ <- tys
        , (_ : fTy : _, resTy) <- primArgResTys pInfo
        , Just vecTcNm <- tyConNameOf resTy
        , indexTy : _ <- funArgTys fTy
        , Just indexTcNm <- tyConNameOf indexTy
        , Right n <- runExcept (tyNatSize te sc nTy)
        -> reduceWHNF $
           mkApps (closedPrim "Clash.Sized.Vector.imap_go" WorkNever (vecImapGoTy vecTcNm indexTcNm))
             [ Right nTy
             , Right nTy
             , Right aTy
             , Right bTy
             , Left (valToTerm (args !! 1))
             , Left (valToTerm (args !! 2))
             , Left (mkIndexLit (mkTyConApp indexTcNm [nTy]) nTy n 0) ]
      _ -> Nothing

  , primStepEntry "Clash.Sized.Vector.imap_go" $ \case
      PrimStepContext{..}
        -- :: forall n m a b. (Index n -> a -> b) -> Vec m a -> Index n -> Vec m b
        | isSubj
        , nTy : mTy : aTy : bTy : _ <- tys
        , f : xs : Suspend nArg : _ <- args
        , DC dc vArgs <- xs
        , Right n' <- runExcept (tyNatSize te sc nTy)
        , Right m <- runExcept (tyNatSize te sc mTy)
        -> case m of
             0 -> reduce (mkVecNil te sc dc bTy)
             m' | (_ : _ : indexTy : _, _) <- primArgResTys pInfo
                , Just indexTcNm <- tyConNameOf indexTy
                , _ : x : xs' : _ <- Either.lefts vArgs
                -> whnfArg nArg $ \mach1 n ->
                     let nTy1 = sink nTy
                         bTy1 = sink bTy
                         f1 = sink (valToTerm f)
                         indexAdd =
                           closedPrim (showt '(Clash.Sized.Internal.Index.+#)) WorkVariable
                             (indexAddTy indexTcNm)
                     in Just $ Next mach1
                          { mTerm =
                              mkVecCons te (mScope mach1) dc bTy1 m'
                                (mkApps f1 [Left n, Left (sink x)])
                                (mkApps (Prim pInfo)
                                   [ Right nTy1
                                   , Right (numTy (m' - 1))
                                   , Right (sink aTy)
                                   , Right bTy1
                                   , Left f1
                                   , Left (sink xs')
                                   , Left (mkApps indexAdd
                                             [ Right nTy1
                                             , Left (natLit n')
                                             , Left n
                                             , Left (mkIndexLit (mkTyConApp indexTcNm [nTy1]) nTy1 n' 1) ]) ]) }
             _ -> Nothing
      _ -> Nothing

  -- :: forall n a. KnownNat n => (a -> a) -> a -> Vec n a
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.iterateI) $ \case
      PrimStepContext{..}
        | isSubj
        , [nTy, aTy] <- tys
        , [_n, f, a] <- args
        , Right n <- runExcept (tyNatSize te sc nTy)
        , Just vecTcNm <- tyConNameOf (snd (primArgResTys pInfo))
        , Just (nilCon, consCon) <- vecDataCons te vecTcNm
        -> case n of
             0 -> reduce (mkVecNil te sc nilCon aTy)
             _ -> reduce $
                  mkVecCons te sc consCon aTy n
                    (valToTerm a)
                    (mkApps (Prim pInfo)
                       [ Right (numTy (n - 1))
                       , Right aTy
                       , Left (natLit (n - 1))
                       , Left (valToTerm f)
                       , Left (mkApps (valToTerm f) [Left (valToTerm a)]) ])
      _ -> Nothing

-- - Zipping
  -- A reduction that allocates: @zipWith@ shares the function and the second
  -- vector by let-binding them on the heap (see #3308), so its result lives
  -- in an extension of the scope of the primitive, into which everything from
  -- before has to be sunk.
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.zipWith) $ \case
      PrimStepContext{..} -- :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
        | isSubj
        , aTy : bTy : cTy : nTy : _ <- tys
        , [f, DC dc vArgs, ys] <- args
        , Just vecTcNm <- tyConNameOf (snd (primArgResTys pInfo))
        , Right n <- runExcept (tyNatSize te sc nTy)
        -> case n of
             0 -> reduce (mkVecNil te sc dc cTy)
             _ | _ : x : xs : _ <- Either.lefts vArgs ->
                 newLetBinding evalEnv mach (valToTerm f) $ \m1 fV ->
                   newLetBinding evalEnv m1 (sink (valToTerm ys)) $ \m2 ysV ->
                     let n1 = numTy (n - 1)
                         fT = sink (heapVarTerm fV)
                         ysT = heapVarTerm ysV
                         bTy' = sink bTy
                     in reduceWith m2 $ mkVecCons te (mScope m2) dc (sink cTy) n
                          (mkApps fT
                             [ Left (sink x)
                             , Left (mkApps ((vecHeadPrim vecTcNm))
                                       [Right n1, Right bTy', Left ysT]) ])
                          (mkApps (Prim pInfo)
                             [ Right (sink aTy), Right bTy', Right (sink cTy), Right n1, Left fT
                             , Left (sink xs)
                             , Left (mkApps ((vecTailPrim vecTcNm))
                                       [Right n1, Right bTy', Left ysT]) ])
               | otherwise -> Nothing
      _ -> Nothing

-- Folding
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.foldr) $ \case
      PrimStepContext{..} -- :: (a -> b -> b) -> b -> Vec n a -> b
        | isSubj
        , aTy : bTy : nTy : _ <- tys
        , f : z : xs : _ <- args
        , DC _ vArgs <- xs
        , Right n <- runExcept (tyNatSize te sc nTy)
        -> case n of
             0 -> reduce (valToTerm z)
             _ -> reduceWHNF $
                  mkApps (valToTerm f)
                    [ Left (Either.lefts vArgs !! 1)
                    , Left (mkApps (Prim pInfo)
                              [ Right aTy
                              , Right bTy
                              , Right (numTy (n - 1))
                              , Left (valToTerm f)
                              , Left (valToTerm z)
                              , Left (Either.lefts vArgs !! 2) ]) ]
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.fold) $ \case
      PrimStepContext{..} -- :: (a -> a -> a) -> Vec (n + 1) a -> a
        | isSubj
        , nTy : aTy : _ <- tys
        , f : vs : _ <- args
        , DC _ vArgs <- vs
        , Right n <- runExcept (tyNatSize te sc nTy)
        -> case n of
             0 -> reduceWHNF (Either.lefts vArgs !! 1)
             _ | (_ : vecTy : _, _) <- primArgResTys pInfo
               , Just vecTcNm <- tyConNameOf vecTy
               , Just tupDc <- pairDataCon te
               -> let n' = n + 1
                      m = n' `div` 2
                      n1 = n' - m
                      mVecTy = mkTyConApp vecTcNm [numTy m, aTy]
                      n1VecTy = mkTyConApp vecTcNm [numTy n1, aTy]
                      splitAtCall =
                        mkApps (closedPrim "Clash.Sized.Vector.fold_split" WorkNever
                                  (foldSplitAtTy vecTcNm))
                          [ Right (numTy m), Right (numTy n1), Right aTy
                          , Left (natLit m), Left (valToTerm vs) ]
                      sel first = selectPair sc tupDc ("as", mVecTy) ("bs", n1VecTy) first splitAtCall
                      -- @fold@ of one half
                      foldHalf len half =
                        mkApps (Prim pInfo)
                          [Right (numTy (len - 1)), Right aTy, Left (valToTerm f), Left half]
                  in reduceWHNF $
                     mkApps (valToTerm f)
                       [Left (foldHalf m (sel True)), Left (foldHalf n1 (sel False))]
             _ -> Nothing
      _ -> Nothing

  , primStepEntry "Clash.Sized.Vector.fold_split" $ \case
      PrimStepContext{..} -- :: Natural -> Vec (m + n) a -> (Vec m a, Vec n a)
        | isSubj
        , mTy : nTy : aTy : _ <- tys
        , Right m <- runExcept (tyNatSize te sc mTy)
        , (_ : vecTy : _, _) <- primArgResTys pInfo
        , Just vecTcNm <- tyConNameOf vecTy
        , Just (nilCon, consCon) <- vecDataCons te vecTcNm
        , Just tupDc <- pairDataCon te
        -> let -- The type arguments of the result type, instantiated
               tyArgs = [mkTyConApp vecTcNm [mTy, aTy], mkTyConApp vecTcNm [nTy, aTy]]
               m1VecTy = mkTyConApp vecTcNm [numTy (m - 1), aTy]
               nVecTy = mkTyConApp vecTcNm [nTy, aTy]
               -- A field of the recursive call
               sel first v =
                 selectPair sc tupDc ("l", m1VecTy) ("r", nVecTy) first $
                   mkApps (Prim pInfo)
                     [ Right (numTy (m - 1)), Right nTy, Right aTy
                     , Left (natLit (m - 1)), Left v ]
           in case m of
                -- (Nil, v)
                0 -> reduce $ mkApps (Data tupDc) $ map Right tyArgs ++
                       [ Left (mkVecNil te sc nilCon aTy)
                       , Left (valToTerm (last args)) ]
                -- (x : fst (fold_split (m-1) xs), snd (fold_split (m-1) xs))
                m' | DC _ vArgs <- last args
                   -> reduce $ mkApps (Data tupDc) $ map Right tyArgs ++
                        [ Left (mkVecCons te sc consCon aTy m' (Either.lefts vArgs !! 1)
                                  (sel True (Either.lefts vArgs !! 2)))
                        , Left (sel False (Either.lefts vArgs !! 2)) ]
                -- The vector doesn't reduce to a data constructor
                _ -> Nothing
      _ -> Nothing

-- - Specialised folds
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.dfold) $ \case
      PrimStepContext{..}
        -- :: KnownNat k => Proxy p -> (forall l . l + 1 <= k => SNat l -> a -> (p @@ l) -> (p @@ (l + 1)))
        --    -> (p @@ 0) -> Vec k a -> (p @@ k)
        | isSubj
        , pTy : kTy : aTy : _ <- tys
        , _ : p : f : z : xs : _ <- args
        , DC _ vArgs <- xs
        , Right k' <- runExcept (tyNatSize te sc kTy)
        -> case k' of
             0 -> reduce (valToTerm z)
             _ | -- The type of @f@ of this call, and the constraint of its
                 -- instance at @k-1@. The data constructor of the latter is
                 -- its evidence.
                 SplitFunForall (PiArg _ (PiArg _ (PiArg fTy _))) _ <-
                   splitFunForallTy (piResultTys te sc ty tys)
               , FunTy witnessTy _ <- tyView (piResultTy te sc fTy (numTy (k' - 1)))
               , TyConApp witnessTcNm _ <- tyView (normalizeType te sc witnessTy)
               , Just witnessTc <- lookupTyCon te witnessTcNm
               , ubp : _ <- tyConDataCons witnessTc
               , (_ : _ : fClashTy : _, _) <- primArgResTys pInfo
               , _ : snatTy : _ <- funArgTys fClashTy
               , Just snatTcNm <- tyConNameOf snatTy
               , Just snatTc <- lookupTyCon te snatTcNm
               , [snatDc] <- tyConDataCons snatTc
               -> reduceWHNF $
                  mkApps (valToTerm f)
                    [ Right (numTy (k' - 1))
                    , Left (Data ubp)
                    , Left (snatTerm snatDc (k' - 1))
                    , Left (Either.lefts vArgs !! 1)
                    , Left (mkApps (Prim pInfo)
                              [ Right pTy
                              , Right (numTy (k' - 1))
                              , Right aTy
                              , Left (natLit (k' - 1))
                              , Left (valToTerm p)
                              , Left (valToTerm f)
                              , Left (valToTerm z)
                              , Left (Either.lefts vArgs !! 2) ]) ]
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.dtfold) $ \case
      PrimStepContext{..}
        -- :: KnownNat k => Proxy p -> (a -> (p @@ 0))
        --    -> (forall l . SNat l -> (p @@ l) -> (p @@ l) -> (p @@ (l + 1)))
        --    -> Vec (2^k) a -> (p @@ k)
        | isSubj
        , pTy : kTy : aTy : _ <- tys
        , _ : p : f : g : xs : _ <- args
        , DC _ vArgs <- xs
        , Right k' <- runExcept (tyNatSize te sc kTy)
        -> case k' of
             0 -> reduceWHNF (mkApps (valToTerm f) [Left (Either.lefts vArgs !! 1)])
             _ | (_ : _ : _ : gTy : vecTy : _, _) <- primArgResTys pInfo
               , Just vecTcNm <- tyConNameOf vecTy
               , snatTy : _ <- funArgTys gTy
               , Just snatTcNm <- tyConNameOf snatTy
               , Just snatTc <- lookupTyCon te snatTcNm
               , [snatDc] <- tyConDataCons snatTc
               , Just tupDc <- pairDataCon te
               -> let k1 = k' - 1
                      k2 = 2 ^ k1
                      halfVecTy = mkTyConApp vecTcNm [numTy k2, aTy]
                      splitAtCall =
                        mkApps (splitAtPrim snatTcNm vecTcNm)
                          [ Right (numTy k2), Right (numTy k2), Right aTy
                          , Left (snatTerm snatDc k2), Left (valToTerm xs) ]
                      -- @dtfold@ of one half. The binders of the pattern have
                      -- the type of the halves.
                      dtfoldHalf first =
                        mkApps (Prim pInfo)
                          [ Right pTy
                          , Right (numTy k1)
                          , Right aTy
                          , Left (natLit k1)
                          , Left (valToTerm p)
                          , Left (valToTerm f)
                          , Left (valToTerm g)
                          , Left (selectPair sc tupDc ("xsL", halfVecTy) ("xsR", halfVecTy) first
                                    splitAtCall) ]
                  in reduceWHNF $
                     mkApps (valToTerm g)
                       [ Right (numTy k1)
                       , Left (snatTerm snatDc k1)
                       , Left (dtfoldHalf True)
                       , Left (dtfoldHalf False) ]
             _ -> Nothing
      _ -> Nothing

-- Misc
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.lazyV) $ \case
      PrimStepContext{..} -- :: KnownNat n => Vec n a -> Vec n a
        | isSubj
        , nTy : aTy : _ <- tys
        , _ : xs : _ <- args
        , Just vecTcNm <- tyConNameOf (snd (primArgResTys pInfo))
        , Right n <- runExcept (tyNatSize te sc nTy)
        , Just (nilCon, consCon) <- vecDataCons te vecTcNm
        -> case n of
             0 -> reduce (mkVecNil te sc nilCon aTy)
             n' -> reduce $
                   mkVecCons te sc consCon aTy n'
                     (mkApps ((vecHeadPrim vecTcNm))
                        [Right (numTy (n' - 1)), Right aTy, Left (valToTerm xs)])
                     (mkApps (Prim pInfo)
                        [ Right (numTy (n' - 1))
                        , Right aTy
                        , Left (natLit (n' - 1))
                        , Left (mkApps ((vecTailPrim vecTcNm))
                                  [Right (numTy (n' - 1)), Right aTy, Left (valToTerm xs)]) ])
      _ -> Nothing

-- Traversable
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.traverse#) $ \case
      PrimStepContext{..} -- :: Applicative f => (a -> f b) -> Vec n a -> f (Vec n b)
        | isSubj
        , aTy : fTy : bTy : nTy : _ <- tys
        , apDict : f : xs : _ <- args
        , DC dc vArgs <- xs
        , Right n <- runExcept (tyNatSize te sc nTy)
        , (_ : _ : vecTy : _, _) <- primArgResTys pInfo
        , Just vecTcNm <- tyConNameOf vecTy
        -> let apSel = mkSelectorCase $(curLoc) sc te
           in case n of
                0 -> reduceWHNF $
                     mkApps (apSel (valToTerm apDict) 1 1)
                       [Right (mkTyConApp vecTcNm [nTy, bTy]), Left (mkVecNil te sc dc bTy)]
                _ | let n'ty = numTy (n - 1)
                  , Just (consCoTy : _) <- dataConInstArgTys te sc dc [nTy, bTy, n'ty]
                  -> let fmapF = apSel (apSel (valToTerm apDict) 1 0) 1 0
                         apF = apSel (valToTerm apDict) 1 2
                     in reduceWHNF $
                        mkApps apF
                          [ Right (mkTyConApp vecTcNm [n'ty, bTy])
                          , Right (mkTyConApp vecTcNm [nTy, bTy])
                          , Left (mkApps fmapF
                                    [ Right bTy
                                    , Right (mkFunTy (mkTyConApp vecTcNm [n'ty, bTy])
                                                     (mkTyConApp vecTcNm [nTy, bTy]))
                                    , Left (mkApps (Data dc)
                                              [Right nTy, Right bTy, Right n'ty, Left (primCo consCoTy)])
                                    , Left (mkApps (valToTerm f) [Left (Either.lefts vArgs !! 1)]) ])
                          , Left (mkApps (Prim pInfo)
                                    [ Right aTy
                                    , Right fTy
                                    , Right bTy
                                    , Right n'ty
                                    , Left (valToTerm apDict)
                                    , Left (valToTerm f)
                                    , Left (Either.lefts vArgs !! 2) ]) ]
                _ -> Nothing
      _ -> Nothing

-- BitPack
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.concatBitVector#) $ \case
      PrimStepContext{..}
        -- :: (KnownNat n, KnownNat m) => Vec n (BitVector m) -> BitVector (n * m)
        | isSubj
        , nTy : mTy : _ <- tys
        , _ : km : v : _ <- args
        , DC _ vArgs <- v
        , Right n <- runExcept (tyNatSize te sc nTy)
        -> case n of
             0 -> reduce (mkBitVectorLit' (extractTySizeInfo te sc ty tys) 0 0)
             n' | Right m <- runExcept (tyNatSize te sc mTy)
                , Just bvTcNm <- tyConNameOf (snd (primArgResTys pInfo))
                -> reduceWHNF $
                   mkApps (bvAppendPrim bvTcNm)
                     [ Right (mkTyConApp typeNatMul [numTy (n' - 1), mTy])
                     , Right mTy
                     , Left (natLit ((n' - 1) * m))
                     , Left (Either.lefts vArgs !! 1)
                     , Left (mkApps (Prim pInfo)
                               [ Right (numTy (n' - 1))
                               , Right mTy
                               , Left (natLit (n' - 1))
                               , Left (valToTerm km)
                               , Left (Either.lefts vArgs !! 2) ]) ]
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.unconcatBitVector#) $ \case
      PrimStepContext{..}
        -- :: (KnownNat n, KnownNat m) => BitVector (n * m) -> Vec n (BitVector m)
        | isSubj
        , nTy : mTy : _ <- tys
        , _ : km : bv : _ <- args
        , Just (vecTcNm, [_, bvMTy]) <- tyConAppOf (snd (primArgResTys pInfo))
        , Just bvTcNm <- tyConNameOf bvMTy
        , Right n <- runExcept (tyNatSize te sc nTy)
        , Just (nilCon, consCon) <- vecDataCons te vecTcNm
        -> let mBVTy = mkTyConApp bvTcNm [mTy]
           in case n of
                0 -> reduce (mkVecNil te sc nilCon mBVTy)
                n' | Right m <- runExcept (tyNatSize te sc mTy)
                   , let n1MTy = mkTyConApp typeNatMul [numTy (n' - 1), mTy]
                         n1BVTy = mkTyConApp bvTcNm [n1MTy]
                   , Just (hd, tl) <- case bitVectorLiteral bv of
                       -- Fast path for literals
                       Just (mski, i) ->
                         let sh = fromInteger ((n' - 1) * m)
                             loMask = bit sh - 1
                         in Just
                              ( mkBitVectorLit mBVTy mTy m (mski `shiftR` sh) (i `shiftR` sh)
                              , mkBitVectorLit n1BVTy n1MTy ((n' - 1) * m)
                                  (mski .&. loMask) (i .&. loMask) )
                       Nothing -> do
                         tupDc <- pairDataCon te
                         let splitCall =
                               mkApps (bvSplitPrim bvTcNm)
                                 [ Right n1MTy, Right mTy, Left (natLit ((n' - 1) * m))
                                 , Left (valToTerm bv) ]
                             sel first = selectPair sc tupDc ("x", mBVTy) ("bv'", n1BVTy) first splitCall
                         pure (sel True, sel False)
                   -> reduce $
                      mkVecCons te sc consCon mBVTy n' hd
                        (mkApps (Prim pInfo)
                           [ Right (numTy (n' - 1))
                           , Right mTy
                           , Left (natLit (n' - 1))
                           , Left (valToTerm km)
                           , Left tl ])
                _ -> Nothing
      _ -> Nothing
  ]

-- * Helpers

-- | @Nil@ and @Cons@, given the name of @Vec@
vecDataCons :: TypeEnv -> TyConName -> Maybe (DataCon, DataCon)
vecDataCons tenv nm = case tyConDataCons <$> lookupTyCon tenv nm of
  Just [nilCon, consCon] -> Just (nilCon, consCon)
  _ -> Nothing

-- | The constructor of pairs
pairDataCon :: TypeEnv -> Maybe DataCon
pairDataCon tenv = case tyConDataCons <$> lookupTyCon tenv (tupleTyConName 2) of
  Just [dc] -> Just dc
  _ -> Nothing

-- | A field of a pair: @case scrut of (l, r) -> l@ (or @r@), whose pattern
-- binds fresh variables of the given names and types
selectPair
  :: Distinct n
  => Scope n
  -> DataCon
  -- ^ The constructor of pairs
  -> (Text, Type n)
  -> (Text, Type n)
  -> Bool
  -- ^ Whether to select the first field
  -> Term n
  -> Term n
selectPair sc0 tupDc (lNm, lTy) (rNm, rTy) first scrut =
  withFreshIdBinders sc0 [(C.mkUnsafeSystemName lNm 0, lTy), (C.mkUnsafeSystemName rNm 1, rTy)] $
    \_ fields vars ->
      Case scrut (if first then lTy else rTy)
        [Alt (DataPat tupDc TNil fields) (vars !! (if first then 0 else 1))]

-- | A natural number literal
natLit :: Integer -> Term n
natLit = Literal . NaturalLiteral

-- | An @SNat@ of the given number
snatTerm :: DataCon -> Integer -> Term n
snatTerm snatDc k = mkApps (Data snatDc) [Right (numTy k), Left (natLit k)]
