{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Clash.GHC.Evaluator.Primitives.Clash.Sized.Vector
  ( primitives
  ) where

import qualified Control.Lens               as Lens
import           Control.Monad.Trans.Except (runExcept)
import           Data.Bits
import qualified Data.Either         as Either
import qualified Data.List           as List
import           Data.Text           (Text)
import           Data.Text.Extra     (showt)

import           GHC.Types.Basic     (Boxity (..))
import           GHC.Builtin.Types   (tupleTyCon)

import           Clash.Core.Evaluator.Types
import           Clash.Core.FreeVars (typeFreeVars)
import Clash.Core.HasType (piResultTys)
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Name (mkUnsafeSystemName)
import           Clash.Core.Subst    (extendTvSubst, mkSubst, substTy)
import Clash.Core.Term
  (IsMultiPrim (..),
   Pat (..),
   PrimInfo (..),
   Term (..),
   WorkInfo (..),
   mkApps,
   PrimUnfolding(..))
import Clash.Core.Type
  (Type (..),
   LitTy (..),
   TypeView (..),
   mkFunTy,
   mkTyConApp,
   normalizeType,
   splitFunForallTy,
   tyView)
import Clash.Core.TyCon (tyConDataCons)
import Clash.Core.Util (mkVec, tyNatSize, dataConInstArgTys, primCo, mkSelectorCase)
import Clash.Core.Var (mkLocalId)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Util (curLoc, textNameLit)
import Clash.Normalize.PrimitiveReductions
  (typeNatMul,
   typeNatSub,
   vecLastPrim,
   vecInitPrim,
   vecHeadPrim,
   vecTailPrim,
   mkVecCons,
   mkVecNil)

import qualified Clash.Normalize.Primitives as NP

import {-# SOURCE #-} Clash.GHC.Evaluator

import qualified Clash.Sized.Internal.Index
import qualified Clash.Sized.Vector

import {-# SOURCE #-} Clash.GHC.Evaluator.Primitive
import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
---------
-- Vector
---------
  [ primStepEntry $(textNameLit 'Clash.Sized.Vector.length) $ \case
      PrimStepContext{..} -- :: KnownNat n => Vec n a -> Int
        | isSubj
        , [nTy, _] <- tys
        , Right n <-runExcept (tyNatSize tcm nTy)
        -> let (_, tyView -> TyConApp intTcNm _) = splitFunForallTy ty
               (Just intTc) = UniqMap.lookup intTcNm tcm
               [intCon] = tyConDataCons intTc
           in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral (toInteger n)))])
      _ -> Nothing


  -- XXX: Not a thing anymore?
  , primStepEntry "Clash.Sized.Vector.maxIndex" $ \case
      PrimStepContext{..}
        | isSubj
        , [nTy, _] <- tys
        , Right n <- runExcept (tyNatSize tcm nTy)
        -> let (_, tyView -> TyConApp intTcNm _) = splitFunForallTy ty
               (Just intTc) = UniqMap.lookup intTcNm tcm
               [intCon] = tyConDataCons intTc
           in  reduce (mkApps (Data intCon) [Left (Literal (IntLiteral (toInteger (n - 1))))])
      _ -> Nothing


-- Indexing
  -- XXX: Not exported
  , primStepEntry "Clash.Sized.Vector.index_int" $ \case
      PrimStepContext{..} -- :: KnownNat n => Vec n a -> Int
        | nTy : aTy : _  <- tys
        , _ : xs : i : _ <- args
        , DC intDc [Left (Literal (IntLiteral i'))] <- i
        -> if i' < 0
              then Nothing
              else case xs of
                     DC _ vArgs  -> case runExcept (tyNatSize tcm nTy) of
                        Right 0  -> Nothing
                        Right n' ->
                          if i' == 0
                             then reduceWHNF (Either.lefts vArgs !! 1)
                             else reduceWHNF $
                                  mkApps (Prim pInfo)
                                         [Right (LitTy (NumTy (n'-1)))
                                         ,Right aTy
                                         ,Left (Literal (NaturalLiteral (n'-1)))
                                         ,Left (Either.lefts vArgs !! 2)
                                         ,Left (mkApps (Data intDc)
                                                       [Left (Literal (IntLiteral (i'-1)))])
                                         ]
                        _ -> Nothing
                     _ -> Nothing
      _ -> Nothing

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
        , (Right _ : Right aTy : Right nTy : _) <- vArgs
        , Right n <- runExcept (tyNatSize tcm nTy)
        -> if n == 0
              then reduceWHNF (Either.lefts vArgs !! 1)
              else reduceWHNF
                    (mkApps (Prim pInfo)
                                         [Right (LitTy (NumTy (n-1)))
                                         ,Right aTy
                                         ,Left (Either.lefts vArgs !! 2)
                                         ])
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
        , (Right _ : Right aTy : Right nTy : _) <- vArgs
        , Right n <- runExcept (tyNatSize tcm nTy)
        -> if n == 0
              then reduceWHNF (Either.lefts vArgs !! 2)
              else reduce $
                   mkVecCons consCon aTy n
                      (Either.lefts vArgs !! 1)
                      (mkApps (Prim pInfo)
                                           [Right (LitTy (NumTy (n-1)))
                                           ,Right aTy
                                           ,Left (Either.lefts vArgs !! 2)])
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.select) $ \case
      PrimStepContext{..} -- :: (CmpNat (i+s) (s*n) ~ GT) => SNat f -> SNat s -> SNat n -> Vec (f + i) a -> Vec n a
        | isSubj
        , iTy : sTy : nTy : fTy : aTy : _ <- tys
        , eq : f : s : n : xs : _ <- args
        , Right n' <- runExcept (tyNatSize tcm nTy)
        , Right f' <- runExcept (tyNatSize tcm fTy)
        , Right i' <- runExcept (tyNatSize tcm iTy)
        , Right s' <- runExcept (tyNatSize tcm sTy)
        , DC _ vArgs <- xs
        -> case n' of
             0 -> reduce (mkVecNil nilCon aTy)
             _ -> case f' of
              0 -> let splitAtCall =
                        mkApps (splitAtPrim snatTcNm vecTcNm)
                               [Right sTy
                               ,Right (LitTy (NumTy (i'-s')))
                               ,Right aTy
                               ,Left (valToTerm s)
                               ,Left (valToTerm xs)
                               ]
                       fVecTy = mkTyConApp vecTcNm [sTy,aTy]
                       iVecTy = mkTyConApp vecTcNm [LitTy (NumTy (i'-s')),aTy]
                       -- Guaranteed no capture, so okay to use unsafe name generation
                       fNm    = mkUnsafeSystemName "fxs" 0
                       iNm    = mkUnsafeSystemName "ixs" 1
                       fId    = mkLocalId fVecTy fNm
                       iId    = mkLocalId iVecTy iNm
                       tupPat = DataPat tupDc [] [fId,iId]
                       iAlt   = (tupPat, (Var iId))
                   in  reduce $
                       mkVecCons consCon aTy n' (Either.lefts vArgs !! 1) $
                       mkApps (Prim pInfo)
                              [Right (LitTy (NumTy (i'-s')))
                              ,Right sTy
                              ,Right (LitTy (NumTy (n'-1)))
                              ,Right (LitTy (NumTy 0))
                              ,Right aTy
                              ,Left (valToTerm eq)
                              ,Left (Literal (NaturalLiteral 0))
                              ,Left (valToTerm s)
                              ,Left (Literal (NaturalLiteral (n'-1)))
                              ,Left (Case splitAtCall iVecTy [iAlt])
                              ]
              _ -> let splitAtCall =
                        mkApps (splitAtPrim snatTcNm vecTcNm)
                               [Right fTy
                               ,Right iTy
                               ,Right aTy
                               ,Left (valToTerm f)
                               ,Left (valToTerm xs)
                               ]
                       fVecTy = mkTyConApp vecTcNm [fTy,aTy]
                       iVecTy = mkTyConApp vecTcNm [iTy,aTy]
                       -- Guaranteed no capture, so okay to use unsafe name generation
                       fNm    = mkUnsafeSystemName "fxs" 0
                       iNm    = mkUnsafeSystemName "ixs" 1
                       fId    = mkLocalId fVecTy fNm
                       iId    = mkLocalId iVecTy iNm
                       tupPat = DataPat tupDc [] [fId,iId]
                       iAlt   = (tupPat, (Var iId))
                   in  reduceWHNF $
                       mkApps (Prim pInfo)
                         [Right iTy
                         ,Right sTy
                         ,Right nTy
                         ,Right (LitTy (NumTy 0))
                         ,Right aTy
                         ,Left (valToTerm eq)
                         ,Left (Literal (NaturalLiteral 0))
                         ,Left (valToTerm s)
                         ,Left (valToTerm n)
                         ,Left (Case splitAtCall iVecTy [iAlt])
                         ]
        where
          (tyArgs,tyView -> TyConApp vecTcNm _) = splitFunForallTy ty
          Just vecTc          = UniqMap.lookup vecTcNm tcm
          [nilCon,consCon]    = tyConDataCons vecTc
          TyConApp snatTcNm _ = tyView (Either.rights tyArgs !! 1)
          tupTcNm            = ghcTyconToTyConName (tupleTyCon Boxed 2)
          (Just tupTc)       = UniqMap.lookup tupTcNm tcm
          [tupDc]            = tyConDataCons tupTc
      _ -> Nothing

-- - Splitting
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.splitAt) $ \case
      PrimStepContext{..} -- :: SNat m -> Vec (m + n) a -> (Vec m a, Vec n a)
        | isSubj
        , (DC snatDc (Right mTy:_)):_ <- args
        , Right m <- runExcept (tyNatSize tcm mTy)
        -> let _:nTy:aTy:_ = tys
               -- Get the tuple data-constructor
               ty1 = piResultTys tcm ty tys
               (_,tyView -> TyConApp tupTcNm tyArgs@(tyArg:_)) = splitFunForallTy ty1
               (Just tupTc)       = UniqMap.lookup tupTcNm tcm
               [tupDc]            = tyConDataCons tupTc
               -- Get the vector data-constructors
               TyConApp vecTcNm _ = tyView tyArg
               Just vecTc         = UniqMap.lookup vecTcNm tcm
               [nilCon,consCon]   = tyConDataCons vecTc
               -- Recursive call to @splitAt@
               splitAtRec v =
                mkApps (Prim pInfo)
                       [Right (LitTy (NumTy (m-1)))
                       ,Right nTy
                       ,Right aTy
                       ,Left (mkApps (Data snatDc)
                                     [ Right (LitTy (NumTy (m-1)))
                                     , Left  (Literal (NaturalLiteral (m-1)))])
                       ,Left v
                       ]
               m1VecTy = mkTyConApp vecTcNm [LitTy (NumTy (m-1)),aTy]
               nVecTy  = mkTyConApp vecTcNm [nTy,aTy]
               -- Guaranteed no capture, so okay to use unsafe name generation
               lNm     = mkUnsafeSystemName "l" 0
               rNm     = mkUnsafeSystemName "r" 1
               lId     = mkLocalId m1VecTy lNm
               rId     = mkLocalId nVecTy rNm
               tupPat  = DataPat tupDc [] [lId,rId]
               lAlt    = (tupPat, (Var lId))
               rAlt    = (tupPat, (Var rId))

           in case m of
             -- (Nil,v)
             0 -> reduce $
                  mkApps (Data tupDc) $ (map Right tyArgs) ++
                    [ Left (mkVecNil nilCon aTy)
                    , Left (valToTerm (last args))
                    ]
             -- (x:xs) <- v
             m' | DC _ vArgs <- last args
                -- (x:fst (splitAt (m-1) xs),snd (splitAt (m-1) xs))
                -> case Either.lefts vArgs of
                    (_ : x : xs : _) ->
                      let (mach1, recId) = newLetBinding tcm mach (splitAtRec xs)
                      in reduceWith mach1 $
                        mkApps (Data tupDc) $ (map Right tyArgs) ++
                          [ Left (mkVecCons consCon aTy m' x
                                    (Case (Var recId) m1VecTy [lAlt]))
                          , Left (Case (Var recId) nVecTy [rAlt])
                          ]
                    _ ->
                      -- v actually reduces to Nil and not Cons, this only happens
                      -- when 'n' would reduce to a negative number; the complement
                      -- of 'm'.
                      --
                      -- See Clash issue: https://github.com/clash-lang/clash-compiler/issues/2831
                      let resTy = getResultTy tcm ty tys
                       in reduce (TyApp (Prim NP.undefined) resTy)

             -- v doesn't reduce to a data-constructor
             _  -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.unconcat) $ \case
      PrimStepContext{..} -- :: KnownNat n => SNamt m -> Vec (n * m) a -> Vec n (Vec m a)
        | isSubj
        , kn : snat : v : _  <- args
        , nTy : mTy : aTy :_ <- tys
        , Lit (NaturalLiteral n) <- kn
        -> let ( Either.rights -> argTys, tyView -> TyConApp vecTcNm _) =
                  splitFunForallTy ty
               Just vecTc = UniqMap.lookup vecTcNm tcm
               [nilCon,consCon]   = tyConDataCons vecTc
               tupTcNm            = ghcTyconToTyConName (tupleTyCon Boxed 2)
               (Just tupTc)       = UniqMap.lookup tupTcNm tcm
               [tupDc]            = tyConDataCons tupTc
               TyConApp snatTcNm _ = tyView (argTys !! 1)
               n1mTy  = mkTyConApp typeNatMul
                            [mkTyConApp typeNatSub [nTy,LitTy (NumTy 1)]
                            ,mTy]
               splitAtCall =
                mkApps (splitAtPrim snatTcNm vecTcNm)
                       [Right mTy
                       ,Right n1mTy
                       ,Right aTy
                       ,Left (valToTerm snat)
                       ,Left (valToTerm v)
                       ]
               mVecTy   = mkTyConApp vecTcNm [mTy,aTy]
               n1mVecTy = mkTyConApp vecTcNm [n1mTy,aTy]
               -- Guaranteed no capture, so okay to use unsafe name generation
               asNm     = mkUnsafeSystemName "as" 0
               bsNm     = mkUnsafeSystemName "bs" 1
               asId     = mkLocalId mVecTy asNm
               bsId     = mkLocalId n1mVecTy bsNm
               tupPat   = DataPat tupDc [] [asId,bsId]
               asAlt    = (tupPat, (Var asId))
               bsAlt    = (tupPat, (Var bsId))

           in  case n of
             0 -> reduce (mkVecNil nilCon mVecTy)
             _ -> reduce $
                  mkVecCons consCon mVecTy n
                    (Case splitAtCall mVecTy [asAlt])
                    (mkApps (Prim pInfo)
                        [Right (LitTy (NumTy (n-1)))
                        ,Right mTy
                        ,Right aTy
                        ,Left (Literal (NaturalLiteral (n-1)))
                        ,Left (valToTerm snat)
                        ,Left (Case splitAtCall n1mVecTy [bsAlt])])
      _ -> Nothing

-- Construction
-- - initialisation
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.replicate) $ \case
      PrimStepContext{..} -- :: SNat n -> a -> Vec n a
        | isSubj
        , let ty' = piResultTys tcm ty tys
        , let (_,resTy) = splitFunForallTy ty'
        , (TyConApp vecTcNm [lenTy,argTy]) <- tyView resTy
        , Right len <- runExcept (tyNatSize tcm lenTy)
        -> let (Just vecTc) = UniqMap.lookup vecTcNm tcm
               [nilCon,consCon] = tyConDataCons vecTc
           in  reduce $
               mkVec nilCon consCon argTy len
                     (replicate (fromInteger len) (valToTerm (last args)))
      _ -> Nothing

-- - Concatenation
  , primStepEntry $(textNameLit '(Clash.Sized.Vector.++)) $ \case
      PrimStepContext{..} -- :: Vec n a -> Vec m a -> Vec (n + m) a
        | isSubj
        , (DC dc vArgs):_ <- args
        , Right nTy : Right aTy : _ <- vArgs
        , Right n <- runExcept (tyNatSize tcm nTy)
        -> case n of
             0  -> reduce (valToTerm (last args))
             n' | (_ : _ : mTy : _) <- tys
                , Right m <- runExcept (tyNatSize tcm mTy)
                -> -- x : (xs ++ ys)
                   reduce $
                   mkVecCons dc aTy (n' + m) (Either.lefts vArgs !! 1)
                     (mkApps (Prim pInfo)
                                          [Right (LitTy (NumTy (n'-1)))
                                          ,Right aTy
                                          ,Right mTy
                                          ,Left (Either.lefts vArgs !! 2)
                                          ,Left (valToTerm (last args))
                                          ])
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.concat) $ \case
      PrimStepContext{..} -- :: Vec n (Vec m a) -> Vec (n * m) a
        | isSubj
        , (nTy : mTy : aTy : _)  <- tys
        , (xs : _)               <- args
        , DC dc vArgs <- xs
        , Right n <- runExcept (tyNatSize tcm nTy)
        -> case n of
            0 -> reduce (mkVecNil dc aTy)
            _ | _ : h' : t : _ <- Either.lefts  vArgs
              , (_,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
              -> reduceWHNF $
                 mkApps (vecAppendPrim vecTcNm)
                        [Right mTy
                        ,Right aTy
                        ,Right $ mkTyConApp typeNatMul
                          [mkTyConApp typeNatSub [nTy,LitTy (NumTy 1)], mTy]
                        ,Left h'
                        ,Left $ mkApps (Prim pInfo)
                          [ Right (LitTy (NumTy (n-1)))
                          , Right mTy
                          , Right aTy
                          , Left t
                          ]
                        ]
            _ -> Nothing
      _ -> Nothing


-- Modifying vectors
  , primStepEntry "Clash.Sized.Vector.replace_int" $ \case
      PrimStepContext{..} -- :: KnownNat n => Vec n a -> Int -> a -> Vec n a
        | nTy : aTy : _  <- tys
        , _ : xs : i : a : _ <- args
        , DC intDc [Left (Literal (IntLiteral i'))] <- i
        -> if i' < 0
              then Nothing
              else case xs of
                     DC vecTcNm vArgs -> case runExcept (tyNatSize tcm nTy) of
                        Right 0  -> Nothing
                        Right n' ->
                          if i' == 0
                             then reduce (mkVecCons vecTcNm aTy n' (valToTerm a) (Either.lefts vArgs !! 2))
                             else reduce $
                                  mkVecCons vecTcNm aTy n' (Either.lefts vArgs !! 1)
                                    (mkApps (Prim pInfo)
                                            [Right (LitTy (NumTy (n'-1)))
                                            ,Right aTy
                                            ,Left (Literal (NaturalLiteral (n'-1)))
                                            ,Left (Either.lefts vArgs !! 2)
                                            ,Left (mkApps (Data intDc)
                                                          [Left (Literal (IntLiteral (i'-1)))])
                                            ,Left (valToTerm a)
                                            ])
                        _ -> Nothing
                     _ -> Nothing
      _ -> Nothing


-- - specialized permutations
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.reverse) $ \case
      PrimStepContext{..} -- :: Vec n a -> Vec n a
        | isSubj
        , nTy : aTy : _  <- tys
        , [DC vecDc vArgs] <- args
        -> case runExcept (tyNatSize tcm nTy) of
             Right 0 -> reduce (mkVecNil vecDc aTy)
             Right n
               | (_,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
               , let (Just vecTc) = UniqMap.lookup vecTcNm tcm
               , let [nilCon,consCon] = tyConDataCons vecTc
               -> reduceWHNF $
                  mkApps (vecAppendPrim vecTcNm)
                    [Right (LitTy (NumTy (n-1)))
                    ,Right aTy
                    ,Right (LitTy (NumTy 1))
                    ,Left (mkApps (Prim pInfo)
                                  [Right (LitTy (NumTy (n-1)))
                                  ,Right aTy
                                  ,Left (Either.lefts vArgs !! 2)
                                  ])
                    ,Left (mkVec nilCon consCon aTy 1 [Either.lefts vArgs !! 1])
                    ]
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.transpose) $ \case
      PrimStepContext{..} -- :: KnownNat n => Vec m (Vec n a) -> Vec n (Vec m a)
        | isSubj
        , nTy : mTy : aTy : _ <- tys
        , kn : xss : _ <- args
        , (_,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
        , DC _ vArgs <- xss
        , Right n <- runExcept (tyNatSize tcm nTy)
        , Right m <- runExcept (tyNatSize tcm mTy)
        -> case m of
          0 -> let (Just vecTc)     = UniqMap.lookup vecTcNm tcm
                   [nilCon,consCon] = tyConDataCons vecTc
               in  reduce $
                   mkVec nilCon consCon (mkTyConApp vecTcNm [mTy,aTy]) n
                    (replicate (fromInteger n) (mkVec nilCon consCon aTy 0 []))
          m' -> let (Just vecTc)     = UniqMap.lookup vecTcNm tcm
                    [_,consCon] = tyConDataCons vecTc
                    Just (consCoTy : _) = dataConInstArgTys consCon
                                            [mTy,aTy,LitTy (NumTy (m'-1))]
                in  reduceWHNF $
                    mkApps (vecZipWithPrim vecTcNm)
                           [ Right aTy
                           , Right (mkTyConApp vecTcNm [LitTy (NumTy (m'-1)),aTy])
                           , Right (mkTyConApp vecTcNm [mTy,aTy])
                           , Right nTy
                           , Left  (mkApps (Data consCon)
                                           [Right mTy
                                           ,Right aTy
                                           ,Right (LitTy (NumTy (m'-1)))
                                           ,Left (primCo consCoTy)
                                           ])
                           , Left  (Either.lefts vArgs !! 1)
                           , Left  (mkApps (Prim pInfo)
                                           [ Right nTy
                                           , Right (LitTy (NumTy (m'-1)))
                                           , Right aTy
                                           , Left  (valToTerm kn)
                                           , Left  (Either.lefts vArgs !! 2)
                                           ])
                           ]
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.rotateLeftS) $ \case
      PrimStepContext{..} -- :: KnownNat n => Vec n a -> SNat d -> Vec n a
        | nTy : aTy : _ : _ <- tys
        , kn : xs : d : _ <- args
        , DC dc vArgs <- xs
        , Right n <- runExcept (tyNatSize tcm nTy)
        -> case n of
             0  -> reduce (mkVecNil dc aTy)
             n' | DC snatDc [_,Left d'] <- d
                , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
                , mach2@Machine{mStack=[],mTerm=Literal (NaturalLiteral d2)} <- whnf eval tcm isSubj (setTerm d' $ stackClear mach)
                -> case (d2 `mod` n) of
                     0  -> reduce (valToTerm xs)
                     d3 -> let (_,tyView -> TyConApp vecTcNm _) = splitFunForallTy ty
                               (Just vecTc)     = UniqMap.lookup vecTcNm tcm
                               [nilCon,consCon] = tyConDataCons vecTc
                           in  reduceWHNF' mach2 $
                               mkApps (Prim pInfo)
                                      [Right nTy
                                      ,Right aTy
                                      ,Right (LitTy (NumTy (d3-1)))
                                      ,Left (valToTerm kn)
                                      ,Left (mkApps (vecAppendPrim vecTcNm)
                                                    [Right (LitTy (NumTy (n'-1)))
                                                    ,Right aTy
                                                    ,Right (LitTy (NumTy 1))
                                                    ,Left  (Either.lefts vArgs !! 2)
                                                    ,Left  (mkVec nilCon consCon aTy 1 [Either.lefts vArgs !! 1])])
                                      ,Left (mkApps (Data snatDc)
                                                    [Right (LitTy (NumTy (d3-1)))
                                                    ,Left  (Literal (NaturalLiteral (d3-1)))])
                                      ]
             _  -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.rotateRightS) $ \case
      PrimStepContext{..} -- :: KnownNat n => Vec n a -> SNat d -> Vec n a
        | isSubj
        , nTy : aTy : _ : _ <- tys
        , kn : xs : d : _ <- args
        , DC dc _ <- xs
        , Right n <- runExcept (tyNatSize tcm nTy)
        -> case n of
             0  -> reduce (mkVecNil dc aTy)
             n' | DC snatDc [_,Left d'] <- d
                , eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
                , mach2@Machine{mStack=[],mTerm=Literal (NaturalLiteral d2)} <- whnf eval tcm isSubj (setTerm d' $ stackClear mach)
                -> case (d2 `mod` n) of
                     0  -> reduce (valToTerm xs)
                     d3 -> let (_,tyView -> TyConApp vecTcNm _) = splitFunForallTy ty
                           in  reduceWHNF' mach2 $
                               mkApps (Prim pInfo)
                                      [Right nTy
                                      ,Right aTy
                                      ,Right (LitTy (NumTy (d3-1)))
                                      ,Left (valToTerm kn)
                                      ,Left (mkVecCons dc aTy n
                                              (mkApps (vecLastPrim vecTcNm)
                                                      [Right (LitTy (NumTy (n'-1)))
                                                      ,Right aTy
                                                      ,Left  (valToTerm xs)])
                                              (mkApps (vecInitPrim vecTcNm)
                                                      [Right (LitTy (NumTy (n'-1)))
                                                      ,Right aTy
                                                      ,Left (valToTerm xs)]))
                                      ,Left (mkApps (Data snatDc)
                                                    [Right (LitTy (NumTy (d3-1)))
                                                    ,Left  (Literal (NaturalLiteral (d3-1)))])
                                      ]
             _  -> Nothing
      _ -> Nothing

-- Element-wise operations
-- - mapping
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.map) $ \case
      PrimStepContext{..} -- :: (a -> b) -> Vec n a -> Vec n b
        | isSubj
        , DC dc vArgs <- args !! 1
        , aTy : bTy : nTy : _ <- tys
        , Right n <- runExcept (tyNatSize tcm nTy)
        -> case n of
             0  -> reduce (mkVecNil dc bTy)
             n' -> reduce $
                   mkVecCons dc bTy n'
                     (mkApps (valToTerm (args !! 0)) [Left (Either.lefts vArgs !! 1)])
                     (mkApps (Prim pInfo)
                                          [Right aTy
                                          ,Right bTy
                                          ,Right (LitTy (NumTy (n' - 1)))
                                          ,Left (valToTerm (args !! 0))
                                          ,Left (Either.lefts vArgs !! 2)])
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.imap) $ \case
      PrimStepContext{..} -- :: forall n a b . KnownNat n => (Index n -> a -> b) -> Vec n a -> Vec n b
        | isSubj
        , nTy : aTy : bTy : _ <- tys
        , (tyArgs,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
        , let (tyArgs',_) = splitFunForallTy (Either.rights tyArgs !! 1)
        , TyConApp indexTcNm _ <- tyView (Either.rights tyArgs' !! 0)
        , Right n <- runExcept (tyNatSize tcm nTy)
        , let iLit = mkIndexLit (Either.rights tyArgs' !! 0) nTy n 0
        -> reduceWHNF $
           mkApps (Prim (PrimInfo "Clash.Sized.Vector.imap_go" (vecImapGoTy vecTcNm indexTcNm) WorkNever SingleResult NoUnfolding))
                  [Right nTy
                  ,Right nTy
                  ,Right aTy
                  ,Right bTy
                  ,Left (valToTerm (args !! 1))
                  ,Left (valToTerm (args !! 2))
                  ,Left iLit
                  ]
      _ -> Nothing

  , primStepEntry "Clash.Sized.Vector.imap_go" $ \case
      PrimStepContext{..}
        | isSubj
        , nTy : mTy : aTy : bTy : _ <- tys
        , f : xs : (Suspend nArg) : _ <- args
        , DC dc vArgs <- xs
        , Right n' <- runExcept (tyNatSize tcm nTy)
        , Right m <- runExcept (tyNatSize tcm mTy)
        -> case m of
             0  -> reduce (mkVecNil dc bTy)
             m'
              | eval <- Evaluator ghcStep ghcUnwind ghcPrimStep ghcPrimUnwind
              , mach1@Machine{mStack=[],mTerm=n} <-
                  whnf eval tcm True (setTerm nArg (stackClear mach))
              ->  let (tyArgs,_) = splitFunForallTy ty
                      TyConApp indexTcNm _ = tyView (Either.rights tyArgs !! 2)
                      iLit = mkIndexLit (Either.rights tyArgs !! 2) nTy n' 1
                   in Just $ flip setTerm (mach1 {mStack = mStack mach}) $ mkVecCons dc bTy m'
                     (mkApps (valToTerm f) [Left n,Left (Either.lefts vArgs !! 1)])
                     (mkApps (Prim pInfo)
                             [Right nTy
                             ,Right (LitTy (NumTy (m'-1)))
                             ,Right aTy
                             ,Right bTy
                             ,Left (valToTerm f)
                             ,Left (Either.lefts vArgs !! 2)
                             ,Left (mkApps (Prim (PrimInfo (showt '(Clash.Sized.Internal.Index.+#)) (indexAddTy indexTcNm) WorkVariable SingleResult NoUnfolding))
                                           [Right nTy
                                           ,Left (Literal (NaturalLiteral n'))
                                           ,Left n
                                           ,Left iLit
                                           ])
                             ])
              | otherwise
              -> Nothing
      _ -> Nothing


  -- :: forall n a. KnownNat n => (a -> a) -> a -> Vec n a
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.iterateI) $ \case
      PrimStepContext{..}
        | isSubj
        , [nTy, aTy] <- tys
        , [_n, f, a] <- args
        , Right n <- runExcept (tyNatSize tcm nTy)
        ->
          let
            TyConApp vecTcNm _ = tyView (getResultTy tcm ty tys)
            Just vecTc = UniqMap.lookup vecTcNm tcm
            [nilCon, consCon] = tyConDataCons vecTc
          in case n of
             0 -> reduce (mkVecNil nilCon aTy)
             _ -> reduce $
              mkVecCons consCon aTy n
                (valToTerm a)
                (mkApps
                  (Prim pInfo)
                  [ Right (LitTy (NumTy (n - 1)))
                  , Right aTy
                  , Left (valToTerm (Lit (NaturalLiteral (n - 1))))
                  , Left (valToTerm f)
                  , Left (mkApps (valToTerm f) [Left (valToTerm a)])
                  ])
      _ -> Nothing


-- - Zipping
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.zipWith) $ \case
      PrimStepContext{..} -- :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
        | isSubj
        , aTy : bTy : cTy : nTy : _ <- tys
        , f : xs : ys : _   <- args
        , DC dc vArgs <- xs
        , (_,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
        , Right n <- runExcept (tyNatSize tcm nTy)
        -> case n of
             0  -> reduce (mkVecNil dc cTy)
             -- We share the function 'f' and the second vector 'ys' via heap
             -- let-bindings instead of inlining 'valToTerm f' / 'valToTerm ys'
             -- twice. See #3308.
             n' ->
               let (mach1, fId)  = newLetBinding tcm mach  (valToTerm f)
                   (mach2, ysId) = newLetBinding tcm mach1 (valToTerm ys)
               in reduceWith mach2 $ mkVecCons dc cTy n'
                     (mkApps (Var fId)
                                [Left (Either.lefts vArgs !! 1)
                                ,Left (mkApps (vecHeadPrim vecTcNm)
                                        [Right (LitTy (NumTy (n'-1)))
                                        ,Right bTy
                                        ,Left  (Var ysId)
                                        ])
                                ])
                     (mkApps (Prim pInfo)
                                          [Right aTy
                                          ,Right bTy
                                          ,Right cTy
                                          ,Right (LitTy (NumTy (n' - 1)))
                                          ,Left (Var fId)
                                          ,Left (Either.lefts vArgs !! 2)
                                          ,Left (mkApps (vecTailPrim vecTcNm)
                                                        [Right (LitTy (NumTy (n'-1)))
                                                        ,Right bTy
                                                        ,Left (Var ysId)
                                                        ])])
      _ -> Nothing


-- Folding
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.foldr) $ \case
      PrimStepContext{..} -- :: (a -> b -> b) -> b -> Vec n a -> b
        | isSubj
        , aTy : bTy : nTy : _ <- tys
        , f : z : xs : _ <- args
        , DC _ vArgs <- xs
        , Right n <- runExcept (tyNatSize tcm nTy)
        -> case n of
             0 -> reduce (valToTerm z)
             _ -> reduceWHNF $
                  mkApps (valToTerm f)
                         [Left (Either.lefts vArgs !! 1)
                         ,Left (mkApps (Prim pInfo)
                                       [Right aTy
                                       ,Right bTy
                                       ,Right (LitTy (NumTy (n-1)))
                                       ,Left  (valToTerm f)
                                       ,Left  (valToTerm z)
                                       ,Left  (Either.lefts vArgs !! 2)
                                       ])
                         ]
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.fold) $ \case
      PrimStepContext{..} -- :: (a -> a -> a) -> Vec (n + 1) a -> a
        | isSubj
        , nTy : aTy :  _ <- tys
        , f : vs : _ <- args
        , DC _ vArgs <- vs
        , Right n <- runExcept (tyNatSize tcm nTy)
        -> case n of
             0 -> reduceWHNF (Either.lefts vArgs !! 1)
             _ -> let (tyArgs,_)         = splitFunForallTy ty
                      TyConApp vecTcNm _ = tyView (Either.rights tyArgs !! 1)
                      tupTcNm      = ghcTyconToTyConName (tupleTyCon Boxed 2)
                      (Just tupTc) = UniqMap.lookup tupTcNm tcm
                      [tupDc]      = tyConDataCons tupTc
                      n'     = n+1
                      m      = n' `div` 2
                      n1     = n' - m
                      mTy    = LitTy (NumTy m)
                      m'ty   = LitTy (NumTy (m-1))
                      n1mTy  = LitTy (NumTy n1)
                      n1m'ty = LitTy (NumTy (n1-1))
                      splitAtCall =
                       mkApps (Prim (PrimInfo "Clash.Sized.Vector.fold_split" (foldSplitAtTy vecTcNm) WorkNever SingleResult NoUnfolding))
                              [Right mTy
                              ,Right n1mTy
                              ,Right aTy
                              ,Left (Literal (NaturalLiteral m))
                              ,Left (valToTerm vs)
                              ]
                      mVecTy   = mkTyConApp vecTcNm [mTy,aTy]
                      n1mVecTy = mkTyConApp vecTcNm [n1mTy,aTy]
                      -- Guaranteed no capture, so okay to use unsafe name generation
                      asNm     = mkUnsafeSystemName "as" 0
                      bsNm     = mkUnsafeSystemName "bs" 1
                      asId     = mkLocalId mVecTy asNm
                      bsId     = mkLocalId n1mVecTy bsNm
                      tupPat   = DataPat tupDc [] [asId,bsId]
                      asAlt    = (tupPat, (Var asId))
                      bsAlt    = (tupPat, (Var bsId))
                  in  reduceWHNF $
                      mkApps (valToTerm f)
                             [Left (mkApps (Prim pInfo)
                                           [Right m'ty
                                           ,Right aTy
                                           ,Left (valToTerm f)
                                           ,Left (Case splitAtCall mVecTy [asAlt])
                                           ])
                             ,Left (mkApps (Prim pInfo)
                                           [Right n1m'ty
                                           ,Right aTy
                                           ,Left  (valToTerm f)
                                           ,Left  (Case splitAtCall n1mVecTy [bsAlt])
                                           ])
                             ]
      _ -> Nothing


  , primStepEntry "Clash.Sized.Vector.fold_split" $ \case
      PrimStepContext{..} -- :: Natural -> Vec (m + n) a -> (Vec m a, Vec n a)
        | isSubj
        , mTy : nTy : aTy : _ <- tys
        , Right m <- runExcept (tyNatSize tcm mTy)
        -> let -- Get the tuple data-constructor
               ty1 = piResultTys tcm ty tys
               (_,tyView -> TyConApp tupTcNm tyArgs@(tyArg:_)) = splitFunForallTy ty1
               (Just tupTc)       = UniqMap.lookup tupTcNm tcm
               [tupDc]            = tyConDataCons tupTc
               -- Get the vector data-constructors
               TyConApp vecTcNm _ = tyView tyArg
               Just vecTc         = UniqMap.lookup vecTcNm tcm
               [nilCon,consCon]   = tyConDataCons vecTc
               -- Recursive call to @splitAt@
               splitAtRec v =
                mkApps (Prim pInfo)
                       [Right (LitTy (NumTy (m-1)))
                       ,Right nTy
                       ,Right aTy
                       ,Left (Literal (NaturalLiteral (m-1)))
                       ,Left v
                       ]
               -- Projection either the first or second field of the recursive
               -- call to @splitAt@
               splitAtSelR v = Case (splitAtRec v)
               m1VecTy = mkTyConApp vecTcNm [LitTy (NumTy (m-1)),aTy]
               nVecTy  = mkTyConApp vecTcNm [nTy,aTy]
               -- Guaranteed no capture, so okay to use unsafe name generation
               lNm     = mkUnsafeSystemName "l" 0
               rNm     = mkUnsafeSystemName "r" 1
               lId     = mkLocalId m1VecTy lNm
               rId     = mkLocalId nVecTy rNm
               tupPat  = DataPat tupDc [] [lId,rId]
               lAlt    = (tupPat, (Var lId))
               rAlt    = (tupPat, (Var rId))
           in case m of
             -- (Nil,v)
             0 -> reduce $
                  mkApps (Data tupDc) $ (map Right tyArgs) ++
                    [ Left (mkVecNil nilCon aTy)
                    , Left (valToTerm (last args))
                    ]
             -- (x:xs) <- v
             m' | DC _ vArgs <- last args
                -- (x:fst (splitAt (m-1) xs),snd (splitAt (m-1) xs))
                -> reduce $
                   mkApps (Data tupDc) $ (map Right tyArgs) ++
                     [ Left (mkVecCons consCon aTy m' (Either.lefts vArgs !! 1)
                               (splitAtSelR (Either.lefts vArgs !! 2) m1VecTy [lAlt]))
                     , Left (splitAtSelR (Either.lefts vArgs !! 2) nVecTy [rAlt])
                     ]
             -- v doesn't reduce to a data-constructor
             _  -> Nothing
      _ -> Nothing

-- - Specialised folds
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.dfold) $ \case
      PrimStepContext{..}
        | isSubj
        , pTy : kTy : aTy : _ <- tys
        , _ : p : f : z : xs : _ <- args
        , DC _ vArgs <- xs
        , Right k' <- runExcept (tyNatSize tcm kTy)
        -> case k'  of
             0 -> reduce (valToTerm z)
             _ -> let (tyArgs,_)  = splitFunForallTy ty
                      (tyArgs',_) = splitFunForallTy (Either.rights tyArgs !! 2)
                      Just (tvN, _) = List.uncons $ Either.lefts tyArgs'
                      ubpT = Either.rights tyArgs' !! 0
                      fTVs = Lens.toListOf typeFreeVars ubpT
                      Just tvK = List.find (/= tvN) fTVs
                      subst0 = extendTvSubst (mkSubst is0) tvN k'ty
                      subst1 = extendTvSubst subst0 tvK (LitTy (NumTy k'))
                      witness = normalizeType tcm (substTy subst1 ubpT)
                      TyConApp tupTcNm _ = tyView witness
                      Just witnessTc = UniqMap.lookup tupTcNm tcm
                      ubp : _ = tyConDataCons witnessTc
                      TyConApp snatTcNm _ = tyView (Either.rights tyArgs' !! 1)
                      Just snatTc = UniqMap.lookup snatTcNm tcm
                      [snatDc]    = tyConDataCons snatTc
                      k'ty        = LitTy (NumTy (k'-1))
                  in  reduceWHNF $
                      mkApps (valToTerm f)
                             [Right k'ty
                             ,Left (Data ubp)
                             ,Left (mkApps (Data snatDc)
                                           [Right k'ty
                                           ,Left (Literal (NaturalLiteral (k'-1)))])
                             ,Left (Either.lefts vArgs !! 1)
                             ,Left (mkApps (Prim pInfo)
                                           [Right pTy
                                           ,Right k'ty
                                           ,Right aTy
                                           ,Left (Literal (NaturalLiteral (k'-1)))
                                           ,Left (valToTerm p)
                                           ,Left (valToTerm f)
                                           ,Left (valToTerm z)
                                           ,Left (Either.lefts vArgs !! 2)
                                           ])
                             ]
        where
          is0 = mScopeNames mach
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.dtfold) $ \case
      PrimStepContext{..}
        | isSubj
        , pTy : kTy : aTy : _ <- tys
        , _ : p : f : g : xs : _ <- args
        , DC _ vArgs <- xs
        , Right k' <- runExcept (tyNatSize tcm kTy)
        -> case k' of
             0 -> reduceWHNF (mkApps (valToTerm f) [Left (Either.lefts vArgs !! 1)])
             _ -> let (tyArgs,_)  = splitFunForallTy ty
                      TyConApp vecTcNm _ = tyView (Either.rights tyArgs !! 4)
                      (tyArgs',_) = splitFunForallTy (Either.rights tyArgs !! 3)
                      TyConApp snatTcNm _ = tyView (Either.rights tyArgs' !! 0)
                      Just snatTc = UniqMap.lookup snatTcNm tcm
                      [snatDc]    = tyConDataCons snatTc
                      tupTcNm     = ghcTyconToTyConName (tupleTyCon Boxed 2)
                      (Just tupTc) = UniqMap.lookup tupTcNm tcm
                      [tupDc]     = tyConDataCons tupTc
                      k'ty        = LitTy (NumTy (k'-1))
                      k2ty        = LitTy (NumTy (2^(k'-1)))
                      splitAtCall =
                       mkApps (splitAtPrim snatTcNm vecTcNm)
                              [Right k2ty
                              ,Right k2ty
                              ,Right aTy
                              ,Left (mkApps (Data snatDc)
                                            [Right k2ty
                                            ,Left (Literal (NaturalLiteral (2^(k'-1))))])
                              ,Left (valToTerm xs)
                              ]
                      xsSVecTy = mkTyConApp vecTcNm [k2ty,aTy]
                      -- Guaranteed no capture, so okay to use unsafe name generation
                      xsLNm    = mkUnsafeSystemName "xsL" 0
                      xsRNm    = mkUnsafeSystemName "xsR" 1
                      xsLId    = mkLocalId k2ty xsLNm
                      xsRId    = mkLocalId k2ty xsRNm
                      tupPat   = DataPat tupDc [] [xsLId,xsRId]
                      asAlt    = (tupPat, (Var xsLId))
                      bsAlt    = (tupPat, (Var xsRId))
                  in  reduceWHNF $
                      mkApps (valToTerm g)
                             [Right k'ty
                             ,Left (mkApps (Data snatDc)
                                           [Right k'ty
                                           ,Left (Literal (NaturalLiteral (k'-1)))])
                             ,Left (mkApps (Prim pInfo)
                                           [Right pTy
                                           ,Right k'ty
                                           ,Right aTy
                                           ,Left (Literal (NaturalLiteral (k'-1)))
                                           ,Left (valToTerm p)
                                           ,Left (valToTerm f)
                                           ,Left (valToTerm g)
                                           ,Left (Case splitAtCall xsSVecTy [asAlt])])
                             ,Left (mkApps (Prim pInfo)
                                           [Right pTy
                                           ,Right k'ty
                                           ,Right aTy
                                           ,Left (Literal (NaturalLiteral (k'-1)))
                                           ,Left (valToTerm p)
                                           ,Left (valToTerm f)
                                           ,Left (valToTerm g)
                                           ,Left (Case splitAtCall xsSVecTy [bsAlt])])
                             ]
      _ -> Nothing

-- Misc
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.lazyV) $ \case
      PrimStepContext{..}
        | isSubj
        , nTy : aTy : _ <- tys
        , _ : xs : _ <- args
        , (_,tyView -> TyConApp vecTcNm _) <- splitFunForallTy ty
        , Right n <- runExcept (tyNatSize tcm nTy)
        -> case n of
             0  -> let (Just vecTc) = UniqMap.lookup vecTcNm tcm
                       [nilCon,_]   = tyConDataCons vecTc
                   in  reduce (mkVecNil nilCon aTy)
             n' -> let (Just vecTc) = UniqMap.lookup vecTcNm tcm
                       [_,consCon]  = tyConDataCons vecTc
                   in  reduce $ mkVecCons consCon aTy n'
                         (mkApps (vecHeadPrim vecTcNm)
                                 [ Right (LitTy (NumTy (n' - 1)))
                                 , Right aTy
                                 , Left  (valToTerm xs)
                                 ])
                         (mkApps (Prim pInfo)
                                 [ Right (LitTy (NumTy (n' - 1)))
                                 , Right aTy
                                 , Left  (Literal (NaturalLiteral (n'-1)))
                                 , Left  (mkApps (vecTailPrim vecTcNm)
                                                 [ Right (LitTy (NumTy (n'-1)))
                                                 , Right aTy
                                                 , Left  (valToTerm xs)
                                                 ])
                                 ])
      _ -> Nothing

-- Traversable
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.traverse#) $ \case
      PrimStepContext{..}
        | isSubj
        , aTy : fTy : bTy : nTy : _ <- tys
        , apDict : f : xs : _ <- args
        , DC dc vArgs <- xs
        , Right n <- runExcept (tyNatSize tcm nTy)
        -> case n of
             0 -> let (pureF,ids') = runPEM (mkSelectorCase $(curLoc) is0 tcm (valToTerm apDict) 1 1) ids
                  in  reduceWHNF' (mach { mSupply = ids' }) $
                      mkApps pureF
                             [Right (mkTyConApp (vecTcNm) [nTy,bTy])
                             ,Left  (mkVecNil dc bTy)]
             _ -> let ((fmapF,apF),ids') = flip runPEM ids $ do
                        fDict  <- mkSelectorCase $(curLoc) is0 tcm (valToTerm apDict) 1 0
                        fmapF' <- mkSelectorCase $(curLoc) is0 tcm fDict 1 0
                        apF'   <- mkSelectorCase $(curLoc) is0 tcm (valToTerm apDict) 1 2
                        return (fmapF',apF')
                      n'ty = LitTy (NumTy (n-1))
                      Just (consCoTy : _) = dataConInstArgTys dc [nTy,bTy,n'ty]
                  in  reduceWHNF' (mach { mSupply = ids' }) $
                      mkApps apF
                             [Right (mkTyConApp vecTcNm [n'ty,bTy])
                             ,Right (mkTyConApp vecTcNm [nTy,bTy])
                             ,Left (mkApps fmapF
                                           [Right bTy
                                           ,Right (mkFunTy (mkTyConApp vecTcNm [n'ty,bTy])
                                                           (mkTyConApp vecTcNm [nTy,bTy]))
                                           ,Left (mkApps (Data dc)
                                                         [Right nTy
                                                         ,Right bTy
                                                         ,Right n'ty
                                                         ,Left (primCo consCoTy)])
                                           ,Left (mkApps (valToTerm f)
                                                         [Left (Either.lefts vArgs !! 1)])
                                           ])
                             ,Left (mkApps (Prim pInfo)
                                           [Right aTy
                                           ,Right fTy
                                           ,Right bTy
                                           ,Right n'ty
                                           ,Left (valToTerm apDict)
                                           ,Left (valToTerm f)
                                           ,Left (Either.lefts vArgs !! 2)
                                           ])
                             ]
        where
          (tyArgs,_)         = splitFunForallTy ty
          TyConApp vecTcNm _ = tyView (Either.rights tyArgs !! 2)
          (ids, is0) = (mSupply mach, mScopeNames mach)
      _ -> Nothing


-- BitPack
  , primStepEntry $(textNameLit 'Clash.Sized.Vector.concatBitVector#) $ \case
      PrimStepContext{..}
        | isSubj
        , nTy : mTy : _ <- tys
        , _  : km  : v : _ <- args
        , DC _ vArgs <- v
        , Right n <- runExcept (tyNatSize tcm nTy)
        -> case n of
             0  -> let resTyInfo = extractTySizeInfo tcm ty tys
                   in  reduce (mkBitVectorLit' resTyInfo 0 0)
             n' | Right m <- runExcept (tyNatSize tcm mTy)
                , (_,tyView -> TyConApp bvTcNm _) <- splitFunForallTy ty
                -> reduceWHNF $
                   mkApps (bvAppendPrim bvTcNm)
                     [ Right (mkTyConApp typeNatMul [LitTy (NumTy (n'-1)),mTy])
                     , Right mTy
                     , Left (Literal (NaturalLiteral ((n'-1)*m)))
                     , Left (Either.lefts vArgs !! 1)
                     , Left (mkApps (Prim pInfo)
                                    [ Right (LitTy (NumTy (n'-1)))
                                    , Right mTy
                                    , Left (Literal (NaturalLiteral (n'-1)))
                                    , Left (valToTerm km)
                                    , Left (Either.lefts vArgs !! 2)
                                    ])
                     ]
             _ -> Nothing
      _ -> Nothing

  , primStepEntry $(textNameLit 'Clash.Sized.Vector.unconcatBitVector#) $ \case
      PrimStepContext{..}
        | isSubj
        , nTy : mTy : _  <- tys
        , _  : km  : bv : _ <- args
        , (_,tyView -> TyConApp vecTcNm [_,bvMTy]) <- splitFunForallTy ty
        , TyConApp bvTcNm _ <- tyView bvMTy
        , Right n <- runExcept (tyNatSize tcm nTy)
        -> case n of
             0 ->
              let (Just vecTc) = UniqMap.lookup vecTcNm tcm
                  [nilCon,_] = tyConDataCons vecTc
              in  reduce (mkVecNil nilCon (mkTyConApp bvTcNm [mTy]))
             n' | Right m <- runExcept (tyNatSize tcm mTy) ->
              let Just vecTc  = UniqMap.lookup vecTcNm tcm
                  [_,consCon] = tyConDataCons vecTc
                  mBVTy       = mkTyConApp bvTcNm [mTy]
                  n1MTy       = mkTyConApp typeNatMul [LitTy (NumTy (n'-1)),mTy]
                  n1BVTy      = mkTyConApp bvTcNm [n1MTy]

                  (hd, tl) = case bitVectorLiteral bv of
                    -- Fast path for literals
                    Just (mski, i) ->
                      let sh     = fromInteger ((n'-1) * m)
                          loMask = bit sh - 1
                      in  ( mkBitVectorLit mBVTy mTy m
                              (mski `shiftR` sh) (i `shiftR` sh)
                          , mkBitVectorLit n1BVTy n1MTy ((n'-1)*m)
                              (mski .&. loMask) (i .&. loMask)
                          )
                    Nothing ->
                      let tupTcNm     = ghcTyconToTyConName (tupleTyCon Boxed 2)
                          Just tupTc  = UniqMap.lookup tupTcNm tcm
                          [tupDc]     = tyConDataCons tupTc
                          splitCall   =
                            mkApps (bvSplitPrim bvTcNm)
                                   [ Right n1MTy
                                   , Right mTy
                                   , Left (Literal (NaturalLiteral ((n'-1)*m)))
                                   , Left (valToTerm bv)
                                   ]
                          -- Guaranteed no capture, so okay to use unsafe name
                          -- generation
                          xNm         = mkUnsafeSystemName "x" 0
                          bvNm        = mkUnsafeSystemName "bv'" 1
                          xId         = mkLocalId mBVTy xNm
                          bvId        = mkLocalId n1BVTy bvNm
                          tupPat      = DataPat tupDc [] [xId,bvId]
                          xAlt        = (tupPat, (Var xId))
                          bvAlt       = (tupPat, (Var bvId))
                      in  ( Case splitCall mBVTy [xAlt]
                          , Case splitCall n1BVTy [bvAlt]
                          )

              in  reduce $ mkVecCons consCon mBVTy n' hd
                    (mkApps (Prim pInfo)
                            [ Right (LitTy (NumTy (n'-1)))
                            , Right mTy
                            , Left (Literal (NaturalLiteral (n'-1)))
                            , Left (valToTerm km)
                            , Left tl
                            ])
             _ -> Nothing
      _ -> Nothing

  ]
