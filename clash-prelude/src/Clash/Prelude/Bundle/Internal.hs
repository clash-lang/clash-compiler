{-|
Copyright  :  (C) 2021,      QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Generate tuple instances for:

  * Applicative f => Bundle f
  * Bundle (Signal dom)
  * Bundle (DSignal dom d)
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Prelude.Bundle.Internal where

import           Control.Monad               (liftM)
import           Clash.Annotations.Primitive (Primitive(InlinePrimitive))
import           Clash.CPP                   (maxTupleSize)
import           Clash.Signal.Internal       (Signal((:-)))
import           Clash.Signal.Delayed.Internal (DSignal(..))
import           Clash.XException            (seqX)
import           Data.List                   (foldl')
import qualified Language.Haskell.TH.Syntax  as TH
import           Language.Haskell.TH
import           Language.Haskell.TH.Compat

idPrimitive :: TH.Name -> DecQ
idPrimitive nm =
  PragmaD . AnnP (ValueAnnotation nm) <$> TH.liftData ip
 where
  ipJson = "[{\"Primitive\": {\"name\": \"" ++ show nm ++ "\", \"primType\": \"Function\"}}]"
  ip = InlinePrimitive [minBound..maxBound] ipJson

-- | Monadic version of concatMap
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)


data InstanceHead
  = SignalIH
  | DSignalIH
  | ApplicativeIH

-- | Contruct all the tuple instances for Bundle.
deriveBundleTuples
  :: Name
  -- ^ Bundle
  -> Name
  -- ^ Unbundled
  -> Name
  -- ^ bundle
  -> Name
  -- ^ unbundle
  -> DecsQ
deriveBundleTuples bundleTyName unbundledTyName bundleName unbundleName = do
  ds1 <- deriveBundleTuples' ApplicativeIH bundleTyName unbundledTyName bundleName unbundleName
  ds2 <- deriveBundleTuples' SignalIH bundleTyName unbundledTyName bundleName unbundleName
  ds3 <- deriveBundleTuples' DSignalIH bundleTyName unbundledTyName bundleName unbundleName
  pure (ds1 ++ ds2 ++ ds3)

-- | Contruct all the tuple instances for Bundle for the given instance head
deriveBundleTuples'
  :: InstanceHead
  -- ^ Instance head
  -> Name
  -- ^ Bundle
  -> Name
  -- ^ Unbundled
  -> Name
  -- ^ bundle
  -> Name
  -- ^ unbundle
  -> DecsQ
deriveBundleTuples' instanceT bundleTyName unbundledTyName bundleName unbundleName = do
  let bundleTy = ConT bundleTyName
      signal   = ConT ''Signal
      dsignal  = ConT ''DSignal

      aNamesAll = map (\i -> mkName ('a':show i)) [1..maxTupleSize::Int]
      aPrimeNamesAll = map (\i -> mkName ('a':show i++"'")) [1..maxTupleSize::Int]
      asNamesAll = map (\i -> mkName ("as" <> show i)) [1..maxTupleSize::Int]
      tNm = mkName "t"
      sTailNm = mkName "sTail"
      sNm = mkName "s"
      xsNm = mkName "xs"
      dom = VarT (mkName "dom")
      d   = VarT (mkName "d")
      f   = VarT (mkName "f")

  flip concatMapM [2..maxTupleSize] $ \tupleNum ->
    let aNames = take tupleNum aNamesAll
        aPrimeNames = take tupleNum aPrimeNamesAll
        asNames = take tupleNum asNamesAll
        vars  = fmap VarT aNames

        bundleNm = case instanceT of
          SignalIH -> "bundle"
          DSignalIH -> "bundleD"
          ApplicativeIH -> error "No overlapping bundle primitive"

        unbundleNm = case instanceT of
          SignalIH -> "unbundle"
          DSignalIH -> "unbundleD"
          ApplicativeIH -> error "No overlapping unbundle primitive"

        bundlePrimName = mkName (bundleNm ++ show tupleNum ++ "#")
        unbundlePrimName = mkName (unbundleNm ++ show tupleNum ++ "#")
        qualBundleNm =
          mkName ("Clash.Prelude.Bundle." ++ bundleNm ++ show tupleNum ++ "#")
        qualUnbundlePrimName =
          mkName ("Clash.Prelude.Bundle." ++ unbundleNm ++ show tupleNum ++ "#")

        mkTupleT = foldl' AppT (TupleT tupleNum)

        bundleHead = case instanceT of
          SignalIH -> signal `AppT` dom
          DSignalIH -> dsignal `AppT` dom `AppT` d
          ApplicativeIH -> f

        -- Instance declaration
        instTy = bundleTy `AppT` bundleHead
                          `AppT` (mkTupleT vars)
                          `AppT` (mkTupleT (map sigType vars))

        -- Associated type Unbundled
#if MIN_VERSION_template_haskell(2,15,0)
        unbundledTypeEq =
          TySynEqn Nothing
            ((ConT unbundledTyName `AppT`
                bundleHead) `AppT` mkTupleT vars )
            $ mkTupleT $ map (AppT bundleHead) vars
        unbundledType = TySynInstD unbundledTypeEq
#else
        unbundledTypeEq =
          TySynEqn
            [ bundleHead, mkTupleT vars ]
            $ mkTupleT $ map (AppT bundleHead) vars
        unbundledType = TySynInstD unbundledTyName unbundledTypeEq
#endif

        mkFunD nm alias = FunD nm [Clause [] (NormalB (VarE alias)) []]
        bundleD = case instanceT of
          ApplicativeIH ->
            FunD bundleName [Clause [TupP (map VarP aNames)] (NormalB bundleFBody) [] ]
          _ -> mkFunD bundleName bundlePrimName
        unbundleD = case instanceT of
          ApplicativeIH ->
            FunD unbundleName [Clause unbundleFPat (NormalB unbundleFBody) [] ]
          _ -> mkFunD unbundleName unbundlePrimName

        sigType t = bundleHead `AppT` t

        unbundleNoInlineAnn = PragmaD (InlineP unbundlePrimName NoInline FunLike AllPhases)

        unbundleSig = SigD unbundlePrimName (
          mkFunTys
            [mkTupleT (map sigType (map VarT aNames))]
            (sigType (mkTupleT (map VarT aNames)))
          )

        seqE nm res = UInfixE (VarE nm) (VarE 'seq) res
        seqXE nm res = UInfixE (VarE nm) (VarE 'seqX) res

        unbundleFBody = case instanceT of
          -- unbundle3# s@~(t@~(a, b, c) :- abcs) =
          --   let (as, bs, cs) = t `seqX` s `seq` unbundle3# abcs in
          --   (a :- as, b :- bs, c :- cs)
          SignalIH ->
            LetE
              [ ValD
                  (TupP (map VarP asNames))
                  (NormalB (
                    tNm `seqXE` (sNm `seqE` (VarE unbundlePrimName `AppE` VarE sTailNm)))) []]
              (mkTupE
                (zipWith
                  (\a as -> UInfixE (VarE a) (ConE '(:-)) (VarE as))
                  aNames
                  asNames))
          DSignalIH ->
            -- unbundle3# xs =
            --   let s@~(t@~(a,b,c) :- abcs) = toSignal xs
            --       (as, bs, cs) = t `seqX` s `seq` unbundle3# abcs in
            --   (a :- as, b :- bs, c :- cs)
            LetE
              [ ValD
                  (AsP sNm (TildeP (UInfixP
                                 (AsP tNm (TildeP (TupP (map VarP aNames))))
                                 '(:-)
                                 (VarP sTailNm))))
                  (NormalB (VarE 'toSignal `AppE` VarE xsNm)) []
              , ValD
                  (TupP (map VarP asNames))
                  (NormalB (
                    tNm `seqXE` (sNm `seqE` (VarE unbundlePrimName `AppE` (ConE 'DSignal `AppE` VarE sTailNm))))) []]
              (mkTupE
                (zipWith
                  (\a as -> ConE 'DSignal `AppE` UInfixE (VarE a) (ConE '(:-)) (VarE 'toSignal `AppE` VarE as))
                  aNames
                  asNames))
          ApplicativeIH ->
            -- unbundle3# s =
            --   ((\(t,_,_) -> t) <$> s, (\(_,t,_) -> t) <$> s, (\(_,_,t) -> t) <$> s)
            mkTupE
              (map (\p -> UInfixE (LamE [TupP p] (VarE tNm)) (VarE '(<$>)) (VarE sNm))
              (take tupleNum (iterate rot (VarP tNm:replicate (tupleNum-1) WildP))))
            where
              rot xs = last xs : init xs


        unbundleFPat = case instanceT of
          SignalIH -> [AsP sNm (TildeP (UInfixP
                                 (AsP tNm (TildeP (TupP (map VarP aNames))))
                                 '(:-)
                                 (VarP sTailNm)))]
          DSignalIH -> [VarP xsNm]
          ApplicativeIH -> [VarP sNm]

        unbundleF =
          FunD
            unbundlePrimName
            [Clause
              unbundleFPat
              (NormalB unbundleFBody)
              [] ]

        -- bundle2# (a1, a2) = (\ a1' a2' -> (a1', a2')) <$> a1 <*> a2
        bundleNoInlineAnn = PragmaD (InlineP bundlePrimName NoInline FunLike AllPhases)

        bundleSig = SigD bundlePrimName (
          mkFunTys
            [sigType (mkTupleT (map VarT aNames))]
            (mkTupleT (map sigType (map VarT aNames)))
          )

        bundleFmap =
          UInfixE
            (LamE (map VarP aPrimeNames) (mkTupE (map VarE aPrimeNames)))
            (VarE '(<$>))
            (VarE (head aNames))

        bundleFBody =
          foldl'
            (\e n -> UInfixE e (VarE '(<*>)) (VarE n))
            bundleFmap
            (tail aNames)

        bundleF =
          FunD
            bundlePrimName
            [Clause
              [TupP (map VarP aNames)]
              (NormalB bundleFBody)
              [] ]
    in case instanceT of
      ApplicativeIH ->
        pure [ -- Instance and its methods
              InstanceD (Just Overlappable) [ConT ''Applicative `AppT` f]
                        instTy [unbundledType, bundleD, unbundleD]
              ]
      _ -> do
        unbundlePrimAnn <- idPrimitive qualUnbundlePrimName
        bundlePrimAnn <- idPrimitive qualBundleNm
        pure [ -- Instance and its methods
              InstanceD Nothing [] instTy [unbundledType, bundleD, unbundleD]

              -- Bundle primitive
            , bundleSig, bundleF, bundlePrimAnn, bundleNoInlineAnn

              -- Unbundle primitive
            , unbundleSig, unbundleF, unbundlePrimAnn, unbundleNoInlineAnn
            ]


mkFunTys :: Foldable t => t Type -> Type -> Type
mkFunTys args res= foldl' go res args
 where
  go l r = AppT (AppT ArrowT l) r
