{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Signal.Bundle.Internal (deriveBundleTuples, idPrimitive) where

import           Control.Monad.Extra         (concatMapM)
import           Clash.Annotations.Primitive (Primitive(InlinePrimitive))
import           Clash.CPP                   (maxTupleSize)
import           Clash.Signal.Internal       (Signal((:-)))
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
  let bundleTy = ConT bundleTyName
      signal   = ConT ''Signal

      aNamesAll = map (\i -> mkName ('a':show i)) [1..maxTupleSize::Int]
      aPrimeNamesAll = map (\i -> mkName ('a':show i++"'")) [1..maxTupleSize::Int]
      asNamesAll = map (\i -> mkName ("as" <> show i)) [1..maxTupleSize::Int]
      tNm = mkName "t"
      sTailNm = mkName "sTail"
      sNm = mkName "s"

  flip concatMapM [2..maxTupleSize] $ \tupleNum ->
    let aNames = take tupleNum aNamesAll
        aPrimeNames = take tupleNum aPrimeNamesAll
        asNames = take tupleNum asNamesAll
        vars  = fmap VarT aNames

        bundlePrimName = mkName ("bundle" ++ show tupleNum ++ "#")
        unbundlePrimName = mkName ("unbundle" ++ show tupleNum ++ "#")
        qualBundleNm = mkName ("Clash.Signal.Bundle.bundle" ++ show tupleNum ++ "#")
        qualUnbundlePrimName = mkName ("Clash.Signal.Bundle.unbundle" ++ show tupleNum ++ "#")

        mkTupleT = foldl' AppT (TupleT tupleNum)

        -- Instance declaration
        instTy = AppT bundleTy $ mkTupleT vars

        -- Associated type Unbundled
#if MIN_VERSION_template_haskell(2,15,0)
        unbundledTypeEq =
          TySynEqn Nothing
            ((ConT unbundledTyName `AppT`
                VarT tNm ) `AppT` mkTupleT vars )
            $ mkTupleT $ map (AppT (signal `AppT` VarT tNm)) vars
        unbundledType = TySynInstD unbundledTypeEq
#else
        unbundledTypeEq =
          TySynEqn
            [ VarT tNm, mkTupleT vars ]
            $ mkTupleT $ map (AppT (signal `AppT` VarT tNm)) vars
        unbundledType = TySynInstD unbundledTyName unbundledTypeEq
#endif

        mkFunD nm alias = FunD nm [Clause [] (NormalB (VarE alias)) []]
        bundleD = mkFunD bundleName bundlePrimName
        unbundleD = mkFunD unbundleName unbundlePrimName

        sigType t = ConT ''Signal `AppT` VarT (mkName "dom") `AppT` t

        -- unbundle3# ~s@((a, b, c) :- abcs) =
        --   let (as, bs, cs) = s `seq` unbundle3# abcs in
        --   (a :- as, b :- bs, c :- cs)
        unbundleNoInlineAnn = PragmaD (InlineP unbundlePrimName NoInline FunLike AllPhases)

        unbundleSig = SigD unbundlePrimName (
          mkFunTys
            [mkTupleT (map sigType (map VarT aNames))]
            (sigType (mkTupleT (map VarT aNames)))
          )

        seqE nm res = UInfixE (VarE nm) (VarE 'seq) res
        seqXE nm res = UInfixE (VarE nm) (VarE 'seqX) res

        unbundleFBody =
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

        unbundleF =
          FunD
            unbundlePrimName
            [Clause
              [AsP sNm (TildeP (UInfixP
                                 (AsP tNm (TildeP (TupP (map VarP aNames))))
                                 '(:-)
                                 (VarP sTailNm)))]
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
    in do
      unbundlePrimAnn <- idPrimitive qualUnbundlePrimName
      bundlePrimAnn <- idPrimitive qualBundleNm
      pure [ -- Instance and its methods
             InstanceD Nothing [] instTy [unbundledType, bundleD, unbundleD]

             -- Bundle primitive
           , bundleSig, bundleF, bundlePrimAnn, bundleNoInlineAnn

             -- Unbundle primitive
           , unbundleSig, unbundleF, unbundlePrimAnn, unbundleNoInlineAnn
           ]

mkFunTys :: Foldable t => t TH.Type -> TH.Type -> TH.Type
mkFunTys args res= foldl' go res args
 where
  go l r = AppT (AppT ArrowT l) r
