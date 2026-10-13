{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests of the 'Data.Binary.Binary' instances of scoped Clash Core
  ("Clash.Core.Scoped.Binary"), on generated terms
  ("Test.Clash.Scoped.Gen.Clash"), with the number of tests that 'withTests'
  gives.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Clash.Tests.Core.Scoped.Binary (tests) where

import Control.Exception (ErrorCall (..), evaluate, try)
import Data.Binary (Binary, decode, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Clash.Core.DataCon as C
import qualified Clash.Core.Subst as C (aeqType)
import qualified Clash.Core.Term as C
import qualified Clash.Core.TyCon as C
import qualified Clash.Core.Var as C (varType)
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Scoped.Alpha (aeqTm, aeqTy)
import Clash.Core.Scoped.Binary
import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.Syntax

import Test.Clash.Scoped (abstractFreeVars, binderRaws, forAllScrambled, withTests)
import Test.Clash.Scoped.Gen.Clash
  (InScope (..), dataCons, genInScope, globals, prims, renameBinders, shrinkInScope,
   showTerm, tyConMap)

-- | A generated closed term: the generated term abstracted over its free
-- variables, converted, so its binders are numbered depth-first
closedTerm :: InScope -> Term 'VoidS
closedTerm (InScope fvs tm) = fromClashTermClosed (abstractFreeVars fvs tm)

roundTrip :: Binary a => a -> a
roundTrip = decode . encode

-- | Decoding and encoding again gives the same encoding
reencodes :: Binary a => a -> Property
reencodes a = counterexample "re-encoding" (encode (roundTrip a) === encode a)

-- | Whether decoding fails with an error
decodeFails :: BL.ByteString -> IO Bool
decodeFails bs = do
  r <- try (evaluate (decodeWithHeader "test" bs :: Int))
  pure $ case r of
    Left (ErrorCall _) -> True
    Right _ -> False

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Binary"
  [ withTests 500 $ testProperty "round trip of terms whose binders are numbered depth-first" $
      forAllShrink genInScope shrinkInScope $ \gen ->
        let tm = closedTerm gen
            tm' = roundTrip tm
        in  counterexample (showTerm (toClashTerm tm)) $
              counterexample "alpha-equivalence" (property (aeqTm tm tm'))
                .&&. counterexample "raw names" (binderRaws tm' === binderRaws tm)
                .&&. reencodes tm

  , withTests 300 $ testProperty "round trip of terms with scrambled raw names" $
      forAllShrink genInScope shrinkInScope $ \gen ->
        forAllScrambled emptyScope (closedTerm gen) $ \tm ->
          let tm' = roundTrip tm
          in  counterexample "alpha-equivalence" (property (aeqTm tm tm'))
                -- Decoding numbers the binders depth-first
                .&&. counterexample "raw names" (binderRaws tm' === binderRaws (roundTrip tm'))

    -- The uniques of the human readable names of binders, e.g. GHC's, don't
    -- leak into the encoding, so the hashes of the manifest only depend on
    -- the raw names
  , withTests 300 $ testProperty "the encoding doesn't depend on the uniques of binders" $
      forAllShrink genInScope shrinkInScope $ \(InScope fvs tm) ->
        let closed = abstractFreeVars fvs tm
        in  encode (fromClashTermClosed closed) === encode (fromClashTermClosed (renameBinders closed))

  , testCase "round trip of global variables" $
      mapM_ (\g -> do
               let g0 = mkGlobalIdFromClash g
                   g1 = roundTrip g0
               globalIdName g1 @?= globalIdName g0
               globalIdUniq g1 @?= globalIdUniq g0
               assertBool "type" (aeqTy (globalIdType g1) (globalIdType g0))
               assertBool "legacy type" (C.aeqType (C.varType (globalIdLegacy g1)) (C.varType g))
               encode g1 @?= encode g0)
        globals

  , testCase "round trip of primitives" $
      mapM_ (\p -> do
               let p0 = mkPrimInfoFromClash p
                   p1 = roundTrip p0
               primName p1 @?= primName p0
               assertBool "type" (aeqTy (primType p1) (primType p0))
               assertBool "legacy type" (C.aeqType (C.primType (primLegacy p1)) (C.primType p))
               encode p1 @?= encode p0)
        prims

  , testCase "round trip of data constructors" $
      mapM_ (\dc -> do
               let dc1 = roundTrip dc
               C.dcName dc1 @?= C.dcName dc
               C.dcUniq dc1 @?= C.dcUniq dc
               C.dcTag dc1 @?= C.dcTag dc
               assertBool "type" (aeqTy (C.dcType dc1) (C.dcType dc))
               assertBool "legacy type" (C.aeqType (C.dcLegacyType dc1) (C.dcLegacyType dc))
               length (C.dcUnivTyVars dc1) @?= length (C.dcUnivTyVars dc)
               length (C.dcExtTyVars dc1) @?= length (C.dcExtTyVars dc)
               encode dc1 @?= encode dc)
        dataCons

  , testCase "round trip of type constructors" $
      mapM_ (\tc -> do
               let tc1 = roundTrip tc
               C.tyConUniq tc1 @?= C.tyConUniq tc
               C.tyConName tc1 @?= C.tyConName tc
               C.tyConArity tc1 @?= C.tyConArity tc
               assertBool "kind" (aeqTy (C.tyConKind tc1) (C.tyConKind tc))
               assertBool "legacy kind" (C.aeqType (C.tyConLegacyKind tc1) (C.tyConLegacyKind tc))
               map C.dcName (C.tyConDataCons tc1) @?= map C.dcName (C.tyConDataCons tc)
               encode tc1 @?= encode tc)
        (UniqMap.elems tyConMap)

  , testGroup "header"
      [ testCase "round trip" $
          decodeWithHeader "test" (encodeWithHeader (42 :: Int)) @?= (42 :: Int)
      , testCase "is recognized" $ do
          assertBool "with" (hasHeader (encodeWithHeader (42 :: Int)))
          assertBool "without" (not (hasHeader (encode (42 :: Int))))
      , testCase "rejects another magic number" $ do
          failed <- decodeFails (BL8.pack "NOTCLASH" <> BL.drop 8 (encodeWithHeader (42 :: Int)))
          assertBool "decoded" failed
      , testCase "rejects another version" $ do
          let bs = encodeWithHeader (42 :: Int)
              (hdr, rest) = BL.splitAt 9 bs
          failed <- decodeFails (hdr <> BL.map (+ 1) (BL.take 1 rest) <> BL.drop 1 rest)
          assertBool "decoded" failed
      , testCase "rejects trailing input" $ do
          failed <- decodeFails (encodeWithHeader (42 :: Int) <> BL.singleton 0)
          assertBool "decoded" failed
      ]
  ]
