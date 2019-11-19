{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Clash.Tests.Vector where

import           Data.List (isInfixOf)
import           Language.Haskell.Interpreter (OptionVal((:=)))
import qualified Language.Haskell.Interpreter as Hint
import           Test.Tasty
import           Test.Tasty.HUnit

import           Clash.Prelude
import qualified Clash.Sized.Vector as Vec

typeCheck
  :: ([Hint.GhcError] -> Assertion)
  -> (String -> Assertion)
  -> String
  -> Assertion
typeCheck fErr fTy expr = do
  result <- Hint.runInterpreter $ do
    Hint.reset
    Hint.set [Hint.languageExtensions := langExts]
    Hint.setImports ["Clash.Prelude"]

    mapM_ Hint.runStmt [test0s, test1s]
    Hint.typeChecksWithDetails expr

  case result of
    Left err -> assertFailure $ "Failed to run interpreter: " <> show err
    Right x  -> either fErr fTy x
 where
  langExts =
    [ Hint.DataKinds
    , Hint.GADTs
    , Hint.TypeOperators
    ]

assertType :: String -> String -> Assertion
assertType ty = typeCheck fErr fTy
 where
  fErr es = assertFailure
    $ "Expression failed to typecheck: " <> show es

  fTy = assertEqual "assertType" ty

assertError :: String -> String -> Assertion
assertError expected = typeCheck fErr fTy
 where
  fErr es
    | any containsErr es = return ()
    | otherwise          = assertFailure
        $ "Expression failed with unexpected error: " <> show es
   where
    containsErr e = expected `isInfixOf` show (Hint.errMsg e)

  fTy ty = assertFailure
    $ "Supposedly invalid expression type checked as: " <> show ty

test0s :: String
test0s = "let test0 = undefined :: Vec n a -> Vec n a"

test1s :: String
test1s = mconcat
  [ "let test1 :: Vec n a -> Vec (n + 1) a;"
  , "test1 = undefined"
  ]

tests :: TestTree
tests = testGroup "Vector"
  [ testCase "test0" $ assertType "Vec n a -> Vec n a" "test0"
  , testCase "test1" $ assertType "Int"
      "let x :: Int; (x :> _) = test1 (Nil :: Vec 0 Int) in x"

  , testGroup "Untouchable GADT Patterns" 
    [ testCase "Cons" $ assertError "is untouchable"
        "let f x@(Cons _ _) = x in f undefined"
    
    , testCase "(:>)" $ assertError "is untouchable"
        "let f x@(_ :> _) = x in f undefined"

    , testCase "(:<)" $ assertError "is untouchable"
        "let f x@(_ :< _) = x in f undefined"
    ]

  , testGroup "Pattern Synonym Examples"
    [ testCase "middleL/R" $ middleL vec  @?= middleR vec
    , testCase "issueEx1"  $ issueEx1 vec @?= vec
    , testCase "issueEx2"  $ issueEx2 vec @?= Vec.length vec /= 0
    ]
  ]
 where
  vec :: Vec 10 Int
  vec = $(listToVecTH [(1 :: Int)..10])

-- Functions below should survive recompilation

middleL :: Vec (n + 2) a -> Vec n a
middleL ((_ :> xs) :< _) = xs

middleR :: Vec (n + 2) a -> Vec n a
middleR (_ :> (xs :< _)) = xs

issueEx1 :: (KnownNat n) => Vec n a -> Vec n a
issueEx1 Nil         = Nil
issueEx1 xs@(_ :> _) = f xs
 where
  f :: (1 <= n, KnownNat n) => Vec n a -> Vec n a
  f = id

issueEx2 :: Vec n a -> Bool
issueEx2 Nil      = False
issueEx2 (_ :< _) = True

