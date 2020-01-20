{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}

module Clash.Tests.NFDataX where

import           Test.Tasty
import           Test.Tasty.HUnit

import           GHC.Generics         (Generic)
import           Clash.Class.BitPack  (pack)
import           Clash.Sized.Vector   (Vec(..))
import           Clash.XException
  (NFDataX(rnfX, hasUndefined, deepErrorX), errorX, ensureSpine)
import           Data.Ord             (Down (Down))

data Void                                     deriving (Generic, NFDataX)
data Unit       = Unit                        deriving (Generic, NFDataX)
data Wrapper    = Wrapper Int                 deriving (Generic, NFDataX)
data Sum        = SumTypeA | SumTypeB         deriving (Generic, NFDataX)
data BigSum     = BS1 | BS2 | BS3 | BS4 | BS5 deriving (Generic, NFDataX)
data Product    = Product Int Int             deriving (Generic, NFDataX)
data SP         = S Int Int | P Int           deriving (Generic, NFDataX)
data Rec0       = Rec0 {  }                   deriving (Generic, NFDataX)
data Rec1       = Rec1 { a :: Int }           deriving (Generic, NFDataX)
data Rec2       = Rec2 { b :: Int, c :: Int } deriving (Generic, NFDataX)
data ProductRec = ProductRec Rec1 (Unit, Sum) deriving (Generic, NFDataX)

sundef :: NFDataX a => a
sundef = ensureSpine undef

dundef :: NFDataX a => a
dundef = deepErrorX "!"

undef :: a
undef = errorX "!"
{-# NOINLINE undef #-}

tests :: TestTree
tests =
  testGroup
    "NFDataX"
    [ testGroup
        "GenericRnf"
        [ testCase "Unit"     $ rnfX (undef :: Unit)                  @?= ()
        , testCase "Wrapper1" $ rnfX (undef :: Wrapper)               @?= ()
        , testCase "Wrapper2" $ rnfX (Wrapper undef)                  @?= ()
        , testCase "Sum"      $ rnfX (undef :: Sum)                   @?= ()
        , testCase "BigSum"   $ rnfX (undef :: BigSum)                @?= ()
        , testCase "Product1" $ rnfX (undef :: Product)               @?= ()
        , testCase "Product2" $ rnfX (Product undef undef :: Product) @?= ()
        , testCase "Product3" $ rnfX (Product 3 undef :: Product)     @?= ()
        , testCase "Product4" $ rnfX (Product undef 5 :: Product)     @?= ()
        , testCase "SP1"      $ rnfX (undef :: SP)                    @?= ()
        , testCase "SP2"      $ rnfX (S undef undef :: SP)            @?= ()
        , testCase "SP3"      $ rnfX (S 3 undef :: SP)                @?= ()
        , testCase "SP3"      $ rnfX (S undef 5 :: SP)                @?= ()
        , testCase "SP4"      $ rnfX (P undef :: SP)                  @?= ()
        , testCase "Rec0"     $ rnfX (undef :: Rec0)                  @?= ()
        , testCase "Rec1_1"   $ rnfX (undef :: Rec1)                  @?= ()
        , testCase "Rec1_2"   $ rnfX (Rec1 undef)                     @?= ()
        , testCase "Rec2_1"   $ rnfX (undef :: Rec2)                  @?= ()
        , testCase "Rec2_2"   $ rnfX (Rec2 3 undef)                   @?= ()
        , testCase "Rec2_3"   $ rnfX (Rec2 undef 5)                   @?= ()
        , testCase "Void"     $ rnfX (undef :: Void)                  @?= ()
        ]
    , testGroup
        "Tuples"
        [ -- Test Template Haskell generated hasUndefined instance for tuples
          testCase "HU1"  $ hasUndefined (undef :: (Int, Int))                  @?= True
        , testCase "HU3"  $ hasUndefined ((undef, undef) :: (Int, Int))         @?= True
        , testCase "HU2"  $ hasUndefined ((undef, 1) :: (Int, Int))             @?= True
        , testCase "HU4"  $ hasUndefined ((1, undef) :: (Int, Int))             @?= True
        , testCase "HU4"  $ hasUndefined ((1, 2) :: (Int, Int))                 @?= False
        , testCase "HU5"  $ hasUndefined ((undef, 1) :: (Rec2, Int))            @?= True
        , testCase "HU6"  $ hasUndefined ((Rec2 undef undef, 1) :: (Rec2, Int)) @?= True
        , testCase "HU7"  $ hasUndefined ((Rec2 1 undef, 1) :: (Rec2, Int))     @?= True
        , testCase "HU8"  $ hasUndefined ((Rec2 1 1, 1) :: (Rec2, Int))         @?= False

          -- Test Template Haskell generated rnfX instance for tuples
        , testCase "RnfX1"  $ rnfX (undef :: (Int, Int))                  @?= ()
        , testCase "RnfX3"  $ rnfX ((undef, undef) :: (Int, Int))         @?= ()
        , testCase "RnfX2"  $ rnfX ((undef, 1) :: (Int, Int))             @?= ()
        , testCase "RnfX4"  $ rnfX ((1, undef) :: (Int, Int))             @?= ()
        , testCase "RnfX4"  $ rnfX ((1, 2) :: (Int, Int))                 @?= ()
        , testCase "RnfX5"  $ rnfX ((undef, 1) :: (Rec2, Int))            @?= ()
        , testCase "RnfX6"  $ rnfX ((Rec2 undef undef, 1) :: (Rec2, Int)) @?= ()
        , testCase "RnfX7"  $ rnfX ((Rec2 1 undef, 1) :: (Rec2, Int))     @?= ()
        , testCase "RnfX8"  $ rnfX ((Rec2 1 1, 1) :: (Rec2, Int))         @?= ()

          -- Test Template Haskell generated deepErrorX/ensureSpine instance for tuples
        , testCase "DU" $ case dundef @(Unit, Unit) of (Unit, Unit) -> () @?= ()
        , testCase "ES1" $ case ensureSpine undef of () -> () @?= ()
        , testCase "ES1" $ case ensureSpine undef of ((), ()) -> () @?= ()
        , testCase "ES2" $ case ensureSpine @(Unit, Unit) undef of (Unit, Unit) -> () @?= ()
        ]
    , testGroup
        "ManualRnf"
        [ testCase "List1"     $ rnfX (undef :: [Int])                @?= ()
        , testCase "List2"     $ rnfX ([undef] :: [Int])              @?= ()
        , testCase "Maybe1"    $ rnfX (undef :: Maybe Int)            @?= ()
        , testCase "Maybe2"    $ rnfX (Just undef :: Maybe Int)       @?= ()
        , testCase "Either1"   $ rnfX (undef :: Either Int Int)       @?= ()
        , testCase "Either2"   $ rnfX (Left undef :: Either Int Int)  @?= ()
        , testCase "Either3"   $ rnfX (Right undef :: Either Int Int) @?= ()
        , testCase "Down1"     $ rnfX (Down undef :: Down Int)        @?= ()
        , testCase "Down2"     $ rnfX (undef :: Down Int)             @?= ()
        ]
    , testGroup
        "GenericHasUndefinedTrue"
        [ testCase "Unit"     $ hasUndefined (undef :: Unit)                  @?= True
        , testCase "Wrapper1" $ hasUndefined (undef :: Wrapper)               @?= True
        , testCase "Wrapper2" $ hasUndefined (Wrapper undef)                  @?= True
        , testCase "Sum"      $ hasUndefined (undef :: Sum)                   @?= True
        , testCase "BigSum"   $ hasUndefined (undef :: BigSum)                @?= True
        , testCase "Product1" $ hasUndefined (undef :: Product)               @?= True
        , testCase "Product2" $ hasUndefined (Product undef undef :: Product) @?= True
        , testCase "Product3" $ hasUndefined (Product 3 undef :: Product)     @?= True
        , testCase "Product4" $ hasUndefined (Product undef 5 :: Product)     @?= True
        , testCase "SP1"      $ hasUndefined (undef :: SP)                    @?= True
        , testCase "SP2"      $ hasUndefined (S undef undef :: SP)            @?= True
        , testCase "SP3"      $ hasUndefined (S 3 undef :: SP)                @?= True
        , testCase "SP3"      $ hasUndefined (S undef 5 :: SP)                @?= True
        , testCase "SP4"      $ hasUndefined (P undef :: SP)                  @?= True
        , testCase "Rec0"     $ hasUndefined (undef :: Rec0)                  @?= True
        , testCase "Rec1_1"   $ hasUndefined (undef :: Rec1)                  @?= True
        , testCase "Rec1_2"   $ hasUndefined (Rec1 undef)                     @?= True
        , testCase "Rec2_1"   $ hasUndefined (undef :: Rec2)                  @?= True
        , testCase "Rec2_2"   $ hasUndefined (Rec2 3 undef)                   @?= True
        , testCase "Rec2_3"   $ hasUndefined (Rec2 undef 5)                   @?= True
        , testCase "Void"     $ hasUndefined (undef :: Void)                  @?= True
        ]
    , testGroup
        "GenericHasUndefinedFalse"
        [ testCase "Unit"     $ hasUndefined ()                               @?= False
        , testCase "Wrapper"  $ hasUndefined (Wrapper 0:: Wrapper)            @?= False
        , testCase "SumA"     $ hasUndefined (SumTypeA :: Sum)                @?= False
        , testCase "SumB"     $ hasUndefined (SumTypeB :: Sum)                @?= False
        , testCase "BigSum1"  $ hasUndefined (BS1 :: BigSum)                  @?= False
        , testCase "BigSum2"  $ hasUndefined (BS2 :: BigSum)                  @?= False
        , testCase "BigSum3"  $ hasUndefined (BS3 :: BigSum)                  @?= False
        , testCase "BigSum4"  $ hasUndefined (BS4 :: BigSum)                  @?= False
        , testCase "BigSum5"  $ hasUndefined (BS5 :: BigSum)                  @?= False
        , testCase "Product"  $ hasUndefined (Product 3 5 :: Product)         @?= False
        , testCase "SP1"      $ hasUndefined (S 3 5 :: SP)                    @?= False
        , testCase "SP2"      $ hasUndefined (P 5 :: SP)                      @?= False
        , testCase "Rec2_3"   $ hasUndefined (Rec2 3 5)                       @?= False
        ]
    , testGroup
        "ManualHasUndefined"
        [ testCase "Vec1"       $ hasUndefined (3 :> errorX "X" :: Vec 5 Int)   @?= True
        , testCase "Vec2"       $ hasUndefined (errorX "X" :: Vec 5 Int)        @?= True
        , testCase "Maybe"      $ hasUndefined (Nothing :: Maybe Bool)          @?= False
        , testCase "BitVector1" $ hasUndefined (pack (Nothing :: Maybe Bool))   @?= True
        , testCase "BitVector2" $ hasUndefined (pack (Just True:: Maybe Bool))  @?= False
        ]
    , testGroup
        "GenericDeepErrorX"
        [ testCase "Unit"       $ case dundef @Unit of Unit -> ()           @?= ()
        , testCase "Wrapper1"   $ case dundef @Wrapper of Wrapper _ -> ()   @?= ()
        , testCase "Product1"   $ case dundef @Product of Product _ _ -> () @?= ()
        , testCase "Rec1_1"     $ case dundef @Rec1 of Rec1 {} -> ()        @?= ()
        , testCase "Rec2_1"     $ case dundef @Rec2 of Rec2 {} -> ()        @?= ()
        , testCase "ProductRec" $ case dundef @ProductRec of ProductRec (Rec1 _) (Unit, _) -> () @?= ()
        ]
    , testGroup
        "GenericEnsureSpine"
        [ testCase "Unit"       $ case sundef @Unit of Unit -> ()           @?= ()
        , testCase "Wrapper1"   $ case sundef @Wrapper of Wrapper _ -> ()   @?= ()
        , testCase "Product1"   $ case sundef @Product of Product _ _ -> () @?= ()
        , testCase "Rec1_1"     $ case sundef @Rec1 of Rec1 {} -> ()        @?= ()
        , testCase "Rec2_1"     $ case sundef @Rec2 of Rec2 {} -> ()        @?= ()
        , testCase "ProductRec" $ case sundef @ProductRec of ProductRec (Rec1 _) (Unit, _) -> () @?= ()
        ]
    ]

