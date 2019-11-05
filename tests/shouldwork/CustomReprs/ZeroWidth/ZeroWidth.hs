-- Tests what should happen when data types have fields marked as zero-width
-- with custom bit representations.
--
-- At the time of writing, 2019-nov-01, we don't handle the following case:
--
--    myFunc (Product bool nonRepresentable) =
--      Product (not bool) nonRepresentable
--
-- Clash will try to project 'nonRepresentable' from the Product type as it
-- want to construct a new Product type with it later on. It doesn't have to,
-- as the construction won't try to include the non-representable type anyway,
-- but Clash doesn't (can't easily?) know this in time.
--
-- In ZeroWidthFailGracefully*.hs we test whether Clash tells the user what's wrong.

module ZeroWidth where

import Clash.Prelude.Testbench
import Clash.Prelude
import GHC.Generics
import Clash.Annotations.BitRepresentation
import Data.Maybe

data SProduct
  = S Bool String
  | P Bool String Bool
{-# ANN module ( DataReprAnn
                   $(liftQ [t|SProduct|])
                   3
                   [ ConstrRepr 'S 0b100 0b100 [0b001, 0b000]
                   , ConstrRepr 'P 0b100 0b000 [0b010, 0b000, 0b001]
                   ] ) #-}

data Product = Product Bool String
{-# ANN module ( DataReprAnn
                   $(liftQ [t|Product|])
                   1
                   [ ConstrRepr 'Product 0b0 0b0 [0b1, 0b0] ] ) #-}

data Record = Record { myBool :: Bool, myString :: String }
{-# ANN module ( DataReprAnn
                   $(liftQ [t|Record|])
                   1
                   [ ConstrRepr 'Record 0b0 0b0 [0b1, 0b0] ] ) #-}

topEntity
  :: SProduct
  -> Product
  -> Record
  -> ( SProduct
     , SProduct
     , Product
     , Product
     , Record
     , Record
     , Record
     )
topEntity sp (Product pb ps) r@(Record rb rs) =
  ( sp1
  , sp2
  , Product pb ps
  , Product (not pb) "xx"
  , Record rb rs
  , Record (not rb) "xx"
  , Record (not (myBool r)) "xx"
  )
 where
  sp2 =
    case sp of
      S b s -> S b s
      P b1 s b2 -> P b1 s b2

  sp1 =
    case sp of
      S b _ -> S (not b) "xx"
      P b1 _ b2 -> P (not b1) "xx" (not b2)
