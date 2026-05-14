{-|
  Copyright  :  (C) 2019, Myrtle Software Ltd
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Blackbox generation for literal data constructors. (System)Verilog only!
-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Primitives.GHC.Literal
 ( assign
 , signed
 , signedLiteral
 , unsigned
 , unsignedLiteral
 , literalTF
 )
 where

import           Data.List.NonEmpty           (NonEmpty)
import qualified Data.List.NonEmpty           as NE
import qualified Data.Text.Lazy               as LT
import           Data.Text
  (Text, stripPrefix, stripSuffix, unpack)
import           Data.Text.Extra              (showtl)
import           Text.Read                    (readMaybe)

import           Clash.Core.Term              (Term)
import           Clash.Core.Type              (Type)
import           Clash.Netlist.Types          (BlackBox)
import           Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, Element(Text), BlackBoxMeta)

unsigned :: Element -> [Element]
unsigned el = [Text "$unsigned(", el, Text ")"]

signed :: Element -> [Element]
signed el = [Text "$signed(", el, Text ")"]

assign :: Element -> [Element] -> [Element]
assign lhs rhs = Text "assign " : lhs : Text " = " : rhs ++ [Text ";"]

signedLiteral :: Int -> Integer -> Element
signedLiteral wordSize wordVal =
  Text (LT.concat [ if wordVal < 0 then "-" else ""
                  , showtl wordSize
                  , "'sd"
                  , showtl (abs wordVal)
                  ])

unsignedLiteral :: Int -> Integer -> Element
unsignedLiteral wordSize wordVal =
  Text (LT.concat [ if wordVal < 0 then "-" else ""
                  , showtl wordSize
                  , "'d"
                  , showtl (abs wordVal)
                  ])

-- | Parse integer in strings of the form "GHC.Word.WordX#" where
-- "GHC.Word.Word" is the prefix given. The first prefix that matches wins.
readSize :: NonEmpty Text -> Text -> Maybe Int
readSize prefixes nm0 = do
  let go [] = Nothing
      go (p:ps) = case stripPrefix p nm0 of
        Just nm1 -> Just nm1
        Nothing  -> go ps
  nm1 <- go (NE.toList prefixes)
  nm2 <- stripSuffix "#" nm1
  readMaybe (unpack nm2)

-- | Constructs "clean" literals.
literalTF
  :: NonEmpty Text
  -- ^ Base names of constructors (for example: @"GHC.Word.W" :| ["GHC.Internal.Word.W"]@).
  -- The 'GHC.Internal.*' variant is needed for GHC >= 9.14, which moved the
  -- constructors into 'ghc-internal'; 'base' re-exports them.
  -> (Bool -> [Either Term Type] -> Int -> (BlackBoxMeta, BlackBox))
  -- ^ Functions processing
  -> BlackBoxFunction
literalTF baseNames tf isDecl primName args _resTy = return $
  case readSize baseNames primName of
    Nothing ->
      Left (concat ["Can only make blackboxes for '", unpack (NE.head baseNames), "X#'"])
    Just n ->
      Right (tf isDecl args n)
