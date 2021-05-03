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
-- "GHC.Word.Word" is the prefix given.
readSize :: Text -> Text -> Maybe Int
readSize prefix nm0 = do
  nm1 <- stripPrefix prefix nm0
  nm2 <- stripSuffix "#" nm1
  readMaybe (unpack nm2)

-- | Constructs "clean" literals.
literalTF
  :: Text
  -- ^ Base name of constructors (for example: "GHC.Word.W")
  -> (Bool -> [Either Term Type] -> Int -> (BlackBoxMeta, BlackBox))
  -- ^ Functions processing
  -> BlackBoxFunction
literalTF baseName tf isDecl primName args _resTy = return $
  case readSize baseName primName of
    Nothing ->
      Left (concat ["Can only make blackboxes for '", unpack baseName, "X#'"])
    Just n ->
      Right (tf isDecl args n)
