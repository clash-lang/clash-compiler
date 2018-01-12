module Clash.CoSim.DSL where

import Prelude

import Data.Either
import Data.List (nub, sort, intercalate)
import Text.Printf (printf)

import Text.Parsec ( (<|>)
                   , alphaNum
                   , char
                   , lower
                   , many
                   , many1
                   , noneOf
                   , parse
                   , Parsec
                   , ParseError
                   , spaces
                   , string
                   , try
                   )

data CoSimDSLToken = HDL String
                   | Var String
                      deriving (Show)


type CoSimDSL = (Maybe String, [CoSimDSLToken])

type NamelessModule = (String, String)

--------------------------------------
---- Parse functions -----------------
--------------------------------------
-- | Parse any text up until a possible variable
hdlParser :: Parsec String st String
hdlParser = many1 (noneOf "$")

-- | Parse Haskell id: [_a-z][A-Za-z0-9_]*
idParser :: Parsec String st String
idParser = (:) <$> (lower <|> char '_') <*> many (alphaNum <|> char '_')

-- | Parse construction "${<id>}", where <id> is parsed by idParser
varParser :: Parsec String st String
varParser = try (string "${") *> idParser <*  (string "}")

-- | Parse a sequence of CoSimDSL tokens
sourceParser :: Parsec String st [CoSimDSLToken]
sourceParser = many (
        Var <$> varParser
    <|> try (HDL <$> string "$")
    <|> HDL <$> hdlParser
    )

withHeaderParser :: Parsec String st [CoSimDSLToken]
withHeaderParser = (:) <$> (Var <$> header) <*> sourceParser
    where
        header = spaces
              *> string "MODULE:"
              *> spaces
              *>

              idParser

              <* spaces
              <* string "---"
              <* many (char '-')
              <* spaces

-- | Try to parse header with module name and then CoSimDSL tokens. When header
-- can't be found, just parse CoSimDSL tokens.
dslParser :: Parsec String st CoSimDSL
dslParser =
  let toCoSimDSL tokens = (Just $ tokenToString $ head tokens, tail tokens) in
  try (toCoSimDSL <$> withHeaderParser) <|> ((,) Nothing) <$> sourceParser


parseDSL
    :: String
    -> Either ParseError CoSimDSL
parseDSL = parse dslParser ""


--------------------------------------
---- Parse result manipulation -------
--------------------------------------
isVar :: CoSimDSLToken -> Bool
isVar (Var _) = True
isVar _       = False

vars :: [CoSimDSLToken] -> [String]
vars = nub . (map tokenToString) . (filter isVar)

tokenToString :: CoSimDSLToken -> String
tokenToString (HDL x) = x
tokenToString (Var x) = x

toVerilog
    :: [CoSimDSLToken]
    -- ^ DSL to convert to verilog module
    -> String
    -- ^ Module name
    -> String
    -- ^ Verilog module
toVerilog dsl mod =
    concat [ printf "module %s (%sresult);" mod (concatMap (++", ") (vars dsl))
           , "\n"
           , concat $ map tokenToString dsl
           , "\n"
           , "endmodule"
           , "\n"
           ]
