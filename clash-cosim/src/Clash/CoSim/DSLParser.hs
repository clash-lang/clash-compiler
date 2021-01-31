module Clash.CoSim.DSLParser
  ( toVerilog
  , vars
  , clks
  , parse
  ) where

import Prelude

import Data.List (nub)
import Text.Printf (printf)

import qualified Text.Parsec
import Text.Parsec ( (<|>)
                   , alphaNum
                   , char
                   , digit
                   , lower
                   , many
                   , many1
                   , noneOf
                   , Parsec
                   , ParseError
                   , spaces
                   , string
                   , try
                   )


data CoSimDSLToken = HDL String
                   -- ^ String representing /dumb/ HDL code
                   | VarName String
                   -- ^ Named argument
                   | VarNum Int
                   -- ^ Anonymous argument
                   | ClkName String
                   -- ^ Named clock argument
                   | ClkNum Int
                   -- ^ Anonymous clock argument
                      deriving (Show)


type CoSimDSL = [CoSimDSLToken]

--------------------------------------
---- Parse functions -----------------
--------------------------------------
-- | Parse any text up until a possible variable
hdlParser :: Parsec String st String
hdlParser = many1 (noneOf "$#")

-- | Parse Haskell id: [_a-z][A-Za-z0-9_]*
varNameParser :: Parsec String st String
varNameParser = (:) <$> (lower <|> char '_') <*> many (alphaNum <|> char '_')

-- | Parse anonymous argument id: [0-9]+
varNumParser :: Parsec String st Int
varNumParser = read <$> many1 digit

clkParser :: Parsec String st CoSimDSLToken
clkParser = try (string "#{") *> numOrName <*  (string "}")
    where
        numOrName = (ClkName <$> varNameParser)
                <|> (ClkNum <$> varNumParser)

-- | Parse construction "${<id>}" or "${<num>}"
varParser :: Parsec String st CoSimDSLToken
varParser = try (string "${") *> numOrName <*  (string "}")
    where
        numOrName = (VarName <$> varNameParser)
                <|> (VarNum <$> varNumParser)

-- | Parse a sequence of CoSimDSL tokens
sourceParser :: Parsec String st CoSimDSL
sourceParser = many (
        varParser
    <|> clkParser
    <|> try (HDL <$> string "$")
    <|> try (HDL <$> string "#")
    <|> HDL <$> hdlParser
    )

withHeaderParser :: Parsec String st (String, CoSimDSL)
withHeaderParser = (,) <$> header <*> sourceParser
    where
        header = spaces
              *> string "MODULE:"
              *> spaces
              *>

              varNameParser

              <* spaces
              <* string "---"
              <* many (char '-')
              <* spaces

-- | Try to parse header with module name and then CoSimDSL tokens. When header
-- can't be found, just parse CoSimDSL tokens.
dslParser :: Parsec String st (Maybe String, CoSimDSL)
dslParser =
  let toCoSimDSL (name, tokens) = (Just $ name, tokens) in
  try (toCoSimDSL <$> withHeaderParser) <|> ((,) Nothing) <$> sourceParser

-- | Parse a HDL template. Will return a ParseError on a fail, or a tuple of the
-- detected module name and a CoSimDSL token. Parsing will detect variable names
-- according to:
--
--     * ${[_a-z][A-Za-z0-9_]*} or ${[0-9]+}
--
-- That is, the following are valid constructs:
--
--     * ${0}
--     * ${20}
--     * ${_abc}
--     * ${vaR}
--     * ${test0}
--
-- But the following are not:
--
--     * ${prime'}
--     * ${Uppercase}
--
-- The parser will detect a module name if it is mentioned as the first thing
-- followed by at least three dashes on the following line. For example:
--
-- @
-- MODULE: my_module_name
-- ----------
-- <!-- HDL code starts here -->
-- @
parse
    :: String
    -> Either ParseError (Maybe String, CoSimDSL)
parse = Text.Parsec.parse dslParser ""


--------------------------------------
---- Parse result manipulation -------
--------------------------------------
varNames :: CoSimDSL -> [String]
varNames []                   = []
varNames (VarName s : tokens) = nub $ s : varNames tokens
varNames (_ : tokens)         = nub $ varNames tokens

clkNames
  :: CoSimDSL
  -> [String]
clkNames = nub . foldr go []
 where
  go (ClkName s) ts = s : ts
  go _           ts = ts

-- | Number of anonymous arguments used in this DSL. It will pick out the highest
-- arugment number, even if not all arguments are used. For example, if ${0} and
-- ${5} had been specified but not all number in between, this function would
-- return 5+1=6 anonymous arguments.
nVarNums :: CoSimDSL -> Int
nVarNums []                  = 0
nVarNums (VarNum n : tokens) = max (succ n) (nVarNums tokens)
nVarNums (_ : tokens)        = nVarNums tokens

nClkNums :: CoSimDSL -> Int
nClkNums = foldr go 0
 where
  go (ClkNum n) tokens = max (succ n) tokens
  go _          tokens = tokens

-- | Generate a list of named arguments and anonymous arguments mentioned in a
-- parsed HDL template.
vars
    :: CoSimDSL
    -> ( [String]
       -- Named arguments
       , [String]
       -- Anonymous arguments
       )
vars tokens =
    let varNames' = varNames tokens in
    (varNames', anonymousNames (nVarNums tokens) varNames')

clks
  :: CoSimDSL
  -> ( [String] -- Names clocks
     , [String] -- Anonymous clocks
     )
clks tokens = (clkNames', anonymousNames (nClkNums tokens) clkNames')
 where
  clkNames' = clkNames tokens

-- Generate a unique name for each anonymous argument. Names will be
-- generated according to this scheme: aa0, aa1, ... In case of conflicts
-- with non-anonymous variable names underscores will be added in this
-- manner: aa0, _aa0, __aa0, ..
anonymousNames
    :: Int
    -- ^ Number of anonymous argument names to generate
    -> [String]
    -- ^ Existing argument names
    -> [String]
    -- ^ Unique anonymous argument names
anonymousNames nVarNums' varNames' =
  let aaName n m = (replicate m '_') ++ "aa" ++ show n in
  map (head . dropWhile (`elem` varNames')) $
  [[aaName n m | m <- [0..]] | n <- [0..nVarNums'-1]]

tokensToString :: CoSimDSL -> String
tokensToString tokens = concatMap tokenToString tokens
    where
        (_varNames', anonymousNames') = vars tokens
        (_clkNames', anonymousClkNames') = clks tokens

        tokenToString :: CoSimDSLToken -> String
        tokenToString (HDL s)     = s
        tokenToString (VarName s) = s
        tokenToString (VarNum n)  = anonymousNames' !! n
        tokenToString (ClkName s) = s
        tokenToString (ClkNum n)  = anonymousClkNames' !! n

-- | Reassemble parsed HDL template into HDL module. Add a header including
-- the detected variables in the HDL template. The module header will always
-- mention the result port name last.
toVerilog
    :: CoSimDSL
    -- ^ DSL to convert to verilog module
    -> String
    -- ^ Module name
    -> String
    -- ^ Verilog module
toVerilog dsl moduleName =
    concat [ printf
                 "module %s (%sresult);"
                 moduleName (concatMap (++", ") $ uncurry (++) (clks dsl) ++ uncurry (++) (vars dsl))
           , "\n"
           , tokensToString dsl
           , "\n"
           , "endmodule"
           , "\n"
           ]
