module CLaSH.Netlist.BlackBox.Parser
  (runParse)
where

import Data.Text.Lazy (Text,pack)
import Data.ListLike.Text.TextLazy ()
import Text.ParserCombinators.UU
import qualified Text.ParserCombinators.UU.Core as PCC (parse)
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
import Text.ParserCombinators.UU.Utils hiding (pBrackets)

import CLaSH.Netlist.BlackBox.Types

type Parser a = P (Str Char Text LineColPos) a

runParse :: Text -> (Line, [Error LineColPos])
runParse = PCC.parse ((,) <$> pLine <*> pEnd)
         . createStr (LineColPos 0 0 0)

pLine :: Parser Line
pLine = pSome pElement

pElement :: Parser Element
pElement  =  pTag
         <|> C <$> pText

pText :: Parser Text
pText = pack <$> pList1 (pRange ('\000','\125'))

pTag :: Parser Element
pTag =  D <$> pDecl
    <|> pTag'

pDecl :: Parser Decl
pDecl = Decl <$> (pKeyWS "~INST" *> pNatural) <*>
        ((:) <$> pOutput <*> pList pInput) <* pKey "~INST"

pOutput :: Parser Line
pOutput = pKeyWS "~OUTPUT" *> pKeyWS "<=" *> pLine' <* pKeyWS "~"

pInput :: Parser Line
pInput = pKeyWS "~INPUT" *> pKeyWS "<=" *> pLine' <* pKeyWS "~"

pTag' :: Parser Element
pTag' =  O             <$  pKey "~RESULT"
     <|> I             <$> ((pKey "~ARG") *> pBrackets pNatural)
     <|> I             <$> ((pKey "~LIT") *> pBrackets pNatural)
     <|> (Clk . Just)  <$> ((pKey "~CLK") *> pBrackets pNatural)
     <|> (Clk Nothing) <$  pKey "~CLKO"
     <|> (Rst . Just)  <$> ((pKey "~RST") *> pBrackets pNatural)
     <|> (Rst Nothing) <$  pKey "~RSTO"
     <|> (Sym 0)       <$ pKey "~SYM"

pBrackets :: Parser a -> Parser a
pBrackets p = pSym '[' *> p <* pSym ']'

pKey :: String -> Parser String
pKey keyw = pToken keyw

pKeyWS :: String -> Parser String
pKeyWS keyw = pToken keyw <* pSpaces

pLine' :: Parser Line
pLine' = pSome pElem'

pElem' :: Parser Element
pElem' = pTag'
      <|> C <$> pText
