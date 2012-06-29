{-# LANGUAGE OverloadedStrings #-}
module CLaSH.Netlist.VHDL where

import Data.Char (isDigit,ord)
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Numeric (showHex)
import Text.PrettyPrint.Leijen.Text

import CLaSH.Netlist.Types
import CLaSH.Netlist.Util

genVHDL :: Component -> (String,Doc)
genVHDL c = (unpack $ componentName c, vhdl)
  where
    vhdl = entity c <$$>
           architecture c

entity :: Component -> Doc
entity c =
    text "entity" <+> text (componentName c) <+> text "is" <$>
      (case ports of
         [] -> empty
         _  -> indent 2 (text "port" <>
                         parens (align $ vcat $ punctuate semi ports) <>
                         semi)
      ) <$>
      text "end entity" <+> text (componentName c) <> semi
  where
    ports = [ text i <+> colon <+> text "in" <+> vhdlType ty
            | (i,ty) <- inputs c ] ++
            [ text (fst $ output c) <+> colon <+> text "out" <+> vhdlType (snd $ output c)
            ]

architecture :: Component -> Doc
architecture c =
  nest 2
    (text "architecture structural of" <+> text (componentName c) <+> text "is" <$$>
     (decls $ declarations c)) <$$>
  nest 2
    (text "begin" <$$>
     (insts $ declarations c)) <$$>
  text "end architecture structural" <> semi

vhdlType :: HWType -> Doc
vhdlType Bool       = text "boolean"
vhdlType Integer    = text "integer"
vhdlType t@(SP _ _) = text "std_logic_vector" <>
                      parens ( int (typeSize t - 1) <+>
                               text "downto 0" )
vhdlType _          = error "vhdlType"

decls :: [Declaration] -> Doc
decls [] = empty
decls ds =
    case dsDoc of
      [] -> empty
      _  -> vcat (punctuate semi dsDoc) <> semi
  where
    dsDoc = catMaybes $ map decl ds

decl :: Declaration -> Maybe Doc
decl (NetDecl id_ ty Nothing) = Just $
  text "signal" <+> text id_ <+> colon <+> vhdlType ty

decl _ = Nothing

insts :: [Declaration] -> Doc
insts [] = empty
insts is =
    case catMaybes $ zipWith inst gensyms is of
      []  -> empty
      is' -> (vcat $ punctuate semi is') <> semi
  where
    gensyms = [text "proc" <> int i | i <- [0..]]

inst :: Doc -> Declaration -> Maybe Doc
inst _ (Assignment id_ (Just (DC i)) ty@(SP _ args) es) = Just $
    text id_ <+> text "<=" <+> assignExpr
  where
    argTys     = snd $ args !! i
    dcExpr     = expr (dcToExpr ty i)
    argExprs   = zipWith toSLV argTys $ map expr es
    assignExpr = hcat $ punctuate (text " & ") (dcExpr:argExprs)

inst _ (Assignment id_ Nothing Integer [e]) = Just $
  text id_ <+> text "<=" <+> expr e

inst _ (InstDecl nm lbl pms) = Just $
    nest 2 $ text lbl <> text "comp_inst" <+> colon <+> text "entity"
              <+> text nm <$$> pms'
  where
    pms' = nest 2 $ text "port map" <$$>
            tupled [text i <+> text "=>" <+> expr e | (i,e) <- pms]

inst _ (BlackBox bs) = Just $ string $ decodeUtf8 bs

inst _ _ = Nothing

expr :: Expr -> Doc
expr (Literal sizeM lit)    = exprLit sizeM lit
expr (Identifier n Nothing) = text n
expr _                      = empty

exprLit :: Maybe Size -> Literal -> Doc
exprLit Nothing   (NumLit i) = int i
exprLit (Just sz) (NumLit i) = bits $ (toBits sz i)
exprLit _         _          = error "exprLit"

toBits :: Integral a => Int -> a -> [Bit]
toBits size val = map (\x -> if odd x then H else L)
                $ reverse
                $ take size
                $ map (`mod` 2)
                $ iterate (`div` 2) val

bits :: [Bit] -> Doc
bits = dquotes . hcat . map bit_char

bit_char :: Bit -> Doc
bit_char H = char '1'
bit_char L = char '0'
bit_char U = char 'U'
bit_char Z = char 'Z'

toSLV :: HWType -> Doc -> Doc
toSLV Bit  d = d
toSLV Bool d = text "toSLV" <> parens d
toSLV _    _ = error "toSLV"

dcToExpr :: HWType -> Int -> Expr
dcToExpr (SP _ args) i = Literal (Just conSize) (NumLit i)
  where
    conSize = ceiling . logBase (2 :: Float) . fromIntegral $ length args

dcToExpr _ _ = error "dcExpr"

mkVHDLBasicId ::
  String
  -> String
mkVHDLBasicId = stripMultiscore . stripLeading . zEncode
  where
    stripLeading = dropWhile (`elem` ['0'..'9'])
    stripMultiscore = concatMap (\cs -> case cs of ('_':_) -> "_"; _ -> cs)
                    . List.group

type UserString    = String -- As the user typed it
type EncodedString = String -- Encoded form

zEncode :: UserString -> EncodedString
zEncode cs = go cs
  where
    go []       = []
    go (c:cs')  = encode_digit_ch c ++ go' cs'
    go' []      = []
    go' (c:cs') = encode_ch c ++ go' cs'

encode_digit_ch :: Char -> EncodedString
encode_digit_ch c | c >= '0' && c <= '9' = encode_as_unicode_char c
encode_digit_ch c | otherwise            = encode_ch c

encode_ch :: Char -> EncodedString
encode_ch c | unencodedChar c = [c]     -- Common case first

-- Constructors
encode_ch '['  = "ZM"
encode_ch ']'  = "ZN"
encode_ch ':'  = "ZC"

-- Variables
encode_ch '&'  = "za"
encode_ch '|'  = "zb"
encode_ch '^'  = "zc"
encode_ch '$'  = "zd"
encode_ch '='  = "ze"
encode_ch '>'  = "zg"
encode_ch '#'  = "zh"
encode_ch '.'  = "zi"
encode_ch '<'  = "zl"
encode_ch '-'  = "zm"
encode_ch '!'  = "zn"
encode_ch '+'  = "zp"
encode_ch '\'' = "zq"
encode_ch '\\' = "zr"
encode_ch '/'  = "zs"
encode_ch '*'  = "zt"
encode_ch '%'  = "zv"
encode_ch c    = encode_as_unicode_char c

encode_as_unicode_char :: Char -> EncodedString
encode_as_unicode_char c = 'z' : if isDigit (head hex_str) then hex_str
                                                           else '0':hex_str
  where hex_str = showHex (ord c) "U"

unencodedChar :: Char -> Bool   -- True for chars that don't need encoding
unencodedChar c   =  c >= 'a' && c <= 'z'
                  || c >= 'A' && c <= 'Z'
                  || c >= '0' && c <= '9'
                  || c == '_' || c == '('
                  || c == ')'
