{-# LANGUAGE OverloadedStrings #-}
module CLaSH.Netlist.VHDL where

import Data.Maybe (catMaybes)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Text.PrettyPrint.Leijen.Text

import CLaSH.Netlist.Id
import CLaSH.Netlist.Types
import CLaSH.Netlist.Util

genVHDL :: Component -> (String,Doc)
genVHDL c = (unpack $ componentName c, vhdl)
  where
    vhdl = tyImports [] <$$>
           entity c <$$>
           architecture c

tyImports :: [String] -> Doc
tyImports _ = vcat $ map (<> semi)
  [ text "library IEEE"
  , text "use IEEE.STD_LOGIC_1164.ALL"
  , text "use IEEE.NUMERIC_STD.ALL"
  , text "use work.all"
  ]


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
vhdlType (Signed n) = text "signed" <>
                      parens ( int (n-1) <+> text "downto 0")
vhdlType t@(SP _ _) = text "std_logic_vector" <>
                      parens ( int (typeSize t - 1) <+>
                               text "downto 0" )
vhdlType t@(Sum _ _) = text "std_logic_vector" <>
                        parens ( int (typeSize t -1) <+>
                                 text "downto 0")
vhdlType (Product p _) = text (mkBasicId p)
vhdlType t          = error $ "vhdlType: " ++ show t

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
insts is = vcat . catMaybes $ zipWith inst gensyms is
--    case catMaybes $ zipWith inst gensyms is of
--      []  -> empty
--      is' -> (vcat $ punctuate semi is') <> semi
  where
    gensyms = [text "proc" <> int i | i <- [0..]]

inst :: Doc -> Declaration -> Maybe Doc
inst _ (Assignment id_ (Just (DC i)) ty@(SP _ args) es) = Just $
    text id_ <+> larrow <+> assignExpr <> semi
  where
    argTys     = snd $ args !! i
    dcExpr     = expr (dcToExpr ty i)
    argExprs   = zipWith toSLV argTys $ map expr es
    assignExpr = hcat $ punctuate (text " & ") (dcExpr:argExprs)

inst _ (Assignment id_ (Just (DC i)) ty@(Sum _ _) []) = Just $
    text id_ <+> larrow <+> assignExpr <> semi
  where
    assignExpr = expr (dcToExpr ty i)

inst _ (Assignment id_ Nothing _ [e]) = Just $
  text id_ <+> larrow <+> expr e <> semi

inst _ (InstDecl nm lbl pms) = Just $
    nest 2 $ text lbl <> text "_comp_inst" <+> colon <+> text "entity"
              <+> text nm <$$> pms' <> semi
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
exprLit _         (BoolLit t) = if t then text "true" else text "false"
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
dcToExpr (Sum _ dcs) i = Literal (Just conSize) (NumLit i)
  where
    conSize = ceiling . logBase (2 :: Float) . fromIntegral $ length dcs

dcToExpr _ _ = error "dcExpr"

larrow :: Doc
larrow = text "<="
