{-# LANGUAGE OverloadedStrings #-}
module CLaSH.Netlist.VHDL where

import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text.Lazy (Text,unpack)
import Text.PrettyPrint.Leijen.Text

import CLaSH.Netlist.Id
import CLaSH.Netlist.Types
import CLaSH.Netlist.Util

genVHDL :: Component -> (String,Doc)
genVHDL c = (unpack $ cName, vhdl)
  where
    cName = componentName c
    vhdl = tyPackage cName ((snd $ output c) : map snd (inputs c)) <$$> linebreak <>
           tyImports cName <$$> linebreak <>
           entity c <$$> linebreak <>
           architecture c

tyPackage :: Text -> [HWType] -> Doc
tyPackage cName tys = imports <$> linebreak <>
  "package" <+> text cName <> "_types" <+> "is" <$>
  indent 2 (vcat $ map tyDec $ nub $ concatMap needsTyDec tys) <$>
  "end package" <+> text cName <> "_types" <> semi
  where
    imports = vcat $ map (<> semi)
                [ "library IEEE"
                , "use IEEE.STD_LOGIC_1164.ALL"
                , "use IEEE.NUMERIC_STD.ALL"
                ]

needsTyDec :: HWType -> [HWType]
needsTyDec ty@(Vector _ elTy) = needsTyDec elTy ++ [ty]
needsTyDec _                  = []

tyDec :: HWType -> Doc
tyDec (Vector _ elTy) = "type" <+> "array_of_" <> tyName elTy <+> "is array (natural range <>) of" <+> vhdlType elTy <> semi
tyDec _               = empty

tyName :: HWType -> Doc
tyName (Vector n elTy) = "array_of_" <> int n <> "_" <> tyName elTy
tyName (Signed n)      = "signed_" <> int n
tyName _               = empty

tyImports :: Text -> Doc
tyImports compName = vcat $ map (<> semi)
  [ "library IEEE"
  , "use IEEE.STD_LOGIC_1164.ALL"
  , "use IEEE.NUMERIC_STD.ALL"
  , "use work." <> text compName <> "_types.ALL"
  , "use work.all"
  ]


entity :: Component -> Doc
entity c =
    "entity" <+> text (componentName c) <+> "is" <$>
      (case ports of
         [] -> empty
         _  -> indent 2 ("port" <>
                         parens (align $ vcat $ punctuate semi ports) <>
                         semi)
      ) <$>
      "end entity" <+> text (componentName c) <> semi
  where
    ports = [ text i <+> colon <+> "in" <+> vhdlType ty
            | (i,ty) <- inputs c ] ++
            [ text i <+> colon <+> "in" <+> vhdlType ty
            | (i,ty) <- hiddenPorts c ] ++
            [ text (fst $ output c) <+> colon <+> "out" <+> vhdlType (snd $ output c)
            ]

architecture :: Component -> Doc
architecture c =
  nest 2
    ("architecture structural of" <+> text (componentName c) <+> "is" <$$>
     (decls $ declarations c)) <$$>
  nest 2
    ("begin" <$$>
     (insts $ declarations c)) <$$>
  "end architecture structural" <> semi

vhdlType :: HWType -> Doc
vhdlType Bit        = "std_logic"
vhdlType Bool       = "boolean"
vhdlType Integer    = "integer"
vhdlType (Signed n) = "signed" <>
                      parens ( int (n-1) <+> "downto 0")
vhdlType (Vector n elTy) = "array_of_" <> tyName elTy <> parens ( int (n-1) <+> "downto 0")
vhdlType t@(SP _ _) = "std_logic_vector" <>
                      parens ( int (typeSize t - 1) <+>
                               "downto 0" )
vhdlType t@(Sum _ _) = "std_logic_vector" <>
                        parens ( int (typeSize t -1) <+>
                                 "downto 0")
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
  "signal" <+> text id_ <+> colon <+> vhdlType ty

decl _ = Nothing

insts :: [Declaration] -> Doc
insts [] = empty
insts is = vcat . map (<> linebreak) . catMaybes $ map inst is

inst :: Declaration -> Maybe Doc
inst (Assignment id_ (Just (DC i)) ty@(SP _ args) es) = Just $
    text id_ <+> larrow <+> assignExpr <> semi
  where
    argTys     = snd $ args !! i
    dcExpr     = expr (dcToExpr ty i)
    argExprs   = zipWith toSLV argTys $ map expr es
    assignExpr = hcat $ punctuate (" & ") (dcExpr:argExprs)

inst (Assignment id_ (Just (DC i)) ty@(Sum _ _) []) = Just $
    text id_ <+> larrow <+> assignExpr <> semi
  where
    assignExpr = expr (dcToExpr ty i)

inst (Assignment id_ Nothing _ [e]) = Just $
  text id_ <+> larrow <+> expr e <> semi

inst (InstDecl nm lbl pms) = Just $
    nest 2 $ text lbl <> "_comp_inst" <+> colon <+> "entity"
              <+> text nm <$$> pms' <> semi
  where
    pms' = nest 2 $ "port map" <$$>
            tupled [text i <+> "=>" <+> expr e | (i,e) <- pms]

inst (BlackBox bs) = Just $ string bs

inst _ = Nothing

expr :: Expr -> Doc
expr (Literal sizeM lit)    = exprLit sizeM lit
expr (Identifier n Nothing) = text n
expr _                      = empty

exprLit :: Maybe Size -> Literal -> Doc
exprLit Nothing   (NumLit i) = int i
exprLit (Just sz) (NumLit i) = bits $ (toBits sz i)
exprLit _         (BoolLit t) = if t then "true" else "false"
exprLit _         (BitLit b) = squotes $ bit_char b
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
toSLV Bool d = "toSLV" <> parens d
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
larrow = "<="
