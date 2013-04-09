{-# LANGUAGE OverloadedStrings #-}
module CLaSH.Netlist.VHDL where

import Control.Monad.State
import Data.HashMap.Lazy  (HashMap,(!))
import Data.Label.PureM as LabelM
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text.Lazy (Text,unpack)
import Text.PrettyPrint.Leijen.Text.Monadic

-- import CLaSH.Netlist.Id
import CLaSH.Netlist.Types
import CLaSH.Netlist.Util
import CLaSH.Util (makeCached,getAndModify,__1,__2)

type VHDLM a = State (Int,HashMap HWType Doc) a

genVHDL :: (Int,HashMap HWType Doc) -> Component -> (String,Doc)
genVHDL s c = (unpack $ cName, flip evalState s vhdl)
  where
    cName = componentName c
    vhdl = tyPackage cName ((snd $ output c) : map snd (inputs c)) <$$> linebreak <>
           tyImports cName <$$> linebreak <>
           entity c <$$> linebreak <>
           architecture c

tyPackage :: Text -> [HWType] -> VHDLM Doc
tyPackage cName tys = imports <$> linebreak <>
  "package" <+> text cName <> "_types" <+> "is" <$>
  indent 2 (vcat $ mapM tyDec $ nub $ concatMap needsTyDec tys) <$>
  "end package" <+> text cName <> "_types" <> semi
  where
    imports = punctuate' semi $ sequence
                [ "library IEEE"
                , "use IEEE.STD_LOGIC_1164.ALL"
                , "use IEEE.NUMERIC_STD.ALL"
                ]

needsTyDec :: HWType -> [HWType]
needsTyDec ty@(Vector _ elTy) = ty:(needsTyDec elTy)
needsTyDec ty@(Product _ tys) = ty:(concatMap needsTyDec tys)
needsTyDec (SP _ tys)         = concatMap (concatMap needsTyDec . snd) tys
needsTyDec _                  = []

tyDec :: HWType -> VHDLM Doc
tyDec (Vector _ elTy) = "type" <+> "array_of_" <> tyName elTy <+> "is array (natural range <>) of" <+> vhdlType elTy <> semi
tyDec ty@(Product _ tys) = "type" <+> tName <+> "is record" <$>
                              indent 2 (vcat $ sequence $ zipWith (\x y -> x <+> colon <+> y <> semi) selNames selTys) <$>
                           "end record" <> semi


  where tName    = tyName ty
        selNames = map (\i -> tName <> "_sel" <> int i) [0..]
        selTys   = map vhdlType tys
tyDec _          = empty

tyName :: HWType -> VHDLM Doc
tyName (Vector n elTy)   = "array_of_" <> int n <> "_" <> tyName elTy
tyName (Signed n)        = "signed_" <> int n
tyName t@(Product _ _)   = makeCached t __2 (do {k <- getAndModify __1 (+1); "product" <> int k })
tyName _                 = empty

tyImports :: Text -> VHDLM Doc
tyImports compName = punctuate' semi $ sequence
  [ "library IEEE"
  , "use IEEE.STD_LOGIC_1164.ALL"
  , "use IEEE.NUMERIC_STD.ALL"
  , "use work." <> text compName <> "_types.ALL"
  , "use work.all"
  ]

entity :: Component -> VHDLM Doc
entity c = do
    p <- ports
    "entity" <+> text (componentName c) <+> "is" <$>
      (case p of
         [] -> empty
         _  -> indent 2 ("port" <>
                         parens (align $ vcat $ punctuate semi ports) <>
                         semi)
      ) <$>
      "end entity" <+> text (componentName c) <> semi
  where
    ports = sequence
          $ [ text i <+> colon <+> "in" <+> vhdlType ty
            | (i,ty) <- inputs c ] ++
            [ text i <+> colon <+> "in" <+> vhdlType ty
            | (i,ty) <- hiddenPorts c ] ++
            [ text (fst $ output c) <+> colon <+> "out" <+> vhdlType (snd $ output c)
            ]

architecture :: Component -> VHDLM Doc
architecture c =
  nest 2
    ("architecture structural of" <+> text (componentName c) <+> "is" <$$>
     (decls $ declarations c)) <$$>
  nest 2
    ("begin" <$$>
     (insts $ declarations c)) <$$>
  "end architecture structural" <> semi

vhdlType :: HWType -> VHDLM Doc
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
vhdlType t@(Product _ _) = fmap (! t) $ LabelM.gets __2
vhdlType t          = error $ "vhdlType: " ++ show t

decls :: [Declaration] -> VHDLM Doc
decls [] = empty
decls ds = do
    d <- dsDoc
    case d of
      [] -> empty
      _  -> vcat (punctuate semi dsDoc) <> semi
  where
    dsDoc = fmap catMaybes $ mapM decl ds

decl :: Declaration -> VHDLM (Maybe Doc)
decl (NetDecl id_ ty Nothing) = fmap Just $
  "signal" <+> text id_ <+> colon <+> vhdlType ty

decl _ = return Nothing

insts :: [Declaration] -> VHDLM Doc
insts [] = empty
insts is = vcat . punctuate linebreak . fmap catMaybes $ mapM inst is

inst :: Declaration -> VHDLM (Maybe Doc)
inst (Assignment id_ (Just (DC i)) ty@(SP _ args) es) = fmap Just $
    text id_ <+> larrow <+> assignExpr <> semi
  where
    argTys     = snd $ args !! i
    dcExpr     = expr (dcToExpr ty i)
    argExprs   = zipWith toSLV argTys $ map expr es
    assignExpr = hcat $ punctuate (" & ") $ sequence (dcExpr:argExprs)

inst (Assignment id_ (Just (DC i)) ty@(Sum _ _) []) = fmap Just $
    text id_ <+> larrow <+> assignExpr <> semi
  where
    assignExpr = expr (dcToExpr ty i)

inst (Assignment id_ (Just (DC 0)) ty@(Product _ _) es) = fmap Just $
    vcat $ sequence $ zipWith (\i e -> text id_ <> dot <> tName <> "_sel" <> int i <+> larrow <+> expr e <> semi) [0..] es
  where
    tName = tyName ty

inst (Assignment id_ Nothing _ [e]) = fmap Just $
  text id_ <+> larrow <+> expr e <> semi

inst (InstDecl nm lbl pms) = fmap Just $
    nest 2 $ text lbl <> "_comp_inst" <+> colon <+> "entity"
              <+> text nm <$$> pms' <> semi
  where
    pms' = nest 2 $ "port map" <$$>
            tupled (sequence [text i <+> "=>" <+> expr e | (i,e) <- pms])

inst (BlackBox bs) = fmap Just $ string bs

inst _ = return Nothing

expr :: Expr -> VHDLM Doc
expr (Literal sizeM lit)    = exprLit sizeM lit
expr (Identifier n Nothing) = text n
expr _                      = empty

exprLit :: Maybe Size -> Literal -> VHDLM Doc
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

bits :: [Bit] -> VHDLM Doc
bits = dquotes . hcat . sequence . map bit_char

bit_char :: Bit -> VHDLM Doc
bit_char H = char '1'
bit_char L = char '0'
bit_char U = char 'U'
bit_char Z = char 'Z'

toSLV :: HWType -> VHDLM Doc -> VHDLM Doc
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

larrow :: VHDLM Doc
larrow = "<="

punctuate' :: Monad m => m Doc -> m [Doc] -> m Doc
punctuate' s d = (vcat $ punctuate s d) <> s

