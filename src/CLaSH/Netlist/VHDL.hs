{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module CLaSH.Netlist.VHDL where

import Control.Lens
import Control.Monad.State
import qualified Data.HashMap.Lazy as HashMap
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text.Lazy (Text,unpack)
import Text.PrettyPrint.Leijen.Text.Monadic

import CLaSH.Netlist.Types
import CLaSH.Netlist.Util

type VHDLM a = State VHDLState a

genVHDL :: Component -> VHDLM (String,Doc)
genVHDL c = do
    _2 .= cName
    fmap (unpack $ cName,) vhdl
  where
    cName = componentName c
    vhdl = tyPackage cName tys <$$> linebreak <>
           tyImports tys <$$> linebreak <>
           entity c <$$> linebreak <>
           architecture c
    tys = (snd $ output c)
        : map snd (inputs c)
        ++ concatMap (\d -> case d of {(NetDecl _ ty _) -> [ty]; _ -> []}) (declarations c)

tyPackage :: Text -> [HWType] -> VHDLM Doc
tyPackage cName tys = do prevDec <- use _3
                         cN      <- use _2
                         let needsDec = nub $ concatMap needsTyDec tys
                             newDec   = filter (not . (`HashMap.member` prevDec)) needsDec
                             useDec   = HashMap.keys $ HashMap.filter ((== cN) . fst) prevDec
                             otherDec = map fst $ HashMap.elems $ HashMap.filterWithKey (\k _ -> k `elem` needsDec) prevDec
                         case (newDec ++ useDec) of
                            []   -> empty
                            cDec -> packageDec otherDec cDec <$$> linebreak <>
                                    packageBodyDec cDec
  where
    packageDec ptys ntys =
      imports ptys <$> linebreak <>
      "package" <+> text cName <> "_types" <+> "is" <$>
        indent 2 (vcat $ mapM tyDec ntys) <$>
      "end" <> semi

    packageBodyDec ntys =
      "package" <+> "body" <+> text cName <> "_types" <+> "is" <$>
        indent 2 (vcat $ mapM funDec ntys) <$>
      "end" <> semi

    imports ptys = punctuate' semi $ sequence $
                [ "library IEEE"
                , "use IEEE.STD_LOGIC_1164.ALL"
                , "use IEEE.NUMERIC_STD.ALL"
                ] ++ map (\x -> "use work." <> text x <> "_types.all") ptys

needsTyDec :: HWType -> [HWType]
needsTyDec ty@(Vector _ elTy) = ty:(needsTyDec elTy)
needsTyDec ty@(Product _ tys) = ty:(concatMap needsTyDec tys)
needsTyDec (SP _ tys)         = concatMap (concatMap needsTyDec . snd) tys
needsTyDec Bool               = [Bool]
needsTyDec _                  = []

tyDec :: HWType -> VHDLM Doc
tyDec Bool = do prevDec <- use _3
                case (HashMap.lookup Bool prevDec) of
                   Nothing -> toSLVDec
                   Just _  -> empty
  where
    toSLVDec = "function" <+> "toSLV" <+> parens ("b" <+> colon <+> "in" <+> "boolean") <+> "return" <+> "std_logic_vector" <> semi

tyDec (Vector _ elTy) = "type" <+> "array_of_" <> tyName elTy <+> "is array (natural range <>) of" <+> vhdlType elTy <> semi
tyDec ty@(Product _ tys) = "type" <+> tName <+> "is record" <$>
                              indent 2 (vcat $ sequence $ zipWith (\x y -> x <+> colon <+> y <> semi) selNames selTys) <$>
                           "end record" <> semi


  where tName    = tyName ty
        selNames = map (\i -> tName <> "_sel" <> int i) [0..]
        selTys   = map vhdlType tys
tyDec _          = empty

funDec :: HWType -> VHDLM Doc
funDec Bool = do prevDec <- use _3
                 cN      <- use _2
                 case (HashMap.lookup Bool prevDec) of
                    Nothing -> do _3 %= HashMap.insert Bool (cN,"boolean")
                                  toSLVDec
                    Just _  -> empty
  where
    toSLVDec = "function" <+> "toSLV" <+> parens ("b" <+> colon <+> "in" <+> "boolean") <+> "return" <+> "std_logic_vector" <+> "is" <$>
               "begin" <$>
                 indent 2 (vcat $ sequence ["if" <+> "b" <+> "then"
                                           ,  indent 2 ("return" <+> dquotes (int 1) <> semi)
                                           ,"else"
                                           ,  indent 2 ("return" <+> dquotes (int 0) <> semi)
                                           ,"end" <+> "if" <> semi
                                           ]) <$>
               "end" <> semi
funDec _ = empty

tyName :: HWType -> VHDLM Doc
tyName (Vector n elTy)   = "array_of_" <> int n <> "_" <> tyName elTy
tyName (Signed n)        = "signed_" <> int n
tyName t@(Product _ _)   = do tMap <- use _3
                              case HashMap.lookup t tMap of
                                Nothing -> do i <- use _1
                                              c <- use _2
                                              _1 += 1
                                              newName <- "product" <> int i
                                              _3 %= HashMap.insert t (c,newName)
                                              return newName
                                Just (_,n) -> return n


tyName _                 = empty

tyImports :: [HWType] -> VHDLM Doc
tyImports tys = do prevDec <- use _3
                   let needsDec = nub $ concatMap needsTyDec tys
                       usePacks = map fst $ HashMap.elems $ HashMap.filterWithKey (\k _ -> k `elem` needsDec) prevDec
                   punctuate' semi $ sequence $ [ "library IEEE"
                                                , "use IEEE.STD_LOGIC_1164.ALL"
                                                , "use IEEE.NUMERIC_STD.ALL"
                                                , "use work.all"
                                                ] ++ map (\x -> "use work." <> text x <> "_types.all") usePacks

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
      "end" <> semi
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
    "end" <> semi

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
vhdlType t@(Product _ _) = tyName t
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
toSLV Bit        d = d
toSLV Bool       d = "toSLV" <> parens d
toSLV Integer    d = toSLV (Signed 32) ("to_signed" <> (tupled $ sequence [d,int 32]))
toSLV (Signed _) d = "std_logic_vector" <> parens d
toSLV _          _ = error "toSLV"

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
