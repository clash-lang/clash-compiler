{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
module CLaSH.Netlist.VHDL where

import qualified Control.Applicative as A
import Control.Lens hiding (Indexed)
import Control.Monad.State
import qualified Data.HashMap.Lazy as HashMap
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text.Lazy (Text,unpack)
import Text.PrettyPrint.Leijen.Text.Monadic

import CLaSH.Netlist.Types
import CLaSH.Netlist.Util
import CLaSH.Util (makeCached,(<:>))

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
tyPackage cName tys = do prevDec <- fmap (HashMap.filter (not . (== cName) . fst)) $ use _3
                         let needsDec = nub $ concatMap needsTyDec tys
                             newDec   = filter (not . (`HashMap.member` prevDec)) needsDec
                             newDecNeeds = nub $ concatMap needsTyDec newDec
                             otherDec = nub $ map fst $ HashMap.elems $ HashMap.filterWithKey (\k _ -> k `elem` newDecNeeds) prevDec
                         case newDec of
                            [] -> empty
                            _  -> packageDec otherDec newDec <$$> linebreak <>
                                  packageBodyDec newDec
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
needsTyDec (Vector _ elTy)    = (needsTyDec elTy) ++ [Vector 0 elTy]
needsTyDec ty@(Product _ tys) = (concatMap needsTyDec tys) ++ [ty]
needsTyDec (SP _ tys)         = concatMap (concatMap needsTyDec . snd) tys
needsTyDec Bool               = [Bool]
needsTyDec Integer            = [Integer]
needsTyDec _                  = []

tyDec :: HWType -> VHDLM Doc
tyDec Bool = "function" <+> "toSLV" <+> parens ("b" <+> colon <+> "in" <+> "boolean") <+> "return" <+> "std_logic_vector" <> semi
tyDec Integer = "function" <+> "to_integer" <+> parens ("i" <+> colon <+> "in" <+> "integer") <+> "return" <+> "integer" <> semi

tyDec (Vector _ elTy) = fmap snd $ makeCached (Vector 0 elTy) _3 ((,) A.<$> (use _2) A.<*> vecDec)
  where
    vecDec = "type" <+> "array_of_" <> tyName elTy <+> "is array (natural range <>) of" <+> vhdlType elTy <> semi

tyDec ty@(Product _ tys) = prodDec
  where
    prodDec = "type" <+> tName <+> "is record" <$>
                indent 2 (vcat $ sequence $ zipWith (\x y -> x <+> colon <+> y <> semi) selNames selTys) <$>
              "end record" <> semi

    tName    = tyName ty
    selNames = map (\i -> tName <> "_sel" <> int i) [0..]
    selTys   = map vhdlType tys

tyDec _ = empty

funDec :: HWType -> VHDLM Doc
funDec Bool = fmap snd $ makeCached Bool _3 ((,) A.<$> (use _2) A.<*> toSLVDec)
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
funDec Integer = fmap snd $ makeCached Integer _3 ((,) A.<$> (use _2) A.<*> toIntegerDec)
  where
    toIntegerDec = "function" <+> "to_integer" <+> parens ("i" <+> colon <+> "in" <+> "integer") <+> "return" <+> "integer" <+> "is" <$>
                   "begin" <$>
                     indent 2 ("return" <+> "i" <> semi) <$>
                   "end" <> semi

funDec _ = empty

tyName :: HWType -> VHDLM Doc
tyName (Vector n elTy)   = "array_of_" <> int n <> "_" <> tyName elTy
tyName (Signed n)        = "signed_" <> int n
tyName t@(Product _ _)   = fmap snd $ makeCached t _3 ((,) A.<$> (use _2) A.<*> prodName)
  where
    prodName = do i <- use _1
                  _1 += 1
                  "product" <> int i

tyName _                 = empty

tyImports :: [HWType] -> VHDLM Doc
tyImports tys = do prevDec <- use _3
                   let needsDec = nub $ concatMap needsTyDec tys
                       usePacks = nub $ map fst $ HashMap.elems $ HashMap.filterWithKey (\k _ -> k `elem` needsDec) prevDec
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
vhdlType (Unsigned n) = "unsigned" <>
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
decl (NetDecl _ ty _) | isEmptyType ty = return Nothing
decl (NetDecl id_ ty Nothing) = fmap Just $
  "signal" <+> text id_ <+> colon <+> vhdlType ty

decl _ = return Nothing

insts :: [Declaration] -> VHDLM Doc
insts [] = empty
insts is = vcat . punctuate linebreak . fmap catMaybes $ mapM inst is

inst :: Declaration -> VHDLM (Maybe Doc)
inst (Assignment id_ e) = fmap Just $
  text id_ <+> larrow <+> expr e <> semi

inst (CondAssignment id_ es) = fmap Just $
  text id_ <+> larrow <+> align (vcat (conds es)) <> semi
    where
      conds :: [(Expr,Expr,Expr)]-> VHDLM [Doc]
      conds []            = return []
      conds [(_,_,e)]     = expr e <:> (return [])
      conds ((l,r,e):es') = (expr e <+> "when" <+> parens (expr l <+> "=" <+> expr r) <+> "else") <:> conds es'

inst (InstDecl nm lbl pms) = fmap Just $
    nest 2 $ text lbl <> "_comp_inst" <+> colon <+> "entity"
              <+> text nm <$$> pms' <> semi
  where
    pms' = nest 2 $ "port map" <$$>
            tupled (sequence [text i <+> "=>" <+> expr e | (i,e) <- pms])

inst (BlackBoxD bs) = fmap Just $ string bs

inst _ = return Nothing

expr :: Expr -> VHDLM Doc
expr (Literal sizeM lit)                           = exprLit sizeM lit
expr (Identifier id_ Nothing)                      = text id_
expr (Identifier id_ (Just (Indexed (ty@(SP _ args),dcI,fI)))) = fromSLV argTy selected
  where
    argTys   = snd $ args !! dcI
    argTy    = argTys !! fI
    argSize  = typeSize argTy
    other    = otherSize argTys (fI-1)
    start    = typeSize ty - 1 - (conSize ty) - other
    end      = start - argSize + 1
    selected = text id_ <> parens (int start <+> "downto" <+> int end)

expr (Identifier id_ (Just (Indexed (ty@(Product _ _),_,fI)))) = text id_ <> dot <> tyName ty <> "_sel" <> int fI
expr (Identifier id_ (Just (DC (ty@(SP _ _),_)))) = text id_ <> parens (int start <+> "downto" <+> int end)
  where
    start = typeSize ty - 1
    end   = typeSize ty - conSize ty

expr (Identifier id_ (Just _)) = text id_
expr (vectorChain -> Just es)                  = tupled (mapM expr es)
expr (DataCon (Vector 1 _) _ [e])              = parens ("others" <+> rarrow <+> expr e)
expr (DataCon (Vector _ _) _ [e1,e2])          = expr e1 <+> "&" <+> expr e2
expr (DataCon ty@(SP _ args) (Just (DC (_,i))) es) = assignExpr
  where
    argTys     = snd $ args !! i
    dcSize     = (conSize ty) + sum (map typeSize argTys)
    dcExpr     = expr (dcToExpr ty i)
    argExprs   = zipWith toSLV argTys $ map expr es
    extraArg   = case (typeSize ty - dcSize) of
                   0 -> []
                   n -> [exprLit (Just n) (NumLit 0)]
    assignExpr = hcat $ punctuate (" & ") $ sequence (dcExpr:argExprs ++ extraArg)

expr (DataCon ty@(Sum _ _) (Just (DC (_,i))) []) = expr (dcToExpr ty i)
expr (DataCon ty@(Product _ _) _ es)         = tupled $ sequence $ zipWith (\i e -> tName <> "_sel" <> int i <+> rarrow <+> expr e) [0..] es
  where
    tName = tyName ty

expr (BlackBoxE bs) = string bs

expr _                                       = empty

otherSize :: [HWType] -> Int -> Int
otherSize _ n | n < 0 = 0
otherSize []     _    = 0
otherSize (a:as) n    = typeSize a + (otherSize as (n-1))

vectorChain :: Expr -> Maybe [Expr]
vectorChain (DataCon (Vector _ _) Nothing _)        = Just []
vectorChain (DataCon (Vector 1 _) (Just _) [e])     = Just [e]
vectorChain (DataCon (Vector _ _) (Just _) [e1,e2]) = (Just e1) <:> vectorChain e2
vectorChain _                                       = Nothing

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
toSLV Bit        d   = parens (int 0 <+> rarrow <+> d)
toSLV Bool       d   = "toSLV" <> parens d
toSLV Integer    d   = toSLV (Signed 32) ("to_signed" <> (tupled $ sequence [d,int 32]))
toSLV (Signed _) d   = "std_logic_vector" <> parens d
toSLV (Unsigned _) d = "std_logic_vector" <> parens d
toSLV hty          _ = error $ "toSLV: " ++ show hty

fromSLV :: HWType -> VHDLM Doc -> VHDLM Doc
fromSLV Bit d          = d <> parens (int 0)
fromSLV Bool d         = "fromSLV" <> parens d
fromSLV Integer d      = "to_integer" <> parens (fromSLV (Signed 32) d)
fromSLV (Signed _) d   = "signed" <> parens d
fromSLV (Unsigned _) d = "unsigned" <> parens d
fromSLV (SP _ _) d     = d
fromSLV (Sum _ _) d    = d
fromSLV hty _          = error $ "fromSLV: " ++ show hty

dcToExpr :: HWType -> Int -> Expr
dcToExpr ty i = Literal (Just $ conSize ty) (NumLit i)

larrow :: VHDLM Doc
larrow = "<="

rarrow :: VHDLM Doc
rarrow = "=>"

punctuate' :: Monad m => m Doc -> m [Doc] -> m Doc
punctuate' s d = (vcat $ punctuate s d) <> s
