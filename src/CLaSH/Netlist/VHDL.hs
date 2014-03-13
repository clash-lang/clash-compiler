{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Generate VHDL for assorted Netlist datatypes
module CLaSH.Netlist.VHDL
  ( genVHDL
  , mkTyPackage
  , vhdlType
  , vhdlTypeDefault
  , vhdlTypeMark
  , inst
  , expr
  )
where

import qualified Control.Applicative                  as A
import           Control.Lens                         hiding (Indexed)
import           Control.Monad                        (join,liftM,when,zipWithM)
import           Control.Monad.State                  (State)
import           Data.Graph.Inductive                 (Gr, mkGraph, topsort')
import qualified Data.HashMap.Lazy                    as HashMap
import qualified Data.HashSet                         as HashSet
import           Data.List                            (mapAccumL,nub)
import           Data.Maybe                           (catMaybes,mapMaybe)
import           Data.Text.Lazy                       (unpack)
import qualified Data.Text.Lazy                       as T
import           Text.PrettyPrint.Leijen.Text.Monadic

import           CLaSH.Netlist.Types
import           CLaSH.Netlist.Util
import           CLaSH.Util                           (curLoc, makeCached, (<:>))

type VHDLM a = State VHDLState a

-- | Generate VHDL for a Netlist component
genVHDL :: Component -> VHDLM (String,Doc)
genVHDL c = (unpack cName,) A.<$> vhdl
  where
    cName   = componentName c
    vhdl    = tyImports <$$> linebreak <>
              entity c <$$> linebreak <>
              architecture c

-- | Generate a VHDL package containing type definitions for the given HWTypes
mkTyPackage :: [HWType]
            -> VHDLM Doc
mkTyPackage hwtys =
   "library IEEE;" <$>
   "use IEEE.STD_LOGIC_1164.ALL;" <$>
   "use IEEE.NUMERIC_STD.ALL;" <$$> linebreak <>
   "package" <+> "types" <+> "is" <$>
      indent 2 ( packageDec <$>
                 vcat (sequence funDecs)
               ) <$>
   "end" <> semi <> packageBodyDec
  where
    hwTysSorted = topSortHWTys hwtys
    usedTys     = nub $ concatMap mkUsedTys hwtys
    packageDec  = vcat $ mapM tyDec hwTysSorted
    (funDecs,funBodys) = unzip . catMaybes $ map funDec usedTys

    packageBodyDec :: VHDLM Doc
    packageBodyDec = case funBodys of
        [] -> empty
        _  -> linebreak <$>
              "package" <+> "body" <+> "types" <+> "is" <$>
                indent 2 (vcat (sequence funBodys)) <$>
              "end" <> semi

mkUsedTys :: HWType
        -> [HWType]
mkUsedTys v@(Vector _ elTy)   = v : mkUsedTys elTy
mkUsedTys p@(Product _ elTys) = p : concatMap mkUsedTys elTys
mkUsedTys sp@(SP _ elTys)     = sp : concatMap mkUsedTys (concatMap snd elTys)
mkUsedTys t                   = [t]

topSortHWTys :: [HWType]
             -> [HWType]
topSortHWTys hwtys = sorted
  where
    nodes  = zip [0..] hwtys
    nodesI = HashMap.fromList (zip hwtys [0..])
    edges  = concatMap edge hwtys
    graph  = mkGraph nodes edges :: Gr HWType ()
    sorted = reverse $ topsort' graph

    edge t@(Vector _ elTy) = maybe [] ((:[]) . (HashMap.lookupDefault (error $ $(curLoc) ++ "Vector") t nodesI,,()))
                                      (HashMap.lookup (mkVecZ elTy) nodesI)
    edge t@(Product _ tys) = let ti = HashMap.lookupDefault (error $ $(curLoc) ++ "Product") t nodesI
                             in mapMaybe (\ty -> liftM (ti,,()) (HashMap.lookup (mkVecZ ty) nodesI)) tys
    edge t@(SP _ ctys)     = let ti = HashMap.lookupDefault (error $ $(curLoc) ++ "SP") t nodesI
                             in concatMap (\(_,tys) -> mapMaybe (\ty -> liftM (ti,,()) (HashMap.lookup (mkVecZ ty) nodesI)) tys) ctys
    edge _                 = []

mkVecZ :: HWType -> HWType
mkVecZ (Vector _ elTy) = Vector 0 elTy
mkVecZ t               = t

needsTyDec :: HWType -> Bool
needsTyDec (Vector _ Bit) = False
needsTyDec (Vector _ _)   = True
needsTyDec (Product _ _)  = True
needsTyDec (SP _ _)       = True
needsTyDec Bool           = True
needsTyDec Integer        = True
needsTyDec _              = False

tyDec :: HWType -> VHDLM Doc
tyDec (Vector _ elTy) = "type" <+> "array_of_" <> tyName elTy <+> "is array (natural range <>) of" <+> vhdlType elTy <> semi

tyDec ty@(Product _ tys) = prodDec
  where
    prodDec = "type" <+> tName <+> "is record" <$>
                indent 2 (vcat $ zipWithM (\x y -> x <+> colon <+> y <> semi) selNames selTys) <$>
              "end record" <> semi

    tName    = tyName ty
    selNames = map (\i -> tName <> "_sel" <> int i) [0..]
    selTys   = map vhdlType tys

tyDec _ = empty

funDec :: HWType -> Maybe (VHDLM Doc,VHDLM Doc)
funDec Bool = Just
  ( "function" <+> "toSLV" <+> parens ("b" <+> colon <+> "in" <+> "boolean") <+> "return" <+> "std_logic_vector" <> semi <$>
    "function" <+> "fromSL" <+> parens ("sl" <+> colon <+> "in" <+> "std_logic") <+> "return" <+> "boolean" <> semi
  , "function" <+> "toSLV" <+> parens ("b" <+> colon <+> "in" <+> "boolean") <+> "return" <+> "std_logic_vector" <+> "is" <$>
    "begin" <$>
      indent 2 (vcat $ sequence ["if" <+> "b" <+> "then"
                                ,  indent 2 ("return" <+> dquotes (int 1) <> semi)
                                ,"else"
                                ,  indent 2 ("return" <+> dquotes (int 0) <> semi)
                                ,"end" <+> "if" <> semi
                                ]) <$>
    "end" <> semi <$>
    "function" <+> "fromSL" <+> parens ("sl" <+> colon <+> "in" <+> "std_logic") <+> "return" <+> "boolean" <+> "is" <$>
    "begin" <$>
      indent 2 (vcat $ sequence ["if" <+> "sl" <+> "=" <+> squotes (int 1) <+> "then"
                                ,   indent 2 ("return" <+> "true" <> semi)
                                ,"else"
                                ,   indent 2 ("return" <+> "false" <> semi)
                                ,"end" <+> "if" <> semi
                                ]) <$>
    "end" <> semi
  )

funDec Integer = Just
  ( "function" <+> "to_integer" <+> parens ("i" <+> colon <+> "in" <+> "integer") <+> "return" <+> "integer" <> semi
  , "function" <+> "to_integer" <+> parens ("i" <+> colon <+> "in" <+> "integer") <+> "return" <+> "integer" <+> "is" <$>
    "begin" <$>
      indent 2 ("return" <+> "i" <> semi) <$>
    "end" <> semi
  )

funDec _ = Nothing

tyImports :: VHDLM Doc
tyImports =
  punctuate' semi $ sequence
    [ "library IEEE"
    , "use IEEE.STD_LOGIC_1164.ALL"
    , "use IEEE.NUMERIC_STD.ALL"
    , "use work.all"
    , "use work.types.all"
    ]


entity :: Component -> VHDLM Doc
entity c = do
    rec (p,ls) <- fmap unzip (ports (maximum ls))
    "entity" <+> text (componentName c) <+> "is" <$>
      (case p of
         [] -> empty
         _  -> indent 2 ("port" <>
                         parens (align $ vcat $ punctuate semi (A.pure p)) <>
                         semi)
      ) <$>
      "end" <> semi
  where
    ports l = sequence
            $ [ (,fromIntegral $ T.length i) A.<$> (fill l (text i) <+> colon <+> "in" <+> vhdlType ty <+> ":=" <+> vhdlTypeDefault ty)
              | (i,ty) <- inputs c ] ++
              [ (,fromIntegral $ T.length i) A.<$> (fill l (text i) <+> colon <+> "in" <+> vhdlType ty <+> ":=" <+> vhdlTypeDefault ty)
              | (i,ty) <- hiddenPorts c ] ++
              [ (,fromIntegral $ T.length (fst $ output c)) A.<$> (fill l (text (fst $ output c)) <+> colon <+> "out" <+> vhdlType (snd $ output c) <+> ":=" <+> vhdlTypeDefault (snd $ output c))
              ]

architecture :: Component -> VHDLM Doc
architecture c =
  nest 2
    ("architecture structural of" <+> text (componentName c) <+> "is" <$$>
     decls (declarations c)) <$$>
  nest 2
    ("begin" <$$>
     insts (declarations c)) <$$>
    "end" <> semi

-- | Convert a Netlist HWType to a VHDL type
vhdlType :: HWType -> VHDLM Doc
vhdlType hwty = do
  when (needsTyDec hwty) (_1 %= HashSet.insert (mkVecZ hwty))
  vhdlType' hwty

vhdlType' :: HWType -> VHDLM Doc
vhdlType' Bit        = "std_logic"
vhdlType' Bool       = "boolean"
vhdlType' (Clock _)  = "std_logic"
vhdlType' (Reset _)  = "std_logic"
vhdlType' Integer    = "integer"
vhdlType' (Signed n) = "signed" <>
                      parens ( int (n-1) <+> "downto 0")
vhdlType' (Unsigned n) = "unsigned" <>
                        parens ( int (n-1) <+> "downto 0")
vhdlType' (Vector n Bit) = "std_logic_vector" <> parens ( int (n-1) <+> "downto 0")
vhdlType' (Vector n elTy) = "array_of_" <> tyName elTy <> parens ( int (n-1) <+> "downto 0")
vhdlType' t@(SP _ _) = "std_logic_vector" <>
                      parens ( int (typeSize t - 1) <+>
                               "downto 0" )
vhdlType' t@(Sum _ _) = "unsigned" <>
                        parens ( int (typeSize t -1) <+>
                                 "downto 0")
vhdlType' t@(Product _ _) = tyName t
vhdlType' Void       = "std_logic_vector" <> parens (int (-1) <+> "downto 0")

-- | Convert a Netlist HWType to the root of a VHDL type
vhdlTypeMark :: HWType -> VHDLM Doc
vhdlTypeMark Bit             = "std_logic"
vhdlTypeMark Bool            = "boolean"
vhdlTypeMark (Clock _)       = "std_logic"
vhdlTypeMark (Reset _)       = "std_logic"
vhdlTypeMark Integer         = "integer"
vhdlTypeMark (Signed _)      = "signed"
vhdlTypeMark (Unsigned _)    = "unsigned"
vhdlTypeMark (Vector _ Bit)  = "std_logic_vector"
vhdlTypeMark (Vector _ elTy) = "array_of_" <> tyName elTy
vhdlTypeMark (SP _ _)        = "std_logic_vector"
vhdlTypeMark (Sum _ _)       = "unsigned"
vhdlTypeMark t@(Product _ _) = tyName t
vhdlTypeMark t               = error $ $(curLoc) ++ "vhdlTypeMark: " ++ show t

tyName :: HWType -> VHDLM Doc
tyName Integer           = "integer"
tyName Bit               = "std_logic"
tyName Bool              = "boolean"
tyName (Vector n Bit)    = "std_logic_vector_" <> int n
tyName (Vector n elTy)   = "array_of_" <> int n <> "_" <> tyName elTy
tyName (Signed n)        = "signed_" <> int n
tyName (Unsigned n)      = "unsigned_" <> int n
tyName t@(Sum _ _)       = "unsigned_" <> int (typeSize t)
tyName t@(Product _ _)   = makeCached t _3 prodName
  where
    prodName = do i <- _2 <<%= (+1)
                  "product" <> int i
tyName t@(SP _ _)        = "std_logic_vector_" <> int (typeSize t)
tyName _ = empty

-- | Convert a Netlist HWType to a default VHDL value for that type
vhdlTypeDefault :: HWType -> VHDLM Doc
vhdlTypeDefault Bit                 = "'0'"
vhdlTypeDefault Bool                = "false"
vhdlTypeDefault Integer             = "0"
vhdlTypeDefault (Signed _)          = "(others => '0')"
vhdlTypeDefault (Unsigned _)        = "(others => '0')"
vhdlTypeDefault (Vector _ elTy)     = parens ("others" <+> rarrow <+> vhdlTypeDefault elTy)
vhdlTypeDefault (SP _ _)            = "(others => '0')"
vhdlTypeDefault (Sum _ _)           = "(others => '0')"
vhdlTypeDefault (Product _ elTys)   = tupled $ mapM vhdlTypeDefault elTys
vhdlTypeDefault (Reset _)           = "'0'"
vhdlTypeDefault (Clock _)           = "'0'"
vhdlTypeDefault Void                = "((-1) downto 0 => '0')"

decls :: [Declaration] -> VHDLM Doc
decls [] = empty
decls ds = do
    rec (dsDoc,ls) <- fmap (unzip . catMaybes) $ mapM (decl (maximum ls)) ds
    case dsDoc of
      [] -> empty
      _  -> vcat (punctuate semi (A.pure dsDoc)) <> semi

decl :: Int ->  Declaration -> VHDLM (Maybe (Doc,Int))
decl l (NetDecl id_ ty netInit) = Just A.<$> (,fromIntegral (T.length id_)) A.<$>
  "signal" <+> fill l (text id_) <+> colon <+> vhdlType ty <+> ":=" <+> maybe (vhdlTypeDefault ty) (expr False) netInit

decl _ _ = return Nothing

insts :: [Declaration] -> VHDLM Doc
insts [] = empty
insts is = vcat . punctuate linebreak . fmap catMaybes $ mapM inst is

-- | Turn a Netlist Declaration to a VHDL concurrent block
inst :: Declaration -> VHDLM (Maybe Doc)
inst (Assignment id_ e) = fmap Just $
  text id_ <+> larrow <+> expr False e <> semi

inst (CondAssignment id_ scrut es) = fmap Just $
  text id_ <+> larrow <+> align (vcat (conds es)) <> semi
    where
      conds :: [(Maybe Expr,Expr)] -> VHDLM [Doc]
      conds []                = return []
      conds [(_,e)]           = expr False e <:> return []
      conds ((Nothing,e):_)   = expr False e <:> return []
      conds ((Just c ,e):es') = (expr False e <+> "when" <+> parens (expr True scrut <+> "=" <+> expr True c) <+> "else") <:> conds es'

inst (InstDecl nm lbl pms) = fmap Just $
    nest 2 $ text lbl <> "_comp_inst" <+> colon <+> "entity"
              <+> text nm <$$> pms' <> semi
  where
    pms' = do
      rec (p,ls) <- fmap unzip $ sequence [ (,fromIntegral (T.length i)) A.<$> fill (maximum ls) (text i) <+> "=>" <+> expr False e | (i,e) <- pms]
      nest 2 $ "port map" <$$> tupled (A.pure p)

inst (BlackBoxD bs) = fmap Just $ string bs

inst _ = return Nothing

-- | Turn a Netlist expression into a VHDL expression
expr :: Bool -- ^ Enclose in parenthesis?
     -> Expr -- ^ Expr to convert
     -> VHDLM Doc
expr _ (Literal sizeM lit)                           = exprLit sizeM lit
expr _ (Identifier id_ Nothing)                      = text id_
expr _ (Identifier id_ (Just (Indexed (ty@(SP _ args),dcI,fI)))) = fromSLV argTy id_ start end
  where
    argTys   = snd $ args !! dcI
    argTy    = argTys !! fI
    argSize  = typeSize argTy
    other    = otherSize argTys (fI-1)
    start    = typeSize ty - 1 - conSize ty - other
    end      = start - argSize + 1

expr _ (Identifier id_ (Just (Indexed (ty@(Product _ _),_,fI)))) = text id_ <> dot <> tyName ty <> "_sel" <> int fI
expr _ (Identifier id_ (Just (DC (ty@(SP _ _),_)))) = text id_ <> parens (int start <+> "downto" <+> int end)
  where
    start = typeSize ty - 1
    end   = typeSize ty - conSize ty

expr _ (Identifier id_ (Just _)) = text id_
expr _ (DataCon (Vector 1 _) _ [e])              = parens (int 0 <+> rarrow <+> expr False e)
expr _ (vectorChain -> Just es)                  = tupled (mapM (expr False) es)
expr _ (DataCon (Vector _ _) _ [e1,e2])          = expr False e1 <+> "&" <+> expr False e2
expr _ (DataCon ty@(SP _ args) (Just (DC (_,i))) es) = assignExpr
  where
    argTys     = snd $ args !! i
    dcSize     = conSize ty + sum (map typeSize argTys)
    dcExpr     = expr False (dcToExpr ty i)
    argExprs   = zipWith toSLV argTys es -- (map (expr False) es)
    extraArg   = case typeSize ty - dcSize of
                   0 -> []
                   n -> [exprLit (Just n) (NumLit 0)]
    assignExpr = hcat $ punctuate " & " $ sequence (dcExpr:argExprs ++ extraArg)

expr _ (DataCon ty@(Sum _ _) (Just (DC (_,i))) []) = "to_unsigned" <> tupled (sequence [int i,int (typeSize ty)])
expr _ (DataCon ty@(Product _ _) _ es)             = tupled $ zipWithM (\i e -> tName <> "_sel" <> int i <+> rarrow <+> expr False e) [0..] es
  where
    tName = tyName ty

expr b (BlackBoxE bs (Just (DC (ty@(SP _ _),_)))) = parenIf b $ parens (string bs) <> parens (int start <+> "downto" <+> int end)
  where
    start = typeSize ty - 1
    end   = typeSize ty - conSize ty
expr b (BlackBoxE bs _) = parenIf b $ string bs

expr _ _ = empty

otherSize :: [HWType] -> Int -> Int
otherSize _ n | n < 0 = 0
otherSize []     _    = 0
otherSize (a:as) n    = typeSize a + otherSize as (n-1)

vectorChain :: Expr -> Maybe [Expr]
vectorChain (DataCon (Vector _ _) Nothing _)        = Just []
vectorChain (DataCon (Vector 1 _) (Just _) [e])     = Just [e]
vectorChain (DataCon (Vector _ _) (Just _) [e1,e2]) = Just e1 <:> vectorChain e2
vectorChain _                                       = Nothing

exprLit :: Maybe Size -> Literal -> VHDLM Doc
exprLit Nothing   (NumLit i) = int i
exprLit (Just sz) (NumLit i) = bits (toBits sz i)
exprLit _         (BoolLit t) = if t then "true" else "false"
exprLit _         (BitLit b) = squotes $ bit_char b
exprLit _         l          = error $ $(curLoc) ++ "exprLit: " ++ show l

toBits :: Integral a => Int -> a -> [Bit]
toBits size val = map (\x -> if odd x then H else L)
                $ reverse
                $ take size
                $ map (`mod` 2)
                $ iterate (`div` 2) val

bits :: [Bit] -> VHDLM Doc
bits = dquotes . hcat . mapM bit_char

bit_char :: Bit -> VHDLM Doc
bit_char H = char '1'
bit_char L = char '0'
bit_char U = char 'U'
bit_char Z = char 'Z'

toSLV :: HWType -> Expr -> VHDLM Doc
toSLV Bit          e = parens (int 0 <+> rarrow <+> expr False e)
toSLV Bool         e = "toSLV" <> parens (expr False e)
toSLV Integer      e = "std_logic_vector" <> parens ("to_signed" <> tupled (sequence [expr False e,int 32]))
toSLV (Signed _)   e = "std_logic_vector" <> parens (expr False e)
toSLV (Unsigned _) e = "std_logic_vector" <> parens (expr False e)
toSLV (Sum _ _)    e = "std_logic_vector" <> parens (expr False e)
toSLV t@(Product _ tys) (Identifier id_ Nothing) = do
    selIds' <- sequence selIds
    parens (hcat $ punctuate " & " (zipWithM toSLV tys selIds'))
  where
    tName    = tyName t
    selNames = map (fmap (displayT . renderOneLine) ) [text id_ <> dot <> tName <> "_sel" <> int i | i <- [0..(length tys)-1]]
    selIds   = map (fmap (\n -> Identifier n Nothing)) selNames
toSLV (Product _ tys) (DataCon _ _ es) = parens (hcat $ punctuate " & " (zipWithM toSLV tys es))
toSLV (SP _ _) e = expr False e
toSLV hty      e = error $ $(curLoc) ++  "toSLV: ty:" ++ show hty ++ "\n expr: " ++ show e

fromSLV :: HWType -> Identifier -> Int -> Int -> VHDLM Doc
fromSLV Bit               id_ start _   = text id_ <> parens (int start)
fromSLV Bool              id_ start _   = "fromSL" <> parens (text id_ <> parens (int start))
fromSLV Integer           id_ start end = "to_integer" <> parens (fromSLV (Signed 32) id_ start end)
fromSLV (Signed _)        id_ start end = "signed" <> parens (text id_ <> parens (int start <+> "downto" <+> int end))
fromSLV (Unsigned _)      id_ start end = "unsigned" <> parens (text id_ <> parens (int start <+> "downto" <+> int end))
fromSLV (Sum _ _)         id_ start end = "unsigned" <> parens (text id_ <> parens (int start <+> "downto" <+> int end))
fromSLV t@(Product _ tys) id_ start _   = tupled $ zipWithM (\s e -> s <+> rarrow <+> e) selNames args
  where
    tName      = tyName t
    selNames   = [tName <> "_sel" <> int i | i <- [0..]]
    argLengths = map typeSize tys
    starts     = start : snd (mapAccumL ((join (,) .) . (-)) start argLengths)
    ends       = map (+1) (tail starts)
    args       = zipWith3 (`fromSLV` id_) tys starts ends

fromSLV (SP _ _)          id_ start end = text id_ <> parens (int start <+> "downto" <+> int end)
fromSLV hty               _   _     _   = error $ $(curLoc) ++ "fromSLV: " ++ show hty

dcToExpr :: HWType -> Int -> Expr
dcToExpr ty i = Literal (Just $ conSize ty) (NumLit i)

larrow :: VHDLM Doc
larrow = "<="

rarrow :: VHDLM Doc
rarrow = "=>"

parenIf :: Monad m => Bool -> m Doc -> m Doc
parenIf True  = parens
parenIf False = id

punctuate' :: Monad m => m Doc -> m [Doc] -> m Doc
punctuate' s d = vcat (punctuate s d) <> s
