{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Generate VHDL for assorted Netlist datatypes
module CLaSH.Backend.Verilog (VerilogState) where

import qualified Control.Applicative                  as A
import           Control.Lens                         hiding (Indexed)
import           Control.Monad                        (forM,join,liftM,when,zipWithM)
import           Control.Monad.State                  (State,get)
import           Data.Graph.Inductive                 (Gr, mkGraph, topsort')
import           Data.HashMap.Lazy                    (HashMap)
import qualified Data.HashMap.Lazy                    as HashMap
import           Data.HashSet                         (HashSet)
import qualified Data.HashSet                         as HashSet
import           Data.List                            (mapAccumL,nubBy)
import           Data.Maybe                           (catMaybes,mapMaybe)
import           Data.Text.Lazy                       (unpack)
import qualified Data.Text.Lazy                       as T
import           Text.PrettyPrint.Leijen.Text.Monadic

import           CLaSH.Backend
import           CLaSH.Netlist.BlackBox.Util          (renderBlackBox)
import           CLaSH.Netlist.Types
import           CLaSH.Netlist.Util
import           CLaSH.Util                           (clog2, curLoc, makeCached, (<:>))

#ifdef CABAL
import qualified Paths_clash_vhdl
#else
import qualified System.FilePath
#endif

-- | State for the 'CLaSH.Netlist.VHDL.VHDLM' monad:
data VerilogState =
  VerilogState
    { _tyCache   :: HashSet HWType -- ^ Previously encountered  HWTypes
    , _tyCount   :: Int -- ^ Product type counter
    , _nameCache :: HashMap HWType Doc -- ^ Cache for previously generated product type names
    }

makeLenses ''VerilogState

instance Backend VerilogState where
  initBackend     = VerilogState HashSet.empty 0 HashMap.empty
#ifdef CABAL
  primDir         = const (Paths_clash_verilog.getDataFileName "primitives")
#else
  primDir _       = return ("clash-verilog_alt" System.FilePath.</> "primitives")
#endif
  extractTypes    = _tyCache
  name            = const "systemverilog"
  extension       = const ".sv"

  genHDL          = genVerilog
  mkTyPackage     = mkTyPackage_
  hdlType         = verilogType
  hdlTypeErrValue = verilogTypeErrValue
  hdlTypeMark     = verilogTypeMark
  hdlSig t ty     = sigType (text t) ty
  inst            = inst_
  expr            = expr_

type VHDLM a = State VerilogState a
type VerilogM a = State VerilogState a

-- | Generate VHDL for a Netlist component
genVerilog :: Component -> VerilogM (String,Doc)
genVerilog c = (unpack cName,) A.<$> verilog
  where
    cName   = componentName c
    verilog = "// Automatically generated SystemVerilog" <$$>
              tyImports <$$>
              module_ c

-- | Generate a SystemVerilog package containing type definitions for the given HWTypes
mkTyPackage_ :: [HWType]
             -> VerilogM Doc
mkTyPackage_ hwtys =
    "package types ;" <$>
      indent 2 (packageDec) <$>
    "endpackage : types"
  where
    usedTys     = nubBy eqHWTy $ concatMap mkUsedTys hwtys
    needsDec    = nubBy eqHWTy $ (hwtys ++ filter needsTyDec usedTys)
    hwTysSorted = topSortHWTys needsDec
    packageDec  = vcat $ mapM tyDec hwTysSorted

    eqHWTy :: HWType -> HWType -> Bool
    eqHWTy (Vector _ elTy1) (Vector _ elTy2) = case (elTy1,elTy2) of
      (Sum _ _,Sum _ _)    -> typeSize elTy1 == typeSize elTy2
      (Unsigned n,Sum _ _) -> n == typeSize elTy2
      (Sum _ _,Unsigned n) -> typeSize elTy1 == n
      (Index u,Unsigned n) -> clog2 (max 2 u) == n
      (Unsigned n,Index u) -> clog2 (max 2 u) == n
      _ -> elTy1 == elTy2
    eqHWTy ty1 ty2 = ty1 == ty2

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
                                      (HashMap.lookup elTy nodesI)
    edge t@(Product _ tys) = let ti = HashMap.lookupDefault (error $ $(curLoc) ++ "Product") t nodesI
                             in mapMaybe (\ty -> liftM (ti,,()) (HashMap.lookup ty nodesI)) tys
    edge t@(SP _ ctys)     = let ti = HashMap.lookupDefault (error $ $(curLoc) ++ "SP") t nodesI
                             in concatMap (\(_,tys) -> mapMaybe (\ty -> liftM (ti,,()) (HashMap.lookup ty nodesI)) tys) ctys
    edge _                 = []

needsTyDec :: HWType -> Bool
needsTyDec (Product _ _)  = True
needsTyDec _              = False

tyDec :: HWType -> VHDLM Doc
tyDec (Vector n elTy) = "typedef" <+> verilogType elTy <+>  "array_of_" <> int n <> "_" <> tyName elTy <+> brackets (int (n-1) <> colon <> int 0) <> semi

tyDec ty@(Product _ tys) = prodDec
  where
    prodDec = "typedef struct {" <$>
                indent 2 (vcat $ zipWithM (\x y -> sigType x y <> semi) selNames tys) <$>
              "}" <+> tName <> semi

    tName    = tyName ty
    selNames = map (\i -> tName <> "_sel" <> int i) [0..]

tyDec _ = empty

tyImports :: VHDLM Doc
tyImports = "import types:: * ;"

module_ :: Component -> VerilogM Doc
module_ c =
    "module" <+> text (componentName c) <> tupled ports <> semi <$>
    indent 2 (inputPorts <$> outputPort <$$> decls (declarations c)) <$$> insts (declarations c) <$>
    "endmodule"
  where
    ports = sequence
          $ [ text i | (i,_) <- inputs c ] ++
            [ text i | (i,_) <- hiddenPorts c] ++
            [ text (fst $ output c) ]

    inputPorts = case (inputs c ++ hiddenPorts c) of
                   [] -> empty
                   p  -> vcat (punctuate semi (sequence [ "input" <+> sigType (text i) ty | (i,ty) <- p ])) <> semi

    outputPort = "output" <+> sigType (text (fst $ output c)) (snd $ output c) <> semi


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
            $ [ (,fromIntegral $ T.length i) A.<$> (fill l (text i) <+> colon <+> "in" <+> vhdlType ty)
              | (i,ty) <- inputs c ] ++
              [ (,fromIntegral $ T.length i) A.<$> (fill l (text i) <+> colon <+> "in" <+> vhdlType ty)
              | (i,ty) <- hiddenPorts c ] ++
              [ (,fromIntegral $ T.length (fst $ output c)) A.<$> (fill l (text (fst $ output c)) <+> colon <+> "out" <+> vhdlType (snd $ output c))
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
  when (needsTyDec hwty) (undefined %= HashSet.insert hwty)
  vhdlType' hwty

vhdlType' :: HWType -> VHDLM Doc
vhdlType' Bool            = "boolean"
vhdlType' (Clock _)       = "std_logic"
vhdlType' (Reset _)       = "std_logic"
vhdlType' Integer         = "integer"
vhdlType' (BitVector n)   = case n of
                              0 -> "std_logic_vector (0 downto 1)"
                              _ -> "std_logic_vector" <> parens (int (n-1) <+> "downto 0")
vhdlType' (Index u)       = "unsigned" <> parens (int (clog2 (max 2 u) - 1) <+> "downto 0")
vhdlType' (Signed n)      = if n == 0 then "signed (0 downto 1)"
                                      else "signed" <> parens (int (n-1) <+> "downto 0")
vhdlType' (Unsigned n)    = if n == 0 then "unsigned (0 downto 1)"
                                      else "unsigned" <> parens ( int (n-1) <+> "downto 0")
vhdlType' (Vector n elTy) = "array_of_" <> tyName elTy <> parens ("0 to " <> int (n-1))
vhdlType' t@(SP _ _)      = "std_logic_vector" <> parens (int (typeSize t - 1) <+> "downto 0")
vhdlType' t@(Sum _ _)     = case typeSize t of
                              0 -> "unsigned (0 downto 1)"
                              n -> "unsigned" <> parens (int (n -1) <+> "downto 0")
vhdlType' t@(Product _ _) = tyName t
vhdlType' Void            = "std_logic_vector" <> parens (int (-1) <+> "downto 0")

verilogType :: HWType -> VerilogM Doc
verilogType Bool       = "logic"
verilogType Integer    = "logic signed [31:0]"
verilogType (BitVector 1)   = "logic"
verilogType (BitVector n)   = "logic" <+> brackets (int (n-1) <> colon <> int 0)
verilogType (Signed n)      = "logic signed" <+> brackets (int (n-1) <> colon <> int 0)
verilogType t@(SP _ _) = brackets (int (typeSize t - 1) <> colon <> int 0)
verilogType t@(Sum _ _) = case typeSize t of
                            0 -> empty
                            n -> brackets (int (n - 1) <> colon <> int 0)
verilogType t@(Vector n elTy) = do
  tyCache %= HashSet.insert t
  tyName t
verilogType t@(Product _ _) = do
  tyCache %= HashSet.insert t
  tyName t
verilogType x = error ($(curLoc) ++ show x ++ "not supported")



sigType :: VerilogM Doc -> HWType -> VerilogM Doc
sigType d t = verilogType t <+> d


-- sigType d Bool            = "logic" <+> d
-- sigType d (Clock _)       = "logic" <+> d
-- sigType d (Reset _)       = "logic" <+> d
-- sigType d Integer         = "logic signed [31:0]" <+> d
-- sigType d (Unsigned n)    = "logic" <+> brackets (int (n-1) <> colon <> int 0) <+> d
-- sigType d (Signed n)      = "logic signed" <+> brackets (int (n-1) <> colon <> int 0) <+> d
-- sigType d t@(Vector n elTy) = do
--   tyCache %= HashSet.insert t
--   tyName t <+> d
-- sigType d t@(SP _ _) = brackets (int (typeSize t - 1) <> colon <> int 0) <+> d
-- sigType d t@(Sum _ _) = case typeSize t of
--                           0 -> d
--                           n -> brackets (int (n - 1) <> colon <> int 0) <+> d
-- sigType d t@(Product _ _) = do tyCache %= HashSet.insert t
--                                tyName t <+> d
-- sigType d (BitVector 1)   = "logic"
-- sigType d (BitVector n)   = "logic" <+> brackets (int (n-1) <> colon <> int 0) <+> d
-- sigType _ x = error ($(curLoc) ++ show x ++ "not supported")

-- | Convert a Netlist HWType to the root of a Verilog type
verilogTypeMark :: HWType -> VHDLM Doc
verilogTypeMark t@(Product _ _) = do
  tyCache %= HashSet.insert t
  tyName t
verilogTypeMark t@(Vector _ _) = do
  tyCache %= HashSet.insert t
  tyName t
verilogTypeMark t               = error $ $(curLoc) ++ "verilogTypeMark: " ++ show t

tyName :: HWType -> VHDLM Doc
-- tyName Integer           = "integer"
-- tyName Bool              = "boolean"
tyName (Vector n elTy)   = "array_of_" <> int n <> "_" <> tyName elTy
tyName (BitVector 1)     = "logic"
tyName (BitVector _)     = "logic_vector"
-- tyName (BitVector n)     = "std_logic_vector_" <> int n
-- tyName t@(Index _)       = "unsigned_" <> int (typeSize t)
tyName (Signed n)        = "signed_" <> int n
-- tyName (Unsigned n)      = "unsigned_" <> int n
-- tyName t@(Sum _ _)       = "unsigned_" <> int (typeSize t)
tyName t@(Product _ _)   = makeCached t nameCache prodName
  where
    prodName = do i <- tyCount <<%= (+1)
                  "product" <> int i
-- tyName t@(SP _ _)        = "std_logic_vector_" <> int (typeSize t)
tyName t =  error $ $(curLoc) ++ "tyName: " ++ show t

-- | Convert a Netlist HWType to an error VHDL value for that type
verilogTypeErrValue :: HWType -> VHDLM Doc
verilogTypeErrValue Bool                = "1'bx"
verilogTypeErrValue Integer         = "{32 {1'bx}}"
verilogTypeErrValue (Unsigned n)    = braces (int n <+> braces "1'bx")
verilogTypeErrValue (Vector n elTy) = braces (int n <+> braces (verilogTypeErrValue elTy))
verilogTypeErrValue t@(Sum _ _)     = braces (int (typeSize t) <+> braces "1'bx")
verilogTypeErrValue (Product _ elTys)   = "'" <> listBraces (mapM verilogTypeErrValue elTys)
verilogTypeErrValue (BitVector 1)   = "1'bx"
verilogTypeErrValue (BitVector n)   = braces (int n <+> braces "1'bx")
verilogTypeErrValue t@(SP _ _)      = braces (int (typeSize t) <+> braces "1'bx")
verilogTypeErrValue e = error $ $(curLoc) ++ "no error value defined for: " ++ show e

-- vhdlTypeErrValue Integer             = "integer'high"
-- vhdlTypeErrValue (BitVector _)       = "(others => 'X')"
-- vhdlTypeErrValue (Index _)           = "(others => 'X')"
-- vhdlTypeErrValue (Signed _)          = "(others => 'X')"
-- vhdlTypeErrValue (Unsigned _)        = "(others => 'X')"
-- vhdlTypeErrValue (Vector _ elTy)     = parens ("others" <+> rarrow <+> vhdlTypeErrValue elTy)
-- vhdlTypeErrValue (SP _ _)            = "(others => 'X')"

-- vhdlTypeErrValue (Product _ elTys)   = tupled $ mapM vhdlTypeErrValue elTys
-- vhdlTypeErrValue (Reset _)           = "'X'"
-- vhdlTypeErrValue (Clock _)           = "'X'"
-- vhdlTypeErrValue Void                = "(0 downto 1 => 'X')"

decls :: [Declaration] -> VerilogM Doc
decls [] = empty
decls ds = do
    dsDoc <- catMaybes A.<$> mapM decl ds
    case dsDoc of
      [] -> empty
      _  -> vcat (punctuate semi (A.pure dsDoc)) <> semi

decl :: Declaration -> VHDLM (Maybe Doc)
decl (NetDecl id_ ty _) = Just A.<$> sigType (text id_) ty

decl _ = return Nothing

insts :: [Declaration] -> VerilogM Doc
insts [] = empty
insts is = indent 2 . vcat . punctuate linebreak . fmap catMaybes $ mapM inst_ is

-- | Turn a Netlist Declaration to a VHDL concurrent block
inst_ :: Declaration -> VerilogM (Maybe Doc)
inst_ (Assignment id_ e) = fmap Just $
  "assign" <+> text id_ <+> equals <+> expr_ False e <> semi

inst_ (CondAssignment id_ scrut es) = fmap Just $
    "always @(*)" <$>
    "case" <> parens (expr_ True scrut) <$>
      (indent 2 $ vcat $ punctuate semi (conds es)) <> semi <$>
    "endcase"
  where
    conds :: [(Maybe Expr,Expr)] -> VerilogM [Doc]
    conds []                = return []
    conds [(_,e)]           = ("default" <+> colon <+> text id_ <+> equals <+> expr_ False e) <:> return []
    conds ((Nothing,e):_)   = ("default" <+> colon <+> text id_ <+> equals <+> expr_ False e) <:> return []
    conds ((Just c ,e):es') = (expr_ True c <+> colon <+> text id_ <+> equals <+> expr_ False e) <:> conds es'

inst_ (InstDecl nm lbl pms) = fmap Just $
    text nm <+> text lbl <$$> pms' <> semi
  where
    pms' = tupled $ sequence [dot <> text i <+> parens (expr_ False e) | (i,e) <- pms]

inst_ (BlackBoxD _ bs bbCtx) = do t <- renderBlackBox bs bbCtx
                                  fmap Just (string t)

inst_ (NetDecl _ _ _) = return Nothing

-- | Turn a Netlist expression into a VHDL expression
expr_ :: Bool -- ^ Enclose in parenthesis?
      -> Expr -- ^ Expr to convert
      -> VerilogM Doc
expr_ _ (Literal sizeM lit)                           = exprLit sizeM lit
expr_ _ (Identifier id_ Nothing)                      = text id_
expr_ _ (Identifier id_ (Just (Indexed (ty@(SP _ args),dcI,fI)))) = fromSLV argTy id_ start end
  where
    argTys   = snd $ args !! dcI
    argTy    = argTys !! fI
    argSize  = typeSize argTy
    other    = otherSize argTys (fI-1)
    start    = typeSize ty - 1 - conSize ty - other
    end      = start - argSize + 1

expr_ _ (Identifier id_ (Just (Indexed (ty@(Product _ _),_,fI)))) = text id_ <> dot <> verilogTypeMark ty <> "_sel" <> int fI
expr_ _ (Identifier id_ (Just (DC (ty@(SP _ _),_)))) = text id_ <> brackets (int start <> colon <> int end)
  where
    start = typeSize ty - 1
    end   = typeSize ty - conSize ty

expr_ _ (Identifier id_ (Just _))                      = text id_

expr_ _ (DataCon ty@(Vector 1 _) _ [e]) = verilogTypeMark ty <> "'" <> braces (expr_ False e)
expr_ _ e@(DataCon ty@(Vector _ elTy) _ [e1,e2]) = verilogTypeMark ty <> "'" <> case vectorChain e of
                                                     Just es -> listBraces (mapM (expr_ False) es)
                                                     Nothing -> braces (expr_ False e1 <+> comma <+> expr_ False e2)

expr_ _ (DataCon ty@(SP _ args) (DC (_,i)) es) = assignExpr
  where
    argTys     = snd $ args !! i
    dcSize     = conSize ty + sum (map typeSize argTys)
    dcExpr     = expr_ False (dcToExpr ty i)
    argExprs   = zipWith toSLV argTys es
    extraArg   = case typeSize ty - dcSize of
                   0 -> []
                   n -> [exprLit (Just (ty,n)) (NumLit 0)]
    assignExpr = braces (hcat $ punctuate comma $ sequence (dcExpr:argExprs ++ extraArg))

expr_ _ (DataCon ty@(Sum _ _) (DC (_,i)) []) = int (typeSize ty) <> "'d" <> int i
expr_ _ (DataCon ty@(Product _ _) _ es) = "'" <> listBraces (zipWithM (\i e -> verilogTypeMark ty <> "_sel" <> int i <> colon <+> expr_ False e) [0..] es)

expr_ b (BlackBoxE pNm _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.Signed.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- bbLitInputs bbCtx
  = exprLit (Just (Signed (fromInteger n),fromInteger n)) i

expr_ b (BlackBoxE pNm _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.Unsigned.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- bbLitInputs bbCtx
  = exprLit (Just (Unsigned (fromInteger n),fromInteger n)) i

expr_ b (BlackBoxE pNm _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.BitVector.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- bbLitInputs bbCtx
  = exprLit (Just (BitVector (fromInteger n),fromInteger n)) i

expr_ b (BlackBoxE _ bs bbCtx b') = do
  t <- renderBlackBox bs bbCtx
  parenIf (b || b') $ string t

expr_ _ (DataTag Bool (Left e))           = parens (expr_ False e <+> "== 32'sd0") <+> "? 1'b0 : 1'b1"
expr_ _ (DataTag Bool (Right id_))        = parens (text id_ <+> "== 1'b0") <+> "? 32'sd0 : 31'sd1"

expr_ _ (DataTag hty@(Sum _ _) (Left e))  = "$unsigned" <> parens (expr_ False e)
expr_ _ (DataTag (Sum _ _) (Right id_))     = "$unsigned" <> parens (text id_)

expr_ _ (DataTag (Product _ _) (Right _)) = "32'sd0"

expr_ _ (DataTag hty@(SP _ _) (Right id_))  = "$unsigned" <> parens
                                                (text id_ <> brackets
                                                (int start <> colon <> int end))
  where
    start = typeSize hty - 1
    end   = typeSize hty - conSize hty

expr_ _ (DataTag (Vector 0 _) (Right _)) = "32'sd0"
expr_ _ (DataTag (Vector _ _) (Right _)) = "32'sd1"

expr_ _ e = error $ $(curLoc) ++ (show e) -- empty

otherSize :: [HWType] -> Int -> Int
otherSize _ n | n < 0 = 0
otherSize []     _    = 0
otherSize (a:as) n    = typeSize a + otherSize as (n-1)

vectorChain :: Expr -> Maybe [Expr]
vectorChain (DataCon (Vector 0 _) _ _)        = Just []
vectorChain (DataCon (Vector 1 _) _ [e])     = Just [e]
vectorChain (DataCon (Vector _ _) _ [e1,e2]) = Just e1 <:> vectorChain e2
vectorChain _                                       = Nothing

exprLit :: Maybe (HWType,Size) -> Literal -> VerilogM Doc
exprLit Nothing         (NumLit i) = integer i
exprLit (Just (hty,sz)) (NumLit i) = case hty of
                                       Unsigned _  -> int sz <> "'d" <> integer i
                                       Signed _    -> int sz <> "'sd" <> integer i
                                       _           -> int sz <> "'b" <> blit

  where
    blit = bits (toBits sz i)
exprLit _             (BoolLit t)  = if t then "1'b1" else "1'b0"
-- exprLit _             (BitLit b)   = squotes $ bit_char b
exprLit _             l            = error $ $(curLoc) ++ "exprLit: " ++ show l

toBits :: Integral a => Int -> a -> [Bit]
toBits size val = map (\x -> if odd x then H else L)
                $ reverse
                $ take size
                $ map (`mod` 2)
                $ iterate (`div` 2) val

bits :: [Bit] -> VerilogM Doc
bits = hcat . mapM bit_char

bit_char :: Bit -> VerilogM Doc
bit_char H = char '1'
bit_char L = char '0'
bit_char U = char 'U'
bit_char Z = char 'Z'

toSLV :: HWType -> Expr -> VerilogM Doc
toSLV Integer e = expr_ False e
-- toSLV Bool         e = "toSLV" <> parens (expr_ False e)
-- toSLV Integer      e = "std_logic_vector" <> parens ("to_signed" <> tupled (sequence [expr_ False e,int 32]))
-- toSLV (BitVector _) e = expr_ False e
-- toSLV (Signed _)   e = "std_logic_vector" <> parens (expr_ False e)
-- toSLV (Unsigned _) e = "std_logic_vector" <> parens (expr_ False e)
-- toSLV (Sum _ _)    e = "std_logic_vector" <> parens (expr_ False e)
-- toSLV t@(Product _ tys) (Identifier id_ Nothing) = do
--     selIds' <- sequence selIds
--     encloseSep lparen rparen " & " (zipWithM toSLV tys selIds')
--   where
--     tName    = tyName t
--     selNames = map (fmap (displayT . renderOneLine) ) [text id_ <> dot <> tName <> "_sel" <> int i | i <- [0..(length tys)-1]]
--     selIds   = map (fmap (\n -> Identifier n Nothing)) selNames
-- toSLV (Product _ tys) (DataCon _ _ es) = encloseSep lparen rparen " & " (zipWithM toSLV tys es)
-- toSLV (SP _ _) e = expr_ False e
-- toSLV (Vector n elTy) (Identifier id_ Nothing) = do
--     selIds' <- sequence (reverse selIds)
--     parens (encloseSep lparen rparen " & " (mapM (toSLV elTy) selIds'))
--   where
--     selNames = map (fmap (displayT . renderOneLine) ) $ reverse [text id_ <> parens (int i) | i <- [0 .. (n-1)]]
--     selIds   = map (fmap (`Identifier` Nothing)) selNames
-- toSLV (Vector n elTy) (DataCon _ _ es) = encloseSep lparen rparen " & " (zipWithM toSLV [elTy,Vector (n-1) elTy] es)
toSLV hty      e = error $ $(curLoc) ++  "toSLV: ty:" ++ show hty ++ "\n expr: " ++ show e

fromSLV :: HWType -> Identifier -> Int -> Int -> VHDLM Doc
-- fromSLV Bool              id_ start _   = "fromSL" <> parens (text id_ <> parens (int start))
fromSLV Integer           id_ start end = fromSLV (Signed 32) id_ start end
-- fromSLV (BitVector _)     id_ start end = text id_ <> parens (int start <+> "downto" <+> int end)
-- fromSLV (Index _)         id_ start end = "unsigned" <> parens (text id_ <> parens (int start <+> "downto" <+> int end))
fromSLV (Signed _)        id_ start end = text id_ <> brackets (int start <> colon <> int end)
-- fromSLV (Unsigned _)      id_ start end = "unsigned" <> parens (text id_ <> parens (int start <+> "downto" <+> int end))
-- fromSLV (Sum _ _)         id_ start end = "unsigned" <> parens (text id_ <> parens (int start <+> "downto" <+> int end))
-- fromSLV t@(Product _ tys) id_ start _   = tupled $ zipWithM (\s e -> s <+> rarrow <+> e) selNames args
--   where
--     tName      = tyName t
--     selNames   = [tName <> "_sel" <> int i | i <- [0..]]
--     argLengths = map typeSize tys
--     starts     = start : snd (mapAccumL ((join (,) .) . (-)) start argLengths)
--     ends       = map (+1) (tail starts)
--     args       = zipWith3 (`fromSLV` id_) tys starts ends

-- fromSLV (SP _ _)          id_ start end = text id_ <> parens (int start <+> "downto" <+> int end)
-- fromSLV (Vector n elTy)   id_ start _   = tupled (fmap reverse args)
--   where
--     argLength = typeSize elTy
--     starts    = take (n + 1) $ iterate (subtract argLength) start
--     ends      = map (+1) (tail starts)
--     args      = zipWithM (fromSLV elTy id_) starts ends
fromSLV hty               _   _     _   = error $ $(curLoc) ++ "fromSLV: " ++ show hty

dcToExpr :: HWType -> Int -> Expr
dcToExpr ty i = Literal (Just (ty,conSize ty)) (NumLit (toInteger i))

larrow :: VHDLM Doc
larrow = "<="

rarrow :: VHDLM Doc
rarrow = "=>"

parenIf :: Monad m => Bool -> m Doc -> m Doc
parenIf True  = parens
parenIf False = id

punctuate' :: Monad m => m Doc -> m [Doc] -> m Doc
punctuate' s d = vcat (punctuate s d) <> s

listBraces :: Monad m => m [Doc] -> m Doc
listBraces = encloseSep lbrace rbrace comma
