{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Generate SystemVerilog for assorted Netlist datatypes
module CLaSH.Backend.SystemVerilog (SystemVerilogState) where

import qualified Control.Applicative                  as A
import           Control.Lens                         hiding (Indexed)
import           Control.Monad                        (join,liftM,zipWithM)
import           Control.Monad.State                  (State)
import           Data.Graph.Inductive                 (Gr, mkGraph, topsort')
import           Data.HashMap.Lazy                    (HashMap)
import qualified Data.HashMap.Lazy                    as HashMap
import           Data.HashSet                         (HashSet)
import qualified Data.HashSet                         as HashSet
import           Data.List                            (mapAccumL,nubBy)
import           Data.Maybe                           (catMaybes,mapMaybe)
import           Data.Text.Lazy                       (unpack)
import           Text.PrettyPrint.Leijen.Text.Monadic

import           CLaSH.Backend
import           CLaSH.Netlist.BlackBox.Util          (parseFail, renderBlackBox)
import           CLaSH.Netlist.Types
import           CLaSH.Netlist.Util
import           CLaSH.Util                           (curLoc, makeCached, (<:>))

#ifdef CABAL
import qualified Paths_clash_systemverilog
#else
import qualified System.FilePath
#endif

-- | State for the 'CLaSH.Backend.SystemVerilog.SystemVerilogM' monad:
data SystemVerilogState =
  SystemVerilogState
    { _tyCache   :: HashSet HWType -- ^ Previously encountered  HWTypes
    , _tyCount   :: Int -- ^ Product type counter
    , _nameCache :: HashMap HWType Doc -- ^ Cache for previously generated product type names
    }

makeLenses ''SystemVerilogState

instance Backend SystemVerilogState where
  initBackend     = SystemVerilogState HashSet.empty 0 HashMap.empty
#ifdef CABAL
  primDir         = const (Paths_clash_systemverilog.getDataFileName "primitives")
#else
  primDir _       = return ("clash-systemverilog" System.FilePath.</> "primitives")
#endif
  extractTypes    = _tyCache
  name            = const "systemverilog"
  extension       = const ".sv"

  genHDL          = genVerilog
  mkTyPackage     = mkTyPackage_
  hdlType         = verilogType
  hdlTypeErrValue = verilogTypeErrValue
  hdlTypeMark     = verilogTypeMark
  hdlSig t ty     = sigDecl (text t) ty
  inst            = inst_
  expr            = expr_

type SystemVerilogM a = State SystemVerilogState a

-- | Generate VHDL for a Netlist component
genVerilog :: Component -> SystemVerilogM (String,Doc)
genVerilog c = (unpack cName,) A.<$> verilog
  where
    cName   = componentName c
    verilog = "// Automatically generated SystemVerilog" <$$>
              tyImports <$$>
              module_ c

-- | Generate a SystemVerilog package containing type definitions for the given HWTypes
mkTyPackage_ :: [HWType]
             -> SystemVerilogM Doc
mkTyPackage_ hwtys =
    "package types ;" <$>
      indent 2 packageDec <$>
      indent 2 funDecs <$>
    "endpackage : types"
  where
    usedTys     = concatMap mkUsedTys hwtys
    needsDec    = nubBy eqReprTy $ (hwtys ++ usedTys)
    hwTysSorted = topSortHWTys needsDec
    packageDec  = vcat $ mapM tyDec hwTysSorted
    funDecs     = vcat $ mapM funDec hwTysSorted

    eqReprTy :: HWType -> HWType -> Bool
    eqReprTy (Vector n ty1) (Vector m ty2)
      | m == n    = eqReprTy ty1 ty2
      | otherwise = False
    eqReprTy ty1 ty2
      | isUnsigned ty1 && isUnsigned ty2 = typeSize ty1 == typeSize ty2
      | otherwise                        = ty1 == ty2

    isUnsigned :: HWType -> Bool
    isUnsigned Bool          = True
    isUnsigned (Unsigned _)  = True
    isUnsigned (BitVector _) = True
    isUnsigned (Index _)     = True
    isUnsigned (Sum _ _)     = True
    isUnsigned (SP _ _)      = True
    isUnsigned _             = False

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

tyDec :: HWType -> SystemVerilogM Doc
tyDec (Vector n elTy) = "typedef" <+> verilogType elTy <+>  "array_of_" <> int n <> "_" <> tyName elTy <+> brackets (int 0 <> colon <> int (n-1)) <> semi

tyDec ty@(Product _ tys) = prodDec
  where
    prodDec = "typedef struct {" <$>
                indent 2 (vcat $ zipWithM (\x y -> sigDecl x y <> semi) selNames tys) <$>
              "}" <+> tName <> semi

    tName    = tyName ty
    selNames = map (\i -> tName <> "_sel" <> int i) [0..]

tyDec _ = empty

funDec :: HWType -> SystemVerilogM Doc
funDec (Clock _) = empty
funDec (Reset _) = empty
funDec t =
  "function logic" <+> brackets (int (typeSize t - 1) <> colon <> int 0)  <+> verilogTypeMark t <> "_to_lv" <> parens (sigDecl "i" t) <> semi <$>
  indent 2 (verilogTypeMark t <> "_to_lv" <+> "=" <+>
    (case t of
       Vector n elTy -> listBraces (sequence [verilogTypeMark elTy <> "_to_lv" <> parens ("i" <> brackets (int i)) | i <- [0..(n-1)]])
       Product _ tys -> listBraces (zipWithM (\elTy i -> verilogTypeMark elTy <> "_to_lv" <> parens ("i" <> dot <> verilogTypeMark t <> "_sel" <> int i)) tys [0..])
       _             -> "i")
       <> semi) <$>
  "endfunction"

tyImports :: SystemVerilogM Doc
tyImports = "import types:: * ;"

module_ :: Component -> SystemVerilogM Doc
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
                   p  -> vcat (punctuate semi (sequence [ "input" <+> sigDecl (text i) ty | (i,ty) <- p ])) <> semi

    outputPort = "output" <+> sigDecl (text (fst $ output c)) (snd $ output c) <> semi

verilogType :: HWType -> SystemVerilogM Doc
verilogType t = do
  tyCache %= HashSet.insert t
  case t of
    (Vector _ _)  -> tyName t
    (Product _ _) -> tyName t
    Integer       -> verilogType (Signed 32)
    (Signed n)    -> "logic signed" <+> brackets (int (n-1) <> colon <> int 0)
    (Clock _)     -> "logic"
    (Reset _)     -> "logic"
    _             -> "logic" <+> brackets (int (typeSize t -1) <> colon <> int 0)

sigDecl :: SystemVerilogM Doc -> HWType -> SystemVerilogM Doc
sigDecl d t = verilogType t <+> d

-- | Convert a Netlist HWType to the root of a Verilog type
verilogTypeMark :: HWType -> SystemVerilogM Doc
verilogTypeMark t = do
  tyCache %= HashSet.insert t
  tyName t

tyName :: HWType -> SystemVerilogM Doc
tyName Integer           = "integer_32"
tyName Bool              = "logic_vector_1"
tyName (Vector n elTy)   = "array_of_" <> int n <> "_" <> tyName elTy
tyName (BitVector n)     = "logic_vector_" <> int n
tyName t@(Index _)       = "logic_vector_" <> int (typeSize t)
tyName (Signed n)        = "signed_" <> int n
tyName (Unsigned n)      = "logic_vector_" <> int n
tyName t@(Sum _ _)       = "logic_vector_" <> int (typeSize t)
tyName t@(Product _ _)   = makeCached t nameCache prodName
  where
    prodName = do i <- tyCount <<%= (+1)
                  "product" <> int i
tyName t@(SP _ _)        = "logic_vector_" <> int (typeSize t)
tyName (Clock _) = "logic"
tyName (Reset _) = "logic"
tyName t =  error $ $(curLoc) ++ "tyName: " ++ show t

-- | Convert a Netlist HWType to an error VHDL value for that type
verilogTypeErrValue :: HWType -> SystemVerilogM Doc
verilogTypeErrValue Bool                = "1'bx"
verilogTypeErrValue Integer         = "{32 {1'bx}}"
verilogTypeErrValue (Unsigned n)    = braces (int n <+> braces "1'bx")
verilogTypeErrValue (Signed n)      = braces (int n <+> braces "1'bx")
verilogTypeErrValue (Vector n elTy) = "'" <> braces (int n <+> braces (verilogTypeErrValue elTy))
verilogTypeErrValue t@(Sum _ _)     = braces (int (typeSize t) <+> braces "1'bx")
verilogTypeErrValue (Product _ elTys)   = "'" <> listBraces (mapM verilogTypeErrValue elTys)
verilogTypeErrValue (BitVector 1)   = "1'bx"
verilogTypeErrValue (BitVector n)   = braces (int n <+> braces "1'bx")
verilogTypeErrValue t@(SP _ _)      = braces (int (typeSize t) <+> braces "1'bx")
verilogTypeErrValue e = error $ $(curLoc) ++ "no error value defined for: " ++ show e

decls :: [Declaration] -> SystemVerilogM Doc
decls [] = empty
decls ds = do
    dsDoc <- catMaybes A.<$> mapM decl ds
    case dsDoc of
      [] -> empty
      _  -> vcat (A.pure dsDoc)

decl :: Declaration -> SystemVerilogM (Maybe Doc)
decl (NetDecl id_ ty netInit) = Just A.<$> sigDecl (text id_) ty <> semi <$>
  maybe empty (\e -> "// pragma translate_off" <$> "initial begin" <$> indent 2 (text id_ <+> "=" <+> expr_ False e <> semi) <$> "end" <$> "// pragma translate_on") netInit

decl _ = return Nothing

insts :: [Declaration] -> SystemVerilogM Doc
insts [] = empty
insts is = indent 2 . vcat . punctuate linebreak . fmap catMaybes $ mapM inst_ is

-- | Turn a Netlist Declaration to a SystemVerilog concurrent block
inst_ :: Declaration -> SystemVerilogM (Maybe Doc)
inst_ (Assignment id_ e) = fmap Just $
  "assign" <+> text id_ <+> equals <+> expr_ False e <> semi

inst_ (CondAssignment id_ scrut es) = fmap Just $
    "always_comb begin" <$>
    indent 2 ("case" <> parens (expr_ True scrut) <$>
                (indent 2 $ vcat $ punctuate semi (conds es)) <> semi <$>
              "endcase") <$>
    "end"
  where
    conds :: [(Maybe Expr,Expr)] -> SystemVerilogM [Doc]
    conds []                = return []
    conds [(_,e)]           = ("default" <+> colon <+> text id_ <+> equals <+> expr_ False e) <:> return []
    conds ((Nothing,e):_)   = ("default" <+> colon <+> text id_ <+> equals <+> expr_ False e) <:> return []
    conds ((Just c ,e):es') = (expr_ True c <+> colon <+> text id_ <+> equals <+> expr_ False e) <:> conds es'

inst_ (InstDecl nm lbl pms) = fmap Just $
    text nm <+> text lbl <$$> pms' <> semi
  where
    pms' = tupled $ sequence [dot <> text i <+> parens (expr_ False e) | (i,e) <- pms]

inst_ (BlackBoxD pNm _ bbCtx)
  | pNm == "CLaSH.Sized.Internal.Signed.resize#"
  , ((Literal _ (NumLit n)):(Literal _ (NumLit m)):_) <- bbLitInputs bbCtx
  , n < m
  = do
    let bs = parseFail "assign ~RESULT = $signed(~ARG[2]);"
    t <- renderBlackBox bs bbCtx
    fmap Just (string t)

inst_ (BlackBoxD _ bs bbCtx) = do
  t <- renderBlackBox bs bbCtx
  fmap Just (string t)

inst_ (NetDecl _ _ _) = return Nothing

-- | Turn a Netlist expression into a SystemVerilog expression
expr_ :: Bool -- ^ Enclose in parenthesis?
      -> Expr -- ^ Expr to convert
      -> SystemVerilogM Doc
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

expr_ _ (DataCon (Vector 1 _) _ [e]) = "'" <> braces (expr_ False e)
expr_ _ e@(DataCon (Vector n _) _ [e1,e2]) = "'" <> case vectorChain e of
                                                     Just es -> listBraces (mapM (expr_ False) es)
                                                     Nothing -> let e2' = expr_ False e2
                                                                in  listBraces $ sequence ((expr_ False e1):[e2' <> brackets (int i) | i <- [0..(n-2)] ])

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

expr_ _ (BlackBoxE pNm _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.Signed.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- bbLitInputs bbCtx
  = exprLit (Just (Signed (fromInteger n),fromInteger n)) i

expr_ _ (BlackBoxE pNm _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.Unsigned.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- bbLitInputs bbCtx
  = exprLit (Just (Unsigned (fromInteger n),fromInteger n)) i

expr_ _ (BlackBoxE pNm _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.BitVector.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- bbLitInputs bbCtx
  = exprLit (Just (BitVector (fromInteger n),fromInteger n)) i

expr_ b (BlackBoxE _ bs bbCtx b') = do
  t <- renderBlackBox bs bbCtx
  parenIf (b || b') $ string t

expr_ _ (DataTag Bool (Left e))           = parens (expr_ False e <+> "== 32'sd0") <+> "? 1'b0 : 1'b1"
expr_ _ (DataTag Bool (Right id_))        = parens (text id_ <+> "== 1'b0") <+> "? 32'sd0 : 31'sd1"

expr_ _ (DataTag (Sum _ _) (Left e))  = "$unsigned" <> parens (expr_ False e)
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

exprLit :: Maybe (HWType,Size) -> Literal -> SystemVerilogM Doc
exprLit Nothing         (NumLit i) = integer i
exprLit (Just (hty,sz)) (NumLit i) = case hty of
                                       Unsigned _   -> int sz <> "'d" <> integer i
                                       Signed _
                                        | i < 0     -> "-" <> int sz <> "'sd" <> integer (abs i)
                                        | otherwise -> int sz <> "'sd" <> integer i
                                       _            -> int sz <> "'b" <> blit

  where
    blit = bits (toBits sz i)
exprLit _             (BoolLit t)  = if t then "1'b1" else "1'b0"
exprLit _             (BitLit b)   = "1'b" <> bit_char b
exprLit _             l            = error $ $(curLoc) ++ "exprLit: " ++ show l

toBits :: Integral a => Int -> a -> [Bit]
toBits size val = map (\x -> if odd x then H else L)
                $ reverse
                $ take size
                $ map (`mod` 2)
                $ iterate (`div` 2) val

bits :: [Bit] -> SystemVerilogM Doc
bits = hcat . mapM bit_char

bit_char :: Bit -> SystemVerilogM Doc
bit_char H = char '1'
bit_char L = char '0'
bit_char U = char 'U'
bit_char Z = char 'Z'

toSLV :: HWType -> Expr -> SystemVerilogM Doc
toSLV t@(Product _ tys) (Identifier id_ Nothing) = do
    selIds' <- sequence selIds
    listBraces (zipWithM toSLV tys selIds')
  where
    tName    = verilogTypeMark t
    selNames = map (fmap (displayT . renderOneLine) ) [text id_ <> dot <> tName <> "_sel" <> int i | i <- [0..(length tys)-1]]
    selIds   = map (fmap (\n -> Identifier n Nothing)) selNames
toSLV (Product _ tys) (DataCon _ _ es) = listBraces (zipWithM toSLV tys es)

toSLV (Vector n elTy) (Identifier id_ Nothing) = do
    selIds' <- sequence (reverse selIds)
    listBraces (mapM (toSLV elTy) selIds')
  where
    selNames = map (fmap (displayT . renderOneLine) ) $ reverse [text id_ <> brackets (int i) | i <- [0 .. (n-1)]]
    selIds   = map (fmap (`Identifier` Nothing)) selNames
toSLV (Vector n elTy) (DataCon _ _ es) = listBraces (zipWithM toSLV [elTy,Vector (n-1) elTy] es)

toSLV _ e = expr_ False e

fromSLV :: HWType -> Identifier -> Int -> Int -> SystemVerilogM Doc
fromSLV t@(Product _ tys) id_ start _ = "'" <> listBraces (zipWithM (\s e -> s <> colon <+> e) selNames args)
  where
    tName      = tyName t
    selNames   = [tName <> "_sel" <> int i | i <- [0..]]
    argLengths = map typeSize tys
    starts     = start : snd (mapAccumL ((join (,) .) . (-)) start argLengths)
    ends       = map (+1) (tail starts)
    args       = zipWith3 (`fromSLV` id_) tys starts ends

fromSLV t@(Vector n elTy) id_ start _ = verilogTypeMark t <> "'" <> parens ("'" <> listBraces (fmap reverse args))
  where
    argLength = typeSize elTy
    starts    = take (n + 1) $ iterate (subtract argLength) start
    ends      = map (+1) (tail starts)
    args      = zipWithM (fromSLV elTy id_) starts ends

fromSLV Integer    id_ start end = fromSLV (Signed 32) id_ start end
fromSLV (Signed _) id_ start end = "$signed" <> parens (text id_ <> brackets (int start <> colon <> int end))

fromSLV _ id_ start end = text id_ <> brackets (int start <> colon <> int end)

dcToExpr :: HWType -> Int -> Expr
dcToExpr ty i = Literal (Just (ty,conSize ty)) (NumLit (toInteger i))

listBraces :: Monad m => m [Doc] -> m Doc
listBraces = encloseSep lbrace rbrace comma

parenIf :: Monad m => Bool -> m Doc -> m Doc
parenIf True  = parens
parenIf False = id
