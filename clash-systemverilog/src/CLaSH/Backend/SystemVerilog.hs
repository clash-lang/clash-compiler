{-|
  Copyright   :  (C) 2015-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Generate SystemVerilog for assorted Netlist datatypes
-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module CLaSH.Backend.SystemVerilog (SystemVerilogState) where

import qualified Control.Applicative                  as A
import           Control.Lens                         hiding (Indexed)
import           Control.Monad                        (liftM,zipWithM)
import           Control.Monad.State                  (State)
import           Data.Graph.Inductive                 (Gr, mkGraph, topsort')
import           Data.HashMap.Lazy                    (HashMap)
import qualified Data.HashMap.Lazy                    as HashMap
import           Data.HashSet                         (HashSet)
import qualified Data.HashSet                         as HashSet
import           Data.List                            (nubBy)
import           Data.Maybe                           (catMaybes,mapMaybe)
import           Data.Text.Lazy                       (pack,unpack)
import qualified Data.Text.Lazy                       as Text
import           Prelude                              hiding ((<$>))
import           Text.PrettyPrint.Leijen.Text.Monadic

import           CLaSH.Backend
import           CLaSH.Netlist.BlackBox.Types         (HdlSyn (..))
import           CLaSH.Netlist.BlackBox.Util          (extractLiterals, renderBlackBox)
import           CLaSH.Netlist.Id                     (mkBasicId')
import           CLaSH.Netlist.Types                  hiding (_intWidth, intWidth)
import           CLaSH.Netlist.Util                   hiding (mkBasicId)
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
    , _tySeen    :: [Identifier] -- ^ Product type counter
    , _nameCache :: HashMap HWType Doc -- ^ Cache for previously generated product type names
    , _genDepth  :: Int -- ^ Depth of current generative block
    , _modNm     :: String
    , _intWidth  :: Int -- ^ Int/Word/Integer bit-width
    , _hdlsyn    :: HdlSyn
    }

makeLenses ''SystemVerilogState

instance Backend SystemVerilogState where
  initBackend     = SystemVerilogState HashSet.empty [] HashMap.empty 0 ""
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
  genStmt True    = do cnt <- use genDepth
                       genDepth += 1
                       if cnt > 0
                          then empty
                          else "generate"
  genStmt False   = do genDepth -= 1
                       cnt <- use genDepth
                       if cnt > 0
                          then empty
                          else "endgenerate"
  inst            = inst_
  expr            = expr_
  iwWidth         = use intWidth
  toBV hty id_    = toSLV hty (Identifier id_ Nothing)
  fromBV hty id_  = simpleFromSLV hty id_
  hdlSyn          = use hdlsyn
  mkBasicId       = return (filterReserved . mkBasicId' True)
  setModName nm s = s {_modNm = nm}

type SystemVerilogM a = State SystemVerilogState a

-- List of reserved SystemVerilog-2012 keywords
reservedWords :: [Identifier]
reservedWords = ["accept_on","alias","always","always_comb","always_ff"
  ,"always_latch","and","assert","assign","assume","automatic","before","begin"
  ,"bind","bins","binsof","bit","break","buf","bufif0","bufif1","byte","case"
  ,"casex","casez","cell","chandle","checker","class","clocking","cmos","config"
  ,"const","constraint","context","continue","cover","covergroup","coverpoint"
  ,"cross","deassign","default","defparam","design","disable","dist","do","edge"
  ,"else","end","endcase","endchecker","endclass","endclocking","endconfig"
  ,"endfunction","endgenerate","endgroup","endinterface","endmodule","endpackage"
  ,"endprimitive","endprogram","endproperty","endspecify","endsequence"
  ,"endtable","endtask","enum","event","eventually","expect","export","extends"
  ,"extern","final","first_match","for","force","foreach","forever","fork"
  ,"forkjoin","function","generate","genvar","global","highz0","highz1","if"
  ,"iff","ifnone","ignore_bins","illegal_bins","implements","implies","import"
  ,"incdir","include","initial","inout","input","inside","instance","int"
  ,"integer","interconnect","interface","intersect","join","join_any"
  ,"join_none","large","let","liblist","library","local","localparam","logic"
  ,"longint","macromodule","matches","medium","modport","module","nand"
  ,"negedge","nettype","new","nexttime","nmos","nor","noshowcancelled","not"
  ,"notif0","notif1","null","or","output","package","packed","parameter","pmos"
  ,"posedge","primitive","priority","program","property","protected","pull0"
  ,"pull1","pulldown","pullup","pulsestyle_ondetect","pulsestyle_onevent"
  ,"pure","rand","randc","randcase","randsequence","rcmos","real","realtime"
  ,"ref","reg","reject_on","release","repeat","restrict","return","rnmos"
  ,"rpmos","rtran","rtranif0","rtranif1","s_always","s_eventually","s_nexttime"
  ,"s_until","s_until_with","scalared","sequence","shortint","shortreal"
  ,"showcancelled","signed","small","soft","solve","specify","specparam"
  ,"static","string","strong","strong0","strong1","struct","super","supply0"
  ,"supply1","sync_accept_on","sync_reject_on","table","tagged","task","this"
  ,"throughout","time","timeprecision","timeunit","tran","tranif0","tranif1"
  ,"tri","tri0","tri1","triand","trior","trireg","type","typedef","union"
  ,"unique","unique0","unsigned","until","until_with","untyped","use","uwire"
  ,"var","vectored","virtual","void","wait","wait_order","wand","weak","weak0"
  ,"weak1","while","wildcard","wire","with","within","wor","xnor","xor"]

filterReserved :: Identifier -> Identifier
filterReserved s = if s `elem` reservedWords
  then s `Text.append` "_r"
  else s

-- | Generate VHDL for a Netlist component
genVerilog :: String -> Component -> SystemVerilogM (String,Doc)
genVerilog _ c = (unpack cName,) A.<$> verilog
  where
    cName   = componentName c
    verilog = "// Automatically generated SystemVerilog-2005" <$$>
              module_ c

-- | Generate a SystemVerilog package containing type definitions for the given HWTypes
mkTyPackage_ :: String
             -> [HWType]
             -> SystemVerilogM [(String,Doc)]
mkTyPackage_ modName hwtys =
    (:[]) A.<$> (modName ++ "_types",) A.<$>
       "package" <+> modNameD <> "_types" <> semi <$>
         indent 2 packageDec <$>
         indent 2 funDecs <$>
       "endpackage" <+> colon <+> modNameD <> "_types"
  where
    modNameD    = text (pack modName)
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
tyDec ty@(Vector n elTy) = do
  syn <- hdlSyn
  case syn of
    Vivado -> "typedef" <+> "logic" <+> brackets (int (typeSize elTy - 1) <> colon <> int 0) <+>
              tyName ty <+> brackets (int 0 <> colon <> int (n-1)) <> semi
    _ -> do case splitVecTy ty of
              Just (ns,elTy') -> do
                let ranges = hcat (mapM (\n' -> brackets (int 0 <> colon <> int (n'-1))) (tail ns))
                "typedef" <+> elTy' <+> ranges <+> tyName ty <+> brackets (int 0 <> colon <> int (head ns - 1)) <> semi
              _ -> error $ $(curLoc) ++ "impossible"
tyDec ty@(Product _ tys) = prodDec
  where
    prodDec = "typedef struct packed {" <$>
                indent 2 (vcat $ zipWithM (\x y -> lvType y <+> x <> semi) selNames tys) <$>
              "}" <+> tName <> semi

    tName    = tyName ty
    selNames = map (\i -> tName <> "_sel" <> int i) [0..]

tyDec _ = empty

splitVecTy :: HWType -> Maybe ([Int],SystemVerilogM Doc)
splitVecTy = fmap splitElemTy . go
  where
    splitElemTy (ns,t) = case t of
      Product _ _ -> (ns, verilogType t)
      Vector _ _  -> error $ $(curLoc) ++ "impossible"
      Clock _ _   -> (ns, "logic")
      Reset _ _   -> (ns, "logic")
      String      -> (ns, "string")
      Signed n    -> (ns ++ [n],"logic signed")
      _           -> (ns ++ [typeSize t], "logic")

    go (Vector n elTy) = case go elTy of
      Just (ns,elTy') -> Just (n:ns,elTy')
      _               -> Just ([n],elTy)
    go _ = Nothing

lvType :: HWType -> SystemVerilogM Doc
lvType ty@(Vector n elTy) = do
  syn <- hdlSyn
  case syn of
    Vivado -> "logic" <+> brackets (int 0 <> colon <> int (n-1)) <> brackets (int (typeSize elTy - 1) <> colon <> int 0)
    _ -> case splitVecTy ty of
      Just (ns,elTy') -> do
        let ranges = hcat (mapM (\n' -> brackets (int 0 <> colon <> int (n'-1))) ns)
        elTy' <> ranges
      _ -> error $ $(curLoc) ++ "impossible"
lvType ty = verilogType ty

funDec :: HWType -> SystemVerilogM Doc
funDec ty@(Vector n elTy) =
  "function" <+> "logic" <+> ranges <+> tName <> "_to_lv" <> parens (sigDecl "i" ty) <> semi <$>
  indent 2
    ("for" <+> parens ("int n = 0" <> semi <+> "n <" <+> int n <> semi <+> "n=n+1") <$>
      indent 2 (tName <> "_to_lv" <> brackets "n" <+> "=" <+> "i[n]" <> semi)) <$>
  "endfunction" <$>
  "function" <+> tName <+> tName <> "_from_lv" <> parens ("logic" <+> ranges <+> "i") <> semi <$>
  indent 2
    ("for" <+> parens ("int n = 0" <> semi <+> "n <" <+> int n <> semi <+> "n=n+1") <$>
      indent 2 (tName <> "_from_lv" <> brackets "n" <+> "=" <+> "i[n]" <> semi)) <$>
  "endfunction" <$>
  "function" <+> tName <+> tName <> "_cons" <> parens (sigDecl "x" elTy <> comma <> vecSigDecl "xs") <> semi <$>
  indent 2
    (tName <> "_cons" <> brackets (int 0) <+> "=" <+> (toSLV elTy (Identifier "x" Nothing)) <> semi <$>
     tName <> "_cons" <> brackets (int 1 <> colon <> int (n-1)) <+> "=" <+> "xs" <> semi) <$>
  "endfunction"
  where
    tName  = tyName ty
    ranges = brackets (int 0 <> colon <> int (n-1)) <>
             brackets (int (typeSize elTy - 1) <> colon <> int 0)

    vecSigDecl :: SystemVerilogM Doc -> SystemVerilogM Doc
    vecSigDecl d = do
      syn <- hdlSyn
      case syn of
        Vivado -> "logic" <+> brackets (int (typeSize elTy - 1) <> colon <> int 0) <+>
                  d <+> brackets (int 0 <> colon <> int (n-2))
        _ -> do case splitVecTy ty of
                  Just (ns,elTy') -> do
                    let ranges' = hcat (mapM (\n' -> brackets (int 0 <> colon <> int (n'-1))) (tail ns))
                    elTy' <+> ranges' <+> d <+> brackets (int 0 <> colon <> int (head ns - 2))
                  _ -> error $ $(curLoc) ++ "impossible"
funDec _ = empty

module_ :: Component -> SystemVerilogM Doc
module_ c =
    "module" <+> text (componentName c) <> tupled ports <> semi <$>
    indent 2 (inputPorts <$> outputPorts <$$> decls (declarations c)) <$$> insts (declarations c) <$>
    "endmodule"
  where
    ports = sequence
          $ [ encodingNote hwty <$> text i | (i,hwty) <- inputs c ] ++
            [ encodingNote hwty <$> text i | (i,hwty) <- hiddenPorts c] ++
            [ encodingNote hwty <$> text i | (i,hwty) <- outputs c]

    inputPorts = case (inputs c ++ hiddenPorts c) of
                   [] -> empty
                   p  -> vcat (punctuate semi (sequence [ "input" <+> sigDecl (text i) ty | (i,ty) <- p ])) <> semi

    outputPorts = case (outputs c) of
                   [] -> empty
                   p  -> vcat (punctuate semi (sequence [ "output" <+> sigDecl (text i) ty | (i,ty) <- p ])) <> semi

verilogType :: HWType -> SystemVerilogM Doc
verilogType t = do
  tyCache %= HashSet.insert t
  case t of
    Product _ _   -> do
      nm <- use modNm
      text (pack nm) <> "_types::" <> tyName t
    Vector _ _ -> do
      nm <- use modNm
      text (pack nm) <> "_types::" <> tyName t
    Signed n      -> "logic signed" <+> brackets (int (n-1) <> colon <> int 0)
    Clock _ _     -> "logic"
    Reset _ _     -> "logic"
    String        -> "string"
    _ -> "logic" <+> brackets (int (typeSize t -1) <> colon <> int 0)

sigDecl :: SystemVerilogM Doc -> HWType -> SystemVerilogM Doc
sigDecl d t = verilogType t <+> d

-- | Convert a Netlist HWType to the root of a Verilog type
verilogTypeMark :: HWType -> SystemVerilogM Doc
verilogTypeMark t = do
  tyCache %= HashSet.insert t
  nm <- use modNm
  let m = tyName t
  case t of
    Product _ _ -> text (pack nm) <> "_types::" <> m
    Vector _ _ -> text (pack nm) <> "_types::" <> m
    _ -> empty

tyName :: HWType -> SystemVerilogM Doc
tyName Bool              = "logic_vector_1"
tyName (Vector n elTy)   = "array_of_" <> int n <> "_" <> tyName elTy
tyName (BitVector n)     = "logic_vector_" <> int n
tyName t@(Index _)       = "logic_vector_" <> int (typeSize t)
tyName (Signed n)        = "signed_" <> int n
tyName (Unsigned n)      = "logic_vector_" <> int n
tyName t@(Sum _ _)       = "logic_vector_" <> int (typeSize t)
tyName t@(Product nm _)  = makeCached t nameCache prodName
  where
    prodName = do
      seen <- use tySeen
      mkId <- mkBasicId
      let nm'  = (mkId . last . Text.splitOn ".") nm
          nm'' = if Text.null nm'
                    then "product"
                    else nm'
          nm3  = if nm'' `elem` seen
                    then go mkId seen (0::Integer) nm''
                    else nm''
      tySeen %= (nm3:)
      text nm3

    go mkId s i n =
      let n' = n `Text.append` Text.pack ('_':show i)
      in  if n' `elem` s
             then go mkId s (i+1) n
             else n'
tyName t@(SP _ _)  = "logic_vector_" <> int (typeSize t)
tyName (Clock _ _) = "logic"
tyName (Reset _ _) = "logic"
tyName t =  error $ $(curLoc) ++ "tyName: " ++ show t

-- | Convert a Netlist HWType to an error VHDL value for that type
verilogTypeErrValue :: HWType -> SystemVerilogM Doc
verilogTypeErrValue (Vector n elTy) = do
  syn <- hdlSyn
  case syn of
    Vivado -> braces (int n <+> braces (int (typeSize elTy) <+> braces "1'bx"))
    _ -> braces (int n <+> braces (verilogTypeErrValue elTy))
verilogTypeErrValue String = "\"ERROR\""
verilogTypeErrValue ty  = braces (int (typeSize ty) <+> braces "1'bx")

decls :: [Declaration] -> SystemVerilogM Doc
decls [] = empty
decls ds = do
    dsDoc <- catMaybes A.<$> mapM decl ds
    case dsDoc of
      [] -> empty
      _  -> punctuate' semi (A.pure dsDoc)

decl :: Declaration -> SystemVerilogM (Maybe Doc)
decl (NetDecl id_ ty) = Just A.<$> sigDecl (text id_) ty

decl _ = return Nothing

insts :: [Declaration] -> SystemVerilogM Doc
insts [] = empty
insts is = indent 2 . vcat . punctuate linebreak . fmap catMaybes $ mapM inst_ is

-- | Turn a Netlist Declaration to a SystemVerilog concurrent block
inst_ :: Declaration -> SystemVerilogM (Maybe Doc)
inst_ (Assignment id_ e) = fmap Just $
  "assign" <+> text id_ <+> equals <+> expr_ False e <> semi

inst_ (CondAssignment id_ _ scrut _ [(Just (BoolLit b), l),(_,r)]) = fmap Just $
    "always_comb begin" <$>
    indent 2 ("if" <> parens (expr_ True scrut) <$>
                (indent 2 $ text id_ <+> equals <+> expr_ False t <> semi) <$>
             "else" <$>
                (indent 2 $ text id_ <+> equals <+> expr_ False f <> semi)) <$>
    "end"
  where
    (t,f) = if b then (l,r) else (r,l)

inst_ (CondAssignment id_ _ scrut scrutTy es) = fmap Just $
    "always_comb begin" <$>
    indent 2 ("case" <> parens (expr_ True scrut) <$>
                (indent 2 $ vcat $ punctuate semi (conds es)) <> semi <$>
              "endcase") <$>
    "end"
  where
    conds :: [(Maybe Literal,Expr)] -> SystemVerilogM [Doc]
    conds []                = return []
    conds [(_,e)]           = ("default" <+> colon <+> text id_ <+> equals <+> expr_ False e) <:> return []
    conds ((Nothing,e):_)   = ("default" <+> colon <+> text id_ <+> equals <+> expr_ False e) <:> return []
    conds ((Just c ,e):es') = (exprLit (Just (scrutTy,conSize scrutTy)) c <+> colon <+> text id_ <+> equals <+> expr_ False e) <:> conds es'

inst_ (InstDecl nm lbl pms) = fmap Just $
    text nm <+> text lbl <$$> pms' <> semi
  where
    pms' = tupled $ sequence [dot <> text i <+> parens (expr_ False e) | (i,_,_,e) <- pms]

inst_ (BlackBoxD _ bs bbCtx) = do
  t <- renderBlackBox bs bbCtx
  fmap Just (string t)

inst_ (NetDecl _ _) = return Nothing

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

expr_ _ (Identifier id_ (Just (Indexed (ty@(Product _ tys),_,fI)))) = do
  id'<- fmap (displayT . renderOneLine) (text id_ <> dot <> tyName ty <> "_sel" <> int fI)
  simpleFromSLV (tys !! fI) id'

expr_ _ (Identifier id_ (Just (Indexed ((Vector _ elTy),1,1)))) = do
  id' <- fmap (displayT . renderOneLine) (text id_ <> brackets (int 0))
  simpleFromSLV elTy id'

expr_ _ (Identifier id_ (Just (Indexed ((Vector n _),1,2)))) = text id_ <> brackets (int 1 <> colon <> int (n-1))

-- This is a HACK for CLaSH.Driver.TopWrapper.mkOutput
-- Vector's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
expr_ _ (Identifier id_ (Just (Indexed ((Vector _ elTy),10,fI)))) = do
  id' <- fmap (displayT . renderOneLine) (text id_ <> brackets (int fI))
  simpleFromSLV elTy id'

expr_ _ (Identifier id_ (Just (DC (ty@(SP _ _),_)))) = text id_ <> brackets (int start <> colon <> int end)
  where
    start = typeSize ty - 1
    end   = typeSize ty - conSize ty

expr_ _ (Identifier id_ (Just _))                      = text id_

expr_ b (DataCon _ (DC (Void, -1)) [e]) =  expr_ b e

expr_ _ (DataCon (Vector 0 _) _ _) =
  error $ $(curLoc) ++ "SystemVerilog: Trying to create a Nil vector."

expr_ _ (DataCon (Vector 1 elTy) _ [e]) = "'" <> braces (toSLV elTy e)

expr_ _ e@(DataCon ty@(Vector _ elTy) _ [e1,e2]) = case vectorChain e of
  Just es -> "'" <> listBraces (mapM (toSLV elTy) es)
  Nothing -> verilogTypeMark ty <> "_cons" <> parens (expr_ False e1 <> comma <+> expr_ False e2)

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
expr_ _ (DataCon (Product _ tys) _ es) = listBraces (zipWithM toSLV tys es)

expr_ _ (BlackBoxE pNm _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.Signed.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLit (Just (Signed (fromInteger n),fromInteger n)) i

expr_ _ (BlackBoxE pNm _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.Unsigned.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLit (Just (Unsigned (fromInteger n),fromInteger n)) i

expr_ _ (BlackBoxE pNm _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.BitVector.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLit (Just (BitVector (fromInteger n),fromInteger n)) i

expr_ _ (BlackBoxE pNm _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.Index.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLit (Just (Index (fromInteger n),fromInteger n)) i

expr_ b (BlackBoxE _ bs bbCtx b') = do
  t <- renderBlackBox bs bbCtx
  parenIf (b || b') $ string t

expr_ _ (DataTag Bool (Left id_))          = text id_ <> brackets (int 0)
expr_ _ (DataTag Bool (Right id_))         = do
  iw <- use intWidth
  "$signed" <> parens (listBraces (sequence [braces (int (iw-1) <+> braces "1'b0"),text id_]))

expr_ _ (DataTag (Sum _ _) (Left id_))     = "$unsigned" <> parens (text id_)
expr_ _ (DataTag (Sum _ _) (Right id_))    = "$signed" <> parens (text id_)

expr_ _ (DataTag (Product _ _) (Right _))  = do
  iw <- use intWidth
  int iw <> "'sd0"

expr_ _ (DataTag hty@(SP _ _) (Right id_)) = "$signed" <> parens
                                               (text id_ <> brackets
                                               (int start <> colon <> int end))
  where
    start = typeSize hty - 1
    end   = typeSize hty - conSize hty

expr_ _ (DataTag (Vector 0 _) (Right _)) = do
  iw <- use intWidth
  int iw <> "'sd0"
expr_ _ (DataTag (Vector _ _) (Right _)) = do
  iw <- use intWidth
  int iw <> "'sd1"

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
exprLit Nothing (NumLit i) = integer i

exprLit (Just (hty,sz)) (NumLit i) = case hty of
  Unsigned _ -> int sz <> "'d" <> integer i
  Index _ -> int (typeSize hty) <> "'d" <> integer i
  Signed _
   | i < 0     -> "-" <> int sz <> "'sd" <> integer (abs i)
   | otherwise -> int sz <> "'sd" <> integer i
  _ -> int sz <> "'b" <> blit
  where
    blit = bits (toBits sz i)
exprLit _             (BoolLit t)   = if t then "1'b1" else "1'b0"
exprLit _             (BitLit b)    = "1'b" <> bit_char b
exprLit _             (StringLit s) = text . pack $ show s
exprLit _             l             = error $ $(curLoc) ++ "exprLit: " ++ show l

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
bit_char U = char 'x'
bit_char Z = char 'z'

toSLV :: HWType -> Expr -> SystemVerilogM Doc
toSLV t e = case t of
  Vector _ _ -> verilogTypeMark t <> "_to_lv" <> parens (expr_ False e)
  _ -> expr_ False e

fromSLV :: HWType -> Identifier -> Int -> Int -> SystemVerilogM Doc
fromSLV t@(Vector _ _) id_ start end = verilogTypeMark t <> "_from_lv" <> parens (text id_ <> brackets (int start <> colon <> int end))
fromSLV (Signed _) id_ start end = "$signed" <> parens (text id_ <> brackets (int start <> colon <> int end))
fromSLV _ id_ start end = text id_ <> brackets (int start <> colon <> int end)

simpleFromSLV :: HWType -> Identifier -> SystemVerilogM Doc
simpleFromSLV t@(Vector _ _) id_ = verilogTypeMark t <> "_from_lv" <> parens (text id_)
simpleFromSLV (Signed _) id_ = "$signed" <> parens (text id_)
simpleFromSLV _ id_ = text id_

dcToExpr :: HWType -> Int -> Expr
dcToExpr ty i = Literal (Just (ty,conSize ty)) (NumLit (toInteger i))

listBraces :: Monad m => m [Doc] -> m Doc
listBraces = encloseSep lbrace rbrace comma

parenIf :: Monad m => Bool -> m Doc -> m Doc
parenIf True  = parens
parenIf False = id

punctuate' :: Monad m => m Doc -> m [Doc] -> m Doc
punctuate' s d = vcat (punctuate s d) <> s

encodingNote :: HWType -> SystemVerilogM Doc
encodingNote (Clock _ _) = "// clock"
encodingNote (Reset _ _) = "// asynchronous reset: active low"
encodingNote _           = empty
