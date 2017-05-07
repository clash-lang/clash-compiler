{-|
  Copyright   :  (C) 2015-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Generate Verilog for assorted Netlist datatypes
-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module CLaSH.Backend.Verilog (VerilogState) where

import qualified Control.Applicative                  as A
import           Control.Lens                         ((+=),(-=),(.=),(%=), makeLenses, use)
import           Control.Monad.State                  (State)
import           Data.Hashable                        (Hashable (..))
import qualified Data.HashSet                         as HashSet
import           Data.Maybe                           (catMaybes,mapMaybe)
import           Data.Text.Lazy                       (pack, unpack)
import qualified Data.Text.Lazy                       as Text
import           Prelude                              hiding ((<$>))
import qualified System.FilePath
import           Text.Printf
import           Text.PrettyPrint.Leijen.Text.Monadic

import           CLaSH.Annotations.Primitive          (HDL (..))
import           CLaSH.Backend
import           CLaSH.Driver.Types                   (SrcSpan, noSrcSpan)
import           CLaSH.Netlist.BlackBox.Types         (HdlSyn)
import           CLaSH.Netlist.BlackBox.Util          (extractLiterals, renderBlackBox)
import           CLaSH.Netlist.Id                     (mkBasicId')
import           CLaSH.Netlist.Types                  hiding (_intWidth, intWidth)
import           CLaSH.Netlist.Util                   hiding (mkBasicId)
import           CLaSH.Util                           (curLoc, (<:>))

#ifdef CABAL
import qualified Paths_clash_lib
#endif

-- | State for the 'CLaSH.Backend.Verilog.VerilogM' monad:
data VerilogState =
  VerilogState
    { _genDepth  :: Int -- ^ Depth of current generative block
    , _idSeen    :: [Identifier]
    , _srcSpan   :: SrcSpan
    , _includes  :: [(String,Doc)]
    , _intWidth  :: Int -- ^ Int/Word/Integer bit-width
    , _hdlsyn    :: HdlSyn
    }

makeLenses ''VerilogState

instance Backend VerilogState where
  initBackend     = VerilogState 0 [] noSrcSpan []
  hdlKind         = const Verilog
#ifdef CABAL
  primDir         = const (Paths_clash_lib.getDataFileName ("prims" System.FilePath.</> "verilog"))
#else
  primDir _       = return ("clash-lib" System.FilePath.</> "prims" System.FilePath.</> "verilog")
#endif
  extractTypes    = const HashSet.empty
  name            = const "verilog"
  extension       = const ".v"

  genHDL          = const genVerilog
  mkTyPackage _ _ = return []
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
  toBV _          = text
  fromBV _        = text
  hdlSyn          = use hdlsyn
  mkBasicId       = return (filterReserved . mkBasicId' True)
  setModName _    = id
  setSrcSpan      = (srcSpan .=)
  getSrcSpan      = use srcSpan

type VerilogM a = State VerilogState a

-- List of reserved Verilog-2005 keywords
reservedWords :: [Identifier]
reservedWords = ["always","and","assign","automatic","begin","buf","bufif0"
  ,"bufif1","case","casex","casez","cell","cmos","config","deassign","default"
  ,"defparam","design","disable","edge","else","end","endcase","endconfig"
  ,"endfunction","endgenerate","endmodule","endprimitive","endspecify"
  ,"endtable","endtask","event","for","force","forever","fork","function"
  ,"generate","genvar","highz0","highz1","if","ifnone","incdir","include"
  ,"initial","inout","input","instance","integer","join","large","liblist"
  ,"library","localparam","macromodule","medium","module","nand","negedge"
  ,"nmos","nor","noshowcancelled","not","notif0","notif1","or","output"
  ,"parameter","pmos","posedge","primitive","pull0","pull1","pulldown","pullup"
  ,"pulsestyle_onevent","pulsestyle_ondetect","rcmos","real","realtime","reg"
  ,"release","repeat","rnmos","rpmos","rtran","rtranif0","rtranif1","scalared"
  ,"showcancelled","signed","small","specify","specparam","strong0","strong1"
  ,"supply0","supply1","table","task","time","tran","tranif0","tranif1","tri"
  ,"tri0","tri1","triand","trior","trireg","unsigned","use","uwire","vectored"
  ,"wait","wand","weak0","weak1","while","wire","wor","xnor","xor"]

filterReserved :: Identifier -> Identifier
filterReserved s = if s `elem` reservedWords
  then s `Text.append` "_r"
  else s

-- | Generate VHDL for a Netlist component
genVerilog :: SrcSpan -> Component -> VerilogM ((String,Doc),[(String,Doc)])
genVerilog sp c = do
    setSrcSpan sp
    v    <- verilog
    incs <- use includes
    return ((unpack cName,v),incs)
  where
    cName   = componentName c
    verilog = "// Automatically generated Verilog-2001" <$$>
              module_ c

module_ :: Component -> VerilogM Doc
module_ c = do
    { addSeen c
    ; m <- "module" <+> text (componentName c) <> tupled ports <> semi <$>
           indent 2 (inputPorts <$> outputPorts <$$> decls (declarations c)) <$$> insts (declarations c) <$>
           "endmodule"
    ; idSeen .= []
    ; return m
    }
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

addSeen :: Component -> VerilogM ()
addSeen c = do
  let iport = map fst $ inputs c
      hport = map fst $ hiddenPorts c
      oport = map fst $ outputs c
      nets  = mapMaybe (\case {NetDecl i _ -> Just i; _ -> Nothing}) $ declarations c
  idSeen .= concat [iport,hport,oport,nets]

mkUniqueId :: Identifier -> VerilogM Identifier
mkUniqueId i = do
  mkId <- mkBasicId
  seen <- use idSeen
  let i' = mkId i
  case i `elem` seen of
    True  -> go mkId seen i' 0
    False -> do idSeen %= (i':)
                return i'
  where
    go :: (Identifier -> Identifier) -> [Identifier] -> Identifier
       -> Int -> VerilogM Identifier
    go mkId seen i' n = do
      let i'' = mkId (Text.append i' (Text.pack ('_':show n)))
      case i'' `elem` seen of
        True  -> go mkId seen i' (n+1)
        False -> do idSeen %= (i'':)
                    return i''

verilogType :: HWType -> VerilogM Doc
verilogType t = case t of
  Signed n -> "signed" <+> brackets (int (n-1) <> colon <> int 0)
  Clock {} -> empty
  Reset {} -> empty
  _        -> brackets (int (typeSize t -1) <> colon <> int 0)

sigDecl :: VerilogM Doc -> HWType -> VerilogM Doc
sigDecl d t = verilogType t <+> d

-- | Convert a Netlist HWType to the root of a Verilog type
verilogTypeMark :: HWType -> VerilogM Doc
verilogTypeMark = const empty

-- | Convert a Netlist HWType to an error VHDL value for that type
verilogTypeErrValue :: HWType -> VerilogM Doc
verilogTypeErrValue ty = braces (int (typeSize ty) <+> braces "1'bx")

decls :: [Declaration] -> VerilogM Doc
decls [] = empty
decls ds = do
    dsDoc <- catMaybes A.<$> mapM decl ds
    case dsDoc of
      [] -> empty
      _  -> punctuate' semi (A.pure dsDoc)

decl :: Declaration -> VerilogM (Maybe Doc)
decl (NetDecl id_ ty) = Just A.<$> "wire" <+> sigDecl (text id_) ty

decl _ = return Nothing

insts :: [Declaration] -> VerilogM Doc
insts [] = empty
insts is = indent 2 . vcat . punctuate linebreak . fmap catMaybes $ mapM inst_ is

-- | Turn a Netlist Declaration to a SystemVerilog concurrent block
inst_ :: Declaration -> VerilogM (Maybe Doc)
inst_ (Assignment id_ e) = fmap Just $
  "assign" <+> text id_ <+> equals <+> expr_ False e <> semi

inst_ (CondAssignment id_ ty scrut _ [(Just (BoolLit b), l),(_,r)]) = fmap Just $ do
    { regId <- mkUniqueId (Text.append id_ "_reg")
    ; "reg" <+> verilogType ty <+> text regId <> semi <$>
      "always @(*) begin" <$>
      indent 2 ("if" <> parens (expr_ True scrut) <$>
                  (indent 2 $ text regId <+> equals <+> expr_ False t <> semi) <$>
               "else" <$>
                  (indent 2 $ text regId <+> equals <+> expr_ False f <> semi)) <$>
      "end" <$>
      "assign" <+> text id_ <+> equals <+> text regId <> semi
    }
  where
    (t,f) = if b then (l,r) else (r,l)


inst_ (CondAssignment id_ ty scrut scrutTy es) = fmap Just $ do
    { regId <- mkUniqueId (Text.append id_ "_reg")
    ; "reg" <+> verilogType ty <+> text regId <> semi <$>
      "always @(*) begin" <$>
      indent 2 ("case" <> parens (expr_ True scrut) <$>
                  (indent 2 $ vcat $ punctuate semi (conds regId es)) <> semi <$>
                "endcase") <$>
      "end" <$>
      "assign" <+> text id_ <+> equals <+> text regId <> semi
    }
  where
    conds :: Identifier -> [(Maybe Literal,Expr)] -> VerilogM [Doc]
    conds _ []                = return []
    conds i [(_,e)]           = ("default" <+> colon <+> text i <+> equals <+> expr_ False e) <:> return []
    conds i ((Nothing,e):_)   = ("default" <+> colon <+> text i <+> equals <+> expr_ False e) <:> return []
    conds i ((Just c ,e):es') = (exprLit (Just (scrutTy,conSize scrutTy)) c <+> colon <+> text i <+> equals <+> expr_ False e) <:> conds i es'

inst_ (InstDecl nm lbl pms) = fmap Just $
    text nm <+> text lbl <$$> pms' <> semi
  where
    pms' = tupled $ sequence [dot <> text i <+> parens (expr_ False e) | (i,_,_,e) <- pms]

inst_ (BlackBoxD _ _ _ Nothing bs bbCtx) = do
  t <- renderBlackBox bs bbCtx
  fmap Just (string t)

inst_ (BlackBoxD _ _ _ (Just (nm,inc)) bs bbCtx) = do
  inc' <- renderBlackBox inc bbCtx
  iw <- use intWidth
  let incHash = hash inc'
      nm'     = Text.concat [ Text.fromStrict nm
                            , Text.pack (printf ("%0" ++ show (iw `div` 4) ++ "X") incHash)
                            ]
  t <- renderBlackBox bs (bbCtx {bbQsysIncName = Just nm'})
  inc'' <- text inc'
  includes %= ((unpack nm', inc''):)
  fmap Just (string t)

inst_ (NetDecl _ _) = return Nothing

-- | Turn a Netlist expression into a SystemVerilog expression
expr_ :: Bool -- ^ Enclose in parenthesis?
      -> Expr -- ^ Expr to convert
      -> VerilogM Doc
expr_ _ (Literal sizeM lit) = exprLit sizeM lit

expr_ _ (Identifier id_ Nothing) = text id_

expr_ _ (Identifier id_ (Just (Indexed (ty@(SP _ args),dcI,fI)))) =
    text id_ <> brackets (int start <> colon <> int end)
  where
    argTys   = snd $ args !! dcI
    argTy    = argTys !! fI
    argSize  = typeSize argTy
    other    = otherSize argTys (fI-1)
    start    = typeSize ty - 1 - conSize ty - other
    end      = start - argSize + 1

expr_ _ (Identifier id_ (Just (Indexed (ty@(Product _ argTys),_,fI)))) =
    text id_ <> brackets (int start <> colon <> int end)
  where
    argTy   = argTys !! fI
    argSize = typeSize argTy
    otherSz = otherSize argTys (fI - 1)
    start   = typeSize ty - 1 - otherSz
    end     = start - argSize + 1

expr_ _ (Identifier id_ (Just (Indexed (ty@(Vector _ argTy),1,1)))) =
    text id_ <> brackets (int start <> colon <> int end)
  where
    argSize = typeSize argTy
    start   = typeSize ty - 1
    end     = start - argSize + 1

expr_ _ (Identifier id_ (Just (Indexed (ty@(Vector _ argTy),1,2)))) =
    text id_ <> brackets (int start <> colon <> int 0)
  where
    argSize = typeSize argTy
    start   = typeSize ty - argSize - 1

expr_ _ (Identifier id_ (Just (Indexed ((RTree 0 _),0,1)))) = text id_

expr_ _ (Identifier id_ (Just (Indexed (ty@(RTree _ _),1,1)))) =
    text id_ <> brackets (int start <> colon <> int end)
  where
    start   = typeSize ty - 1
    end     = typeSize ty `div` 2

expr_ _ (Identifier id_ (Just (Indexed (ty@(RTree _ _),1,2)))) =
    text id_ <> brackets (int start <> colon <> int 0)
  where
    start   = (typeSize ty `div` 2) - 1

-- This is a HACK for CLaSH.Driver.TopWrapper.mkOutput
-- Vector's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
expr_ _ (Identifier id_ (Just (Indexed (ty@(Vector _ argTy),10,fI)))) =
    text id_ <> brackets (int start <> colon <> int end)
  where
    argSize = typeSize argTy
    start   = typeSize ty - (fI * argSize) - 1
    end     = start - argSize + 1

-- This is a HACK for CLaSH.Driver.TopWrapper.mkOutput
-- RTree's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
expr_ _ (Identifier id_ (Just (Indexed (ty@(RTree _ argTy),10,fI)))) =
    text id_ <> brackets (int start <> colon <> int end)
  where
    argSize = typeSize argTy
    start   = typeSize ty - (fI * argSize) - 1
    end     = start - argSize + 1

expr_ _ (Identifier id_ (Just (DC (ty@(SP _ _),_)))) = text id_ <> brackets (int start <> colon <> int end)
  where
    start = typeSize ty - 1
    end   = typeSize ty - conSize ty

expr_ _ (Identifier id_ (Just _))                      = text id_

expr_ b (DataCon _ (DC (Void, -1)) [e]) = expr_ b e

expr_ _ (DataCon ty@(Vector 0 _) _ _) = verilogTypeErrValue ty

expr_ _ (DataCon (Vector 1 _) _ [e]) = expr_ False e
expr_ _ e@(DataCon (Vector _ _) _ es@[_,_]) =
  case vectorChain e of
    Just es' -> listBraces (mapM (expr_ False) es')
    Nothing  -> listBraces (mapM (expr_ False) es)

expr_ _ (DataCon (RTree 0 _) _ [e]) = expr_ False e
expr_ _ e@(DataCon (RTree _ _) _ es@[_,_]) =
  case rtreeChain e of
    Just es' -> listBraces (mapM (expr_ False) es')
    Nothing  -> listBraces (mapM (expr_ False) es)

expr_ _ (DataCon ty@(SP _ args) (DC (_,i)) es) = assignExpr
  where
    argTys     = snd $ args !! i
    dcSize     = conSize ty + sum (map typeSize argTys)
    dcExpr     = expr_ False (dcToExpr ty i)
    argExprs   = map (expr_ False) es
    extraArg   = case typeSize ty - dcSize of
                   0 -> []
                   n -> [int n <> "'b" <> bits (replicate n U)]
    assignExpr = braces (hcat $ punctuate comma $ sequence (dcExpr:argExprs ++ extraArg))

expr_ _ (DataCon ty@(Sum _ _) (DC (_,i)) []) = int (typeSize ty) <> "'d" <> int i

expr_ _ (DataCon (Product _ _) _ es) = listBraces (mapM (expr_ False) es)

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.Signed.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLit (Just (Signed (fromInteger n),fromInteger n)) i

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.Unsigned.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLit (Just (Unsigned (fromInteger n),fromInteger n)) i

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.BitVector.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLit (Just (BitVector (fromInteger n),fromInteger n)) i

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "CLaSH.Sized.Internal.Index.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLit (Just (Index (fromInteger n),fromInteger n)) i

expr_ b (BlackBoxE _ _ _ Nothing bs bbCtx b') = do
  t <- renderBlackBox bs bbCtx
  parenIf (b || b') $ string t

expr_ b (BlackBoxE _ _ _ (Just (nm,inc)) bs bbCtx b') = do
  inc' <- renderBlackBox inc bbCtx
  iw <- use intWidth
  let incHash = hash inc'
      nm'     = Text.concat [ Text.fromStrict nm
                            , Text.pack (printf ("%0" ++ show (iw `div` 4) ++ "X") incHash)
                            ]
  t <- renderBlackBox bs (bbCtx {bbQsysIncName = Just nm'})
  inc'' <- text inc'
  includes %= ((unpack nm', inc''):)
  parenIf (b || b') $ string t

expr_ _ (DataTag Bool (Left id_))          = text id_ <> brackets (int 0)
expr_ _ (DataTag Bool (Right id_))         = do
  iw <- use intWidth
  "$unsigned" <> parens (listBraces (sequence [braces (int (iw-1) <+> braces "1'b0"),text id_]))

expr_ _ (DataTag (Sum _ _) (Left id_))     = "$unsigned" <> parens (text id_)
expr_ _ (DataTag (Sum _ _) (Right id_))    = "$unsigned" <> parens (text id_)

expr_ _ (DataTag (Product _ _) (Right _))  = do
  iw <- use intWidth
  int iw <> "'sd0"

expr_ _ (DataTag hty@(SP _ _) (Right id_)) = "$unsigned" <> parens
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

expr_ _ (DataTag (RTree 0 _) (Right _)) = do
  iw <- use intWidth
  int iw <> "'sd0"
expr_ _ (DataTag (RTree _ _) (Right _)) = do
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

rtreeChain :: Expr -> Maybe [Expr]
rtreeChain (DataCon (RTree 0 _) _ [e])     = Just [e]
rtreeChain (DataCon (RTree _ _) _ [e1,e2]) = Just e1 <:> rtreeChain e2
rtreeChain _                               = Nothing

exprLit :: Maybe (HWType,Size) -> Literal -> VerilogM Doc
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

bits :: [Bit] -> VerilogM Doc
bits = hcat . mapM bit_char

bit_char :: Bit -> VerilogM Doc
bit_char H = char '1'
bit_char L = char '0'
bit_char U = char 'x'
bit_char Z = char 'z'

dcToExpr :: HWType -> Int -> Expr
dcToExpr ty i = Literal (Just (ty,conSize ty)) (NumLit (toInteger i))

listBraces :: Monad m => m [Doc] -> m Doc
listBraces = encloseSep lbrace rbrace comma

parenIf :: Monad m => Bool -> m Doc -> m Doc
parenIf True  = parens
parenIf False = id

punctuate' :: Monad m => m Doc -> m [Doc] -> m Doc
punctuate' s d = vcat (punctuate s d) <> s

encodingNote :: HWType -> VerilogM Doc
encodingNote (Clock _ _) = "// clock"
encodingNote (Reset _ _) = "// asynchronous reset: active low"
encodingNote _           = empty
