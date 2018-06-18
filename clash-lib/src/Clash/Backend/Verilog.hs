{-|
  Copyright   :  (C) 2015-2016, University of Twente,
                          2017, Google Inc.
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

module Clash.Backend.Verilog (VerilogState, include) where

import           Control.Applicative                  ((<*), (*>))
import qualified Control.Applicative                  as A
import           Control.Lens                         ((+=),(-=),(.=),(%=), makeLenses, use)
import           Control.Monad                        (forM)
import           Control.Monad.State                  (State)
import qualified Data.HashSet                         as HashSet
import           Data.Maybe                           (catMaybes,fromMaybe,mapMaybe)
import           Data.List                            (nub)
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid                          hiding (Product, Sum)
#endif
import           Data.Semigroup.Monad
import           Data.Text.Lazy                       (pack, unpack)
import qualified Data.Text.Lazy                       as Text
import           Data.Text.Prettyprint.Doc.Extra
#ifdef CABAL
import qualified Data.Version
#endif
import qualified System.FilePath

import           Clash.Annotations.Primitive          (HDL (..))
import           Clash.Backend
import           Clash.Driver.Types                   (SrcSpan, noSrcSpan)
import           Clash.Netlist.BlackBox.Types         (HdlSyn)
import           Clash.Netlist.BlackBox.Util          (extractLiterals, renderBlackBox)
import           Clash.Netlist.Id                     (IdType (..), mkBasicId')
import           Clash.Netlist.Types                  hiding (_intWidth, intWidth)
import           Clash.Netlist.Util                   hiding (mkIdentifier, extendIdentifier)
import           Clash.Signal.Internal                (ClockKind (..))
import           Clash.Util                           (curLoc, (<:>))

#ifdef CABAL
import qualified Paths_clash_lib
#endif

-- | State for the 'Clash.Backend.Verilog.VerilogM' monad:
data VerilogState =
  VerilogState
    { _genDepth  :: Int -- ^ Depth of current generative block
    , _idSeen    :: [Identifier]
    , _srcSpan   :: SrcSpan
    , _includes  :: [(String,Doc)]
    , _imports   :: [Text.Text]
    , _intWidth  :: Int -- ^ Int/Word/Integer bit-width
    , _hdlsyn    :: HdlSyn
    }

makeLenses ''VerilogState

primsRoot :: IO FilePath
#ifdef CABAL
primsRoot = Paths_clash_lib.getDataFileName "prims"
#else
primsRoot = return ("clash-lib" System.FilePath.</> "prims")
#endif

instance Backend VerilogState where
  initBackend     = VerilogState 0 [] noSrcSpan [] []
  hdlKind         = const Verilog
  primDirs        = const $ do root <- primsRoot
                               return [ root System.FilePath.</> "common"
                                      , root System.FilePath.</> "commonverilog"
                                      , root System.FilePath.</> "verilog"
                                      ]
  extractTypes    = const HashSet.empty
  name            = const "verilog"
  extension       = const ".v"

  genHDL          = const genVerilog
  mkTyPackage _ _ = return []
  hdlType _       = verilogType
  hdlTypeErrValue = verilogTypeErrValue
  hdlTypeMark     = verilogTypeMark
  hdlRecSel       = verilogRecSel
  hdlSig t ty     = sigDecl (string t) ty
  genStmt True    = do cnt <- use genDepth
                       genDepth += 1
                       if cnt > 0
                          then emptyDoc
                          else "generate"
  genStmt False   = do genDepth -= 1
                       cnt <- use genDepth
                       if cnt > 0
                          then emptyDoc
                          else "endgenerate"
  inst            = inst_
  expr            = expr_
  iwWidth         = use intWidth
  toBV _          = string
  fromBV _        = string
  hdlSyn          = use hdlsyn
  mkIdentifier    = return go
    where
      go Basic    nm = filterReserved (mkBasicId' True nm)
      go Extended (rmSlash -> nm) = case go Basic nm of
        nm' | nm /= nm' -> Text.concat ["\\",nm," "]
            |otherwise  -> nm'
  extendIdentifier = return go
    where
      go Basic nm ext = filterReserved (mkBasicId' True (nm `Text.append` ext))
      go Extended (rmSlash . escapeTemplate -> nm) ext =
        let nmExt = nm `Text.append` ext
        in  case go Basic nm ext of
              nm' | nm' /= nmExt -> case Text.head nmExt of
                      '#' -> Text.concat ["\\",nmExt," "]
                      _   -> Text.concat ["\\#",nmExt," "]
                  | otherwise    -> nm'

  setModName _    = id
  setSrcSpan      = (srcSpan .=)
  getSrcSpan      = use srcSpan
  blockDecl _ ds  = do
    decs <- decls ds
    if isEmpty decs
      then insts ds
      else
        pure decs <> line <>
        insts ds
  unextend = return rmSlash
  addIncludes inc = includes %= (inc++)
  addLibraries _ = return ()
  addImports inps = imports %= (inps ++)

rmSlash :: Identifier -> Identifier
rmSlash nm = fromMaybe nm $ do
  nm1 <- Text.stripPrefix "\\" nm
  pure (Text.filter (not . (== ' ')) nm1)

type VerilogM a = Mon (State VerilogState) a

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
    Mon (setSrcSpan sp)
    v    <- commentHeader <> line <> module_ c
    incs <- Mon $ use includes
    return ((unpack cName,v),incs)
  where
#ifdef CABAL
    clashVer = Data.Version.showVersion Paths_clash_lib.version
#else
    clashVer = "development"
#endif
    cName    = componentName c
    commentHeader
         = "/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE."
      <> line <> "** GENERATED BY CLASH " <> string (Text.pack clashVer) <> ". DO NOT MODIFY."
      <> line <> "*/"

module_ :: Component -> VerilogM Doc
module_ c = addSeen c *> modVerilog <* Mon (idSeen .= [] >> imports .= [])
  where
    modVerilog = do
      body <- modBody
      imps <- Mon $ use imports
      modHeader <> line <> modPorts <> line <> include (nub imps) <> pure body <> line <> modEnding

    modHeader  = "module" <+> string (componentName c)
    modPorts   = indent 4 (tupleInputs inPorts <> line <> tupleOutputs outPorts <> semi)
    modBody    = indent 2 (decls (declarations c)) <> line <> line <> insts (declarations c)
    modEnding  = "endmodule"

    inPorts  = sequence [ sigPort Nothing   p | p       <- inputs c  ]
    outPorts = sequence [ sigPort (Just wr) p | (wr, p) <- outputs c ]

    wr2ty Nothing     = "input"
    wr2ty (Just Wire) = "output" <+> "wire"
    wr2ty (Just Reg)  = "output" <+> "reg"

    -- map a port to its verilog type, port name, and any encoding notes
    sigPort (wr2ty -> portTy) (nm, hwTy)
      = portTy <+> verilogType' True hwTy <+> string nm <+> encodingNote hwTy

    -- slightly more readable than 'tupled', makes the output Haskell-y-er
    commafy v = (comma <> space) <> pure v

    tupleInputs v = v >>= \case
      []     -> lparen <+> string "// No inputs" <> line
      (x:xs) -> lparen <+> string "// Inputs"
                      <> line <> (string "  " <> pure x)
                      <> line <> vcat (forM xs commafy)
                      <> line

    tupleOutputs v = v >>= \case
      []     -> string "  // No outputs" <> line <> rparen
      (x:xs) -> string "  // Outputs"
                  <> line <> (if (length (inputs c)) > 0
                         then comma <> space <> pure x
                         else string "  " <> pure x)
                  <> (if null xs then emptyDoc else line <> vcat (forM xs commafy))
                  <> line <> rparen

include :: Monad m => [Text.Text] -> Mon m Doc
include [] = emptyDoc
include xs = line <>
  indent 2 (vcat (mapM (\i -> string "`include" <+> dquotes (string i)) xs))
  <> line <> line

wireOrReg :: WireOrReg -> VerilogM Doc
wireOrReg Wire = "wire"
wireOrReg Reg  = "reg"

addSeen :: Component -> VerilogM ()
addSeen c = do
  let iport = map fst $ inputs c
      oport = map (fst.snd) $ outputs c
      nets  = mapMaybe (\case {NetDecl' _ _ i _ -> Just i; _ -> Nothing}) $ declarations c
  Mon $ idSeen .= concat [iport,oport,nets]

-- render a type; by default, removing zero-sizes is an aesthetic operation
-- and is only valid for decls (e.g. when rendering module ports), so don't
-- do it by default to be safe
verilogType :: HWType -> VerilogM Doc
verilogType = verilogType' False

verilogType' :: Bool -> HWType -> VerilogM Doc
verilogType' isDecl t =
  let -- if the size is zero, it's single bit, so if we're
      -- emitting a decl, then we can skip it - but we can't
      -- skip it when selecting other values (e.g a slice)
      renderVerilogTySize l
        | l == 0 && isDecl = emptyDoc
        | otherwise        = brackets (int l <> colon <> int 0)

      -- signed types have to be rendered specially
      getVerilogTy (Signed n) = ("signed" <> space, n)
      getVerilogTy _          = (emptyDoc,    typeSize t)

  in case t of
       -- special case: Bit, Bool, clocks and resets
       Clock _ _ Gated -> verilogType' isDecl (gatedClockType t)
       Clock {} -> emptyDoc
       Reset {} -> emptyDoc
       Bit      -> emptyDoc
       Bool     -> emptyDoc

       -- otherwise, print the type and prefix
       ty | (prefix, sz) <- getVerilogTy ty
         -> prefix <> renderVerilogTySize (sz-1)

gatedClockType :: HWType -> HWType
gatedClockType (Clock _ _ Gated) = Product "GatedClock" [Bit,Bool]
gatedClockType ty = ty
{-# INLINE gatedClockType #-}

sigDecl :: VerilogM Doc -> HWType -> VerilogM Doc
sigDecl d t = verilogType t <+> d

-- | Convert a Netlist HWType to the root of a Verilog type
verilogTypeMark :: HWType -> VerilogM Doc
verilogTypeMark = const emptyDoc

-- | Convert a Netlist HWType to an error VHDL value for that type
verilogTypeErrValue :: HWType -> VerilogM Doc
verilogTypeErrValue ty = braces (int (typeSize ty) <+> braces "1'bx")

verilogRecSel
  :: HWType
  -> Int
  -> VerilogM Doc
verilogRecSel ty i = case modifier 0 (Indexed (ty,0,i)) of
  Just (start,end) -> brackets (int start <> colon <> int end)
  _ -> error "Can't make a record selector"

decls :: [Declaration] -> VerilogM Doc
decls [] = emptyDoc
decls ds = do
    dsDoc <- catMaybes <$> (mapM decl ds)
    case dsDoc of
      [] -> emptyDoc
      _  -> punctuate' semi (A.pure dsDoc)

decl :: Declaration -> VerilogM (Maybe Doc)
decl (NetDecl' noteM wr id_ tyE) =
  Just <$> maybe id addNote noteM (wireOrReg wr <+> tyDec tyE)
  where
    tyDec (Left  ty) = string ty <+> string id_
    tyDec (Right ty) = sigDecl (string id_) ty
    addNote n = mappend ("//" <+> string n <> line)

decl _ = return Nothing

insts :: [Declaration] -> VerilogM Doc
insts [] = emptyDoc
insts is = indent 2 . vcat . punctuate line . fmap catMaybes $ mapM inst_ is

-- | Turn a Netlist Declaration to a SystemVerilog concurrent block
inst_ :: Declaration -> VerilogM (Maybe Doc)
inst_ (Assignment id_ e) = fmap Just $
  "assign" <+> string id_ <+> equals <+> expr_ False e <> semi

inst_ (CondAssignment id_ _ scrut _ [(Just (BoolLit b), l),(_,r)]) = fmap Just $
   "always @(*) begin" <> line <>
   indent 2 ("if" <> parens (expr_ True scrut) <> line <>
               (indent 2 $ string id_ <+> equals <+> expr_ False t <> semi) <> line <>
            "else" <> line <>
               (indent 2 $ string id_ <+> equals <+> expr_ False f <> semi)) <> line <>
   "end"
  where
    (t,f) = if b then (l,r) else (r,l)


inst_ (CondAssignment id_ _ scrut scrutTy es) = fmap Just $
    "always @(*) begin" <> line <>
    indent 2 ("case" <> parens (expr_ True scrut) <> line <>
                (indent 2 $ vcat $ punctuate semi (conds id_ es)) <> semi <> line <>
              "endcase") <> line <>
    "end"
  where
    conds :: Identifier -> [(Maybe Literal,Expr)] -> VerilogM [Doc]
    conds _ []                = return []
    conds i [(_,e)]           = ("default" <+> colon <+> string i <+> equals <+> expr_ False e) <:> return []
    conds i ((Nothing,e):_)   = ("default" <+> colon <+> string i <+> equals <+> expr_ False e) <:> return []
    conds i ((Just c ,e):es') = (exprLit (Just (scrutTy,conSize scrutTy)) c <+> colon <+> string i <+> equals <+> expr_ False e) <:> conds i es'

inst_ (InstDecl _ nm lbl pms) = fmap Just $
    nest 2 (string nm <+> string lbl <> line <> pms' <> semi)
  where
    pms' = tupled $ sequence [dot <> expr_ False i <+> parens (expr_ False e) | (i,_,_,e) <- pms]

inst_ (BlackBoxD _ libs imps inc bs bbCtx) =
  fmap Just (Mon (column (renderBlackBox libs imps inc bs bbCtx)))

inst_ (NetDecl' _ _ _ _) = return Nothing

-- | Calculate the beginning and end index into a variable, to get the
-- desired field.
modifier
  :: Int
  -- ^ Offset, only used when we have nested modifiers
  -> Modifier
  -> Maybe (Int,Int)
modifier offset (Indexed (ty@(SP _ args),dcI,fI)) = Just (start+offset,end+offset)
  where
    argTys   = snd $ args !! dcI
    argTy    = argTys !! fI
    argSize  = typeSize argTy
    other    = otherSize argTys (fI-1)
    start    = typeSize ty - 1 - conSize ty - other
    end      = start - argSize + 1

modifier offset (Indexed (ty@(Product _ argTys),_,fI)) = Just (start+offset,end+offset)
  where
    argTy   = argTys !! fI
    argSize = typeSize argTy
    otherSz = otherSize argTys (fI - 1)
    start   = typeSize ty - 1 - otherSz
    end     = start - argSize + 1

modifier offset (Indexed (ty@(Clock _ _ Gated),_,fI)) = Just (start+offset,end+offset)
  where
    argTys  = [Bit, Bool]
    argTy   = argTys !! fI
    argSize = typeSize argTy
    otherSz = otherSize argTys (fI - 1)
    start   = typeSize ty - 1 - otherSz
    end     = start - argSize + 1

modifier offset (Indexed (ty@(Vector _ argTy),1,0)) = Just (start+offset,end+offset)
  where
    argSize = typeSize argTy
    start   = typeSize ty - 1
    end     = start - argSize + 1

modifier offset (Indexed (ty@(Vector _ argTy),1,1)) = Just (start+offset,offset)
  where
    argSize = typeSize argTy
    start   = typeSize ty - argSize - 1

modifier offset (Indexed (ty@(RTree 0 _),0,0)) = Just (start+offset,offset)
  where
    start   = typeSize ty - 1

modifier offset (Indexed (ty@(RTree _ _),1,0)) = Just (start+offset,end+offset)
  where
    start   = typeSize ty - 1
    end     = typeSize ty `div` 2

modifier offset (Indexed (ty@(RTree _ _),1,1)) = Just (start+offset,offset)
  where
    start   = (typeSize ty `div` 2) - 1

-- This is a HACK for Clash.Driver.TopWrapper.mkOutput
-- Vector's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
modifier offset (Indexed (ty@(Vector _ argTy),10,fI)) = Just (start+offset,end+offset)
  where
    argSize = typeSize argTy
    start   = typeSize ty - (fI * argSize) - 1
    end     = start - argSize + 1

-- This is a HACK for Clash.Driver.TopWrapper.mkOutput
-- RTree's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
modifier offset (Indexed (ty@(RTree _ argTy),10,fI)) = Just (start+offset,end+offset)
  where
    argSize = typeSize argTy
    start   = typeSize ty - (fI * argSize) - 1
    end     = start - argSize + 1

modifier offset (DC (ty@(SP _ _),_)) = Just (start+offset,end+offset)
  where
    start = typeSize ty - 1
    end   = typeSize ty - conSize ty

modifier offset (Nested m1 m2) = do
  case modifier offset m1 of
    Nothing    -> modifier offset m2
    Just (s,e) -> case modifier e m2 of
      -- In case the second modifier is `Nothing` that means we want the entire
      -- thing calculated by the first modifier
      Nothing -> Just (s,e)
      m       -> m

modifier _ _ = Nothing

-- | Turn a Netlist expression into a SystemVerilog expression
expr_ :: Bool -- ^ Enclose in parenthesis?
      -> Expr -- ^ Expr to convert
      -> VerilogM Doc
expr_ _ (Literal sizeM lit) = exprLit sizeM lit

expr_ _ (Identifier id_ Nothing) = string id_

expr_ _ (Identifier id_ (Just m)) = case modifier 0 m of
  Nothing          -> string id_
  Just (start,end) -> string id_ <> brackets (int start <> colon <> int end)

expr_ b (DataCon _ (DC (Void {}, -1)) [e]) = expr_ b e

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

expr_ _ (DataCon (Clock _ _ Gated) _ es) = listBraces (mapM (expr_ False) es)

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "Clash.Sized.Internal.Signed.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLit (Just (Signed (fromInteger n),fromInteger n)) i

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "Clash.Sized.Internal.Unsigned.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLit (Just (Unsigned (fromInteger n),fromInteger n)) i

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "Clash.Sized.Internal.BitVector.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLit (Just (BitVector (fromInteger n),fromInteger n)) i

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "Clash.Sized.Internal.BitVector.fromInteger##"
  , [Literal _ i] <- extractLiterals bbCtx
  = exprLit (Just (Bit,1)) i

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "Clash.Sized.Internal.Index.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLit (Just (Index (fromInteger n),fromInteger n)) i

expr_ b (BlackBoxE _ libs imps inc bs bbCtx b') = do
  parenIf (b || b') (Mon (renderBlackBox libs imps inc bs bbCtx <*> pure 0))

expr_ _ (DataTag Bool (Left id_))          = string id_ <> brackets (int 0)
expr_ _ (DataTag Bool (Right id_))         = do
  iw <- Mon (use intWidth)
  "$unsigned" <> parens (listBraces (sequence [braces (int (iw-1) <+> braces "1'b0"),string id_]))

expr_ _ (DataTag (Sum _ _) (Left id_))     = "$unsigned" <> parens (string id_)
expr_ _ (DataTag (Sum _ _) (Right id_))    = "$unsigned" <> parens (string id_)

expr_ _ (DataTag (Product _ _) (Right _))  = do
  iw <- Mon (use intWidth)
  int iw <> "'sd0"

expr_ _ (DataTag hty@(SP _ _) (Right id_)) = "$unsigned" <> parens
                                               (string id_ <> brackets
                                               (int start <> colon <> int end))
  where
    start = typeSize hty - 1
    end   = typeSize hty - conSize hty

expr_ _ (DataTag (Vector 0 _) (Right _)) = do
  iw <- Mon $ use intWidth
  int iw <> "'sd0"
expr_ _ (DataTag (Vector _ _) (Right _)) = do
  iw <- Mon $ use intWidth
  int iw <> "'sd1"

expr_ _ (DataTag (RTree 0 _) (Right _)) = do
  iw <- Mon $ use intWidth
  int iw <> "'sd0"
expr_ _ (DataTag (RTree _ _) (Right _)) = do
  iw <- Mon $ use intWidth
  int iw <> "'sd1"

expr_ b (ConvBV _ _ _ e) = expr_ b e

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
exprLit _             (StringLit s) = string . pack $ show s
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
listBraces = align . encloseSep lbrace rbrace comma

parenIf :: Monad m => Bool -> m Doc -> m Doc
parenIf True  = parens
parenIf False = id

punctuate' :: Monad m => Mon m Doc -> Mon m [Doc] -> Mon m Doc
punctuate' s d = vcat (punctuate s d) <> s

encodingNote :: HWType -> VerilogM Doc
encodingNote (Clock _ _ Gated) = "// gated clock"
encodingNote (Clock {})        = "// clock"
encodingNote (Reset {})        = "// asynchronous reset: active high"
encodingNote _                 = emptyDoc
