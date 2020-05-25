{-|
  Copyright   :  (C) 2015-2016, University of Twente,
                     2017-2018, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Generate SystemVerilog for assorted Netlist datatypes
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Backend.SystemVerilog (SystemVerilogState) where

import qualified Control.Applicative                  as A
import           Control.Lens                         hiding (Indexed)
import           Control.Monad                        (forM,liftM,zipWithM)
import           Control.Monad.State                  (State)
import           Data.Bits                            (Bits, testBit)
import           Data.HashMap.Lazy                    (HashMap)
import qualified Data.HashMap.Lazy                    as HashMap
import qualified Data.HashMap.Strict                  as HashMapS
import           Data.HashSet                         (HashSet)
import qualified Data.HashSet                         as HashSet
import           Data.List                            (nub, nubBy)
import           Data.List.Extra                      ((<:>))
import           Data.Maybe                           (catMaybes,fromMaybe,mapMaybe)
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid                          hiding (Sum, Product)
#endif
import           Data.Semigroup.Monad
import qualified Data.Text.Lazy                       as Text
import qualified Data.Text                            as TextS
import           Data.Text.Prettyprint.Doc.Extra
import qualified System.FilePath

import           Clash.Annotations.Primitive          (HDL (..))
import           Clash.Annotations.BitRepresentation.Internal
  (ConstrRepr'(..), DataRepr'(..))
import           Clash.Annotations.BitRepresentation.ClashLib
  (bitsToBits)
import           Clash.Annotations.BitRepresentation.Util
  (BitOrigin(Lit, Field), bitOrigins, bitRanges)
import           Clash.Core.Var                       (Attr'(..))
import           Clash.Debug                          (traceIf)
import           Clash.Backend
import           Clash.Backend.Verilog
  (bits, bit_char, encodingNote, exprLit, include, noEmptyInit, uselibs)
import           Clash.Netlist.BlackBox.Types         (HdlSyn (..))
import           Clash.Netlist.BlackBox.Util
  (extractLiterals, renderBlackBox, renderFilePath)
import           Clash.Netlist.Id                     (IdType (..), mkBasicId')
import           Clash.Netlist.Types                  hiding (_intWidth, intWidth)
import           Clash.Netlist.Util                   hiding (mkIdentifier, extendIdentifier)
import           Clash.Signal.Internal                (ActiveEdge (..))
import           Clash.Util
  (SrcSpan, noSrcSpan, curLoc, makeCached, first, on, indexNote)
import           Clash.Util.Graph                     (reverseTopSort)

-- | State for the 'Clash.Backend.SystemVerilog.SystemVerilogM' monad:
data SystemVerilogState =
  SystemVerilogState
    { _tyCache   :: HashSet HWType -- ^ Previously encountered  HWTypes
    , _tySeen    :: HashMapS.HashMap Identifier Word -- ^ Product type counter
    , _nameCache :: HashMap HWType Doc -- ^ Cache for previously generated product type names
    , _genDepth  :: Int -- ^ Depth of current generative block
    , _modNm     :: Identifier
    , _idSeen    :: HashMapS.HashMap Identifier Word
    , _oports    :: [Identifier]
    , _srcSpan   :: SrcSpan
    , _includes  :: [(String,Doc)]
    , _imports   :: [Text.Text]
    , _libraries :: [Text.Text]
    , _dataFiles      :: [(String,FilePath)]
    -- ^ Files to be copied: (filename, old path)
    , _memoryDataFiles:: [(String,String)]
    -- ^ Files to be stored: (filename, contents). These files are generated
    -- during the execution of 'genNetlist'.
    , _tyPkgCtx  :: Bool
    -- ^ Are we in the context of generating the @_types@  package?
    , _intWidth  :: Int -- ^ Int/Word/Integer bit-width
    , _hdlsyn    :: HdlSyn
    , _escapedIds :: Bool
    , _undefValue :: Maybe (Maybe Int)
    }

makeLenses ''SystemVerilogState

instance Backend SystemVerilogState where
  initBackend     = SystemVerilogState HashSet.empty HashMapS.empty HashMap.empty
                                       0 "" HashMapS.empty [] noSrcSpan [] [] []
                                       [] [] False
  hdlKind         = const SystemVerilog
  primDirs        = const $ do root <- primsRoot
                               return [ root System.FilePath.</> "common"
                                      , root System.FilePath.</> "commonverilog"
                                      , root System.FilePath.</> "systemverilog"
                                      ]
  extractTypes    = _tyCache
  name            = const "systemverilog"
  extension       = const ".sv"

  genHDL          = genVerilog
  mkTyPackage     = mkTyPackage_
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
  toBV hty id_    = toSLV hty (Identifier (Text.toStrict id_) Nothing)
  fromBV hty id_  = simpleFromSLV hty (Text.toStrict id_)
  hdlSyn          = use hdlsyn
  mkIdentifier    = do
      allowEscaped <- use escapedIds
      return (go allowEscaped)
    where
      go _ Basic nm = case (TextS.take 1024 . filterReserved) (mkBasicId' SystemVerilog True nm) of
        nm' | TextS.null nm' -> "_clash_internal"
            | otherwise -> nm'
      go esc Extended (rmSlash . escapeTemplate -> nm) = case go esc Basic nm of
        nm' | esc && nm /= nm' -> TextS.concat ["\\",nm," "]
            | otherwise -> nm'
  extendIdentifier = do
      allowEscaped <- use escapedIds
      return (go allowEscaped)
    where
      go _ Basic nm ext =
        case (TextS.take 1024 . filterReserved) (mkBasicId' SystemVerilog True (nm `TextS.append` ext)) of
          nm' | TextS.null nm' -> "_clash_internal"
              | otherwise -> nm'
      go esc Extended (rmSlash -> nm) ext =
        let nmExt = nm `TextS.append` ext
        in  case go esc Basic nm ext of
              nm' | esc && nm' /= nmExt -> case TextS.isPrefixOf "c$" nmExt of
                      True -> TextS.concat ["\\",nmExt," "]
                      _    -> TextS.concat ["\\c$",nmExt," "]
                  | otherwise -> nm'

  setModName nm s = s {_modNm = nm}
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
  addLibraries libs = libraries %= (libs ++)
  addImports inps = imports %= (inps ++)
  addAndSetData f = do
    fs <- use dataFiles
    let (fs',f') = renderFilePath fs f
    dataFiles .= fs'
    return f'
  getDataFiles = use dataFiles
  addMemoryDataFile f = memoryDataFiles %= (f:)
  getMemoryDataFiles = use memoryDataFiles
  seenIdentifiers = idSeen
  ifThenElseExpr _ = True

rmSlash :: Identifier -> Identifier
rmSlash nm = fromMaybe nm $ do
  nm1 <- TextS.stripPrefix "\\" nm
  pure (TextS.filter (not . (== ' ')) nm1)

type SystemVerilogM a = Mon (State SystemVerilogState) a

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
  then s `TextS.append` "_r"
  else s

-- | Generate SystemVerilog for a Netlist component
genVerilog :: Identifier -> SrcSpan -> HashMapS.HashMap Identifier Word -> Component -> SystemVerilogM ((String,Doc),[(String,Doc)])
genVerilog _ sp seen c = preserveSeen $ do
    Mon $ idSeen .= seen
    -- Don't have type names conflict with module names
    Mon $ tySeen %= HashMapS.unionWith max seen
    Mon $ setSrcSpan sp
    v    <- verilog
    incs <- Mon $ use includes
    return ((TextS.unpack cName,v),incs)
  where
    cName   = componentName c
    verilog = commentHeader <> line <>
              timescale <> line <>
              module_ c
    commentHeader
         = "/* AUTOMATICALLY GENERATED SYSTEMVERILOG-2005 SOURCE CODE."
      <> line <> "** GENERATED BY CLASH " <> string (Text.pack clashVer) <> ". DO NOT MODIFY."
      <> line <> "*/"
    timescale = "`timescale 100fs/100fs"

-- | Generate a SystemVerilog package containing type definitions for the given HWTypes
mkTyPackage_ :: Identifier
             -> [HWType]
             -> SystemVerilogM [(String,Doc)]
mkTyPackage_ modName hwtys = do
    Mon (tyPkgCtx .= True)
    normTys <- nub <$> mapM (normaliseType) (hwtys ++ usedTys)
    let
      needsDec    = nubBy eqReprTy $ normTys
      hwTysSorted = topSortHWTys needsDec
      packageDec  = vcat $ fmap catMaybes $ mapM tyDec hwTysSorted
      funDecs     = vcat $ fmap catMaybes $ mapM funDec hwTysSorted

    pkg <- (:[]) A.<$> (TextS.unpack modName ++ "_types",) A.<$>
       "package" <+> modNameD <> "_types" <> semi <> line <>
         indent 2 packageDec <> line <>
         indent 2 funDecs <> line <>
       "endpackage" <+> colon <+> modNameD <> "_types"
    Mon (tyPkgCtx .= False)
    return pkg
  where
    modNameD    = stringS modName
    usedTys     = concatMap mkUsedTys hwtys

    eqReprTy :: HWType -> HWType -> Bool
    eqReprTy (Vector n ty1) (Vector m ty2)
      | m == n    = eqReprTy ty1 ty2
      | otherwise = False
    eqReprTy (RTree n ty1) (RTree m ty2)
      | m == n    = eqReprTy ty1 ty2
      | otherwise = False
    eqReprTy Bit  ty2 = ty2 `elem` [Bit,Bool]
    eqReprTy Bool ty2 = ty2 `elem` [Bit,Bool]
    eqReprTy ty1 ty2
      | isUnsigned ty1 && isUnsigned ty2 = typeSize ty1 == typeSize ty2
      | otherwise                        = ty1 == ty2

    isUnsigned :: HWType -> Bool
    isUnsigned (Unsigned _)        = True
    isUnsigned (BitVector _)       = True
    isUnsigned (Index _)           = True
    isUnsigned (Sum _ _)           = True
    isUnsigned (CustomSum _ _ _ _) = True
    isUnsigned (SP _ _)            = True
    isUnsigned (CustomSP _ _ _ _)  = True
    isUnsigned _                   = False

mkUsedTys :: HWType
        -> [HWType]
mkUsedTys v@(Vector _ elTy)     = v : mkUsedTys elTy
mkUsedTys t@(RTree _ elTy)      = t : mkUsedTys elTy
mkUsedTys p@(Product _ _ elTys) = p : concatMap mkUsedTys elTys
mkUsedTys sp@(SP _ elTys)       = sp : concatMap mkUsedTys (concatMap snd elTys)
mkUsedTys t                     = [t]

topSortHWTys :: [HWType]
             -> [HWType]
topSortHWTys hwtys = sorted
  where
    nodes  = zip [0..] hwtys
    nodesI = HashMap.fromList (zip hwtys [0..])
    edges  = concatMap edge hwtys
    sorted =
      case reverseTopSort nodes edges of
        Left err -> error ("[BUG IN CLASH] topSortHWTys: " ++ err)
        Right ns -> ns

    edge t@(Vector _ elTy) = maybe [] ((:[]) . (HashMap.lookupDefault (error $ $(curLoc) ++ "Vector") t nodesI,))
                                      (HashMap.lookup elTy nodesI)
    edge t@(RTree _ elTy)  = maybe [] ((:[]) . (HashMap.lookupDefault (error $ $(curLoc) ++ "RTree") t nodesI,))
                                      (HashMap.lookup elTy nodesI)
    edge t@(Product _ _ tys) = let ti = HashMap.lookupDefault (error $ $(curLoc) ++ "Product") t nodesI
                               in mapMaybe (\ty -> liftM (ti,) (HashMap.lookup ty nodesI)) tys
    edge t@(SP _ ctys)     = let ti = HashMap.lookupDefault (error $ $(curLoc) ++ "SP") t nodesI
                             in concatMap (\(_,tys) -> mapMaybe (\ty -> liftM (ti,) (HashMap.lookup ty nodesI)) tys) ctys
    edge _                 = []

normaliseType :: HWType -> SystemVerilogM HWType
normaliseType (Annotated _ ty) = normaliseType ty
normaliseType (Vector n ty)    = Vector n <$> (normaliseType ty)
normaliseType (RTree d ty)     = RTree d <$> (normaliseType ty)
normaliseType (Product nm lbls tys) = Product nm lbls <$> (mapM normaliseType tys)
normaliseType ty@(SP _ elTys)      = do
  Mon $ mapM_ ((tyCache %=) . HashSet.insert) (concatMap snd elTys)
  return (BitVector (typeSize ty))
normaliseType (CustomSP _ _dataRepr size elTys) = do
  Mon $ mapM_ ((tyCache %=) . HashSet.insert) [ty | (_, _, subTys) <- elTys, ty <- subTys]
  return (BitVector size)
normaliseType ty@(Index _) = return (Unsigned (typeSize ty))
normaliseType ty@(Sum _ _) = return (BitVector (typeSize ty))
normaliseType ty@(CustomSum _ _ _ _) = return (BitVector (typeSize ty))
normaliseType (Clock _) = return Bit
normaliseType (Reset {}) = return Bit
normaliseType (BiDirectional dir ty) = BiDirectional dir <$> normaliseType ty
normaliseType ty = return ty


range :: Either Int Int -> SystemVerilogM Doc
range (Left n)  = brackets (int (n-1) <> colon <> int 0)
range (Right n) = brackets (int 0 <> colon <> int (n-1))

tyDec :: HWType -> SystemVerilogM (Maybe Doc)
tyDec ty@(Vector n elTy) | typeSize ty > 0 = Just A.<$> do
  syn <- Mon hdlSyn
  case syn of
    Vivado -> case splitVecTy ty of
      Just ([Right n',Left n''],elTy') ->
        "typedef" <+> elTy' <+> brackets (int (n''-1) <> colon <> int 0) <+>
        tyName ty <+> brackets (int 0 <> colon <> int (n'-1)) <> semi
      _ ->
        "typedef" <+> "logic" <+> brackets (int (typeSize elTy - 1) <> colon <> int 0) <+>
        tyName ty <+> brackets (int 0 <> colon <> int (n-1)) <> semi
    _ -> case splitVecTy ty of
      Just (Right n':ns,elTy') ->
        "typedef" <+> elTy' <+> hcat (mapM range ns) <+> tyName ty <+>
        brackets (int 0 <> colon <> int (n' - 1)) <> semi
      _ -> error $ $(curLoc) ++ "impossible"
tyDec ty@(RTree n elTy) | typeSize elTy > 0 = Just A.<$> do
  syn <- Mon hdlSyn
  case syn of
    Vivado -> case splitVecTy ty of
      Just ([Right n',Left n''],elTy') -> -- n' == 2^n
        "typedef" <+> elTy' <+> brackets (int 0 <> colon <> int (n''-1)) <+>
        tyName ty <+> brackets (int 0 <> colon <> int (n'-1)) <> semi
      _ ->
        "typedef" <+> "logic" <+> brackets (int (typeSize elTy - 1) <> colon <> int 0) <+>
        tyName ty <+> brackets (int 0 <> colon <> int (2^n-1)) <> semi
    _ -> case splitVecTy ty of
      Just (Right n':ns,elTy') -> -- n' == 2^n
        "typedef" <+> elTy' <+> hcat (mapM range ns) <+> tyName ty <+>
        brackets (int 0 <> colon <> int (n' - 1)) <> semi
      _ -> error $ $(curLoc) ++ "impossible"
tyDec ty@(Product _ _ tys) | typeSize ty > 0 = Just A.<$> prodDec
  where
    prodDec = "typedef struct packed {" <> line <>
                indent 2 (vcat $ fmap catMaybes $ zipWithM combineM selNames tys) <> line <>
              "}" <+> tName <> semi

    combineM x y = do
      yM <- lvType y
      case yM of
        Nothing -> pure Nothing
        Just y' -> Just A.<$> (pure y' <+> x <> semi)
    tName    = tyName ty
    selNames = map (\i -> tName <> "_sel" <> int i) [0..]

tyDec _ = pure Nothing

splitVecTy :: HWType -> Maybe ([Either Int Int],SystemVerilogM Doc)
splitVecTy = fmap splitElemTy . go
  where
    splitElemTy (ns,t) = case t of
      Product {} -> (ns, verilogType t)
      Vector {}  -> error $ $(curLoc) ++ "impossible"
      Clock {}   -> (ns, verilogType t)
      Reset {}   -> (ns, "logic")
      Bool       -> (ns, "logic")
      Bit        -> (ns, "logic")
      String     -> (ns, "string")
      Signed n   -> (ns ++ [Left n],"logic signed")
      _          -> (ns ++ [Left (typeSize t)], "logic")

    go (Vector n elTy) = case go elTy of
      Just (ns,elTy') -> Just (Right n:ns,elTy')
      _               -> Just ([Right n],elTy)

    go (RTree n elTy) = let n' = 2^n in case go elTy of
      Just (ns,elTy') -> Just (Right n':ns,elTy')
      _               -> Just ([Right n'],elTy)

    go _ = Nothing

lvType :: HWType -> SystemVerilogM (Maybe Doc)
lvType ty@(Vector n elTy) | typeSize ty > 0 = Just A.<$> do
  syn <- Mon hdlSyn
  case syn of
    Vivado -> "logic" <+> brackets (int 0 <> colon <> int (n-1)) <> brackets (int (typeSize elTy - 1) <> colon <> int 0)
    _ -> case splitVecTy ty of
      Just (ns,elTy') -> elTy' <> hcat (mapM range ns)
      _ -> error $ $(curLoc) ++ "impossible"
lvType ty@(RTree n elTy) | typeSize elTy > 0 = Just A.<$> do
  syn <- Mon hdlSyn
  case syn of
    Vivado -> "logic" <+> brackets (int 0 <> colon <> int (2^n-1)) <> brackets (int (typeSize elTy - 1) <> colon <> int 0)
    _ -> case splitVecTy ty of
      Just (ns,elTy') -> elTy' <> hcat (mapM range ns)
      _ -> error $ $(curLoc) ++ "impossible"
lvType ty | typeSize ty > 0 = Just A.<$> verilogType ty
lvType _ = pure Nothing

funDec :: HWType -> SystemVerilogM (Maybe Doc)
funDec ty@(Vector n elTy) | typeSize ty > 0 = Just A.<$>
  "function" <+> "automatic" <+> "logic" <+> ranges <+> tName <> "_to_lv" <> parens (sigDecl "i" ty) <> semi <> line <>
  indent 2
    ("for" <+> parens ("int n = 0" <> semi <+> "n <" <+> int n <> semi <+> "n=n+1") <> line <>
      indent 2 (tName <> "_to_lv" <> brackets "n" <+> "=" <+> "i[n]" <> semi)) <> line <>
  "endfunction" <> line <>
  "function" <+> "automatic" <+> tName <+> tName <> "_from_lv" <> parens ("logic" <+> ranges <+> "i") <> semi <> line <>
  indent 2
    ("for" <+> parens ("int n = 0" <> semi <+> "n <" <+> int n <> semi <+> "n=n+1") <> line <>
      indent 2 (tName <> "_from_lv" <> brackets "n" <+> "=" <+> "i[n]" <> semi)) <> line <>
  "endfunction" <> line <>
  if n > 1 then
    "function" <+> "automatic" <+> tName <+> tName <> "_cons" <> parens (sigDecl "x" elTy <> comma <> vecSigDecl "xs") <> semi <> line <>
    indent 2
      (tName <> "_cons" <> brackets (int 0) <+> "=" <+> (toSLV elTy (Identifier "x" Nothing)) <> semi <> line <>
       tName <> "_cons" <> brackets (int 1 <> colon <> int (n-1)) <+> "=" <+> "xs" <> semi) <> line <>
    "endfunction"
  else
    "function" <+> "automatic" <+> tName <+> tName <> "_cons" <> parens (sigDecl "x" elTy) <> semi <> line <>
    indent 2
      (tName <> "_cons" <> brackets (int 0) <+> "=" <+> (toSLV elTy (Identifier "x" Nothing)) <> semi) <> line <>
    "endfunction"
  where
    tName  = tyName ty
    ranges = brackets (int 0 <> colon <> int (n-1)) <>
             brackets (int (typeSize elTy - 1) <> colon <> int 0)

    vecSigDecl :: SystemVerilogM Doc -> SystemVerilogM Doc
    vecSigDecl d = do
      syn <- Mon hdlSyn
      case syn of
        Vivado -> case splitVecTy ty of
          Just ([Right n',Left n''],elTy') ->
            elTy' <+> brackets (int 0 <> colon <> int (n''-1)) <+>
            d <+> brackets (int 0 <> colon <> int (n'-2))
          _ ->
            "logic" <+> brackets (int (typeSize elTy - 1) <> colon <> int 0) <+>
            d <+> brackets (int 0 <> colon <> int (n-2))
        _ -> case splitVecTy ty of
         Just (Right n':ns,elTy') ->
           elTy' <+> hcat (mapM range ns) <+> d <+>
           brackets (int 0 <> colon <> int (n' - 2))
         _ -> error $ $(curLoc) ++ "impossible"


funDec ty@(RTree n elTy) | typeSize elTy > 0 = Just A.<$>
  "function" <+> "automatic" <+> "logic" <+> ranges <+> tName <> "_to_lv" <> parens (sigDecl "i" ty) <> semi <> line <>
  indent 2
    ("for" <+> parens ("int n = 0" <> semi <+> "n <" <+> int (2^n) <> semi <+> "n=n+1") <> line <>
      indent 2 (tName <> "_to_lv" <> brackets "n" <+> "=" <+> "i[n]" <> semi)) <> line <>
  "endfunction" <> line <>
  "function" <+> "automatic" <+> tName <+> tName <> "_from_lv" <> parens ("logic" <+> ranges <+> "i") <> semi <> line <>
  indent 2
    ("for" <+> parens ("int n = 0" <> semi <+> "n <" <+> int (2^n) <> semi <+> "n=n+1") <> line <>
      indent 2 (tName <> "_from_lv" <> brackets "n" <+> "=" <+> "i[n]" <> semi)) <> line <>
  "endfunction" <> line <>
  (if n > 0
      then
        "function" <+> "automatic" <+> tName <+> tName <> "_br" <> parens (treeSigDecl "l" <> comma <> treeSigDecl "r") <> semi <> line <>
        indent 2
          (tName <> "_br" <> brackets (int 0 <> colon <> int (2^(n-1)-1)) <+> "=" <+> "l" <> semi <> line <>
           tName <> "_br" <> brackets (int (2^(n-1)) <> colon <> int (2^n-1)) <+> "=" <+> "r" <> semi) <> line <>
        "endfunction"
      else
        emptyDoc)
  where
    treeSigDecl :: SystemVerilogM Doc -> SystemVerilogM Doc
    treeSigDecl d = do
      syn <- Mon hdlSyn
      case syn of
        Vivado -> case splitVecTy (RTree (n-1) elTy) of
          Just ([Right n',Left n''],elTy') -> -- n' == 2 ^ (n-1)
            elTy' <+> brackets (int 0 <> colon <> int (n''-1)) <+>
            d <+> brackets (int 0 <> colon <> int (n' - 1))
          _ ->
            "logic" <+> brackets (int (typeSize elTy - 1) <> colon <> int 0) <+>
            d <+> brackets (int 0 <> colon <> int (2^(n-1)-1))
        _ -> case splitVecTy (RTree (n-1) elTy) of
          Just (Right n':ns,elTy') -> -- n' == 2 ^ (n-1)
            elTy' <+> hcat (mapM range ns) <+> d <+>
            brackets (int 0 <> colon <> int (n' - 1))
          _ -> error $ $(curLoc) ++ "impossible"

    tName  = tyName ty
    ranges = brackets (int 0 <> colon <> int (2^n-1)) <>
             brackets (int (typeSize elTy - 1) <> colon <> int 0)

funDec _ = pure Nothing

module_ :: Component -> SystemVerilogM Doc
module_ c =
  addSeen c *> modVerilog <* Mon (imports .= [] >> oports .= [])
 where
  modVerilog = do
    body <- modBody
    imps <- Mon $ use imports
    libs <- Mon $ use libraries
    modHeader <> line <> modPorts <> line <> include (nub imps) <> uselibs (nub libs) <> pure body <> line <> modEnding

  modHeader  = "module" <+> stringS (componentName c)
  modPorts   = indent 4 (tupleInputs inPorts <> line <> tupleOutputs outPorts <> semi)
  modBody    = indent 2 (decls (declarations c)) <> line <> line <> indent 2 (insts (declarations c))
  modEnding  = "endmodule"

  inPorts  = sequence [ sigPort (Nothing,isBiSignalIn ty) (i,ty) Nothing | (i,ty)  <- inputs c  ]
  outPorts = sequence [ sigPort (Just wr,False) p iEM | (wr, p, iEM) <- outputs c ]

  wr2ty (Nothing,isBidirectional)
    | isBidirectional
    = "inout"
    | otherwise
    = "input"
  wr2ty (Just _,_)
    = "output"

  -- map a port to its verilog type, port name, and any encoding notes
  sigPort (wr2ty -> portTy) (nm, hwTy) iEM
    = addAttrs (hwTypeAttrs hwTy)
        (portTy <+> sigDecl (stringS nm) hwTy <> iE <+> encodingNote hwTy)
    where
      iE = maybe emptyDoc (noEmptyInit . expr_ False) iEM
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

addSeen :: Component -> SystemVerilogM ()
addSeen c = do
  let iport = map fst (inputs c)
      oport = map (fst . (\(_,x,_)->x)) $ outputs c
      nets  = mapMaybe (\case {NetDecl' _ _ i _ _ -> Just i; _ -> Nothing}) $ declarations c
  Mon (idSeen %= (HashMapS.unionWith max (HashMapS.fromList (concatMap (map (,0)) [iport,oport,nets]))))
  Mon (oports .= oport)

mkUniqueId :: Identifier -> SystemVerilogM Identifier
mkUniqueId i = do
  mkId <- Mon (mkIdentifier <*> pure Extended)
  seen <- Mon $ use idSeen
  let i' = mkId i
  case HashMapS.lookup i seen of
    Just n -> go mkId seen i' n
    Nothing -> do Mon (idSeen %= (HashMapS.insert i' 0))
                  return i'
  where
    go :: (Identifier -> Identifier) -> HashMapS.HashMap Identifier Word -> Identifier
       -> Word -> SystemVerilogM Identifier
    go mkId seen i' n = do
      let i'' = mkId (TextS.append i' (TextS.pack ('_':show n)))
      case HashMapS.lookup i'' seen of
        Just _  -> go mkId seen i' (n+1)
        Nothing -> do Mon (idSeen %= (HashMapS.insert i'' (n+1)))
                      return i''

verilogType :: HWType -> SystemVerilogM Doc
verilogType t_ = do
  t <- normaliseType t_
  Mon (tyCache %= HashSet.insert t)
  let logicOrWire | isBiSignalIn t = "wire"
                  | otherwise      = "logic"
  pkgCtx <- Mon $ use tyPkgCtx
  nm <- Mon $ use modNm
  let pvrType = if pkgCtx
                then tyName t
                else stringS nm <> "_types::" <> tyName t
  case t of
    Product {}    -> pvrType
    Vector {}     -> pvrType
    RTree {}      -> pvrType
    Signed n      -> logicOrWire <+> "signed" <+> brackets (int (n-1) <> colon <> int 0)
    Clock _       -> "logic"
    Reset {}      -> "logic"
    Bit           -> "logic"
    Bool          -> "logic"
    String        -> "string"
    FileType      -> "integer"
    _ -> logicOrWire <+> brackets (int (typeSize t -1) <> colon <> int 0)

sigDecl :: SystemVerilogM Doc -> HWType -> SystemVerilogM Doc
sigDecl d t = verilogType t <+> d

-- | Convert a Netlist HWType to the root of a Verilog type
verilogTypeMark :: HWType -> SystemVerilogM Doc
verilogTypeMark t_ = do
  t <- normaliseType t_
  Mon (tyCache %= HashSet.insert t)
  pkgCtx <- Mon $ use tyPkgCtx
  nm <- Mon $ use modNm
  let pvrType = if pkgCtx
                then tyName t
                else stringS nm <> "_types::" <> tyName t
  case t of
    Product {} -> pvrType
    Vector {}  -> pvrType
    RTree {}   -> pvrType
    _ -> emptyDoc

tyName :: HWType -> SystemVerilogM Doc
tyName Bool                  = "logic"
tyName Bit                   = "logic"
tyName (Vector n elTy)       = "array_of_" <> int n <> "_" <> tyName elTy
tyName (RTree n elTy)        = "tree_of_" <> int n <> "_" <> tyName elTy
tyName (BitVector n)         = "logic_vector_" <> int n
tyName t@(Index _)           = "logic_vector_" <> int (typeSize t)
tyName (Signed n)            = "signed_" <> int n
tyName (Unsigned n)          = "logic_vector_" <> int n
tyName t@(Sum _ _)           = "logic_vector_" <> int (typeSize t)
tyName t@(CustomSum _ _ _ _) = "logic_vector_" <> int (typeSize t)
tyName t@(CustomSP _ _ _ _)  = "logic_vector_" <> int (typeSize t)
tyName t@(Product nm _ _)      = do
  tN <- normaliseType t
  Mon (makeCached tN nameCache prodName)
  where
    prodName = do
      seen <- use tySeen
      mkId <- mkIdentifier <*> pure Basic
      let nm'  = (mkId . last . TextS.splitOn ".") nm
          nm'' = if TextS.null nm'
                    then "product"
                    else nm'
          (nm3,count) = case HashMapS.lookup nm'' seen of
                          Just cnt -> go mkId seen cnt nm''
                          Nothing  -> (nm'',0)
      tySeen %= HashMap.insert nm3 count
      stringS nm3

    go mkId s i n =
      let n' = n `TextS.append` TextS.pack ('_':show i)
      in  case HashMapS.lookup n' s of
                 Just _  -> go mkId s (i+1) n
                 Nothing -> (n',i+1)
tyName t@(SP _ _) = "logic_vector_" <> int (typeSize t)
tyName (Clock _)  = "logic"
tyName (Reset {}) = "logic"
tyName t =  error $ $(curLoc) ++ "tyName: " ++ show t

-- | Convert a Netlist HWType to an error SystemVerilog value for that type
verilogTypeErrValue :: HWType -> SystemVerilogM Doc
verilogTypeErrValue (Vector n elTy) = do
  syn <- Mon hdlSyn
  case syn of
    Vivado -> char '\'' <> braces (int n <+> braces (singularErrValue elTy))
    _ -> char '\'' <> braces (int n <+> braces (verilogTypeErrValue elTy))
verilogTypeErrValue (RTree n elTy) = do
  syn <- Mon hdlSyn
  case syn of
    Vivado -> char '\'' <> braces (int (2^n) <+> braces (singularErrValue elTy))
    _ -> char '\'' <> braces (int (2^n) <+> braces (verilogTypeErrValue elTy))
verilogTypeErrValue String = "\"ERROR\""
verilogTypeErrValue ty = singularErrValue ty

singularErrValue :: HWType -> SystemVerilogM Doc
singularErrValue ty = do
  udf <- Mon (use undefValue)
  case udf of
    Nothing       -> braces (int (typeSize ty) <+> braces "1'bx")
    Just Nothing  -> int (typeSize ty) <> "'d0 /* undefined */"
    Just (Just x) -> braces (int (typeSize ty) <+> braces ("1'b" <> int x)) <+> "/* undefined */"

verilogRecSel
  :: HWType
  -> Int
  -> SystemVerilogM Doc
verilogRecSel ty i = tyName ty <> "_sel" <> int i

decls :: [Declaration] -> SystemVerilogM Doc
decls [] = emptyDoc
decls ds = do
    dsDoc <- catMaybes A.<$> mapM decl ds
    case dsDoc of
      [] -> emptyDoc
      _  -> punctuate' semi (A.pure dsDoc)

decl :: Declaration -> SystemVerilogM (Maybe Doc)
decl (NetDecl' noteM _ id_ tyE iEM) =
  Just A.<$> maybe id addNote noteM (addAttrs attrs (typ tyE))
  where
    typ (Left  ty) = stringS ty <+> stringS id_ <> iE
    typ (Right ty) = sigDecl (stringS id_) ty <> iE
    addNote n = mappend ("//" <+> stringS n <> line)
    attrs = fromMaybe [] (hwTypeAttrs A.<$> either (const Nothing) Just tyE)
    iE = maybe emptyDoc (noEmptyInit . expr_ False) iEM

decl _ = return Nothing

-- | Convert single attribute to systemverilog syntax
renderAttr :: Attr' -> Text.Text
renderAttr (StringAttr'  key value) = Text.pack $ concat [key, " = ", show value]
renderAttr (IntegerAttr' key value) = Text.pack $ concat [key, " = ", show value]
renderAttr (BoolAttr'    key True ) = Text.pack $ concat [key, " = ", "1"]
renderAttr (BoolAttr'    key False) = Text.pack $ concat [key, " = ", "0"]
renderAttr (Attr'        key      ) = Text.pack $ key

-- | Add attribute notation to given declaration
addAttrs
  :: [Attr']
  -> SystemVerilogM Doc
  -> SystemVerilogM Doc
addAttrs []     t = t
addAttrs attrs' t =
  "(*" <+> attrs'' <+> "*)" <+> t
    where
      attrs'' = string $ Text.intercalate ", " (map renderAttr attrs')

insts :: [Declaration] -> SystemVerilogM Doc
insts [] = emptyDoc
insts (TickDecl id_:ds) = comment "//" id_ <> line <> insts ds
insts (d:ds) = do
  docM <- inst_ d
  case docM of
    Nothing -> insts ds
    Just doc -> pure doc <> line <> line <> insts ds

stdMatch
  :: Bits a
  => Int
  -> a
  -> a
  -> String
stdMatch 0 _mask _value = []
stdMatch size mask value =
  symbol : stdMatch (size - 1) mask value
  where
    symbol =
      if testBit mask (size - 1) then
        if testBit value (size - 1) then
          '1'
        else
          '0'
      else
        '?'

patLitCustom'
  :: Int
  -> ConstrRepr'
  -> SystemVerilogM Doc
patLitCustom' size (ConstrRepr' _name _n mask value _anns) =
  int size <> squote <> "b" <> (string $ Text.pack $ stdMatch size mask value)

patLitCustom
  :: HWType
  -> Literal
  -> SystemVerilogM Doc
patLitCustom (CustomSum _name _dataRepr size reprs) (NumLit (fromIntegral -> i)) =
  patLitCustom' size (fst $ reprs !! i)

patLitCustom (CustomSP _name _dataRepr size reprs) (NumLit (fromIntegral -> i)) =
  let (cRepr, _id, _tys) = reprs !! i in
  patLitCustom' size cRepr

patLitCustom x y = error $ $(curLoc) ++ unwords
  [ "You can only pass CustomSP / CustomSum / CustomProduct and a NumLit to"
  , "this function, not", show x, "and", show y]

patMod :: HWType -> Literal -> Literal
patMod hwTy (NumLit i) = NumLit (i `mod` (2 ^ typeSize hwTy))
patMod _ l = l

-- | Helper function for inst_, handling CustomSP and CustomSum
inst_' :: TextS.Text -> Expr -> HWType -> [(Maybe Literal, Expr)] -> SystemVerilogM (Maybe Doc)
inst_' id_ scrut scrutTy es = fmap Just $
  "always_comb begin" <> line <> indent 2 casez <> line <> "end"
    where
      casez =
        "casez" <+> parens var <> line <>
          indent 2 (conds esNub) <> line <>
        "endcase"

      esMod = map (first (fmap (patMod scrutTy))) es
      esNub = nubBy ((==) `on` fst) esMod
      var   = expr_ True scrut

      conds :: [(Maybe Literal,Expr)] -> SystemVerilogM Doc
      conds []                = error $ $(curLoc) ++ "Empty list of conditions invalid."
      conds [(_,e)]           = "default" <+> ":" <+> stringS id_ <+> "=" <+> expr_ False e <> ";"
      conds ((Nothing,e):_)   = "default" <+> ":" <+> stringS id_ <+> "=" <+> expr_ False e <> ";"
      conds ((Just c ,e):es') =
        mask' <+> ":" <+> stringS id_ <+> "=" <+> expr_ False e <> ";" <> line <> conds es'
          where
            mask' = patLitCustom scrutTy c

-- | Turn a Netlist Declaration to a SystemVerilog concurrent block
inst_ :: Declaration -> SystemVerilogM (Maybe Doc)
inst_ (TickDecl {}) = return Nothing

inst_ (Assignment id_ e) = fmap Just $
  "assign" <+> stringS id_ <+> equals <+> align (expr_ False e <> semi)

inst_ (CondAssignment id_ ty scrut _ [(Just (BoolLit b), l),(_,r)]) = fmap Just $ do
    { syn <- Mon hdlSyn
    ; p   <- Mon $ use oports
    ; if syn == Vivado && id_ `elem` p
         then do
              { regId <- mkUniqueId =<< Mon (extendIdentifier <*> pure Extended <*> pure id_ <*> pure "_reg")
              ; verilogType ty <+> stringS regId <> semi <> line <>
                "always_comb begin" <> line <>
                indent 2 ("if" <> parens (expr_ True scrut) <> line <>
                            (indent 2 $ stringS regId <+> equals <+> expr_ False t <> semi) <> line <>
                         "else" <> line <>
                            (indent 2 $ stringS regId <+> equals <+> expr_ False f <> semi)) <> line <>
                "end" <> line <>
                "assign" <+> stringS id_ <+> equals <+> stringS regId <> semi
              }
         else "always_comb begin" <> line <>
              indent 2 ("if" <> parens (expr_ True scrut) <> line <>
                          (indent 2 $ stringS id_ <+> equals <+> expr_ False t <> semi) <> line <>
                       "else" <> line <>
                          (indent 2 $ stringS id_ <+> equals <+> expr_ False f <> semi)) <> line <>
              "end"
    }
  where
    (t,f) = if b then (l,r) else (r,l)

inst_ (CondAssignment id_ _ scrut scrutTy@(CustomSP {}) es) =
  inst_' id_ scrut scrutTy es

inst_ (CondAssignment id_ _ scrut scrutTy@(CustomSum {}) es) =
  inst_' id_ scrut scrutTy es

inst_ (CondAssignment id_ _ scrut scrutTy@(CustomProduct {}) es) =
  inst_' id_ scrut scrutTy es

inst_ (CondAssignment id_ ty scrut scrutTy es) = fmap Just $ do
    { syn <- Mon hdlSyn
    ; p <- Mon $ use oports
    ; if syn == Vivado && id_ `elem` p
         then do
           { regId <- mkUniqueId =<< Mon (extendIdentifier <*> pure Extended <*> pure id_ <*> pure "_reg")
           ; verilogType ty <+> stringS regId <> semi <> line <>
             "always_comb begin" <> line <>
             indent 2 ("case" <> parens (expr_ True scrut) <> line <>
                         (indent 2 $ vcat $ punctuate semi (conds regId es)) <> semi <> line <>
                       "endcase") <> line <>
             "end" <> line <>
             "assign" <+> stringS id_ <+> equals <+> stringS regId <> semi
           }
         else "always_comb begin" <> line <>
              indent 2 ("case" <> parens (expr_ True scrut) <> line <>
                          (indent 2 $ vcat $ punctuate semi (conds id_ es)) <> semi <> line <>
                        "endcase") <> line <>
              "end"
    }
  where
    conds :: Identifier -> [(Maybe Literal,Expr)] -> SystemVerilogM [Doc]
    conds _ []                = return []
    conds i [(_,e)]           = ("default" <+> colon <+> stringS i <+> equals <+> expr_ False e) <:> return []
    conds i ((Nothing,e):_)   = ("default" <+> colon <+> stringS i <+> equals <+> expr_ False e) <:> return []
    conds i ((Just c ,e):es') = (exprLitSV (Just (scrutTy,conSize scrutTy)) c <+> colon <+> stringS i <+> equals <+> expr_ False e) <:> conds i es'

inst_ (InstDecl _ _ nm lbl ps pms) = fmap Just $
    nest 2 (stringS nm <> params <> stringS lbl <> line <> pms' <> semi)
  where
    pms' = tupled $ sequence [dot <> expr_ False i <+> parens (expr_ False e) | (i,_,_,e) <- pms]
    params
      | null ps   = space
      | otherwise = line <> "#" <> tupled (sequence [dot <> expr_ False i <+> parens (expr_ False e) | (i,_,e) <- ps]) <> line

inst_ (BlackBoxD _ libs imps inc bs bbCtx) =
  fmap Just (Mon (column (renderBlackBox libs imps inc bs bbCtx)))

inst_ (Seq ds) = Just <$> seqs ds

inst_ (NetDecl' {}) = return Nothing

-- | Render a data constructor application for data constructors having a
-- custom bit representation.
customReprDataCon
  :: DataRepr'
  -- ^ Custom representation of data type
  -> ConstrRepr'
  -- ^ Custom representation of a specific constructor of @dataRepr@
  -> [(HWType, Expr)]
  -- ^ Arguments applied to constructor
  -> SystemVerilogM Doc
customReprDataCon dataRepr constrRepr args =
  braces $ hcat $ punctuate ", " $ mapM range' origins
    where
      size = drSize dataRepr

      -- Build bit representations for all constructor arguments
      argExprs = map (uncurry toSLV) args :: [SystemVerilogM Doc]

      -- Spread bits of constructor arguments using masks
      origins = bitOrigins dataRepr constrRepr :: [BitOrigin]

      range'
        :: BitOrigin
        -> SystemVerilogM Doc
      range' (Lit (bitsToBits -> ns)) =
        int (length ns) <> squote <> "b" <> hcat (mapM (bit_char undefValue) ns)
      range' (Field n start end) =
        -- We want to select the bits starting from 'start' downto and including
        -- 'end'. We cannot use slice notation in Verilog, as the preceding
        -- expression might not be an identifier.
        let fsize = start - end + 1 in
        let expr' = argExprs !! n in

        if | fsize == size ->
               -- If sizes are equal, rotating / resizing amounts to doing nothing
               expr'
           | end == 0 ->
               -- Rotating is not necessary if relevant bits are already at the end
               int fsize <> squote <> parens expr'
           | otherwise ->
               -- Select bits 'start' downto and including 'end'
               let rotated  = parens expr' <+> ">>" <+> int end in
               int fsize <> squote <> parens rotated

seq_ :: Seq -> SystemVerilogM Doc
seq_ (AlwaysClocked edge clk ds) =
  "always @" <>
    parens (case edge of {Rising -> "posedge"; _ -> "negedge"} <+>
            expr_ False clk) <+> "begin" <> line <>
    indent 2 (seqs ds) <> line <>
  "end"

seq_ (Initial ds) =
  "initial begin" <> line <>
  indent 2 (seqs ds) <> line <>
  "end"

seq_ (AlwaysComb ds) =
  "always @* begin" <> line <>
  indent 2 (seqs ds) <> line <>
  "end"

seq_ (Branch scrut scrutTy es) =
    "case" <> parens (expr_ True scrut) <> line <>
      (indent 2 $ vcat $ conds es) <> line <>
    "endcase"
   where
        conds :: [(Maybe Literal,[Seq])] -> SystemVerilogM [Doc]
        conds [] =
          return []
        conds [(_,sq)] =
          ("default" <+> colon <+> "begin" <> line <>
            indent 2 (seqs sq) <> line <>
          "end") <:> return []
        conds ((Nothing,sq):_) =
          ("default" <+> colon <+> "begin" <> line <>
            indent 2 (seqs sq) <> line <>
          "end") <:> return []
        conds ((Just c ,sq):es') =
          (exprLitSV (Just (scrutTy,conSize scrutTy)) c <+> colon <+> "begin" <> line <>
            indent 2 (seqs sq) <> line <>
          "end") <:> conds es'

seq_ (SeqDecl sd) = case sd of
  Assignment id_ e ->
    stringS id_ <+> equals <+> expr_ False e <> semi

  BlackBoxD {} ->
    fromMaybe <$> emptyDoc <*> inst_ sd

  Seq ds ->
    seqs ds

  _ -> error (show sd)

seqs :: [Seq] -> SystemVerilogM Doc
seqs [] = emptyDoc
seqs (SeqDecl (TickDecl id_):ds) = "//" <+> stringS id_ <> line <> seqs ds
seqs (d:ds) = seq_ d <> line <> line <> seqs ds

-- | Turn a Netlist expression into a SystemVerilog expression
expr_ :: Bool -- ^ Enclose in parentheses?
      -> Expr -- ^ Expr to convert
      -> SystemVerilogM Doc
expr_ _ (Literal sizeM lit) = exprLitSV sizeM lit
expr_ _ (Identifier id_ Nothing) = stringS id_
expr_ _ (Identifier id_ (Just (Indexed (CustomSP _id dataRepr _size args,dcI,fI)))) =
  case fieldTy of
    Void {} ->
      error (unexpectedProjectionErrorMsg dataRepr dcI fI)
    _ ->
      expFromSLV fieldTy (braces $ hcat $ punctuate ", " $ sequence ranges)
 where
  (ConstrRepr' _name _n _mask _value anns, _, fieldTypes) = args !! dcI
  ranges = map range' $ bitRanges (anns !! fI)
  range' (start, end) = stringS id_ <> brackets (int start <> ":" <> int end)
  fieldTy = indexNote ($(curLoc) ++ "panic") fieldTypes fI

expr_ _ (Identifier id_ (Just (Indexed (CustomProduct _id dataRepr _size _maybeFieldNames args,dcI,fI)))) =
  case fieldTy of
    Void {} ->
      error (unexpectedProjectionErrorMsg dataRepr dcI fI)
    _ ->
      expFromSLV fieldTy (braces $ hcat $ punctuate ", " $ sequence ranges)
 where
  (anns, fieldTypes) = unzip args
  ranges = map range' $ bitRanges (anns !! fI)
  range' (start, end) = stringS id_ <> brackets (int start <> ":" <> int end)
  fieldTy = indexNote ($(curLoc) ++ "panic") fieldTypes fI

expr_ _ (Identifier id_ (Just (Indexed (ty@(SP _ args),dcI,fI)))) = fromSLV argTy id_ start end
  where
    argTys   = snd $ args !! dcI
    argTy    = argTys !! fI
    argSize  = typeSize argTy
    other    = otherSize argTys (fI-1)
    start    = typeSize ty - 1 - conSize ty - other
    end      = start - argSize + 1

expr_ _ (Identifier id_ (Just (Indexed (ty@(Product _ _ tys),_,fI)))) = do
  id'<- fmap (Text.toStrict . renderOneLine) (stringS id_ <> dot <> tyName ty <> "_sel" <> int fI)
  simpleFromSLV (tys !! fI) id'

expr_ _ (Identifier id_ (Just (Indexed ((Vector _ elTy),1,0)))) = do
  id' <- fmap (Text.toStrict . renderOneLine) (stringS id_ <> brackets (int 0))
  simpleFromSLV elTy id'

expr_ _ (Identifier id_ (Just (Indexed ((Vector n _),1,1)))) = stringS id_ <> brackets (int 1 <> colon <> int (n-1))

-- This is a "Hack", we cannot construct trees with a negative depth. This is
-- here so that we can recognise merged RTree modifiers. See the code in
-- @Clash.Backend.nestM@ which construct these tree modifiers.
expr_ _ (Identifier id_ (Just (Indexed (RTree (-1) _,l,r)))) =
  stringS id_ <> brackets (int l <> colon <> int (r-1))

expr_ _ (Identifier id_ (Just (Indexed ((RTree 0 elTy),0,0)))) = do
  id' <- fmap (Text.toStrict . renderOneLine) (stringS id_ <> brackets (int 0))
  simpleFromSLV elTy id'

expr_ _ (Identifier id_ (Just (Indexed ((RTree n _),1,0)))) =
  let z = 2^(n-1)
  in  stringS id_ <> brackets (int 0 <> colon <> int (z-1))

expr_ _ (Identifier id_ (Just (Indexed ((RTree n _),1,1)))) =
  let z  = 2^(n-1)
      z' = 2^n
  in stringS id_ <> brackets (int z <> colon <> int (z'-1))

-- This is a HACK for Clash.Driver.TopWrapper.mkOutput
-- Vector's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
expr_ _ (Identifier id_ (Just (Indexed ((Vector _ elTy),10,fI)))) = do
  id' <- fmap (Text.toStrict . renderOneLine) (stringS id_ <> brackets (int fI))
  simpleFromSLV elTy id'

-- This is a HACK for Clash.Driver.TopWrapper.mkOutput
-- RTree's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
expr_ _ (Identifier id_ (Just (Indexed ((RTree _ elTy),10,fI)))) = do
  id' <- fmap (Text.toStrict . renderOneLine) (stringS id_ <> brackets (int fI))
  simpleFromSLV elTy id'

expr_ _ (Identifier id_ (Just (DC (ty@(SP _ _),_)))) = stringS id_ <> brackets (int start <> colon <> int end)
  where
    start = typeSize ty - 1
    end   = typeSize ty - conSize ty

expr_ _ (Identifier id_ (Just m@Nested {})) = case modifier 0 [] m of
  Nothing -> stringS id_
  Just (mods,resTy) -> do
    nm <- Mon $ use modNm
    pkgCtx <- Mon $ use tyPkgCtx
    let prefix = if pkgCtx then emptyDoc else stringS nm <> "_types::"
    let e = stringS id_ <> hcat (mapM (either bracketNMod bracketNMod) (reverse mods))
    case resTy of
      Signed _ -> "$signed" <> parens e
      Vector {}
        | Left (NRange {}):_ <- mods
        -> e
        | otherwise  -> do
        Mon (tyCache %= HashSet.insert resTy)
        prefix <> tyName resTy <> "_from_lv" <> parens e
      RTree {}
        | Left (NRange {}):_ <- mods
        -> e
        | otherwise -> do
        Mon (tyCache %= HashSet.insert resTy)
        prefix <> tyName resTy <> "_from_lv" <> parens e
      _ -> e
 where
  bracketNMod (NElem i)    = brackets (int i)
  bracketNMod (NRange s e) = brackets (int s <> colon <> int e)

-- See [Note] integer projection
expr_ _ (Identifier id_ (Just (Indexed ((Signed w),_,_))))  = do
  iw <- Mon $ use intWidth
  traceIf (iw < w) ($(curLoc) ++ "WARNING: result smaller than argument") $
    stringS id_

-- See [Note] integer projection
expr_ _ (Identifier id_ (Just (Indexed ((Unsigned w),_,_))))  = do
  iw <- Mon $ use intWidth
  traceIf (iw < w) ($(curLoc) ++ "WARNING: result smaller than argument") $
    stringS id_

-- See [Note] mask projection
expr_ _ (Identifier _ (Just (Indexed ((BitVector _),_,0)))) = do
  iw <- Mon $ use intWidth
  traceIf True ($(curLoc) ++ "WARNING: synthesizing bitvector mask to dontcare") $
    verilogTypeErrValue (Signed iw)

-- See [Note] bitvector projection
expr_ _ (Identifier id_ (Just (Indexed ((BitVector w),_,1)))) = do
  iw <- Mon $ use intWidth
  traceIf (iw < w) ($(curLoc) ++ "WARNING: result smaller than argument") $
    stringS id_

expr_ _ (Identifier id_ (Just (Sliced ((BitVector _,start,end))))) =
  stringS id_ <> brackets (int start <> ":" <> int end)

expr_ _ (Identifier id_ (Just _)) = stringS id_

expr_ b (DataCon _ (DC (Void {}, -1)) [e]) =  expr_ b e

expr_ _ (DataCon ty@(Vector 0 _) _ _) = verilogTypeErrValue ty

expr_ _ (DataCon (Vector 1 elTy) _ [e]) = "'" <> braces (toSLV elTy e)

expr_ _ e@(DataCon ty@(Vector _ elTy) _ [e1,e2]) = case vectorChain e of
  Just es -> "'" <> listBraces (mapM (toSLV elTy) es)
  Nothing -> verilogTypeMark ty <> "_cons" <> parens (expr_ False e1 <> comma <+> expr_ False e2)

expr_ _ (DataCon (RTree 0 elTy) _ [e]) = "'" <> braces (toSLV elTy e)

expr_ _ e@(DataCon ty@(RTree _ elTy) _ [e1,e2]) = case rtreeChain e of
  Just es -> "'" <> listBraces (mapM (toSLV elTy) es)
  Nothing -> verilogTypeMark ty <> "_br" <> parens (expr_ False e1 <> comma <+> expr_ False e2)

expr_ _ (DataCon (SP {}) (DC (BitVector _,_)) es) = assignExpr
  where
    argExprs   = map (expr_ False) es
    assignExpr = braces (hcat $ punctuate comma $ sequence argExprs)

expr_ _ (DataCon ty@(SP _ args) (DC (_,i)) es) = assignExpr
  where
    argTys     = snd $ args !! i
    dcSize     = conSize ty + sum (map typeSize argTys)
    dcExpr     = expr_ False (dcToExpr ty i)
    argExprs   = zipWith toSLV argTys es
    extraArg   = case typeSize ty - dcSize of
                   0 -> []
                   n -> [int n <> "'b" <> bits undefValue (replicate n U)]
    assignExpr = braces (hcat $ punctuate comma $ sequence (dcExpr:argExprs ++ extraArg))

expr_ _ (DataCon ty@(Sum _ _) (DC (_,i)) []) = int (typeSize ty) <> "'d" <> int i
expr_ _ (DataCon ty@(CustomSum _ _ _ tys) (DC (_,i)) []) =
  let (ConstrRepr' _ _ _ value _) = fst $ tys !! i in
  int (typeSize ty) <> squote <> "d" <> int (fromIntegral value)
expr_ _ (DataCon (CustomSP _ dataRepr _size args) (DC (_,i)) es) =
  let (cRepr, _, argTys) = args !! i in
  customReprDataCon dataRepr cRepr (zip argTys es)
expr_ _ (DataCon (CustomProduct _ dataRepr _size _labels tys) _ es) |
  DataRepr' _typ _size [cRepr] <- dataRepr =
  customReprDataCon dataRepr cRepr (zip (map snd tys) es)

expr_ _ (DataCon (Product _ _ tys) _ es) = listBraces (zipWithM toSLV tys es)

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "Clash.Sized.Internal.Signed.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLitSV (Just (Signed (fromInteger n),fromInteger n)) i

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "Clash.Sized.Internal.Unsigned.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLitSV (Just (Unsigned (fromInteger n),fromInteger n)) i

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "Clash.Sized.Internal.BitVector.fromInteger#"
  , [Literal _ (NumLit n), Literal _ (NumLit m), Literal _ (NumLit i)] <- extractLiterals bbCtx
  = exprLitSV (Just (BitVector (fromInteger n),fromInteger n)) (BitVecLit m i)

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "Clash.Sized.Internal.BitVector.fromInteger##"
  , [Literal _ m, Literal _ i] <- extractLiterals bbCtx
  = let NumLit m' = m
        NumLit i' = i
    in exprLitSV (Just (Bit,1)) (BitLit $ toBit m' i')

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "Clash.Sized.Internal.Index.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLitSV (Just (Index (fromInteger n),fromInteger n)) i

expr_ b (BlackBoxE _ libs imps inc bs bbCtx b') =
  parenIf (b || b') (Mon (renderBlackBox libs imps inc bs bbCtx <*> pure 0))

expr_ _ (DataTag Bool (Left id_))          = stringS id_ <> brackets (int 0)
expr_ _ (DataTag Bool (Right id_))         = do
  iw <- Mon $ use intWidth
  "$unsigned" <> parens (listBraces (sequence [braces (int (iw-1) <+> braces "1'b0"),stringS id_]))

expr_ _ (DataTag (Sum _ _) (Left id_))     = "$unsigned" <> parens (stringS id_)
expr_ _ (DataTag (Sum _ _) (Right id_))    = "$unsigned" <> parens (stringS id_)

expr_ _ (DataTag (Product {}) (Right _))  = do
  iw <- Mon $ use intWidth
  int iw <> "'sd0"

expr_ _ (DataTag hty@(SP _ _) (Right id_)) = "$unsigned" <> parens
                                               (stringS id_ <> brackets
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

expr_ b (ConvBV topM t True e) = do
  nm <- Mon $ use modNm
  pkgCtx <- Mon $ use tyPkgCtx
  let prefix = if pkgCtx then emptyDoc else stringS nm <> "_types::"
  case t of
    Vector {} -> do
      Mon (tyCache %= HashSet.insert t)
      maybe prefix ((<> "_types::") . stringS) topM <>
        tyName t <> "_to_lv" <> parens (expr_ False e)
    RTree {} -> do
      Mon (tyCache %= HashSet.insert t)
      maybe prefix ((<> "_types::") . stringS) topM <>
        tyName t <> "_to_lv" <> parens (expr_ False e)
    _ -> expr b e

expr_ b (ConvBV topM t False e) = do
  nm <- Mon $ use modNm
  pkgCtx <- Mon $ use tyPkgCtx
  let prefix = if pkgCtx then emptyDoc else stringS nm <> "_types::"
  case t of
    Vector {} -> do
      Mon (tyCache %= HashSet.insert t)
      maybe prefix ((<> "_types::") . stringS) topM <>
        tyName t <> "_from_lv" <> parens (expr_ False e)
    RTree {} -> do
      Mon (tyCache %= HashSet.insert t)
      maybe prefix ((<> "_types::") . stringS) topM <>
        tyName t <> "_from_lv" <> parens (expr_ False e)
    _ -> expr b e

expr_ b (IfThenElse c t e) =
  parenIf b (expr_ True c <+> "?" <+> expr_ True t <+> ":" <+> expr_ True e)

expr_ _ e = error $ $(curLoc) ++ (show e) -- empty

exprLitSV :: Maybe (HWType,Size) -> Literal -> SystemVerilogM Doc
exprLitSV = exprLit undefValue

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
rtreeChain (DataCon (RTree _ _) _ [e1,e2]) = A.liftA2 (++) (rtreeChain e1)
                                                           (rtreeChain e2)
rtreeChain _                               = Nothing

toSLV :: HWType -> Expr -> SystemVerilogM Doc
toSLV t e = case t of
  Vector _ _ -> braces (verilogTypeMark t <> "_to_lv" <> parens (expr_ False e))
  RTree _ _ -> braces (verilogTypeMark t <> "_to_lv" <> parens (expr_ False e))
  _ -> expr_ False e

fromSLV :: HWType -> Identifier -> Int -> Int -> SystemVerilogM Doc
fromSLV t@(Vector _ _) id_ start end = verilogTypeMark t <> "_from_lv" <> parens (stringS id_ <> brackets (int start <> colon <> int end))
fromSLV t@(RTree _ _) id_ start end = verilogTypeMark t <> "_from_lv" <> parens (stringS id_ <> brackets (int start <> colon <> int end))
fromSLV (Signed _) id_ start end = "$signed" <> parens (stringS id_ <> brackets (int start <> colon <> int end))
fromSLV _ id_ start end = stringS id_ <> brackets (int start <> colon <> int end)

simpleFromSLV :: HWType -> Identifier -> SystemVerilogM Doc
simpleFromSLV t@(Vector _ _) id_ = verilogTypeMark t <> "_from_lv" <> parens (stringS id_)
simpleFromSLV t@(RTree _ _) id_ = verilogTypeMark t <> "_from_lv" <> parens (stringS id_)
simpleFromSLV (Signed _) id_ = "$signed" <> parens (stringS id_)
simpleFromSLV _ id_ = stringS id_

expFromSLV :: HWType -> SystemVerilogM Doc -> SystemVerilogM Doc
expFromSLV t@(Vector _ _) exp_ = verilogTypeMark t <> "_from_lv" <> parens exp_
expFromSLV t@(RTree _ _) exp_ = verilogTypeMark t <> "_from_lv" <> parens exp_
expFromSLV (Signed _) exp_ = "$signed" <> parens exp_
expFromSLV _ exp_ = exp_

dcToExpr :: HWType -> Int -> Expr
dcToExpr ty i = Literal (Just (ty,conSize ty)) (NumLit (toInteger i))

listBraces :: Monad m => m [Doc] -> m Doc
listBraces = align . encloseSep lbrace rbrace comma

parenIf :: Monad m => Bool -> m Doc -> m Doc
parenIf True  = parens
parenIf False = id

punctuate' :: Monad m => Mon m Doc -> Mon m [Doc] -> Mon m Doc
punctuate' s d = vcat (punctuate s d) <> s


data NMod
  = NRange Int Int
  | NElem Int

-- | Calculate the beginning and end index into a variable, to get the
-- desired field. Also returns the HWType of the result.
--
-- NB: returns a list of slices and indices when selections are into vectors and
-- rtrees. Left -> index/slice from an unpacked array; Right -> slice from a
-- packed type
modifier
  :: Int
  -- ^ Offset, only used when we have nested modifiers
  -> [Either NMod NMod]
  -- ^ Ranges selected so far
  -> Modifier
  -> Maybe ([Either NMod NMod],HWType)
modifier offset mods (Sliced (BitVector _,start,end)) =
  let m = Right (NRange (start+offset) (end+offset)) in
  case mods of
    Right {}:rest -> Just (m:rest, BitVector (start-end+1))
    _ -> Just (m:mods, BitVector (start-end+1))

modifier offset mods (Indexed (ty@(SP _ args),dcI,fI)) =
  case mods of
    Right {}:rest -> Just (m:rest, argTy)
    _ -> Just (m:mods,argTy)
  where
    argTys   = snd $ args !! dcI
    argTy    = argTys !! fI
    argSize  = typeSize argTy
    other    = otherSize argTys (fI-1)
    start    = typeSize ty - 1 - conSize ty - other
    end      = start - argSize + 1
    m        = Right (NRange (start+offset) (end+offset))

modifier offset mods (Indexed (ty@(Product _ _ argTys),_,fI)) =
  let m = Right (NRange (start+offset) (end+offset)) in
  case mods of
    Right {}:rest -> Just (m:rest, argTy)
    _ -> Just (m:mods,argTy)
  where
    argTy   = argTys !! fI
    argSize = typeSize argTy
    otherSz = otherSize argTys (fI - 1)
    start   = typeSize ty - 1 - otherSz
    end     = start - argSize + 1

modifier offset mods (Indexed (ty@(Vector _ argTy),1,0)) = case mods of
    Right {}:rest -> Just (Right (NRange (start+offset) (end+offset)):rest, argTy)
    Left (NRange b _):rest -> Just (Left (NElem b):rest,argTy)
    _ -> Just (Left (NElem 0):mods,argTy)
  where
    argSize = typeSize argTy
    start   = typeSize ty - 1
    end     = start - argSize + 1

modifier offset mods (Indexed (ty@(Vector n argTy),1,1)) = case mods of
    Right {}:rest -> Just (Right (NRange (start+offset) offset):rest, Vector (n-1) argTy)
    Left (NRange b e):rest -> Just (Left (NRange (b+1) e):rest, Vector (n-1) argTy)
    _ -> Just (Left (NRange 1 (n-1)):mods, Vector (n-1) argTy)
  where
    argSize = typeSize argTy
    start   = typeSize ty - argSize - 1

modifier offset mods (Indexed (ty@(RTree 0 argTy),0,0)) = case mods of
    Right {}:rest -> Just (Right (NRange (start+offset) offset):rest, argTy)
    Left (NRange b _):rest -> Just (Left (NElem b):rest,argTy)
    _ -> Just (Left (NElem 0):mods,argTy)
  where
    start   = typeSize ty - 1

modifier offset mods (Indexed (ty@(RTree d argTy),1,0)) = case mods of
    Right {}:rest -> Just (Right (NRange (start+offset) (end+offset)):rest, RTree (d-1) argTy)
    Left (NRange b _):rest -> Just (Left (NRange b (b+lhsSz-1)):rest,RTree (d-1) argTy)
    _ -> Just (Left (NRange 0 (lhsSz-1)):mods,RTree (d-1) argTy)
  where
    start   = typeSize ty - 1
    end     = typeSize ty `div` 2
    lhsSz   = (d-1)^(2 :: Int)

modifier offset mods (Indexed (ty@(RTree d argTy),1,1)) = case mods of
    Right {}:rest -> Just (Right (NRange (start+offset) offset):rest, RTree  (d-1) argTy)
    Left (NRange _ e):rest -> Just (Left (NRange (e+1-rhsS) e):rest,RTree (d-1) argTy)
    _ -> Just (Left (NRange rhsS rhsE):mods,RTree (d-1) argTy)
  where
    start   = (typeSize ty `div` 2) - 1
    rhsS    = (d-1)^(2 :: Int)
    rhsE    = d^(2 :: Int)-1

-- This is a HACK for Clash.Driver.TopWrapper.mkOutput
-- Vector's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
modifier offset mods (Indexed (ty@(Vector _ argTy),10,fI)) = case mods of
    Right {}:rest -> Just (Right (NRange (start+offset) (end+offset)):rest, argTy)
    Left (NRange b _):rest -> Just (Left (NElem (fI+b)):rest, argTy)
    _ -> Just (Left (NElem fI):mods,argTy)
  where
    argSize = typeSize argTy
    start   = typeSize ty - (fI * argSize) - 1
    end     = start - argSize + 1

-- This is a HACK for Clash.Driver.TopWrapper.mkOutput
-- RTree's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
modifier offset mods (Indexed (ty@(RTree _ argTy),10,fI)) = case mods of
    Right {}:rest -> Just (Right (NRange (start+offset) (end+offset)):rest, argTy)
    Left (NRange b _):rest -> Just (Left (NElem (b+fI)):rest, argTy)
    _ -> Just (Left (NElem fI):mods, argTy)
  where
    argSize = typeSize argTy
    start   = typeSize ty - (fI * argSize) - 1
    end     = start - argSize + 1

modifier offset mods (Indexed (CustomSP typName _dataRepr _size args,dcI,fI)) =
  case bitRanges (anns !! fI) of
    [(start,end)] ->
      let m = Right (NRange (start+offset) (end+offset)) in
      case mods of
        Right {}:rest -> Just (m:rest, argTy)
        _ -> Just (m:mods, argTy)
    _ ->
      error $ $(curLoc) ++ "Cannot handle projection out of a "
           ++ "non-contiguously or zero-width encoded field. Tried to project "
           ++ "field " ++ show fI ++ " of constructor " ++ show dcI ++ " of "
           ++ "data type " ++ show typName ++  "."
 where
  (ConstrRepr' _name _n _mask _value anns, _, argTys) = args !! dcI
  argTy = argTys !! fI

modifier offset mods (Indexed (CustomProduct typName dataRepr _size _maybeFieldNames args,dcI,fI))
  | DataRepr' _typ _size [cRepr] <- dataRepr
  , ConstrRepr' _cName _pos _mask _val fieldAnns <- cRepr =
  case bitRanges (fieldAnns !! fI) of
    [(start,end)] ->
      let m = Right (NRange (start+offset) (end+offset)) in
      case mods of
        Right {}:rest -> Just (m:rest, argTy)
        _ -> Just (m:mods,argTy)
    _ ->
      error $ $(curLoc) ++ "Cannot handle projection out of a "
           ++ "non-contiguously or zero-width encoded field. Tried to project "
           ++ "field " ++ show fI ++ " of constructor " ++ show dcI ++ " of "
           ++ "data type " ++ show typName ++ "."
 where
  argTy = map snd args !! fI

modifier offset mods (DC (ty@(SP _ _),_)) =
    let m = Right (NRange (start+offset) (end+offset)) in
    case mods of
      Right {}:rest -> Just (m:rest, ty)
      _ -> Just (m:mods,ty)
  where
    start = typeSize ty - 1
    end   = typeSize ty - conSize ty

modifier offset mods (Nested m1 m2) = do
  case modifier offset mods m1 of
    Nothing -> modifier offset mods m2
    Just (mods1,argTy) ->
      let m3 = case mods1 of
                 Right (NRange _ e):_ -> modifier e mods1 m2
                 _ -> modifier 0 mods1 m2
      in case m3 of
        -- In case the second modifier is `Nothing` that means we want the entire
        -- thing calculated by the first modifier
        Nothing -> Just (mods1,argTy)
        m       -> m

modifier _ _ _ = Nothing
