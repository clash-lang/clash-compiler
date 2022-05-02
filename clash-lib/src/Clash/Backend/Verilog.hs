{-|
  Copyright   :  (C) 2015-2016, University of Twente,
                     2017-2018, Google Inc.,
                     2021-2022, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Generate Verilog for assorted Netlist datatypes
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Backend.Verilog
  ( VerilogState
  , include
  , uselibs
  , encodingNote
  , exprLit
  , bits
  , bit_char
  , noEmptyInit
  -- * split ranges
  , Range (..)
  , continueWithRange
  )
where

import qualified Control.Applicative                  as A
import           Control.Lens                         (Lens',(.=),(%=), makeLenses, use)
import           Control.Monad                        (forM)
import           Control.Monad.State                  (State)
import           Data.Bifunctor                       (first, second)
import           Data.Bits                            (Bits, testBit)
import qualified Data.ByteString.Char8                as B8
import           Data.Coerce                          (coerce)
import           Data.Function                        (on)
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as HashMap
import           Data.HashSet                         (HashSet)
import qualified Data.HashSet                         as HashSet
import           Data.Maybe                           (catMaybes, fromMaybe)
import           Data.Monoid                          (Ap(Ap))
import           Data.Monoid.Extra                    ()
import           Data.List
  (mapAccumL, mapAccumR, nubBy, foldl')
import           Data.List.Extra                      ((<:>))
import           Data.Text.Lazy                       (pack)
import qualified Data.Text.Lazy                       as Text
import qualified Data.Text                            as TextS
import           Data.Text.Prettyprint.Doc.Extra
import qualified System.FilePath
import           GHC.Stack                            (HasCallStack)

import           Clash.Annotations.Primitive          (HDL (..))
import           Clash.Annotations.BitRepresentation.ClashLib
  (bitsToBits)
import           Clash.Annotations.BitRepresentation.Internal
  (ConstrRepr'(..), DataRepr'(..), ConstrRepr'(..))
import           Clash.Annotations.BitRepresentation.Util
  (BitOrigin(Lit, Field), bitOrigins, bitRanges)
import           Clash.Core.Var                       (Attr'(..))
import           Clash.Backend
import           Clash.Debug                          (traceIf)
import           Clash.Driver.Types                   (ClashOpts(..))
import           Clash.Explicit.BlockRam.Internal     (unpackNats)
import           Clash.Netlist.BlackBox.Types         (HdlSyn)
import           Clash.Netlist.BlackBox.Util
  (extractLiterals, renderBlackBox, renderFilePath)
import qualified Clash.Netlist.Id                     as Id
import           Clash.Netlist.Types                  hiding (_intWidth, intWidth)
import           Clash.Netlist.Util
import           Clash.Signal.Internal                (ActiveEdge (..))
import           Clash.Util
  (SrcSpan, noSrcSpan, curLoc, indexNote, makeCached)

-- | State for the 'Clash.Backend.Verilog.VerilogM' monad:
data VerilogState =
  VerilogState
    { _idSeen    :: IdentifierSet
    , _topNm     :: Identifier
    , _srcSpan   :: SrcSpan
    , _includes  :: [(String,Doc)]
    , _imports   :: HashSet Text.Text
    , _libraries :: HashSet Text.Text
    , _dataFiles      :: [(String,FilePath)]
    -- ^ Files to be copied: (filename, old path)
    , _memoryDataFiles:: [(String,String)]
    -- ^ Files to be stored: (filename, contents). These files are generated
    -- during the execution of 'genNetlist'.
    , _customConstrs :: HashMap TextS.Text Identifier
    -- ^ Custom data constructor => Verilog function name
    , _intWidth  :: Int -- ^ Int/Word/Integer bit-width
    , _hdlsyn    :: HdlSyn
    , _undefValue :: Maybe (Maybe Int)
    , _aggressiveXOptBB_ :: AggressiveXOptBB
    , _domainConfigurations_ :: DomainMap
    }

makeLenses ''VerilogState

instance HasIdentifierSet VerilogState where
  identifierSet = idSeen

instance Backend VerilogState where
  initBackend opts = VerilogState
    { _idSeen=Id.emptyIdentifierSet (opt_escapedIds opts) (opt_lowerCaseBasicIds opts) Verilog
    , _topNm=Id.unsafeMake ""
    , _srcSpan=noSrcSpan
    , _includes=[]
    , _imports=HashSet.empty
    , _libraries=HashSet.empty
    , _dataFiles=[]
    , _memoryDataFiles=[]
    , _customConstrs=HashMap.empty
    , _intWidth=opt_intWidth opts
    , _hdlsyn=opt_hdlSyn opts
    , _undefValue=opt_forceUndefined opts
    , _aggressiveXOptBB_=coerce (opt_aggressiveXOptBB opts)
    , _domainConfigurations_=emptyDomainMap
    }
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
  hdlHWTypeKind _ = pure PrimitiveType -- Everything is a bitvector!
  hdlTypeErrValue = verilogTypeErrValue
  hdlTypeMark     = verilogTypeMark
  hdlRecSel       = verilogRecSel
  hdlSig t ty     = sigDecl (string t) ty
  inst            = inst_
  expr            = expr_
  iwWidth         = use intWidth
  toBV ty e       = case ty of
    Signed _ -> "$unsigned" <> parens (string e)
    _ -> string e
  fromBV ty e     = case ty of
    Signed _ -> "$signed" <> parens (string e)
    _ -> string e
  hdlSyn          = use hdlsyn
  setModName _    = id
  setTopName nm s = s {_topNm = nm}
  getTopName      = use topNm
  setSrcSpan      = (srcSpan .=)
  getSrcSpan      = use srcSpan
  blockDecl _ ds  = do
    decs <- decls ds
    if isEmpty decs
      then insts ds
      else
        pure decs <> line <>
        insts ds
  addIncludes inc = includes %= (inc ++)
  addLibraries libs = libraries %= (\s -> foldl' (flip HashSet.insert) s libs)
  addImports inps = imports %= (\s -> foldl' (flip HashSet.insert) s inps)
  addAndSetData f = do
    fs <- use dataFiles
    let (fs',f') = renderFilePath fs f
    dataFiles .= fs'
    return f'
  getDataFiles = use dataFiles
  addMemoryDataFile f = memoryDataFiles %= (f:)
  getMemoryDataFiles = use memoryDataFiles
  ifThenElseExpr _ = True
  aggressiveXOptBB = use aggressiveXOptBB_
  renderEnums = pure (RenderEnums False)
  domainConfigurations = use domainConfigurations_
  setDomainConfigurations confs s = s {_domainConfigurations_ = confs}

type VerilogM a = Ap (State VerilogState) a

-- | Generate Verilog for a Netlist component
genVerilog
  :: SrcSpan
  -> IdentifierSet
  -> Component
  -> VerilogM ((String, Doc), [(String, Doc)])
genVerilog sp seen c = do
    -- Don't have type names conflict with module names or with previously
    -- generated type names.
    --
    -- TODO: Collect all type names up front, to prevent relatively costly union.
    -- TODO: Investigate whether type names / signal names collide in the first place
    Ap $ idSeen %= Id.union seen

    Ap (setSrcSpan sp)
    v    <- commentHeader <> line <> timescale <> line <> module_ c
    incs <- Ap $ use includes
    return ((TextS.unpack (Id.toText cName), v), incs)
  where
    cName    = componentName c
    commentHeader
         = "/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE."
      <> line <> "** GENERATED BY CLASH " <> string (Text.pack clashVer) <> ". DO NOT MODIFY."
      <> line <> "*/"
    timescale = "`timescale 100fs/100fs"

sigPort
  :: Maybe WireOrReg
  -> Identifier
  -> HWType
  -> Maybe Expr
  -> VerilogM Doc
sigPort wor (Id.toText -> pName) hwType iEM =
    addAttrs (hwTypeAttrs hwType)
      (portType <+> verilogType hwType <+> stringS pName <> iE <> encodingNote hwType)
  where
    portType = case wor of
                 Nothing   -> if isBiSignalIn hwType then "inout" else "input"
                 Just Wire -> "output" <+> "wire"
                 Just Reg  -> "output" <+> "reg"

    iE = maybe emptyDoc (noEmptyInit . expr_ False) iEM

module_ :: Component -> VerilogM Doc
module_ c =
  modVerilog <* Ap (imports .= HashSet.empty >> libraries .= HashSet.empty)
  where
    modVerilog = do
      body <- modBody
      imps <- Ap $ use imports
      libs <- Ap $ use libraries
      modHeader <> line <> modPorts <> line <>
        include (HashSet.toList imps) <>
        uselibs (HashSet.toList libs) <>
        pure body <> line <> modEnding

    modHeader  = "module" <+> pretty (componentName c)
    modPorts   = indent 4 (tupleInputs inPorts <> line <> tupleOutputs outPorts <> semi)
    modBody    = indent 2 (decls (declarations c)) <> line <> line <> indent 2 (insts (declarations c))
    modEnding  = "endmodule"

    inPorts  = sequence [ sigPort Nothing id_ hwType Nothing | (id_, hwType) <- inputs c  ]
    outPorts = sequence [ sigPort (Just wireOrReg) id_ hwType iEM | (wireOrReg, (id_, hwType), iEM) <- outputs c ]

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

include :: Monad m => [Text.Text] -> Ap m Doc
include [] = emptyDoc
include xs = line <>
  indent 2 (vcat (mapM (\i -> string "`include" <+> dquotes (string i)) xs))
  <> line <> line

uselibs :: Monad m => [Text.Text] -> Ap m Doc
uselibs [] = emptyDoc
uselibs xs = line <>
  -- NOTE: We must produce a single uselib directive as later ones overwrite earlier ones.
  indent 2 (string "`uselib" <+> (hsep (mapM (\l -> ("lib=" <> string l)) xs)))
  <> line <> line

wireRegFileDoc :: WireOrReg -> (Either a HWType) -> VerilogM Doc
wireRegFileDoc _    (Right FileType) = "integer"
wireRegFileDoc Wire _                = "wire"
wireRegFileDoc Reg  _                = "reg"

verilogType :: HWType -> VerilogM Doc
verilogType t = case t of
  Signed n -> "signed" <+> brackets (int (n-1) <> colon <> int 0)
  Clock {} -> emptyDoc
  Reset {} -> emptyDoc
  Enable {} -> emptyDoc
  Bit      -> emptyDoc
  Bool     -> emptyDoc
  FileType -> emptyDoc
  Annotated _ ty -> verilogType ty
  _        -> brackets (int (typeSize t -1) <> colon <> int 0)

sigDecl :: VerilogM Doc -> HWType -> VerilogM Doc
sigDecl d t = verilogType t <+> d

-- | Convert a Netlist HWType to the root of a Verilog type
verilogTypeMark :: HWType -> VerilogM Doc
verilogTypeMark = const emptyDoc

-- | Convert a Netlist HWType to an error Verilog value for that type
verilogTypeErrValue :: HWType -> VerilogM Doc
verilogTypeErrValue ty = do
  udf <- Ap (use undefValue)
  case udf of
    Nothing       -> braces (int (typeSize ty) <+> braces "1'bx")
    Just Nothing  -> int (typeSize ty) <> "'d0 /* undefined */"
    Just (Just x) -> braces (int (typeSize ty) <+> braces ("1'b" <> int x)) <+> "/* undefined */"

verilogRecSel
  :: HWType
  -> Int
  -> VerilogM Doc
verilogRecSel ty i = case modifier (Contiguous 0 0) (Indexed (ty,0,i)) of
  Just (Contiguous start end,_resTy) -> brackets (int start <> colon <> int end)
  _ -> error "Can't make a record selector"

decls :: [Declaration] -> VerilogM Doc
decls [] = emptyDoc
decls ds = do
    dsDoc <- catMaybes <$> (mapM decl ds)
    case dsDoc of
      [] -> emptyDoc
      _  -> punctuate' semi (A.pure dsDoc)

-- | Add attribute notation to given declaration
addAttrs
  :: [Attr']
  -> VerilogM Doc
  -> VerilogM Doc
addAttrs []     t = t
addAttrs attrs' t =
  "(*" <+> attrs'' <+> "*)" <+> t
 where
  attrs'' = string $ Text.intercalate ", " (map renderAttr attrs')

-- | Convert single attribute to verilog syntax
renderAttr :: Attr' -> Text.Text
renderAttr (StringAttr'  key value) = pack $ concat [key, " = ", show value]
renderAttr (IntegerAttr' key value) = pack $ concat [key, " = ", show value]
renderAttr (BoolAttr'    key True ) = pack $ concat [key, " = ", "1"]
renderAttr (BoolAttr'    key False) = pack $ concat [key, " = ", "0"]
renderAttr (Attr'        key      ) = pack $ key

decl :: Declaration -> VerilogM (Maybe Doc)
decl (NetDecl' noteM wr id_ tyE iEM) =
  Just A.<$> maybe id addNote noteM (addAttrs attrs (wireRegFileDoc wr tyE <+> tyDec tyE))
  where
    tyDec (Left  ty) = stringS ty <+> pretty id_ <> iE
    tyDec (Right ty) = sigDecl (pretty id_) ty <> iE
    addNote n = mappend ("//" <+> stringS n <> line)
    attrs = fromMaybe [] (hwTypeAttrs A.<$> either (const Nothing) Just tyE)
    iE    = maybe emptyDoc (noEmptyInit . expr_ False) iEM

decl _ = return Nothing

noEmptyInit :: (Monad m, Semigroup (m Doc)) => m Doc -> m Doc
noEmptyInit d = do
  d1 <- d
  if isEmpty d1
     then emptyDoc
     else (space <> string "=" <+> d)

insts :: [Declaration] -> VerilogM Doc
insts [] = emptyDoc
insts (TickDecl (Comment c):ds) = comment "//" c <> line <> insts ds
insts (TickDecl (Directive d):ds) = pretty d <> ";" <> line <> insts ds
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
  -> VerilogM Doc
patLitCustom' size (ConstrRepr' _name _n mask value _anns) =
  int size <> squote <> "b" <> (string $ Text.pack $ stdMatch size mask value)

patLitCustom
  :: HWType
  -> Literal
  -> VerilogM Doc
patLitCustom (CustomSum _name _dataRepr size reprs) (NumLit (fromIntegral -> i)) =
  patLitCustom' size (fst $ reprs !! i)

patLitCustom (CustomSP _name _dataRepr size reprs) (NumLit (fromIntegral -> i)) =
  let (cRepr, _id, _tys) = reprs !! i in
  patLitCustom' size cRepr

patLitCustom hwTy _
  | CustomProduct _name dataRepr size _maybeFieldNames _reprs <- hwTy
  , DataRepr' _typ _size [cRepr] <- dataRepr =
  patLitCustom' size cRepr

patLitCustom x y = error $ $(curLoc) ++ unwords
  [ "You can only pass CustomSP / CustomSum / CustomProduct and a NumLit to "
  , "this function, not", show x, "and", show y ]

patMod :: HWType -> Literal -> Literal
patMod hwTy (NumLit i) = NumLit (i `mod` (2 ^ typeSize hwTy))
patMod _ l = l

-- | Helper function for inst_, handling CustomSP and CustomSum
inst_'
  :: TextS.Text
  -> Expr
  -> HWType
  -> [(Maybe Literal, Expr)]
  -> VerilogM (Maybe Doc)
inst_' id_ scrut scrutTy es = fmap Just $
  "always @(*) begin" <> line <>
    indent 2 casez <> line <>
  "end"
    where
      casez =
        "casez" <+> parens var <> line <>
          indent 2 (conds esNub) <> line <>
        "endcase"

      esMod = map (first (fmap (patMod scrutTy))) es
      esNub = nubBy ((==) `on` fst) esMod
      var   = expr_ True scrut

      conds :: [(Maybe Literal,Expr)] -> VerilogM Doc
      conds []                = error $ $(curLoc) ++ "Empty list of conditions invalid."
      conds [(_,e)]           = "default" <+> ":" <+> stringS id_ <+> "=" <+> expr_ False e <> ";"
      conds ((Nothing,e):_)   = "default" <+> ":" <+> stringS id_ <+> "=" <+> expr_ False e <> ";"
      conds ((Just c ,e):es') =
        mask' <+> ":" <+> stringS id_ <+> "=" <+> expr_ False e <> ";" <> line <> conds es'
          where
            mask' = patLitCustom scrutTy c

-- | Turn a Netlist Declaration to a Verilog concurrent block
inst_ :: Declaration -> VerilogM (Maybe Doc)
inst_ (TickDecl {}) = return Nothing
inst_ (Assignment id_ e) = fmap Just $
  "assign" <+> pretty id_ <+> equals <+> expr_ False e <> semi

inst_ (CondAssignment id_ _ scrut _ [(Just (BoolLit b), l),(_,r)]) = fmap Just $
   "always @(*) begin" <> line <>
   indent 2 ("if" <> parens (expr_ True scrut) <> line <>
               (indent 2 $ pretty id_ <+> equals <+> expr_ False t <> semi) <> line <>
            "else" <> line <>
               (indent 2 $ pretty id_ <+> equals <+> expr_ False f <> semi)) <> line <>
   "end"
  where
    (t,f) = if b then (l,r) else (r,l)

inst_ (CondAssignment id_ _ scrut scrutTy@(CustomSP {}) es) =
  inst_' (Id.toText id_) scrut scrutTy es

inst_ (CondAssignment id_ _ scrut scrutTy@(CustomSum {}) es) =
  inst_' (Id.toText id_) scrut scrutTy es

inst_ (CondAssignment id_ _ scrut scrutTy@(CustomProduct {}) es) =
  inst_' (Id.toText id_) scrut scrutTy es

inst_ (CondAssignment id_ _ scrut scrutTy es) = fmap Just $
    "always @(*) begin" <> line <>
    indent 2 ("case" <> parens (expr_ True scrut) <> line <>
                (indent 2 $ vcat $ punctuate semi (conds (Id.toText id_) es)) <> semi <> line <>
              "endcase") <> line <>
    "end"
  where
    conds :: IdentifierText -> [(Maybe Literal,Expr)] -> VerilogM [Doc]
    conds _ []                = return []
    conds i [(_,e)]           = ("default" <+> colon <+> stringS i <+> equals <+> expr_ False e) <:> return []
    conds i ((Nothing,e):_)   = ("default" <+> colon <+> stringS i <+> equals <+> expr_ False e) <:> return []
    conds i ((Just c ,e):es') = (exprLitV (Just (scrutTy,conSize scrutTy)) c <+> colon <+> stringS i <+> equals <+> expr_ False e) <:> conds i es'

inst_ (InstDecl _ _ attrs nm lbl ps pms0) = fmap Just $
    attrs' <> nest 2 (pretty nm <> params <> pretty lbl <> line <> pms2 <> semi)
  where
    pms2 = case pms0 of
      NamedPortMap pms1 ->
        -- ( .clk (clk_0), .arst (arst_0), ........ )
        let pm i e = dot <> expr_ False i <+> parens (expr_ False e) in
        tupled $ sequence [pm i e | (i,_,_,e) <- pms1]
      IndexedPortMap pms1 ->
         -- ( clk_0, arst_0, ..... )
        tupled $ sequence [expr_ False e | (_,_,e) <- pms1]

    params
      | null ps   = space
      | otherwise = line <> "#" <> tupled (sequence [dot <> expr_ False i <+> parens (expr_ False e) | (i,_,e) <- ps]) <> line
    attrs'
      | null attrs = emptyDoc
      | otherwise  = addAttrs attrs line

inst_ (BlackBoxD _ libs imps inc bs bbCtx) =
  fmap Just (Ap (column (renderBlackBox libs imps inc bs bbCtx)))

inst_ (Seq ds) = Just <$> seqs ds

inst_ (NetDecl' {}) = return Nothing

inst_ (ConditionalDecl cond ds) = Just <$>
  "`ifdef" <+> pretty cond <> line <> indent 2 (insts ds) <> line <> "`endif"

seq_ :: Seq -> VerilogM Doc
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
    conds :: [(Maybe Literal,[Seq])] -> VerilogM [Doc]
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
      (exprLitV (Just (scrutTy,conSize scrutTy)) c <+> colon <+> "begin" <> line <>
        indent 2 (seqs sq) <> line <>
      "end") <:> conds es'

seq_ (SeqDecl sd) = case sd of
  Assignment id_ e ->
    pretty id_ <+> equals <+> expr_ False e <> semi

  BlackBoxD {} ->
    fromMaybe <$> emptyDoc <*> inst_ sd

  Seq ds ->
    seqs ds

  _ -> error ("seq_: " ++ show sd)

seqs :: [Seq] -> VerilogM Doc
seqs [] = emptyDoc
seqs (SeqDecl (TickDecl (Comment c)):ds) = comment "//" c <> line <> seqs ds
seqs (SeqDecl (TickDecl (Directive d)):ds) = pretty d <> ";" <> line <> seqs ds
seqs (d:ds) = seq_ d <> line <> line <> seqs ds

-- | Range slice, can be contiguous, or split into multiple sub-ranges
data Range
  = Contiguous Int Int
  | Split [(Int,Int,Provenance)]

-- | Original index range of a split range element
data Provenance
  = Provenance Int Int

-- | Slice ranges out of a split-range element
inRange
  :: [(Int,Int)]
  -- ^ start and end indexes into the original data type
  -> (Int,Int,Provenance)
  -- ^ Element of a split range
  -> ([(Int,Int)],[(Int,Int,Provenance)])
  -- ^
  -- 1. stand and end indexes to be sliced from the rest of the split range elements
  -- 2. Subset of the current split range for the projected data type
inRange [] _ = ([],[])
inRange ((start,end):ses) orig@(_,endRange,Provenance _ endProvenance) =
{-
The following explains the index calculations

== Start ==
-----------------------------------
|     2     | |    1   | |   0    |  <- split range element number
|15|14|13|12| |10| 9| 8| | 4| 3| 2|  <- split range indexes
-----------------------------------
| 9| 8| 7| 6| | 5| 4| 3| | 2| 1| 0|  <- original indexes of the data type (provenance)
-----------------------------------
                   4          1      <- `start` and `end` index that we want to slice

== split range element 2 ==
startOffset: start(4) - endProvenance(6) = -2

next start: 4
next end:   1

== split range element 1 ==
startOffset: start(4) - endProvenance(3) = 1
endOffSet  : end(1) - endProvenance(3) = -2

startRangeNew: endRange(8) + startOffSet(1) = 9
endRangeNew  : endRange(8)

startProvenanceNew: start(4) - end(1)                    = 3
endProvenanceNew  : startProvenanceNew(3)-startOffset(1) = 2

newSplitRange:
-------
|  1  |
| 9| 8| <- new split range element
-------
| 3| 2| <- index into the projected data type

next start: endProvenance(3) - 1 = 2
next end  : 1

== split range element 0 ==
startOffset: start(2) - endProvenance(0) = 2
endOffset  : end(1) - endProvenance(0)   = 1

startRangeNew: endRange(2) + startOffSet(2) = 4
endRangeNew  : endRange(2) + endOffSet(1)   = 3

startProvenanceNew: start(2) - end(1) = 1
endProvenanceNew  :                   = 0

newSplitRange:
-------
|  0  |
| 4| 3| <- new split range element
-------
| 1| 0| <- index into the projected data type
-}
  let startOffset = start - endProvenance
      endOffset   = end   - endProvenance
  in
  if startOffset >= 0 then
    let startRangeNew = endRange + startOffset
        endRangeNew =
          if endOffset >= 0 then
             endRange + endOffset
          else
            endRange

        startProvenanceNew = start - end
        endProvenanceNew =
          if endOffset >= 0 then
            0
          else
            startProvenanceNew - startOffset

        newSplitRange =
          ( startRangeNew
          , endRangeNew
          , Provenance startProvenanceNew endProvenanceNew)
    in
    if endOffset >= 0 then
      -- try to slice the next start+end in the current split range element
      second (newSplitRange:) (inRange ses orig)
    else
      -- continue the slice in the next split range element
      ((endProvenance-1,end):ses,[newSplitRange])
  else
    -- start offset beyond last bit in the element of the split range
    ((start,end):ses,[])

-- | Create an Split range element
buildSplitRange
  :: Int
  -- ^ Offset
  -> Int
  -- ^ End index into the original data type
  -> (Int,Int)
  -- ^ start and end index for this sub-range
  -> (Int,(Int,Int,Provenance))
buildSplitRange offset eP (s,e) =
  let d = s-e in
  (eP+d+1,(s + offset, e + offset, Provenance (eP+d) eP))

-- | Select a sub-range from a range
continueWithRange
  :: [(Int,Int)]
  -- ^ Starts and ends
  -> HWType
  -- ^ Type of the projection
  -> Range
  -- ^ Range selected so far
  -> (Range, HWType)
continueWithRange ses hty r = case r of
  Contiguous _ offset -> case ses of
    [(start,end)] ->
      (Contiguous (start+offset) (end+offset), hty)
    ses1 ->
      let ses2 = snd (mapAccumR (buildSplitRange offset) 0 ses1) in
      (Split ses2, hty)
  Split rs -> case concat (snd (mapAccumL inRange ses rs)) of
    [] -> error "internal error"
    [(s1,e1,_)] -> (Contiguous s1 e1,hty)
    rs1 -> (Split rs1,hty)

-- | Calculate the beginning and end index into a variable, to get the
-- desired field.
-- Also returns the HWType of the result.
modifier
  :: HasCallStack
  => Range
  -- ^ Range selected so far
  -> Modifier
  -> Maybe (Range,HWType)
modifier r (Sliced (BitVector _,start,end)) =
  Just (continueWithRange [(start,end)] hty r)
  where
    hty = BitVector (start-end-1)


modifier r (Indexed (ty@(SP _ args),dcI,fI)) =
  Just (continueWithRange [(start,end)] argTy r)
  where
    argTys   = snd $ args !! dcI
    argTy    = argTys !! fI
    argSize  = typeSize argTy
    other    = otherSize argTys (fI-1)
    start    = typeSize ty - 1 - conSize ty - other
    end      = start - argSize + 1

modifier r (Indexed (ty@(Product _ _ argTys),_,fI)) =
  Just (continueWithRange [(start,end)] argTy r)
  where
    argTy   = argTys !! fI
    argSize = typeSize argTy
    otherSz = otherSize argTys (fI - 1)
    start   = typeSize ty - 1 - otherSz
    end     = start - argSize + 1

modifier r (Indexed (ty@(Vector _ argTy),1,0)) =
  Just (continueWithRange [(start,end)] argTy r)
  where
    argSize = typeSize argTy
    start   = typeSize ty - 1
    end     = start - argSize + 1

modifier r (Indexed (ty@(Vector n argTy),1,1)) =
  Just (continueWithRange [(start,0)] hty r)
  where
    argSize = typeSize argTy
    start   = typeSize ty - argSize - 1
    hty     = Vector (n-1) argTy

modifier r (Indexed (ty@(RTree 0 argTy),0,0)) =
  Just (continueWithRange [(start,0)] argTy r)
  where
    start   = typeSize ty - 1

modifier r (Indexed (ty@(RTree d argTy),1,0)) =
  Just (continueWithRange [(start,end)] hty r)
  where
    start   = typeSize ty - 1
    end     = typeSize ty `div` 2
    hty     = RTree (d-1) argTy

modifier r (Indexed (ty@(RTree d argTy),1,1)) =
  Just (continueWithRange [(start,0)] hty r)
  where
    start   = (typeSize ty `div` 2) - 1
    hty     = RTree (d-1) argTy

-- This is a HACK for Clash.Netlist.Util.mkTopOutput
-- Vector's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
modifier r (Indexed (ty@(Vector _ argTy),10,fI)) =
  Just (continueWithRange [(start,end)] argTy r)
  where
    argSize = typeSize argTy
    start   = typeSize ty - (fI * argSize) - 1
    end     = start - argSize + 1

-- This is a HACK for Clash.Netlist.Util.mkTopOutput
-- RTree's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
modifier r (Indexed (ty@(RTree _ argTy),10,fI)) =
  Just (continueWithRange [(start,end)] argTy r)
  where
    argSize = typeSize argTy
    start   = typeSize ty - (fI * argSize) - 1
    end     = start - argSize + 1

modifier r (Indexed (CustomSP _typName _dataRepr _size args,dcI,fI)) =
  Just (continueWithRange ses argTy r)
  where
    ses = bitRanges (anns !! fI)
    (ConstrRepr' _name _n _mask _value anns, _, argTys) = args !! dcI
    argTy = argTys !! fI

modifier r (Indexed (CustomProduct _typName dataRepr _size _maybeFieldNames args,_,fI))
  | DataRepr' _typ _size [cRepr] <- dataRepr
  , ConstrRepr' _cName _pos _mask _val fieldAnns <- cRepr
  = let ses = bitRanges (fieldAnns !! fI) in Just (continueWithRange ses argTy r)
 where
  argTy = map snd args !! fI

modifier r (DC (ty@(SP _ _),_)) =
  Just (continueWithRange [(start,end)] ty r)
  where
    start = typeSize ty - 1
    end   = typeSize ty - conSize ty

modifier r (Nested m1 m2) = do
  case modifier r m1 of
    Nothing -> modifier r m2
    Just (r1,argTy) -> case modifier r1 m2 of
      -- In case the second modifier is `Nothing` that means we want the entire
      -- thing calculated by the first modifier
      Nothing -> Just (r1,argTy)
      m       -> m

modifier _ _ = Nothing

-- | Render a data constructor application for data constructors having a
-- custom bit representation.
customReprDataCon
  :: DataRepr'
  -- ^ Custom representation of data type
  -> ConstrRepr'
  -- ^ Custom representation of a specific constructor of @dataRepr@
  -> [(HWType, Expr)]
  -- ^ Arguments applied to constructor
  -> VerilogM Doc
customReprDataCon dataRepr constrRepr [] =
  let origins = bitOrigins dataRepr constrRepr :: [BitOrigin] in
  case origins of
    [Lit (bitsToBits -> ns)] ->
      int (length ns) <> squote <> "b" <> hcat (mapM (bit_char undefValue) ns)
    _ -> error "internal error"
customReprDataCon dataRepr constrRepr args = do
  funId <- mkConstrFunction
  Ap (imports %= HashSet.insert (Text.pack (TextS.unpack (Id.toText funId) ++ ".inc")))
  pretty funId <> tupled (mapM (expr_ False . snd) nzArgs)
 where
  nzArgs = filter ((/=0) . typeSize . fst) args

  mkConstrFunction :: Ap (State VerilogState) Identifier
  mkConstrFunction = makeCached (crName constrRepr) customConstrs $ do
    let size    = drSize dataRepr
        aTys    = map fst args
        origins = bitOrigins dataRepr constrRepr :: [BitOrigin]
    let mkId nm = Id.makeBasic nm
    ids <- mapM (\n -> mkId (TextS.pack ('v':show n))) [1..length args]
    fId <- mkId (crName constrRepr)
    let fInps =
          [ case typeSize t of
              0 -> emptyDoc
              1 -> "input" <+> pretty i <> semi <> line
              n -> "input" <+> brackets (int (n-1) <> colon <> int 0) <+>
                               pretty i <> semi <> line
          | (i,t) <- zip ids aTys
          ]

    let range' (Lit (bitsToBits -> ns)) =
          int (length ns) <> squote <> "b" <> hcat (mapM (bit_char undefValue) ns)
        range' (Field n start end) =
          let v   = ids !! n
              aTy = aTys !! n
          in case typeSize aTy of
               0 -> error "internal error"
               1 -> if start == 0 && end == 0 then
                      pretty v
                    else
                      error "internal error"
               _ -> pretty v <> brackets (int start <> colon <> int end)

    let val = case origins of
                []  -> error "internal error"
                [r] -> range' r
                rs  -> listBraces (mapM range' rs)

    let oSz = case size of
                0 -> error "internal error"
                1 -> emptyDoc
                n -> brackets (int (n-1) <> colon <> int 0)

    funDoc <-
      "function" <+> oSz <+> pretty fId <> semi <> line <>
      hcat (sequence fInps) <>
      "begin" <> line <>
      indent 2 (pretty fId <+> "=" <+> val <> semi) <> line <>
      "end" <> line <>
      "endfunction"
    Ap (includes %= ((TextS.unpack (Id.toText fId) ++ ".inc",funDoc):))
    return fId

-- | Turn a Netlist expression into a Verilog expression
expr_ :: Bool -- ^ Enclose in parentheses?
      -> Expr -- ^ Expr to convert
      -> VerilogM Doc
expr_ _ (Literal sizeM lit) = exprLitV sizeM lit

expr_ _ (Identifier id_ Nothing) = pretty id_

expr_ _ (Identifier id_ (Just (Indexed (CustomSP _id dataRepr _size args,dcI,fI)))) =
  case fieldTy of
    Void {} -> error (unexpectedProjectionErrorMsg dataRepr dcI fI)
    _       -> braces $ hcat $ punctuate ", " $ sequence ranges
 where
  (ConstrRepr' _name _n _mask _value anns, _, fieldTypes) = args !! dcI
  ranges = map range' $ bitRanges (anns !! fI)
  range' (start, end) = pretty id_ <> brackets (int start <> ":" <> int end)
  fieldTy = indexNote ($(curLoc) ++ "panic") fieldTypes fI

expr_ _ (Identifier d_ (Just (Indexed (CustomProduct _id dataRepr _size _maybeFieldNames tys, dcI, fI))))
  | DataRepr' _typ _size [cRepr] <- dataRepr
  , ConstrRepr' _cName _pos _mask _val anns <- cRepr =
  let ranges = map range' (bitRanges (anns !! fI)) in
  case fieldTy of
    Void {} -> error (unexpectedProjectionErrorMsg dataRepr dcI fI)
    _       -> braces $ hcat $ punctuate ", " $ sequence ranges
 where
  (_fieldAnn, fieldTy) = indexNote ($(curLoc) ++ "panic") tys fI
  range' (start, end) = pretty d_ <> brackets (int start <> ":" <> int end)

-- See [Note] integer projection
expr_ _ (Identifier id_ (Just (Indexed ((Signed w),_,_))))  = do
  iw <- Ap $ use intWidth
  traceIf (iw < w) ($(curLoc) ++ "WARNING: result smaller than argument") $
    pretty id_

-- See [Note] integer projection
expr_ _ (Identifier id_ (Just (Indexed ((Unsigned w),_,_))))  = do
  iw <- Ap $ use intWidth
  traceIf (iw < w) ($(curLoc) ++ "WARNING: result smaller than argument") $
    pretty id_

-- See [Note] mask projection
expr_ _ (Identifier _ (Just (Indexed ((BitVector _),_,0)))) = do
  iw <- Ap $ use intWidth
  traceIf True ($(curLoc) ++ "WARNING: synthesizing bitvector mask to dontcare") $
    verilogTypeErrValue (Signed iw)

-- See [Note] bitvector projection
expr_ _ (Identifier id_ (Just (Indexed ((BitVector w),_,1)))) = do
  iw <- Ap $ use intWidth
  traceIf (iw < w) ($(curLoc) ++ "WARNING: result smaller than argument") $
    pretty id_

expr_ _ (Identifier id_ (Just m)) = case modifier (Contiguous 0 0) m of
  Nothing -> pretty id_
  Just (Contiguous start end,resTy) -> case resTy of
    Signed _ -> "$signed" <> parens (slice start end)
    _        -> slice start end
  Just (Split rs,resTy) ->
    let rs1 = listBraces (mapM (\(start,end,_) -> slice start end) rs) in
    case resTy of
      Signed _ -> "$signed" <> parens rs1
      _ -> rs1

 where
  slice s e = pretty id_ <> brackets (int s <> colon <> int e)

expr_ b (DataCon _ (DC (Void {}, -1)) [e]) = expr_ b e

expr_ _ (DataCon ty@(Vector 0 _) _ _) = verilogTypeErrValue ty

expr_ _ (DataCon (Vector 1 _) _ [e]) = expr_ False e
expr_ _ e@(DataCon (Vector _ _) _ es@[_,_]) =
    listBraces $ mapM (expr_ False) $ fromMaybe es $ vectorChain e

expr_ _ (DataCon (MemBlob n m) _ [n0, m0, _, runs, _, ends])
  | Literal _ (NumLit n1) <- n0
  , n == fromInteger n1
  , Literal _ (NumLit m1) <- m0
  , m == fromInteger m1
  , Literal Nothing (StringLit runs0) <- runs
  , Literal Nothing (StringLit ends0) <- ends
  , es <- unpackNats n m (B8.pack runs0) (B8.pack ends0) =
    let el val = exprLitV (Just (BitVector m, m)) (BitVecLit 0 $ toInteger val)
    in listBraces $ mapM el es

expr_ _ (DataCon (RTree 0 _) _ [e]) = expr_ False e
expr_ _ e@(DataCon (RTree _ _) _ es@[_,_]) =
    listBraces $ mapM (expr_ False) $ fromMaybe es $ rtreeChain e

expr_ _ (DataCon (SP {}) (DC (BitVector _,_)) es) = assignExpr
  where
    argExprs   = map (expr_ False) es
    assignExpr = braces (hcat $ punctuate comma $ sequence argExprs)

expr_ _ (DataCon ty@(SP _ args) (DC (_,i)) es) = assignExpr
  where
    argTys     = snd $ args !! i
    dcSize     = conSize ty + sum (map typeSize argTys)
    dcExpr     = expr_ False (dcToExpr ty i)
    argExprs   = map (expr_ False) es
    extraArg   = case typeSize ty - dcSize of
                   0 -> []
                   n -> [int n <> "'b" <> bits undefValue (replicate n U)]
    assignExpr = braces (hcat $ punctuate comma $ sequence (dcExpr:argExprs ++ extraArg))

expr_ _ (DataCon ty@(Sum _ _) (DC (_,i)) []) = int (typeSize ty) <> "'d" <> int i

expr_ _ (DataCon ty@(CustomSum _ _ _ tys) (DC (_,i)) []) =
  let (ConstrRepr' _ _ _ value _) = fst $ tys !! i in
  int (typeSize ty) <> squote <> "d" <> int (fromIntegral value)
expr_ _ (DataCon (CustomSP _name dataRepr _size constrs) (DC (_,constrNr)) es) =
  let (cRepr, _, argTys) = constrs !! constrNr in
  customReprDataCon dataRepr cRepr (zip argTys es)
expr_ _ (DataCon (CustomProduct _ dataRepr _size _labels tys) _ es) |
  DataRepr' _typ _size [cRepr] <- dataRepr =
  customReprDataCon dataRepr cRepr (zip (map snd tys) es)
expr_ _ (DataCon (Product {}) _ es) = listBraces (mapM (expr_ False) es)

expr_ _ (DataCon (Enable _) _ [e]) =
  expr_ False e

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "Clash.Sized.Internal.Signed.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLitV (Just (Signed (fromInteger n),fromInteger n)) i

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "Clash.Sized.Internal.Unsigned.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLitV (Just (Unsigned (fromInteger n),fromInteger n)) i

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "Clash.Sized.Internal.BitVector.fromInteger#"
  , [Literal _ (NumLit n), Literal _ m, Literal _ i] <- extractLiterals bbCtx
  = let NumLit m' = m
        NumLit i' = i
    in exprLitV (Just (BitVector (fromInteger n),fromInteger n)) (BitVecLit m' i')

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "Clash.Sized.Internal.BitVector.fromInteger##"
  , [Literal _ m, Literal _ i] <- extractLiterals bbCtx
  = let NumLit m' = m
        NumLit i' = i
    in exprLitV (Just (Bit,1)) (BitLit $ toBit m' i')

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "Clash.Sized.Internal.Index.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLit undefValue (Just (Index (fromInteger n),fromInteger n)) i

expr_ b (BlackBoxE _ libs imps inc bs bbCtx b') = do
  parenIf (b || b') (Ap (renderBlackBox libs imps inc bs bbCtx <*> pure 0))

expr_ _ (DataTag Bool (Left id_)) = pretty id_ <> brackets (int 0)
expr_ _ (DataTag Bool (Right id_)) = do
  iw <- Ap (use intWidth)
  "$unsigned" <> parens (listBraces (sequence [braces (int (iw-1) <+> braces "1'b0"),pretty id_]))

expr_ _ (DataTag (Sum _ _) (Left id_))     = "$unsigned" <> parens (pretty id_)
expr_ _ (DataTag (Sum _ _) (Right id_))    = "$unsigned" <> parens (pretty id_)

expr_ _ (DataTag (Product {}) (Right _))  = do
  iw <- Ap (use intWidth)
  int iw <> "'sd0"

expr_ _ (DataTag hty@(SP _ _) (Right id_)) = "$unsigned" <> parens
                                               (pretty id_ <> brackets
                                               (int start <> colon <> int end))
  where
    start = typeSize hty - 1
    end   = typeSize hty - conSize hty

expr_ _ (DataTag (Vector 0 _) (Right _)) = do
  iw <- Ap $ use intWidth
  int iw <> "'sd0"
expr_ _ (DataTag (Vector _ _) (Right _)) = do
  iw <- Ap $ use intWidth
  int iw <> "'sd1"

expr_ _ (DataTag (RTree 0 _) (Right _)) = do
  iw <- Ap $ use intWidth
  int iw <> "'sd0"
expr_ _ (DataTag (RTree _ _) (Right _)) = do
  iw <- Ap $ use intWidth
  int iw <> "'sd1"

expr_ b (ToBv _ _ e) = expr_ b e
expr_ b (FromBv _ _ e) = expr_ b e

expr_ b (IfThenElse c t e) =
  parenIf b (expr_ True c <+> "?" <+> expr_ True t <+> ":" <+> expr_ True e)

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

exprLitV :: Maybe (HWType,Size) -> Literal -> VerilogM Doc
exprLitV = exprLit undefValue

exprLit :: Lens' s (Maybe (Maybe Int)) -> Maybe (HWType,Size) -> Literal -> Ap (State s) Doc
exprLit _ Nothing (NumLit i) = integer i

exprLit k (Just (hty,sz)) (NumLit i) = case hty of
  Unsigned _
   | i < 0     -> string "-" <> int sz <> string "'d" <> integer (abs i)
   | otherwise -> int sz <> string "'d" <> integer i
  Index _ -> int (typeSize hty) <> string "'d" <> integer i
  Signed _
   | i < 0     -> string "-" <> int sz <> string "'sd" <> integer (abs i)
   | otherwise -> int sz <> string "'sd" <> integer i
  _ -> int sz <> string "'b" <> blit
  where
    blit = bits k (toBits sz i)
exprLit k (Just (_,sz)) (BitVecLit m i) = int sz <> string "'b" <> bvlit
  where
    bvlit = bits k (toBits' sz m i)

exprLit _ _             (BoolLit t)   = string $ if t then "1'b1" else "1'b0"
exprLit k _             (BitLit b)    = string "1'b" <> bit_char k b
exprLit _ _             (StringLit s) = string . pack $ show s
exprLit _ _             l             = error $ $(curLoc) ++ "exprLit: " ++ show l

toBits :: Integral a => Int -> a -> [Bit]
toBits size val = map (\x -> if odd x then H else L)
                $ reverse
                $ take size
                $ map (`mod` 2)
                $ iterate (`div` 2) val

toBits' :: Integral a => Int -> a -> a -> [Bit]
toBits' size msk val = map (\(m,i) -> if odd m then U else (if odd i then H else L))
                $
                ( reverse . take size)
                $ zip
                  ( map (`mod` 2) $ iterate (`div` 2) msk)
                  ( map (`mod` 2) $ iterate (`div` 2) val)


bits :: Lens' s (Maybe (Maybe Int)) -> [Bit] -> Ap (State s) Doc
bits k = hcat . traverse (bit_char k)

bit_char' :: Bit -> Char
bit_char' H = '1'
bit_char' L = '0'
bit_char' U = 'x'
bit_char' Z = 'z'

bit_char :: Lens' s (Maybe (Maybe Int)) -> Bit -> Ap (State s) Doc
bit_char k b = do
  udf <- Ap (use k)
  case (udf,b) of
    (Just Nothing,U)  -> char '0'
    (Just (Just i),U) -> "'" <> int i <> "'"
    _                 -> char (bit_char' b)


dcToExpr :: HWType -> Int -> Expr
dcToExpr ty i = Literal (Just (ty,conSize ty)) (NumLit (toInteger i))

listBraces :: Monad m => m [Doc] -> m Doc
listBraces = align . enclose lbrace rbrace . hsep . punctuate (comma <+> softline)

parenIf :: Monad m => Bool -> m Doc -> m Doc
parenIf True  = parens
parenIf False = id

punctuate' :: Monad m => Ap m Doc -> Ap m [Doc] -> Ap m Doc
punctuate' s d = vcat (punctuate s d) <> s

encodingNote :: Applicative m => HWType -> m Doc
encodingNote (Clock _) = string " // clock"
encodingNote (Reset _) = string " // reset"
encodingNote (Enable _) = string " // enable"
encodingNote (Annotated _ t) = encodingNote t
encodingNote _         = emptyDoc
