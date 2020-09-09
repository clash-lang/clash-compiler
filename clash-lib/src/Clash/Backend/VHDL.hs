{-|
  Copyright   :  (C) 2015-2016, University of Twente,
                     2017-2018, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Generate VHDL for assorted Netlist datatypes
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Backend.VHDL (VHDLState) where

import           Control.Arrow                        (second)
import           Control.Applicative                  (liftA2)
import           Control.Lens                         hiding (Indexed, Empty)
import           Control.Monad                        (forM,join,zipWithM)
import           Control.Monad.State                  (State, StateT)
import           Data.Bits                            (testBit, Bits)
import           Data.Hashable                        (Hashable)
import           Data.HashMap.Lazy                    (HashMap)
import qualified Data.HashMap.Lazy                    as HashMap
import qualified Data.HashMap.Strict                  as HashMapS
import           Data.HashSet                         (HashSet)
import qualified Data.HashSet                         as HashSet
import           Data.List
  (mapAccumL, nub, nubBy, intersperse, group, sort)
import           Data.List.Extra                      ((<:>), equalLength)
import           Data.Maybe                           (catMaybes,fromMaybe,mapMaybe)
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid                          hiding (Sum, Product)
#endif
import           Data.Semigroup.Monad.Extra
import qualified Data.Text.Lazy                       as T
import qualified Data.Text                            as TextS
import qualified Data.Text.Prettyprint.Doc            as PP
import           Data.Text.Prettyprint.Doc.Extra
import           GHC.Stack                            (HasCallStack)
import qualified System.FilePath
import           Text.Printf
import           TextShow                             (showt)

import           Clash.Annotations.Primitive          (HDL (..))
import           Clash.Annotations.BitRepresentation.Internal
  (ConstrRepr'(..), DataRepr'(..))
import           Clash.Annotations.BitRepresentation.ClashLib
  (bitsToBits)
import           Clash.Annotations.BitRepresentation.Util
  (BitOrigin(Lit, Field), bitOrigins, bitRanges)
import           Clash.Backend
import           Clash.Core.Var                       (Attr'(..),attrName)
import           Clash.Debug                          (traceIf)
import           Clash.Netlist.BlackBox.Types         (HdlSyn (..))
import           Clash.Netlist.BlackBox.Util
  (extractLiterals, renderBlackBox, renderFilePath)
import           Clash.Netlist.Id                     (IdType (..), mkBasicId')
import           Clash.Netlist.Types                  hiding (_intWidth, intWidth)
import           Clash.Netlist.Util                   hiding (mkIdentifier)
import           Clash.Util
  (SrcSpan, noSrcSpan, clogBase, curLoc, first, makeCached, on, indexNote)
import           Clash.Util.Graph                     (reverseTopSort)

import           Clash.Backend.Verilog (Range (..), continueWithRange)

-- | State for the 'Clash.Netlist.VHDL.VHDLM' monad:
data VHDLState =
  VHDLState
  { _tyCache   :: (HashSet HWType)
  -- ^ Previously encountered HWTypes
  , _tySeen    :: HashMap Identifier Word
  -- ^ Generated product types
  , _nameCache :: (HashMap (HWType, Bool) TextS.Text)
  -- ^ Cache for type names. Bool indicates whether this name includes length
  -- information in its first "part". See `tyName'` for more information.
  , _modNm     :: Identifier
  , _srcSpan   :: SrcSpan
  , _libraries :: [T.Text]
  , _packages  :: [T.Text]
  , _includes  :: [(String,Doc)]
  , _dataFiles      :: [(String,FilePath)]
  -- ^ Files to be copied: (filename, old path)
  , _memoryDataFiles:: [(String,String)]
  -- ^ Files to be stored: (filename, contents). These files are generated
  -- during the execution of 'genNetlist'.
  , _idSeen    :: HashMapS.HashMap Identifier Word
  , _intWidth  :: Int
  -- ^ Int/Word/Integer bit-width
  , _hdlsyn    :: HdlSyn
  -- ^ For which HDL synthesis tool are we generating VHDL
  , _extendedIds :: Bool
  , _undefValue :: Maybe (Maybe Int)
  , _aggressiveXOptBB_ :: AggressiveXOptBB
  }

makeLenses ''VHDLState

instance Backend VHDLState where
  initBackend     = VHDLState HashSet.empty HashMap.empty HashMap.empty ""
                              noSrcSpan [] [] [] [] [] HashMapS.empty
  hdlKind         = const VHDL
  primDirs        = const $ do root <- primsRoot
                               return [ root System.FilePath.</> "common"
                                      , root System.FilePath.</> "vhdl"
                                      ]
  extractTypes    = _tyCache
  name            = const "vhdl"
  extension       = const ".vhdl"

  genHDL          = genVHDL
  mkTyPackage     = mkTyPackage_
  hdlType Internal      (filterTransparent -> ty) = sizedQualTyName ty
  hdlType (External nm) (filterTransparent -> ty) =
    let sized = sizedQualTyName ty in
    case ty of
      Bit         -> sized
      Bool        -> sized
      Signed _    -> sized
      Unsigned _  -> sized
      BitVector _ -> sized
      _           -> pretty nm <> dot <> sized
  hdlTypeErrValue = sizedQualTyNameErrValue
  hdlTypeMark     = qualTyName
  hdlRecSel       = vhdlRecSel
  hdlSig t ty     = sigDecl (pretty t) ty
  genStmt         = const emptyDoc
  inst            = inst_
  expr            = expr_
  iwWidth         = use intWidth
  toBV t id_
    | isBV t = pretty id_
    | otherwise = do
      nm <- Mon $ use modNm
      seen <- use seenIdentifiers
      -- This is a bit hacky, as id_ is just a rendered expression.
      -- But if it's a bare identifier that we've seen before,
      -- then this identifier has a defined type and we can skip the explicit type qualification.
      let e | T.toStrict id_ `HashMapS.member` seen = pretty id_
            | otherwise = hdlTypeMark t <> squote <> parens (pretty id_)
      pretty (TextS.toLower nm) <> "_types.toSLV" <> parens e
  fromBV t id_
    | isBV t = pretty id_
    | otherwise = do
      nm <- Mon $ use modNm
      qualTyName t <> "'" <> parens (pretty (TextS.toLower nm) <> "_types.fromSLV" <> parens (pretty id_))
  hdlSyn          = use hdlsyn
  mkIdentifier    = do
      allowExtended <- use extendedIds
      return (go allowExtended)
    where
      go _ Basic nm =
        case (stripTrailingUnderscore . filterReserved) (TextS.toLower (mkBasicId' VHDL True nm)) of
          nm' | TextS.null nm' -> "clash_internal"
              | otherwise -> nm'
      go esc Extended (rmSlash -> nm) = case go esc Basic nm of
        nm' | esc && nm /= nm' -> TextS.concat ["\\",nm,"\\"]
            | otherwise -> nm'
  extendIdentifier = do
      allowExtended <- use extendedIds
      return (go allowExtended)
    where
      go _ Basic nm ext =
        case (stripTrailingUnderscore . filterReserved) (TextS.toLower (mkBasicId' VHDL True (nm `TextS.append` ext))) of
          nm' | TextS.null nm' -> "clash_internal"
              | otherwise -> nm'
      go esc Extended ((rmSlash . escapeTemplate) -> nm) ext =
        let nmExt = nm `TextS.append` ext
        in  case go esc Basic nm ext of
              nm' | esc && nm' /= nmExt -> case TextS.isPrefixOf "c$" nmExt of
                      True -> TextS.concat ["\\",nmExt,"\\"]
                      _    -> TextS.concat ["\\c$",nmExt,"\\"]
                  | otherwise -> nm'

  setModName nm s = s {_modNm = nm}
  setSrcSpan      = (srcSpan .=)
  getSrcSpan      = use srcSpan
  blockDecl nm ds = do
    decs <- decls ds
    let attrs = [ (id_, attr)
                | NetDecl' _ _ id_ (Right hwtype) _ <- ds
                , attr <- hwTypeAttrs hwtype]
    if isEmpty decs
       then insts ds
       else nest 2
              (pretty nm <+> colon <+> "block" <> line <>
               pure decs <>
               if null attrs
                then emptyDoc
                else line <> line <> renderAttrs attrs) <> line <>
            nest 2
              ("begin" <> line <>
                insts ds) <> line <>
            "end block" <> semi
  unextend = return rmSlash
  addIncludes inc = includes %= (inc++)
  addLibraries libs = libraries %= (libs ++)
  addImports imps = packages %= (imps ++)
  addAndSetData f = do
    fs <- use dataFiles
    let (fs',f') = renderFilePath fs f
    dataFiles .= fs'
    return f'
  getDataFiles = use dataFiles
  addMemoryDataFile f = memoryDataFiles %= (f:)
  getMemoryDataFiles = use memoryDataFiles
  seenIdentifiers = idSeen
  ifThenElseExpr _ = False
  aggressiveXOptBB = use aggressiveXOptBB_

rmSlash :: Identifier -> Identifier
rmSlash nm = fromMaybe nm $ do
  nm1 <- TextS.stripPrefix "\\" nm
  pure (TextS.filter (not . (== '\\')) nm1)

type VHDLM a = Mon (State VHDLState) a

-- Check if the underlying type is a BitVector
isBV :: HWType -> Bool
isBV (normaliseType -> BitVector _) = True
isBV _ = False

-- | Time units: are added to 'reservedWords' as simulators trip over signals
-- named after them.
timeUnits :: [Identifier]
timeUnits = ["fs", "ps", "ns", "us", "ms", "sec", "min", "hr"]

-- | Identifiers which are imported from the following:
--
-- use IEEE.STD_LOGIC_1164.ALL;
-- use IEEE.NUMERIC_STD.ALL;
-- use IEEE.MATH_REAL.ALL;
-- use std.textio.all;
--
-- Clash should not use these identifiers, as it can lead to errors when
-- interfacing with an EDA tool.
--
-- See https://github.com/clash-lang/clash-compiler/issues/1439.
--
importedNames :: [Identifier]
importedNames =
  [ -- ieee.std_logic_1164.all
    "std_ulogic", "std_ulogic_vector", "resolved", "std_logic", "std_logic_vector"
  , "x01", "x01z", "ux01", "ux01z", "to_bit", "to_bitvector", "to_stdulogic"
  , "to_stdlogicvector", "to_stdulogicvector", "to_01", "to_x01", "to_x01z"
  , "to_ux01", "rising_edge", "falling_edge", "is_x"
    -- ieee.numeric_std.all
  , "unresolved_unsigned", "unresolved_signed", "u_unsigned", "u_signed"
  , "unsigned", "signed", "find_leftmost", "find_rightmost", "minimum"
  , "maximum", "shift_left", "shift_right", "rotate_left", "rotate_right"
  , "resize", "to_integer", "to_unsigned", "to_signed", "std_match"
    -- ieee.math_real.all
  , "math_e", "math_1_over_e", "math_pi", "math_2_pi", "math_1_over_pi"
  , "math_pi_over_2", "math_pi_over_3", "path_pi_over_4", "path_3_pi_over_2"
  , "math_log_of_2", "math_log_of_10", "math_log10_of_e", "math_sqrt_2"
  , "math_1_over_sqrt_2", "math_sqrt_pi", "math_deg_to_rad", "math_rad_to_deg"
  , "sign", "ceil", "floor", "round", "trunc", "realmax", "realmin", "uniform"
  , "sqrt", "cbrt", "exp", "log", "log2", "log10", "sin", "cos", "tan", "arcsin"
  , "arccos", "arctan", "sinh", "cosh", "tanh", "arcsinh", "arccosh", "arctanh"
    -- std.textio.all
  , "line", "text", "side", "width", "justify", "input", "output", "readline"
  , "read", "sread", "string_read", "bread", "binary_read", "oread", "octal_read"
  , "hread", "hex_read", "writeline", "tee", "write", "swrite", "string_write"
  , "bwrite", "binary_write", "owrite", "octal_write", "hwrite", "hex_write"
  ]

-- List of reserved VHDL-2008 keywords
-- + used internal names: toslv, fromslv, tagtoenum, datatotag
-- + used IEEE library names: integer, boolean, std_logic, std_logic_vector,
--   signed, unsigned, to_integer, to_signed, to_unsigned, string
reservedWords :: [Identifier]
reservedWords = ["abs","access","after","alias","all","and","architecture"
  ,"array","assert","assume","assume_guarantee","attribute","begin","block"
  ,"body","buffer","bus","case","component","configuration","constant","context"
  ,"cover","default","disconnect","downto","else","elsif","end","entity","exit"
  ,"fairness","file","for","force","function","generate","generic","group"
  ,"guarded","if","impure","in","inertial","inout","is","label","library"
  ,"linkage","literal","loop","map","mod","nand","new","next","nor","not","null"
  ,"of","on","open","or","others","out","package","parameter","port","postponed"
  ,"procedure","process","property","protected","pure","range","record"
  ,"register","reject","release","rem","report","restrict","restrict_guarantee"
  ,"return","rol","ror","select","sequence","severity","signal","shared","sla"
  ,"sll","sra","srl","strong","subtype","then","to","transport","type"
  ,"unaffected","units","until","use","variable","vmode","vprop","vunit","wait"
  ,"when","while","with","xnor","xor","toslv","fromslv","tagtoenum","datatotag"
  ,"integer", "boolean", "std_logic", "std_logic_vector", "signed", "unsigned"
  ,"to_integer", "to_signed", "to_unsigned", "string","log"] ++ timeUnits ++ importedNames

filterReserved :: Identifier -> Identifier
filterReserved s = if s `elem` reservedWords
  then s `TextS.append` "_r"
  else s

stripTrailingUnderscore :: Identifier -> Identifier
stripTrailingUnderscore = TextS.dropWhileEnd (== '_')

-- | Generate unique (partial) names for product fields. Example:
--
-- >>> productFieldNames [Unsigned 6, Unsigned 6, Bit, Bool]
-- ["unsigned6_0", "unsigned6_1", "bit", "boolean"]
productFieldNames
  :: HasCallStack
  => Maybe [TextS.Text]
  -- ^ Label hints. From user records, for example.
  -> [HWType]
  -- ^ Field types
  -> VHDLM [TextS.Text]
productFieldNames labels0 fields = do
  let labels1 = sequence labels0 ++ repeat Nothing
  hFields <- zipWithM hName labels1 fields

  let grouped = group $ sort $ hFields
      counted = HashMapS.fromList (map (\(g:gs) -> (g, succ (length gs))) grouped)
      names   = snd $ mapAccumL (name' counted) HashMapS.empty hFields

  return names
 where
  hName
    :: Maybe Identifier
    -> HWType
    -> VHDLM Identifier
  hName Nothing field  =
    tyName' False field
  hName (Just label) _field = do
    Mon (mkIdentifier <*> pure Basic <*> pure label)

  name'
    :: HashMap TextS.Text Int
    -> HashMap TextS.Text Int
    -> TextS.Text
    -> (HashMap TextS.Text Int, TextS.Text)
  name' counted countMap fieldName
    | counted HashMapS.! fieldName > 1 =
        -- Seen this fieldname more than once, so we need to add a number
        -- as a postfix:
        let succ' n = Just (maybe (0 :: Int) (+1) n) in
        let countMap' = HashMapS.alter succ' fieldName countMap in
        -- Each field will get a distinct number:
        let count = countMap' HashMapS.! fieldName in
        (countMap', TextS.concat [fieldName, "_", showt count])
    | otherwise =
        -- This fieldname has only been seen once, so we don't need to add
        -- a number as a postfix:
        (countMap, fieldName)

productFieldName
  :: HasCallStack
  => Maybe [TextS.Text]
  -- ^ Label hints. From user records, for example.
  -> [HWType]
  -- ^ Field types
  -> Int
  -- ^ Index of field
  -> VHDLM Doc
productFieldName labels fields fieldIndex = do
  -- TODO: cache
  names <- productFieldNames labels fields
  return (PP.pretty (names !! fieldIndex))

selectProductField
  :: HasCallStack
  => Maybe [TextS.Text]
  -- ^ Label hints. From user records, for example.
  -> [HWType]
  -- ^ Field types
  -> Int
  -- ^ Index of field
  -> VHDLM Doc
selectProductField fieldLabels fieldTypes fieldIndex =
  "_sel" <> int fieldIndex <> "_" <> productFieldName fieldLabels fieldTypes fieldIndex

-- | Generate VHDL for a Netlist component
genVHDL :: Identifier -> SrcSpan -> HashMapS.HashMap Identifier Word -> Component -> VHDLM ((String,Doc),[(String,Doc)])
genVHDL nm sp seen c = preserveSeen $ do
    Mon $ idSeen .= seen
    -- Don't have type names conflict with component names
    Mon $ tySeen %= HashMap.unionWith max seen
    Mon $ setSrcSpan sp
    v <- vhdl
    i <- Mon $ use includes
    Mon $ libraries .= []
    Mon $ packages  .= []
    return ((TextS.unpack cName,v),i)
  where
    cName   = componentName c
    vhdl    = do
      ent  <- entity c
      arch <- architecture c
      imps <- tyImports nm
      ("-- Automatically generated VHDL-93" <> line <>
       pure imps <> line <> line <>
       pure ent <> line <> line <>
       pure arch)

-- | Generate a VHDL package containing type definitions for the given HWTypes
mkTyPackage_ :: Identifier
             -> [HWType]
             -> VHDLM [(String,Doc)]
mkTyPackage_ modName (map filterTransparent -> hwtys) = do
    { syn <- Mon hdlSyn
    ; mkId <- Mon (mkIdentifier <*> pure Basic)
    ; let usedTys     = concatMap mkUsedTys hwtys
    ; let normTys0    = nub (map mkVecZ (hwtys ++ usedTys))
    ; let sortedTys0  = topSortHWTys normTys0
          packageDec  = vcat $ mapM tyDec (nubBy eqTypM sortedTys0)
          (funDecs,funBodies) = unzip . mapMaybe (funDec syn) $ nubBy eqTypM (map normaliseType sortedTys0)

    ; (:[]) <$> (TextS.unpack $ mkId (modName `TextS.append` "_types"),) <$>
      "library IEEE;" <> line <>
      "use IEEE.STD_LOGIC_1164.ALL;" <> line <>
      "use IEEE.NUMERIC_STD.ALL;" <> line <> line <>
      "package" <+> pretty (mkId (modName `TextS.append` "_types")) <+> "is" <> line <>
         indent 2 ( packageDec <> line <>
                    vcat (sequence funDecs)
                  ) <> line <>
      "end" <> semi <> packageBodyDec funBodies
    }
  where
    packageBodyDec :: [VHDLM Doc] -> VHDLM Doc
    packageBodyDec funBodies = case funBodies of
      [] -> emptyDoc
      _  -> do
        { mkId <- Mon (mkIdentifier <*> pure Basic)
        ; line <> line <>
         "package" <+> "body" <+> pretty (mkId (modName `TextS.append` "_types")) <+> "is" <> line <>
           indent 2 (vcat (sequence funBodies)) <> line <>
         "end" <> semi
        }

    eqTypM :: HWType -> HWType -> Bool
    eqTypM (Signed _) (Signed _)         = True
    eqTypM (Unsigned _) (Unsigned _)     = True
    eqTypM (BitVector _) (BitVector _)   = True
    eqTypM ty1 ty2                       = ty1 == ty2

mkUsedTys :: HWType -> [HWType]
mkUsedTys hwty = hwty : case hwty of
  Vector _ elTy        -> mkUsedTys elTy
  RTree _ elTy         -> mkUsedTys elTy
  Product _ _ elTys    -> concatMap mkUsedTys elTys
  SP _ elTys           -> concatMap mkUsedTys (concatMap snd elTys)
  BiDirectional _ elTy -> mkUsedTys elTy
  Annotated _ elTy     -> mkUsedTys elTy
  CustomProduct _ _ _ _ tys0 ->
    concatMap mkUsedTys (map snd tys0)
  CustomSP _ _ _ tys0 ->
    let tys1 = concat [tys | (_repr, _id, tys) <- tys0] in
    concatMap mkUsedTys tys1
  _ ->
    []

topSortHWTys
  :: [HWType]
  -> [HWType]
topSortHWTys hwtys = sorted
  where
    nodes  = zip [0..] hwtys
    nodesI = HashMap.fromList (zip hwtys [0..])
    edges  = concatMap edge hwtys

    sorted =
      case reverseTopSort nodes edges of
        Left err -> error $ $(curLoc) ++ "[BUG IN CLASH] topSortHWTys: " ++ err
        Right ns -> ns

    -- `elTy` needs to be rendered before `t`
    edge t@(Vector _ elTy) =
      case HashMap.lookup (mkVecZ elTy) nodesI of
        Just node ->
          [(nodesI HashMap.! t, node)]
        Nothing ->
          []

    -- `elTy` needs to be rendered before `t`
    edge t@(RTree _ elTy) =
      let vecZ = mkVecZ elTy in
      case HashMap.lookup vecZ nodesI of
        Just node ->
          [(nodesI HashMap.! t, node)] ++ edge elTy
        Nothing ->
          []

    -- `tys` need to be rendered before `t`
    edge t@(Product _ _ tys0) =
      let tys1 = [HashMap.lookup (mkVecZ ty) nodesI | ty <- tys0] in
      map (nodesI HashMap.! t,) (catMaybes tys1)

    edge t@(SP _ tys0) =
      let tys1 = concat (map snd tys0) in
      let tys2 = [HashMap.lookup (mkVecZ ty) nodesI | ty <- tys1] in
      map (nodesI HashMap.! t,) (catMaybes tys2)

    edge t@(CustomSP _ _ _ tys0) =
      let tys1 = concat [tys | (_repr, _id, tys) <- tys0] in
      let tys2 = [HashMap.lookup (mkVecZ ty) nodesI | ty <- tys1] in
      map (nodesI HashMap.! t,) (catMaybes tys2)

    edge t@(CustomProduct _ _ _ _ (map snd -> tys0)) =
      let tys1 = [HashMap.lookup (mkVecZ ty) nodesI | ty <- tys0] in
      map (nodesI HashMap.! t,) (catMaybes tys1)

    edge _ = []

mkVecZ :: HWType -> HWType
mkVecZ (Vector _ elTy) = Vector 0 elTy
mkVecZ (RTree _ elTy)  = RTree 0 elTy
mkVecZ t               = t

typAliasDec :: HasCallStack => HWType -> VHDLM Doc
typAliasDec hwty =
  "subtype" <+> tyName hwty
            <+> "is"
            <+> sizedTyName (normaliseType hwty)
            <> semi

tyDec :: HasCallStack => HWType -> VHDLM Doc
tyDec hwty = do
  syn <- Mon hdlSyn
  case hwty of
    -- "Proper" custom types:
    Vector _ elTy ->
      case syn of
        Vivado ->
          "type" <+> tyName hwty
                 <+> "is array (integer range <>) of std_logic_vector"
                 <> parens (int (typeSize elTy - 1) <+> "downto 0")
                 <> semi

        _ ->
          "type" <+> tyName hwty
                 <+> "is array (integer range <>) of"
                 <+> sizedQualTyName elTy
                 <> semi

    RTree _ elTy ->
      case syn of
        Vivado ->
          "type" <+> tyName hwty
                 <+> "is array (integer range <>) of"
                 <+> "std_logic_vector"
                 <> parens (int (typeSize elTy - 1) <+> "downto 0")
                 <> semi

        _ ->
          "type" <+> tyName hwty
                 <+> "is array (integer range <>) of"
                 <+> sizedQualTyName elTy
                 <> semi

    Product _ labels tys@(_:_:_) ->
      let selNames = map (\i -> tyName hwty <> selectProductField labels tys i) [0..] in
      let selTys   = map sizedQualTyName tys in
      "type" <+> tyName hwty <+> "is record" <> line  <>
        indent 2 (vcat $ zipWithM (\x y -> x <+> colon <+> y <> semi) selNames selTys) <> line <>
      "end record" <> semi

    -- Type aliases:
    Clock _           -> typAliasDec hwty
    Reset _           -> typAliasDec hwty
    Index _           -> typAliasDec hwty
    CustomSP _ _ _ _  -> typAliasDec hwty
    SP _ _            -> typAliasDec hwty
    Sum _ _           -> typAliasDec hwty
    CustomSum _ _ _ _ -> typAliasDec hwty
    CustomProduct {}  -> typAliasDec hwty

    -- VHDL builtin types:
    BitVector _ -> emptyDoc
    Bool        -> emptyDoc
    Bit         -> emptyDoc
    Unsigned _  -> emptyDoc
    Signed _    -> emptyDoc
    String      -> emptyDoc
    Integer     -> emptyDoc

    -- Transparent types:
    BiDirectional _ ty -> tyDec ty
    Annotated _ ty -> tyDec ty

    Void {} -> emptyDoc
    KnownDomain {} -> emptyDoc

    _ -> error $ $(curLoc) ++ show hwty




funDec :: HdlSyn -> HWType -> Maybe (VHDLM Doc,VHDLM Doc)
funDec _ Bool = Just
  ( "function" <+> "toSLV" <+> parens ("b" <+> colon <+> "in" <+> "boolean") <+> "return" <+> "std_logic_vector" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("sl" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "boolean" <> semi <> line <>
    "function" <+> "tagToEnum" <+> parens ("s" <+> colon <+> "in" <+> "signed") <+> "return" <+> "boolean" <> semi <> line <>
    "function" <+> "dataToTag" <+> parens ("b" <+> colon <+> "in" <+> "boolean") <+> "return" <+> "signed" <> semi
  , "function" <+> "toSLV" <+> parens ("b" <+> colon <+> "in" <+> "boolean") <+> "return" <+> "std_logic_vector" <+> "is" <> line <>
    "begin" <> line <>
      indent 2 (vcat $ sequence ["if" <+> "b" <+> "then"
                                ,  indent 2 ("return" <+> dquotes (int 1) <> semi)
                                ,"else"
                                ,  indent 2 ("return" <+> dquotes (int 0) <> semi)
                                ,"end" <+> "if" <> semi
                                ]) <> line <>
    "end" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("sl" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "boolean" <+> "is" <> line <>
    "begin" <> line <>
      indent 2 (vcat $ sequence ["if" <+> "sl" <+> "=" <+> dquotes (int 1) <+> "then"
                                ,   indent 2 ("return" <+> "true" <> semi)
                                ,"else"
                                ,   indent 2 ("return" <+> "false" <> semi)
                                ,"end" <+> "if" <> semi
                                ]) <> line <>
    "end" <> semi <> line <>
    "function" <+> "tagToEnum" <+> parens ("s" <+> colon <+> "in" <+> "signed") <+> "return" <+> "boolean" <+> "is" <> line <>
    "begin" <> line <>
      indent 2 (vcat $ sequence ["if" <+> "s" <+> "=" <+> "to_signed" <> parens (int 0 <> comma <> (Mon (use intWidth) >>= int)) <+> "then"
                                ,   indent 2 ("return" <+> "false" <> semi)
                                ,"else"
                                ,   indent 2 ("return" <+> "true" <> semi)
                                ,"end" <+> "if" <> semi
                                ]) <> line <>
    "end" <> semi <> line <>
    "function" <+> "dataToTag" <+> parens ("b" <+> colon <+> "in" <+> "boolean") <+> "return" <+> "signed" <+> "is" <> line <>
    "begin" <> line <>
      indent 2 (vcat $ sequence ["if" <+> "b" <+> "then"
                                ,  indent 2 ("return" <+> "to_signed" <> parens (int 1 <> comma <> (Mon (use intWidth) >>= int)) <> semi)
                                ,"else"
                                ,  indent 2 ("return" <+> "to_signed" <> parens (int 0 <> comma <> (Mon (use intWidth) >>= int)) <> semi)
                                ,"end" <+> "if" <> semi
                                ]) <> line <>
    "end" <> semi
  )

funDec _ bit@Bit = Just
  ( "function" <+> "toSLV" <+> parens ("sl" <+> colon <+> "in" <+> tyName bit) <+> "return" <+> "std_logic_vector" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> tyName bit <> semi
  , "function" <+> "toSLV" <+> parens ("sl" <+> colon <+> "in" <+> tyName bit) <+> "return" <+> "std_logic_vector" <+> "is" <> line <>
    "begin" <> line <>
      indent 2 ("return" <+> "std_logic_vector'" <> parens (int 0 <+> rarrow <+> "sl") <> semi) <> line <>
    "end" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> tyName bit <+> "is" <> line <>
      indent 2
        ( "alias islv : std_logic_vector (0 to slv'length - 1) is slv;"
        ) <> line <>
    "begin" <> line <>
      indent 2 ("return" <+> "islv" <> parens (int 0) <> semi) <> line <>
    "end" <> semi
  )

funDec _ (Signed _) = Just
  ( "function" <+> "toSLV" <+> parens ("s" <+> colon <+> "in" <+> "signed") <+> "return" <+> "std_logic_vector" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "signed" <> semi
  , "function" <+> "toSLV" <+> parens ("s" <+> colon <+> "in" <+> "signed") <+> "return" <+> "std_logic_vector" <+> "is" <> line <>
    "begin" <> line <>
      indent 2 ("return" <+> "std_logic_vector" <> parens ("s") <> semi) <> line <>
    "end" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "signed" <+> "is" <> line <>
      indent 2 ("alias islv : std_logic_vector(0 to slv'length - 1) is slv;") <> line <>
    "begin" <> line <>
      indent 2 ("return" <+> "signed" <> parens ("islv") <> semi) <> line <>
    "end" <> semi
  )

funDec _ (Unsigned _) = Just
  ( "function" <+> "toSLV" <+> parens ("u" <+> colon <+> "in" <+> "unsigned") <+> "return" <+> "std_logic_vector" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "unsigned" <> semi
  , "function" <+> "toSLV" <+> parens ("u" <+> colon <+> "in" <+> "unsigned") <+> "return" <+> "std_logic_vector" <+> "is"  <> line <>
    "begin" <> line <>
      indent 2 ("return" <+> "std_logic_vector" <> parens ("u") <> semi) <> line <>
    "end" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "unsigned" <+> "is"  <> line <>
      indent 2 "alias islv : std_logic_vector(0 to slv'length - 1) is slv;" <> line <>
    "begin" <> line <>
      indent 2 ("return" <+> "unsigned" <> parens ("islv") <> semi) <> line <>
    "end" <> semi

  )

funDec _ t@(Product _ labels elTys) = Just
  ( "function" <+> "toSLV" <+> parens ("p :" <+> sizedQualTyName t) <+> "return std_logic_vector" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> sizedQualTyName t <> semi
  , "function" <+> "toSLV" <+> parens ("p :" <+> sizedQualTyName t) <+> "return std_logic_vector" <+> "is" <> line <>
    "begin" <> line <>
    indent 2 ("return" <+> parens (hcat (punctuate " & " elTyToSLV)) <> semi) <> line <>
    "end" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> sizedQualTyName t <+> "is" <> line <>
      "alias islv : std_logic_vector(0 to slv'length - 1) is slv;" <> line <>
    "begin" <> line <>
    indent 2 ("return" <+> parens (hcat (punctuate "," elTyFromSLV)) <> semi) <> line <>
    "end" <> semi
  )
  where
    elTyToSLV = forM [0..(length elTys - 1)]
                     (\i -> "toSLV" <>
                            parens ("p." <> tyName t <> selectProductField labels elTys i))

    argLengths = map typeSize elTys
    starts     = 0 : snd (mapAccumL ((join (,) .) . (+)) 0 argLengths)
    ends       = map (subtract 1) (tail starts)

    elTyFromSLV = forM (zip starts ends)
                       (\(s,e) -> "fromSLV" <>
                          parens ("islv" <> parens (int s <+> "to" <+> int e)))

funDec syn t@(Vector _ elTy) = Just
  ( "function" <+> "toSLV" <+> parens ("value : " <+> qualTyName t) <+> "return std_logic_vector" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> qualTyName t <> semi
  , "function" <+> "toSLV" <+> parens ("value : " <+> qualTyName t) <+> "return std_logic_vector" <+> "is" <> line <>
      indent 2
        ( "alias ivalue    :" <+> qualTyName t <> "(1 to value'length) is value;" <> line <>
          "variable result :" <+> "std_logic_vector" <> parens ("1 to value'length * " <> int (typeSize elTy)) <> semi
        ) <> line <>
    "begin" <> line <>
      indent 2
        ("for i in ivalue'range loop" <> line <>
            indent 2
              (  "result" <> parens (parens ("(i - 1) * " <> int (typeSize elTy)) <+> "+ 1" <+>
                                             "to i*" <> int (typeSize elTy)) <+>
                          ":=" <+> (case syn of
                                      Vivado -> "ivalue" <> parens ("i")
                                      _  -> "toSLV" <> parens ("ivalue" <> parens ("i"))) <> semi
              ) <> line <>
         "end" <+> "loop" <> semi <> line <>
         "return" <+> "result" <> semi
        ) <> line <>
    "end" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> qualTyName t <+> "is" <> line <>
      indent 2
        ( "alias islv      :" <+> "std_logic_vector" <> "(0 to slv'length - 1) is slv;" <> line <>
          "variable result :" <+> qualTyName t <> parens ("0 to slv'length / " <> eSz <+> "- 1") <> semi
        ) <> line <>
    "begin" <> line <>
      indent 2
        ("for i in result'range loop" <> line <>
            indent 2
              ( "result" <> parens "i" <+> ":=" <+> case syn of
                    Vivado -> getElem <> semi
                    _ | BitVector _ <- elTy -> getElem <> semi
                      | otherwise           -> "fromSLV" <> parens getElem <> semi

              ) <> line <>
         "end" <+> "loop" <> semi <> line <>
         "return" <+> "result" <> semi
        ) <> line <>
    "end" <> semi
  )
  where
    eSz     = int (typeSize elTy)
    getElem = "islv" <> parens ("i * " <> eSz <+> "to (i+1) * " <> eSz <+> "- 1")

funDec _ (BitVector _) = Just
  ( "function" <+> "toSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "std_logic_vector" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "std_logic_vector" <> semi
  , "function" <+> "toSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "std_logic_vector" <+> "is" <> line <>
    "begin" <> line <>
      indent 2 ("return" <+> "slv" <> semi) <> line <>
    "end" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "std_logic_vector" <+> "is" <> line <>
    "begin" <> line <>
      indent 2 ("return" <+> "slv" <> semi) <> line <>
    "end" <> semi
  )

funDec syn t@(RTree _ elTy) = Just
  ( "function" <+> "toSLV" <+> parens ("value : " <+> qualTyName t) <+> "return std_logic_vector" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> qualTyName t <> semi
  , "function" <+> "toSLV" <+> parens ("value : " <+> qualTyName t) <+> "return std_logic_vector" <+> "is" <> line <>
      indent 2
        ( "alias ivalue    :" <+> qualTyName t <> "(1 to value'length) is value;" <> line <>
          "variable result :" <+> "std_logic_vector" <> parens ("1 to value'length * " <> int (typeSize elTy)) <> semi
        ) <> line <>
    "begin" <> line <>
      indent 2
        ("for i in ivalue'range loop" <> line <>
            indent 2
              (  "result" <> parens (parens ("(i - 1) * " <> int (typeSize elTy)) <+> "+ 1" <+>
                                             "to i*" <> int (typeSize elTy)) <+>
                          ":=" <+> (case syn of
                                      Vivado -> "ivalue" <> parens ("i")
                                      _ -> "toSLV" <> parens ("ivalue" <> parens ("i"))) <> semi
              ) <> line <>
         "end" <+> "loop" <> semi <> line <>
         "return" <+> "result" <> semi
        ) <> line <>
    "end" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> qualTyName t <+> "is" <> line <>
      indent 2
        ( "alias islv      :" <+> "std_logic_vector" <> "(0 to slv'length - 1) is slv;" <> line <>
          "variable result :" <+> qualTyName t <> parens ("0 to slv'length / " <> eSz <+> "- 1") <> semi
        ) <> line <>
    "begin" <> line <>
      indent 2
        ("for i in result'range loop" <> line <>
            indent 2
              ( "result" <> parens "i" <+> ":=" <+> case syn of
                    Vivado -> getElem <> semi
                    _ | BitVector _ <- elTy -> getElem <> semi
                      | otherwise           -> "fromSLV" <> parens getElem <> semi

              ) <> line <>
         "end" <+> "loop" <> semi <> line <>
         "return" <+> "result" <> semi
        ) <> line <>
    "end" <> semi
  )
  where
    eSz     = int (typeSize elTy)
    getElem = "islv" <> parens ("i * " <> eSz <+> "to (i+1) * " <> eSz <+> "- 1")

funDec _ _ = Nothing

tyImports :: Identifier -> VHDLM Doc
tyImports nm = do
  mkId <- Mon (mkIdentifier <*> pure Basic)
  libs <- Mon $ use libraries
  packs <- Mon $ use packages
  punctuate' semi $ sequence
    ([ "library IEEE"
     , "use IEEE.STD_LOGIC_1164.ALL"
     , "use IEEE.NUMERIC_STD.ALL"
     , "use IEEE.MATH_REAL.ALL"
     , "use std.textio.all"
     , "use work.all"
     , "use work." <> pretty (mkId (nm `TextS.append` "_types")) <> ".all"
     ] ++ (map (("library" <+>) . pretty) (nub libs))
       ++ (map (("use" <+>) . pretty) (nub packs)))


-- TODO: Way too much happening on a single line
port :: Num t
     => TextS.Text
     -> HWType
     -> VHDLM Doc
     -> Int
     -> Maybe Expr
     -> VHDLM (Doc, t)
port elName hwType portDirection fillToN iEM =
  (,fromIntegral $ TextS.length elName) <$>
  (encodingNote hwType <> fill fillToN (pretty elName) <+> colon <+> direction
   <+> sizedQualTyName hwType <> iE)
 where
  direction | isBiSignalIn hwType = "inout"
            | otherwise           = portDirection

  iE = maybe emptyDoc (noEmptyInit . expr_ False) iEM

-- [Note] Hack entity attributes in architecture
--
-- By default we print attributes inside the entity block. This conforms
-- to the VHDL standard (IEEE Std 1076-1993, 5.1 Attribute specification,
-- paragraph 9), and is subsequently implemented in this way by open-source
-- simulators such as GHDL.
---
-- Intel and Xilinx use their own annotation schemes unfortunately, which
-- require attributes in the architecture.
--
-- References:
--  * https://www.mail-archive.com/ghdl-discuss@gna.org/msg03175.html
--  * https://forums.xilinx.com/t5/Simulation-and-Verification/wrong-attribute-decorations-of-port-signals-generated-by-write/m-p/704905#M16265
--  * http://quartushelp.altera.com/15.0/mergedProjects/hdl/vhdl/vhdl_file_dir_chip.htm

entity :: Component -> VHDLM Doc
entity c = do
    syn <- Mon hdlSyn
    rec (p,ls) <- fmap unzip (ports (maximum ls))
    "entity" <+> pretty (componentName c) <+> "is" <> line <>
      (case p of
         [] -> emptyDoc
         _  -> case syn of
          -- See: [Note] Hack entity attributes in architecture
          Other -> indent 2 (rports p <> if null attrs then emptyDoc else
                              line <> line <> rattrs) <> line <> "end" <> semi
          _     -> indent 2 (rports p) <> line <> "end" <> semi
      )
  where
    ports l = sequence $ [port iName hwType "in" l Nothing | (iName, hwType) <- inputs c]
                      ++ [port oName hwType "out" l iEM | (_, (oName, hwType), iEM) <- outputs c]

    rports p = "port" <> (parens (align (vcat (punctuate semi (pure p))))) <> semi

    rattrs      = renderAttrs attrs
    attrs       = inputAttrs ++ outputAttrs
    inputAttrs  = [(id_, attr) | (id_, hwtype) <- inputs c, attr <- hwTypeAttrs hwtype]
    outputAttrs = [(id_, attr) | (_wireOrReg, (id_, hwtype), _) <- outputs c, attr <- hwTypeAttrs hwtype]


architecture :: Component -> VHDLM Doc
architecture c = do {
  ; syn <- Mon hdlSyn
  ; let attrs = case syn of
                  -- See: [Note] Hack entity attributes in architecture
                  Other -> declAttrs
                  _     -> inputAttrs ++ outputAttrs ++ declAttrs
  ; nest 2
      (("architecture structural of" <+> pretty (componentName c) <+> "is" <> line <>
       decls (declarations c)) <> line <>
       if null attrs then emptyDoc else line <> line <> renderAttrs attrs) <> line <>
    nest 2
      ("begin" <> line <>
       insts (declarations c)) <> line <>
      "end" <> semi
  }
 where
   netdecls    = filter isNetDecl (declarations c)
   declAttrs   = [(id_, attr) | NetDecl' _ _ id_ (Right hwtype) _ <- netdecls, attr <- hwTypeAttrs hwtype]
   inputAttrs  = [(id_, attr) | (id_, hwtype) <- inputs c, attr <- hwTypeAttrs hwtype]
   outputAttrs = [(id_, attr) | (_wireOrReg, (id_, hwtype), _) <- outputs c, attr <- hwTypeAttrs hwtype]

   isNetDecl :: Declaration -> Bool
   isNetDecl (NetDecl' _ _ _ (Right _) _) = True
   isNetDecl _                            = False

attrType
  :: t ~ HashMap T.Text T.Text
  => t
  -> Attr'
  -> t
attrType types attr =
  case HashMap.lookup name' types of
    Nothing    -> HashMap.insert name' type' types
    Just type'' | type'' == type' -> types
                | otherwise -> error $
                      $(curLoc) ++ unwords [ T.unpack name', "already assigned"
                                           , T.unpack type'', "while we tried to"
                                           , "add", T.unpack type' ]
 where
  name' = T.pack $ attrName attr
  type' = T.pack $ case attr of
            BoolAttr' _ _    -> "boolean"
            IntegerAttr' _ _ -> "integer"
            StringAttr' _ _  -> "string"
            Attr' _          -> "bool"

-- | Create 'attrname -> type' mapping for given attributes. Will err if multiple
-- types are assigned to the same name.
attrTypes :: [Attr'] -> HashMap T.Text T.Text
attrTypes = foldl attrType HashMap.empty

-- | Create a 'attrname -> (type, [(signalname, value)]). Will err if multiple
-- types are assigned to the same name.
attrMap
  :: forall t
   . t ~ HashMap T.Text (T.Text, [(TextS.Text, T.Text)])
  => [(TextS.Text, Attr')]
  -> t
attrMap attrs = foldl go empty' attrs
 where
  empty' = HashMap.fromList
           [(k, (types HashMap.! k, [])) | k <- HashMap.keys types]
  types = attrTypes (map snd attrs)

  go :: t -> (TextS.Text, Attr') -> t
  go map' attr = HashMap.adjust
                   (go' attr)
                   (T.pack $ attrName $ snd attr)
                   map'

  go'
    :: (TextS.Text, Attr')
    -> (T.Text, [(TextS.Text, T.Text)])
    -> (T.Text, [(TextS.Text, T.Text)])
  go' (signalName, attr) (typ, elems) =
    (typ, (signalName, renderAttr attr) : elems)

renderAttrs
  :: [(TextS.Text, Attr')]
  -> VHDLM Doc
renderAttrs (attrMap -> attrs) =
  vcat $ sequence $ intersperse " " $ map renderAttrGroup (assocs attrs)
 where
  renderAttrGroup
    :: (T.Text, (T.Text, [(TextS.Text, T.Text)]))
    -> VHDLM Doc
  renderAttrGroup (attrname, (typ, elems)) =
    ("attribute" <+> string attrname <+> colon <+> string typ <> semi)
    <> line <>
    (vcat $ sequence $ map (renderAttrDecl attrname) elems)

  renderAttrDecl
    :: T.Text
    -> (TextS.Text, T.Text)
    -> VHDLM Doc
  renderAttrDecl attrname (signalName, value) =
        "attribute"
    <+> string attrname
    <+> "of"
    <+> stringS signalName
    <+> colon
    <+> "signal is"
    <+> string value
    <> semi

-- | Return all key/value pairs in the map in arbitrary key order.
assocs :: Eq a => Hashable a => HashMap a b -> [(a,b)]
assocs m = zip keys (map (m HashMap.!) keys)
 where
  keys = (HashMap.keys m)

-- | Convert single attribute to VHDL syntax
renderAttr :: Attr' -> T.Text
renderAttr (StringAttr'  _key value) = T.replace "\\\"" "\"\"" $ T.pack $ show value
renderAttr (IntegerAttr' _key value) = T.pack $ show value
renderAttr (BoolAttr'    _key True ) = T.pack $ "true"
renderAttr (BoolAttr'    _key False) = T.pack $ "false"
renderAttr (Attr'        _key      ) = T.pack $ "true"

sigDecl :: VHDLM Doc -> HWType -> VHDLM Doc
sigDecl d t = d <+> colon <+> sizedQualTyName t

-- | Append size information to given type string
appendSize :: VHDLM Doc -> HWType -> VHDLM Doc
appendSize baseType sizedType = case sizedType of
  BitVector n -> baseType <> parens (int (n-1) <+> "downto 0")
  Signed n    -> baseType <> parens (int (n-1) <+> "downto 0")
  Unsigned n  -> baseType <> parens (int (n-1) <+> "downto 0")
  Vector n _  -> baseType <> parens ("0 to" <+> int (n-1))
  RTree d _   -> baseType <> parens ("0 to" <+> int ((2^d)-1))
  Annotated _ elTy -> appendSize baseType elTy
  _           -> baseType

-- | Same as @qualTyName@, but instantiate generic types with their size.
sizedQualTyName :: HWType -> VHDLM Doc
sizedQualTyName (filterTransparent -> hwty) = appendSize (qualTyName hwty) hwty

-- | Same as @tyName@, but instantiate generic types with their size.
sizedTyName :: HWType -> VHDLM Doc
sizedTyName (filterTransparent -> hwty) = appendSize (tyName hwty) hwty

-- | Same as @tyName@, but return fully qualified name (name, including module)
qualTyName :: HWType -> VHDLM Doc
qualTyName (filterTransparent -> hwty) = case hwty of
  -- Builtin types:
  Bit -> tyName hwty
  Bool -> tyName hwty
  Signed _ -> tyName hwty
  Unsigned _ -> tyName hwty
  BitVector _ -> tyName hwty

  -- Transparent types:
  BiDirectional _ elTy -> qualTyName elTy
  Annotated _ elTy -> qualTyName elTy

  -- Custom types:
  _ -> do
    modName <- Mon (use modNm)
    pretty (TextS.toLower modName) <> "_types." <> tyName hwty

-- | Generates a unique name for a given type. This action will cache its
-- results, thus returning the same answer for the same @HWType@ argument.
-- Some type names do not have specific names, but are instead basic types
-- in VHDL.
tyName
  :: HWType
  -- ^ Type to name
  -> VHDLM Doc
tyName t = do
  nm <- tyName' False t
  pretty nm

-- | Generates a unique name for a given type. This action will cache its
-- results, thus returning the same answer for the same @HWType@ argument.
-- Some type names do not have specific names, but are instead basic types
-- in VHDL.
tyName'
  :: HasCallStack
  => Bool
  -- ^ Include length information in first part of name. For example, say we
  -- want to generate a name for a vector<signed>, where the vector is of length
  -- 5, and signed has 64 bits. When given `True`, this function would
  -- generate `array_of_5_signed_64`. When given `False` it would generate
  -- `array_of_signed_64`. Note that parts other than the first part will always
  -- have length information. This option is useful for generating names in
  -- VHDL, where the `False` case is needed to create generic types.
  -> HWType
  -- ^ Type to name
  -> VHDLM TextS.Text
tyName' rec0 (filterTransparent -> t) = do
  Mon (tyCache %= HashSet.insert t)
  case t of
    KnownDomain {} ->
      return (error ($(curLoc) ++ "Forced to print KnownDomain tyName"))
    Void _ ->
      return (error ($(curLoc) ++ "Forced to print Void tyName: " ++ show t))
    Bool          -> return "boolean"
    Signed n      ->
      let app = if rec0 then ["_", showt n] else [] in
      return $ TextS.concat $ "signed" : app
    Unsigned n    ->
      let app = if rec0 then ["_", showt n] else [] in
      return $ TextS.concat $ "unsigned" : app
    BitVector n   ->
      let app = if rec0 then ["_", showt n] else [] in
      return $ TextS.concat $ "std_logic_vector" : app
    String        -> return "string"
    Integer       -> return "integer"
    Bit           -> return "std_logic"
    Vector n elTy -> do
      elTy' <- tyName' True elTy
      let nm = TextS.concat [ "array_of_"
                            , if rec0 then showt n `TextS.append` "_" else ""
                            , elTy']
      Mon $ makeCached (t, rec0) nameCache (return nm)
    RTree n elTy  -> do
      elTy' <- tyName' True elTy
      let nm = TextS.concat [ "tree_of_"
                            , if rec0 then showt n `TextS.append` "_" else ""
                            , elTy']
      Mon $ makeCached (t, rec0) nameCache (return nm)
    -- TODO: nice formatting for Index. I.e., 2000 = 2e3, 1024 = 2pow10
    Index n ->
      return ("index_" `TextS.append` showt n)
    Clock nm0 ->
      let nm1 = "clk_" `TextS.append` nm0 in
      Mon $ makeCached (t, False) nameCache (userTyName "clk" nm1 t)
    Reset nm0 ->
      let nm1 = "rst_" `TextS.append` nm0 in
      Mon $ makeCached (t, False) nameCache (userTyName "rst" nm1 t)
    Sum nm _  ->
      Mon $ makeCached (t, False) nameCache (userTyName "sum" nm t)
    CustomSum nm _ _ _ ->
      Mon $ makeCached (t, False) nameCache (userTyName "sum" nm t)
    SP nm _ ->
      Mon $ makeCached (t, False) nameCache (userTyName "sp" nm t)
    CustomSP nm _ _ _ ->
      Mon $ makeCached (t, False) nameCache (userTyName "sp" nm t)
    Product nm _ _ ->
      Mon $ makeCached (t, False) nameCache (userTyName "product" nm t)
    CustomProduct nm _ _ _ _ ->
      Mon $ makeCached (t, False) nameCache (userTyName "product" nm t)
    Annotated _ hwTy ->
      tyName' rec0 hwTy
    BiDirectional _ hwTy ->
      tyName' rec0 hwTy
    FileType -> return "file"

-- | Returns underlying type of given HWType. That is, the type by which it
-- eventually will be represented in VHDL.
normaliseType :: HWType -> HWType
normaliseType hwty = case hwty of
  Void {} -> hwty
  KnownDomain {} -> hwty

  -- Base types:
  Bool          -> hwty
  Signed _      -> hwty
  Unsigned _    -> hwty
  BitVector _   -> hwty
  String        -> hwty
  Integer       -> hwty
  Bit           -> hwty
  FileType      -> hwty

  -- Complex types, for which a user defined type is made in VHDL:
  Vector _ _    -> hwty
  RTree _ _     -> hwty
  Product _ _ _ -> hwty

  -- Simple types, for which a subtype (without qualifiers) will be made in VHDL:
  Clock _           -> Bit
  Reset _           -> Bit
  Index _           -> Unsigned (typeSize hwty)
  CustomSP _ _ _ _  -> BitVector (typeSize hwty)
  SP _ _            -> BitVector (typeSize hwty)
  Sum _ _           -> BitVector (typeSize hwty)
  CustomSum _ _ _ _ -> BitVector (typeSize hwty)
  CustomProduct {}  -> BitVector (typeSize hwty)

  -- Transparent types:
  Annotated _ elTy -> normaliseType elTy
  BiDirectional _ elTy -> normaliseType elTy

-- | Recursively remove transparent types from given type
filterTransparent :: HWType -> HWType
filterTransparent hwty = case hwty of
  Bool              -> hwty
  Signed _          -> hwty
  Unsigned _        -> hwty
  BitVector _       -> hwty
  String            -> hwty
  Integer           -> hwty
  Bit               -> hwty
  Clock _           -> hwty
  Reset _           -> hwty
  Index _           -> hwty
  Sum _ _           -> hwty
  CustomSum _ _ _ _ -> hwty
  FileType          -> hwty

  Vector n elTy     -> Vector n (filterTransparent elTy)
  RTree n elTy      -> RTree n (filterTransparent elTy)
  Product nm labels elTys  ->
    Product nm labels (map filterTransparent elTys)

  SP nm0 constrs ->
    SP nm0
      (map (\(nm1, tys) -> (nm1, map filterTransparent tys)) constrs)

  CustomSP nm0 drepr size constrs ->
    CustomSP nm0 drepr size
      (map (\(repr, nm1, tys) -> (repr, nm1, map filterTransparent tys)) constrs)

  CustomProduct nm0 drepr size maybeFieldNames constrs ->
    CustomProduct nm0 drepr size maybeFieldNames
      (map (second filterTransparent) constrs)

  -- Transparent types:
  Annotated _ elTy -> elTy
  BiDirectional _ elTy -> elTy

  Void {} -> hwty
  KnownDomain {} -> hwty

-- | Create a unique type name for user defined types
userTyName
  :: Identifier
  -- ^ Default name
  -> Identifier
  -- ^ Identifier stored in @hwTy@
  -> HWType
  -- ^ Type to give a (unique) name
  -> StateT VHDLState Identity TextS.Text
userTyName dflt nm0 hwTy = do
  tyCache %= HashSet.insert hwTy
  seen <- use tySeen
  mkId <- mkIdentifier <*> pure Basic
  let nm1 = (mkId . last . TextS.splitOn ".") nm0
      nm2 = if TextS.null nm1 then dflt else nm1
      (nm3,count) = case HashMap.lookup nm2 seen of
                      Just cnt -> go mkId seen cnt nm2
                      Nothing  -> (nm2,0)
  tySeen %= HashMap.insert nm3 count
  return nm3
  where
    go mkId seen count nm0' =
      let nm1' = nm0' `TextS.append` TextS.pack ('_':show count) in
      case HashMap.lookup nm1' seen of
        Just _  -> go mkId seen (count+1) nm0'
        Nothing -> (nm1',count+1)


-- | Convert a Netlist HWType to an error VHDL value for that type
sizedQualTyNameErrValue :: HWType -> VHDLM Doc
sizedQualTyNameErrValue Bool                = do
  udf <- Mon (use undefValue)
  case udf of
    Just (Just 0) -> "false"
    _             -> "true"
sizedQualTyNameErrValue Bit                 = singularErrValue
sizedQualTyNameErrValue t@(Vector n elTy)   = do
  syn <-Mon hdlSyn
  case syn of
    Vivado -> qualTyName t <> "'" <> parens (int 0 <+> "to" <+> int (n-1) <+> rarrow <+>
                "std_logic_vector'" <> parens (int 0 <+> "to" <+> int (typeSize elTy - 1) <+>
                 rarrow <+> singularErrValue))
    _ -> qualTyName t <> "'" <> parens (int 0 <+> "to" <+> int (n-1) <+> rarrow <+> sizedQualTyNameErrValue elTy)
sizedQualTyNameErrValue t@(RTree n elTy)    = do
  syn <-Mon hdlSyn
  case syn of
    Vivado -> qualTyName t <> "'" <>  parens (int 0 <+> "to" <+> int (2^n - 1) <+> rarrow <+>
                "std_logic_vector'" <> parens (int 0 <+> "to" <+> int (typeSize elTy - 1) <+>
                 rarrow <+> singularErrValue))
    _ -> qualTyName t <> "'" <>  parens (int 0 <+> "to" <+> int (2^n - 1) <+> rarrow <+> sizedQualTyNameErrValue elTy)
sizedQualTyNameErrValue t@(Product _ _ elTys) =
  qualTyName t <> "'" <> tupled (mapM sizedQualTyNameErrValue elTys)
sizedQualTyNameErrValue (Reset {}) = singularErrValue
sizedQualTyNameErrValue (Clock _)  = singularErrValue
sizedQualTyNameErrValue (Void {})  =
  return (error ($(curLoc) ++ "[CLASH BUG] Forced to print Void error value"))
sizedQualTyNameErrValue String              = "\"ERROR\""
sizedQualTyNameErrValue t =
  qualTyName t <> "'" <> parens (int 0 <+> "to" <+> int (typeSize t - 1) <+> rarrow <+> singularErrValue)

singularErrValue :: VHDLM Doc
singularErrValue = do
  udf <- Mon (use undefValue)
  case udf of
    Nothing       -> "'-'"
    Just Nothing  -> "'0'"
    Just (Just x) -> "'" <> int x <> "'"

vhdlRecSel
  :: HWType
  -> Int
  -> VHDLM Doc
vhdlRecSel p@(Product _ labels tys) i =
  tyName p <> selectProductField labels tys i
vhdlRecSel ty i =
  tyName ty <> "_sel" <> int i

decls :: [Declaration] -> VHDLM Doc
decls [] = emptyDoc
decls ds = do
    rec (dsDoc,ls) <- fmap (unzip . catMaybes) $ mapM (decl (maximum ls)) ds
    case dsDoc of
      [] -> emptyDoc
      _  -> punctuate' semi (pure dsDoc)

decl :: Int ->  Declaration -> VHDLM (Maybe (Doc,Int))
decl l (NetDecl' noteM _ id_ ty iEM) = Just <$> (,fromIntegral (TextS.length id_)) <$>
  maybe id addNote noteM ("signal" <+> fill l (pretty id_) <+> colon <+> either pretty sizedQualTyName ty <> iE)
  where
    addNote n = mappend ("--" <+> pretty n <> line)
    iE = maybe emptyDoc (noEmptyInit . expr_ False) iEM

decl _ (InstDecl Comp _ nm _ gens pms) = fmap (Just . (,0)) $ do
  { rec (p,ls) <- fmap unzip $ sequence [ (,formalLength i) <$> fill (maximum ls) (expr_ False i) <+> colon <+> portDir dir <+> sizedQualTyName ty | (i,dir,ty,_) <- pms ]
  ; rec (g,lsg) <- fmap unzip $ sequence [ (,formalLength i) <$> fill (maximum lsg) (expr_ False i) <+> colon <+> tyName ty | (i,ty,_) <- gens]
  ; "component" <+> pretty nm <> line <>
    ( if null g then emptyDoc
        else indent 2 ("generic" <> line <> tupledSemi (pure g) <> semi) <> line
    )
    <> indent 2 ("port" <+> tupledSemi (pure p) <> semi) <> line <>
    "end component"
  }
 where
    formalLength (Identifier i _) = fromIntegral (TextS.length i)
    formalLength _                = 0

    portDir In  = "in"
    portDir Out = "out"

decl _ _ = return Nothing

noEmptyInit :: VHDLM Doc -> VHDLM Doc
noEmptyInit d = do
  d1 <- d
  if isEmpty d1
     then emptyDoc
     else (space <> ":=" <+> d)

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
        '-'

patLitCustom'
  :: Bits a
  => VHDLM Doc
  -> Int
  -> a
  -> a
  -> VHDLM Doc
patLitCustom' var size mask value =
  let mask' = string $ T.pack $ stdMatch size mask value in
  "std_match" <> parens (dquotes mask' <> comma <+> var)

patLitCustom
  :: VHDLM Doc
  -> HWType
  -> Literal
  -> VHDLM Doc
patLitCustom var (CustomSum _name _dataRepr size reprs) (NumLit (fromIntegral -> i)) =
  patLitCustom' var size mask value
    where
      ((ConstrRepr' _name _n mask value _anns), _id) = reprs !! i

patLitCustom var (CustomSP _name _dataRepr size reprs) (NumLit (fromIntegral -> i)) =
  patLitCustom' var size mask value
    where
      ((ConstrRepr' _name _n mask value _anns), _id, _tys) = reprs !! i

patLitCustom _ x y = error $ $(curLoc) ++ unwords
  [ "You can only pass CustomSP / CustomSum and a NumLit to this function,"
  , "not", show x, "and", show y]

insts :: [Declaration] -> VHDLM Doc
insts [] = emptyDoc
insts (TickDecl id_:ds) = comment "--" id_ <> line <> insts ds
insts (d:ds) = do
  d' <- inst_ d
  case d' of
    Just doc -> pure doc <> line <> line <> insts ds
    _ -> insts ds

-- | Helper function for inst_, handling CustomSP and CustomSum
inst_' :: TextS.Text -> Expr -> HWType -> [(Maybe Literal, Expr)] -> VHDLM (Maybe Doc)
inst_' id_ scrut scrutTy es = fmap Just $
  (pretty id_ <+> larrow <+> align (vcat (conds esNub) <> semi))
    where
      esMod = map (first (fmap (patMod scrutTy))) es
      esNub = nubBy ((==) `on` fst) esMod
      var   = expr_ True scrut

      conds :: [(Maybe Literal,Expr)] -> VHDLM [Doc]
      conds []                = return []
      conds [(_,e)]           = expr_ False e <:> return []
      conds ((Nothing,e):_)   = expr_ False e <:> return []
      conds ((Just c ,e):es') = expr_ False e <+> "when"
                                              <+> patLitCustom var scrutTy c
                                              <+> "else"
                                              <:> conds es'

-- | Turn a Netlist Declaration to a VHDL concurrent block
inst_ :: Declaration -> VHDLM (Maybe Doc)
inst_ (Assignment id_ e) = fmap Just $
  pretty id_ <+> larrow <+> align (expr_ False e) <> semi

inst_ (CondAssignment id_ _ scrut _ [(Just (BoolLit b), l),(_,r)]) = fmap Just $
  pretty id_ <+> larrow
           <+> align (vsep (sequence [expr_ False t <+> "when" <+>
                                      expr_ False scrut <+> "else"
                                     ,expr_ False f <> semi
                                     ]))
  where
    (t,f) = if b then (l,r) else (r,l)

inst_ (CondAssignment id_ _ scrut scrutTy@(CustomSP _ _ _ _) es) =
  inst_' id_ scrut scrutTy es

inst_ (CondAssignment id_ _ scrut scrutTy@(CustomSum _ _ _ _) es) =
  inst_' id_ scrut scrutTy es

inst_ (CondAssignment id_ _ scrut scrutTy@(CustomProduct _ _ _ _ _) es) =
  inst_' id_ scrut scrutTy es

inst_ (CondAssignment id_ _sig scrut scrutTy es) = fmap Just $
    "with" <+> parens (expr_ True scrut) <+> "select" <> line <>
      indent 2 (pretty id_ <+> larrow <+> align (vcat (punctuate comma (conds esNub)) <> semi))
  where
    esMod = map (first (fmap (patMod scrutTy))) es
    esNub = nubBy ((==) `on` fst) esMod

    conds :: [(Maybe Literal,Expr)] -> VHDLM [Doc]
    conds []                = return []
    conds [(_,e)]           = expr_ False e <+> "when" <+> "others" <:> return []
    conds ((Nothing,e):_)   = expr_ False e <+> "when" <+> "others" <:> return []
    conds ((Just c ,e):es') = expr_ False e <+> "when" <+> patLit scrutTy c <:> conds es'

inst_ (InstDecl entOrComp libM nm lbl gens pms) = do
    maybe (return ()) (\lib -> Mon (libraries %= (T.fromStrict lib:))) libM
    fmap Just $
      nest 2 $ pretty lbl <+> colon <> entOrComp'
                <+> maybe emptyDoc ((<> ".") . pretty) libM <> pretty nm <> line <> gms <> pms' <> semi
  where
    gms | [] <- gens = emptyDoc
        | otherwise =  do
      rec (p,ls) <- fmap unzip $ sequence [ (,formalLength i) <$> fill (maximum ls) (expr_ False i) <+> "=>" <+> expr_ False e | (i,_,e) <- gens]
      nest 2 ("generic map" <> line <> tupled (pure p)) <> line
    pms' = do
      rec (p,ls) <- fmap unzip $ sequence [ (,formalLength i) <$> fill (maximum ls) (expr_ False i) <+> "=>" <+> expr_ False e | (i,_,_,e) <- pms]
      nest 2 $ "port map" <> line <> tupled (pure p)
    formalLength (Identifier i _) = fromIntegral (TextS.length i)
    formalLength _                = 0
    entOrComp' = case entOrComp of { Entity -> " entity"; Comp -> " component"; Empty -> ""}

inst_ (BlackBoxD _ libs imps inc bs bbCtx) =
  fmap Just (Mon (column (renderBlackBox libs imps inc bs bbCtx)))

inst_ _ = return Nothing

-- | Render a data constructor application for data constructors having a
-- custom bit representation.
customReprDataCon
  :: DataRepr'
  -- ^ Custom representation of data type
  -> ConstrRepr'
  -- ^ Custom representation of a specific constructor of @dataRepr@
  -> [(HWType, Expr)]
  -- ^ Arguments applied to constructor
  -> VHDLM Doc
customReprDataCon dataRepr constrRepr args =
  "std_logic_vector'" <> parens (hcat $ punctuate " & " $ mapM range origins)
    where
      DataRepr' _typ size _constrs = dataRepr

      -- Build bit representations for all constructor arguments
      argSLVs = map (uncurry toSLV) args :: [VHDLM Doc]

      -- Spread bits of constructor arguments using masks
      origins = bitOrigins dataRepr constrRepr :: [BitOrigin]

      range
        :: BitOrigin
        -> VHDLM Doc
      range (Lit (bitsToBits -> ns)) =
        dquotes $ hcat $ mapM bit_char ns
      range (Field n start end) =
        -- We want to select the bits starting from 'start' downto and including
        -- 'end'. We cannot use "(start downto end)" in VHDL, as the preceeding
        -- expression might be anything. This notation only works on identifiers
        -- unfortunately.
        let fsize = start - end + 1 in
        let expr' = argSLVs !! n in

        -- HACK: While expr' is a std_logic_vector (see call `toSLV`), it cannot
        -- be cast to unsigned in case of literals. This is fixed by explicitly
        -- casting it to std_logic_vector.
        let unsigned = "unsigned" <> parens ("std_logic_vector'" <> parens expr') in

        if | fsize == size ->
               -- If sizes are equal, rotating / resizing amounts to doing nothing
               expr'
           | end == 0 ->
               -- Rotating is not necessary if relevant bits are already at the end
               let resized = "resize" <> parens (unsigned <> comma <> int fsize) in
               "std_logic_vector" <> parens resized
           | otherwise ->
               -- Select bits 'start' downto and including 'end'
               let rotated  = unsigned <+> "srl" <+> int end in
               let resized = "resize" <> parens (rotated <> comma <> int fsize) in
               "std_logic_vector" <> parens resized

-- | Turn a Netlist expression into a VHDL expression
expr_
  :: HasCallStack
  => Bool
  -- ^ Enclose in parentheses?
  -> Expr
  -- ^ Expr to convert
  -> VHDLM Doc
expr_ _ (Literal sizeM lit) = exprLit sizeM lit
expr_ _ (Identifier id_ Nothing) = pretty id_

expr_ _ (Identifier id_ (Just m)) = do
  syn <- Mon hdlSyn
  maybe (pretty id_) (foldr renderModifier (pretty id_)) (buildModifier syn [] m)

expr_ b (DataCon _ (DC (Void {}, -1)) [e]) =  expr_ b e

expr_ _ (DataCon ty@(Vector 0 _) _ _) = sizedQualTyNameErrValue ty

expr_ _ (DataCon ty@(Vector 1 elTy) _ [e])       = do
  syn <- Mon hdlSyn
  case syn of
    Vivado -> qualTyName ty <> "'" <> parens (int 0 <+> rarrow <+> toSLV elTy e)
    _ -> qualTyName ty <> "'" <> parens (int 0 <+> rarrow <+> expr_ False e)
expr_ _ e@(DataCon ty@(Vector _ elTy) _ [e1,e2]) = do
  syn <- Mon hdlSyn
  case syn of
    Vivado -> qualTyName ty <> "'" <> case vectorChain e of
      Just es -> align (tupled (mapM (toSLV elTy) es))
      Nothing -> parens ("std_logic_vector'" <> parens (toSLV elTy e1) <+> "&" <+> expr_ False e2)
    _ -> qualTyName ty <> "'" <> case vectorChain e of
            Just es -> align (tupled (mapM (expr_ False) es))
            Nothing -> parens (qualTyName elTy <> "'" <> parens (expr_ False e1) <+> "&" <+> expr_ False e2)

expr_ _ (DataCon ty@(RTree 0 elTy) _ [e]) = do
  syn <- Mon hdlSyn
  case syn of
    Vivado -> qualTyName ty <> "'" <> parens (int 0 <+> rarrow <+> toSLV elTy e)
    _ -> qualTyName ty <> "'" <> parens (int 0 <+> rarrow <+> expr_ False e)
expr_ _ e@(DataCon ty@(RTree d elTy) _ [e1,e2]) = qualTyName ty <> "'" <> case rtreeChain e of
  Just es -> tupled (mapM (expr_ False) es)
  Nothing -> parens (qualTyName (RTree (d-1) elTy) <> "'" <> parens (expr_ False e1) <+>
                     "&" <+> expr_ False e2)

expr_ _ (DataCon (SP {}) (DC (BitVector _,_)) es) = assignExpr
  where
    argExprs   = map (parens . expr_ False) es
    assignExpr = "std_logic_vector'" <> parens (hcat $ punctuate " & " $ sequence argExprs)

expr_ _ (DataCon ty@(SP _ args) (DC (_,i)) es) = assignExpr
  where
    argTys     = snd $ args !! i
    dcSize     = conSize ty + sum (map typeSize argTys)
    dcExpr     = expr_ False (dcToExpr ty i)
    argExprs   = map parens (zipWith toSLV argTys es)
    extraArg   = case typeSize ty - dcSize of
                   0 -> []
                   n -> [bits (replicate n U)]
    assignExpr = "std_logic_vector'" <> parens (hcat $ punctuate " & " $ sequence (dcExpr:argExprs ++ extraArg))

expr_ _ (DataCon ty@(Sum _ _) (DC (_,i)) []) =
  expr_ False (dcToExpr ty i)
expr_ _ (DataCon ty@(CustomSum _ _ _ tys) (DC (_,i)) []) =
  let (ConstrRepr' _ _ _ value _) = fst $ tys !! i in
  "std_logic_vector" <> parens ("to_unsigned" <> parens (int (fromIntegral value) <> comma <> int (typeSize ty)))
expr_ _ (DataCon (CustomSP _ dataRepr _size args) (DC (_,i)) es) =
  let (cRepr, _, argTys) = args !! i in
  customReprDataCon dataRepr cRepr (zip argTys es)
expr_ _ (DataCon (CustomProduct _ dataRepr _size _labels tys) _ es) |
  DataRepr' _typ _size [cRepr] <- dataRepr =
  customReprDataCon dataRepr cRepr (zip (map snd tys) es)

expr_ _ (DataCon ty@(Product _ labels tys) _ es) =
    tupled $ zipWithM (\i e' -> tyName ty <> selectProductField labels tys i <+> rarrow <+> expr_ False e') [0..] es

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
  , [Literal _ (NumLit n), Literal _ m, Literal _ i] <- extractLiterals bbCtx
  = let NumLit m' = m
        NumLit i' = i
    in exprLit (Just (BitVector (fromInteger n),fromInteger n)) (BitVecLit m' i')

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "Clash.Sized.Internal.BitVector.fromInteger##"
  , [Literal _ m, Literal _ i] <- extractLiterals bbCtx
  = let NumLit m' = m
        NumLit i' = i
    in exprLit (Just (Bit,1)) (BitLit $ toBit m' i')

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "Clash.Sized.Internal.Index.fromInteger#"
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  , Just k <- clogBase 2 n
  , let k' = max 1 k
  = exprLit (Just (Unsigned k',k')) i

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "Clash.Sized.Internal.Index.maxBound#"
  , [Literal _ (NumLit n)] <- extractLiterals bbCtx
  , n > 0
  , Just k <- clogBase 2 n
  , let k' = max 1 k
  = exprLit (Just (Unsigned k',k')) (NumLit (n-1))

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "GHC.Types.I#"
  , [Literal _ (NumLit n)] <- extractLiterals bbCtx
  = do iw <- Mon $ use intWidth
       exprLit (Just (Signed iw,iw)) (NumLit n)

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "GHC.Types.W#"
  , [Literal _ (NumLit n)] <- extractLiterals bbCtx
  = do iw <- Mon $ use intWidth
       exprLit (Just (Unsigned iw,iw)) (NumLit n)

expr_ b (BlackBoxE _ libs imps inc bs bbCtx b') = do
  parenIf (b || b') (Mon (renderBlackBox libs imps inc bs bbCtx <*> pure 0))

expr_ _ (DataTag Bool (Left id_)) = "tagToEnum" <> parens (pretty id_)
expr_ _ (DataTag Bool (Right id_)) = "dataToTag" <> parens (pretty id_)

expr_ _ (DataTag hty@(Sum _ _) (Left id_)) =
  "std_logic_vector" <> parens ("resize" <> parens ("unsigned" <> parens ("std_logic_vector" <> parens (pretty id_)) <> "," <> int (typeSize hty)))
expr_ _ (DataTag (Sum _ _) (Right id_)) = do
  iw <- Mon $ use intWidth
  "signed" <> parens ("std_logic_vector" <> parens ("resize" <> parens ("unsigned" <> parens (pretty id_) <> "," <> int iw)))

expr_ _ (DataTag (Product {}) (Right _))  = do
  iw <- Mon $ use intWidth
  "to_signed" <> parens (int 0 <> "," <> int iw)
expr_ _ (DataTag hty@(SP _ _) (Right id_)) = do {
    ; iw <- Mon $ use intWidth
    ; "signed" <> parens ("std_logic_vector" <> parens (
      "resize" <> parens ("unsigned" <> parens (pretty id_ <> parens (int start <+> "downto" <+> int end))
                          <> "," <> int iw)))
    }
  where
    start = typeSize hty - 1
    end   = typeSize hty - conSize hty

expr_ _ (DataTag (Vector 0 _) (Right _)) = do
  iw <- Mon $ use intWidth
  "to_signed" <> parens (int 0 <> "," <> int iw)
expr_ _ (DataTag (Vector _ _) (Right _)) = do
  iw <- Mon $ use intWidth
  "to_signed" <> parens (int 1 <> "," <> int iw)

expr_ _ (DataTag (RTree 0 _) (Right _)) = do
  iw <- Mon $ use intWidth
  "to_signed" <> parens (int 0 <> "," <> int iw)
expr_ _ (DataTag (RTree _ _) (Right _)) = do
  iw <- Mon $ use intWidth
  "to_signed" <> parens (int 1 <> "," <> int iw)

expr_ _ (ConvBV topM hwty True e) = do
  nm <- Mon $ use modNm
  case topM of
    Nothing -> pretty nm <> "_types" <> dot <> "toSLV" <>
               parens (qualTyName hwty <> "'" <> parens (expr_ False e))
    Just t  -> pretty t <> dot <> pretty t <> "_types" <> dot <> "toSLV" <> parens (expr_ False e)

expr_ _ (ConvBV topM _ False e) = do
  nm <- Mon $ use modNm
  maybe (pretty nm <> "_types" ) (\t -> pretty t <> dot <> pretty t <> "_types") topM <> dot <>
    "fromSLV" <> parens (expr_ False e)

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
rtreeChain (DataCon (RTree 1 _) _ [e])     = Just [e]
rtreeChain (DataCon (RTree _ _) _ [e1,e2]) = liftA2 (++) (rtreeChain e1) (rtreeChain e2)
rtreeChain _ = Nothing

exprLit :: Maybe (HWType,Size) -> Literal -> VHDLM Doc
exprLit Nothing (NumLit i) = integer i

exprLit (Just (hty,sz)) (NumLit i) = case hty of
  Unsigned n
    | i < (-2^(31 :: Integer)) -> "unsigned" <> parens ("std_logic_vector" <> parens ("signed'" <> parens lit))
    | i < 0                    -> "unsigned" <> parens ("std_logic_vector" <> parens ("to_signed" <> parens(integer i <> "," <> int n)))
    | i < 2^(31 :: Integer) -> "to_unsigned" <> parens (integer i <> "," <> int n)
    | otherwise -> "unsigned'" <> parens lit
  Signed n
    | i < 2^(31 :: Integer) && i > (-2^(31 :: Integer)) -> "to_signed" <> parens (integer i <> "," <> int n)
    | otherwise -> "signed'" <> parens lit
  BitVector _ -> "std_logic_vector'" <> parens lit
  Bit         -> squotes (int (fromInteger i `mod` 2))
  _           -> blit

  where
    validHexLit = sz `mod` 4 == 0 && sz /= 0
    lit = if validHexLit then hlit else blit
    blit = bits (toBits sz i)
    i'   = case hty of
             Signed _ -> let mask = 2^(sz-1) in case divMod i mask of
                (s,i'') | even s    -> i''
                        | otherwise -> i'' - mask
             _ -> i `mod` 2^sz
    hlit = (if i' < 0 then "-" else emptyDoc) <> hex (toHex sz i')

exprLit (Just (hty,sz)) (BitVecLit m i) = case m of
  0 -> exprLit (Just (hty,sz)) (NumLit i)
  _ -> "std_logic_vector'" <> parens bvlit
  where
    bvlit = bits (toBits' sz m i)


exprLit _             (BoolLit t)   = if t then "true" else "false"
exprLit _             (BitLit b)    = squotes $ bit_char b
exprLit _             (StringLit s) = pretty . T.pack $ show s
exprLit _             l             = error $ $(curLoc) ++ "exprLit: " ++ show l

patLit :: HWType -> Literal -> VHDLM Doc
patLit Bit (NumLit i) = if i == 0 then "'0'" else "'1'"
patLit hwTy (NumLit i) =
  let sz = conSize hwTy
  in  case sz `mod` 4 of
        0 -> hex  (toHex sz i)
        _ -> bits (toBits sz i)
patLit _    l          = exprLit Nothing l

patMod :: HWType -> Literal -> Literal
patMod hwTy (NumLit i) = NumLit (i `mod` (2 ^ typeSize hwTy))
patMod _ l = l

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

bits :: [Bit] -> VHDLM Doc
bits = dquotes . hcat . mapM bit_char

toHex :: Int -> Integer -> String
toHex sz i =
  let Just d = clogBase 16 (2^sz)
  in  printf ("%0" ++ show d ++ "X") (abs i)

hex :: String -> VHDLM Doc
hex s = char 'x' <> dquotes (pretty (T.pack s))

bit_char :: Bit -> VHDLM Doc
bit_char H = char '1'
bit_char L = char '0'
bit_char U = do
  udf <- Mon (use undefValue)
  case udf of
    Nothing -> char '-'
    Just Nothing -> char '0'
    Just (Just i) -> "'" <> int i <> "'"
bit_char Z = char 'Z'

toSLV :: HasCallStack => HWType -> Expr -> VHDLM Doc
toSLV Bool         e = do
  nm <- Mon $ use modNm
  pretty (TextS.toLower nm) <> "_types.toSLV" <> parens (expr_ False e)
toSLV Bit          e = do
  nm <- Mon $ use modNm
  pretty (TextS.toLower nm) <> "_types.toSLV" <> parens (expr_ False e)
toSLV (Clock {})    e = do
  nm <- Mon $ use modNm
  pretty (TextS.toLower nm) <> "_types.toSLV" <> parens (expr_ False e)
toSLV (Reset {})    e = do
  nm <- Mon $ use modNm
  pretty (TextS.toLower nm) <> "_types.toSLV" <> parens (expr_ False e)
toSLV (BitVector _) e = expr_ True e
toSLV (Signed _)   e = "std_logic_vector" <> parens (expr_ False e)
toSLV (Unsigned _) e = "std_logic_vector" <> parens (expr_ False e)
toSLV (Index _)    e = "std_logic_vector" <> parens (expr_ False e)
toSLV (Sum _ _)    e = expr_ False e
toSLV (CustomSum _ _dataRepr size reprs) (DataCon _ (DC (_,i)) _) =
  let (ConstrRepr' _ _ _ value _) = fst $ reprs !! i in
  let unsigned = "to_unsigned" <> parens (int (fromIntegral value) <> comma <> int size) in
  "std_logic_vector" <> parens unsigned
toSLV (CustomSum {}) e = "std_logic_vector" <> parens (expr_ False e)
toSLV t@(Product _ labels tys) (Identifier id_ Nothing) = do
    selIds' <- sequence selIds
    encloseSep lparen rparen " & " (zipWithM toSLV tys selIds')
  where
    tName    = tyName t
    selNames = map (fmap (T.toStrict . renderOneLine) ) [pretty id_ <> dot <> tName <> selectProductField labels tys i | i <- [0..(length tys)-1]]
    selIds   = map (fmap (\n -> Identifier n Nothing)) selNames
toSLV (Product _ _ tys) (DataCon _ _ es) | equalLength tys es =
  -- Need equalLenght for code seen in ZipWithUnitVector
  encloseSep lparen rparen " & " (zipWithM toSLV tys es)
toSLV (CustomProduct _ _ _ _ _) e = do
  -- Custom representations are represented as bitvectors in HDL, so we don't
  -- need to do anything.
  expr_ False e
toSLV t@(Product _ _ _) e = do
  nm <- Mon $ use modNm
  pretty (TextS.toLower nm) <> "_types.toSLV" <> parens (qualTyName t <> "'" <> parens (expr_ False e))
toSLV (SP _ _) e       = expr_ False e
toSLV (CustomSP _ _ _ _) e =
  -- Custom representations are represented as bitvectors in HDL, so we don't
  -- need to do anything.
  expr_ False e
toSLV (Vector n elTy) (Identifier id_ Nothing) = do
    selIds' <- sequence selIds
    syn <- Mon hdlSyn
    parens (vcat $ punctuate " & "
      (case syn of
        Vivado -> mapM (expr_ False) selIds'
        _ -> mapM (toSLV elTy) selIds'))
  where
    selNames = map (fmap (T.toStrict . renderOneLine) ) $ [pretty id_ <> parens (int i) | i <- [0 .. (n-1)]]
    selIds   = map (fmap (`Identifier` Nothing)) selNames
-- Don't split up newtype wrappers, or void-filtered types
toSLV (Vector _ _) e@(DataCon _ (DC (Void Nothing, -1)) _) = do
  nm <- Mon $ use modNm
  pretty (TextS.toLower nm) <> "_types.toSLV" <> parens (expr_ False e)
toSLV (Vector n elTy) (DataCon _ _ es) =
  "std_logic_vector'" <> (parens $ vcat $ punctuate " & " (zipWithM toSLV [elTy,Vector (n-1) elTy] es))
toSLV (Vector _ _) e = do
  nm <- Mon $ use modNm
  pretty (TextS.toLower nm) <> "_types.toSLV" <> parens (expr_ False e)
toSLV (RTree _ _) e = do
  nm <- Mon (use modNm)
  pretty (TextS.toLower nm) <> "_types.toSLV" <> parens (expr_ False e)
toSLV hty e = error $ $(curLoc) ++  "toSLV:\n\nType: " ++ show hty ++ "\n\nExpression: " ++ show e

dcToExpr :: HWType -> Int -> Expr
dcToExpr ty i = Literal (Just (ty,conSize ty)) (NumLit (toInteger i))

larrow :: VHDLM Doc
larrow = "<="

rarrow :: VHDLM Doc
rarrow = "=>"

parenIf :: Monad m => Bool -> Mon m Doc -> Mon m Doc
parenIf True  = parens
parenIf False = id

punctuate' :: Monad m => Mon m Doc -> Mon m [Doc] -> Mon m Doc
punctuate' s d = vcat (punctuate s d) <> s

encodingNote :: HWType -> VHDLM Doc
encodingNote (Clock _)  = "-- clock" <> line
encodingNote (Reset _ ) = "-- reset" <> line
encodingNote (Annotated _ t) = encodingNote t
encodingNote _          = emptyDoc

tupledSemi :: Applicative f => f [Doc] -> f Doc
tupledSemi = align . encloseSep (flatAlt (lparen <+> emptyDoc) lparen)
                                (flatAlt (emptyDoc <+> rparen) rparen)
                                (semi <+> emptyDoc)

-- | VHDL name modifiers
data VHDLModifier
  -- | SLV slice (descending index)
  = Range    Range
  -- | Element selection
  | Idx      Int
  -- | Array slice (ascending index)
  | Slice    Int Int
  -- | Selected names
  | Select   (VHDLM Doc)
  -- | Projecting a 'Word#' out of a 'Word8', or 'Int#' ouf of an 'Int8', see
  -- [Note] integer projection
  | Resize
  -- | Projecting a 'Natural' out of a 'BitVector', see [Note] bitvector projection
  | ResizeAndConvert
  -- | Projecting the mask out of a 'BitVector', see [Note] mask projection
  | DontCare

-- | Create a sequence of VHDL name modifiers from our internal 'Modifier'
-- data type. Note that the modifiers are in "reverse" order, so build a
-- complete modified name using 'foldr' over the list by this function.
--
-- [Note] Continuing from an SLV slice
-- SOP and custom products are represented as std_logic_vector, this means that
-- their elements are also std_logic_vector. So when we project an element out
-- of an SOP or custom project, and want to do a further projection on that,
-- we have to do further SLV slicing; instead of e.g. creating a 'selected'
-- modifier. Finally, when we render the modified name, we have to check
-- whether the ultimately projected type needs to be converted from this SLV
-- slice, to the proper type.
buildModifier
  :: HasCallStack
  => HdlSyn
  -> [(VHDLModifier,HWType)]
  -- ^ The list of modifiers so far, note that this list is in reverse order
  -- in which they should eventually be applied to the name we want to modify
  -> Modifier
  -> Maybe [(VHDLModifier,HWType)]
  -- ^ 'Nothing' indicates that the 'Modifier' does not result into a VHDL name
  -- modifier. i.e. we can use the identifier as is; this happens when we get
  -- projections out of product types with only one non-zero field.
buildModifier _ prevM (Sliced (_,start,end)) = case prevM of
  (prev:rest)
    | (Range r,_) <- prev -> -- See [Note] Continuing from an SLV slice
      Just (first Range (continueWithRange [(start,end)] hty r) : rest)
  _ ->
      Just ((Range (Contiguous start end),hty) : prevM)
 where
  hty = BitVector (start-end+1)

buildModifier _ prevM (Indexed (ty@(SP _ args),dcI,fI)) = case prevM of
  (prev:rest)
    | (Range r,_) <- prev -> -- See [Note] Continuing from an SLV slice
      Just (first Range (continueWithRange [(start,end)] argTy r) : rest)
  _ ->
      Just ((Range (Contiguous start end),argTy) : prevM)
 where
  argTys   = snd (indexNote "SOP type: invalid constructor index" args dcI)
  argTy    = indexNote "SOP type: invalid field index" argTys fI
  argSize  = typeSize argTy
  other    = otherSize argTys (fI-1)
  start    = typeSize ty - 1 - conSize ty - other
  end      = start - argSize + 1

buildModifier _ prevM (Indexed (ty@(Product _ labels tys),_,fI)) = case prevM of
  (prev:rest)
    | (Range r,_) <- prev -> -- See [Note] Continuing from an SLV slice
      let argSize = typeSize argTy
          otherSz = otherSize tys (fI - 1)
          start   = typeSize ty - 1 - otherSz
          end     = start - argSize + 1
      in  Just (first Range (continueWithRange [(start,end)] argTy r) : rest)
  _ ->
      let d = dot <> tyName ty <> selectProductField labels tys fI
      in  Just ((Select d,argTy):prevM)
 where
  argTy = indexNote "Product type: invalid field index" tys fI

buildModifier syn prevM (Indexed (ty@(Vector _ argTy),1,0)) = case prevM of
  (prev:rest)
    | (Range r,_) <- prev -> -- See [Note] Continuing from an SLV slice
      let argSize = typeSize argTy
          start   = typeSize ty - 1
          end     = start - argSize + 1
      in  Just (first Range (continueWithRange [(start,end)] argTy r) : rest)
    | (Slice start _,Vector _ argTyP) <- prev
    , argTy == argTyP ->
      -- If the last modifier was an array slice, we just pick its first element
      Just (vivadoRange syn argTy ((Idx start,argTy):rest))
  _ ->
      Just (vivadoRange syn argTy ((Idx 0,argTy):prevM))

buildModifier _ prevM (Indexed (ty@(Vector n argTy),1,1)) = case prevM of
  (prev:rest)
    | (Range r,_) <- prev -> -- See [Note] Continuing from an SLV slice
      let argSize = typeSize argTy
          start   = typeSize ty - argSize - 1
      in  Just (first Range (continueWithRange [(start,0)] tyN r) : rest)
    | (Slice start end,Vector _ argTyP) <- prev
    , argTy == argTyP ->
      -- If the last modifier was an array slice, we just pick the tail of that slice
      Just ((Slice (start + 1) end,tyN) : rest)
  _ ->
      Just ((Slice 1 (n-1),tyN) : prevM)
 where
  tyN = Vector (n-1) argTy

buildModifier syn prevM (Indexed (ty@(RTree _ argTy),0,0)) = case prevM of
  (prev:rest)
    | (Range r,_) <- prev -> -- See [Note] Continuing from an SLV slice
      let start = typeSize ty - 1
      in  Just (first Range (continueWithRange [(start,0)] argTy r) : rest)
    | (Slice start _,RTree _ argTyP) <- prev
    , argTy == argTyP ->
      -- If the last modifier was an array slice, we just pick its first element
      Just (vivadoRange syn argTy ((Idx start,argTy):rest))
  _ ->
      Just (vivadoRange syn argTy ((Idx 0,argTy):prevM))

buildModifier _ prevM (Indexed (ty@(RTree d argTy),1,0)) = case prevM of
  (prev:rest)
    | (Range r,_) <- prev -> -- See [Note] Continuing from an SLV slice
      let start = typeSize ty - 1
          end   = typeSize ty `div` 2
      in  Just (first Range (continueWithRange [(start,end)] tyN r) : rest)
    | (Slice start _,RTree _ argTyP) <- prev
    , argTy == argTyP ->
      -- If the last modifier was an array slice, we just pick the left half
      Just ((Slice start (start+z-1),tyN) : rest)
  _ ->
      Just ((Slice 0 (z-1),tyN) : prevM)
 where
  tyN = RTree (d-1) argTy
  z   = 2^(d - 1)

buildModifier _ prevM (Indexed (ty@(RTree d argTy),1,1)) = case prevM of
  (prev:rest)
    | (Range r,_) <- prev -> -- See [Note] Continuing from an SLV slice
      let start = typeSize ty `div` 2 - 1
      in  Just (first Range (continueWithRange [(start,0)] tyN r) : rest)
    | (Slice _ end,RTree _ argTyP) <- prev
    , argTy == argTyP ->
      -- If the last modifier was an array slice, we just pick the right half
      Just ((Slice (end - z + 1) end,tyN) : rest)
  _ ->
      Just ((Slice z (z'-1),tyN) : prevM)
 where
  tyN = RTree (d-1) argTy
  z   = 2^(d - 1)
  z'  = 2^d

-- This is a HACK for Clash.Driver.TopWrapper.mkOutput
-- Vector's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
buildModifier syn prevM (Indexed (ty@(Vector _ argTy),10,fI)) = case prevM of
  (prev:rest)
    | (Range r,_) <- prev -> -- See [Note] Continuing from an SLV slice
      let argSize = typeSize argTy
          start   = typeSize ty - (fI * argSize) - 1
          end     = start - argSize + 1
      in  Just (first Range (continueWithRange [(start,end)] argTy r) : rest)
    | (Slice start _,Vector _ argTyP) <- prev
    , argTy == argTyP ->
      -- If the last modifier was an array slice, we offset from its starting element
      Just (vivadoRange syn argTy ((Idx (start+fI),argTy):rest))
  _ ->
      Just (vivadoRange syn argTy (((Idx fI,argTy):prevM)))

-- This is a HACK for Clash.Driver.TopWrapper.mkOutput
-- RTree's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
buildModifier syn prevM (Indexed (ty@(RTree _ argTy),10,fI)) = case prevM of
  (prev:rest)
    | (Range r,_) <- prev -> -- See [Note] Continuing from an SLV slice
      let argSize = typeSize argTy
          start   = typeSize ty - (fI * argSize) - 1
          end     = start - argSize + 1
      in  Just (first Range (continueWithRange [(start,end)] argTy r) : rest)
    | (Slice start _,RTree 1 argTyP) <- prev
    , argTy == argTyP ->
      -- If the last modifier was an array slice, we offset from its starting element
      Just (vivadoRange syn argTy ((Idx (start+fI),argTy):rest))
  _ ->
      Just (vivadoRange syn argTy ((Idx fI,argTy):prevM))

buildModifier _ prevM (Indexed (CustomSP _ dataRepr size args,dcI,fI))
  | Void {} <- argTy
  = error (unexpectedProjectionErrorMsg dataRepr dcI fI)
  | otherwise
  = case prevM of
    (prev:rest)
      | (Range r,_) <- prev -> -- See [Note] Continuing from an SLV slice
        Just (first Range (continueWithRange ses argTy r) : rest)
    _ ->
        Just (first Range (continueWithRange ses argTy (Contiguous (size-1) 0)) : prevM)
 where
  (ConstrRepr' _name _n _mask _value anns, _, argTys) =
    indexNote "Custom SOP type: invalid constructor index" args dcI
  ses = bitRanges (indexNote "Custom SOP type: invalid annotation index" anns fI)
  argTy = indexNote "Custom SOP type: invalid field index" argTys fI

buildModifier _ prevM (Indexed (CustomProduct _ dataRepr size _ args,dcI,fI))
  | Void {} <- argTy
  = error (unexpectedProjectionErrorMsg dataRepr dcI fI)
  | DataRepr' _typ _size [cRepr] <- dataRepr
  , ConstrRepr' _cName _pos _mask _val fieldAnns <- cRepr
  , let ses = bitRanges (indexNote "Custom product type: invalid annotation index"
                         fieldAnns fI)
  = case prevM of
      (prev:rest)
        | (Range r,_) <- prev -> -- See [Note] Continuing from an SLV slice
          Just (first Range (continueWithRange ses argTy r) : rest)
      _ ->
          Just (first Range (continueWithRange ses argTy (Contiguous (size-1) 0)):prevM)
 where
  argTy = snd (indexNote "Custom product type: invalid field index" args fI)

buildModifier _ prevM (DC (ty@(SP _ _),_)) = case prevM of
  (prev:rest)
    | (Range r,_) <- prev -> -- See [Note] Continuing from an SLV slice
      Just (first Range (continueWithRange [(start,end)] tyN r) : rest)
  _ ->
      Just ((Range (Contiguous start end),tyN):prevM)
 where
  start = typeSize ty - 1
  end   = typeSize ty - conSize ty
  tyN   = BitVector (start - end + 1)

buildModifier syn prevM (Nested m1 m2) = case buildModifier syn prevM m1 of
  Nothing -> buildModifier syn prevM m2
  Just prevM1 -> case buildModifier syn prevM1 m2 of
      -- In case the second modifier is `Nothing` that means we want the entire
      -- thing calculated by the first modifier
      Nothing -> Just prevM1
      m       -> m

-- [Note] integer projection
--
-- The idea behind these expressions is to translate cases like:
--
-- > :: Int8 -> Int#
-- > \case I8# i -> i
--
-- Which is fine, because no bits are lost. However, these expression might
-- also be the result of the W/W transformation (or uses of unsafeToInteger)
-- for:
--
-- > :: Signed 128 -> Integer
-- > \case S i -> i
--
-- which is very bad because `Integer` is represented by 64 bits meaning we
-- we lose the top 64 bits in the above translation.
--
-- Just as bad is that
--
-- > :: Word8 -> Word#
-- > \case W8# w -> w
--
-- > :: Unsigned 8 -> Integer
-- > \case U i -> i
--
-- result in the same expression... even though their resulting types are
-- different. TODO: this needs  to be fixed!
buildModifier _ prevM (Indexed (ty@(Signed _),_,_)) = Just ((Resize,ty):prevM)
buildModifier _ prevM (Indexed (ty@(Unsigned _),_,_)) = Just ((Resize,ty):prevM)

-- [Note] mask projection
--
-- This covers the case of either:
--
-- `Clash.Sized.Internal.BitVector.unsafeToMask` or
--
-- > :: BitVector 8 -> Integer
-- > \case BV m wild -> m
--
-- introduced by the W/W transformation. Both of which we prefer not to see
-- but will allow. Since the mask is pretty much a simulation artifact we
-- emit don't cares so stuff gets optimised away.
buildModifier _ prevM (Indexed (ty@(BitVector _),_,0)) = Just ((DontCare,ty):prevM)

-- [Note] bitvector projection
--
-- This covers the case of either:
--
-- `Clash.Sized.Internal.BitVector.unsafeToNatural` or
--
-- > :: BitVector 8 -> Integer
-- > \case BV wild i -> i
--
-- introduced by the W/W transformation. Both of which we prefer not to see
-- but will allow.
buildModifier _ prevM (Indexed (ty@(BitVector _),_,1)) = Just ((ResizeAndConvert,ty):prevM)

buildModifier _ _ _ = Nothing

-- | Add an SLV slice for the entire element when we're in the Vivado code-path.
-- This is needed after an element projection from an array (Vec or RTree), as
-- elements are stored as SLVs in the Vivado code-path. This enabled two things:
--
-- 1. Nested modifiers treat the projected element as an SLV, and adjust their
--    projection behavior accordingly.
-- 2. Projected elements are converted from SLV to the proper VHDL type.
vivadoRange
  :: HdlSyn
  -> HWType
  -> [(VHDLModifier, HWType)]
  -> [(VHDLModifier, HWType)]
vivadoRange syn ty mods = case syn of
  Vivado -> (Range (Contiguous (typeSize ty - 1) 0),ty):mods
  _ -> mods

-- | Render a VHDL modifier on to of a (potentially modified) VHDL name
renderModifier
  :: (VHDLModifier,HWType)
  -> VHDLM Doc
  -- ^ (Potentially modified) VHDL name
  -> VHDLM Doc
  -- ^ Modified VHDL name
renderModifier (Idx n,_) doc = doc <> parens (int n)
renderModifier (Slice start end,_) doc = doc <> parens (int start <+> "to" <+> int end)
renderModifier (Select sel,_) doc = doc <> sel
-- See [Note] integer projection
renderModifier (Resize,ty) doc = do
  iw <- Mon (use intWidth)
  -- These integer projections always come last, so it's safe not to return a
  -- modified name, but an expression instead.
  traceIf (iw < typeSize ty) ($(curLoc) ++ "WARNING: result smaller than argument") $
    "resize" <> parens (doc <> "," <> int iw)
renderModifier (ResizeAndConvert,ty) doc = do
  iw <- Mon (use intWidth)
  -- These natural projections always come last, so it's safe not to return a
  -- modified name, but an expression instead.
  traceIf (iw < typeSize ty) ($(curLoc) ++ "WARNING: result smaller than argument") $
    "resize" <> parens ("unsigned" <> parens doc <> "," <> int iw)
-- See [Note] mask projection
renderModifier (DontCare,_) _ = do
  iw <- Mon (use intWidth)
  -- These mask projections always come last, so it's safe not to return a
  -- modified name, but an expression instead.
  traceIf True ($(curLoc) ++ "WARNING: rendering bitvector mask as dontcare") $
    sizedQualTyNameErrValue (Unsigned iw)
renderModifier (Range r,t) doc = do
  nm <- Mon (use modNm)
  let doc1 = case r of
        Contiguous start end -> slice start end
        Split rs -> parens (hcat (punctuate " & " (mapM (\(s,e,_) -> slice s e) rs)))
  case normaliseType t of
    BitVector _ -> doc1
    -- See [Note] Continuing from an SLV slice
    _ ->
      qualTyName t <> "'" <>
      parens (pretty (TextS.toLower nm) <> "_types.fromSLV" <> parens doc1)
 where
  slice s e = doc <> parens (int s <+> "downto" <+> int e)
