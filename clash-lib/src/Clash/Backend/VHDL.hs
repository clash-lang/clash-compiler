{-|
  Copyright   :  (C) 2015-2016, University of Twente,
                     2017-2018, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Generate VHDL for assorted Netlist datatypes
-}

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Clash.Backend.VHDL (VHDLState) where

import           Control.Applicative                  (liftA2)
import           Control.Lens                         hiding (Indexed)
import           Control.Monad                        (forM,join,liftM,zipWithM)
import           Control.Monad.State                  (State)
import           Data.Bits                            (testBit, Bits)
import           Data.Hashable                        (Hashable)
import           Data.HashMap.Lazy                    (HashMap)
import qualified Data.HashMap.Lazy                    as HashMap
import           Data.HashSet                         (HashSet)
import qualified Data.HashSet                         as HashSet
import           Data.List                            (mapAccumL,nub,nubBy,intersperse)
import           Data.Maybe                           (catMaybes,fromMaybe,mapMaybe)
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid                          hiding (Sum, Product)
#endif
import           Data.Semigroup.Monad.Extra
import qualified Data.Text.Lazy                       as T
import qualified Data.Text                            as TextS
import           Data.Text.Prettyprint.Doc.Extra
import           GHC.Stack                            (HasCallStack)
import qualified System.FilePath
import           Text.Printf

import           Clash.Annotations.Primitive          (HDL (..))
import           Clash.Annotations.BitRepresentation.Internal
  (ConstrRepr'(..))
import           Clash.Annotations.BitRepresentation.ClashLib
  (bitsToBits)
import           Clash.Annotations.BitRepresentation.Util
  (BitOrigin(Lit, Field), bitOrigins, bitRanges)
import           Clash.Backend
import           Clash.Core.Var                       (Attr'(..),attrName)
import           Clash.Netlist.BlackBox.Types         (HdlSyn (..))
import           Clash.Netlist.BlackBox.Util
  (extractLiterals, renderBlackBox, renderFilePath)
import           Clash.Netlist.Id                     (IdType (..), mkBasicId')
import           Clash.Netlist.Types                  hiding (_intWidth, intWidth)
import           Clash.Netlist.Util                   hiding (mkIdentifier)
import           Clash.Signal.Internal                (ClockKind (..))
import           Clash.Util
  (SrcSpan, noSrcSpan, clogBase, curLoc, first, makeCached, on, traceIf, (<:>))
import           Clash.Util.Graph                     (reverseTopSort)

#ifdef CABAL
import qualified Paths_clash_lib
#endif

-- | State for the 'Clash.Netlist.VHDL.VHDLM' monad:
data VHDLState =
  VHDLState
  { _tyCache   :: (HashSet HWType)     -- ^ Previously encountered HWTypes
  , _tySeen    :: [Identifier]         -- ^ Generated product types
  , _nameCache :: (HashMap HWType Doc) -- ^ Cache for previously generated product type names
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
  , _idSeen    :: [Identifier]
  , _intWidth  :: Int                  -- ^ Int/Word/Integer bit-width
  , _hdlsyn    :: HdlSyn               -- ^ For which HDL synthesis tool are we generating VHDL
  }

makeLenses ''VHDLState

squote :: Mon (State VHDLState) Doc
squote = string "'"

primsRoot :: IO FilePath
#ifdef CABAL
primsRoot = Paths_clash_lib.getDataFileName "prims"
#else
primsRoot = return ("clash-lib" System.FilePath.</> "prims")
#endif

instance Backend VHDLState where
  initBackend     = VHDLState HashSet.empty [] HashMap.empty "" noSrcSpan [] [] [] [] [] []
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
  hdlType Internal      ty = vhdlType ty
  hdlType (External nm) ty = case ty of
    Vector _ _  -> pretty nm <> dot <> vhdlType ty
    RTree _ _   -> pretty nm <> dot <> vhdlType ty
    Product _ _ -> pretty nm <> dot <> vhdlType ty
    _           -> vhdlType ty
  hdlTypeErrValue = vhdlTypeErrValue
  hdlTypeMark     = vhdlTypeMark
  hdlRecSel       = vhdlRecSel
  hdlSig t ty     = sigDecl (pretty t) ty
  genStmt         = const emptyDoc
  inst            = inst_
  expr            = expr_
  iwWidth         = use intWidth
  toBV _ id_      = do
    nm <- Mon $ use modNm
    pretty (TextS.toLower nm) <> "_types.toSLV" <> parens (pretty id_)
  fromBV _ id_  = do
    nm <- Mon $ use modNm
    pretty (TextS.toLower nm) <> "_types.fromSLV" <> parens (pretty id_)
  hdlSyn          = use hdlsyn
  mkIdentifier    = return go
    where
      go Basic nm = (stripTrailingUnderscore . filterReserved)
                    (TextS.toLower (mkBasicId' True nm))
      go Extended (rmSlash -> nm) = case go Basic nm of
        nm' | nm /= nm' -> TextS.concat ["\\",nm,"\\"]
            |otherwise  -> nm'
  extendIdentifier = return go
    where
      go Basic nm ext = (stripTrailingUnderscore . filterReserved)
                        (TextS.toLower (mkBasicId' True (nm `TextS.append` ext)))
      go Extended ((rmSlash . escapeTemplate) -> nm) ext =
        let nmExt = nm `TextS.append` ext
        in  case go Basic nm ext of
              nm' | nm' /= nmExt -> case TextS.head nmExt of
                      '#' -> TextS.concat ["\\",nmExt,"\\"]
                      _   -> TextS.concat ["\\#",nmExt,"\\"]
                  | otherwise    -> nm'

  setModName nm s = s {_modNm = nm}
  setSrcSpan      = (srcSpan .=)
  getSrcSpan      = use srcSpan
  blockDecl nm ds = do
    decs <- decls ds
    let attrs = [ (id_, attr)
                | NetDecl' _ _ id_ (Right hwtype) <- ds
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

rmSlash :: Identifier -> Identifier
rmSlash nm = fromMaybe nm $ do
  nm1 <- TextS.stripPrefix "\\" nm
  pure (TextS.filter (not . (== '\\')) nm1)

type VHDLM a = Mon (State VHDLState) a

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
  ,"to_integer", "to_signed", "to_unsigned", "string"]

filterReserved :: Identifier -> Identifier
filterReserved s = if s `elem` reservedWords
  then s `TextS.append` "_r"
  else s

stripTrailingUnderscore :: Identifier -> Identifier
stripTrailingUnderscore = TextS.dropWhileEnd (== '_') -- TextS.takeWhile (/= '_')

-- | Generate VHDL for a Netlist component
genVHDL :: Identifier -> SrcSpan -> [Identifier] -> Component -> VHDLM ((String,Doc),[(String,Doc)])
genVHDL nm sp seen c = preserveSeen $ do
    Mon $ idSeen .= seen
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
mkTyPackage_ modName hwtys = do
    { syn <- Mon hdlSyn
    ; mkId <- Mon (mkIdentifier <*> pure Basic)
    ; let usedTys     = concatMap mkUsedTys hwtys
    ; normTys <- nub <$> mapM (fmap mkVecZ . normaliseType) (hwtys ++ usedTys)
    ; let sortedTys   = topSortHWTys normTys
          packageDec  = vcat $ mapM tyDec sortedTys
          (funDecs,funBodies) = unzip . mapMaybe (funDec syn) $ nubBy eqTypM sortedTys

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
    eqTypM (Clock _ _ g) (Clock _ _ g')  = g == g'
    eqTypM ty1 ty2 = ty1 == ty2

mkUsedTys :: HWType
        -> [HWType]
mkUsedTys v@(Vector _ elTy)   = v : mkUsedTys elTy
mkUsedTys v@(RTree _ elTy)    = v : mkUsedTys elTy
mkUsedTys p@(Product _ elTys) = p : concatMap mkUsedTys elTys
mkUsedTys sp@(SP _ elTys)     = sp : concatMap mkUsedTys (concatMap snd elTys)
mkUsedTys t                   = [t]

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
        Left err -> error ("[BUG IN CLASH] topSortHWTys: " ++ err)
        Right ns -> ns

    edge t@(Vector _ elTy) = maybe [] ((:[]) . (HashMap.lookupDefault (error $ $(curLoc) ++ "Vector") t nodesI,))
                                      (HashMap.lookup (mkVecZ elTy) nodesI)
    edge t@(RTree _ elTy)  = maybe [] ((:[]) . (HashMap.lookupDefault (error $ $(curLoc) ++ "RTree") t nodesI,))
                                      (HashMap.lookup (mkVecZ elTy) nodesI)
    edge t@(Product _ tys) = let ti = HashMap.lookupDefault (error $ $(curLoc) ++ "Product") t nodesI
                             in mapMaybe (\ty -> liftM (ti,) (HashMap.lookup (mkVecZ ty) nodesI)) tys
    edge _                 = []

normaliseType :: HWType -> VHDLM HWType
normaliseType (Annotated _ ty) = normaliseType ty
normaliseType (Vector n ty)    = Vector n <$> (normaliseType ty)
normaliseType (RTree d ty)     = RTree d <$> (normaliseType ty)
normaliseType (Product nm tys) = Product nm <$> (mapM normaliseType tys)
normaliseType ty@(SP _ elTys)      = do
  Mon $ mapM_ ((tyCache %=) . HashSet.insert) (concatMap snd elTys)
  return (BitVector (typeSize ty))
normaliseType (CustomSP _ _dataRepr size elTys) = do
  Mon $ mapM_ ((tyCache %=) . HashSet.insert) [ty | (_, _, subTys) <- elTys, ty <- subTys]
  return (BitVector size)
normaliseType ty@(Index _) = return (Unsigned (typeSize ty))
normaliseType ty@(Sum _ _) = return (BitVector (typeSize ty))
normaliseType ty@(CustomSum _ _ _ _) = return (BitVector (typeSize ty))
normaliseType (Clock _ _ Gated) =
  return (Product "GatedClock" [Bit,Bool])
normaliseType (Clock {}) = return Bit
normaliseType (Reset {}) = return Bit
normaliseType (BiDirectional _ ty) = normaliseType ty
normaliseType ty = return ty

mkVecZ :: HWType -> HWType
mkVecZ (Vector _ elTy) = Vector 0 elTy
mkVecZ (RTree _ elTy)  = RTree 0 elTy
mkVecZ t               = t

tyDec :: HWType -> VHDLM Doc
tyDec (Vector _ elTy) = do
  syn <- Mon hdlSyn
  case syn of
    Vivado -> "type" <+> "array_of_" <> tyName elTy <+> "is array (integer range <>) of"
              <+> "std_logic_vector" <> parens (int (typeSize elTy - 1) <+> "downto 0") <> semi
    _ -> "type" <+> "array_of_" <> tyName elTy <+> "is array (integer range <>) of"
         <+> vhdlType elTy <> semi

tyDec (RTree _ elTy) = do
  syn <- Mon hdlSyn
  case syn of
    Vivado -> "type" <+> "tree_of_" <> tyName elTy <+> "is array (integer range <>) of"
              <+> "std_logic_vector" <> parens (int (typeSize elTy - 1) <+> "downto 0") <> semi
    _ ->  "type" <+> "tree_of_" <> tyName elTy <+> "is array (integer range <>) of" <+> vhdlType elTy <> semi

tyDec ty@(Product _ tys@(_:_:_)) = prodDec
  where
    prodDec = "type" <+> tName <+> "is record" <> line <>
                indent 2 (vcat $ zipWithM (\x y -> x <+> colon <+> y <> semi) selNames selTys) <> line <>
              "end record" <> semi

    tName    = tyName ty
    selNames = map (\i -> tName <> "_sel" <> int i) [0..]
    selTys   = map vhdlType tys

tyDec _ = emptyDoc


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

funDec _ Bit = Just
  ( "function" <+> "toSLV" <+> parens ("sl" <+> colon <+> "in" <+> "std_logic") <+> "return" <+> "std_logic_vector" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "std_logic" <> semi
  , "function" <+> "toSLV" <+> parens ("sl" <+> colon <+> "in" <+> "std_logic") <+> "return" <+> "std_logic_vector" <+> "is" <> line <>
    "begin" <> line <>
      indent 2 ("return" <+> "std_logic_vector'" <> parens (int 0 <+> rarrow <+> "sl") <> semi) <> line <>
    "end" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "std_logic" <+> "is" <> line <>
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
    "begin" <> line <>
      indent 2 ("return" <+> "signed" <> parens ("slv") <> semi) <> line <>
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
    "begin" <> line <>
      indent 2 ("return" <+> "unsigned" <> parens ("slv") <> semi) <> line <>
    "end" <> semi

  )

funDec _ t@(Product _ elTys) = Just
  ( "function" <+> "toSLV" <+> parens ("p :" <+> vhdlType t) <+> "return std_logic_vector" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> vhdlType t <> semi
  , "function" <+> "toSLV" <+> parens ("p :" <+> vhdlType t) <+> "return std_logic_vector" <+> "is" <> line <>
    "begin" <> line <>
    indent 2 ("return" <+> parens (hcat (punctuate " & " elTyToSLV)) <> semi) <> line <>
    "end" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> vhdlType t <+> "is" <> line <>
      "alias islv : std_logic_vector(0 to slv'length - 1) is slv;" <> line <>
    "begin" <> line <>
    indent 2 ("return" <+> parens (hcat (punctuate "," elTyFromSLV)) <> semi) <> line <>
    "end" <> semi
  )
  where
    elTyToSLV = forM [0..(length elTys - 1)]
                     (\i -> "toSLV" <>
                            parens ("p." <> tyName t <> "_sel" <> int i))

    argLengths = map typeSize elTys
    starts     = 0 : snd (mapAccumL ((join (,) .) . (+)) 0 argLengths)
    ends       = map (subtract 1) (tail starts)

    elTyFromSLV = forM (zip starts ends)
                       (\(s,e) -> "fromSLV" <>
                          parens ("islv" <> parens (int s <+> "to" <+> int e)))

funDec syn t@(Vector _ elTy) = Just
  ( "function" <+> "toSLV" <+> parens ("value : " <+> vhdlTypeMark t) <+> "return std_logic_vector" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> vhdlTypeMark t <> semi
  , "function" <+> "toSLV" <+> parens ("value : " <+> vhdlTypeMark t) <+> "return std_logic_vector" <+> "is" <> line <>
      indent 2
        ( "alias ivalue    :" <+> vhdlTypeMark t <> "(1 to value'length) is value;" <> line <>
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
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> vhdlTypeMark t <+> "is" <> line <>
      indent 2
        ( "alias islv      :" <+> "std_logic_vector" <> "(0 to slv'length - 1) is slv;" <> line <>
          "variable result :" <+> vhdlTypeMark t <> parens ("0 to slv'length / " <> eSz <+> "- 1") <> semi
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
  ( "function" <+> "toSLV" <+> parens ("value : " <+> vhdlTypeMark t) <+> "return std_logic_vector" <> semi <> line <>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> vhdlTypeMark t <> semi
  , "function" <+> "toSLV" <+> parens ("value : " <+> vhdlTypeMark t) <+> "return std_logic_vector" <+> "is" <> line <>
      indent 2
        ( "alias ivalue    :" <+> vhdlTypeMark t <> "(1 to value'length) is value;" <> line <>
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
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> vhdlTypeMark t <+> "is" <> line <>
      indent 2
        ( "alias islv      :" <+> "std_logic_vector" <> "(0 to slv'length - 1) is slv;" <> line <>
          "variable result :" <+> vhdlTypeMark t <> parens ("0 to slv'length / " <> eSz <+> "- 1") <> semi
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
     -> VHDLM (Doc, t)
port elName hwType portDirection fillToN =
  (,fromIntegral $ TextS.length elName) <$> (encodingNote hwType <> fill fillToN (pretty elName) <+> colon <+> direction <+> vhdlType hwType)
 where
  direction | isBiSignalIn hwType = "inout"
            | otherwise           = portDirection


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
          _     -> indent 2 (rports p) <> "end" <> semi
      )
  where
    ports l = sequence $ [port iName hwType "in" l | (iName, hwType) <- inputs c]
                      ++ [port oName hwType "out" l | (_, (oName, hwType)) <- outputs c]

    rports p = "port" <> (parens (align (vcat (punctuate semi (pure p))))) <> semi

    rattrs      = renderAttrs attrs
    attrs       = inputAttrs ++ outputAttrs
    inputAttrs  = [(id_, attr) | (id_, hwtype) <- inputs c, attr <- hwTypeAttrs hwtype]
    outputAttrs = [(id_, attr) | (_wireOrReg, (id_, hwtype)) <- outputs c, attr <- hwTypeAttrs hwtype]


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
   declAttrs   = [(id_, attr) | NetDecl' _ _ id_ (Right hwtype) <- netdecls, attr <- hwTypeAttrs hwtype]
   inputAttrs  = [(id_, attr) | (id_, hwtype) <- inputs c, attr <- hwTypeAttrs hwtype]
   outputAttrs = [(id_, attr) | (_wireOrReg, (id_, hwtype)) <- outputs c, attr <- hwTypeAttrs hwtype]

   isNetDecl :: Declaration -> Bool
   isNetDecl (NetDecl' _ _ _ (Right _)) = True
   isNetDecl _                          = False

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
  renderAttrDecl attrname (signalName, value) = "attribute"
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
renderAttr (StringAttr'  _key value) = T.pack $ show value
renderAttr (IntegerAttr' _key value) = T.pack $ show value
renderAttr (BoolAttr'    _key True ) = T.pack $ "true"
renderAttr (BoolAttr'    _key False) = T.pack $ "false"
renderAttr (Attr'        _key      ) = T.pack $ "true"

-- | Convert a Netlist HWType to a VHDL type
vhdlType :: HWType -> VHDLM Doc
vhdlType hwty = do
    hwty' <- normaliseType hwty
    Mon (tyCache %= HashSet.insert hwty')
    go hwty'
  where
    go :: HWType -> VHDLM Doc
    go Bool            = "boolean"
    go Bit             = "std_logic"
    go (Clock {})      = "std_logic"
    go (Reset {})      = "std_logic"
    go (BitVector n)   = case n of
                           0 -> "std_logic_vector (0 downto 1)"
                           _ -> "std_logic_vector" <> parens (int (n-1) <+> "downto 0")
    go (Signed n)      = case n of
                           0 -> "signed (0 downto 1)"
                           _ -> "signed" <> parens (int (n-1) <+> "downto 0")
    go (Unsigned n)    = case n of
                           0 -> "unsigned (0 downto 1)"
                           _ -> "unsigned" <> parens ( int (n-1) <+> "downto 0")
    go (Vector n elTy) = do
      nm <- Mon $ use modNm
      pretty (TextS.toLower nm) <> "_types.array_of_" <> tyName elTy <> parens ("0 to " <> int (n-1))
    go (RTree d elTy)  = do
      nm <- Mon $ use modNm
      pretty (TextS.toLower nm) <> "_types.tree_of_" <> tyName elTy <> parens ("0 to " <> int ((2^d)-1))
    go t@(Product _ _) = do
      nm <- Mon $ use modNm
      pretty (TextS.toLower nm) <> "_types." <> tyName t
    go (Void {})       = "std_logic_vector (0 downto 1)"
    go String          = "string"
    go ty              = error $ $(curLoc) ++ "vhdlType: type is not normalised: " ++ show ty

sigDecl :: VHDLM Doc -> HWType -> VHDLM Doc
sigDecl d t = d <+> colon <+> vhdlType t

-- | Convert a Netlist HWType to the root of a VHDL type
vhdlTypeMark :: HWType -> VHDLM Doc
vhdlTypeMark hwty = do
  hwty' <- normaliseType hwty
  Mon (tyCache %= HashSet.insert hwty')
  go hwty'
  where
    go Bool            = "boolean"
    go Bit             = "std_logic"
    go (Clock {})      = "std_logic"
    go (Reset {})      = "std_logic"
    go (BitVector _)   = "std_logic_vector"
    go (Signed _)      = "signed"
    go (Unsigned _)    = "unsigned"
    go (Vector _ elTy) = do
      nm <- Mon $ use modNm
      pretty (TextS.toLower nm) <> "_types.array_of_" <> tyName elTy
    go (RTree _ elTy)  = do
      nm <- Mon $ use modNm
      pretty (TextS.toLower nm) <> "_types.tree_of_" <> tyName elTy
    go t@(Product _ _) = do
      nm <- Mon $ use modNm
      pretty (TextS.toLower nm) <> "_types." <> tyName t
    go t               = error $ $(curLoc) ++ "vhdlTypeMark: " ++ show t

tyName :: HWType -> VHDLM Doc
tyName Bool              = "boolean"
tyName Bit               = "std_logic"
tyName (Clock {})        = "std_logic"
tyName (Reset {})        = "std_logic"
tyName (Vector n elTy)   = "array_of_" <> int n <> "_" <> tyName elTy
tyName (RTree n elTy)    = "tree_of_" <> int n <> "_" <> tyName elTy
tyName (BitVector n)     = "std_logic_vector_" <> int n
tyName t@(Index _)       = "unsigned_" <> int (typeSize t)
tyName (Signed n)        = "signed_" <> int n
tyName (Unsigned n)      = "unsigned_" <> int n
tyName t@(Sum _ _)       = "std_logic_vector_" <> int (typeSize t)
tyName t@(Product nm _)  = do
    tN <- normaliseType t
    Mon $ makeCached tN nameCache prodName
  where
    prodName = do
      tyCache %= HashSet.insert t
      seen <- use tySeen
      mkId <- mkIdentifier <*> pure Basic
      let nm'  = (mkId . last . TextS.splitOn ".") nm
          nm'' = if TextS.null nm'
                    then "product"
                    else nm'
          nm3  = if nm'' `elem` seen
                    then go mkId seen (0::Integer) nm''
                    else nm''
      tySeen %= (nm3:)
      pretty nm3

    go mkId s i n =
      let n' = n `TextS.append` TextS.pack ('_':show i)
      in  if n' `elem` s
             then go mkId s (i+1) n
             else n'
tyName t@(SP _ _)        = "std_logic_vector_" <> int (typeSize t)
tyName e = error $ $(curLoc) ++ show e

-- | Convert a Netlist HWType to an error VHDL value for that type
vhdlTypeErrValue :: HWType -> VHDLM Doc
vhdlTypeErrValue Bool                = "true"
vhdlTypeErrValue Bit                 = "'-'"
vhdlTypeErrValue t@(Vector n elTy)   = do
  syn <-Mon hdlSyn
  case syn of
    Vivado -> vhdlTypeMark t <> "'" <> parens (int 0 <+> "to" <+> int (n-1) <+> rarrow <+>
                "std_logic_vector'" <> parens (int 0 <+> "to" <+> int (typeSize elTy - 1) <+>
                 rarrow <+> "'-'"))
    _ -> vhdlTypeMark t <> "'" <> parens (int 0 <+> "to" <+> int (n-1) <+> rarrow <+> vhdlTypeErrValue elTy)
vhdlTypeErrValue t@(RTree n elTy)    = do
  syn <-Mon hdlSyn
  case syn of
    Vivado -> vhdlTypeMark t <> "'" <>  parens (int 0 <+> "to" <+> int (2^n - 1) <+> rarrow <+>
                "std_logic_vector'" <> parens (int 0 <+> "to" <+> int (typeSize elTy - 1) <+>
                 rarrow <+> "'-'"))
    _ -> vhdlTypeMark t <> "'" <>  parens (int 0 <+> "to" <+> int (2^n - 1) <+> rarrow <+> vhdlTypeErrValue elTy)
vhdlTypeErrValue t@(Product _ elTys) = vhdlTypeMark t <> "'" <> tupled (mapM vhdlTypeErrValue elTys)
vhdlTypeErrValue (Reset {})          = "'-'"
vhdlTypeErrValue (Clock _ _ Source)  = "'-'"
vhdlTypeErrValue (Clock _ _ Gated)   = "('-',false)"
vhdlTypeErrValue (Void {})           = "std_logic_vector'(0 downto 1 => '-')"
vhdlTypeErrValue String              = "\"ERROR\""
vhdlTypeErrValue t                   = vhdlTypeMark t <> "'" <> parens (int 0 <+> "to" <+> int (typeSize t - 1) <+> rarrow <+> "'-'")

vhdlRecSel
  :: HWType
  -> Int
  -> VHDLM Doc
vhdlRecSel ty i = tyName ty <> "_sel" <> int i

decls :: [Declaration] -> VHDLM Doc
decls [] = emptyDoc
decls ds = do
    rec (dsDoc,ls) <- fmap (unzip . catMaybes) $ mapM (decl (maximum ls)) ds
    case dsDoc of
      [] -> emptyDoc
      _  -> punctuate' semi (pure dsDoc)

decl :: Int ->  Declaration -> VHDLM (Maybe (Doc,Int))
decl l (NetDecl' noteM _ id_ ty) = Just <$> (,fromIntegral (TextS.length id_)) <$>
  maybe id addNote noteM ("signal" <+> fill l (pretty id_) <+> colon <+> either pretty vhdlType ty)
  where
    addNote n = mappend ("--" <+> pretty n <> line)

decl _ (InstDecl Comp _ nm _ pms) = fmap (Just . (,0)) $ do
  { rec (p,ls) <- fmap unzip $ sequence [ (,formalLength i) <$> fill (maximum ls) (expr_ False i) <+> colon <+> portDir dir <+> vhdlType ty | (i,dir,ty,_) <- pms ]
  ; "component" <+> pretty nm <> line <>
      indent 2 ("port" <+> tupledSemi (pure p) <> semi) <> line <>
    "end component"
  }
 where
    formalLength (Identifier i _) = fromIntegral (TextS.length i)
    formalLength _                = 0

    portDir In  = "in"
    portDir Out = "out"

decl _ _ = return Nothing

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
insts is = vcat . punctuate line . fmap catMaybes $ mapM inst_ is

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

inst_ (InstDecl entOrComp libM nm lbl pms) = do
    maybe (return ()) (\lib -> Mon (libraries %= (T.fromStrict lib:))) libM
    fmap Just $
      nest 2 $ pretty lbl <+> colon <+> entOrComp'
                <+> maybe emptyDoc ((<> ".") . pretty) libM <> pretty nm <> line <> pms' <> semi
  where
    pms' = do
      rec (p,ls) <- fmap unzip $ sequence [ (,formalLength i) <$> fill (maximum ls) (expr_ False i) <+> "=>" <+> expr_ False e | (i,_,_,e) <- pms]
      nest 2 $ "port map" <> line <> tupled (pure p)
    formalLength (Identifier i _) = fromIntegral (TextS.length i)
    formalLength _                = 0
    entOrComp' = case entOrComp of { Entity ->"entity"; _ -> "component" }

inst_ (BlackBoxD _ libs imps inc bs bbCtx) =
  fmap Just (Mon (column (renderBlackBox libs imps inc bs bbCtx)))

inst_ _ = return Nothing

-- | Turn a Netlist expression into a VHDL expression
expr_ :: Bool -- ^ Enclose in parenthesis?
     -> Expr -- ^ Expr to convert
     -> VHDLM Doc
expr_ _ (Literal sizeM lit) = exprLit sizeM lit
expr_ _ (Identifier id_ Nothing) = pretty id_
expr_ _ (Identifier id_ (Just (Indexed (CustomSP _id _dataRepr _size args,dcI,fI)))) = do
  nm <- Mon $ use modNm
  let cast = vhdlTypeMark resultType <> squote
  let fSLV = stringS (TextS.toLower nm) <> "_types.fromSLV"
  cast <> parens (fSLV <> parens (hcat $ punctuate " & " $ ranges))
    where
      resultType = fieldTypes !! fI
      (ConstrRepr' _name _n _mask _value anns, _, fieldTypes) = args !! dcI

      ranges =
        mapM range $ bitRanges (anns !! fI)

      range (start, end) =
        pretty id_ <> parens (int start <+> "downto" <+> int end)

expr_ _ (Identifier id_ (Just (Indexed (ty@(SP _ args),dcI,fI)))) =
  fromSLV argTy id_ start end
    where
      argTys   = snd $ args !! dcI
      argTy    = argTys !! fI
      argSize  = typeSize argTy
      other    = otherSize argTys (fI-1)
      start    = typeSize ty - 1 - conSize ty - other
      end      = start - argSize + 1

expr_ _ (Identifier id_ (Just (Indexed (ty@(Product _ _),_,fI)))) =
  pretty id_ <> dot <> tyName ty <> "_sel" <> int fI

expr_ _ (Identifier id_ (Just (Indexed (ty@(Clock _ _ Gated),_,fI)))) = do
  ty' <- normaliseType ty
  pretty id_ <> dot <> tyName ty' <> "_sel" <> int fI

expr_ _ (Identifier id_ (Just (Indexed ((Vector _ elTy),1,0)))) = do
  syn <- Mon hdlSyn
  case syn of
    Vivado -> do
      id' <- fmap (T.toStrict . renderOneLine) (pretty id_ <> parens (int 0))
      fromSLV elTy id' (typeSize elTy - 1) 0
    _ -> pretty id_ <> parens (int 0)
expr_ _ (Identifier id_ (Just (Indexed ((Vector n _),1,1)))) = pretty id_ <> parens (int 1 <+> "to" <+> int (n-1))

-- This is a "Hack", we cannot construct trees with a negative depth. This is
-- here so that we can recognise merged RTree modifiers. See the code in
-- @Clash.Backend.nestM@ which construct these tree modifiers.
expr_ _ (Identifier id_ (Just (Indexed (RTree (-1) _,l,r)))) =
  pretty id_ <> parens (int l <+> "to" <+> int (r-1))

expr_ _ (Identifier id_ (Just (Indexed ((RTree 0 elTy),0,0)))) = do
  syn <- Mon hdlSyn
  case syn of
    Vivado -> do
      id' <- fmap (T.toStrict . renderOneLine) (pretty id_ <> parens (int 0))
      fromSLV elTy id' (typeSize elTy - 1) 0
    _ -> pretty id_ <> parens (int 0)
expr_ _ (Identifier id_ (Just (Indexed ((RTree n _),1,0)))) =
  let z = 2^(n-1)
  in  pretty id_ <> parens (int 0 <+> "to" <+> int (z-1))
expr_ _ (Identifier id_ (Just (Indexed ((RTree n _),1,1)))) =
  let z  = 2^(n-1)
      z' = 2^n
  in  pretty id_ <> parens (int z <+> "to" <+> int (z'-1))

-- This is a HACK for Clash.Driver.TopWrapper.mkOutput
-- Vector's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
expr_ _ (Identifier id_ (Just (Indexed ((Vector _ elTy),10,fI)))) = do
  syn <- Mon hdlSyn
  case syn of
    Vivado -> do
      id' <- fmap (T.toStrict . renderOneLine) (pretty id_ <> parens (int fI))
      fromSLV elTy id' (typeSize elTy - 1) 0
    _ -> pretty id_ <> parens (int fI)

-- This is a HACK for Clash.Driver.TopWrapper.mkOutput
-- RTree's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
expr_ _ (Identifier id_ (Just (Indexed ((RTree _ elTy),10,fI)))) = do
  syn <- Mon hdlSyn
  case syn of
    Vivado -> do
      id' <- fmap (T.toStrict . renderOneLine) (pretty id_ <> parens (int fI))
      fromSLV elTy id' (typeSize elTy - 1) 0
    _ -> pretty id_ <> parens (int fI)

expr_ _ (Identifier id_ (Just (DC (ty@(SP _ _),_)))) = pretty id_ <> parens (int start <+> "downto" <+> int end)
  where
    start = typeSize ty - 1
    end   = typeSize ty - conSize ty

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
expr_ _ (Identifier id_ (Just (Indexed ((Signed w),_,_))))  = do
  iw <- Mon $ use intWidth
  traceIf (iw < w) ($(curLoc) ++ "WARNING: result smaller than argument") $
    "resize" <> parens (pretty id_ <> "," <> int iw)
expr_ _ (Identifier id_ (Just (Indexed ((Unsigned w),_,_)))) = do
  iw <- Mon $ use intWidth
  traceIf (iw < w) ($(curLoc) ++ "WARNING: result smaller than argument") $
    "resize" <> parens (pretty id_ <> "," <> int iw)

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
expr_ _ (Identifier _ (Just (Indexed ((BitVector _),_,0)))) = do
  iw <- Mon $ use intWidth
  traceIf True ($(curLoc) ++ "WARNING: synthesizing bitvector mask to dontcare") $
    vhdlTypeErrValue (Signed iw)

-- [Note] bitvector projection
--
-- This covers the case of either:
--
-- `Clash.Sized.Internal.BitVector.unsafeToInteger` or
--
-- > :: BitVector 8 -> Integer
-- > \case BV wild i -> i
--
-- introduced by the
expr_ _ (Identifier id_ (Just (Indexed ((BitVector w),_,1)))) = do
  iw <- Mon $ use intWidth
  traceIf (iw < w) ($(curLoc) ++ "WARNING: result smaller than argument") $
    "signed" <> parens ("std_logic_vector" <> parens ("resize" <>
      parens ("unsigned" <> parens (pretty id_) <> "," <> int iw)))

expr_ b (Identifier id_ (Just (Nested m1 m2))) = case nestM m1 m2 of
  Just m3 -> expr_ b (Identifier id_ (Just m3))
  _ -> do
    k <- expr_ b (Identifier id_ (Just m1))
    expr_ b (Identifier (T.toStrict $ renderOneLine k) (Just m2))

expr_ _ (Identifier id_ (Just _)) = pretty id_

expr_ b (DataCon _ (DC (Void {}, -1)) [e]) =  expr_ b e

expr_ _ (DataCon ty@(Vector 0 _) _ _) = vhdlTypeErrValue ty

expr_ _ (DataCon ty@(Vector 1 elTy) _ [e])       = do
  syn <- Mon hdlSyn
  case syn of
    Vivado -> vhdlTypeMark ty <> "'" <> parens (int 0 <+> rarrow <+> toSLV elTy e)
    _ -> vhdlTypeMark ty <> "'" <> parens (int 0 <+> rarrow <+> expr_ False e)
expr_ _ e@(DataCon ty@(Vector _ elTy) _ [e1,e2]) = do
  syn <- Mon hdlSyn
  case syn of
    Vivado -> vhdlTypeMark ty <> "'" <> case vectorChain e of
      Just es -> align (tupled (mapM (toSLV elTy) es))
      Nothing -> parens ("std_logic_vector'" <> parens (toSLV elTy e1) <+> "&" <+> expr_ False e2)
    _ -> vhdlTypeMark ty <> "'" <> case vectorChain e of
            Just es -> align (tupled (mapM (expr_ False) es))
            Nothing -> parens (vhdlTypeMark elTy <> "'" <> parens (expr_ False e1) <+> "&" <+> expr_ False e2)

expr_ _ (DataCon ty@(RTree 0 elTy) _ [e]) = do
  syn <- Mon hdlSyn
  case syn of
    Vivado -> vhdlTypeMark ty <> "'" <> parens (int 0 <+> rarrow <+> toSLV elTy e)
    _ -> vhdlTypeMark ty <> "'" <> parens (int 0 <+> rarrow <+> expr_ False e)
expr_ _ e@(DataCon ty@(RTree d elTy) _ [e1,e2]) = vhdlTypeMark ty <> "'" <> case rtreeChain e of
  Just es -> tupled (mapM (expr_ False) es)
  Nothing -> parens (vhdlTypeMark (RTree (d-1) elTy) <> "'" <> parens (expr_ False e1) <+>
                     "&" <+> expr_ False e2)

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
expr_ _ (DataCon (CustomSP _ dataRepr size args) (DC (_,i)) es) =
  "std_logic_vector'" <> parens (hcat $ punctuate " & " $ mapM range origins)
    where
      (cRepr, _, argTys) = args !! i

      -- Build bit representations for all constructor arguments
      argExprs = zipWith toSLV argTys es :: [VHDLM Doc]

      -- Spread bits of constructor arguments using masks
      origins = bitOrigins dataRepr cRepr :: [BitOrigin]

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
        let expr' = argExprs !! n in

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

expr_ _ (DataCon ty@(Product _ _) _ es) =
    tupled $ zipWithM (\i e' -> tyName ty <> "_sel" <> int i <+> rarrow <+> expr_ False e') [0..] es

expr_ _ (DataCon ty@(Clock _ _ Gated) _ es) = do
    ty' <- normaliseType ty
    tupled $ zipWithM (\i e' -> tyName ty' <> "_sel" <> int i <+> rarrow <+> expr_ False e') [0..] es

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

expr_ _ (DataTag (Product _ _) (Right _))  = do
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
               parens (vhdlTypeMark hwty <> "'" <> parens (expr_ False e))
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
bit_char U = char '-'
bit_char Z = char 'Z'

toSLV :: HWType -> Expr -> VHDLM Doc
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
toSLV t@(Product _ tys) (Identifier id_ Nothing) = do
    selIds' <- sequence selIds
    encloseSep lparen rparen " & " (zipWithM toSLV tys selIds')
  where
    tName    = tyName t
    selNames = map (fmap (T.toStrict . renderOneLine) ) [pretty id_ <> dot <> tName <> "_sel" <> int i | i <- [0..(length tys)-1]]
    selIds   = map (fmap (\n -> Identifier n Nothing)) selNames
toSLV (Product _ tys) (DataCon _ _ es) = do
  encloseSep lparen rparen " & " (zipWithM toSLV tys es)
toSLV (Product _ _) e = do
  nm <- Mon $ use modNm
  pretty (TextS.toLower nm) <> "_types.toSLV" <> parens (expr_ False e)
toSLV (SP _ _) e       = expr_ False e
toSLV (CustomSP _ _ _ _) e = expr_ False e
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
toSLV (Vector n elTy) (DataCon _ _ es) =
  "std_logic_vector'" <> (parens $ vcat $ punctuate " & " (zipWithM toSLV [elTy,Vector (n-1) elTy] es))
toSLV (Vector _ _) e = do
  nm <- Mon $ use modNm
  pretty (TextS.toLower nm) <> "_types.toSLV" <> parens (expr_ False e)
toSLV hty e = error $ $(curLoc) ++  "toSLV:\n\nType: " ++ show hty ++ "\n\nExpression: " ++ show e

fromSLV :: HasCallStack => HWType -> Identifier -> Int -> Int -> VHDLM Doc
fromSLV Bool              id_ start _   = do
  nm <- Mon $ use modNm
  pretty (TextS.toLower nm) <> "_types.fromSLV" <> parens (pretty id_ <> parens (int start <+> "downto" <+> int start))
fromSLV Bit                 id_ start _   = pretty id_ <> parens (int start)
fromSLV (BitVector _)       id_ start end = pretty id_ <> parens (int start <+> "downto" <+> int end)
fromSLV (Index _)           id_ start end = "unsigned" <> parens (pretty id_ <> parens (int start <+> "downto" <+> int end))
fromSLV (Signed _)          id_ start end = "signed" <> parens (pretty id_ <> parens (int start <+> "downto" <+> int end))
fromSLV (Unsigned _)        id_ start end = "unsigned" <> parens (pretty id_ <> parens (int start <+> "downto" <+> int end))
fromSLV (Sum _ _)           id_ start end = pretty id_ <> parens (int start <+> "downto" <+> int end)
fromSLV (CustomSum _ _ _ _) id_ start end = pretty id_ <> parens (int start <+> "downto" <+> int end)
fromSLV t@(Product _ tys) id_ start _ = do
    tupled $ zipWithM (\s e -> s <+> rarrow <+> e) selNames args
  where
    tName      = tyName t
    selNames   = [tName <> "_sel" <> int i | i <- [0..]]
    argLengths = map typeSize tys
    starts     = start : snd (mapAccumL ((join (,) .) . (-)) start argLengths)
    ends       = map (+1) (tail starts)
    args       = zipWith3 (`fromSLV` id_) tys starts ends

fromSLV (CustomSP _ _ _ _)  id_ start end = pretty id_ <> parens (int start <+> "downto" <+> int end)
fromSLV (SP _ _)            id_ start end = pretty id_ <> parens (int start <+> "downto" <+> int end)
fromSLV (Vector n elTy)     id_ start _   =
    if n > 1 then tupled args
             else parens (int 0 <+> rarrow <+> fmap head args)
  where
    argLength = typeSize elTy
    starts    = take (n + 1) $ iterate (subtract argLength) start
    ends      = map (+1) (tail starts)
    args      = do syn <- Mon hdlSyn
                   let elTy' = case syn of
                                 Vivado -> BitVector (argLength - 1)
                                 _ -> elTy
                   zipWithM (fromSLV elTy' id_) starts ends
fromSLV (Clock {})        id_ start _   = pretty id_ <> parens (int start)
fromSLV (Reset {})        id_ start _   = pretty id_ <> parens (int start)
fromSLV hty               _   _     _   = error $ $(curLoc) ++ "fromSLV: " ++ show hty

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
encodingNote (Clock _ _ Gated) = "-- gated clock" <> line
encodingNote (Clock {})        = "-- clock" <> line
encodingNote (Reset {})        = "-- asynchronous reset: active high" <> line
encodingNote _                 = emptyDoc

tupledSemi :: Applicative f => f [Doc] -> f Doc
tupledSemi = align . encloseSep (flatAlt (lparen <+> emptyDoc) lparen)
                                (flatAlt (emptyDoc <+> rparen) rparen)
                                (semi <+> emptyDoc)
