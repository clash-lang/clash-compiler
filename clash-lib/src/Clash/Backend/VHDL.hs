{-|
  Copyright   :  (C) 2015-2016, University of Twente,
                          2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Generate VHDL for assorted Netlist datatypes
-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Backend.VHDL (VHDLState) where

import qualified Control.Applicative                  as A
import           Control.Lens                         hiding (Indexed)
import           Control.Monad                        (forM,join,liftM,zipWithM)
import           Control.Monad.State                  (State)
import           Data.Graph.Inductive                 (Gr, mkGraph, topsort')
import           Data.Hashable                        (Hashable (..))
import           Data.HashMap.Lazy                    (HashMap)
import qualified Data.HashMap.Lazy                    as HashMap
import           Data.HashSet                         (HashSet)
import qualified Data.HashSet                         as HashSet
import           Data.List                            (mapAccumL,nub,nubBy)
import           Data.Maybe                           (catMaybes,fromMaybe,mapMaybe)
import           Data.Text.Lazy                       (unpack)
import qualified Data.Text.Lazy                       as T
import           Prelude                              hiding ((<$>))
import qualified System.FilePath
import           Text.Printf
import           Text.PrettyPrint.Leijen.Text         (isEmpty)
import           Text.PrettyPrint.Leijen.Text.Monadic

import           Clash.Annotations.Primitive          (HDL (..))
import           Clash.Backend
import           Clash.Driver.Types                   (SrcSpan, noSrcSpan)
import           Clash.Netlist.BlackBox.Types         (HdlSyn (..))
import           Clash.Netlist.BlackBox.Util          (extractLiterals, renderBlackBox)
import           Clash.Netlist.Id                     (IdType (..), mkBasicId')
import           Clash.Netlist.Types                  hiding (_intWidth, intWidth)
import           Clash.Netlist.Util                   hiding (mkIdentifier)
import           Clash.Signal.Internal                (ClockKind (..))
import           Clash.Util                           (clogBase, curLoc, first, makeCached, on, (<:>))

#ifdef CABAL
import qualified Paths_clash_lib
#endif

-- | State for the 'Clash.Netlist.VHDL.VHDLM' monad:
data VHDLState =
  VHDLState
  { _tyCache   :: (HashSet HWType)     -- ^ Previously encountered HWTypes
  , _tySeen    :: [Identifier]         -- ^ Generated product types
  , _nameCache :: (HashMap HWType Doc) -- ^ Cache for previously generated product type names
  , _modNm     :: String
  , _srcSpan   :: SrcSpan
  , _libraries :: [T.Text]
  , _packages  :: [T.Text]
  , _includes  :: [(String,Doc)]
  , _intWidth  :: Int                  -- ^ Int/Word/Integer bit-width
  , _hdlsyn    :: HdlSyn               -- ^ For which HDL synthesis tool are we generating VHDL
  }

makeLenses ''VHDLState

instance Backend VHDLState where
  initBackend     = VHDLState HashSet.empty [] HashMap.empty "" noSrcSpan [] [] []
  hdlKind         = const VHDL
#ifdef CABAL
  primDir         = const (Paths_clash_lib.getDataFileName ("prims" System.FilePath.</> "vhdl"))
#else
  primDir _       = return ("clash-lib" System.FilePath.</> "prims" System.FilePath.</> "vhdl")
#endif
  extractTypes    = _tyCache
  name            = const "vhdl"
  extension       = const ".vhdl"

  genHDL          = genVHDL
  mkTyPackage     = mkTyPackage_
  hdlType Internal      ty = vhdlType ty
  hdlType (External nm) ty = case ty of
    Vector _ _  -> text nm <> dot <> vhdlType ty
    RTree _ _   -> text nm <> dot <> vhdlType ty
    Product _ _ -> text nm <> dot <> vhdlType ty
    _           -> vhdlType ty
  hdlTypeErrValue = vhdlTypeErrValue
  hdlTypeMark     = vhdlTypeMark
  hdlSig t ty     = sigDecl (text t) ty
  genStmt         = const empty
  inst            = inst_
  expr            = expr_
  iwWidth         = use intWidth
  toBV _ id_      = do
    nm <- use modNm
    text (T.toLower $ T.pack nm) <> "_types.toSLV" <> parens (text id_)
  fromBV hty id_  = fromSLV hty id_ (typeSize hty - 1) 0
  hdlSyn          = use hdlsyn
  mkIdentifier    = return go
    where
      go Basic    nm = filterReserved (T.toLower (mkBasicId' True nm))
      go Extended (rmSlash -> nm) = case go Basic nm of
        nm' | nm /= nm' -> T.concat ["\\",nm,"\\"]
            |otherwise  -> nm'
  extendIdentifier = return go
    where
      go Basic nm ext = filterReserved (T.toLower (mkBasicId' True (nm `T.append` ext)))
      go Extended ((rmSlash . escapeTemplate) -> nm) ext =
        let nmExt = nm `T.append` ext
        in  case go Basic nm ext of
              nm' | nm' /= nmExt -> case T.head nmExt of
                      '#' -> T.concat ["\\",nmExt,"\\"]
                      _   -> T.concat ["\\#",nmExt,"\\"]
                  | otherwise    -> nm'

  setModName nm s = s {_modNm = nm}
  setSrcSpan      = (srcSpan .=)
  getSrcSpan      = use srcSpan
  blockDecl nm ds = do
    decs   <- decls ds
    if isEmpty decs
       then insts ds
       else nest 2
              (text nm <+> colon <+> "block" <$$>
               pure decs) <$$>
            nest 2
              ("begin" <$$>
                insts ds) <$$>
            "end block" <> semi
  unextend = return rmSlash

rmSlash :: Identifier -> Identifier
rmSlash nm = fromMaybe nm $ do
  nm1 <- T.stripPrefix "\\" nm
  pure (T.filter (not . (== '\\')) nm1)

type VHDLM a = State VHDLState a

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
  then s `T.append` "_r"
  else s

-- | Generate VHDL for a Netlist component
genVHDL :: String -> SrcSpan -> Component -> VHDLM ((String,Doc),[(String,Doc)])
genVHDL nm sp c = do
    setSrcSpan sp
    v <- vhdl
    i <- use includes
    libraries .= []
    packages  .= []
    return ((unpack cName,v),i)
  where
    cName   = componentName c
    vhdl    = do
      ent  <- entity c
      arch <- architecture c
      imps <- tyImports nm
      ("-- Automatically generated VHDL-93" <$$>
       pure imps <$$> linebreak <>
       pure ent <$$> linebreak <>
       pure arch)

-- | Generate a VHDL package containing type definitions for the given HWTypes
mkTyPackage_ :: String
             -> [HWType]
             -> VHDLM [(String,Doc)]
mkTyPackage_ modName hwtys = do
    { syn <- hdlSyn
    ; mkId <- mkIdentifier <*> pure Basic
    ; let usedTys     = concatMap mkUsedTys hwtys
    ; normTys <- nub A.<$> mapM (fmap mkVecZ . normaliseType) (hwtys ++ usedTys)
    ; let sortedTys   = topSortHWTys normTys
          packageDec  = vcat $ mapM tyDec sortedTys
          (funDecs,funBodies) = unzip . mapMaybe (funDec syn) $ nubBy eqTypM sortedTys

    ; (:[]) A.<$> (unpack $ mkId (T.pack modName `T.append` "_types"),) A.<$>
      "library IEEE;" <$>
      "use IEEE.STD_LOGIC_1164.ALL;" <$>
      "use IEEE.NUMERIC_STD.ALL;" <$$> linebreak <>
      "package" <+> text (mkId (T.pack modName `T.append` "_types")) <+> "is" <$>
         indent 2 ( packageDec <$>
                    vcat (sequence funDecs)
                  ) <$>
      "end" <> semi <> packageBodyDec funBodies
    }
  where
    packageBodyDec :: [VHDLM Doc] -> VHDLM Doc
    packageBodyDec funBodies = case funBodies of
      [] -> empty
      _  -> do
        { mkId <- mkIdentifier <*> pure Basic
        ; linebreak <$>
         "package" <+> "body" <+> text (mkId (T.pack modName `T.append` "_types")) <+> "is" <$>
           indent 2 (vcat (sequence funBodies)) <$>
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
    edge t@(RTree _ elTy)  = maybe [] ((:[]) . (HashMap.lookupDefault (error $ $(curLoc) ++ "RTree") t nodesI,,()))
                                      (HashMap.lookup (mkVecZ elTy) nodesI)
    edge t@(Product _ tys) = let ti = HashMap.lookupDefault (error $ $(curLoc) ++ "Product") t nodesI
                             in mapMaybe (\ty -> liftM (ti,,()) (HashMap.lookup (mkVecZ ty) nodesI)) tys
    edge _                 = []

normaliseType :: HWType -> VHDLM HWType
normaliseType (Vector n ty)    = Vector n A.<$> (normaliseType ty)
normaliseType (RTree d ty)     = RTree d A.<$> (normaliseType ty)
normaliseType (Product nm tys) = Product nm A.<$> (mapM normaliseType tys)
normaliseType ty@(SP _ elTys)      = do
  mapM_ ((tyCache %=) . HashSet.insert) (concatMap snd elTys)
  return (BitVector (typeSize ty))
normaliseType ty@(Index _)     = return (Unsigned (typeSize ty))
normaliseType ty@(Sum _ _)     = return (Unsigned (typeSize ty))
normaliseType (Clock nm rt Gated) =
  return (Product "GatedClock" [Clock nm rt Source,Bool])
normaliseType ty = return ty

mkVecZ :: HWType -> HWType
mkVecZ (Vector _ elTy) = Vector 0 elTy
mkVecZ (RTree _ elTy)  = RTree 0 elTy
mkVecZ t               = t

tyDec :: HWType -> VHDLM Doc
tyDec (Vector _ elTy) = do
  syn <- hdlSyn
  case syn of
    Vivado -> "type" <+> "array_of_" <> tyName elTy <+> "is array (integer range <>) of"
              <+> "std_logic_vector" <> parens (int (typeSize elTy - 1) <+> "downto 0") <> semi
    _ -> "type" <+> "array_of_" <> tyName elTy <+> "is array (integer range <>) of"
         <+> vhdlType elTy <> semi

tyDec (RTree _ elTy) = do
  syn <- hdlSyn
  case syn of
    Vivado -> "type" <+> "tree_of_" <> tyName elTy <+> "is array (integer range <>) of"
              <+> "std_logic_vector" <> parens (int (typeSize elTy - 1) <+> "downto 0") <> semi
    _ ->  "type" <+> "tree_of_" <> tyName elTy <+> "is array (integer range <>) of" <+> vhdlType elTy <> semi

tyDec ty@(Product _ tys@(_:_:_)) = prodDec
  where
    prodDec = "type" <+> tName <+> "is record" <$>
                indent 2 (vcat $ zipWithM (\x y -> x <+> colon <+> y <> semi) selNames selTys) <$>
              "end record" <> semi

    tName    = tyName ty
    selNames = map (\i -> tName <> "_sel" <> int i) [0..]
    selTys   = map vhdlType tys

tyDec _ = empty


funDec :: HdlSyn -> HWType -> Maybe (VHDLM Doc,VHDLM Doc)
funDec _ Bool = Just
  ( "function" <+> "toSLV" <+> parens ("b" <+> colon <+> "in" <+> "boolean") <+> "return" <+> "std_logic_vector" <> semi <$>
    "function" <+> "fromSLV" <+> parens ("sl" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "boolean" <> semi <$>
    "function" <+> "tagToEnum" <+> parens ("s" <+> colon <+> "in" <+> "signed") <+> "return" <+> "boolean" <> semi <$>
    "function" <+> "dataToTag" <+> parens ("b" <+> colon <+> "in" <+> "boolean") <+> "return" <+> "signed" <> semi
  , "function" <+> "toSLV" <+> parens ("b" <+> colon <+> "in" <+> "boolean") <+> "return" <+> "std_logic_vector" <+> "is" <$>
    "begin" <$>
      indent 2 (vcat $ sequence ["if" <+> "b" <+> "then"
                                ,  indent 2 ("return" <+> dquotes (int 1) <> semi)
                                ,"else"
                                ,  indent 2 ("return" <+> dquotes (int 0) <> semi)
                                ,"end" <+> "if" <> semi
                                ]) <$>
    "end" <> semi <$>
    "function" <+> "fromSLV" <+> parens ("sl" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "boolean" <+> "is" <$>
    "begin" <$>
      indent 2 (vcat $ sequence ["if" <+> "sl" <+> "=" <+> dquotes (int 1) <+> "then"
                                ,   indent 2 ("return" <+> "true" <> semi)
                                ,"else"
                                ,   indent 2 ("return" <+> "false" <> semi)
                                ,"end" <+> "if" <> semi
                                ]) <$>
    "end" <> semi <$>
    "function" <+> "tagToEnum" <+> parens ("s" <+> colon <+> "in" <+> "signed") <+> "return" <+> "boolean" <+> "is" <$>
    "begin" <$>
      indent 2 (vcat $ sequence ["if" <+> "s" <+> "=" <+> "to_signed" <> parens (int 0 <> comma <> (use intWidth >>= int)) <+> "then"
                                ,   indent 2 ("return" <+> "false" <> semi)
                                ,"else"
                                ,   indent 2 ("return" <+> "true" <> semi)
                                ,"end" <+> "if" <> semi
                                ]) <$>
    "end" <> semi <$>
    "function" <+> "dataToTag" <+> parens ("b" <+> colon <+> "in" <+> "boolean") <+> "return" <+> "signed" <+> "is" <$>
    "begin" <$>
      indent 2 (vcat $ sequence ["if" <+> "b" <+> "then"
                                ,  indent 2 ("return" <+> "to_signed" <> parens (int 1 <> comma <> (use intWidth >>= int)) <> semi)
                                ,"else"
                                ,  indent 2 ("return" <+> "to_signed" <> parens (int 0 <> comma <> (use intWidth >>= int)) <> semi)
                                ,"end" <+> "if" <> semi
                                ]) <$>
    "end" <> semi
  )

funDec _ (Signed _) = Just
  ( "function" <+> "toSLV" <+> parens ("s" <+> colon <+> "in" <+> "signed") <+> "return" <+> "std_logic_vector" <> semi <$>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "signed" <> semi
  , "function" <+> "toSLV" <+> parens ("s" <+> colon <+> "in" <+> "signed") <+> "return" <+> "std_logic_vector" <+> "is" <$>
    "begin" <$>
      indent 2 ("return" <+> "std_logic_vector" <> parens ("s") <> semi) <$>
    "end" <> semi <$>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "signed" <+> "is" <$>
    "begin" <$>
      indent 2 ("return" <+> "signed" <> parens ("slv") <> semi) <$>
    "end" <> semi
  )

funDec _ (Unsigned _) = Just
  ( "function" <+> "toSLV" <+> parens ("u" <+> colon <+> "in" <+> "unsigned") <+> "return" <+> "std_logic_vector" <> semi <$>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "unsigned" <> semi
  , "function" <+> "toSLV" <+> parens ("u" <+> colon <+> "in" <+> "unsigned") <+> "return" <+> "std_logic_vector" <+> "is"  <$>
    "begin" <$>
      indent 2 ("return" <+> "std_logic_vector" <> parens ("u") <> semi) <$>
    "end" <> semi <$>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "unsigned" <+> "is"  <$>
    "begin" <$>
      indent 2 ("return" <+> "unsigned" <> parens ("slv") <> semi) <$>
    "end" <> semi

  )

funDec _ t@(Product _ elTys) = Just
  ( "function" <+> "toSLV" <+> parens ("p :" <+> vhdlType t) <+> "return std_logic_vector" <> semi <$>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> vhdlType t <> semi
  , "function" <+> "toSLV" <+> parens ("p :" <+> vhdlType t) <+> "return std_logic_vector" <+> "is" <$>
    "begin" <$>
    indent 2 ("return" <+> parens (hcat (punctuate " & " elTyToSLV)) <> semi) <$>
    "end" <> semi <$>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> vhdlType t <+> "is" <$>
      "alias islv : std_logic_vector(0 to slv'length - 1) is slv;" <$>
    "begin" <$>
    indent 2 ("return" <+> parens (hcat (punctuate "," elTyFromSLV)) <> semi) <$>
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
  ( "function" <+> "toSLV" <+> parens ("value : " <+> vhdlTypeMark t) <+> "return std_logic_vector" <> semi <$>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> vhdlTypeMark t <> semi
  , "function" <+> "toSLV" <+> parens ("value : " <+> vhdlTypeMark t) <+> "return std_logic_vector" <+> "is" <$>
      indent 2
        ( "alias ivalue    :" <+> vhdlTypeMark t <> "(1 to value'length) is value;" <$>
          "variable result :" <+> "std_logic_vector" <> parens ("1 to value'length * " <> int (typeSize elTy)) <> semi
        ) <$>
    "begin" <$>
      indent 2
        ("for i in ivalue'range loop" <$>
            indent 2
              (  "result" <> parens (parens ("(i - 1) * " <> int (typeSize elTy)) <+> "+ 1" <+>
                                             "to i*" <> int (typeSize elTy)) <+>
                          ":=" <+> (case syn of
                                      Vivado -> "ivalue" <> parens ("i")
                                      _  -> "toSLV" <> parens ("ivalue" <> parens ("i"))) <> semi
              ) <$>
         "end" <+> "loop" <> semi <$>
         "return" <+> "result" <> semi
        ) <$>
    "end" <> semi <$>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> vhdlTypeMark t <+> "is" <$>
      indent 2
        ( "alias islv      :" <+> "std_logic_vector" <> "(0 to slv'length - 1) is slv;" <$>
          "variable result :" <+> vhdlTypeMark t <> parens ("0 to slv'length / " <> eSz <+> "- 1") <> semi
        ) <$>
    "begin" <$>
      indent 2
        ("for i in result'range loop" <$>
            indent 2
              ( "result" <> parens "i" <+> ":=" <+> case syn of
                    Vivado -> getElem <> semi
                    _ | BitVector _ <- elTy -> getElem <> semi
                      | otherwise           -> "fromSLV" <> parens getElem <> semi

              ) <$>
         "end" <+> "loop" <> semi <$>
         "return" <+> "result" <> semi
        ) <$>
    "end" <> semi
  )
  where
    eSz     = int (typeSize elTy)
    getElem = "islv" <> parens ("i * " <> eSz <+> "to (i+1) * " <> eSz <+> "- 1")

funDec _ (BitVector _) = Just
  ( "function" <+> "toSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "std_logic_vector" <> semi <$>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "std_logic_vector" <> semi
  , "function" <+> "toSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "std_logic_vector" <+> "is" <$>
    "begin" <$>
      indent 2 ("return" <+> "slv" <> semi) <$>
    "end" <> semi <$>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "std_logic_vector" <+> "is" <$>
    "begin" <$>
      indent 2 ("return" <+> "slv" <> semi) <$>
    "end" <> semi
  )

funDec _ (Clock {}) = Just
  ( "function" <+> "toSLV" <+> parens ("sl" <+> colon <+> "in" <+> "std_logic") <+> "return" <+> "std_logic_vector" <> semi <$>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "std_logic" <> semi
  , "function" <+> "toSLV" <+> parens ("sl" <+> colon <+> "in" <+> "std_logic") <+> "return" <+> "std_logic_vector" <+> "is" <$>
    "begin" <$>
      indent 2 ("return" <+> "(0 => sl)" <> semi) <$>
    "end" <> semi <$>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "std_logic" <+> "is" <$>
      indent 2 "alias islv : std_logic_vector(0 to slv'length - 1) is slv;" <$>
    "begin" <$>
      indent 2 ("return" <+> "islv(0)" <> semi) <$>
    "end" <> semi
  )

funDec syn t@(RTree _ elTy) = Just
  ( "function" <+> "toSLV" <+> parens ("value : " <+> vhdlTypeMark t) <+> "return std_logic_vector" <> semi <$>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> vhdlTypeMark t <> semi
  , "function" <+> "toSLV" <+> parens ("value : " <+> vhdlTypeMark t) <+> "return std_logic_vector" <+> "is" <$>
      indent 2
        ( "alias ivalue    :" <+> vhdlTypeMark t <> "(1 to value'length) is value;" <$>
          "variable result :" <+> "std_logic_vector" <> parens ("1 to value'length * " <> int (typeSize elTy)) <> semi
        ) <$>
    "begin" <$>
      indent 2
        ("for i in ivalue'range loop" <$>
            indent 2
              (  "result" <> parens (parens ("(i - 1) * " <> int (typeSize elTy)) <+> "+ 1" <+>
                                             "to i*" <> int (typeSize elTy)) <+>
                          ":=" <+> (case syn of
                                      Vivado -> "ivalue" <> parens ("i")
                                      _ -> "toSLV" <> parens ("ivalue" <> parens ("i"))) <> semi
              ) <$>
         "end" <+> "loop" <> semi <$>
         "return" <+> "result" <> semi
        ) <$>
    "end" <> semi <$>
    "function" <+> "fromSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> vhdlTypeMark t <+> "is" <$>
      indent 2
        ( "alias islv      :" <+> "std_logic_vector" <> "(0 to slv'length - 1) is slv;" <$>
          "variable result :" <+> vhdlTypeMark t <> parens ("0 to slv'length / " <> eSz <+> "- 1") <> semi
        ) <$>
    "begin" <$>
      indent 2
        ("for i in result'range loop" <$>
            indent 2
              ( "result" <> parens "i" <+> ":=" <+> case syn of
                    Vivado -> getElem <> semi
                    _ | BitVector _ <- elTy -> getElem <> semi
                      | otherwise           -> "fromSLV" <> parens getElem <> semi

              ) <$>
         "end" <+> "loop" <> semi <$>
         "return" <+> "result" <> semi
        ) <$>
    "end" <> semi
  )
  where
    eSz     = int (typeSize elTy)
    getElem = "islv" <> parens ("i * " <> eSz <+> "to (i+1) * " <> eSz <+> "- 1")

funDec _ _ = Nothing

tyImports :: String -> VHDLM Doc
tyImports nm = do
  mkId <- mkIdentifier <*> pure Basic
  libs <- use libraries
  packs <- use packages
  punctuate' semi $ sequence
    ([ "library IEEE"
     , "use IEEE.STD_LOGIC_1164.ALL"
     , "use IEEE.NUMERIC_STD.ALL"
     , "use IEEE.MATH_REAL.ALL"
     , "use std.textio.all"
     , "use work.all"
     , "use work." <> text (mkId (T.pack nm `T.append` "_types")) <> ".all"
     ] ++ (map (("library" <+>) . text) (nub libs))
       ++ (map (("use" <+>) . text) (nub packs)))

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
            $ [ (,fromIntegral $ T.length i) A.<$> (encodingNote ty <$> fill l (text i) <+> colon <+> "in" <+> vhdlType ty)
              | (i,ty) <- inputs c ] ++
              [ (,fromIntegral $ T.length i) A.<$> (encodingNote ty <$> fill l (text i) <+> colon <+> "out" <+> vhdlType ty)
              | (_,(i,ty)) <- outputs c ]

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
    hwty' <- normaliseType hwty
    tyCache %= HashSet.insert hwty'
    go hwty'
  where
    go :: HWType -> VHDLM Doc
    go Bool            = "boolean"
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
      nm <- use modNm
      text (T.toLower $ T.pack nm) <> "_types.array_of_" <> tyName elTy <> parens ("0 to " <> int (n-1))
    go (RTree d elTy)  = do
      nm <- use modNm
      text (T.toLower $ T.pack nm) <> "_types.tree_of_" <> tyName elTy <> parens ("0 to " <> int ((2^d)-1))
    go t@(Product _ _) = do
      nm <- use modNm
      text (T.toLower $ T.pack nm) <> "_types." <> tyName t
    go Void            = "std_logic_vector" <> parens (int (-1) <+> "downto 0")
    go String          = "string"
    go ty              = error $ $(curLoc) ++ "vhdlType: type is not normalised: " ++ show ty

sigDecl :: VHDLM Doc -> HWType -> VHDLM Doc
sigDecl d t = d <+> colon <+> vhdlType t

-- | Convert a Netlist HWType to the root of a VHDL type
vhdlTypeMark :: HWType -> VHDLM Doc
vhdlTypeMark hwty = do
  hwty' <- normaliseType hwty
  tyCache %= HashSet.insert hwty'
  go hwty'
  where
    go Bool            = "boolean"
    go (Clock {})      = "std_logic"
    go (Reset {})      = "std_logic"
    go (BitVector _)   = "std_logic_vector"
    go (Signed _)      = "signed"
    go (Unsigned _)    = "unsigned"
    go (Vector _ elTy) = do
      nm <- use modNm
      text (T.toLower $ T.pack nm) <> "_types.array_of_" <> tyName elTy
    go (RTree _ elTy)  = do
      nm <- use modNm
      text (T.toLower $ T.pack nm) <> "_types.tree_of_" <> tyName elTy
    go t@(Product _ _) = do
      nm <- use modNm
      text (T.toLower $ T.pack nm) <> "_types." <> tyName t
    go t               = error $ $(curLoc) ++ "vhdlTypeMark: " ++ show t

tyName :: HWType -> VHDLM Doc
tyName Bool              = "boolean"
tyName (Clock {})        = "std_logic"
tyName (Reset {})        = "std_logic"
tyName (Vector n elTy)   = "array_of_" <> int n <> "_" <> tyName elTy
tyName (RTree n elTy)    = "tree_of_" <> int n <> "_" <> tyName elTy
tyName (BitVector n)     = "std_logic_vector_" <> int n
tyName t@(Index _)       = "unsigned_" <> int (typeSize t)
tyName (Signed n)        = "signed_" <> int n
tyName (Unsigned n)      = "unsigned_" <> int n
tyName t@(Sum _ _)       = "unsigned_" <> int (typeSize t)
tyName t@(Product nm _)  = do
    tN <- normaliseType t
    makeCached tN nameCache prodName
  where
    prodName = do
      tyCache %= HashSet.insert t
      seen <- use tySeen
      mkId <- mkIdentifier <*> pure Basic
      let nm'  = (mkId . last . T.splitOn ".") nm
          nm'' = if T.null nm'
                    then "product"
                    else nm'
          nm3  = if nm'' `elem` seen
                    then go mkId seen (0::Integer) nm''
                    else nm''
      tySeen %= (nm3:)
      text nm3

    go mkId s i n =
      let n' = n `T.append` T.pack ('_':show i)
      in  if n' `elem` s
             then go mkId s (i+1) n
             else n'
tyName t@(SP _ _)        = "std_logic_vector_" <> int (typeSize t)
tyName _ = empty

-- | Convert a Netlist HWType to an error VHDL value for that type
vhdlTypeErrValue :: HWType -> VHDLM Doc
vhdlTypeErrValue Bool                = "true"
vhdlTypeErrValue t@(Vector n elTy)   = do
  syn <-hdlSyn
  case syn of
    Vivado -> vhdlTypeMark t <> "'" <> parens (int 0 <+> "to" <+> int (n-1) <+> rarrow <+>
                "std_logic_vector'" <> parens (int 0 <+> "to" <+> int (typeSize elTy - 1) <+>
                 rarrow <+> "'-'"))
    _ -> vhdlTypeMark t <> "'" <> parens (int 0 <+> "to" <+> int (n-1) <+> rarrow <+> vhdlTypeErrValue elTy)
vhdlTypeErrValue t@(RTree n elTy)    = do
  syn <-hdlSyn
  case syn of
    Vivado -> vhdlTypeMark t <> "'" <>  parens (int 0 <+> "to" <+> int (2^n - 1) <+> rarrow <+>
                "std_logic_vector'" <> parens (int 0 <+> "to" <+> int (typeSize elTy - 1) <+>
                 rarrow <+> "'-'"))
    _ -> vhdlTypeMark t <> "'" <>  parens (int 0 <+> "to" <+> int (2^n - 1) <+> rarrow <+> vhdlTypeErrValue elTy)
vhdlTypeErrValue t@(Product _ elTys) = vhdlTypeMark t <> "'" <> tupled (mapM vhdlTypeErrValue elTys)
vhdlTypeErrValue (Reset {})          = "'-'"
vhdlTypeErrValue (Clock _ _ Source)  = "'-'"
vhdlTypeErrValue (Clock _ _ Gated)   = "('-',false)"
vhdlTypeErrValue Void                = "std_logic_vector'(0 downto 1 => '-')"
vhdlTypeErrValue String              = "\"ERROR\""
vhdlTypeErrValue t                   = vhdlTypeMark t <> "'" <> parens (int 0 <+> "to" <+> int (typeSize t - 1) <+> rarrow <+> "'-'")

decls :: [Declaration] -> VHDLM Doc
decls [] = empty
decls ds = do
    rec (dsDoc,ls) <- fmap (unzip . catMaybes) $ mapM (decl (maximum ls)) ds
    case dsDoc of
      [] -> empty
      _  -> punctuate' semi (A.pure dsDoc)

decl :: Int ->  Declaration -> VHDLM (Maybe (Doc,Int))
decl l (NetDecl' noteM _ id_ ty) = Just A.<$> (,fromIntegral (T.length id_)) A.<$>
  maybe id addNote noteM ("signal" <+> fill l (text id_) <+> colon <+> either text vhdlType ty)
  where
    addNote n = ("--" <+> text n <$>)

decl _ _ = return Nothing

insts :: [Declaration] -> VHDLM Doc
insts [] = empty
insts is = vcat . punctuate linebreak . fmap catMaybes $ mapM inst_ is

-- | Turn a Netlist Declaration to a VHDL concurrent block
inst_ :: Declaration -> VHDLM (Maybe Doc)
inst_ (Assignment id_ e) = fmap Just $
  text id_ <+> larrow <+> expr_ False e <> semi

inst_ (CondAssignment id_ _ scrut _ [(Just (BoolLit b), l),(_,r)]) = fmap Just $
  text id_ <+> larrow
           <+> align (vsep (sequence [expr_ False t <+> "when" <+>
                                      expr_ False scrut <+> "else"
                                     ,expr_ False f <> semi
                                     ]))
  where
    (t,f) = if b then (l,r) else (r,l)

inst_ (CondAssignment id_ _ scrut scrutTy es) = fmap Just $
    "with" <+> parens (expr_ True scrut) <+> "select" <$>
      indent 2 (text id_ <+> larrow <+> align (vcat (punctuate comma (conds esNub)) <> semi))
  where
    esMod = map (first (fmap (patMod scrutTy))) es
    esNub = nubBy ((==) `on` fst) esMod

    conds :: [(Maybe Literal,Expr)] -> VHDLM [Doc]
    conds []                = return []
    conds [(_,e)]           = expr_ False e <+> "when" <+> "others" <:> return []
    conds ((Nothing,e):_)   = expr_ False e <+> "when" <+> "others" <:> return []
    conds ((Just c ,e):es') = expr_ False e <+> "when" <+> patLit scrutTy c <:> conds es'

inst_ (InstDecl libM nm lbl pms) = do
    maybe (return ()) (\lib -> libraries %= (lib:)) libM
    fmap Just $
      nest 2 $ text lbl <+> colon <+> "entity"
                <+> maybe empty ((<> ".") . text) libM <> text nm <$$> pms' <> semi
  where
    pms' = do
      rec (p,ls) <- fmap unzip $ sequence [ (,formalLength i) A.<$> fill (maximum ls) (expr_ False i) <+> "=>" <+> expr_ False e | (i,_,_,e) <- pms]
      nest 2 $ "port map" <$$> tupled (A.pure p)
    formalLength (Identifier i _) = fromIntegral (T.length i)
    formalLength _                = 0


inst_ (BlackBoxD _ libs packs Nothing bs bbCtx) = do
  libraries %= ((map T.fromStrict libs) ++)
  packages  %= ((map T.fromStrict packs) ++)
  t <- renderBlackBox bs bbCtx
  fmap Just (string t)

inst_ (BlackBoxD _ libs packs (Just (nm,inc)) bs bbCtx) = do
  libraries %= ((map T.fromStrict libs) ++)
  packages  %= ((map T.fromStrict packs) ++)
  inc' <- renderBlackBox inc bbCtx
  iw <- use intWidth
  let incHash = hash inc'
      nm'     = T.concat [ T.fromStrict nm
                         , T.pack (printf ("%0" ++ show (iw `div` 4) ++ "X") incHash)
                         ]
  t <- renderBlackBox bs (bbCtx {bbQsysIncName = Just nm'})
  inc'' <- text inc'
  includes %= ((unpack nm', inc''):)
  fmap Just (string t)

inst_ _ = return Nothing

-- | Turn a Netlist expression into a VHDL expression
expr_ :: Bool -- ^ Enclose in parenthesis?
     -> Expr -- ^ Expr to convert
     -> VHDLM Doc
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

expr_ _ (Identifier id_ (Just (Indexed (ty@(Product _ _),_,fI)))) =
  text id_ <> dot <> tyName ty <> "_sel" <> int fI

expr_ _ (Identifier id_ (Just (Indexed (ty@(Clock _ _ Gated),_,fI)))) = do
  ty' <- normaliseType ty
  text id_ <> dot <> tyName ty' <> "_sel" <> int fI

expr_ _ (Identifier id_ (Just (Indexed ((Vector _ elTy),1,1)))) = do
  syn <- hdlSyn
  case syn of
    Vivado -> do
      id' <- fmap (displayT . renderOneLine) (text id_ <> parens (int 0))
      fromSLV elTy id' (typeSize elTy - 1) 0
    _ -> text id_ <> parens (int 0)
expr_ _ (Identifier id_ (Just (Indexed ((Vector n _),1,2)))) = text id_ <> parens (int 1 <+> "to" <+> int (n-1))

-- This is a "Hack", we cannot construct trees with a negative depth. This is
-- here so that we can recognise merged RTree modifiers. See the code in
-- @Clash.Backend.nestM@ which construct these tree modifiers.
expr_ _ (Identifier id_ (Just (Indexed (RTree (-1) _,l,r)))) =
  text id_ <> parens (int l <+> "to" <+> int (r-1))

expr_ _ (Identifier id_ (Just (Indexed ((RTree 0 elTy),0,1)))) = do
  syn <- hdlSyn
  case syn of
    Vivado -> do
      id' <- fmap (displayT . renderOneLine) (text id_ <> parens (int 0))
      fromSLV elTy id' (typeSize elTy - 1) 0
    _ -> text id_ <> parens (int 0)
expr_ _ (Identifier id_ (Just (Indexed ((RTree n _),1,1)))) =
  let z = 2^(n-1)
  in  text id_ <> parens (int 0 <+> "to" <+> int (z-1))
expr_ _ (Identifier id_ (Just (Indexed ((RTree n _),1,2)))) =
  let z  = 2^(n-1)
      z' = 2^n
  in  text id_ <> parens (int z <+> "to" <+> int (z'-1))

-- This is a HACK for Clash.Driver.TopWrapper.mkOutput
-- Vector's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
expr_ _ (Identifier id_ (Just (Indexed ((Vector _ elTy),10,fI)))) = do
  syn <- hdlSyn
  case syn of
    Vivado -> do
      id' <- fmap (displayT . renderOneLine) (text id_ <> parens (int fI))
      fromSLV elTy id' (typeSize elTy - 1) 0
    _ -> text id_ <> parens (int fI)

-- This is a HACK for Clash.Driver.TopWrapper.mkOutput
-- RTree's don't have a 10'th constructor, this is just so that we can
-- recognize the particular case
expr_ _ (Identifier id_ (Just (Indexed ((RTree _ elTy),10,fI)))) = do
  syn <- hdlSyn
  case syn of
    Vivado -> do
      id' <- fmap (displayT . renderOneLine) (text id_ <> parens (int fI))
      fromSLV elTy id' (typeSize elTy - 1) 0
    _ -> text id_ <> parens (int fI)

expr_ _ (Identifier id_ (Just (DC (ty@(SP _ _),_)))) = text id_ <> parens (int start <+> "downto" <+> int end)
  where
    start = typeSize ty - 1
    end   = typeSize ty - conSize ty

expr_ _ (Identifier id_ (Just (Indexed ((Signed _ ),_,_))))  = do
  iw <- use intWidth
  "resize" <> parens (text id_ <> "," <> int iw)
expr_ _ (Identifier id_ (Just (Indexed ((Unsigned _),_,_)))) = do
  iw <- use intWidth
  "resize" <> parens (text id_ <> "," <> int iw)

expr_ b (Identifier id_ (Just (Nested m1 m2))) = case nestM m1 m2 of
  Just m3 -> expr_ b (Identifier id_ (Just m3))
  _ -> do
    k <- expr_ b (Identifier id_ (Just m1))
    expr_ b (Identifier (displayT (renderOneLine k)) (Just m2))

expr_ _ (Identifier id_ (Just _)) = text id_

expr_ b (DataCon _ (DC (Void, -1)) [e]) =  expr_ b e

expr_ _ (DataCon ty@(Vector 0 _) _ _) = vhdlTypeErrValue ty

expr_ _ (DataCon ty@(Vector 1 elTy) _ [e])       = do
  syn <- hdlSyn
  case syn of
    Vivado -> vhdlTypeMark ty <> "'" <> parens (int 0 <+> rarrow <+> toSLV elTy e)
    _ -> vhdlTypeMark ty <> "'" <> parens (int 0 <+> rarrow <+> expr_ False e)
expr_ _ e@(DataCon ty@(Vector _ elTy) _ [e1,e2]) = do
  syn <- hdlSyn
  case syn of
    Vivado -> vhdlTypeMark ty <> "'" <> case vectorChain e of
      Just es -> tupled (mapM (toSLV elTy) es)
      Nothing -> parens ("std_logic_vector'" <> parens (toSLV elTy e1) <+> "&" <+> expr_ False e2)
    _ -> vhdlTypeMark ty <> "'" <> case vectorChain e of
            Just es -> tupled (mapM (expr_ False) es)
            Nothing -> parens (vhdlTypeMark elTy <> "'" <> parens (expr_ False e1) <+> "&" <+> expr_ False e2)

expr_ _ (DataCon ty@(RTree 0 elTy) _ [e]) = do
  syn <- hdlSyn
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
    argExprs   = zipWith toSLV argTys es
    extraArg   = case typeSize ty - dcSize of
                   0 -> []
                   n -> [bits (replicate n U)]
    assignExpr = "std_logic_vector'" <> parens (hcat $ punctuate " & " $ sequence (dcExpr:argExprs ++ extraArg))

expr_ _ (DataCon ty@(Sum _ _) (DC (_,i)) []) = "to_unsigned" <> tupled (sequence [int i,int (typeSize ty)])
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
  , [Literal _ (NumLit n), Literal _ i] <- extractLiterals bbCtx
  = exprLit (Just (BitVector (fromInteger n),fromInteger n)) i

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
  = do iw <- use intWidth
       exprLit (Just (Signed iw,iw)) (NumLit n)

expr_ _ (BlackBoxE pNm _ _ _ _ bbCtx _)
  | pNm == "GHC.Types.W#"
  , [Literal _ (NumLit n)] <- extractLiterals bbCtx
  = do iw <- use intWidth
       exprLit (Just (Unsigned iw,iw)) (NumLit n)

expr_ b (BlackBoxE _ libs packs Nothing bs bbCtx b') = do
  libraries %= ((map T.fromStrict libs) ++)
  packages  %= ((map T.fromStrict packs) ++)
  t <- renderBlackBox bs bbCtx
  parenIf (b || b') $ string t

expr_ b (BlackBoxE _ libs packs (Just (nm,inc)) bs bbCtx b') = do
  libraries %= ((map T.fromStrict libs) ++)
  packages  %= ((map T.fromStrict packs) ++)
  inc' <- renderBlackBox inc bbCtx
  iw <- use intWidth
  let incHash = hash inc'
      nm'     = T.concat [ T.fromStrict nm
                         , T.pack (printf ("%0" ++ show (iw `div` 4) ++ "X") incHash)
                         ]
  t <- renderBlackBox bs (bbCtx {bbQsysIncName = Just nm'})
  inc'' <- text inc'
  includes %= ((unpack nm', inc''):)
  parenIf (b || b') $ string t

expr_ _ (DataTag Bool (Left id_)) = "tagToEnum" <> parens (text id_)
expr_ _ (DataTag Bool (Right id_)) = "dataToTag" <> parens (text id_)

expr_ _ (DataTag hty@(Sum _ _) (Left id_)) =
  "resize" <> parens ("unsigned" <> parens ("std_logic_vector" <> parens (text id_)) <> "," <> int (typeSize hty))
expr_ _ (DataTag (Sum _ _) (Right id_)) = do
  iw <- use intWidth
  "signed" <> parens ("std_logic_vector" <> parens ("resize" <> parens (text id_ <> "," <> int iw)))

expr_ _ (DataTag (Product _ _) (Right _))  = do
  iw <- use intWidth
  "to_signed" <> parens (int 0 <> "," <> int iw)
expr_ _ (DataTag hty@(SP _ _) (Right id_)) = do {
    ; iw <- use intWidth
    ; "signed" <> parens ("std_logic_vector" <> parens (
      "resize" <> parens ("unsigned" <> parens (text id_ <> parens (int start <+> "downto" <+> int end))
                          <> "," <> int iw)))
    }
  where
    start = typeSize hty - 1
    end   = typeSize hty - conSize hty

expr_ _ (DataTag (Vector 0 _) (Right _)) = do
  iw <- use intWidth
  "to_signed" <> parens (int 0 <> "," <> int iw)
expr_ _ (DataTag (Vector _ _) (Right _)) = do
  iw <- use intWidth
  "to_signed" <> parens (int 1 <> "," <> int iw)

expr_ _ (DataTag (RTree 0 _) (Right _)) = do
  iw <- use intWidth
  "to_signed" <> parens (int 0 <> "," <> int iw)
expr_ _ (DataTag (RTree _ _) (Right _)) = do
  iw <- use intWidth
  "to_signed" <> parens (int 1 <> "," <> int iw)

expr_ _ (ConvBV topM hwty True e) = do
  nm <- use modNm
  case topM of
    Nothing -> text (T.pack nm) <> "_types" <> dot <> "toSLV" <>
               parens (vhdlTypeMark hwty <> "'" <> parens (expr_ False e))
    Just t  -> text t <> dot <> text t <> "_types" <> dot <> "toSLV" <> parens (expr_ False e)

expr_ _ (ConvBV topM _ False e) = do
  nm <- use modNm
  maybe (text (T.pack nm) <> "_types" ) (\t -> text t <> dot <> text t <> "_types") topM <> dot <>
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
rtreeChain (DataCon (RTree _ _) _ [e1,e2]) = A.liftA2 (++) (rtreeChain e1) (rtreeChain e2)
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
    hlit = (if i' < 0 then "-" else empty) <> hex (toHex sz i')
exprLit _             (BoolLit t)   = if t then "true" else "false"
exprLit _             (BitLit b)    = squotes $ bit_char b
exprLit _             (StringLit s) = text . T.pack $ show s
exprLit _             l             = error $ $(curLoc) ++ "exprLit: " ++ show l

patLit :: HWType -> Literal -> VHDLM Doc
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

bits :: [Bit] -> VHDLM Doc
bits = dquotes . hcat . mapM bit_char

toHex :: Int -> Integer -> String
toHex sz i =
  let Just d = clogBase 16 (2^sz)
  in  printf ("%0" ++ show d ++ "X") (abs i)

hex :: String -> VHDLM Doc
hex s = char 'x' <> dquotes (text (T.pack s))

bit_char :: Bit -> VHDLM Doc
bit_char H = char '1'
bit_char L = char '0'
bit_char U = char '-'
bit_char Z = char 'Z'

toSLV :: HWType -> Expr -> VHDLM Doc
toSLV Bool         e = do
  nm <- use modNm
  text (T.toLower $ T.pack nm) <> "_types.toSLV" <> parens (expr_ False e)
toSLV (Clock {})    e = do
  nm <- use modNm
  text (T.toLower $ T.pack nm) <> "_types.toSLV" <> parens (expr_ False e)
toSLV (Reset {})    e = do
  nm <- use modNm
  text (T.toLower $ T.pack nm) <> "_types.toSLV" <> parens (expr_ False e)
toSLV (BitVector _) e = expr_ False e
toSLV (Signed _)   e = "std_logic_vector" <> parens (expr_ False e)
toSLV (Unsigned _) e = "std_logic_vector" <> parens (expr_ False e)
toSLV (Index _)    e = "std_logic_vector" <> parens (expr_ False e)
toSLV (Sum _ _)    e = "std_logic_vector" <> parens (expr_ False e)
toSLV t@(Product _ tys) (Identifier id_ Nothing) = do
    selIds' <- sequence selIds
    encloseSep lparen rparen " & " (zipWithM toSLV tys selIds')
  where
    tName    = tyName t
    selNames = map (fmap (displayT . renderOneLine) ) [text id_ <> dot <> tName <> "_sel" <> int i | i <- [0..(length tys)-1]]
    selIds   = map (fmap (\n -> Identifier n Nothing)) selNames
toSLV (Product _ tys) (DataCon _ _ es) = do
  encloseSep lparen rparen " & " (zipWithM toSLV tys es)
toSLV (Product _ _) e = do
  nm <- use modNm
  text (T.toLower $ T.pack nm) <> "_types.toSLV" <> parens (expr_ False e)
toSLV (SP _ _) e = expr_ False e
toSLV (Vector n elTy) (Identifier id_ Nothing) = do
    selIds' <- sequence selIds
    syn <- hdlSyn
    parens (vcat $ punctuate " & "
      (case syn of
        Vivado -> mapM (expr_ False) selIds'
        _ -> mapM (toSLV elTy) selIds'))
  where
    selNames = map (fmap (displayT . renderOneLine) ) $ [text id_ <> parens (int i) | i <- [0 .. (n-1)]]
    selIds   = map (fmap (`Identifier` Nothing)) selNames
toSLV (Vector n elTy) (DataCon _ _ es) = parens $ vcat $ punctuate " & " (zipWithM toSLV [elTy,Vector (n-1) elTy] es)
toSLV (Vector _ _) e = do
  nm <- use modNm
  text (T.toLower $ T.pack nm) <> "_types.toSLV" <> parens (expr_ False e)
toSLV hty      e = error $ $(curLoc) ++  "toSLV: ty:" ++ show hty ++ "\n expr: " ++ show e

fromSLV :: HWType -> Identifier -> Int -> Int -> VHDLM Doc
fromSLV Bool              id_ start _   = do
  nm <- use modNm
  text (T.toLower $ T.pack nm) <> "_types.fromSLV" <> parens (text id_ <> parens (int start <+> "downto" <+> int start))
fromSLV (BitVector _)     id_ start end = text id_ <> parens (int start <+> "downto" <+> int end)
fromSLV (Index _)         id_ start end = "unsigned" <> parens (text id_ <> parens (int start <+> "downto" <+> int end))
fromSLV (Signed _)        id_ start end = "signed" <> parens (text id_ <> parens (int start <+> "downto" <+> int end))
fromSLV (Unsigned _)      id_ start end = "unsigned" <> parens (text id_ <> parens (int start <+> "downto" <+> int end))
fromSLV (Sum _ _)         id_ start end = "unsigned" <> parens (text id_ <> parens (int start <+> "downto" <+> int end))
fromSLV t@(Product _ tys) id_ start _ = do
    tupled $ zipWithM (\s e -> s <+> rarrow <+> e) selNames args
  where
    tName      = tyName t
    selNames   = [tName <> "_sel" <> int i | i <- [0..]]
    argLengths = map typeSize tys
    starts     = start : snd (mapAccumL ((join (,) .) . (-)) start argLengths)
    ends       = map (+1) (tail starts)
    args       = zipWith3 (`fromSLV` id_) tys starts ends

fromSLV (SP _ _)          id_ start end = text id_ <> parens (int start <+> "downto" <+> int end)
fromSLV (Vector n elTy)   id_ start _   =
    if n > 1 then tupled args
             else parens (int 0 <+> rarrow <+> fmap head args)
  where
    argLength = typeSize elTy
    starts    = take (n + 1) $ iterate (subtract argLength) start
    ends      = map (+1) (tail starts)
    args      = do syn <- hdlSyn
                   let elTy' = case syn of
                                 Vivado -> BitVector (argLength - 1)
                                 _ -> elTy
                   zipWithM (fromSLV elTy' id_) starts ends
fromSLV (Clock {})        id_ start _   = text id_ <> parens (int start)
fromSLV (Reset {})        id_ start _   = text id_ <> parens (int start)
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

encodingNote :: HWType -> VHDLM Doc
encodingNote (Clock _ _ Gated) = "-- gated clock"
encodingNote (Clock {})        = "-- clock"
encodingNote (Reset {})        = "-- asynchronous reset: active high"
encodingNote _                 = empty
