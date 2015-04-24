{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Generate VHDL for assorted Netlist datatypes
module CLaSH.Backend.VHDL (VHDLState) where

import qualified Control.Applicative                  as A
import           Control.Lens                         hiding (Indexed)
import           Control.Monad                        (forM,join,liftM,zipWithM)
import           Control.Monad.State                  (State)
import           Data.Graph.Inductive                 (Gr, mkGraph, topsort')
import           Data.HashMap.Lazy                    (HashMap)
import qualified Data.HashMap.Lazy                    as HashMap
import           Data.HashSet                         (HashSet)
import qualified Data.HashSet                         as HashSet
import           Data.List                            (mapAccumL,nubBy)
import           Data.Maybe                           (catMaybes,mapMaybe)
import           Data.Text.Lazy                       (unpack)
import qualified Data.Text.Lazy                       as T
import           Prelude                              hiding ((<$>))
import           Text.PrettyPrint.Leijen.Text.Monadic

import           CLaSH.Backend
import           CLaSH.Netlist.BlackBox.Util          (extractLiterals, renderBlackBox)
import           CLaSH.Netlist.Types
import           CLaSH.Netlist.Util
import           CLaSH.Util                           (clog2, curLoc, makeCached, (<:>))

#ifdef CABAL
import qualified Paths_clash_vhdl
#else
import qualified System.FilePath
#endif

-- | State for the 'CLaSH.Netlist.VHDL.VHDLM' monad:
data VHDLState =
  VHDLState
  { _tyCache   :: (HashSet HWType)     -- ^ Previously encountered HWTypes
  , _tyCount   :: Int                  -- ^ Product type counter
  , _nameCache :: (HashMap HWType Doc) -- ^ Cache for previously generated product type names
  }

makeLenses ''VHDLState

instance Backend VHDLState where
  initBackend     = VHDLState HashSet.empty 0 HashMap.empty
#ifdef CABAL
  primDir         = const (Paths_clash_vhdl.getDataFileName "primitives")
#else
  primDir _       = return ("clash-vhdl" System.FilePath.</> "primitives")
#endif
  extractTypes    = _tyCache
  name            = const "vhdl"
  extension       = const ".vhdl"

  genHDL          = genVHDL
  mkTyPackage     = mkTyPackage_
  hdlType         = vhdlType
  hdlTypeErrValue = vhdlTypeErrValue
  hdlTypeMark     = vhdlTypeMark
  hdlSig t ty     = sigDecl (text t) ty
  inst            = inst_
  expr            = expr_

type VHDLM a = State VHDLState a

-- | Generate VHDL for a Netlist component
genVHDL :: Component -> VHDLM (String,Doc)
genVHDL c = (unpack cName,) A.<$> vhdl
  where
    cName   = componentName c
    vhdl    = "-- Automatically generated VHDL" <$$>
              tyImports <$$> linebreak <>
              entity c <$$> linebreak <>
              architecture c

-- | Generate a VHDL package containing type definitions for the given HWTypes
mkTyPackage_ :: [HWType]
             -> VHDLM Doc
mkTyPackage_ hwtys =
   "library IEEE;" <$>
   "use IEEE.STD_LOGIC_1164.ALL;" <$>
   "use IEEE.NUMERIC_STD.ALL;" <$$> linebreak <>
   "package" <+> "types" <+> "is" <$>
      indent 2 ( packageDec <$>
                 vcat (sequence funDecs)
               ) <$>
   "end" <> semi <> packageBodyDec
  where
    usedTys     = concatMap mkUsedTys hwtys
    needsDec    = nubBy eqReprTy . map mkVecZ $ (hwtys ++ usedTys)
    hwTysSorted = topSortHWTys needsDec
    packageDec  = vcat $ mapM tyDec hwTysSorted
    (funDecs,funBodies) = unzip $ maxDec : (catMaybes $ map funDec (nubBy eqTypM hwTysSorted))

    packageBodyDec :: VHDLM Doc
    packageBodyDec = case funBodies of
        [] -> empty
        _  -> linebreak <$>
              "package" <+> "body" <+> "types" <+> "is" <$>
                indent 2 (vcat (sequence funBodies)) <$>
              "end" <> semi

    eqReprTy :: HWType -> HWType -> Bool
    eqReprTy (Vector n ty1) (Vector m ty2) = n == m && eqReprTy ty1 ty2
    eqReprTy ty1 ty2
      | isUnsigned ty1 && isUnsigned ty2 ||
        isSLV ty1 && isSLV ty2              = typeSize ty1 == typeSize ty2
      | otherwise                           = ty1 == ty2

    eqTypM (Vector n ty1) (Vector m ty2) = n == m && eqReprTy ty1 ty2
    eqTypM (Signed _) (Signed _) = True
    eqTypM ty1 ty2 = isUnsigned ty1 && isUnsigned ty2 ||
                     isSLV      ty1 && isSLV      ty2 ||
                     ty1 == ty2

    isUnsigned :: HWType -> Bool
    isUnsigned (Unsigned _)  = True
    isUnsigned (Index _)     = True
    isUnsigned (Sum _ _)     = True
    isUnsigned _             = False

    isSLV :: HWType -> Bool
    isSLV (BitVector _) = True
    isSLV (SP _ _)      = True
    isSLV _             = False

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
                                      (HashMap.lookup (mkVecZ elTy) nodesI)
    edge t@(Product _ tys) = let ti = HashMap.lookupDefault (error $ $(curLoc) ++ "Product") t nodesI
                             in mapMaybe (\ty -> liftM (ti,,()) (HashMap.lookup (mkVecZ ty) nodesI)) tys
    edge t@(SP _ ctys)     = let ti = HashMap.lookupDefault (error $ $(curLoc) ++ "SP") t nodesI
                             in concatMap (\(_,tys) -> mapMaybe (\ty -> liftM (ti,,()) (HashMap.lookup (mkVecZ ty) nodesI)) tys) ctys
    edge _                 = []

mkVecZ :: HWType -> HWType
mkVecZ (Vector _ elTy) = Vector 0 elTy
mkVecZ t               = t

tyDec :: HWType -> VHDLM Doc
tyDec (Vector _ elTy) = "type" <+> "array_of_" <> tyName elTy <+> "is array (integer range <>) of" <+> vhdlType elTy <> semi

tyDec ty@(Product _ tys) = prodDec
  where
    prodDec = "type" <+> tName <+> "is record" <$>
                indent 2 (vcat $ zipWithM (\x y -> x <+> colon <+> y <> semi) selNames selTys) <$>
              "end record" <> semi

    tName    = tyName ty
    selNames = map (\i -> tName <> "_sel" <> int i) [0..]
    selTys   = map vhdlType tys

tyDec _ = empty


maxDec :: (VHDLM Doc, VHDLM Doc)
maxDec =
  ( "function" <+> "max" <+> parens ("left, right: in integer") <+> "return integer" <> semi
  , "function" <+> "max" <+> parens ("left, right: in integer") <+> "return integer" <+> "is" <$>
    "begin" <$>
      indent 2 (vcat $ sequence [ "if" <+> "left > right" <+> "then return left" <> semi
                                , "else return right" <> semi
                                , "end if" <> semi
                                ]) <$>
    "end" <> semi
  )

funDec :: HWType -> Maybe (VHDLM Doc,VHDLM Doc)
funDec Bool = Just
  ( "function" <+> "toSLV" <+> parens ("b" <+> colon <+> "in" <+> "boolean") <+> "return" <+> "std_logic_vector" <> semi <$>
    "function" <+> "fromSLV" <+> parens ("sl" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "boolean" <> semi
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
    "end" <> semi
  )

funDec Integer = Just
  ( "function" <+> "to_integer" <+> parens ("i" <+> colon <+> "in" <+> "integer") <+> "return" <+> "integer" <> semi <$>
    "function" <+> "toSLV" <+> parens ("i" <+> colon <+> "in" <+> "integer") <+> "return" <+> "std_logic_vector" <> semi
  , "function" <+> "to_integer" <+> parens ("i" <+> colon <+> "in" <+> "integer") <+> "return" <+> "integer" <+> "is" <$>
    "begin" <$>
      indent 2 ("return" <+> "i" <> semi) <$>
    "end" <> semi <$>
    "function" <+> "toSLV" <+> parens ("i" <+> colon <+> "in" <+> "integer") <+> "return" <+> "std_logic_vector" <+> "is" <$>
    "begin" <$>
      indent 2 ("return" <+> "std_logic_vector" <> parens ("to_signed" <> parens ("i" <> comma <> int 32)) <> semi) <$>
    "end" <> semi
  )

funDec (Index _) = Just unsignedToSlvDec

funDec (Signed _) = Just
  ( "function" <+> "toSLV" <+> parens ("s" <+> colon <+> "in" <+> "signed") <+> "return" <+> "std_logic_vector" <> semi
  , "function" <+> "toSLV" <+> parens ("s" <+> colon <+> "in" <+> "signed") <+> "return" <+> "std_logic_vector" <+> "is" <$>
    "begin" <$>
      indent 2 ("return" <+> "std_logic_vector" <> parens ("s") <> semi) <$>
    "end" <> semi
  )

funDec (Unsigned _) = Just unsignedToSlvDec

funDec (Sum _ _) = Just unsignedToSlvDec

funDec t@(Product _ elTys) = Just
  ( "function" <+> "toSLV" <+> parens ("p :" <+> vhdlType t) <+> "return std_logic_vector" <> semi
  , "function" <+> "toSLV" <+> parens ("p :" <+> vhdlType t) <+> "return std_logic_vector" <+> "is" <$>
    "begin" <$>
    indent 2 ("return" <+> parens (hcat (punctuate " & " elTyPrint)) <> semi) <$>
    "end" <> semi
  )
  where
    elTyPrint = forM [0..(length elTys - 1)]
                     (\i -> "toSLV" <>
                            parens ("p." <> vhdlType t <> "_sel" <> int i))

funDec t@(Vector _ elTy) = Just
  ( "function" <+> "toSLV" <+> parens ("value : " <+> vhdlTypeMark t) <+> "return std_logic_vector" <> semi
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
                          ":=" <+> "toSLV" <> parens ("ivalue" <> parens ("i")) <> semi
              ) <$>
         "end" <+> "loop" <> semi <$>
         "return" <+> "result" <> semi
        ) <$>
    "end" <> semi
  )

funDec (BitVector _) = Just slvToSlvDec
funDec (SP _ _)      = Just slvToSlvDec

funDec _ = Nothing

unsignedToSlvDec :: (VHDLM Doc, VHDLM Doc)
unsignedToSlvDec =
  ( "function" <+> "toSLV" <+> parens ("u" <+> colon <+> "in" <+> "unsigned") <+> "return" <+> "std_logic_vector" <> semi
  , "function" <+> "toSLV" <+> parens ("u" <+> colon <+> "in" <+> "unsigned") <+> "return" <+> "std_logic_vector" <+> "is"  <$>
    "begin" <$>
      indent 2 ("return" <+> "std_logic_vector" <> parens ("u") <> semi) <$>
    "end" <> semi
  )

slvToSlvDec :: (VHDLM Doc, VHDLM Doc)
slvToSlvDec =
  ( "function" <+> "toSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "std_logic_vector" <> semi
  , "function" <+> "toSLV" <+> parens ("slv" <+> colon <+> "in" <+> "std_logic_vector") <+> "return" <+> "std_logic_vector" <+> "is" <$>
    "begin" <$>
      indent 2 ("return" <+> "slv" <> semi) <$>
    "end" <> semi
  )

tyImports :: VHDLM Doc
tyImports =
  punctuate' semi $ sequence
    [ "library IEEE"
    , "use IEEE.STD_LOGIC_1164.ALL"
    , "use IEEE.NUMERIC_STD.ALL"
    , "use IEEE.MATH_REAL.ALL"
    , "use work.all"
    , "use work.types.all"
    ]


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
  tyCache %= HashSet.insert hwty
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

sigDecl :: VHDLM Doc -> HWType -> VHDLM Doc
sigDecl d t = d <+> colon <+> vhdlType t

-- | Convert a Netlist HWType to the root of a VHDL type
vhdlTypeMark :: HWType -> VHDLM Doc
vhdlTypeMark hwty = do
  tyCache %= HashSet.insert hwty
  vhdlTypeMark' hwty
  where
    vhdlTypeMark' Bool            = "boolean"
    vhdlTypeMark' (Clock _)       = "std_logic"
    vhdlTypeMark' (Reset _)       = "std_logic"
    vhdlTypeMark' Integer         = "integer"
    vhdlTypeMark' (BitVector _)   = "std_logic_vector"
    vhdlTypeMark' (Index _)       = "unsigned"
    vhdlTypeMark' (Signed _)      = "signed"
    vhdlTypeMark' (Unsigned _)    = "unsigned"
    vhdlTypeMark' (Vector _ elTy) = "array_of_" <> tyName elTy
    vhdlTypeMark' (SP _ _)        = "std_logic_vector"
    vhdlTypeMark' (Sum _ _)       = "unsigned"
    vhdlTypeMark' t@(Product _ _) = tyName t
    vhdlTypeMark' t               = error $ $(curLoc) ++ "vhdlTypeMark: " ++ show t

tyName :: HWType -> VHDLM Doc
tyName Integer           = "integer"
tyName Bool              = "boolean"
tyName (Vector n elTy)   = "array_of_" <> int n <> "_" <> tyName elTy
tyName (BitVector n)     = "std_logic_vector_" <> int n
tyName t@(Index _)       = "unsigned_" <> int (typeSize t)
tyName (Signed n)        = "signed_" <> int n
tyName (Unsigned n)      = "unsigned_" <> int n
tyName t@(Sum _ _)       = "unsigned_" <> int (typeSize t)
tyName t@(Product _ _)   = makeCached t nameCache prodName
  where
    prodName = do i <- tyCount <<%= (+1)
                  "product" <> int i
tyName t@(SP _ _)        = "std_logic_vector_" <> int (typeSize t)
tyName _ = empty

-- | Convert a Netlist HWType to an error VHDL value for that type
vhdlTypeErrValue :: HWType -> VHDLM Doc
vhdlTypeErrValue Bool                = "true"
vhdlTypeErrValue Integer             = "integer'high"
vhdlTypeErrValue (BitVector _)       = "(others => 'X')"
vhdlTypeErrValue (Index _)           = "(others => 'X')"
vhdlTypeErrValue (Signed _)          = "(others => 'X')"
vhdlTypeErrValue (Unsigned _)        = "(others => 'X')"
vhdlTypeErrValue (Vector _ elTy)     = parens ("others" <+> rarrow <+> vhdlTypeErrValue elTy)
vhdlTypeErrValue (SP _ _)            = "(others => 'X')"
vhdlTypeErrValue (Sum _ _)           = "(others => 'X')"
vhdlTypeErrValue (Product _ elTys)   = tupled $ mapM vhdlTypeErrValue elTys
vhdlTypeErrValue (Reset _)           = "'X'"
vhdlTypeErrValue (Clock _)           = "'X'"
vhdlTypeErrValue Void                = "(0 downto 1 => 'X')"

decls :: [Declaration] -> VHDLM Doc
decls [] = empty
decls ds = do
    rec (dsDoc,ls) <- fmap (unzip . catMaybes) $ mapM (decl (maximum ls)) ds
    case dsDoc of
      [] -> empty
      _  -> punctuate' semi (A.pure dsDoc)

decl :: Int ->  Declaration -> VHDLM (Maybe (Doc,Int))
decl l (NetDecl id_ ty) = Just A.<$> (,fromIntegral (T.length id_)) A.<$>
  "signal" <+> fill l (text id_) <+> colon <+> vhdlType ty

decl _ _ = return Nothing

insts :: [Declaration] -> VHDLM Doc
insts [] = empty
insts is = vcat . punctuate linebreak . fmap catMaybes $ mapM inst_ is

-- | Turn a Netlist Declaration to a VHDL concurrent block
inst_ :: Declaration -> VHDLM (Maybe Doc)
inst_ (Assignment id_ e) = fmap Just $
  text id_ <+> larrow <+> expr_ False e <> semi

inst_ (CondAssignment id_ scrut es) = fmap Just $
    "with" <+> parens (expr_ True scrut) <+> "select" <$>
      indent 2 (text id_ <+> larrow <+> align (vcat (punctuate comma (conds es)) <> semi))
  where
    conds :: [(Maybe Expr,Expr)] -> VHDLM [Doc]
    conds []                = return []
    conds [(_,e)]           = expr_ False e <+> "when" <+> "others" <:> return []
    conds ((Nothing,e):_)   = expr_ False e <+> "when" <+> "others" <:> return []
    conds ((Just c ,e):es') = expr_ False e <+> "when" <+> parens (expr_ True c) <:> conds es'

inst_ (InstDecl nm lbl pms) = fmap Just $
    nest 2 $ text lbl <+> colon <+> "entity"
              <+> text nm <$$> pms' <> semi
  where
    pms' = do
      rec (p,ls) <- fmap unzip $ sequence [ (,fromIntegral (T.length i)) A.<$> fill (maximum ls) (text i) <+> "=>" <+> expr_ False e | (i,e) <- pms]
      nest 2 $ "port map" <$$> tupled (A.pure p)

inst_ (BlackBoxD _ bs bbCtx) = do t <- renderBlackBox bs bbCtx
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

expr_ _ (Identifier id_ (Just (Indexed (ty@(Product _ _),_,fI)))) = text id_ <> dot <> tyName ty <> "_sel" <> int fI
expr_ _ (Identifier id_ (Just (DC (ty@(SP _ _),_)))) = text id_ <> parens (int start <+> "downto" <+> int end)
  where
    start = typeSize ty - 1
    end   = typeSize ty - conSize ty

expr_ _ (Identifier id_ (Just _)) = text id_
expr_ _ (DataCon ty@(Vector 1 _) _ [e])           = vhdlTypeMark ty <> "'" <> parens (int 0 <+> rarrow <+> expr_ False e)
expr_ _ e@(DataCon ty@(Vector _ elTy) _ [e1,e2])     = vhdlTypeMark ty <> "'" <> case vectorChain e of
                                                     Just es -> tupled (mapM (expr_ False) es)
                                                     Nothing -> parens (vhdlTypeMark elTy <> "'" <> parens (expr_ False e1) <+> "&" <+> expr_ False e2)
expr_ _ (DataCon ty@(SP _ args) (DC (_,i)) es) = assignExpr
  where
    argTys     = snd $ args !! i
    dcSize     = conSize ty + sum (map typeSize argTys)
    dcExpr     = expr_ False (dcToExpr ty i)
    argExprs   = zipWith toSLV argTys es
    extraArg   = case typeSize ty - dcSize of
                   0 -> []
                   n -> [exprLit (Just (ty,n)) (NumLit 0)]
    assignExpr = "std_logic_vector'" <> parens (hcat $ punctuate " & " $ sequence (dcExpr:argExprs ++ extraArg))

expr_ _ (DataCon ty@(Sum _ _) (DC (_,i)) []) = "to_unsigned" <> tupled (sequence [int i,int (typeSize ty)])
expr_ _ (DataCon ty@(Product _ _) _ es)             = tupled $ zipWithM (\i e -> tName <> "_sel" <> int i <+> rarrow <+> expr_ False e) [0..] es
  where
    tName = tyName ty

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

expr_ b (BlackBoxE _ bs bbCtx b') = do
  t <- renderBlackBox bs bbCtx
  parenIf (b || b') $ string t

expr_ _ (DataTag Bool (Left id_))          = "false when" <+> text id_ <+> "= 0 else true"
expr_ _ (DataTag Bool (Right id_))         = "1 when" <+> text id_ <+> "else 0"
expr_ _ (DataTag hty@(Sum _ _) (Left id_)) = "to_unsigned" <> tupled (sequence [text id_,int (typeSize hty)])
expr_ _ (DataTag (Sum _ _) (Right id_))    = "to_integer" <> parens (text id_)

expr_ _ (DataTag (Product _ _) (Right _))  = int 0
expr_ _ (DataTag hty@(SP _ _) (Right id_)) = "to_integer" <> parens
                                               ("unsigned" <> parens
                                               (text id_ <> parens
                                               (int start <+> "downto" <+> int end)))
  where
    start = typeSize hty - 1
    end   = typeSize hty - conSize hty

expr_ _ (DataTag (Vector 0 _) (Right _)) = int 0
expr_ _ (DataTag (Vector _ _) (Right _)) = int 1

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

exprLit :: Maybe (HWType,Size) -> Literal -> VHDLM Doc
exprLit Nothing       (NumLit i)   = integer i
exprLit (Just (hty,sz)) (NumLit i) = case hty of
                                       Unsigned _  -> "unsigned'" <> parens blit
                                       Signed   _  -> "signed'" <> parens blit
                                       BitVector _ -> "std_logic_vector'" <> parens blit
                                       _           -> blit

  where
    blit = bits (toBits sz i)
exprLit _             (BoolLit t)  = if t then "true" else "false"
exprLit _             (BitLit b)   = squotes $ bit_char b
exprLit _             l            = error $ $(curLoc) ++ "exprLit: " ++ show l

toBits :: Integral a => Int -> a -> [Bit]
toBits size val = map (\x -> if odd x then H else L)
                $ reverse
                $ take size
                $ map (`mod` 2)
                $ iterate (`div` 2) val

bits :: [Bit] -> VHDLM Doc
bits = dquotes . hcat . mapM bit_char

bit_char :: Bit -> VHDLM Doc
bit_char H = char '1'
bit_char L = char '0'
bit_char U = char 'U'
bit_char Z = char 'Z'

toSLV :: HWType -> Expr -> VHDLM Doc
toSLV Bool         e = "toSLV" <> parens (expr_ False e)
toSLV Integer      e = "std_logic_vector" <> parens ("to_signed" <> tupled (sequence [expr_ False e,int 32]))
toSLV (BitVector _) e = expr_ False e
toSLV (Signed _)   e = "std_logic_vector" <> parens (expr_ False e)
toSLV (Unsigned _) e = "std_logic_vector" <> parens (expr_ False e)
toSLV (Sum _ _)    e = "std_logic_vector" <> parens (expr_ False e)
toSLV t@(Product _ tys) (Identifier id_ Nothing) = do
    selIds' <- sequence selIds
    encloseSep lparen rparen " & " (zipWithM toSLV tys selIds')
  where
    tName    = tyName t
    selNames = map (fmap (displayT . renderOneLine) ) [text id_ <> dot <> tName <> "_sel" <> int i | i <- [0..(length tys)-1]]
    selIds   = map (fmap (\n -> Identifier n Nothing)) selNames
toSLV (Product _ tys) (DataCon _ _ es) = encloseSep lparen rparen " & " (zipWithM toSLV tys es)
toSLV (SP _ _) e = expr_ False e
toSLV (Vector n elTy) (Identifier id_ Nothing) = do
    selIds' <- sequence (reverse selIds)
    parens (encloseSep lparen rparen " & " (mapM (toSLV elTy) selIds'))
  where
    selNames = map (fmap (displayT . renderOneLine) ) $ reverse [text id_ <> parens (int i) | i <- [0 .. (n-1)]]
    selIds   = map (fmap (`Identifier` Nothing)) selNames
toSLV (Vector n elTy) (DataCon _ _ es) = encloseSep lparen rparen " & " (zipWithM toSLV [elTy,Vector (n-1) elTy] es)
toSLV hty      e = error $ $(curLoc) ++  "toSLV: ty:" ++ show hty ++ "\n expr: " ++ show e

fromSLV :: HWType -> Identifier -> Int -> Int -> VHDLM Doc
fromSLV Bool              id_ start _   = "fromSLV" <> parens (text id_ <> parens (int start))
fromSLV Integer           id_ start end = "to_integer" <> parens (fromSLV (Signed 32) id_ start end)
fromSLV (BitVector _)     id_ start end = text id_ <> parens (int start <+> "downto" <+> int end)
fromSLV (Index _)         id_ start end = "unsigned" <> parens (text id_ <> parens (int start <+> "downto" <+> int end))
fromSLV (Signed _)        id_ start end = "signed" <> parens (text id_ <> parens (int start <+> "downto" <+> int end))
fromSLV (Unsigned _)      id_ start end = "unsigned" <> parens (text id_ <> parens (int start <+> "downto" <+> int end))
fromSLV (Sum _ _)         id_ start end = "unsigned" <> parens (text id_ <> parens (int start <+> "downto" <+> int end))
fromSLV t@(Product _ tys) id_ start _   = tupled $ zipWithM (\s e -> s <+> rarrow <+> e) selNames args
  where
    tName      = tyName t
    selNames   = [tName <> "_sel" <> int i | i <- [0..]]
    argLengths = map typeSize tys
    starts     = start : snd (mapAccumL ((join (,) .) . (-)) start argLengths)
    ends       = map (+1) (tail starts)
    args       = zipWith3 (`fromSLV` id_) tys starts ends

fromSLV (SP _ _)          id_ start end = text id_ <> parens (int start <+> "downto" <+> int end)
fromSLV (Vector n elTy)   id_ start _   = tupled (fmap reverse args)
  where
    argLength = typeSize elTy
    starts    = take (n + 1) $ iterate (subtract argLength) start
    ends      = map (+1) (tail starts)
    args      = zipWithM (fromSLV elTy id_) starts ends
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
