{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Create Netlists out of normalized CoreHW Terms
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module CLaSH.Netlist where

import           Control.Exception                (throw)
import           Control.Lens                     ((.=),(^.),_1,_2)
import qualified Control.Lens                     as Lens
import           Control.Monad.State.Strict       (runStateT)
import           Control.Monad.Writer.Strict      (listen, runWriterT, tell)
import           Data.Binary.IEEE754              (floatToWord, doubleToWord)
import           Data.Char                        (ord)
import           Data.Either                      (lefts,partitionEithers)
import           Data.HashMap.Lazy                (HashMap)
import qualified Data.HashMap.Lazy                as HashMap
import           Data.List                        (elemIndex)
import           Data.Maybe                       (fromMaybe)
import           Data.Set                         (toList,fromList)
import qualified Data.Text.Lazy                   as Text
import           Unbound.Generics.LocallyNameless (Embed (..), name2String,
                                                  runFreshMT, unbind, unembed,
                                                  unrebind)

import           SrcLoc                           (SrcSpan,noSrcSpan)

import           CLaSH.Core.DataCon               (DataCon (..))
import           CLaSH.Core.FreeVars              (typeFreeVars)
import           CLaSH.Core.Literal               (Literal (..))
import           CLaSH.Core.Pretty                (showDoc)
import           CLaSH.Core.Term                  (Pat (..), Term (..), TmName)
import qualified CLaSH.Core.Term                  as Core
import           CLaSH.Core.Type                  (Type (..))
import           CLaSH.Core.TyCon                 (TyConName, TyCon)
import           CLaSH.Core.Util                  (collectArgs, isVar, termType)
import           CLaSH.Core.Var                   (Id, Var (..))
import           CLaSH.Driver.Types               (CLaSHException (..))
import           CLaSH.Netlist.BlackBox
import           CLaSH.Netlist.BlackBox.Types     (BlackBoxTemplate)
import           CLaSH.Netlist.Id
import           CLaSH.Netlist.Types              as HW
import           CLaSH.Netlist.Util
import           CLaSH.Normalize.Util
import           CLaSH.Primitives.Types           as P
import           CLaSH.Util

-- | Generate a hierarchical netlist out of a set of global binders with
-- @topEntity@ at the top.
genNetlist :: HashMap TmName (Type,SrcSpan,Term)
           -- ^ Global binders
           -> PrimMap BlackBoxTemplate
           -- ^ Primitive definitions
           -> HashMap TyConName TyCon
           -- ^ TyCon cache
           -> (HashMap TyConName TyCon -> Type -> Maybe (Either String HWType))
           -- ^ Hardcoded Type -> HWType translator
           -> Maybe Int
           -- ^ Symbol count
           -> String
           -- ^ Name of the module containing the @topEntity@
           -> [(String,FilePath)]
           -- ^ Set of collected data-files
           -> Int
           -- ^ Int/Word/Integer bit-width
           -> (Identifier -> Identifier)
           -- ^ valid identifiers
           -> [Identifier]
           -- ^ Seen components
           -> TmName
           -- ^ Name of the @topEntity@
           -> IO ([(SrcSpan,Component)],[(String,FilePath)],[Identifier])
genNetlist globals primMap tcm typeTrans mStart modName dfiles iw mkId seen topEntity = do

  (_,s) <- runNetlistMonad globals primMap tcm typeTrans modName dfiles iw mkId seen $ genComponent topEntity mStart
  return (HashMap.elems $ _components s, _dataFiles s, _seenComps s)

-- | Run a NetlistMonad action in a given environment
runNetlistMonad :: HashMap TmName (Type,SrcSpan,Term)
                -- ^ Global binders
                -> PrimMap BlackBoxTemplate
                -- ^ Primitive Definitions
                -> HashMap TyConName TyCon
                -- ^ TyCon cache
                -> (HashMap TyConName TyCon -> Type -> Maybe (Either String HWType))
                -- ^ Hardcode Type -> HWType translator
                -> String
                -- ^ Name of the module containing the @topEntity@
                -> [(String,FilePath)]
                -- ^ Set of collected data-files
                -> Int
                -- ^ Int/Word/Integer bit-width
                -> (Identifier -> Identifier)
                -- ^ valid identifiers
                -> [Identifier]
                -- ^ Seen components
                -> NetlistMonad a
                -- ^ Action to run
                -> IO (a, NetlistState)
runNetlistMonad s p tcm typeTrans modName dfiles iw mkId seen
  = runFreshMT
  . flip runStateT s'
  . (fmap fst . runWriterT)
  . runNetlist
  where
    s' = NetlistState s HashMap.empty 0 HashMap.empty p typeTrans tcm (Text.empty,noSrcSpan) dfiles iw mkId [] seen' names
    (seen',names) = genNames mkId modName seen HashMap.empty (HashMap.keys s)

genNames :: (Identifier -> Identifier)
         -> String
         -> [Identifier]
         -> HashMap TmName Identifier
         -> [TmName]
         -> ([Identifier], HashMap TmName Identifier)
genNames mkId modName = go
  where
    go s m []       = (s,m)
    go s m (nm:nms) = let nm' = genComponentName s mkId modName nm
                          s'  = nm':s
                          m'  = HashMap.insert nm nm' m
                      in  go s' m' nms

-- | Generate a component for a given function (caching)
genComponent :: TmName -- ^ Name of the function
             -> Maybe Int -- ^ Starting value of the unique counter
             -> NetlistMonad (SrcSpan,Component)
genComponent compName mStart = do
  compExprM <- fmap (HashMap.lookup compName) $ Lens.use bindings
  case compExprM of
    Nothing -> do
      (_,sp) <- Lens.use curCompNm
      throw (CLaSHException sp ($(curLoc) ++ "No normalized expression found for: " ++ show compName) Nothing)
    Just (_,_,expr_) -> do
      c@(_,Component _ clks _ _ _) <- makeCached compName components $ genComponentT compName expr_ mStart
      -- This might seem redundant, because you think `genComponentT` already
      -- added those clocks, right? wrong!
      --
      -- `makeCached` stores the value returned by a monadic action, so when
      -- we use a cached result, its clocks weren't added to the current
      -- writer which is keeping track of used clock ports.
      tell (fromList clks)
      return c

-- | Generate a component for a given function
genComponentT :: TmName -- ^ Name of the function
              -> Term -- ^ Corresponding term
              -> Maybe Int -- ^ Starting value of the unique counter
              -> NetlistMonad (SrcSpan,Component)
genComponentT compName componentExpr mStart = do
  varCount .= fromMaybe 0 mStart
  componentName' <- (HashMap.! compName) <$> Lens.use componentNames
  sp <- ((^. _2) . (HashMap.! compName)) <$> Lens.use bindings
  curCompNm .= (componentName',sp)

  tcm <- Lens.use tcCache
  seenIds .= []
  (arguments,binders,result) <- do { normalizedM <- splitNormalized tcm componentExpr
                                   ; case normalizedM of
                                       Right normalized -> mkUniqueNormalized normalized
                                       Left err         -> throw (CLaSHException sp err Nothing)
                                   }

  let ids = HashMap.fromList
          $ map (\(Id v (Embed t)) -> (v,t))
          $ arguments ++ map fst binders

  gamma <- (ids `HashMap.union`) . HashMap.map (^. _1)
           <$> Lens.use bindings

  varEnv .= gamma

  typeTrans    <- Lens.use typeTranslator
  let resType  = unsafeCoreTypeToHWType $(curLoc) typeTrans tcm $ HashMap.lookupDefault (error $ $(curLoc) ++ "resType" ++ show (result,HashMap.keys ids)) result ids
      argTypes = map (\(Id _ (Embed t)) -> unsafeCoreTypeToHWType $(curLoc) typeTrans tcm t) arguments

  let netDecls = map (\(id_,_) ->
                        NetDecl (Text.pack . name2String $ varName id_)
                                (unsafeCoreTypeToHWType $(curLoc) typeTrans tcm . unembed $ varType id_)
                     ) $ filter ((/= result) . varName . fst) binders
  (decls,clks) <- listen $ concat <$> mapM (uncurry mkDeclarations . second unembed) binders

  let compInps       = zip (map (Text.pack . name2String . varName) arguments) argTypes
      compOutp       = (Text.pack $ name2String result, resType)
      component      = Component componentName' (toList clks) compInps [compOutp] (netDecls ++ decls)
  return (sp,component)


genComponentName :: [Identifier] -> (Identifier -> Identifier) -> String -> TmName -> Identifier
genComponentName seen mkId prefix nm =
  let i = mkId . stripDollarPrefixes . last
        . Text.splitOn (Text.pack ".") . Text.pack
        $ name2String nm
      i' = if Text.null i
              then Text.pack "Component"
              else i
      i'' = mkId (Text.pack (prefix ++ "_") `Text.append` i')
  in  if i'' `elem` seen
         then go 0 i''
         else i''
  where
    go :: Integer -> Identifier -> Identifier
    go n i =
      let i' = mkId (i `Text.append` Text.pack ('_':show n))
      in  if i' `elem` seen
             then go (n+1) i
             else i'

-- | Generate a list of Declarations for a let-binder
mkDeclarations :: Id -- ^ LHS of the let-binder
               -> Term -- ^ RHS of the let-binder
               -> NetlistMonad [Declaration]
mkDeclarations bndr (Var _ v) = mkFunApp bndr v []

mkDeclarations _ e@(Case _ _ []) = do
  (_,sp) <- Lens.use curCompNm
  throw (CLaSHException sp ($(curLoc) ++ "Not in normal form: Case-decompositions with an empty list of alternatives not supported:\n\n" ++ showDoc e) Nothing)

mkDeclarations bndr e@(Case scrut _ [alt]) = do
  (pat,v) <- unbind alt
  (_,sp) <- Lens.use curCompNm
  (varTy,varTm) <- case v of
                     (Var t n) -> return (t,n)
                     _ -> throw (CLaSHException sp ($(curLoc) ++ "Not in normal form: RHS of case-projection is not a variable:\n\n" ++ showDoc e) Nothing)
  typeTrans    <- Lens.use typeTranslator
  tcm          <- Lens.use tcCache
  scrutTy      <- termType tcm scrut
  let sHwTy = unsafeCoreTypeToHWType $(curLoc) typeTrans tcm scrutTy
      vHwTy = unsafeCoreTypeToHWType $(curLoc) typeTrans tcm varTy
  (selId,decls) <- case scrut of
    (Var _ scrutNm) -> return (Text.pack $ name2String scrutNm,[])
    _ -> do
       let scrutId = Text.pack . (++ "_case_scrut") . name2String $ varName bndr
       (newExpr, newDecls) <- mkExpr False (Left scrutId) scrutTy scrut
       case newExpr of
         (Identifier newId Nothing) -> return (newId,newDecls)
         _ -> do
          scrutId' <- mkUniqueIdentifier scrutId
          let scrutDecl = NetDecl scrutId' sHwTy
              scrutAssn = Assignment scrutId' newExpr
          return (scrutId',newDecls ++ [scrutDecl,scrutAssn])
  let dstId    = Text.pack . name2String $ varName bndr
      altVarId = Text.pack $ name2String varTm
      modifier = case pat of
        DataPat (Embed dc) ids -> let (exts,tms) = unrebind ids
                                      tmsTys     = map (unembed . varType) tms
                                      tmsFVs     = concatMap (Lens.toListOf typeFreeVars) tmsTys
                                      extNms     = map varName exts
                                      tms'       = if any (`elem` tmsFVs) extNms
                                                      then throw (CLaSHException sp ($(curLoc) ++ "Not in normal form: Pattern binds existential variables:\n\n" ++ showDoc e) Nothing)
                                                      else tms
                                  in case elemIndex (Id varTm (Embed varTy)) tms' of
                                       Nothing -> Nothing
                                       Just fI
                                        | sHwTy /= vHwTy -> Just (Indexed (sHwTy,dcTag dc - 1,fI))
                                        -- When element and subject have the same HW-type,
                                        -- then the projections is just the identity
                                        | otherwise      -> Just (DC (Void,0))
        _ -> throw (CLaSHException sp ($(curLoc) ++ "Not in normal form: Unexpected pattern in case-projection:\n\n" ++ showDoc e) Nothing)
      extractExpr = Identifier (maybe altVarId (const selId) modifier) modifier
  return (decls ++ [Assignment dstId extractExpr])

mkDeclarations bndr (Case scrut altTy alts) = do
  alts'                  <- reorderPats <$> mapM unbind alts
  tcm                    <- Lens.use tcCache
  scrutTy                <- termType tcm scrut
  scrutHTy               <- unsafeCoreTypeToHWTypeM $(curLoc) scrutTy
  altHTy                 <- unsafeCoreTypeToHWTypeM $(curLoc) altTy
  let scrutId = Text.pack . (++ "_case_scrut") . name2String $ varName bndr
  (_,sp) <- Lens.use curCompNm
  (scrutExpr,scrutDecls) <- first (mkScrutExpr sp scrutHTy (fst (head alts'))) <$> mkExpr True (Left scrutId) scrutTy scrut
  (exprs,altsDecls)      <- (second concat . unzip) <$> mapM (mkCondExpr scrutHTy) alts'

  let dstId = Text.pack . name2String $ varName bndr
  return $! scrutDecls ++ altsDecls ++ [CondAssignment dstId altHTy scrutExpr scrutHTy exprs]
  where
    mkCondExpr :: HWType -> (Pat,Term) -> NetlistMonad ((Maybe HW.Literal,Expr),[Declaration])
    mkCondExpr scrutHTy (pat,alt) = do
      let altId = Text.pack . (++ "_case_alt") . name2String $ varName bndr
      (altExpr,altDecls) <- mkExpr False (Left altId) altTy alt
      (,altDecls) <$> case pat of
        DefaultPat           -> return (Nothing,altExpr)
        DataPat (Embed dc) _ -> return (Just (dcToLiteral scrutHTy (dcTag dc)),altExpr)
        LitPat  (Embed (IntegerLiteral i)) -> return (Just (NumLit i),altExpr)
        LitPat  (Embed (IntLiteral i)) -> return (Just (NumLit i), altExpr)
        LitPat  (Embed (WordLiteral w)) -> return (Just (NumLit w), altExpr)
        LitPat  (Embed (CharLiteral c)) -> return (Just (NumLit . toInteger $ ord c), altExpr)
        LitPat  (Embed (Int64Literal i)) -> return (Just (NumLit i), altExpr)
        LitPat  (Embed (Word64Literal w)) -> return (Just (NumLit w), altExpr)
        LitPat  (Embed (NaturalLiteral n)) -> return (Just (NumLit n), altExpr)
        _  -> do
          (_,sp) <- Lens.use curCompNm
          throw (CLaSHException sp ($(curLoc) ++ "Not an integer literal in LitPat:\n\n" ++ showDoc pat) Nothing)

    mkScrutExpr :: SrcSpan -> HWType -> Pat -> Expr -> Expr
    mkScrutExpr sp scrutHTy pat scrutE = case pat of
      DataPat (Embed dc) _ -> let modifier = Just (DC (scrutHTy,dcTag dc - 1))
                              in case scrutE of
                                  Identifier scrutId _ -> Identifier scrutId modifier
                                  _ -> throw (CLaSHException sp ($(curLoc) ++ "Not in normal form: Not a variable reference or primitive as subject of a case-statement:\n\n" ++ show scrutE) Nothing)
      _ -> scrutE

    -- GHC puts default patterns in the first position, we want them in the
    -- last position.
    reorderPats :: [(Pat,Term)] -> [(Pat,Term)]
    reorderPats ((DefaultPat,e):alts') = alts' ++ [(DefaultPat,e)]
    reorderPats alts'                  = alts'

mkDeclarations bndr app =
  let (appF,(args,tyArgs)) = second partitionEithers $ collectArgs app
  in case appF of
    Var _ f
      | null tyArgs -> mkFunApp bndr f args
      | otherwise   -> do
        (_,sp) <- Lens.use curCompNm
        throw (CLaSHException sp ($(curLoc) ++ "Not in normal form: Var-application with Type arguments:\n\n" ++ showDoc app) Nothing)
    _ -> do
      (exprApp,declsApp) <- mkExpr False (Right bndr) (unembed $ varType bndr) app
      let dstId = Text.pack . name2String $ varName bndr
          assn  = case exprApp of
                    Identifier _ Nothing -> []
                    _ -> [Assignment dstId exprApp]
      return (declsApp ++ assn)

-- | Generate a list of Declarations for a let-binder where the RHS is a function application
mkFunApp :: Id -- ^ LHS of the let-binder
         -> TmName -- ^ Name of the applied function
         -> [Term] -- ^ Function arguments
         -> NetlistMonad [Declaration]
mkFunApp dst fun args = do
  normalized <- Lens.use bindings
  case HashMap.lookup fun normalized of
    Just _ -> do
      (_,Component compName hidden compInps [compOutp] _) <- preserveVarEnv $ genComponent fun Nothing
      if length args == length compInps
        then do tcm <- Lens.use tcCache
                argTys                <- mapM (termType tcm) args
                let dstId = Text.pack . name2String $ varName dst
                (argExprs,argDecls)   <- fmap (second concat . unzip) $! mapM (\(e,t) -> mkExpr False (Left dstId) t e) (zip args argTys)
                (argExprs',argDecls') <- (second concat . unzip) <$> mapM (toSimpleVar dst) (zip argExprs argTys)
                let hiddenAssigns = map (\(i,t) -> (i,In,t,Identifier i Nothing)) hidden
                    inpAssigns    = zipWith (\(i,t) e -> (i,In,t,e)) compInps argExprs'
                    outpAssign    = (fst compOutp,Out,snd compOutp,Identifier dstId Nothing)
                    instLabel     = Text.concat [compName, Text.pack "_", dstId]
                    instDecl      = InstDecl compName instLabel (outpAssign:hiddenAssigns ++ inpAssigns)
                tell (fromList hidden)
                return (argDecls ++ argDecls' ++ [instDecl])
        else error $ $(curLoc) ++ "under-applied normalized function"
    Nothing -> case args of
      [] -> do
        let dstId = Text.pack . name2String $ varName dst
        return [Assignment dstId (Identifier (Text.pack $ name2String fun) Nothing)]
      _ -> error $ $(curLoc) ++ "Unknown function: " ++ showDoc fun

toSimpleVar :: Id
            -> (Expr,Type)
            -> NetlistMonad (Expr,[Declaration])
toSimpleVar _ (e@(Identifier _ _),_) = return (e,[])
toSimpleVar dst (e,ty) = do
  let argNm = Text.pack . (++ "_app_arg") . name2String $ varName dst
  argNm' <- mkUniqueIdentifier argNm
  hTy <- unsafeCoreTypeToHWTypeM $(curLoc) ty
  let argDecl = NetDecl argNm' hTy
      argAssn = Assignment argNm' e
  return (Identifier argNm' Nothing,[argDecl,argAssn])

-- | Generate an expression for a term occurring on the RHS of a let-binder
mkExpr :: Bool -- ^ Treat BlackBox expression as declaration
       -> (Either Identifier Id) -- ^ Id to assign the result to
       -> Type -- ^ Type of the LHS of the let-binder
       -> Term -- ^ Term to convert to an expression
       -> NetlistMonad (Expr,[Declaration]) -- ^ Returned expression and a list of generate BlackBox declarations
mkExpr _ _ _ (Core.Literal l) = do
  iw <- Lens.use intWidth
  case l of
    IntegerLiteral i -> return (HW.Literal (Just (Signed iw,iw)) $ NumLit i, [])
    IntLiteral i     -> return (HW.Literal (Just (Signed iw,iw)) $ NumLit i, [])
    WordLiteral w    -> return (HW.Literal (Just (Unsigned iw,iw)) $ NumLit w, [])
    Int64Literal i   -> return (HW.Literal (Just (Signed 64,64)) $ NumLit i, [])
    Word64Literal w  -> return (HW.Literal (Just (Unsigned 64,64)) $ NumLit w, [])
    CharLiteral c    -> return (HW.Literal (Just (Unsigned 21,21)) . NumLit . toInteger $ ord c, [])
    FloatLiteral r   -> let f = fromRational r :: Float
                            i = toInteger (floatToWord f)
                        in  return (HW.Literal (Just (BitVector 32,32)) (NumLit i), [])
    DoubleLiteral r  -> let d = fromRational r :: Double
                            i = toInteger (doubleToWord d)
                        in  return (HW.Literal (Just (BitVector 64,64)) (NumLit i), [])
    NaturalLiteral n -> return (HW.Literal (Just (Unsigned iw,iw)) $ NumLit n, [])
    _ -> error $ $(curLoc) ++ "not an integer or char literal"

mkExpr bbEasD bndr ty app = do
  let (appF,args) = collectArgs app
      tmArgs      = lefts args
  hwTy    <- unsafeCoreTypeToHWTypeM $(curLoc) ty
  (_,sp) <- Lens.use curCompNm
  case appF of
    Data dc
      | all (\e -> isConstant e || isVar e) tmArgs -> mkDcApplication hwTy bndr dc tmArgs
      | otherwise                                  ->
        throw (CLaSHException sp ($(curLoc) ++ "Not in normal form: DataCon-application with non-Simple arguments:\n\n" ++ showDoc app) Nothing)
    Prim nm _ -> mkPrimitive False bbEasD bndr nm args ty
    Var _ f
      | null tmArgs -> return (Identifier (Text.pack $ name2String f) Nothing,[])
      | otherwise ->
        throw (CLaSHException sp ($(curLoc) ++ "Not in normal form: top-level binder in argument position:\n\n" ++ showDoc app) Nothing)
    _ -> throw (CLaSHException sp ($(curLoc) ++ "Not in normal form: application of a Let/Lam/Case:\n\n" ++ showDoc app) Nothing)

-- | Generate an expression for a DataCon application occurring on the RHS of a let-binder
mkDcApplication :: HWType -- ^ HWType of the LHS of the let-binder
                -> (Either Identifier Id) -- ^ Id to assign the result to
                -> DataCon -- ^ Applied DataCon
                -> [Term] -- ^ DataCon Arguments
                -> NetlistMonad (Expr,[Declaration]) -- ^ Returned expression and a list of generate BlackBox declarations
mkDcApplication dstHType bndr dc args = do
  tcm                 <- Lens.use tcCache
  argTys              <- mapM (termType tcm) args
  let isSP (SP _ _) = True
      isSP _        = False
  let argNm = either id (Text.pack . (++ "_app_arg") . name2String . varName) bndr
  (argExprs,argDecls) <- fmap (second concat . unzip) $! mapM (\(e,t) -> mkExpr (isSP dstHType) (Left argNm) t e) (zip args argTys)
  argHWTys            <- mapM coreTypeToHWTypeM argTys
  fmap (,argDecls) $! case (argHWTys,argExprs) of
    -- Is the DC just a newtype wrapper?
    ([Just argHwTy],[argExpr]) | argHwTy == dstHType ->
      return (HW.DataCon dstHType (DC (Void,-1)) [argExpr])
    _ -> case dstHType of
      SP _ dcArgPairs -> do
        let dcI      = dcTag dc - 1
            dcArgs   = snd $ indexNote ($(curLoc) ++ "No DC with tag: " ++ show dcI) dcArgPairs dcI
        case compare (length dcArgs) (length argExprs) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType,dcI)) argExprs)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"
      Product _ dcArgs ->
        case compare (length dcArgs) (length argExprs) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType,0)) argExprs)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"
      Sum _ _ ->
        return (HW.DataCon dstHType (DC (dstHType,dcTag dc - 1)) [])
      Bool ->
        let dc' = case dcTag dc of
                   1  -> HW.Literal Nothing (BoolLit False)
                   2  -> HW.Literal Nothing (BoolLit True)
                   tg -> error $ $(curLoc) ++ "unknown bool literal: " ++ showDoc dc ++ "(tag: " ++ show tg ++ ")"
        in  return dc'
      Vector 0 _ -> return (HW.DataCon dstHType VecAppend [])
      Vector 1 _ -> case argExprs of
                      [_,e,_] -> return (HW.DataCon dstHType VecAppend [e])
                      _       -> error $ $(curLoc) ++ "Unexpected number of arguments for `Cons`: " ++ showDoc args
      Vector _ _ -> case argExprs of
                      [_,e1,e2] -> return (HW.DataCon dstHType VecAppend [e1,e2])
                      _         -> error $ $(curLoc) ++ "Unexpected number of arguments for `Cons`: " ++ showDoc args
      RTree 0 _ -> case argExprs of
                      [_,e] -> return (HW.DataCon dstHType RTreeAppend [e])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `LR`: " ++ showDoc args
      RTree _ _ -> case argExprs of
                      [_,e1,e2] -> return (HW.DataCon dstHType RTreeAppend [e1,e2])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `BR`: " ++ showDoc args
      String ->
        let dc' = case dcTag dc of
                    1 -> HW.Literal Nothing (StringLit "")
                    _ -> error $ $(curLoc) ++ "mkDcApplication undefined for: " ++ show (dstHType,dc,dcTag dc,args,argHWTys)
        in  return dc'
      _ -> error $ $(curLoc) ++ "mkDcApplication undefined for: " ++ show (dstHType,dc,args,argHWTys)
