{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Functions to create BlackBox Contexts and fill in BlackBox templates
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Netlist.BlackBox where

import           Control.Exception             (throw)
import           Control.Lens                  ((<<%=),(%=))
import qualified Control.Lens                  as Lens
import           Control.Monad                 (when)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Char                     (ord)
import           Data.Either                   (lefts)
import qualified Data.HashMap.Lazy             as HashMap
import qualified Data.IntMap                   as IntMap
import           Data.Maybe                    (catMaybes)
import           Data.Semigroup.Monad
import qualified Data.Set                      as Set
import           Data.Text.Lazy                (fromStrict)
import qualified Data.Text.Lazy                as Text
import           Data.Text                     (unpack)
import qualified Data.Text                     as TextS
import qualified System.Console.ANSI           as ANSI
import           System.Console.ANSI
  ( hSetSGR, SGR(SetConsoleIntensity, SetColor), Color(Magenta)
  , ConsoleIntensity(BoldIntensity), ConsoleLayer(Foreground), ColorIntensity(Vivid))
import           System.IO
  (hPutStrLn, stderr, hFlush, hIsTerminalDevice)
import           Util                          (OverridingBool(..))

-- import           Clash.Backend                 as N
import           Clash.Core.DataCon            as D (dcTag)
import           Clash.Core.FreeVars           (termFreeIds)
import           Clash.Core.Literal            as L (Literal (..))
import           Clash.Core.Name
  (Name (..), mkUnsafeSystemName)
import           Clash.Core.Pretty             (showPpr)
import           Clash.Core.Subst              (extendIdSubst, mkSubst, substTm)
import           Clash.Core.Term               as C (Term (..))
import           Clash.Core.Type               as C (Type (..), ConstTy (..),
                                                splitFunTys)
import           Clash.Core.TyCon              as C (tyConDataCons)
import           Clash.Core.Util               (collectArgs, isFun, termType)
import           Clash.Core.Var                as V (Id, Var (..), mkId, modifyVarName)
import           Clash.Core.VarEnv
  (extendInScopeSet, mkInScopeSet, lookupVarEnv, unionInScope, uniqAway, unitVarSet)
import {-# SOURCE #-} Clash.Netlist
  (genComponent, mkDcApplication, mkDeclarations, mkExpr, mkNetDecl,
   mkProjection, mkSelection)
import qualified Clash.Backend                 as Backend
import           Clash.Driver.Types
  (opt_primWarn, opt_color, ClashOpts)
import           Clash.Netlist.BlackBox.Types  as B
import           Clash.Netlist.BlackBox.Util   as B
import           Clash.Netlist.Id              (IdType (..))
import           Clash.Netlist.Types           as N
import           Clash.Netlist.Util            as N
import           Clash.Normalize.Util          (isConstant)
import           Clash.Primitives.Types        as P
import           Clash.Unique                  (lookupUniqMap')
import           Clash.Util

-- | Emits (colorized) warning to stderr
warn
  :: ClashOpts
  -> String
  -> IO ()
warn opts msg = do
  -- TODO: Put in appropriate module
  useColor <-
    case opt_color opts of
      Always -> return True
      Never  -> return False
      Auto   -> hIsTerminalDevice stderr

  hSetSGR stderr [SetConsoleIntensity BoldIntensity]
  when useColor $ hSetSGR stderr [SetColor Foreground Vivid Magenta]
  hPutStrLn stderr $ "[WARNING] " ++ msg
  hSetSGR stderr [ANSI.Reset]
  hFlush stderr

-- | Generate the context for a BlackBox instantiation.
mkBlackBoxContext
  :: Id
  -- ^ Identifier binding the primitive/blackbox application
  -> [Term]
  -- ^ Arguments of the primitive/blackbox application
  -> NetlistMonad (BlackBoxContext,[Declaration])
mkBlackBoxContext resId args = do
    -- Make context inputs
    tcm             <- Lens.use tcCache
    let resNm = nameOcc (varName resId)
    (imps,impDecls) <- unzip <$> mapM (mkArgument resNm) args
    (funs,funDecls) <- mapAccumLM (addFunction tcm) IntMap.empty (zip args [0..])

    -- Make context result
    let res = Identifier resNm Nothing
    resTy <- unsafeCoreTypeToHWTypeM $(curLoc) (V.varType resId)

    lvl <- Lens.use curBBlvl
    (nm,_) <- Lens.use curCompNm

    return ( Context (res,resTy) imps funs [] lvl nm
           , concat impDecls ++ concat funDecls
           )
  where
    addFunction tcm im (arg,i) = if isFun tcm arg
      then do curBBlvl Lens.+= 1
              (f,d) <- mkFunInput resId arg
              curBBlvl Lens.-= 1
              let im' = IntMap.insert i f im
              return (im',d)
      else return (im,[])

prepareBlackBox
  :: TextS.Text
  -> BlackBox
  -> BlackBoxContext
  -> NetlistMonad (BlackBox,[Declaration])
prepareBlackBox pNm templ bbCtx =
  if verifyBlackBoxContext bbCtx templ
     then do
        (t2,decls) <-
          onBlackBox
            (fmap (first BBTemplate) . setSym bbCtx)
            (\bbName bbHash bbFunc -> pure (BBFunction bbName bbHash bbFunc, []))
            templ
        return (t2,decls)
     else do
       (_,sp) <- Lens.use curCompNm
       templ' <- onBlackBox (getMon . prettyBlackBox)
                            (\n h f -> return $ Text.pack $ show (BBFunction n h f))
                            templ
       let msg = $(curLoc) ++ "Can't match template for " ++ show pNm ++ " :\n\n" ++ Text.unpack templ' ++
                "\n\nwith context:\n\n" ++ show bbCtx
       throw (ClashException sp msg Nothing)

mkArgument
  :: Identifier
  -- ^ LHS of the original let-binder
  -> Term
  -> NetlistMonad ( (Expr,HWType,Bool)
                  , [Declaration]
                  )
mkArgument bndr e = do
    tcm   <- Lens.use tcCache
    let ty = termType tcm e
    iw    <- Lens.use intWidth
    hwTyM <- N.termHWTypeM e
    let eTyMsg = "(" ++ showPpr e ++ " :: " ++ showPpr ty ++ ")"
    ((e',t,l),d) <- case hwTyM of
      Nothing
        | (Prim nm _,_) <- collectArgs e
        , nm == "Clash.Transformations.removedArg"
        -> return ((Identifier nm Nothing, Void Nothing, False),[])
        | otherwise
        -> return ((error ($(curLoc) ++ "Forced to evaluate untranslatable type: " ++ eTyMsg), Void Nothing, False), [])
      Just hwTy -> case collectArgs e of
        (C.Var v,[]) -> return ((Identifier (nameOcc (varName v)) Nothing,hwTy,False),[])
        (C.Literal (IntegerLiteral i),[]) -> return ((N.Literal (Just (Signed iw,iw)) (N.NumLit i),hwTy,True),[])
        (C.Literal (IntLiteral i), []) -> return ((N.Literal (Just (Signed iw,iw)) (N.NumLit i),hwTy,True),[])
        (C.Literal (WordLiteral w), []) -> return ((N.Literal (Just (Unsigned iw,iw)) (N.NumLit w),hwTy,True),[])
        (C.Literal (CharLiteral c), []) -> return ((N.Literal (Just (Unsigned 21,21)) (N.NumLit . toInteger $ ord c),hwTy,True),[])
        (C.Literal (StringLiteral s),[]) -> return ((N.Literal Nothing (N.StringLit s),hwTy,True),[])
        (C.Literal (Int64Literal i), []) -> return ((N.Literal (Just (Signed 64,64)) (N.NumLit i),hwTy,True),[])
        (C.Literal (Word64Literal i), []) -> return ((N.Literal (Just (Unsigned 64,64)) (N.NumLit i),hwTy,True),[])
        (C.Literal (NaturalLiteral n), []) -> return ((N.Literal (Just (Unsigned iw,iw)) (N.NumLit n),hwTy,True),[])
        (Prim f _,args) -> do
          (e',d) <- mkPrimitive True False (Left bndr) f args ty
          case e' of
            (Identifier _ _) -> return ((e',hwTy,False), d)
            _                -> return ((e',hwTy,isConstant e), d)
        (Data dc, args) -> do
          (exprN,dcDecls) <- mkDcApplication hwTy (Left bndr) dc (lefts args)
          return ((exprN,hwTy,isConstant e),dcDecls)
        (Case scrut ty' [alt],[]) -> do
          (projection,decls) <- mkProjection False (Left bndr) scrut ty' alt
          return ((projection,hwTy,False),decls)
        _ ->
          return ((Identifier (error ($(curLoc) ++ "Forced to evaluate unexpected function argument: " ++ eTyMsg)) Nothing
                  ,hwTy,False),[])
    return ((e',t,l),d)

mkPrimitive
  :: Bool
  -- ^ Put BlackBox expression in parenthesis
  -> Bool
  -- ^ Treat BlackBox expression as declaration
  -> (Either Identifier Id)
  -- ^ Id to assign the result to
  -> TextS.Text
  -- ^ Name of primitive
  -> [Either Term Type]
  -- ^ Arguments
  -> Type
  -- ^ Result type
  -> NetlistMonad (Expr,[Declaration])
mkPrimitive bbEParen bbEasD dst nm args ty = do
  go =<< HashMap.lookup nm <$> Lens.use primitives
  where
    go
      :: Maybe CompiledPrimitive
      -> NetlistMonad (Expr, [Declaration])
    go =
      \case
        Just (P.BlackBoxHaskell bbName funcName (_fHash, func)) ->
          case func bbEasD dst nm args ty of
            Left err -> do
              -- Blackbox template function returned an error:
              let err' = unwords [ $(curLoc) ++ "Could not create blackbox"
                                 , "template using", show funcName, "for"
                                 , show bbName ++ ".", "Function reported: \n\n"
                                 , err ]
              (_,sp) <- Lens.use curCompNm
              throw (ClashException sp err' Nothing)
            Right ((BlackBoxMeta {..}), bbTemplate) ->
              -- Blackbox template generation succesful. Rerun 'go', but this time
              -- around with a 'normal' @BlackBox@
              go (Just (P.BlackBox bbName bbKind Nothing bbOutputReg bbLibrary bbImports bbIncludes bbTemplate))
        Just p@(P.BlackBox {outputReg = wr, warning = wn}) -> do
          -- Print blackbox warning if warning is set on this blackbox and
          -- printing warnings is enabled globally
          isTB <- Lens.use isTestBench
          opts <- Lens.use clashOpts
          primWarn <- opt_primWarn <$> Lens.use clashOpts
          seen <- Set.member nm <$> Lens.use seenPrimitives
          case (wn, primWarn, seen, isTB) of
            (Just msg, True, False, False) -> do
              liftIO $ warn opts $ "Dubious primitive instantiation: "
                                ++ unpack msg
                                ++ " (disable with -fclash-no-prim-warn)"
            _ ->
              return ()

          seenPrimitives %= (Set.insert nm)

          case kind p of
            TDecl -> do
              let tempD = template p
                  pNm = name p
                  wr' = if wr then Reg else Wire
              resM <- resBndr True wr' dst
              case resM of
                Just (dst',dstNm,dstDecl) -> do
                  (bbCtx,ctxDcls)   <- mkBlackBoxContext dst' (lefts args)
                  (templ,templDecl) <- prepareBlackBox pNm tempD bbCtx
                  let bbDecl = N.BlackBoxD pNm (libraries p) (imports p)
                                           (includes p) templ bbCtx
                  return (Identifier dstNm Nothing,dstDecl ++ ctxDcls ++ templDecl ++ [bbDecl])
                Nothing -> return (Identifier "__VOID__" Nothing,[])
            TExpr -> do
              let tempE = template p
                  pNm = name p
              if bbEasD
                then do
                  resM <- resBndr True Wire dst
                  case resM of
                    Just (dst',dstNm,dstDecl) -> do
                      (bbCtx,ctxDcls)     <- mkBlackBoxContext dst' (lefts args)
                      (bbTempl,templDecl) <- prepareBlackBox pNm tempE bbCtx
                      let tmpAssgn = Assignment dstNm
                                        (BlackBoxE pNm (libraries p) (imports p)
                                                   (includes p) bbTempl bbCtx
                                                   bbEParen)
                      return (Identifier dstNm Nothing, dstDecl ++ ctxDcls ++ templDecl ++ [tmpAssgn])
                    Nothing -> return (Identifier "__VOID__" Nothing,[])
                else do
                  resM <- resBndr False Wire dst
                  case resM of
                    Just (dst',_,_) -> do
                      (bbCtx,ctxDcls)     <- mkBlackBoxContext dst' (lefts args)
                      (bbTempl,templDecl) <- prepareBlackBox pNm tempE bbCtx
                      return (BlackBoxE pNm (libraries p) (imports p) (includes p) bbTempl bbCtx bbEParen,ctxDcls ++ templDecl)
                    Nothing -> return (Identifier "__VOID__" Nothing,[])
        Just (P.Primitive pNm _)
          | pNm == "GHC.Prim.tagToEnum#" -> do
              hwTy <- N.unsafeCoreTypeToHWTypeM $(curLoc) ty
              case args of
                [Right (ConstTy (TyCon tcN)), Left (C.Literal (IntLiteral i))] -> do
                  tcm <- Lens.use tcCache
                  let dcs = tyConDataCons (tcm `lookupUniqMap'` tcN)
                      dc  = dcs !! fromInteger i
                  (exprN,dcDecls) <- mkDcApplication hwTy dst dc []
                  return (exprN,dcDecls)
                [Right _, Left scrut] -> do
                  tcm     <- Lens.use tcCache
                  let scrutTy = termType tcm scrut
                  (scrutExpr,scrutDecls) <- mkExpr False (Left "#tte_rhs") scrutTy scrut
                  case scrutExpr of
                    Identifier id_ Nothing -> return (DataTag hwTy (Left id_),scrutDecls)
                    _ -> do
                      scrutHTy <- unsafeCoreTypeToHWTypeM $(curLoc) scrutTy
                      tmpRhs <- mkUniqueIdentifier Extended "#tte_rhs"
                      let netDeclRhs   = NetDecl Nothing tmpRhs scrutHTy
                          netAssignRhs = Assignment tmpRhs scrutExpr
                      return (DataTag hwTy (Left tmpRhs),[netDeclRhs,netAssignRhs] ++ scrutDecls)
                _ -> error $ $(curLoc) ++ "tagToEnum: " ++ show (map (either showPpr showPpr) args)
          | pNm == "GHC.Prim.dataToTag#" -> case args of
              [Right _,Left (Data dc)] -> do
                iw <- Lens.use intWidth
                return (N.Literal (Just (Signed iw,iw)) (NumLit $ toInteger $ dcTag dc - 1),[])
              [Right _,Left scrut] -> do
                tcm      <- Lens.use tcCache
                let scrutTy = termType tcm scrut
                scrutHTy <- unsafeCoreTypeToHWTypeM $(curLoc) scrutTy
                (scrutExpr,scrutDecls) <- mkExpr False (Left "#dtt_rhs") scrutTy scrut
                case scrutExpr of
                  Identifier id_ Nothing -> return (DataTag scrutHTy (Right id_),scrutDecls)
                  _ -> do
                    tmpRhs  <- mkUniqueIdentifier Extended "#dtt_rhs"
                    let netDeclRhs   = NetDecl Nothing tmpRhs scrutHTy
                        netAssignRhs = Assignment tmpRhs scrutExpr
                    return (DataTag scrutHTy (Right tmpRhs),[netDeclRhs,netAssignRhs] ++ scrutDecls)
              _ -> error $ $(curLoc) ++ "dataToTag: " ++ show (map (either showPpr showPpr) args)
          | otherwise ->
              return (BlackBoxE "" [] [] []
                        (BBTemplate [C $ mconcat ["NO_TRANSLATION_FOR:",fromStrict pNm]])
                        emptyBBContext False,[])
        _ -> do
          (_,sp) <- Lens.use curCompNm
          throw (ClashException sp ($(curLoc) ++ "No blackbox found for: " ++ unpack nm) Nothing)

    resBndr
      :: Bool
      -> WireOrReg
      -> (Either Identifier Id)
      -> NetlistMonad (Maybe (Id,Identifier,[Declaration]))
      -- Nothing when the binder would have type `Void`
    resBndr mkDec wr dst' = case dst' of
      Left dstL -> case mkDec of
        False -> do
          -- TODO: check that it's okay to use `mkUnsafeSystemName`
          let nm' = mkUnsafeSystemName dstL 0
              id_ = mkId ty nm'
          return (Just (id_,dstL,[]))
        True -> do
          nm'  <- extendIdentifier Extended dstL "_res"
          nm'' <- mkUniqueIdentifier Extended nm'
          -- TODO: check that it's okay to use `mkUnsafeInternalName`
          let nm3 = mkUnsafeSystemName nm'' 0
          hwTy <- N.unsafeCoreTypeToHWTypeM $(curLoc) ty
          let id_    = mkId ty nm3
              idDecl = NetDecl' Nothing wr nm'' (Right hwTy)
          case hwTy of
            Void {} -> return Nothing
            _       -> return (Just (id_,nm'',[idDecl]))
      Right dstR -> return (Just (dstR,nameOcc . varName $ dstR,[]))

-- | Create an template instantiation text and a partial blackbox content for an
-- argument term, given that the term is a function. Errors if the term is not
-- a function
mkFunInput
  :: Id
  -- ^ Identifier binding the encompassing primitive/blackbox application
  -> Term
  -- ^ The function argument term
  -> NetlistMonad
      ((Either BlackBox (Identifier,[Declaration])
       ,WireOrReg
       ,[BlackBoxTemplate]
       ,[BlackBoxTemplate]
       ,[((TextS.Text,TextS.Text),BlackBox)]
       ,BlackBoxContext)
      ,[Declaration])
mkFunInput resId e = do
  let (appE,args) = collectArgs e
  (bbCtx,dcls) <- mkBlackBoxContext resId (lefts args)
  templ <- case appE of
            Prim nm _ -> do
              bbM <- fmap (HashMap.lookup nm) $ Lens.use primitives
              (_,sp) <- Lens.use curCompNm
              let templ = case bbM of
                            Just (P.BlackBox {..}) -> Left (kind,outputReg,libraries,imports,includes,nm,template)
                            _ -> throw (ClashException sp ($(curLoc) ++ "No blackbox found for: " ++ unpack nm) Nothing)
              return templ
            Data dc -> do
              tcm <- Lens.use tcCache
              let eTy = termType tcm e
                  (_,resTy) = splitFunTys tcm eTy
              resHTyM <- coreTypeToHWTypeM resTy
              case resHTyM of
                Just resHTy@(SP _ dcArgPairs) -> do
                  let dcI      = dcTag dc - 1
                      dcArgs   = snd $ indexNote ($(curLoc) ++ "No DC with tag: " ++ show dcI) dcArgPairs dcI
                      dcInps   = [ Identifier (TextS.pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(0::Int)..(length dcArgs - 1)]]
                      dcApp    = DataCon resHTy (DC (resHTy,dcI)) dcInps
                      dcAss    = Assignment "~RESULT" dcApp
                  return (Right (("",[dcAss]),Wire))
                Just resHTy@(Product _ dcArgs) -> do
                  let dcInps = [ Identifier (TextS.pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(0::Int)..(length dcArgs - 1)]]
                      dcApp  = DataCon resHTy (DC (resHTy,0)) dcInps
                      dcAss  = Assignment "~RESULT" dcApp
                  return (Right (("",[dcAss]),Wire))
                Just resHTy@(Vector _ _) -> do
                  let dcInps = [ Identifier (TextS.pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(1::Int)..2] ]
                      dcApp  = DataCon resHTy (DC (resHTy,1)) dcInps
                      dcAss  = Assignment "~RESULT" dcApp
                  return (Right (("",[dcAss]),Wire))
                -- The following happens for things like `Maybe ()`
                Just resHTy@(Sum _ _) -> do
                  let dcI   = dcTag dc - 1
                      dcApp = DataCon resHTy (DC (resHTy,dcI)) []
                      dcAss = Assignment "~RESULT" dcApp
                  return (Right (("",[dcAss]),Wire))
                -- The following happens for things like `(1,())`
                Just _ -> do
                  let inp   = Identifier "~ARG[0]" Nothing
                      assgn = Assignment "~RESULT" inp
                  return (Right (("",[assgn]),Wire))
                _ -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showPpr e
            C.Var fun -> do
              normalized <- Lens.use bindings
              case lookupVarEnv fun normalized of
                Just _ -> do
                  (_,_,Component compName compInps [snd -> compOutp] _) <- preserveVarEnv $ genComponent fun
                  let inpAssigns    = zipWith (\(i,t) e' -> (Identifier i Nothing,In,t,e')) compInps [ Identifier (TextS.pack ("~ARG[" ++ show x ++ "]")) Nothing | x <- [(0::Int)..] ]
                      outpAssign    = (Identifier (fst compOutp) Nothing,Out,snd compOutp,Identifier "~RESULT" Nothing)
                  i <- varCount <<%= (+1)
                  let instLabel     = TextS.concat [compName,TextS.pack ("_" ++ show i)]
                      instDecl      = InstDecl Entity Nothing compName instLabel (outpAssign:inpAssigns)
                  return (Right (("",[instDecl]),Wire))
                Nothing -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showPpr e
            C.Lam {} -> do
              let is0 = mkInScopeSet (Lens.foldMapOf termFreeIds unitVarSet appE)
              go is0 0 appE
            _ -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showPpr e
  case templ of
    Left (TDecl,oreg,libs,imps,inc,_,templ') -> do
      (l',templDecl)
        <- onBlackBox
            (fmap (first BBTemplate) . setSym bbCtx)
            (\bbName bbHash bbFunc -> pure $ (BBFunction bbName bbHash bbFunc, []))
            templ'
      return ((Left l',if oreg then Reg else Wire,libs,imps,inc,bbCtx),dcls ++ templDecl)
    Left (TExpr,_,libs,imps,inc,nm,templ') -> do
      onBlackBox
        (\t -> do t' <- getMon (prettyBlackBox t)
                  let assn = Assignment "~RESULT" (Identifier (Text.toStrict t') Nothing)
                  return ((Right ("",[assn]),Wire,libs,imps,inc,bbCtx),dcls))
        (\bbName bbHash (TemplateFunction k g _) -> do
          let f' bbCtx' = do
                let assn = Assignment "~RESULT"
                            (BlackBoxE nm libs imps inc templ' bbCtx' False)
                p <- getMon (Backend.blockDecl "" [assn])
                return p
          return ((Left (BBFunction bbName bbHash (TemplateFunction k g f'))
                  ,Wire
                  ,[]
                  ,[]
                  ,[]
                  ,bbCtx
                  )
                 ,dcls
                 )
        )
        templ'
    Right (decl,wr) ->
      return ((Right decl,wr,[],[],[],bbCtx),dcls)
  where
    go is0 n (Lam id_ e') = do
      lvl <- Lens.use curBBlvl
      let nm    = TextS.concat
                    ["~ARGN[",TextS.pack (show lvl),"][",TextS.pack (show n),"]"]
          v'    = uniqAway is0 (modifyVarName (\v -> v {nameOcc = nm}) id_)
          subst = extendIdSubst (mkSubst is0) id_ (C.Var v')
          e''   = substTm "mkFunInput.goLam" subst e'
          is1   = extendInScopeSet is0 v'
      go is1 (n+(1::Int)) e''

    go _ _ (C.Var v) = do
      let assn = Assignment "~RESULT" (Identifier (nameOcc (varName v)) Nothing)
      return (Right (("",[assn]),Wire))

    go _ _ (Case scrut ty [alt]) = do
      (projection,decls) <- mkProjection False (Left "#bb_res") scrut ty alt
      let assn = Assignment "~RESULT" projection
      nm <- if null decls
               then return ""
               else mkUniqueIdentifier Basic "projection"
      return (Right ((nm,decls ++ [assn]),Wire))

    go _ _ (Case scrut ty alts@(_:_:_)) = do
      -- TODO: check that it's okay to use `mkUnsafeSystemName`
      let resId'  = resId {varName = mkUnsafeSystemName "~RESULT" 0}
      selectionDecls <- mkSelection resId' scrut ty alts
      nm <- mkUniqueIdentifier Basic "selection"
      return (Right ((nm,selectionDecls),Reg))

    go _ _ e'@(App _ _) = do
      tcm <- Lens.use tcCache
      let eType = termType tcm e'
      (appExpr,appDecls) <- mkExpr False (Left "#bb_res") eType e'
      let assn = Assignment "~RESULT" appExpr
      nm <- if null appDecls
               then return ""
               else mkUniqueIdentifier Basic "block"
      return (Right ((nm,appDecls ++ [assn]),Wire))

    go is0 _ e'@(Letrec {}) = do
      tcm <- Lens.use tcCache
      let normE = splitNormalized tcm e'
      ([],[],_,[],binders,resultM) <- case normE of
        Right norm -> do
          isCur <- Lens.use globalInScope
          globalInScope Lens..= is0 `unionInScope` isCur
          norm' <- mkUniqueNormalized Nothing norm
          globalInScope Lens..= isCur
          return norm'
        Left err -> error err
      case resultM of
        Just result -> do
          let binders' = map (\(id_,tm) -> (goR result id_,tm)) binders
          netDecls <- fmap catMaybes . mapM mkNetDecl $ filter ((/= result) . fst) binders
          decls    <- concat <$> mapM (uncurry mkDeclarations) binders'
          Just (NetDecl' _ rw _ _) <- mkNetDecl . head $ filter ((==result) . fst) binders
          nm <- mkUniqueIdentifier Basic "fun"
          return (Right ((nm,netDecls ++ decls),rw))
        Nothing -> return (Right (("",[]),Wire))
      where
        -- TODO: check that it's okay to use `mkUnsafeSystemName`
        goR r id_ | id_ == r  = id_ {varName = mkUnsafeSystemName "~RESULT" 0}
                  | otherwise = id_

    go _ _ e' = error $ $(curLoc) ++ "Cannot make function input for: " ++ showPpr e'
