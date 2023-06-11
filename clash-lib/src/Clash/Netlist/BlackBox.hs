{-# LANGUAGE NamedFieldPuns #-}
{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017     , Google Inc.,
                    2021-2022, QBayLogic B.V.
                    2022     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Functions to create BlackBox Contexts and fill in BlackBox templates
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Netlist.BlackBox where

import           Control.Exception             (throw)
import           Control.Lens                  ((%=))
import qualified Control.Lens                  as Lens
import           Control.Monad                 (when, replicateM, zipWithM)
import           Control.Monad.Reader          (asks)
import           Control.Monad.Extra           (concatMapM)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Bifunctor                (first, second)
import           Data.Char                     (ord)
import           Data.Either                   (lefts, partitionEithers)
import           Data.Foldable                 (for_)
import qualified Data.HashMap.Lazy             as HashMap
import qualified Data.IntMap                   as IntMap
import           Data.List                     (elemIndex, partition)
import           Data.List.Extra               (countEq, mapAccumLM)
import           Data.Maybe                    (listToMaybe, fromJust, fromMaybe)
import           Data.Monoid                   (Ap(getAp))
import qualified Data.Set                      as Set
import           Data.Text.Lazy                (fromStrict)
import qualified Data.Text.Lazy                as Text
import           Data.Text                     (unpack)
import qualified Data.Text                     as TextS
import           Data.Text.Extra
import           GHC.Stack
  (HasCallStack, callStack, prettyCallStack)
import qualified System.Console.ANSI           as ANSI
import           System.Console.ANSI
  ( hSetSGR, SGR(SetConsoleIntensity, SetColor), Color(Magenta, Red)
  , ConsoleIntensity(BoldIntensity), ConsoleLayer(Foreground), ColorIntensity(Vivid))
import           System.IO
  (hPutStrLn, stderr, hFlush, hIsTerminalDevice)
#if MIN_VERSION_ghc(9,4,0)
import           GHC.Data.Bool                 (OverridingBool(..))
#elif MIN_VERSION_ghc(9,0,0)
import           GHC.Utils.Misc                (OverridingBool(..))
#else
import           Util                          (OverridingBool(..))
#endif

import           Clash.Annotations.Primitive
  ( PrimitiveGuard(HasBlackBox, DontTranslate)
  , PrimitiveWarning(WarnNonSynthesizable, WarnAlways)
  , extractPrim, HDL(VHDL))
import           Clash.Core.DataCon            as D (dcTag)
import           Clash.Core.FreeVars           (freeIds)
import           Clash.Core.HasType
import           Clash.Core.Literal            as L (Literal (..))
import           Clash.Core.Name
  (Name (..), mkUnsafeSystemName)
import qualified Clash.Netlist.Id              as Id
import           Clash.Core.Pretty             (showPpr)
import           Clash.Core.Subst              (extendIdSubst, mkSubst, substTm)
import           Clash.Core.Term               as C
  (IsMultiPrim (..), PrimInfo (..), Term (..), WorkInfo (..), collectArgs,
   collectArgsTicks, collectBndrs, mkApps, PrimUnfolding(..))
import           Clash.Core.TermInfo
import           Clash.Core.Type               as C
  (Type (..), ConstTy (..), TypeView (..), mkFunTy, splitFunTys, tyView)
import           Clash.Core.TyCon              as C (TyConMap, tyConDataCons)
import           Clash.Core.Util
  (inverseTopSortLetBindings, splitShouldSplit)
import           Clash.Core.Var                as V
  (Id, mkLocalId, modifyVarName)
import           Clash.Core.VarEnv
  (extendInScopeSet, mkInScopeSet, lookupVarEnv, uniqAway, unitVarSet)
import {-# SOURCE #-} Clash.Netlist
  (genComponent, mkDcApplication, mkDeclarations, mkExpr, mkNetDecl,
   mkProjection, mkSelection, mkFunApp, mkDeclarations')
import qualified Clash.Backend                 as Backend
import qualified Clash.Data.UniqMap as UniqMap
import           Clash.Debug                   (debugIsOn)
import           Clash.Driver.Types
  (ClashOpts(opt_primWarn, opt_color, opt_werror))
import           Clash.Netlist.BlackBox.Types  as B
import           Clash.Netlist.BlackBox.Util   as B
import           Clash.Netlist.Types           as N
import           Clash.Netlist.Util            as N
import           Clash.Normalize.Primitives    (removedArg)
import           Clash.Primitives.Types        as P
import qualified Clash.Primitives.Util         as P
import           Clash.Signal.Internal         (ActiveEdge (..))
import           Clash.Util
import qualified Clash.Util.Interpolate        as I

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

  case opt_werror opts of
    True -> do
      when useColor $ hSetSGR stderr [SetColor Foreground Vivid Red]
      throw (ClashException noSrcSpan msg Nothing)

    False -> do
      when useColor $ hSetSGR stderr [SetColor Foreground Vivid Magenta]
      hPutStrLn stderr $ "[WARNING] " ++ msg
      hSetSGR stderr [ANSI.Reset]
      hFlush stderr

-- | Generate the context for a BlackBox instantiation.
mkBlackBoxContext
  :: HasCallStack
  => TextS.Text
  -- ^ Blackbox function name
  -> [Id]
  -- ^ Identifiers binding the primitive/blackbox application
  -> [Either Term Type]
  -- ^ Arguments of the primitive/blackbox application
  -> NetlistMonad (BlackBoxContext,[Declaration])
mkBlackBoxContext bbName resIds args@(lefts -> termArgs) = do
    -- Make context inputs
    san <- asks _sanitizeNames
    let
      resNms = fmap (Id.unsafeFromCoreId san) resIds
      resNm = fromMaybe (error "mkBlackBoxContext: head") (listToMaybe resNms)
    resTys <- mapM (unsafeCoreTypeToHWTypeM' $(curLoc) . coreTypeOf) resIds
    (imps,impDecls) <- unzip <$> zipWithM (mkArgument bbName resNm) [0..] termArgs
    (funs,funDecls) <-
      mapAccumLM
        (addFunction (map coreTypeOf resIds))
        IntMap.empty
        (zip termArgs [0..])

    -- Make context result
    let ress = map (flip Identifier Nothing) resNms

    lvl <- Lens.use curBBlvl
    (nm,_) <- Lens.use curCompNm

    -- Set "context name" to value set by `Clash.Magic.setName`, default to the
    -- name of the closest binder
    ctxName1 <- fromMaybe (map Id.toText resNms) . fmap pure <$> Lens.view setName
    -- Update "context name" with prefixes and suffixes set by
    -- `Clash.Magic.prefixName` and `Clash.Magic.suffixName`
    ctxName2 <- mapM affixName ctxName1

    return ( Context bbName (zip ress resTys) imps funs [] lvl nm (listToMaybe ctxName2)
           , concat impDecls ++ concat funDecls
           )
  where
    addFunction resTys im (arg,i) = do
      tcm <- Lens.view tcCache
      if isFun tcm arg then do
        -- Only try to calculate function plurality when primitive actually
        -- exists. Here to prevent crashes on __INTERNAL__ primitives.
        prim <- HashMap.lookup bbName <$> Lens.view primitives
        funcPlurality <-
          case extractPrim <$> prim of
            Just (Just p) ->
              P.getFunctionPlurality p args resTys i
            _ ->
              pure 1

        curBBlvl Lens.+= 1
        (fs,ds) <- case resIds of
          (resId:_) -> unzip <$> replicateM funcPlurality (mkFunInput resId arg)
          _ -> error "internal error: insufficient resIds"
        curBBlvl Lens.-= 1

        let im' = IntMap.insert i fs im
        return (im', concat ds)
      else
        return (im, [])

prepareBlackBox
  :: TextS.Text
  -> BlackBox
  -> BlackBoxContext
  -> NetlistMonad (BlackBox,[Declaration])
prepareBlackBox _pNm templ bbCtx =
  case verifyBlackBoxContext bbCtx templ of
    Nothing -> do
      (t2,decls) <-
        onBlackBox
          (fmap (first BBTemplate) . setSym bbCtx)
          (\bbName bbHash bbFunc -> pure (BBFunction bbName bbHash bbFunc, []))
          templ
      for_ decls goDecl
      return (t2,decls)
    Just err0 -> do
      (_,sp) <- Lens.use curCompNm
      let err1 = concat [ "Couldn't instantiate blackbox for "
                        , Data.Text.unpack (bbName bbCtx), ". Verification "
                        , "procedure reported:\n\n" ++ err0 ]
      throw (ClashException sp ($(curLoc) ++ err1) Nothing)
 where
  -- Right now we assume that (1) a blackbox doesn't assign to a signal
  -- declared outside the black box template and (2) all uses of a signal
  -- within a blackbox are correct for the targeted HDL (i.e. we don't try
  -- to generate new signals when a signal is used incorrectly).
  goDecl = \case
    Assignment i u _ ->
      declareUse u i

    CondAssignment i _ _ _ _ -> do
      -- Currently, all CondAssignment get rendered as `always @*` blocks in
      -- (System)Verilog. This means when we target these HDL, this is _really_
      -- a blocking procedural assignment.
      SomeBackend b <- Lens.use backend
      let use = case Backend.hdlKind b of { VHDL -> Cont ; _ -> Proc Blocking }
      declareUse use i

    Seq seqs -> for_ seqs goSeq

    _ -> pure ()

  goSeq = \case
    AlwaysClocked _ _ seqs ->
      for_ seqs goSeq

    Initial seqs ->
      for_ seqs goSeq

    AlwaysComb seqs ->
      for_ seqs goSeq

    SeqDecl conc ->
      goDecl conc

    Branch _ _ alts ->
      let seqs = concatMap snd alts
       in for_ seqs goSeq

-- | Determine if a term represents a literal
isLiteral :: Term -> Bool
isLiteral e = case collectArgs e of
  (Data _, args)   -> all (either isLiteral (const True)) args
  (Prim _, args) -> all (either isLiteral (const True)) args
  (C.Literal _,_)  -> True
  _                -> False

mkArgument
  :: TextS.Text
  -- ^ Blackbox function name
  -> Identifier
  -- ^ LHS of the original let-binder. Is used as a name hint to generate new
  -- names in case the argument is a declaration.
  -> Int
  -- ^ Argument n (zero-indexed). Used for error message.
  -> Term
  -> NetlistMonad ( (Expr,HWType,Bool)
                  , [Declaration]
                  )
mkArgument bbName bndr nArg e = do
    tcm   <- Lens.view tcCache
    let ty = inferCoreTypeOf tcm e
    iw    <- Lens.view intWidth
    hwTyM <- fmap stripFiltered <$> N.termHWTypeM e
    let eTyMsg = "(" ++ showPpr e ++ " :: " ++ showPpr ty ++ ")"
    ((e',t,l),d) <- case hwTyM of
      Nothing
        | (Prim p,_) <- collectArgs e
        , primName p == showt 'removedArg
        -> return ((Identifier (Id.unsafeMake (showt 'removedArg)) Nothing, Void Nothing, False), [])
        | otherwise
        -> return ((error ($(curLoc) ++ "Forced to evaluate untranslatable type: " ++ eTyMsg), Void Nothing, False), [])
      Just hwTy -> case collectArgsTicks e of
        (C.Var v,[],_) -> do
          san <- asks _sanitizeNames
          return ((Identifier (Id.unsafeFromCoreId san v) Nothing,hwTy,False),[])
        (C.Literal (IntegerLiteral i),[],_) ->
          return ((N.Literal (Just (Signed iw,iw)) (N.NumLit i),hwTy,True),[])
        (C.Literal (IntLiteral i), [],_) ->
          return ((N.Literal (Just (Signed iw,iw)) (N.NumLit i),hwTy,True),[])
        (C.Literal (WordLiteral w), [],_) ->
          return ((N.Literal (Just (Unsigned iw,iw)) (N.NumLit w),hwTy,True),[])
        (C.Literal (CharLiteral c), [],_) ->
          return ((N.Literal (Just (Unsigned 21,21)) (N.NumLit . toInteger $ ord c),hwTy,True),[])
        (C.Literal (StringLiteral s),[],_) ->
          return ((N.Literal Nothing (N.StringLit s),hwTy,True),[])
        (C.Literal (Int64Literal i), [],_) ->
          return ((N.Literal (Just (Signed 64,64)) (N.NumLit i),hwTy,True),[])
        (C.Literal (Word64Literal i), [],_) ->
          return ((N.Literal (Just (Unsigned 64,64)) (N.NumLit i),hwTy,True),[])
#if MIN_VERSION_base(4,16,0)
        (C.Literal (Int8Literal i), [],_) ->
          return ((N.Literal (Just (Signed 8,8)) (N.NumLit i),hwTy,True),[])
        (C.Literal (Int16Literal i), [],_) ->
          return ((N.Literal (Just (Signed 16,16)) (N.NumLit i),hwTy,True),[])
        (C.Literal (Int32Literal i), [],_) ->
          return ((N.Literal (Just (Signed 16,16)) (N.NumLit i),hwTy,True),[])
        (C.Literal (Word8Literal i), [],_) ->
          return ((N.Literal (Just (Unsigned 8,8)) (N.NumLit i),hwTy,True),[])
        (C.Literal (Word16Literal i), [],_) ->
          return ((N.Literal (Just (Unsigned 16,16)) (N.NumLit i),hwTy,True),[])
        (C.Literal (Word32Literal i), [],_) ->
          return ((N.Literal (Just (Unsigned 32,32)) (N.NumLit i),hwTy,True),[])
#endif
        (C.Literal (NaturalLiteral n), [],_) ->
          return ((N.Literal (Just (Unsigned iw,iw)) (N.NumLit n),hwTy,True),[])
        (Prim pinfo,args,ticks) -> withTicks ticks $ \tickDecls -> do
          (e',d) <- mkPrimitive True False Concurrent (NetlistId bndr ty) pinfo args tickDecls
          case e' of
            (Identifier _ _) -> return ((e',hwTy,False), d)
            _                -> return ((e',hwTy,isLiteral e), d)
        (Data dc, args,_) -> do
          (exprN,dcDecls) <- mkDcApplication Concurrent [hwTy] (NetlistId bndr ty) dc (lefts args)
          return ((exprN,hwTy,isLiteral e),dcDecls)
        (Case scrut ty' [alt],[],_) -> do
          (projection,decls) <- mkProjection False (NetlistId bndr ty) scrut ty' alt
          return ((projection,hwTy,False),decls)
        (Let _bnds _term, [], _ticks) -> do
          (exprN, letDecls) <- mkExpr False Concurrent (NetlistId bndr ty) e
          return ((exprN,hwTy,False),letDecls)
        _ -> do
          let errMsg = [I.i|
            Forced to evaluate unexpected function argument:

              #{eTyMsg}

            in 'mkArgument' for argument #{nArg} of blackbox #{show bbName}.
          |]

          return ((Identifier (error ($(curLoc) ++ errMsg)) Nothing, hwTy, False), [])

    return ((e',t,l),d)

-- | Extract a compiled primitive from a guarded primitive. Emit a warning if
-- the guard wants to, or fail entirely.
extractPrimWarnOrFail
  :: HasCallStack
  => TextS.Text
  -- ^ Name of primitive
  -> NetlistMonad CompiledPrimitive
extractPrimWarnOrFail nm = do
  prim <- HashMap.lookup nm <$> Lens.view primitives
  case prim of
    Just (HasBlackBox warnings compiledPrim) ->
      -- See if we need to warn the user
      if null warnings then return compiledPrim else go warnings compiledPrim
    Just DontTranslate -> do
      -- We need to error because we encountered a primitive the user
      -- explicitly requested not to translate
      (_,sp) <- Lens.use curCompNm
      let msg = $(curLoc) ++ "Clash was forced to translate '" ++ unpack nm
             ++ "', but this value was marked with DontTranslate. Did you forget"
             ++ " to include a blackbox for one of the constructs using this?"
             ++ (if debugIsOn then "\n\n" ++ prettyCallStack callStack ++ "\n\n" else [])
      throw (ClashException sp msg Nothing)
    Nothing -> do
      -- Blackbox requested, but no blackbox found at all!
      (_,sp) <- Lens.use curCompNm
      let msg = $(curLoc) ++ "No blackbox found for: " ++ unpack nm
             ++ ". Did you forget to include directories containing "
             ++ "primitives? You can use '-i/my/prim/dir' to achieve this."
             ++ (if debugIsOn then "\n\n" ++ prettyCallStack callStack ++ "\n\n" else [])
      throw (ClashException sp msg Nothing)
 where
  go
    :: [PrimitiveWarning]
    -> CompiledPrimitive
    -> NetlistMonad CompiledPrimitive

  go ((WarnAlways warning):ws) cp = do
    opts <- Lens.view clashOpts
    let primWarn = opt_primWarn opts
    seen <- Set.member nm <$> Lens.use seenPrimitives

    when (primWarn && not seen)
      $ liftIO
      $ warn opts
      $ "Dubious primitive instantiation for "
     ++ unpack nm
     ++ ": "
     ++ warning
     ++ " (disable with -fclash-no-prim-warn)"

    go ws cp

  go ((WarnNonSynthesizable warning):ws) cp = do
    isTB <- Lens.use isTestBench
    if isTB then go ws cp else go ((WarnAlways warning):ws) cp

  go [] cp = do
    seenPrimitives %= Set.insert nm
    return cp

mkPrimitive
  :: Bool
  -- ^ Put BlackBox expression in parenthesis
  -> Bool
  -- ^ Treat BlackBox expression as declaration
  -> DeclarationType
  -- ^ Are we concurrent or sequential?
  -> NetlistId
  -- ^ Id to assign the result to
  -> PrimInfo
  -- ^ Primitive info
  -> [Either Term Type]
  -- ^ Arguments
  -> [Declaration]
  -- ^ Tick declarations
  -> NetlistMonad (Expr,[Declaration])
mkPrimitive bbEParen bbEasD declType dst pInfo args tickDecls =
  go =<< extractPrimWarnOrFail (primName pInfo)
  where
    tys = netlistTypes dst
    ty = fromMaybe (error "mkPrimitive") (listToMaybe tys)

    go
      :: CompiledPrimitive
      -> NetlistMonad (Expr, [Declaration])
    go =
      \case
        P.BlackBoxHaskell bbName wf _usedArgs multiResult funcName (_fHash, func) -> do
          bbFunRes <- func bbEasD (primName pInfo) args tys
          case bbFunRes of
            Left err -> do
              -- Blackbox template function returned an error:
              let err' = unwords [ $(curLoc) ++ "Could not create blackbox"
                                 , "template using", show funcName, "for"
                                 , show bbName ++ ".", "Function reported: \n\n"
                                 , err ]
              (_,sp) <- Lens.use curCompNm
              throw (ClashException sp err' Nothing)
            Right (BlackBoxMeta {..}, bbTemplate) ->
              -- Blackbox template generation successful. Rerun 'go', but this time
              -- around with a 'normal' @BlackBox@
              go (P.BlackBox
                    bbName wf bbRenderVoid multiResult bbKind () bbOutputUsage
                    bbLibrary bbImports bbFunctionPlurality bbIncludes
                    bbResultNames bbResultInits bbTemplate)
        -- See 'setupMultiResultPrim' in "Clash.Normalize.Transformations":
        P.BlackBox {name="c$multiPrimSelect"} ->
          pure (Noop, [])
        p@P.BlackBox {multiResult=True, name, template} -> do
          -- Multi result primitives assign their results to signals
          -- provided as arguments. Hence, we ignore any declarations
          -- from 'resBndr1'.
          tcm <- Lens.view tcCache
          let (args1, resArgs) = splitMultiPrimArgs (multiPrimInfo' tcm pInfo) args
          (bbCtx, ctxDcls) <- mkBlackBoxContext (primName pInfo) resArgs args1
          (templ, templDecl) <- prepareBlackBox name template bbCtx
          let bbDecl = N.BlackBoxD name (libraries p) (imports p) (includes p) templ bbCtx
          return (Noop, ctxDcls ++ templDecl ++ tickDecls ++ [bbDecl])
        p@(P.BlackBox {template, name=pNm, kind,outputUsage}) ->
          case kind of
            TDecl -> do
              resM <- resBndr1 True dst
              case resM of
                Just (dst',dstNm,dstDecl) -> do
                  (bbCtx,ctxDcls)   <- mkBlackBoxContext (primName pInfo) [dst'] args
                  (templ,templDecl) <- prepareBlackBox pNm template bbCtx
                  let bbDecl = N.BlackBoxD pNm (libraries p) (imports p)
                                           (includes p) templ bbCtx
                  declareUse outputUsage dstNm
                  return (Identifier dstNm Nothing,dstDecl ++ ctxDcls ++ templDecl ++ tickDecls ++ [bbDecl])

                -- Render declarations as a Noop when requested
                Nothing | RenderVoid <- renderVoid p -> do
                  -- TODO: We should probably 'mkBlackBoxContext' to accept empty lists
                  let dst1 = mkLocalId ty (mkUnsafeSystemName "__VOID_TDECL_NOOP__" 0)
                  (bbCtx,ctxDcls) <- mkBlackBoxContext (primName pInfo) [dst1] args
                  (templ,templDecl) <- prepareBlackBox pNm template bbCtx
                  let bbDecl = N.BlackBoxD pNm (libraries p) (imports p)
                                           (includes p) templ bbCtx
                  return (Noop, ctxDcls ++ templDecl ++ tickDecls ++ [bbDecl])

                -- Otherwise don't render them
                Nothing -> return (Noop,[])
            TExpr -> do
              if bbEasD
                then do
                  resM <- resBndr1 True dst
                  case resM of
                    Just (dst',dstNm,dstDecl) -> do
                      (bbCtx,ctxDcls) <- mkBlackBoxContext (primName pInfo) [dst'] args
                      (bbTempl,templDecl) <- prepareBlackBox pNm template bbCtx
                      let bbE =  BlackBoxE pNm (libraries p) (imports p) (includes p) bbTempl bbCtx bbEParen
                      tmpAssgn <- case declType of
                        Concurrent -> contAssign dstNm bbE
                        Sequential -> procAssign Blocking dstNm bbE
                      return (Identifier dstNm Nothing, dstDecl ++ ctxDcls ++ templDecl ++ [tmpAssgn])

                    -- Render expression as a Noop when requested
                    Nothing | RenderVoid <- renderVoid p -> do
                      -- TODO: We should probably 'mkBlackBoxContext' to accept empty lists
                      let dst1 = mkLocalId ty (mkUnsafeSystemName "__VOID_TEXPRD_NOOP__" 0)
                      (bbCtx,ctxDcls) <- mkBlackBoxContext (primName pInfo) [dst1] args
                      (templ,templDecl) <- prepareBlackBox pNm template bbCtx
                      let bbDecl = N.BlackBoxD pNm (libraries p) (imports p)
                                               (includes p) templ bbCtx
                      return (Noop, ctxDcls ++ templDecl ++ tickDecls ++ [bbDecl])

                    -- Otherwise don't render them
                    Nothing -> return (Identifier (Id.unsafeMake "__VOID_TEXPRD__") Nothing,[])
                else do
                  resM <- resBndr1 False dst
                  case resM of
                    Just (dst',_,_) -> do
                      (bbCtx,ctxDcls)      <- mkBlackBoxContext (primName pInfo) [dst'] args
                      (bbTempl,templDecl0) <- prepareBlackBox pNm template bbCtx
                      let templDecl1 = case primName pInfo of
                            "Clash.Sized.Internal.BitVector.fromInteger#"
                              | [N.Literal _ (NumLit _), N.Literal _ _, N.Literal _ _] <- extractLiterals bbCtx -> []
                            "Clash.Sized.Internal.BitVector.fromInteger##"
                              | [N.Literal _ _, N.Literal _ _] <- extractLiterals bbCtx -> []
                            "Clash.Sized.Internal.Index.fromInteger#"
                              | [N.Literal _ (NumLit _), N.Literal _ _] <- extractLiterals bbCtx -> []
                            "Clash.Sized.Internal.Signed.fromInteger#"
                              | [N.Literal _ (NumLit _), N.Literal _ _] <- extractLiterals bbCtx -> []
                            "Clash.Sized.Internal.Unsigned.fromInteger#"
                              | [N.Literal _ (NumLit _), N.Literal _ _] <- extractLiterals bbCtx -> []
                            _ -> templDecl0
                      return (BlackBoxE pNm (libraries p) (imports p) (includes p) bbTempl bbCtx bbEParen,ctxDcls ++ templDecl1)
                    -- Render expression as a Noop when requested
                    Nothing | RenderVoid <- renderVoid p -> do
                      -- TODO: We should probably 'mkBlackBoxContext' to accept empty lists
                      let dst1 = mkLocalId ty (mkUnsafeSystemName "__VOID_TEXPRE_NOOP__" 0)
                      (bbCtx,ctxDcls) <- mkBlackBoxContext (primName pInfo) [dst1] args
                      (templ,templDecl) <- prepareBlackBox pNm template bbCtx
                      let bbDecl = N.BlackBoxD pNm (libraries p) (imports p)
                                               (includes p) templ bbCtx
                      return (Noop, ctxDcls ++ templDecl ++ tickDecls ++ [bbDecl])

                    -- Otherwise don't render them
                    Nothing -> return (Identifier (Id.unsafeMake "__VOID__") Nothing,[])
        P.Primitive pNm _ _
          | pNm == "GHC.Prim.tagToEnum#" -> do
              hwTy <- N.unsafeCoreTypeToHWTypeM' $(curLoc) ty
              case args of
                [Right (ConstTy (TyCon tcN)), Left (C.Literal (IntLiteral i))] -> do
                  tcm <- Lens.view tcCache
                  let dcs = tyConDataCons (UniqMap.find tcN tcm)
                      dc  = dcs !! fromInteger i
                  (exprN,dcDecls) <- mkDcApplication declType [hwTy] dst dc []
                  return (exprN,dcDecls)
                [Right _, Left scrut] -> do
                  tcm     <- Lens.view tcCache
                  let scrutTy = inferCoreTypeOf tcm scrut
                  (scrutExpr,scrutDecls) <-
                    mkExpr False Concurrent (NetlistId (Id.unsafeMake "c$tte_rhs") scrutTy) scrut
                  case scrutExpr of
                    Identifier id_ Nothing -> return (DataTag hwTy (Left id_),scrutDecls)
                    _ -> do
                      scrutHTy <- unsafeCoreTypeToHWTypeM' $(curLoc) scrutTy
                      tmpRhs <- Id.make "c$tte_rhs"
                      let assignTy = case declType of { Concurrent -> Cont ; Sequential -> Proc Blocking }
                      netDecl <- N.mkInit declType assignTy tmpRhs scrutHTy scrutExpr
                      return (DataTag hwTy (Left tmpRhs), netDecl ++ scrutDecls)
                _ -> error $ $(curLoc) ++ "tagToEnum: " ++ show (map (either showPpr showPpr) args)
          | pNm == "GHC.Prim.dataToTag#" -> case args of
              [Right _,Left (Data dc)] -> do
                iw <- Lens.view intWidth
                return (N.Literal (Just (Signed iw,iw)) (NumLit $ toInteger $ dcTag dc - 1),[])
              [Right _,Left scrut] -> do
                tcm      <- Lens.view tcCache
                let scrutTy = inferCoreTypeOf tcm scrut
                scrutHTy <- unsafeCoreTypeToHWTypeM' $(curLoc) scrutTy
                (scrutExpr,scrutDecls) <-
                  mkExpr False Concurrent (NetlistId (Id.unsafeMake "c$dtt_rhs") scrutTy) scrut
                case scrutExpr of
                  Identifier id_ Nothing -> return (DataTag scrutHTy (Right id_),scrutDecls)
                  _ -> do
                    tmpRhs <- Id.make "c$dtt_rhs"
                    let assignTy = case declType of { Concurrent -> Cont ; Sequential -> Proc Blocking }
                    netDecl <- N.mkInit declType assignTy tmpRhs scrutHTy scrutExpr
                    return (DataTag scrutHTy (Right tmpRhs),netDecl ++ scrutDecls)
              _ -> error $ $(curLoc) ++ "dataToTag: " ++ show (map (either showPpr showPpr) args)

          | pNm == "Clash.Explicit.SimIO.mealyIO" -> do
              resM <- resBndr1 True dst
              case resM of
                Just (_,dstNm,dstDecl) -> do
                  tcm <- Lens.view tcCache
                  mealyDecls <- collectMealy dstNm dst tcm (lefts args)
                  return (Noop, dstDecl ++ mealyDecls)
                Nothing -> return (Noop,[])

          | pNm == "Clash.Explicit.SimIO.bindSimIO#" -> do
              (expr,decls) <- collectBindIO dst (lefts args)
              resM <- resBndr True dst
              case resM of
                Just (_,dstNms,dstDecl) -> case expr of
                  Noop ->
                    return (Noop,decls)
                  _ -> case dstNms of
                    [dstNm] -> do
                      declareUse (Proc Blocking) dstNm
                      return ( Identifier dstNm Nothing
                             , dstDecl ++ decls ++ [Assignment dstNm (Proc Blocking) expr])
                    _ -> error $ $(curLoc) ++ "bindSimIO: " ++ show resM
                _ ->
                  return (Noop,decls)

          | pNm == "Clash.Explicit.SimIO.apSimIO#" -> do
              collectAppIO dst (lefts args) []

          | pNm == "Clash.Explicit.SimIO.fmapSimIO#" -> do
              resM <- resBndr1 True dst
              case resM of
                Just (_,dstNm,dstDecl) -> case lefts args of
                  (fun0:arg0:_) -> do
                    tcm <- Lens.view tcCache
                    let arg1 = unSimIO tcm arg0
                        fun1 = case fun0 of
                          Lam b bE ->
                            let is0 = mkInScopeSet (Lens.foldMapOf freeIds unitVarSet fun0)
                                subst = extendIdSubst (mkSubst is0) b arg1
                            in  substTm "mkPrimitive.fmapSimIO" subst bE
                          _ -> mkApps fun0 [Left arg1]
                    (expr,bindDecls) <- mkExpr False Sequential dst fun1
                    assn <- case expr of
                              Noop -> pure []
                              _ -> do declareUse (Proc Blocking) dstNm
                                      pure [Assignment dstNm (Proc Blocking) expr]
                    return (Identifier dstNm Nothing, dstDecl ++ bindDecls ++ assn)
                  args1 -> error ("internal error: fmapSimIO# has insufficient arguments"
                                  <> showPpr args1)
                Nothing -> case lefts args of
                  (_:arg0:_) -> do
                    (_,bindDecls) <- mkExpr True Sequential dst arg0
                    return (Noop, bindDecls)
                  args1 -> error ("internal error: fmapSimIO# has insufficient arguments"
                                  <> showPpr args1)


          | pNm == "Clash.Explicit.SimIO.unSimIO#" ->
              case lefts args of
                (arg:_) -> mkExpr False Sequential dst arg
                _ -> error "internal error: insufficient arguments"

          | pNm == "Clash.Explicit.SimIO.pureSimIO#" -> do
              (expr,decls) <- case lefts args of
                (arg:_) -> mkExpr False Sequential dst arg
                _ -> error "internal error: insufficient arguments"
              resM <- resBndr True dst
              case resM of
                Just (_,dstNms,dstDecl) -> case expr of
                  Noop ->
                    return (Noop,decls)
                  _ -> case dstNms of
                    [dstNm] -> do
                      declareUse (Proc Blocking) dstNm
                      return ( Identifier dstNm Nothing
                             , dstDecl ++ decls ++ [Assignment dstNm (Proc Blocking) expr])
                    _ -> error "internal error"
                _ ->
                  return (Noop,decls)

          | pNm == "GHC.Num.Integer.IS" -> do
              (expr,decls) <- case lefts args of
                (arg:_) -> mkExpr False Concurrent dst arg
                _ -> error "internal error: insufficient arguments"
              iw <- Lens.view intWidth
              return (N.DataCon (Signed iw) (DC (Void Nothing,-1)) [expr],decls)

          | pNm == "GHC.Num.Integer.IP" -> do
              (expr,decls) <- case lefts args of
                (arg:_) -> mkExpr False Concurrent dst arg
                _ -> error "internal error: insufficient arguments"
              case expr of
                N.Literal Nothing (NumLit _) -> return (expr,decls)
                _ -> error "non-constant ByteArray# not supported"

          | pNm == "GHC.Num.Integer.IN" -> do
              (expr,decls) <- case lefts args of
                (arg:_) -> mkExpr False Concurrent dst arg
                _ -> error "internal error: insufficient arguments"
              case expr of
                N.Literal Nothing (NumLit i) ->
                  return (N.Literal Nothing (NumLit (negate i)),decls)
                _ -> error "non-constant ByteArray# not supported"

          | pNm == "GHC.Num.Natural.NS" -> do
              (expr,decls) <- case lefts args of
                (arg:_) -> mkExpr False Concurrent dst arg
                _ -> error "internal error: insufficient arguments"
              iw <- Lens.view intWidth
              return (N.DataCon (Unsigned iw) (DC (Void Nothing,-1)) [expr],decls)

          | pNm == "GHC.Num.Integer.NB" -> do
              (expr,decls) <- case lefts args of
                (arg:_) -> mkExpr False Concurrent dst arg
                _ -> error "internal error: insufficient arguments"
              case expr of
                N.Literal Nothing (NumLit _) -> return (expr,decls)
                _ -> error "non-constant ByteArray# not supported"

          | otherwise ->
              return (BlackBoxE "" [] [] []
                        (BBTemplate [Text $ mconcat ["NO_TRANSLATION_FOR:",fromStrict pNm]])
                        (emptyBBContext pNm) False,[])

    -- Do we need to create a new identifier to assign the result?
    --
    -- CoreId: No, this is an original LHS of a let-binder, and already has a
    --         corresponding NetDecl; unlike NetlistIds, it is not already
    --         assigned, it will be assigned by the BlackBox/Primitive.
    --
    -- NetlistId: This is a derived (either from an CoreId or other NetlistId)
    --            identifier created in the NetlistMonad that's already being
    --            used in an assignment, i.e. we cannot assign it again.
    --
    --            So if it is a declaration BlackBox (indicated by 'mkDec'),
    --            we will have to create a new NetlistId, create a NetDecl for
    --            it, and use this new NetlistId for the assignment inside the
    --            declaration BlackBox
    --
    -- MultiId: This is like a CoreId, but it's split over multiple identifiers
    --          because it was originally of a product type where the element
    --          types should not be part of an aggregate type in the generated
    --          HDL (e.g. Clocks should not be part of an aggregate, because
    --          tools like verilator don't like it)
    resBndr
      :: Bool
      -- Do we need to create and declare a new identifier in case we're given
      -- a NetlistId?
      -> NetlistId
      -- CoreId/NetlistId/MultiId
      -> NetlistMonad (Maybe ([Id],[Identifier],[Declaration]))
      -- Nothing when the binder would have type `Void`
    resBndr mkDec dst' = do
      resHwTy <- case tys of
        (ty1:_) -> unsafeCoreTypeToHWTypeM' $(curLoc) ty1
        _ -> error "internal error: insufficient types"
      san <- asks _sanitizeNames
      if isVoid resHwTy then
        pure Nothing
      else
        case dst' of
          NetlistId dstL ty' -> case mkDec of
            False -> do
              -- TODO: check that it's okay to use `mkUnsafeSystemName`
              let nm' = mkUnsafeSystemName (Id.toText dstL) 0
                  id_ = mkLocalId ty' nm'
              return (Just ([id_],[dstL],[]))
            True -> do
              nm2 <- Id.suffix dstL "res"
              -- TODO: check that it's okay to use `mkUnsafeInternalName`
              let nm3 = mkUnsafeSystemName (Id.toText nm2) 0
                  id_ = mkLocalId ty nm3

              idDeclM <- mkNetDecl (id_, mkApps (Prim pInfo) args)
              case idDeclM of
                [] -> return Nothing
                [idDecl] -> return (Just ([id_],[nm2],[idDecl]))
                ids -> error [I.i|
                  Unexpected nested use of multi result primitive. Ids:

                    #{show ids}

                  Multi primitive should only appear on the RHS of a
                  let-binding. Please report this as a bug.
                |]

          CoreId dstR ->
            return (Just ([dstR], [Id.unsafeFromCoreId san dstR], []))
          MultiId ids ->
            return (Just (ids, map (Id.unsafeFromCoreId san) ids, []))

    -- Like resBndr, but fails on MultiId
    resBndr1
      :: HasCallStack
      => Bool
      -> NetlistId
      -> NetlistMonad (Maybe (Id,Identifier,[Declaration]))
    resBndr1 mkDec dst' = resBndr mkDec dst' >>= \case
      Nothing -> pure Nothing
      Just ([id_],[nm_],decls) -> pure (Just (id_,nm_,decls))
      _ -> error "internal error"

-- | Turn a 'mealyIO' expression into a two sequential processes, one "initial"
-- process for the starting state, and one clocked sequential process.
collectMealy
  :: HasCallStack
  => Identifier
  -- ^ Identifier to assign the final result to
  -> NetlistId
  -- ^ Id to assign the final result to
  -> TyConMap
  -> [Term]
  -- ^ The arguments to 'mealyIO'
  -> NetlistMonad [Declaration]
collectMealy dstNm dst tcm (kd:clk:mealyFun:mealyInit:mealyIn:_) = do
  let (lefts -> args0,res0) = collectBndrs mealyFun
      is0 = mkInScopeSet (Lens.foldMapOf freeIds unitVarSet res0 <>
                          Lens.foldMapOf freeIds unitVarSet mealyInit <>
                          Lens.foldMapOf freeIds unitVarSet mealyIn)
      -- Given that we're creating a sequential list of statements from the
      -- let-bindings, make sure that everything is inverse topologically sorted
      (bs,res) = case res0 of
        Letrec bsU e | let bsN = inverseTopSortLetBindings bsU -> case e of
          C.Var resN -> (bsN,resN)
          _ ->
            let u = case dst of
                      CoreId u0 -> u0
                      _ -> uniqAway is0
                            (mkLocalId (inferCoreTypeOf tcm e)
                                        (mkUnsafeSystemName "mealyres" 0))
            in  (bsN ++ [(u,e)], u)
        e ->
          let u = case dst of
                    CoreId u0 -> u0
                    _ -> uniqAway is0
                           (mkLocalId (inferCoreTypeOf tcm e)
                                      (mkUnsafeSystemName "mealyres" 0))
          in  ([(u,e)], u)
#if __GLASGOW_HASKELL__ >= 900
      args1 = args0
#else
      -- Drop the 'State# World' argument
      args1 = init args0
#endif
      -- Take into account that the state argument is split over multiple
      -- binders because it contained types that are not allowed to occur in
      -- a HDL aggregate type
      mealyInitLength = length (splitShouldSplit tcm [inferCoreTypeOf tcm mealyInit])
      (sArgs,iArgs) = splitAt mealyInitLength args1
  -- Give all binders a unique name
  let sBindings = map (,mealyInit) sArgs ++ map (,mealyIn) iArgs ++ bs
  normE <- mkUniqueNormalized is0 Nothing ([], sBindings, res)
  case normE of
    -- We're not expecting any input or output wrappers
    (_,[],[],_,[],binders0,Just result) -> do
      let (sBinders,binders1) = splitAt (length sArgs) binders0
          (iBinders,binders2) = splitAt (length iArgs) binders1
          -- Get all the "original" let-bindings, without the above "mealyres".
          -- We don't want to make a NetDecl for it.
          bindersN = case res0 of
            Letrec _ (C.Var {}) -> binders2
            _                   -> init binders2

      -- Create new net declarations for state-binders, input-binders, and all
      -- the "original" let-bindings in 'mealyFun'
      --
      -- The first set is only assigned in the always block, so they must be
      -- 'reg' in Verilog terminology
      netDeclsSeq <- concatMapM mkNetDecl (sBinders ++ bindersN)
      -- The second set is assigned using concurrent assignment, so don't need
      -- to be 'reg'
      netDeclsInp <- concatMapM mkNetDecl iBinders

      -- If the 'mealyFun' was not a let-expression with a variable reference
      -- as a body then we used the LHS of the entire 'mealyIO' expression as
      -- a new let-binding; otherwise 'mkUniqueNormalized' would not work.
      --
      -- However, 'mkUniqueNormalized' made a new unique name for that LHS,
      -- which is not what we want. We want to assign the last expression to the
      -- LHS of 'mealyIO'.
      let bindersE = case res0 of
                        Letrec _ (C.Var {}) -> binders2
                        _ -> case dst of
                          -- See above why we do this.
                          CoreId u0 -> init binders2 ++ [(u0,snd (last binders2))]
                          _ -> binders2
      seqDecls <- concat <$> mapM (uncurry (mkDeclarations' Sequential)) bindersE

      -- When the body the let-expression of 'mealyFun' was variable reference,
      -- or in case we had to create a new identifier because the original LHS
      -- was not available: then we need to assign
      (resExpr,resDecls) <- case res0 of
        Letrec _ (C.Var {}) -> mkExpr False Concurrent dst (C.Var result)
        _ -> case dst of
          CoreId {} -> pure (Noop,[])
          _ -> mkExpr False Sequential dst (C.Var result)

      resAssn <- case resExpr of
            Noop -> pure []
            _ -> do
              assign <- SeqDecl <$> procAssign Blocking dstNm resExpr
              pure [Seq [AlwaysComb [assign]]]

      -- Create the declarations for the "initial state" block
      let sDst = case sBinders of
                   [] -> error "internal error: insufficient sBinders"
                   [(b,_)] -> CoreId b
                   _       -> MultiId (map fst sBinders)

      (exprInit,initDecls) <- mkExpr False Sequential sDst mealyInit

      initAssign <- case exprInit of
        Identifier _ Nothing -> pure []
        Noop -> pure []
        _ -> case sBinders of
          ((b,_):_) -> do san <- asks _sanitizeNames
                          assn <- procAssign Blocking (Id.unsafeFromCoreId san b) exprInit
                          pure [assn]
          _ -> error "internal error: insufficient sBinders"

      -- Create the declarations that corresponding to the input
      let iDst = case iBinders of
                   []      -> error "internal error: insufficient iBinders"
                   [(b,_)] -> CoreId b
                   _       -> MultiId (map fst iBinders)

      (exprArg,inpDeclsMisc) <- mkExpr False Concurrent iDst mealyIn

      argAssign <- case iBinders of
        ((i,_):_) -> do san <- asks _sanitizeNames
                        assn <- contAssign (Id.unsafeFromCoreId san i) exprArg
                        pure [assn]
        _ -> error "internal error: insufficient iBinders"

      -- Split netdecl declarations and other declarations
      let (netDeclsSeqMisc,seqDeclsOther) = partition isNet (seqDecls ++ resDecls)
          (netDeclsInit,initDeclsOther)   = partition isNet initDecls
      -- All assignments happens within a sequential block, so the nets need to
      -- be of type 'reg' in Verilog nomenclature
      let netDeclsSeq1 = netDeclsSeq ++ netDeclsSeqMisc ++ netDeclsInit

      -- We run mealy block in the opposite clock edge of the the ambient system
      -- because we're basically clocked logic; so we need to have our outputs
      -- ready before the ambient system starts sampling them. The clockGen code
      -- ensures that the "opposite" edge always comes first.
      kdTy <- unsafeCoreTypeToHWTypeM $(curLoc) (inferCoreTypeOf tcm kd)
      let edge = case stripVoid (stripFiltered kdTy) of
                   KnownDomain _ _ Rising _ _ _  -> Falling
                   KnownDomain _ _ Falling _ _ _ -> Rising
                   _ -> error "internal error"
      (clkExpr,clkDecls) <-
        mkExpr False Concurrent (NetlistId (Id.unsafeMake "__MEALY_CLK__") (inferCoreTypeOf tcm clk)) clk

      -- collect the declarations related to the input
      let netDeclsInp1 = netDeclsInp ++ inpDeclsMisc

      -- Collate everything
      return (clkDecls ++ netDeclsSeq1 ++ netDeclsInp1 ++ argAssign ++
                [ Seq [Initial (map SeqDecl (initDeclsOther ++ initAssign))]
                , Seq [AlwaysClocked edge clkExpr (map SeqDecl seqDeclsOther)]
                ] ++ resAssn)
    _ -> error "internal error"
 where
  isNet NetDecl' {} = True
  isNet _ = False

collectMealy _ _ _ _ = error "internal error"

-- | Collect the sequential declarations for 'bindIO'
collectBindIO :: NetlistId -> [Term] -> NetlistMonad (Expr,[Declaration])
#if __GLASGOW_HASKELL__ >= 900
collectBindIO dst (m:Lam x q@e:_) = do
#else
collectBindIO dst (m:Lam x q@(Lam _ e):_) = do
#endif
  tcm <- Lens.view tcCache
  (ds0,subst) <- collectAction tcm
  let qS = substTm "collectBindIO1" subst q
  case splitNormalized tcm qS of
    Right (args,bs0,res) -> do
      let bs = inverseTopSortLetBindings bs0
      let is0 = mkInScopeSet (Lens.foldMapOf freeIds unitVarSet qS)
      normE <- mkUniqueNormalized is0 Nothing (args,bs,res)
      case normE of
        (_,_,[],_,[],binders,Just result) -> do
          ds1 <- concatMapM (uncurry (mkDeclarations' Sequential)) binders
          netDecls <- concatMapM mkNetDecl binders
          san <- asks _sanitizeNames
          return (Identifier (Id.unsafeFromCoreId san result) Nothing, netDecls ++ ds0 ++ ds1)
        _ -> error "internal error"
    _ -> case substTm "collectBindIO2" subst e of
      Letrec {} -> error "internal error"
      (collectArgs -> (Prim p,args))
        | primName p == "Clash.Explicit.SimIO.bindSimIO#" -> do
            (expr,ds1) <- collectBindIO dst (lefts args)
            return (expr, ds0 ++ ds1)
      eS -> do
        (expr,ds1) <- mkExpr False Sequential dst eS
        return (expr, ds0 ++ ds1)
 where
  collectAction tcm = case splitNormalized tcm m of
    Right (args,bs0,res) -> do
      let bs = inverseTopSortLetBindings bs0
      let is0 = mkInScopeSet (Lens.foldMapOf freeIds unitVarSet m)
      normE <- mkUniqueNormalized is0 Nothing (args,(x,m):bs,res)
      case normE of
        (_,_,[],_,[],binders@(b:_),Just result) -> do
          let binders1 = tail binders ++ [(fst b, C.Var result)]
          ds1 <- concatMapM (uncurry (mkDeclarations' Sequential)) binders1
          netDecls <- concatMapM mkNetDecl binders
          return (netDecls ++ ds1,extendIdSubst (mkSubst eInScopeSet) x (Var (fst b)))
        _ -> error "internal error"
    _ -> do
      ([x'],s) <- mkUnique (mkSubst eInScopeSet) [x]
      netDecls <- concatMapM mkNetDecl [(x',m)]
      ds1 <- mkDeclarations' Sequential x' m
      return (netDecls ++ ds1,s)

  eInScopeSet = mkInScopeSet (Lens.foldMapOf freeIds unitVarSet e)

collectBindIO _ es = error ("internal error:\n" ++ showPpr es)

-- | Collect the sequential declarations for 'appIO'
collectAppIO :: NetlistId -> [Term] -> [Term] -> NetlistMonad (Expr,[Declaration])
collectAppIO dst (fun1:arg1:_) rest = case collectArgs fun1 of
  (Prim (PrimInfo "Clash.Explicit.SimIO.fmapSimIO#" _ _ _ _),(lefts -> (fun0:arg0:_))) -> do
    tcm <- Lens.view tcCache
    let argN = map (Left . unSimIO tcm) (arg0:arg1:rest)
    mkExpr False Sequential dst (mkApps fun0 argN)
  (Prim (PrimInfo "Clash.Explicit.SimIO.apSimIO#" _ _ _ _),(lefts -> args)) -> do
    collectAppIO dst args (arg1:rest)
  _ -> error ("internal error:\n" ++ showPpr (fun1:arg1:rest))


collectAppIO _ es _ = error ("internal error:\n" ++ showPpr es)

-- | Unwrap the new-type wrapper for things of type SimIO, this is needed to
-- allow applications of the `State# World` token to the underlying IO type.
--
-- XXX: this is most likely needed because Ghc2Core that threw away the cast
-- that this unwrapping; we should really start to support casts.
unSimIO
  :: TyConMap
  -> Term
  -> Term
unSimIO tcm arg =
  let argTy = inferCoreTypeOf tcm arg
  in  case tyView argTy of
        TyConApp _ [tcArg] ->
          mkApps (Prim (PrimInfo
                          "Clash.Explicit.SimIO.unSimIO#"
                          (mkFunTy argTy tcArg)
                          WorkNever
                          SingleResult
                          NoUnfolding))
                 [Left arg]
        _ -> error ("internal error:\n" ++ showPpr arg)

-- | Create an template instantiation text and a partial blackbox content for an
-- argument term, given that the term is a function. Errors if the term is not
-- a function
mkFunInput
  :: HasCallStack
  => Id
  -- ^ Identifier binding the encompassing primitive/blackbox application. Used
  -- as a name hint if 'mkFunInput' needs intermediate signals.
  -> Term
  -- ^ The function argument term
  -> NetlistMonad
      ((Either BlackBox (Identifier,[Declaration])
       ,Usage
       ,[BlackBoxTemplate]
       ,[BlackBoxTemplate]
       ,[((TextS.Text,TextS.Text),BlackBox)]
       ,BlackBoxContext)
      ,[Declaration])
mkFunInput resId e =
 let (appE,args,ticks) = collectArgsTicks e
 in  withTicks ticks $ \tickDecls -> do
  tcm <- Lens.view tcCache
  -- TODO: Rewrite this function to use blackbox functions. Right now it
  -- TODO: generates strings that are later parsed/interpreted again. Silly!
  templ <- case appE of
            Prim p -> do
              bb  <- extractPrimWarnOrFail (primName p)
              case bb of
                P.BlackBox {..} ->
                  pure (Left (kind,outputUsage,libraries,imports,includes,primName p,template))
                P.Primitive pn _ pt ->
                  error $ $(curLoc) ++ "Unexpected blackbox type: "
                                    ++ "Primitive " ++ show pn
                                    ++ " " ++ show pt
                P.BlackBoxHaskell{name=pName, multiResult=True} ->
                  -- TODO: dev pointers
                  error [I.i|
                    Encountered multiresult primitive as a direct argument to
                    another primitive. This should not happen.

                      Encountered: #{pName}

                    Please report this as an issue.
                  |]
                P.BlackBoxHaskell{name=pName, functionName=fName, function=(_, func)} -> do
                  -- Determine result type of this blackbox. If it's not a
                  -- function, simply use its term type.
                  let (_, resTy) = splitFunTys tcm (inferCoreTypeOf tcm e)
                  bbhRes <- func True pName args [resTy]
                  case bbhRes of
                    Left err ->
                      error $ $(curLoc) ++ show fName ++ " yielded an error: "
                                        ++ err
                    Right (BlackBoxMeta{..}, template) ->
                      pure $
                        Left ( bbKind, bbOutputUsage, bbLibrary, bbImports
                             , bbIncludes, pName, template)
            Data dc -> do
              let eTy = inferCoreTypeOf tcm e
                  (_,resTy) = splitFunTys tcm eTy

              resHTyM0 <- coreTypeToHWTypeM resTy
              let resHTyM1 = (\fHwty -> (stripFiltered fHwty, flattenFiltered fHwty)) <$> resHTyM0

              case resHTyM1 of
                -- Special case where coreTypeToHWTypeM determined a type to
                -- be completely transparent.
                Just (_resHTy, [areVoids@(countEq False -> 1)]) -> do
                  let nonVoidArgI = fromJust (elemIndex False areVoids)
                  let arg = Id.unsafeMake (TextS.concat ["~ARG[", showt nonVoidArgI, "]"])
                  let assign = Assignment (Id.unsafeMake "~RESULT") Cont (Identifier arg Nothing)
                  return (Right ((Id.unsafeMake "", tickDecls ++ [assign]), Cont))

                -- Because we filter void constructs, the argument indices and
                -- the field indices don't necessarily correspond anymore. We
                -- use the result of coreTypeToHWTypeM to figure out what the
                -- original indices are. Please see the documentation in
                -- Clash.Netlist.Util.mkADT for more information.
                Just (resHTy@(SP _ _), areVoids0) -> do
                  let
                      dcI       = dcTag dc - 1
                      areVoids1 = indexNote ($(curLoc) ++ "No areVoids with index: " ++ show dcI) areVoids0 dcI
                      mkArg i   = Id.unsafeMake ("~ARG[" <> showt i <> "]")
                      dcInps    = [Identifier (mkArg x) Nothing | x <- originalIndices areVoids1]
                      dcApp     = DataCon resHTy (DC (resHTy,dcI)) dcInps
                      dcAss     = Assignment (Id.unsafeMake "~RESULT") Cont dcApp
                  return (Right ((Id.unsafeMake "",tickDecls ++ [dcAss]), Cont))

                -- CustomSP the same as SP, but with a user-defined bit
                -- level representation
                Just (resHTy@(CustomSP {}), areVoids0) -> do
                  let
                      dcI       = dcTag dc - 1
                      areVoids1 = indexNote ($(curLoc) ++ "No areVoids with index: " ++ show dcI) areVoids0 dcI
                      mkArg i   = Id.unsafeMake ("~ARG[" <> showt i <> "]")
                      dcInps    = [Identifier (mkArg x) Nothing | x <- originalIndices areVoids1]
                      dcApp     = DataCon resHTy (DC (resHTy,dcI)) dcInps
                      dcAss     = Assignment (Id.unsafeMake "~RESULT") Cont dcApp
                  return (Right ((Id.unsafeMake "",tickDecls ++ [dcAss]), Cont))

                -- Like SP, we have to retrieve the index BEFORE filtering voids
                Just (resHTy@(Product _ _ _), areVoids1:_) -> do
                  let mkArg i    = Id.unsafeMake ("~ARG[" <> showt i <> "]")
                      dcInps    = [ Identifier (mkArg x) Nothing | x <- originalIndices areVoids1]
                      dcApp     = DataCon resHTy (DC (resHTy,0)) dcInps
                      dcAss     = Assignment (Id.unsafeMake "~RESULT") Cont dcApp
                  return (Right ((Id.unsafeMake "",tickDecls ++ [dcAss]), Cont))

                -- Vectors never have defined areVoids (or all set to False), as
                -- it would be converted to Void otherwise. We can therefore
                -- safely ignore it:
                Just (resHTy@(Vector _ _), _areVoids) -> do
                  let mkArg i = Id.unsafeMake ("~ARG[" <> showt i <> "]")
                      dcInps = [ Identifier (mkArg x) Nothing | x <- [(1::Int)..2] ]
                      dcApp  = DataCon resHTy (DC (resHTy,1)) dcInps
                      dcAss  = Assignment (Id.unsafeMake "~RESULT") Cont dcApp
                  return (Right ((Id.unsafeMake "",tickDecls ++ [dcAss]), Cont))

                -- Sum types OR a Sum type after filtering empty types:
                Just (resHTy@(Sum _ _), _areVoids) -> do
                  let dcI   = dcTag dc - 1
                      dcApp = DataCon resHTy (DC (resHTy,dcI)) []
                      dcAss = Assignment (Id.unsafeMake "~RESULT") Cont dcApp
                  return (Right ((Id.unsafeMake "",tickDecls ++ [dcAss]), Cont))

                -- Same as Sum, but with user defined bit level representation
                Just (resHTy@(CustomSum {}), _areVoids) -> do
                  let dcI   = dcTag dc - 1
                      dcApp = DataCon resHTy (DC (resHTy,dcI)) []
                      dcAss = Assignment (Id.unsafeMake "~RESULT") Cont dcApp
                  return (Right ((Id.unsafeMake "",tickDecls ++ [dcAss]), Cont))

                Just (Void {}, _areVoids) ->
                  return (error $ $(curLoc) ++ "Encountered Void in mkFunInput."
                                            ++ " This is a bug in Clash.")

                _ -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showPpr e
            C.Var fun -> do
              topAnns <- Lens.use topEntityAnns
              case lookupVarEnv fun topAnns of
                Just _ ->
                  error $ $(curLoc) ++ "Cannot make function input for partially applied Synthesize-annotated: " ++ showPpr e
                _ -> do
                  normalized <- Lens.use bindings
                  case lookupVarEnv fun normalized of
                    Just _ -> do
                      (meta,N.Component compName compInps [(_,compOutp,_)] _) <-
                        preserveVarEnv $ genComponent fun

                      let
                        ComponentMeta{cmWereVoids} = meta
                        inpAssign (i, t) e' = (Identifier i Nothing, In, t, e')
                        inpVar i = Id.unsafeMake ("~VAR[arg" <> showt i <> "][" <> showt i <> "]")
                        inpVars = [Identifier (inpVar i)  Nothing | i <- originalIndices cmWereVoids]
                        inpAssigns = zipWith inpAssign compInps inpVars
                        outpAssign =
                          ( Identifier (fst compOutp) Nothing
                          , Out
                          , snd compOutp
                          , Identifier (Id.unsafeMake "~RESULT") Nothing )
                      instLabel <- Id.next compName
                      let
                        portMap = NamedPortMap (outpAssign:inpAssigns)
                        instDecl = InstDecl Entity Nothing [] compName instLabel [] portMap
                      return (Right ((Id.unsafeMake "",tickDecls ++ [instDecl]), Cont))
                    Nothing -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showPpr e
            C.Lam {} -> do
              let is0 = mkInScopeSet (Lens.foldMapOf freeIds unitVarSet appE)
              either Left (Right . first (second (tickDecls ++))) <$> go is0 0 appE
            _ -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showPpr e
  let pNm = case appE of
              Prim p -> primName p
              _ -> "__INTERNAL__"
  (bbCtx,dcls) <- mkBlackBoxContext pNm [resId] args
  case templ of
    Left (TDecl,outputUsage,libs,imps,inc,_,templ') -> do
      (l',templDecl)
        <- onBlackBox
            (fmap (first BBTemplate) . setSym bbCtx)
            (\bbName bbHash bbFunc -> pure $ (BBFunction bbName bbHash bbFunc, []))
            templ'
      return ((Left l',outputUsage,libs,imps,inc,bbCtx),dcls ++ templDecl)
    Left (TExpr,_,libs,imps,inc,nm,templ') -> do
      onBlackBox
        (\t -> do t' <- getAp (prettyBlackBox t)
                  let t'' = Id.unsafeMake (Text.toStrict t')
                      assn = Assignment (Id.unsafeMake "~RESULT") Cont (Identifier t'' Nothing)
                  return ((Right (Id.unsafeMake "",[assn]),Cont,libs,imps,inc,bbCtx),dcls))
        (\bbName bbHash (TemplateFunction k g _) -> do
          let f' bbCtx' = do
                let assn = Assignment (Id.unsafeMake "~RESULT") Cont
                            (BlackBoxE nm libs imps inc templ' bbCtx' False)
                p <- getAp (Backend.blockDecl (Id.unsafeMake "") [assn])
                return p
          return ((Left (BBFunction bbName bbHash (TemplateFunction k g f'))
                  ,Cont
                  ,[]
                  ,[]
                  ,[]
                  ,bbCtx
                  )
                 ,dcls
                 )
        )
        templ'
    Right (decl,u) ->
      return ((Right decl,u,[],[],[],bbCtx),dcls)
  where
    goExpr app@(collectArgsTicks -> (C.Var fun,args@(_:_),ticks)) = do
      tcm <- Lens.view tcCache
      resTy <- unsafeCoreTypeToHWTypeM' $(curLoc) (inferCoreTypeOf tcm app)
      let (tmArgs,tyArgs) = partitionEithers args
      if null tyArgs
        then
          withTicks ticks $ \tickDecls -> do
            resNm <- Id.make "result"
            appDecls <- mkFunApp resNm fun tmArgs tickDecls
            let assn = [ Assignment (Id.unsafeMake "~RESULT") Cont (Identifier resNm Nothing)
                       , NetDecl Nothing resNm resTy ]
            nm <- Id.makeBasic "block"
            return (Right ((nm,assn++appDecls), Cont))
        else do
          (_,sp) <- Lens.use curCompNm
          throw (ClashException sp ($(curLoc) ++ "Not in normal form: Var-application with Type arguments:\n\n" ++ showPpr app) Nothing)
    goExpr e' = do
      tcm <- Lens.view tcCache
      let eType = inferCoreTypeOf tcm e'
      (appExpr,appDecls) <- mkExpr False Concurrent (NetlistId (Id.unsafeMake "c$bb_res") eType) e'
      let assn = Assignment (Id.unsafeMake "~RESULT") Cont appExpr
      nm <- if null appDecls
               then return (Id.unsafeMake "")
               else Id.makeBasic "block"
      return (Right ((nm,appDecls ++ [assn]), Cont))

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
      san <- asks _sanitizeNames
      let assn = Assignment (Id.unsafeMake "~RESULT") Cont (Identifier (Id.unsafeFromCoreId san v) Nothing)
      return (Right ((Id.unsafeMake "",[assn]), Cont))

    go _ _ (Case scrut ty [alt]) = do
      tcm <- Lens.view tcCache
      let sTy = inferCoreTypeOf tcm scrut
      (projection,decls) <- mkProjection False (NetlistId (Id.unsafeMake "c$bb_res") sTy) scrut ty alt
      let assn = Assignment (Id.unsafeMake "~RESULT") Cont projection
      nm <- if null decls
               then return (Id.unsafeMake "")
               else Id.makeBasic "projection"
      return (Right ((nm,decls ++ [assn]), Cont))

    go _ _ (Case scrut ty alts@(_:_:_)) = do
      resNm <- Id.make "result"
      resTy <- unsafeCoreTypeToHWTypeM' $(curLoc) ty
      -- It's safe to use 'mkUnsafeSystemName' here: only the name, not the
      -- unique, will be used
      let resId'  = NetlistId resNm ty
      selectionDecls <- mkSelection Concurrent resId' scrut ty alts []
      let assn = [ NetDecl' Nothing resNm resTy Nothing
                 , Assignment (Id.unsafeMake "~RESULT") Cont (Identifier resNm Nothing) ]
      nm <- Id.makeBasic "selection"
      return (Right ((nm,assn++selectionDecls), Cont))

    go is0 _ e'@(Let{}) = do
      tcm <- Lens.view tcCache
      let normE = splitNormalized tcm e'
      (_,[],[],_,[],binders,resultM) <- case normE of
        Right norm -> mkUniqueNormalized is0 Nothing norm
        Left err -> error err
      case resultM of
        Just result -> do
          -- TODO: figure out what to do with multires blackboxes here
          netDecls <- concatMapM mkNetDecl $ binders
          decls    <- concatMapM (uncurry mkDeclarations) binders
          nm <- Id.makeBasic "fun"
          san <- asks _sanitizeNames
          let resultId = Id.unsafeFromCoreId san result
          -- TODO: Due to reasons lost in the mists of time, #1265 creates an
          -- assignement here, whereas it previously wouldn't. With the PR in
          -- tests break when reverting to the old behavior. In some cases this
          -- creates "useless" assignments. We should investigate whether we can
          -- get the old behavior back.
          let resDecl = Assignment (Id.unsafeMake "~RESULT") Cont (Identifier resultId Nothing)
          return (Right ((nm,resDecl:netDecls ++ decls), Cont))
        Nothing -> return (Right ((Id.unsafeMake "",[]), Cont))

    go is0 n (Tick _ e') = go is0 n e'

    go _ _ e'@(App {}) = goExpr e'
    go _ _ e'@(C.Data {}) = goExpr e'
    go _ _ e'@(C.Literal {}) = goExpr e'
    go _ _ e'@(Cast {}) = goExpr e'
    go _ _ e'@(Prim {}) = goExpr e'
    go _ _ e'@(TyApp {}) = goExpr e'

    go _ _ e'@(Case _ _ []) =
      error $ $(curLoc) ++ "Cannot make function input for case without alternatives: " ++ show e'

    go _ _ e'@(TyLam {}) =
      error $ $(curLoc) ++ "Cannot make function input for TyLam: " ++ show e'
