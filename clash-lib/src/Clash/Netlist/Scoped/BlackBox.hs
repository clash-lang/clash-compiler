{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017     , Google Inc.,
                    2021-2026, QBayLogic B.V.
                    2022     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Blackboxes of Netlist generation on scoped Clash Core: creating blackbox
  contexts, and filling in the templates of primitives.

  The handlers get the t'CompCtx' of the terms they are given, which gives
  the HDL identifiers of their local variables. Where they go under the
  binders of a term, e.g. those of the lambdas of a function argument of a
  blackbox ('mkFunInput') or the let-bindings of a 'mealyIO' machine, these
  handlers open the binders, and extend the context with the HDL identifiers
  of the variables they bind.

  The result of a blackbox is not always a local variable: it can be a signal
  that Netlist generation made up. Internally, a result is therefore given by
  its HDL identifier and its type ('ResId').
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- 'dataToTagSmall#' and 'dataToTagLarge#' (used below in TH name quotes so that
-- Clash can match the primops by their canonical names across GHC versions) are
-- deprecated as of GHC 9.14 in favor of @dataToTag#@ from @GHC.Magic@.
{-# OPTIONS_GHC -Wno-deprecations #-}

module Clash.Netlist.Scoped.BlackBox
  ( mkBlackBoxContext
  , mkPrimitive
  , extractPrimWarnOrFail
  , prepareBlackBox
  ) where

import           Control.Applicative           ((<|>))
import           Control.Exception             (throw)
import           Control.Lens                  ((%=))
import qualified Control.Lens                  as Lens
import           Control.Monad                 (replicateM, unless, zipWithM)
import           Control.Monad.Extra           (concatMapM)
import           Data.Bifunctor                (first, second)
import           Data.Either                   (lefts, partitionEithers)
import qualified Data.HashMap.Lazy             as HashMap
import qualified Data.IntMap                   as IntMap
import           Data.List.Extra               (countEq, mapAccumLM)
import           Data.List.NonEmpty            (NonEmpty (..))
import           Data.List                     (elemIndex, partition)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromJust, fromMaybe, listToMaybe)
import           Data.Monoid                   (Ap (getAp))
import           Data.Text                     (Text, unpack)
import qualified Data.Text                     as TextS
import qualified Data.Text.Lazy                as Text
import           Data.Text.Extra               (showt)
import           GHC.Num.Integer               (Integer (..))
import           GHC.Num.Natural               (Natural (..))
import qualified GHC.Prim
import           GHC.Stack                     (HasCallStack, callStack, prettyCallStack)

import Clash.Core.Basic (ConstTy (..), IsMultiPrim (..), WorkInfo (..))
import           Clash.Annotations.Primitive
  (HDL (VHDL), PrimitiveGuard (..), PrimitiveWarning (..), extractPrim)
import qualified Clash.Backend                 as Backend
import           Clash.Core.DataCon            (dcTag)
import           Clash.Core.Literal            (Literal (IntLiteral))
import qualified Clash.Core.Name               as C
import           Clash.Core.Pretty             (showPpr)
import           Clash.Core.TyCon              (tyConDataCons)
import           Clash.Core.Scoped.TysPrim (undefinedTy)
import qualified Clash.Data.UniqMap            as UniqMap
import           Clash.Netlist.BlackBox.Types
  (BlackBoxMeta (..), BlackBoxTemplate, Element (Text), RenderVoid (..),
   TemplateKind (..))
import           Clash.Netlist.BlackBox.Util
  (extractLiterals, onBlackBox, prettyBlackBox, setSym, verifyBlackBoxContext)
import qualified Clash.Netlist.Id              as Id
import           Clash.Netlist.Types           as N
  hiding (Literal)
import qualified Clash.Netlist.Types           as N (Expr (Literal))
import           Clash.Netlist.Util
  (affixName, condAssign, contAssign, declareUse, flattenFiltered, isVoid, mkInit,
   mkLiteral, originalIndices, preserveVarEnv, procAssign,
   stripFiltered, stripVoid)
import           Clash.Normalize.Scoped.Primitives    (removedArg)
import qualified Clash.Primitives.Types        as P
import           Clash.Primitives.Util  (getFunctionPlurality)
import           Clash.Signal.Internal         (ActiveEdge (..))
import           Clash.Util                    (ClashException (..), curLoc, indexNote)
import qualified Clash.Util.Interpolate        as I
import qualified Clash.Warning                 as W
import           Clash.Debug                   (debugIsOn)
import           Clash.Driver.Warning          (warnAboutM)
import           Data.Foldable                 (for_)
import qualified Data.Set                      as Set

import           Clash.Core.Scoped.FreeVars    (tmNameOccursIn)
import           Clash.Core.Scoped.HasType     (inferCoreTypeOf)
import           Clash.Core.Scoped.NameMap     (lookupGlobalEnv)
import           Clash.Core.Scoped.Subst
  (extendTmSubst, idSubst, openBind, openIdBinder, openTyBinder, substTm,
   teleIdVars, withFreshIdBinder)
import           Clash.Core.Scoped.Syntax
import           Clash.Core.Scoped.Term
  (collectArgs, collectArgsTicks, collectTicks, mkApps, mkTicks, stripTicks)
import           Clash.Core.Scoped.TermInfo
  (isFun, isLiteral, multiPrimInfo', splitMultiPrimArgs)
import           Clash.Core.Scoped.Type        (TypeView (..), mkFunTy, splitFunTys, tyView)
import           Clash.Core.Scoped.Util
  (inverseTopSortOrderOf, primAtType, splitShouldSplit)

import {-# SOURCE #-} Clash.Netlist.Scoped
  (genComponent, mkDcApplication, mkDeclarations, mkDeclarations', mkExpr,
   mkFunApp, mkNetDecl, mkProjection, mkSelection)
import           Clash.Netlist.Scoped.Types
import           Clash.Netlist.Scoped.Util
  (coreTypeToHWTypeM, globalIdentifier, idToOutPort, localIdText, primWarnContext, renameBinder, uniqueIdentifier,
   unsafeCoreTypeToHWTypeM, unsafeCoreTypeToHWTypeM', withTicks)

-- | The result of a blackbox: its HDL identifier and its type. See the module
-- header.
type ResId n = (Identifier, Type n)

-- | An identifier as the identifier of a result ('ResId'), or of a local
-- variable: made from its text alone ('Clash.Netlist.Id.unsafeMake'), so that
-- it loses its structure, which e.g. 'Id.suffix' uses to make new identifiers.
rawIdentifier :: Identifier -> Identifier
rawIdentifier = Id.unsafeMake . Id.toText

-- | The result of a blackbox that is a local variable
localResId :: HasCallStack => CompCtx n -> LocalId n -> ResId n
localResId ctx v = (localIdentifier ctx (localIdName v), localIdType v)

-- | Generate the context for a BlackBox instantiation.
mkBlackBoxContext
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> Text
  -- ^ Blackbox function name
  -> DeclarationType
  -- ^ Are we concurrent or sequential?
  -> [LocalId n]
  -- ^ Identifiers binding the primitive/blackbox application
  -> [Either (Term n) (Type n)]
  -- ^ Arguments of the primitive/blackbox application
  -> NetlistMonad (BlackBoxContext,[Declaration])
mkBlackBoxContext ctx bbName declType resIds =
  mkBlackBoxContextRes ctx bbName declType (map (localResId ctx) resIds)

-- | 'mkBlackBoxContext' for results that are not necessarily local variables
mkBlackBoxContextRes
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> Text
  -- ^ Blackbox function name
  -> DeclarationType
  -- ^ Are we concurrent or sequential?
  -> [ResId n]
  -- ^ The results of the primitive/blackbox application
  -> [Either (Term n) (Type n)]
  -- ^ Arguments of the primitive/blackbox application
  -> NetlistMonad (BlackBoxContext,[Declaration])
mkBlackBoxContextRes ctx bbName declType resIds args@(lefts -> termArgs) = do
    -- Make context inputs
    let
      resNms = map fst resIds
      resNm = fromMaybe (error "mkBlackBoxContext: head") (listToMaybe resNms)
    resTys <- mapM (unsafeCoreTypeToHWTypeM' $(curLoc) (compScope ctx) . snd) resIds
    (imps,impDecls) <- unzip <$> zipWithM (mkArgument ctx bbName resNm declType) [0..] termArgs
    (funs,funDecls) <-
      mapAccumLM
        (addFunction (map snd resIds))
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
      if isFun (compTypeEnv ctx) (compScope ctx) arg then do
        -- Only try to calculate function plurality when primitive actually
        -- exists. Here to prevent crashes on __INTERNAL__ primitives.
        prim <- HashMap.lookup bbName <$> Lens.view primitives
        funcPlurality <-
          case extractPrim <$> prim of
            Just (Just p) ->
              getFunctionPlurality ctx p args resTys i
            _ ->
              pure 1

        curBBlvl Lens.+= 1
        (fs,ds) <- case resIds of
          (resId:_) -> unzip <$> replicateM funcPlurality (mkFunInput ctx bbName declType resId arg)
          _ -> error "internal error: insufficient resIds"
        curBBlvl Lens.-= 1

        let im' = IntMap.insert i fs im
        return (im', concat ds)
      else
        return (im, [])

mkArgument
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> Text
  -- ^ Blackbox function name
  -> Identifier
  -- ^ LHS of the original let-binder. Is used as a name hint to generate new
  -- names in case the argument is a declaration.
  -> DeclarationType
  -- ^ Are we concurrent or sequential?
  -> Int
  -- ^ Argument n (zero-indexed). Used for error message.
  -> Term n
  -> NetlistMonad ( (Expr,HWType,Bool)
                  , [Declaration]
                  )
mkArgument ctx bbName bndr declType nArg e = do
    let ty = inferCoreTypeOf (compTypeEnv ctx) (compScope ctx) e
    iw    <- Lens.view intWidth
    hwTyM <- fmap stripFiltered <$> coreTypeToHWTypeM (compScope ctx) ty
    let eTyMsg = "(" ++ showPpr e ++ " :: " ++ showPpr ty ++ ")"
    ((e',t,l),d) <- case hwTyM of
      Nothing
        | (Prim p,_) <- collectArgs e
        , primName p == primName removedArg
        -> return ((Identifier (Id.unsafeMake (primName removedArg)) Nothing, Void Nothing, False), [])
        | otherwise
        -> return ((error ($(curLoc) ++ "Forced to evaluate untranslatable type: " ++ eTyMsg), Void Nothing, False), [])
      Just hwTy -> case collectArgsTicks e of
        (Var _ _ v,[],_) -> do
          return ((Identifier (localIdentifier ctx v) Nothing,hwTy,False),[])
        (GlobalVar g,[],_) -> do
          return ((Identifier (globalIdentifier g) Nothing,hwTy,False),[])
        (Literal lit,[],_) ->
          return ((mkLiteral iw lit,hwTy,True),[])

        (Prim pinfo,args,ticks) -> withTicks ctx ticks $ \tickDecls -> do
          (e',d) <- mkPrimitive ctx True False declType (NetlistId bndr ty) pinfo args tickDecls
          case e' of
            (Identifier _ _) -> return ((e',hwTy,False), d)
            _                -> return ((e',hwTy,isLiteral e), d)
        (Data dc, args,_) -> do
          (exprN,dcDecls) <- mkDcApplication ctx declType [hwTy] (NetlistId bndr ty) dc (lefts args)
          return ((exprN,hwTy,isLiteral e),dcDecls)
        (Case scrut ty' [alt],[],_) -> do
          (projection,decls) <- mkProjection ctx declType False (NetlistId bndr ty) scrut ty' alt
          return ((projection,hwTy,False),decls)
        (Let _ _, [], _ticks) -> do
          (exprN, letDecls) <- mkExpr ctx False declType (NetlistId bndr ty) e
          return ((exprN,hwTy,False),letDecls)
        _ -> do
          let errMsg = [I.i|
            Forced to evaluate unexpected function argument:

              #{eTyMsg}

            in 'mkArgument' for argument #{nArg} of blackbox #{show bbName}.
          |]

          return ((Identifier (error ($(curLoc) ++ errMsg)) Nothing, hwTy, False), [])

    return ((e',t,l),d)

-- | Turn a primitive application into an expression and declarations
mkPrimitive
  :: forall n
   . (HasCallStack, Distinct n)
  => CompCtx n
  -> Bool
  -- ^ Put BlackBox expression in parenthesis
  -> Bool
  -- ^ Treat BlackBox expression as declaration
  -> DeclarationType
  -- ^ Are the declarations concurrent or sequential?
  -> NetlistId n
  -- ^ Id to assign the result to
  -> PrimInfo
  -- ^ Primitive info
  -> [Either (Term n) (Type n)]
  -- ^ Arguments
  -> [Declaration]
  -- ^ Tick declarations
  -> NetlistMonad (Expr, [Declaration])
mkPrimitive ctx bbEParen bbEasD declType dst pInfo args tickDecls =
  go =<< extractPrimWarnOrFail context (primName pInfo)
  where
    tenv = compTypeEnv ctx
    sc = compScope ctx
    context = primWarnContext [] dstName args
    dstName =
      case dst of
        NetlistId i _ -> unpack (Id.toText i)
        CoreId i -> localIdText ctx i
        MultiId is -> unwords (map (localIdText ctx) is)
    tys = netlistTypes dst
    ty = fromMaybe (error "mkPrimitive") (listToMaybe tys)
    assignTy = declTypeUsage declType

    go
      :: P.CompiledPrimitive
      -> NetlistMonad (Expr, [Declaration])
    go =
      \case
        P.BlackBoxHaskell bbName wf _usedArgs multiResult funcName (_fHash, func) -> do
          bbFunRes <- callBlackBoxFunction ctx func bbEasD (primName pInfo) args tys
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
        -- See Note [MultiResult type] in
        -- "Clash.Normalize.Scoped.Transformations.MultiPrim":
        P.BlackBox {name="c$multiPrimSelect"} ->
          pure (Noop, [])
        p@P.BlackBox {multiResult=True, name, template} -> do
          -- Multi result primitives assign their results to signals
          -- provided as arguments. Hence, we ignore any declarations
          -- from 'resBndr1'.
          tcm <- Lens.view tcCache
          let (args1, resArgs) = splitMultiPrimArgs (multiPrimInfo' tcm pInfo) args
              resIds = [ LocalId o t v | Var o t v <- resArgs ]
          (bbCtx, ctxDcls) <- mkBlackBoxContext ctx (primName pInfo) declType resIds args1
          (templ, templDecl) <- prepareBlackBox name template bbCtx
          let bbDecl = N.BlackBoxD name (P.libraries p) (P.imports p) (P.includes p) templ bbCtx
          return (Noop, ctxDcls ++ templDecl ++ tickDecls ++ [bbDecl])
        p@(P.BlackBox {template, name=pNm, kind,outputUsage}) ->
          case kind of
            TDecl -> do
              resM <- resBndr1 True dst
              case resM of
                Just (dst',dstNm,dstDecl) -> do
                  (bbCtx,ctxDcls)   <- mkBlackBoxContextRes ctx (primName pInfo) declType [dst'] args
                  (templ,templDecl) <- prepareBlackBox pNm template bbCtx
                  let bbDecl = N.BlackBoxD pNm (P.libraries p) (P.imports p)
                                           (P.includes p) templ bbCtx
                  declareUse outputUsage dstNm
                  return (Identifier dstNm Nothing,dstDecl ++ ctxDcls ++ templDecl ++ tickDecls ++ [bbDecl])

                -- Render declarations as a Noop when requested
                Nothing | RenderVoid <- P.renderVoid p -> do
                  -- TODO: We should probably 'mkBlackBoxContext' to accept empty lists
                  let dst1 = (Id.unsafeMake "__VOID_TDECL_NOOP__", ty)
                  (bbCtx,ctxDcls) <- mkBlackBoxContextRes ctx (primName pInfo) declType [dst1] args
                  (templ,templDecl) <- prepareBlackBox pNm template bbCtx
                  let bbDecl = N.BlackBoxD pNm (P.libraries p) (P.imports p)
                                           (P.includes p) templ bbCtx
                  return (Noop, ctxDcls ++ templDecl ++ tickDecls ++ [bbDecl])

                -- Otherwise don't render them
                Nothing -> return (Noop,[])
            TExpr -> do
              if bbEasD
                then do
                  resM <- resBndr1 True dst
                  case resM of
                    Just (dst',dstNm,dstDecl) -> do
                      (bbCtx,ctxDcls) <- mkBlackBoxContextRes ctx (primName pInfo) declType [dst'] args
                      (bbTempl,templDecl) <- prepareBlackBox pNm template bbCtx
                      let bbE =  BlackBoxE pNm (P.libraries p) (P.imports p) (P.includes p) bbTempl bbCtx bbEParen
                      tmpAssgn <- case declType of
                        Concurrent -> contAssign dstNm bbE
                        Sequential -> procAssign Blocking dstNm bbE
                      return (Identifier dstNm Nothing, dstDecl ++ ctxDcls ++ templDecl ++ [tmpAssgn])

                    -- Render expression as a Noop when requested
                    Nothing | RenderVoid <- P.renderVoid p -> do
                      -- TODO: We should probably 'mkBlackBoxContext' to accept empty lists
                      let dst1 = (Id.unsafeMake "__VOID_TEXPRD_NOOP__", ty)
                      (bbCtx,ctxDcls) <- mkBlackBoxContextRes ctx (primName pInfo) declType [dst1] args
                      (templ,templDecl) <- prepareBlackBox pNm template bbCtx
                      let bbDecl = N.BlackBoxD pNm (P.libraries p) (P.imports p)
                                               (P.includes p) templ bbCtx
                      return (Noop, ctxDcls ++ templDecl ++ tickDecls ++ [bbDecl])

                    -- Otherwise don't render them
                    Nothing -> return (Identifier (Id.unsafeMake "__VOID_TEXPRD__") Nothing,[])
                else do
                  resM <- resBndr1 False dst
                  case resM of
                    Just (dst',_,_) -> do
                      (bbCtx,ctxDcls)      <- mkBlackBoxContextRes ctx (primName pInfo) declType [dst'] args
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
                      return (BlackBoxE pNm (P.libraries p) (P.imports p) (P.includes p) bbTempl bbCtx bbEParen,ctxDcls ++ templDecl1)
                    -- Render expression as a Noop when requested
                    Nothing | RenderVoid <- P.renderVoid p -> do
                      -- TODO: We should probably 'mkBlackBoxContext' to accept empty lists
                      let dst1 = (Id.unsafeMake "__VOID_TEXPRE_NOOP__", ty)
                      (bbCtx,ctxDcls) <- mkBlackBoxContextRes ctx (primName pInfo) declType [dst1] args
                      (templ,templDecl) <- prepareBlackBox pNm template bbCtx
                      let bbDecl = N.BlackBoxD pNm (P.libraries p) (P.imports p)
                                               (P.includes p) templ bbCtx
                      return (Noop, ctxDcls ++ templDecl ++ tickDecls ++ [bbDecl])

                    -- Otherwise don't render them
                    Nothing -> return (Identifier (Id.unsafeMake "__VOID__") Nothing,[])
        P.Primitive pNm _ _
          | pNm == showt 'GHC.Prim.tagToEnum# -> do
              hwTy <- unsafeCoreTypeToHWTypeM' $(curLoc) (compScope ctx) ty
              case args of
                [Right (ConstTy (TyCon tcN)), Left (Literal (IntLiteral i))] -> do
                  tcm <- Lens.view tcCache
                  let dcs = tyConDataCons (UniqMap.find tcN tcm)
                      dc  = dcs !! fromInteger i
                  (exprN,dcDecls) <- mkDcApplication ctx declType [hwTy] dst dc []
                  return (exprN,dcDecls)
                [Right _, Left scrut] -> do
                  let scrutTy = inferCoreTypeOf tenv sc scrut
                  (scrutExpr,scrutDecls) <-
                    mkExpr ctx False declType (NetlistId (Id.unsafeMake "c$tte_rhs") scrutTy) scrut
                  case scrutExpr of
                    -- Singleton enumerations have no hardware representation,
                    -- just like their constructor applications.
                    _ | isVoid hwTy -> return (Noop,scrutDecls)
                    Identifier id_ Nothing -> return (DataTag hwTy (Left id_),scrutDecls)
                    _ -> do
                      scrutHTy <- unsafeCoreTypeToHWTypeM' $(curLoc) (compScope ctx) scrutTy
                      tmpRhs <- Id.make "c$tte_rhs"
                      netDecl <- mkInit declType assignTy tmpRhs scrutHTy scrutExpr
                      return (DataTag hwTy (Left tmpRhs), netDecl ++ scrutDecls)
                _ -> error $ $(curLoc) ++ "tagToEnum: " ++ show (map (either showPpr showPpr) args)
          -- 'dataToTag#' was removed from GHC.Prim in favor of
          -- 'dataToTagSmall#' / 'dataToTagLarge#'. Kept as a literal for
          -- compatibility with older GHCs where it may still appear.
          | pNm == "GHC.Prim.dataToTag#" -> case args of
              [Right _,Left (Data dc)] -> do
                iw <- Lens.view intWidth
                return (N.Literal (Just (Signed iw,iw)) (NumLit $ toInteger $ dcTag dc - 1),[])
              [Right _,Left scrut] -> mkDataToTag ctx declType assignTy scrut
              _ -> error $ $(curLoc) ++ "dataToTag: " ++ show (map (either showPpr showPpr) args)

#if MIN_VERSION_ghc(9,10,0)
          -- 'dataToTagSmall#' / 'dataToTagLarge#' only exist on GHC >= 9.10;
          -- TH-quoting them requires the symbols to be in scope at compile
          -- time, so this clause is CPP-gated.
          | pNm `elem`
            [showt 'GHC.Prim.dataToTagSmall#, showt 'GHC.Prim.dataToTagLarge#] -> case args of
              [Right _, Right _,Left (Data dc)] -> do
                iw <- Lens.view intWidth
                return (N.Literal (Just (Signed iw,iw)) (NumLit $ toInteger $ dcTag dc - 1),[])
              [Right _, Right _,Left scrut] -> mkDataToTag ctx declType assignTy scrut
              _ -> error $ $(curLoc) ++ "dataToTag: " ++ show (map (either showPpr showPpr) args)
#endif

          | pNm == "Clash.Explicit.SimIO.mealyIO" -> do
              resM <- resBndr1 True dst
              case resM of
                Just (_,dstNm,dstDecl) -> do
                  mealyDecls <- collectMealy ctx dstNm dst (lefts args)
                  return (Noop, dstDecl ++ mealyDecls)
                Nothing -> return (Noop,[])

          | pNm == "Clash.Explicit.SimIO.bindSimIO#" -> do
              (expr,decls) <- collectBindIO ctx dst (lefts args)
              resM <- resBndr True dst
              case resM of
                Just (_,dstNms,dstDecl) -> case expr of
                  Noop ->
                    return (Noop,decls)
                  _ -> case dstNms of
                    [dstNm] -> do
                      declareUse (Proc Blocking) dstNm
                      assn <- procAssign Blocking dstNm expr
                      return ( Identifier dstNm Nothing
                             , dstDecl ++ decls ++ [assn])
                    _ -> error $ $(curLoc) ++ "bindSimIO: " ++ show dstNms
                _ ->
                  return (Noop,decls)

          | pNm == "Clash.Explicit.SimIO.apSimIO#" -> do
              collectAppIO ctx dst (lefts args) []

          | pNm == "Clash.Explicit.SimIO.fmapSimIO#" -> do
              resM <- resBndr1 True dst
              case resM of
                Just (_,dstNm,dstDecl) -> case lefts args of
                  (fun0:arg0:_) -> do
                    let arg1 = unSimIO ctx arg0
                        fun1 = case fun0 of
                          -- Beta reduction
                          Lam b bE -> substTm (extendTmSubst (idSubst sc) b arg1) bE
                          _ -> mkApps fun0 [Left arg1]
                    (expr,bindDecls) <- mkExpr ctx False Sequential dst fun1
                    assn <- case expr of
                              Noop -> pure []
                              _ -> do declareUse (Proc Blocking) dstNm
                                      assn <- procAssign Blocking dstNm expr
                                      pure [assn]
                    return (Identifier dstNm Nothing, dstDecl ++ bindDecls ++ assn)
                  args1 -> error ("internal error: fmapSimIO# has insufficient arguments"
                                  <> showPpr args1)
                Nothing -> case lefts args of
                  (_:arg0:_) -> do
                    (_,bindDecls) <- mkExpr ctx True Sequential dst arg0
                    return (Noop, bindDecls)
                  args1 -> error ("internal error: fmapSimIO# has insufficient arguments"
                                  <> showPpr args1)

          | pNm == "Clash.Explicit.SimIO.unSimIO#" ->
              case lefts args of
                (arg:_) -> mkExpr ctx False Sequential dst arg
                _ -> error "internal error: insufficient arguments"

          | pNm == "Clash.Explicit.SimIO.pureSimIO#" -> do
              (expr,decls) <- case lefts args of
                (arg:_) -> mkExpr ctx False Sequential dst arg
                _ -> error "internal error: insufficient arguments"
              resM <- resBndr True dst
              case resM of
                Just (_,dstNms,dstDecl) -> case expr of
                  Noop ->
                    return (Noop,decls)
                  _ -> case dstNms of
                    [dstNm] -> do
                      declareUse (Proc Blocking) dstNm
                      assn <- procAssign Blocking dstNm expr
                      return ( Identifier dstNm Nothing
                             , dstDecl ++ decls ++ [assn])
                    _ -> error "internal error"
                _ ->
                  return (Noop,decls)

          | pNm == showt 'IS -> do
              (expr,decls) <- case lefts args of
                (arg:_) -> mkExpr ctx False declType dst arg
                _ -> error "internal error: insufficient arguments"
              iw <- Lens.view intWidth
              return (N.DataCon (Signed iw) (DC (Void Nothing,-1)) [expr],decls)

          | pNm == showt 'IP -> do
              (expr,decls) <- case lefts args of
                (arg:_) -> mkExpr ctx False declType dst arg
                _ -> error "internal error: insufficient arguments"
              case expr of
                N.Literal Nothing (NumLit _) -> return (expr,decls)
                _ -> error "non-constant ByteArray# not supported"

          | pNm == showt 'IN -> do
              (expr,decls) <- case lefts args of
                (arg:_) -> mkExpr ctx False declType dst arg
                _ -> error "internal error: insufficient arguments"
              case expr of
                N.Literal Nothing (NumLit i) ->
                  return (N.Literal Nothing (NumLit (negate i)),decls)
                _ -> error "non-constant ByteArray# not supported"

          | pNm == showt 'NS -> do
              (expr,decls) <- case lefts args of
                (arg:_) -> mkExpr ctx False declType dst arg
                _ -> error "internal error: insufficient arguments"
              iw <- Lens.view intWidth
              return (N.DataCon (Unsigned iw) (DC (Void Nothing,-1)) [expr],decls)

          | pNm == showt 'NB -> do
              (expr,decls) <- case lefts args of
                (arg:_) -> mkExpr ctx False declType dst arg
                _ -> error "internal error: insufficient arguments"
              case expr of
                N.Literal Nothing (NumLit _) -> return (expr,decls)
                _ -> error "non-constant ByteArray# not supported"

          | otherwise ->
              return (BlackBoxE "" [] [] []
                        (BBTemplate [Text $ mconcat ["NO_TRANSLATION_FOR:",Text.fromStrict pNm]])
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
      -> NetlistId n
      -- CoreId/NetlistId/MultiId
      -> NetlistMonad (Maybe ([ResId n],[Identifier],[Declaration]))
      -- Nothing when the binder would have type `Void`
    resBndr mkDec dst' = do
      resHwTy <- case tys of
        (ty1:_) -> unsafeCoreTypeToHWTypeM' $(curLoc) (compScope ctx) ty1
        _ -> error "internal error: insufficient types"
      if isVoid resHwTy then
        pure Nothing
      else
        case dst' of
          NetlistId dstL ty' -> case mkDec of
            False ->
              return (Just ([(rawIdentifier dstL, ty')],[dstL],[]))
            True -> do
              nm2 <- Id.suffix dstL "res"
              -- The new identifier is a local variable of an extension of
              -- the scope, so that 'mkNetDecl' can declare it
              idDeclM <-
                withFreshIdBinder sc (C.mkUnsafeSystemName (Id.toText nm2) 0) ty $ \sc' b ->
                  let ctx' = extendCompCtx sc' [(binderLocalId b, rawIdentifier nm2)] ctx
                  in mkNetDecl ctx' (binderLocalId b, sink (mkApps (Prim pInfo) args))
              case idDeclM of
                [] -> return Nothing
                [idDecl] -> return (Just ([(rawIdentifier nm2, ty)],[nm2],[idDecl]))
                ids -> error [I.i|
                  Unexpected nested use of multi result primitive. Ids:

                    #{show ids}

                  Multi primitive should only appear on the RHS of a
                  let-binding. Please report this as a bug.
                |]

          CoreId dstR ->
            let r = localResId ctx dstR
            in return (Just ([r], [fst r], []))
          MultiId ids ->
            let rs = map (localResId ctx) ids
            in return (Just (rs, map fst rs, []))

    -- Like resBndr, but fails on MultiId
    resBndr1
      :: HasCallStack
      => Bool
      -> NetlistId n
      -> NetlistMonad (Maybe (ResId n,Identifier,[Declaration]))
    resBndr1 mkDec dst' = resBndr mkDec dst' >>= \case
      Nothing -> pure Nothing
      Just ([id_],[nm_],decls) -> pure (Just (id_,nm_,decls))
      _ -> error "internal error"

-- | Lower a non-constant @dataToTag# scrut@ to netlist. For plain @Sum@/@SP@
-- types, the bits stored in @scrut@ /are/ the GHC tag, so a 'DataTag' netlist
-- expression that the backend renders as a bit-extract is sufficient. For
-- types with a user-defined bit representation (@CustomSum@, @CustomSP@,
-- @CustomProduct@) the stored bits are arbitrary, so we lower to a
-- 'CondAssignment' that maps each constructor's custom bit pattern back to
-- its GHC tag (@dcTag - 1@). See issue #2724.
mkDataToTag
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> DeclarationType
  -> Usage
  -> Term n
  -> NetlistMonad (Expr, [Declaration])
mkDataToTag ctx declType assignTy scrut = do
  let scrutTy = inferCoreTypeOf (compTypeEnv ctx) (compScope ctx) scrut
  scrutHTy <- unsafeCoreTypeToHWTypeM' $(curLoc) (compScope ctx) scrutTy
  (scrutExpr, scrutDecls) <-
    mkExpr ctx False declType (NetlistId (Id.unsafeMake "c$dtt_rhs") scrutTy) scrut
  (scrutId, scrutDecls') <- case scrutExpr of
    Identifier id_ Nothing -> pure (id_, scrutDecls)
    _ -> do
      tmpRhs <- Id.make "c$dtt_rhs"
      netDecl <- mkInit declType assignTy tmpRhs scrutHTy scrutExpr
      pure (tmpRhs, netDecl ++ scrutDecls)
  let nReprs = case scrutHTy of
        CustomSum _ _ _ reprs -> Just (length reprs)
        CustomSP  _ _ _ reprs -> Just (length reprs)
        CustomProduct {}      -> Just 1
        _                     -> Nothing
  case nReprs of
    Nothing -> pure (DataTag scrutHTy (Right scrutId), scrutDecls')
    Just k -> do
      iw <- Lens.view intWidth
      let sIw = Signed iw
      if k <= 1
        then pure ( N.Literal (Just (sIw, iw)) (NumLit 0)
                  , scrutDecls' )
        else do
          tag <- Id.make "c$dtt_tag"
          let alts = [ ( Just (NumLit (toInteger i))
                       , N.Literal (Just (sIw, iw)) (NumLit (toInteger i)))
                     | i <- [0 .. k - 1] ]
              tagDecl = NetDecl' Nothing tag sIw Nothing
          assn <- condAssign tag sIw (Identifier scrutId Nothing) scrutHTy alts
          -- Wrap the tag identifier in an identity 'DataCon' so the caller's
          -- catch-all assignment fires (an 'Identifier _ Nothing' would be
          -- skipped, leaving the destination unassigned). All backends render
          -- 'DataCon _ (DC (Void {}, -1)) [e]' as 'e' verbatim.
          pure ( N.DataCon sIw (DC (Void Nothing, -1)) [Identifier tag Nothing]
               , tagDecl : assn : scrutDecls' )

-- | Create an template instantiation text and a partial blackbox content for an
-- argument term, given that the term is a function. Errors if the term is not
-- a function
mkFunInput
  :: forall n
   . (HasCallStack, Distinct n)
  => CompCtx n
  -> Text
  -- ^ Name of the primitive of which the function in question is an argument.
  -- Used for error reporting.
  -> DeclarationType
  -- ^ Are we concurrent or sequential?
  -> ResId n
  -- ^ The result of the encompassing primitive/blackbox application. Used
  -- as a name hint if 'mkFunInput' needs intermediate signals.
  -> Term n
  -- ^ The function argument term
  -> NetlistMonad
      ((Either BlackBox (Identifier,[Declaration])
       ,Usage
       ,[BlackBoxTemplate]
       ,[BlackBoxTemplate]
       ,[((TextS.Text,TextS.Text),BlackBox)]
       ,BlackBoxContext)
      ,[Declaration])
mkFunInput ctx parentName declType resId e =
 let (appE,args,ticks) = collectArgsTicks e
 in  withTicks ctx ticks $ \tickDecls -> do
  -- TODO: Rewrite this function to use blackbox functions. Right now it
  -- TODO: generates strings that are later parsed/interpreted again. Silly!
  templ <- case appE of
            Prim p -> do
              bb  <- extractPrimWarnOrFail
                       (primWarnContext [] (unpack (Id.toText (fst resId))) args
                          <> ", as an argument of: " <> unpack parentName)
                       (primName p)
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
                  let (_, resTy) = splitFunTys tenv sc (inferCoreTypeOf tenv sc e)
                  bbhRes <- callBlackBoxFunction ctx func True pName args [resTy]
                  case bbhRes of
                    Left err ->
                      error $ $(curLoc) ++ show fName ++ " yielded an error: "
                                        ++ err
                    Right (BlackBoxMeta{..}, template) ->
                      pure $
                        Left ( bbKind, bbOutputUsage, bbLibrary, bbImports
                             , bbIncludes, pName, template)
            Data dc -> do
              let eTy = inferCoreTypeOf tenv sc e
                  (_,resTy) = splitFunTys tenv sc eTy

              resHTyM0 <- coreTypeToHWTypeM sc resTy
              let resHTyM1 = (\fHwty -> (stripFiltered fHwty, flattenFiltered fHwty)) <$> resHTyM0

              case resHTyM1 of
                -- Special case where coreTypeToHWTypeM determined a type to
                -- be completely transparent.
                Just (_resHTy, [areVoids@(countEq False -> 1)]) -> do
                  let nonVoidArgI = fromJust (elemIndex False areVoids)
                  let arg = Id.unsafeMake (TextS.concat ["~ARG[", showt nonVoidArgI, "]"])
                  let assign = Assignment (Id.unsafeMake "~RESULT") assignTy (Identifier arg Nothing)
                  return (Right ((Id.unsafeMake "", tickDecls ++ [assign]), assignTy))

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
                      dcAss     = Assignment (Id.unsafeMake "~RESULT") assignTy dcApp
                  return (Right ((Id.unsafeMake "",tickDecls ++ [dcAss]), assignTy))

                -- CustomSP the same as SP, but with a user-defined bit
                -- level representation
                Just (resHTy@(CustomSP {}), areVoids0) -> do
                  let
                      dcI       = dcTag dc - 1
                      areVoids1 = indexNote ($(curLoc) ++ "No areVoids with index: " ++ show dcI) areVoids0 dcI
                      mkArg i   = Id.unsafeMake ("~ARG[" <> showt i <> "]")
                      dcInps    = [Identifier (mkArg x) Nothing | x <- originalIndices areVoids1]
                      dcApp     = DataCon resHTy (DC (resHTy,dcI)) dcInps
                      dcAss     = Assignment (Id.unsafeMake "~RESULT") assignTy dcApp
                  return (Right ((Id.unsafeMake "",tickDecls ++ [dcAss]), assignTy))

                -- Like SP, we have to retrieve the index BEFORE filtering voids
                Just (resHTy@(Product _ _ _), areVoids1:_) -> do
                  let mkArg i    = Id.unsafeMake ("~ARG[" <> showt i <> "]")
                      dcInps    = [ Identifier (mkArg x) Nothing | x <- originalIndices areVoids1]
                      dcApp     = DataCon resHTy (DC (resHTy,0)) dcInps
                      dcAss     = Assignment (Id.unsafeMake "~RESULT") assignTy dcApp
                  return (Right ((Id.unsafeMake "",tickDecls ++ [dcAss]), assignTy))

                -- Vectors never have defined areVoids (or all set to False), as
                -- it would be converted to Void otherwise. We can therefore
                -- safely ignore it:
                Just (resHTy@(Vector _ _), _areVoids) -> do
                  let mkArg i = Id.unsafeMake ("~ARG[" <> showt i <> "]")
                      dcInps = [ Identifier (mkArg x) Nothing | x <- [(1::Int)..2] ]
                      dcApp  = DataCon resHTy (DC (resHTy,1)) dcInps
                      dcAss  = Assignment (Id.unsafeMake "~RESULT") assignTy dcApp
                  return (Right ((Id.unsafeMake "",tickDecls ++ [dcAss]), assignTy))

                -- Sum types OR a Sum type after filtering empty types:
                Just (resHTy@(Sum _ _), _areVoids) -> do
                  let dcI   = dcTag dc - 1
                      dcApp = DataCon resHTy (DC (resHTy,dcI)) []
                      dcAss = Assignment (Id.unsafeMake "~RESULT") assignTy dcApp
                  return (Right ((Id.unsafeMake "",tickDecls ++ [dcAss]), assignTy))

                -- Same as Sum, but with user defined bit level representation
                Just (resHTy@(CustomSum {}), _areVoids) -> do
                  let dcI   = dcTag dc - 1
                      dcApp = DataCon resHTy (DC (resHTy,dcI)) []
                      dcAss = Assignment (Id.unsafeMake "~RESULT") assignTy dcApp
                  return (Right ((Id.unsafeMake "",tickDecls ++ [dcAss]), assignTy))

                Just (Void {}, _areVoids) ->
                  return (error $ $(curLoc) ++ "Encountered Void in mkFunInput."
                                            ++ " This is a bug in Clash.")

                _ -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showPpr e
            GlobalVar fun -> do
              topAnns <- Lens.use topEntityAnns
              case lookupGlobalEnv fun topAnns of
                Just _ ->
                  error $ $(curLoc) ++ "Cannot make function input for partially applied Synthesize-annotated: " ++ showPpr e
                _ -> do
                  normalized <- Lens.use bindings
                  case lookupGlobalEnv fun normalized of
                    Just _ -> do
                      (meta,N.Component compName compInps compOutps _) <-
                        preserveVarEnv $ genComponent fun

                      let
                        ComponentMeta{cmWereVoids} = meta
                        inpAssign (i, t) e' = (Identifier i Nothing, In, t, e')
                        inpVar i = Id.unsafeMake ("~VAR[arg" <> showt i <> "][" <> showt i <> "]")
                        inpVars = [Identifier (inpVar i)  Nothing | i <- originalIndices cmWereVoids]
                        inpAssigns = zipWith inpAssign compInps inpVars
                        outpAssigns = case compOutps of
                          [] -> [] -- See issue #2549
                          [(_,compOutp,_)] ->
                            [ ( Identifier (fst compOutp) Nothing
                              , Out
                              , snd compOutp
                              , Identifier (Id.unsafeMake "~RESULT") Nothing )
                            ]
                          outps ->
                            error [I.i|
                              Cannot handle multi-result function as an argument to
                              a primitive.

                              Primitive: #{parentName}

                              Argument: #{showPpr fun}

                              Outputs: #{show (map (\(_,x,_) -> x) outps)}

                              Please report this as an issue.
                            |]
                      instLabel <- Id.next compName
                      let
                        portMap = NamedPortMap (outpAssigns ++ inpAssigns)
                        instDecl = InstDecl Entity Nothing [] compName instLabel [] portMap
                      return (Right ((Id.unsafeMake "",tickDecls ++ [instDecl]), Cont))
                    Nothing -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showPpr e
            Lam {} -> do
              Right . first (second (tickDecls ++)) <$> go ctx 0 appE
            _ -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showPpr e
  let pNm = case appE of
              Prim p -> primName p
              _ -> "__INTERNAL__"
  (bbCtx,dcls) <- mkBlackBoxContextRes ctx pNm declType [resId] args
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
                      assn = Assignment (Id.unsafeMake "~RESULT") assignTy (Identifier t'' Nothing)
                  return ((Right (Id.unsafeMake "",[assn]),assignTy,libs,imps,inc,bbCtx),dcls))
        (\bbName bbHash (TemplateFunction k g _) -> do
          let assn bbCtx' = Assignment (Id.unsafeMake "~RESULT") assignTy
                              (BlackBoxE nm libs imps inc templ' bbCtx' False)
              f' = TemplateFunction k g $ \bbCtx' ->
                     getAp (Backend.blockDecl (Id.unsafeMake "") [assn bbCtx'])
          return ((Left (BBFunction bbName bbHash f')
                  ,assignTy
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
    tenv = compTypeEnv ctx
    sc = compScope ctx
    assignTy = declTypeUsage declType

    goExpr
      :: Distinct x
      => CompCtx x
      -> Term x
      -> NetlistMonad ((Identifier, [Declaration]), Usage)
    goExpr c app@(collectArgsTicks -> (appF,args@(_:_),ticks))
      | Just fun <- funOf appF = do
      resTy <- unsafeCoreTypeToHWTypeM' $(curLoc) (compScope c) (inferCoreTypeOf (compTypeEnv c) (compScope c) app)
      let (tmArgs,tyArgs) = partitionEithers args
      if null tyArgs
        then
          withTicks c ticks $ \tickDecls -> do
            resNm <- Id.make "result"
            appDecls <- mkFunApp c declType resNm fun tmArgs tickDecls
            let assn = [ Assignment (Id.unsafeMake "~RESULT") assignTy (Identifier resNm Nothing)
                       , NetDecl Nothing resNm resTy ]
            nm <- Id.makeBasic "block"
            return ((nm,assn++appDecls), assignTy)
        else do
          (_,sp) <- Lens.use curCompNm
          throw (ClashException sp ($(curLoc) ++ "Not in normal form: Var-application with Type arguments:\n\n" ++ showPpr app) Nothing)
    goExpr c e' = do
      let eType = inferCoreTypeOf (compTypeEnv c) (compScope c) e'
      (appExpr,appDecls) <- mkExpr c False declType (NetlistId (Id.unsafeMake "c$bb_res") eType) e'
      let assn = Assignment (Id.unsafeMake "~RESULT") assignTy appExpr
      nm <- if null appDecls
               then return (Id.unsafeMake "")
               else Id.makeBasic "block"
      return ((nm,appDecls ++ [assn]), assignTy)

    funOf :: Term x -> Maybe (Either (LocalId x) GlobalId)
    funOf = \case
      Var o ty f -> Just (Left (LocalId o ty f))
      GlobalVar g -> Just (Right g)
      _ -> Nothing

    -- The lambdas of the function get the HDL identifiers of the arguments of
    -- the function in the template
    go
      :: Distinct x
      => CompCtx x
      -> Int
      -> Term x
      -> NetlistMonad ((Identifier, [Declaration]), Usage)
    go c n (Lam b e') = do
      lvl <- Lens.use curBBlvl
      let nm = TextS.concat ["~ARGN[",TextS.pack (show lvl),"][",TextS.pack (show n),"]"]
      openIdBinder (compScope c) b e' $ \sc' b' e'' ->
        go (extendCompCtx sc' [(binderLocalId b', Id.unsafeMake nm)] c) (n+1) e''

    go c _ (Var _ _ v) = do
      let assn = Assignment (Id.unsafeMake "~RESULT") assignTy (Identifier (localIdentifier c v) Nothing)
      return ((Id.unsafeMake "",[assn]), assignTy)

    go _ _ (GlobalVar g) = do
      let assn = Assignment (Id.unsafeMake "~RESULT") assignTy
                   (Identifier (globalIdentifier g) Nothing)
      return ((Id.unsafeMake "",[assn]), assignTy)

    go c _ (Case scrut ty [alt]) = do
      let sTy = inferCoreTypeOf (compTypeEnv c) (compScope c) scrut
      (projection,decls) <- mkProjection c declType False (NetlistId (Id.unsafeMake "c$bb_res") sTy) scrut ty alt
      let assn = Assignment (Id.unsafeMake "~RESULT") assignTy projection
      nm <- if null decls
               then return (Id.unsafeMake "")
               else Id.makeBasic "projection"
      return ((nm,decls ++ [assn]), assignTy)

    go c _ (Case scrut ty (alt:alts@(_:_))) = do
      resNm <- Id.make "result"
      resTy <- unsafeCoreTypeToHWTypeM' $(curLoc) (compScope c) ty
      let resId'  = NetlistId resNm ty
      selectionDecls <- mkSelection c declType resId' scrut ty (alt :| alts) []
      let assn = [ NetDecl' Nothing resNm resTy Nothing
                 , Assignment (Id.unsafeMake "~RESULT") assignTy (Identifier resNm Nothing) ]
      nm <- Id.makeBasic "selection"
      return ((nm,assn++selectionDecls), assignTy)

    go c _ e'@(Let {}) =
      fromMaybe (error ($(curLoc) ++ "Not in normal form: " ++ showPpr e')) $
      splitLets c e' $ \c1 binds0 res0 ->
      mkUniqueLets c1 binds0 res0 $ \c2 binders resultM ->
      case resultM of
        Just result -> do
          -- TODO: figure out what to do with multires blackboxes here
          netDecls <- concatMapM (mkNetDecl c2) binders
          decls    <- concatMapM (uncurry (mkDeclarations c2)) binders
          nm <- Id.makeBasic "fun"
          let resultId = localIdentifier c2 (localIdName result)
          -- TODO: Due to reasons lost in the mists of time, #1265 creates an
          -- assignement here, whereas it previously wouldn't. With the PR in
          -- tests break when reverting to the old behavior. In some cases this
          -- creates "useless" assignments. We should investigate whether we can
          -- get the old behavior back.
          let resDecl = Assignment (Id.unsafeMake "~RESULT") assignTy (Identifier resultId Nothing)
          return ((nm,resDecl:netDecls ++ decls), assignTy)
        Nothing -> return ((Id.unsafeMake "",[]), Cont)

    go c n (Tick _ e') = go c n e'

    go c _ e'@(App {}) = goExpr c e'
    go c _ e'@(Data {}) = goExpr c e'
    go c _ e'@(Literal {}) = goExpr c e'
    go c _ e'@(Cast {}) = goExpr c e'
    go c _ e'@(Prim {}) = goExpr c e'
    go c _ e'@(TyApp {}) = goExpr c e'

    go _ _ e'@(Case _ _ []) =
      error $ $(curLoc) ++ "Cannot make function input for case without alternatives: " ++ showPpr e'

    go _ _ e'@(TyLam {}) =
      error $ $(curLoc) ++ "Cannot make function input for TyLam: " ++ showPpr e'

-- * SimIO

-- | Turn a 'mealyIO' expression into a two sequential processes, one "initial"
-- process for the starting state, and one clocked sequential process.
--
-- The arguments of the mealy function and the let-bindings of its body are
-- opened into an extension of the scope of the context, and become the
-- let-bindings of the processes. The let-bindings are visited in the order of
-- 'inverseTopSortOrderOf'.
collectMealy
  :: forall n
   . (HasCallStack, Distinct n)
  => CompCtx n
  -> Identifier
  -- ^ Identifier to assign the final result to
  -> NetlistId n
  -- ^ Id to assign the final result to
  -> [Term n]
  -- ^ The arguments to 'mealyIO'
  -> NetlistMonad [Declaration]
collectMealy ctx dstNm dst (kd:clk:mealyFun:mealyInit:mealyIn:_) =
  openLams ctx mealyFun $ \(ctx1 :: CompCtx l1) args0 res0 ->
    case res0 of
      Let bind body -> openBind (compScope ctx1) bind body $ \(sc2 :: Scope l2) bind' body' ->
        withExtTrans @n @l1 @l2 $
          let ctx2 = extendCompCtx sc2 [] ctx1
              -- Given that we're creating a sequential list of statements from
              -- the let-bindings, make sure that everything is inverse
              -- topologically sorted
              bs = inverseTopSortBinds $ case bind' of
                NonRec b x -> [(binderLocalId b, sink x)]
                Rec tele xs -> zip [ LocalId o t v | Var o t v <- teleIdVars tele ] xs
          in case body' of
               Var o t v -> withBindings ctx2 (map sink args0) bs (LocalId o t v) True
               e -> withResult ctx2 (map sink args0) bs e
      e -> withResult ctx1 args0 [] e
 where
  tenv = compTypeEnv ctx
  sc = compScope ctx

  -- The body of the mealy function is not a variable: bind it to a new
  -- variable, or to the binder of the 'mealyIO' expression
  withResult
    :: forall x
     . DExt n x
    => CompCtx x
    -> [LocalId x]
    -> [(LocalId x, Term x)]
    -> Term x
    -> NetlistMonad [Declaration]
  withResult c args bs e =
    let (occ, ty) = case dst of
          CoreId u0 ->
            -- It has the HDL identifier of the binder as its name, see
            -- 'withBindings' for how the binder replaces it
            ( (localIdOcc u0) {C.nameOcc = Id.toText (localIdentifier ctx (localIdName u0))}
            , sink (localIdType u0) )
          _ ->
            ( C.mkUnsafeSystemName "mealyres" 0
            , inferCoreTypeOf tenv (compScope c) e )
    in withFreshIdBinder (compScope c) occ ty $ \(sc' :: Scope y) b ->
         withExtTrans @n @x @y $
           let u = binderLocalId b
           in withBindings (extendCompCtx sc' [] c) (map sink args)
                (sinkBinds bs ++ [(u, sink e)]) u False

  withBindings
    :: forall x
     . DExt n x
    => CompCtx x
    -> [LocalId x]
    -- ^ The arguments of the mealy function
    -> [(LocalId x, Term x)]
    -- ^ The let-bindings of its body
    -> LocalId x
    -- ^ The result
    -> Bool
    -- ^ Whether the body of the mealy function is a let-expression whose body
    -- is a variable
    -> NetlistMonad [Declaration]
  withBindings c args1 bs res isVarBody = do
    let
      -- Take into account that the state argument is split over multiple
      -- binders because it contained types that are not allowed to occur in
      -- a HDL aggregate type
      mealyInitLength = length (splitShouldSplit tenv sc [inferCoreTypeOf tenv sc mealyInit])
      (sArgs,iArgs) = splitAt mealyInitLength args1
      -- Give all binders a unique name
      sBindings = map (,sink mealyInit) sArgs ++ map (,sink mealyIn) iArgs ++ bs
    mkUniqueLets c sBindings res $ \(ctxU :: CompCtx o) binders0 resultM ->
     withExtTrans @n @x @o $
     case resultM of
      Just result -> do
        let (sBinders,binders1) = splitAt (length sArgs) binders0
            (iBinders,binders2) = splitAt (length iArgs) binders1
            -- Get all the "original" let-bindings, without the above "mealyres".
            -- We don't want to make a NetDecl for it.
            bindersN = if isVarBody then binders2 else init binders2
            dstU = sink dst

        -- Create new net declarations for state-binders, input-binders, and all
        -- the "original" let-bindings in 'mealyFun'
        --
        -- The first set is only assigned in the always block, so they must be
        -- 'reg' in Verilog terminology
        netDeclsSeq <- concatMapM (mkNetDecl ctxU) (sBinders ++ bindersN)
        -- The second set is assigned using concurrent assignment, so don't need
        -- to be 'reg'
        netDeclsInp <- concatMapM (mkNetDecl ctxU) iBinders

        -- If the 'mealyFun' was not a let-expression with a variable reference
        -- as a body then we used the LHS of the entire 'mealyIO' expression as
        -- a new let-binding; otherwise 'mkUniqueLets' would not work.
        --
        -- However, 'mkUniqueLets' made a new unique name for that LHS, which is
        -- not what we want. We want to assign the last expression to the LHS
        -- of 'mealyIO'.
        let bindersE = if isVarBody then binders2 else case dst of
              -- See above why we do this.
              CoreId u0 -> init binders2 ++ [(sink u0, snd (last binders2))]
              _ -> binders2
        seqDecls <- concat <$> mapM (uncurry (mkDeclarations' ctxU Sequential)) bindersE

        -- When the body the let-expression of 'mealyFun' was variable reference,
        -- or in case we had to create a new identifier because the original LHS
        -- was not available: then we need to assign
        (resExpr,resDecls) <-
          if isVarBody then mkExpr ctxU False Concurrent dstU (localIdVar result)
          else case dst of
            CoreId {} -> pure (Noop,[])
            _ -> mkExpr ctxU False Sequential dstU (localIdVar result)

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

        (exprInit,initDecls) <- mkExpr ctxU False Sequential sDst (sink mealyInit)

        initAssign <- case exprInit of
          Identifier _ Nothing -> pure []
          Noop -> pure []
          _ -> case sBinders of
            ((b,_):_) -> do assn <- procAssign Blocking (localIdentifier ctxU (localIdName b)) exprInit
                            pure [assn]
            _ -> error "internal error: insufficient sBinders"

        -- Create the declarations that corresponding to the input
        let iDst = case iBinders of
                     []      -> error "internal error: insufficient iBinders"
                     [(b,_)] -> CoreId b
                     _       -> MultiId (map fst iBinders)

        (exprArg,inpDeclsMisc) <- mkExpr ctxU False Concurrent iDst (sink mealyIn)

        argAssign <- case iBinders of
          ((i,_):_) -> do assn <- contAssign (localIdentifier ctxU (localIdName i)) exprArg
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
        kdTy <- unsafeCoreTypeToHWTypeM $(curLoc) sc (inferCoreTypeOf tenv sc kd)
        let edge = case stripVoid (stripFiltered kdTy) of
                     KnownDomain _ _ Rising _ _ _  -> Falling
                     KnownDomain _ _ Falling _ _ _ -> Rising
                     _ -> error "internal error"
        (clkExpr,clkDecls) <-
          mkExpr ctx False Concurrent (NetlistId (Id.unsafeMake "__MEALY_CLK__") (inferCoreTypeOf tenv sc clk)) clk

        -- collect the declarations related to the input
        let netDeclsInp1 = netDeclsInp ++ inpDeclsMisc

        -- Collate everything
        return (clkDecls ++ netDeclsSeq1 ++ netDeclsInp1 ++ argAssign ++
                  [ Seq [Initial (map SeqDecl (initDeclsOther ++ initAssign))]
                  , Seq [AlwaysClocked edge clkExpr (map SeqDecl seqDeclsOther)]
                  ] ++ resAssn)
      _ -> error "internal error"

  isNet NetDecl' {} = True
  isNet _ = False

collectMealy _ _ _ _ = error "internal error"

-- | Collect the sequential declarations for 'bindIO'
collectBindIO
  :: forall n
   . (HasCallStack, Distinct n)
  => CompCtx n
  -> NetlistId n
  -> [Term n]
  -> NetlistMonad (Expr,[Declaration])
collectBindIO ctx dst (m:Lam x q:_) =
  openIdBinder (compScope ctx) x q $ \(sc1 :: Scope l1) x' q' ->
    collectAction (extendCompCtx sc1 [] ctx) (binderLocalId x') (sink m) $ \(ctx2 :: CompCtx l2) ds0 ->
      withExtTrans @n @l1 @l2 $
        let qS = sink q'
        in fromMaybe (notSplit ctx2 ds0 qS) $
           splitLets ctx2 qS $ \ctx3 bs0 res ->
           mkUniqueLets ctx3 (inverseTopSortBinds bs0) res $ \ctxU binders resultM ->
           case resultM of
             Just result -> do
               ds1 <- concatMapM (uncurry (mkDeclarations' ctxU Sequential)) binders
               netDecls <- concatMapM (mkNetDecl ctxU) binders
               return ( Identifier (localIdentifier ctxU (localIdName result)) Nothing
                      , netDecls ++ ds0 ++ ds1)
             Nothing -> error "internal error"
 where
  notSplit
    :: DExt n y
    => CompCtx y
    -> [Declaration]
    -> Term y
    -> NetlistMonad (Expr,[Declaration])
  notSplit c ds0 = \case
    Let {} -> error "internal error"
    (collectArgs -> (Prim p,args))
      | primName p == "Clash.Explicit.SimIO.bindSimIO#" -> do
          (expr,ds1) <- collectBindIO c (sink dst) (lefts args)
          return (expr, ds0 ++ ds1)
    eS -> do
      (expr,ds1) <- mkExpr c False Sequential (sink dst) eS
      return (expr, ds0 ++ ds1)

  -- The declarations of the action, which binds the variable of the lambda
  collectAction
    :: forall x r
     . Distinct x
    => CompCtx x
    -> LocalId x
    -- ^ The variable bound to the result of the action, without an HDL
    -- identifier in the context
    -> Term x
    -- ^ The action
    -> (forall y. DExt x y => CompCtx y -> [Declaration] -> NetlistMonad r)
    -> NetlistMonad r
  collectAction c xv mS k =
    fromMaybe notLet $
    splitLets c mS $ \(c2 :: CompCtx y) bs0 res ->
    mkUniqueLets c2 ((sink xv, sink mS) : inverseTopSortBinds bs0) res $ \(cU :: CompCtx z) binders resultM ->
    withExtTrans @x @y @z $
    case (binders, resultM) of
      (b:_, Just result) -> do
        -- The variable is assigned the result of the action
        let binders1 = drop 1 binders ++ [(fst b, localIdVar result)]
        ds1 <- concatMapM (uncurry (mkDeclarations' cU Sequential)) binders1
        netDecls <- concatMapM (mkNetDecl cU) binders
        k cU (netDecls ++ ds1)
      _ -> error "internal error"
   where
    notLet = do
      i <- uniqueIdentifier (localIdOcc xv)
      let c' = extendCompCtx (compScope c) [(xv, i)] c
      netDecls <- concatMapM (mkNetDecl c') [(xv,mS)]
      ds1 <- mkDeclarations' c' Sequential xv mS
      k c' (netDecls ++ ds1)

collectBindIO _ _ es = error ("internal error:\n" ++ showPpr es)

-- | Collect the sequential declarations for 'appIO'
collectAppIO
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> NetlistId n
  -> [Term n]
  -> [Term n]
  -> NetlistMonad (Expr,[Declaration])
collectAppIO ctx dst (fun1:arg1:_) rest = case collectArgs fun1 of
  (Prim p,(lefts -> (fun0:arg0:_)))
    | primName p == "Clash.Explicit.SimIO.fmapSimIO#" -> do
        let argN = map (Left . unSimIO ctx) (arg0:arg1:rest)
        mkExpr ctx False Sequential dst (mkApps fun0 argN)
  (Prim p,(lefts -> args))
    | primName p == "Clash.Explicit.SimIO.apSimIO#" ->
        collectAppIO ctx dst args (arg1:rest)
  _ -> error ("internal error:\n" ++ showPpr (fun1:arg1:rest))

collectAppIO _ _ es _ = error ("internal error:\n" ++ showPpr es)

-- | Unwrap the new-type wrapper for things of type SimIO, this is needed to
-- allow applications of the `State# World` token to the underlying IO type.
--
-- XXX: this is most likely needed because Ghc2Core that threw away the cast
-- that this unwrapping; we should really start to support casts.
unSimIO
  :: Distinct n
  => CompCtx n
  -> Term n
  -> Term n
unSimIO ctx arg =
  let argTy = inferCoreTypeOf (compTypeEnv ctx) (compScope ctx) arg
  in  case tyView argTy of
        TyConApp _ [tcArg] ->
          mkApps (primAtType
                    (mkPrimInfo
                       "Clash.Explicit.SimIO.unSimIO#"
                       -- primAtType sets the type
                       undefinedTy
                       WorkNever
                       SingleResult
                       NoUnfolding)
                    (mkFunTy argTy tcArg))
                 [Left arg]
        _ -> error ("internal error:\n" ++ showPpr arg)

-- * Let-expressions

-- | Open the lambdas and type lambdas at the head of a term into an extension
-- of the scope of a context. The context gets the type variables, but no HDL
-- identifiers for the term variables. Gives the variables of the lambdas.
openLams
  :: forall n r
   . Distinct n
  => CompCtx n
  -> Term n
  -> (forall l. DExt n l => CompCtx l -> [LocalId l] -> Term l -> r)
  -> r
openLams ctx0 e0 k = go ctx0 [] e0
 where
  go :: forall x. DExt n x => CompCtx x -> [LocalId x] -> Term x -> r
  go ctx accRev = \case
    Lam b body -> openIdBinder (compScope ctx) b body $ \(sc' :: Scope y) b' body' ->
      withExtTrans @n @x @y $
        go (extendCompCtx sc' [] ctx) (binderLocalId b' : map sink accRev) body'
    TyLam b body -> openTyBinder (compScope ctx) b body $ \(sc' :: Scope y) _ body' ->
      withExtTrans @n @x @y $
        go (extendCompCtx sc' [] ctx)
           (map sink accRev) body'
    body -> k ctx (reverse accRev) body

-- | Order let-bindings such that bindings come after the ones they refer to,
-- unless they are part of a cycle, see 'inverseTopSortOrderOf'
inverseTopSortBinds :: [(LocalId l, Term l)] -> [(LocalId l, Term l)]
inverseTopSortBinds bs =
  map (indexed IntMap.!) (inverseTopSortOrderOf (map (localIdName . fst) bs) (map snd bs))
 where
  indexed = IntMap.fromList (zip [0..] bs)


-- | Open a let-expression into an extension of the scope of a context: its
-- let-bindings and its result, a variable. Nested let-expressions in its body
-- are opened too. The ticks around the let-expression go on every right-hand
-- side. 'Nothing' if the term is not a let-expression, or if its result is not
-- a variable.
--
-- Like 'Clash.Netlist.Scoped.Util.splitNormalized' on a term without
-- lambdas. The variables of the bindings don't have HDL
-- identifiers in the extended context yet, see 'mkUniqueLets'.
splitLets
  :: forall n r
   . Distinct n
  => CompCtx n
  -> Term n
  -> (forall l. DExt n l => CompCtx l -> [(LocalId l, Term l)] -> LocalId l -> r)
  -> Maybe r
splitLets ctx0 e0 k = case collectTicks e0 of
  (e1@Let {}, ticks) -> go ctx0 ticks [] e1
  _ -> Nothing
 where
  go :: forall x. DExt n x => CompCtx x -> [TickInfo x] -> [(LocalId x, Term x)] -> Term x -> Maybe r
  go ctx ticks accRev = \case
    Let bind body -> openBind (compScope ctx) bind body $ \(sc' :: Scope l') bind' body' ->
      withExtTrans @n @x @l' $
        let new = case bind' of
              NonRec b x -> [(binderLocalId b, sink x)]
              Rec bs xs -> zip [ LocalId o ty v | Var o ty v <- teleIdVars bs ] xs
        in go (extendCompCtx sc' [] ctx) (sink1 ticks) (reverse new ++ sinkBinds accRev) body'
    e -> case stripTicks e of
      Var o ty v ->
        Just (k ctx [ (i, mkTicks x ticks) | (i, x) <- reverse accRev ] (LocalId o ty v))
      _ -> Nothing

-- | Move let-bindings into an extended scope
sinkBinds :: DExt x y => [(LocalId x, Term x)] -> [(LocalId y, Term y)]
sinkBinds = map (\(i, x) -> (sink i, sink x))

-- | Give the let-bindings of a let-expression of a component unique HDL
-- identifiers, and extend the context with them. The variables of the
-- let-bindings must be in the scope of the context, without HDL identifiers,
-- see 'splitLets'.
--
-- Like 'Clash.Netlist.Scoped.Util.mkUniqueNormalized', on a let-expression
-- rather than a component. Like it, it adds a let-binding when the result has
-- to be redirected, so the continuation gets the let-bindings and the result
-- in a further extended scope.
--
-- The result is 'Nothing' when it is void.
mkUniqueLets
  :: forall l r
   . (HasCallStack, Distinct l)
  => CompCtx l
  -> [(LocalId l, Term l)]
  -- ^ Let-bindings
  -> LocalId l
  -- ^ The result
  -> (forall o. DExt l o => CompCtx o -> [(LocalId o, Term o)] -> Maybe (LocalId o) -> NetlistMonad r)
  -> NetlistMonad r
mkUniqueLets ctx binds res k = do
  -- Translating the type of the result fails if it is not translatable
  _ <- unsafeCoreTypeToHWTypeM $(curLoc) (compScope ctx) (localIdType res)
  res1Name <- uniqueIdentifier (localIdOcc res)
  portM <- idToOutPort (compScope ctx) res1Name (localIdType res)
  case portM of
    Nothing -> do
      names <- mapM (\(b, _) -> (b,) <$> uniqueIdentifier (localIdOcc b)) binds
      k (extendCompCtx sc names ctx) binds Nothing
    Just _ -> do
      -- Collect new names, see 'renameBinder' for more information. The
      -- blackboxes see the original names.
      let ctx0 = extendCompCtx sc
                   [ (b, Id.unsafeMake (C.nameOcc (localIdOcc b))) | (b, _) <- binds ] ctx
      renames <- concatMapM (renameBinder ctx0 binds) binds
      let
        isRes = (== localIdName res)
        resRenameM0 = case filter (isRes . fst) renames of
          (_, occ) : _ -> Just occ
          [] -> Nothing
        renames0 = Map.fromList [ (tmNameRaw nm, occ) | (nm, occ) <- renames, not (isRes nm) ]
        -- Is the result variable read by any of the other binders? In that case
        -- we need to add a redirection as most synthesis tools don't allow reads
        -- from output ports. Note that if the result is renamed anyway, we don't
        -- have to do anything here.
        resultRead = any (tmNameOccursIn (localIdName res) . snd) binds
        resRenameM1 =
          (\occ -> res {localIdOcc = occ}) <$> resRenameM0
            <|> (if resultRead
                   then Just res {localIdOcc = C.appendToName (localIdOcc res) "_rec"}
                   else Nothing)
        renamed b = maybe b (\occ -> b {localIdOcc = occ})
                      (Map.lookup (tmNameRaw (localIdName b)) renames0)
        (bndrsL, bndrsR) = case break (isRes . localIdName) (map fst binds) of
          (ls, _ : rs) -> (ls, rs)
          _ -> error (concat [ "internal error: unable to find: "
                             , show (localIdOcc res), " in: "
                             , show (map (localIdOcc . fst) binds) ])

      -- The result binder: if it was renamed, it is an internal signal with
      -- an indirection to the result
      resN <- case resRenameM1 of
        Nothing -> pure (res, res1Name)
        Just newName0 -> do
          let newName0' = newName0 {localIdType = stripAnnTypes (localIdType newName0)}
          i <- uniqueIdentifier (localIdOcc newName0')
          pure (newName0', i)

      namesL <- mapM (\b -> (b,) <$> uniqueIdentifier (localIdOcc (renamed b))) bndrsL
      namesR <- mapM (\b -> (b,) <$> uniqueIdentifier (localIdOcc (renamed b))) bndrsR

      let
        binds1 = [ (if isRes (localIdName b) then fst resN else b, e) | (b, e) <- binds ]
        names = namesL ++ [resN] ++ namesR
      case resRenameM1 of
        Nothing ->
          k (extendCompCtx sc names ctx) binds1 (Just (fst resN))
        Just _ ->
          -- Assign the internal signal to the result
          withFreshIdBinder sc (localIdOcc res) (localIdType res) $ \sc' b ->
            let res1 = binderLocalId b
                extraBind = (res1, sink (localIdVar (fst resN)))
            in k (extendCompCtx sc' ([ (sink v, i) | (v, i) <- names ] ++ [(res1, res1Name)]) ctx)
                 (sinkBinds binds1 ++ [extraBind]) (Just res1)
 where
  sc = compScope ctx

  stripAnnTypes :: Type x -> Type x
  stripAnnTypes = \case
    AnnType _ ty -> stripAnnTypes ty
    ty -> ty

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
                        , TextS.unpack (bbName bbCtx), ". Verification "
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

-- | Extract a compiled primitive from a guarded primitive. Emit a warning if
-- the guard wants to, or fail entirely.
extractPrimWarnOrFail
  :: HasCallStack
  => String
  -- ^ Extra context to add to warnings, e.g. the id the primitive's result
  -- is assigned to
  -> TextS.Text
  -- ^ Name of primitive
  -> NetlistMonad P.CompiledPrimitive
extractPrimWarnOrFail context nm = do
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
    -> P.CompiledPrimitive
    -> NetlistMonad P.CompiledPrimitive

  -- The key a primitive is remembered under, see 'go [] cp' below. Note it
  -- includes the current component: the warning mentions component specific
  -- context, which would get lost if the primitive were only warned about once
  -- per compilation. A single run therefore reveals all offending sites rather
  -- than only the first.
  seenKey = do
    (curCompId,_) <- Lens.use curCompNm
    pure (Id.toText curCompId <> ":" <> nm)

  go ((WarnAlways warning):ws) cp = do
    warnPrimitive W.WarnDubiousPrimitive warning
    go ws cp

  go ((WarnNonSynthesizable warning):ws) cp = do
    isTB <- Lens.use isTestBench
    unless isTB (warnPrimitive W.WarnNonSynthesizable warning)
    go ws cp

  -- Only mark the primitive as seen once the whole list of warnings has been
  -- emitted. A primitive can carry several 'WarnAlways' annotations, and all of
  -- them should be reported, see 'tests/shouldwork/PrimitiveGuards/MultipleGuards'.
  go [] cp = do
    key <- seenKey
    seenPrimitives %= Set.insert key
    return cp

  warnPrimitive :: W.ClashWarning -> String -> NetlistMonad ()
  warnPrimitive w warning = do
    seen <- Set.member <$> seenKey <*> Lens.use seenPrimitives
    (curCompId,sp) <- Lens.use curCompNm

    unless seen
      $ warnAboutM w sp
      $ "Dubious primitive instantiation in "
     ++ unpack (Id.toText curCompId)
     ++ (if null context then "" else " (" ++ context ++ ")")
     ++ " for "
     ++ unpack nm
     ++ ": "
     ++ warning
