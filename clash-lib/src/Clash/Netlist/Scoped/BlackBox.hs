{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017     , Google Inc.,
                    2021-2026, QBayLogic B.V.
                    2022     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Blackboxes of Netlist generation on scoped Clash Core: creating blackbox
  contexts, and filling in the templates of primitives. The counterpart of
  "Clash.Netlist.BlackBox".

  The handlers get the t'CompCtx' of the terms they are given, which gives
  the HDL identifiers of their local variables. Where Clash Core renamed the
  binders of a term it went under, e.g. those of the lambdas of a function
  argument of a blackbox ('mkFunInput'), these handlers open the binders, and
  extend the context with the HDL identifiers of the variables they bind.

  The result of a blackbox is not always a local variable: it can be a signal
  that Netlist generation made up. Internally, a result is therefore given by
  its HDL identifier and its type ('ResId').

  Types are still translated to 'HWType's on Clash Core ('hwTypeOf'), TODO(Phase
  5, Util), and the plurality of function arguments is determined on Clash
  Core, TODO(Phase 5, Primitives).
-}

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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Netlist.Scoped.BlackBox
  ( mkBlackBoxContext
  , mkPrimitive
  , extractPrimWarnOrFail
  ) where

import           Control.Applicative           ((<|>))
import           Control.Exception             (throw)
import qualified Control.Lens                  as Lens
import           Control.Monad                 (replicateM, zipWithM)
import           Control.Monad.Extra           (concatMapM)
import           Data.Bifunctor                (first, second)
import           Data.Either                   (lefts, partitionEithers)
import qualified Data.HashMap.Lazy             as HashMap
import qualified Data.IntMap                   as IntMap
import           Data.List.Extra               (countEq, mapAccumLM)
import           Data.List.NonEmpty            (NonEmpty (..))
import           Data.List                     (elemIndex)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromJust, fromMaybe, listToMaybe)
import           Data.Monoid                   (Ap (getAp))
import           Data.Text                     (Text, unpack)
import qualified Data.Text                     as TextS
import qualified Data.Text.Lazy                as Text
import           Data.Text.Extra               (showt)
import           GHC.Stack                     (HasCallStack)

import           Clash.Annotations.Primitive   (extractPrim)
import qualified Clash.Backend                 as Backend
import           Clash.Core.DataCon            (dcTag)
import qualified Clash.Core.Name               as C
import           Clash.Core.Pretty             (showPpr)
import qualified Clash.Core.Term               as C
import           Clash.Netlist.BlackBox        (extractPrimWarnOrFail)
import qualified Clash.Netlist.BlackBox        as Legacy
import           Clash.Netlist.BlackBox.Types
  (BlackBoxMeta (..), BlackBoxTemplate, TemplateKind (..))
import           Clash.Netlist.BlackBox.Util   (onBlackBox, prettyBlackBox, setSym)
import qualified Clash.Netlist.Id              as Id
import           Clash.Netlist.Types           as N
  hiding (Literal, NetlistId (..), netlistId1, netlistTypes, netlistTypes1)
import           Clash.Netlist.Util
  (affixName, coreTypeToHWTypeM, flattenFiltered, mkLiteral, originalIndices,
   preserveVarEnv, primWarnContext, stripFiltered, unsafeCoreTypeToHWTypeM,
   unsafeCoreTypeToHWTypeM')
import           Clash.Normalize.Primitives    (removedArg)
import qualified Clash.Primitives.Types        as P
import qualified Clash.Primitives.Util         as P
import           Clash.Util                    (ClashException (..), curLoc, indexNote)
import qualified Clash.Util.Interpolate        as I

import           Clash.Core.Scoped.FreeVars    (tmNameOccursIn)
import           Clash.Core.Scoped.HasType     (inferCoreTypeOf)
import           Clash.Core.Scoped.NameMap     (lookupGlobalEnv)
import           Clash.Core.Scoped.Subst
  (openBind, openIdBinder, teleIdVars, withFreshIdBinder)
import           Clash.Core.Scoped.Syntax
import           Clash.Core.Scoped.Term
  (collectArgs, collectArgsTicks, collectTicks, mkTicks, stripTicks)
import           Clash.Core.Scoped.TermInfo    (isFun, isLiteral)
import           Clash.Core.Scoped.Type        (splitFunTys)

import {-# SOURCE #-} Clash.Netlist.Scoped
  (genComponent, mkDcApplication, mkDeclarations, mkExpr, mkFunApp, mkNetDecl,
   mkProjection, mkSelection)
import           Clash.Netlist.Scoped.Legacy   (toLegacyNetlistId)
import           Clash.Netlist.Scoped.Types
import           Clash.Netlist.Scoped.Util
  (idToOutPort, renameBinder, uniqueIdentifier, withTicks)

-- | The result of a blackbox: its HDL identifier and its type. See the module
-- header.
type ResId n = (Identifier, Type n)

-- | The result of a blackbox that is a local variable
localResId :: HasCallStack => CompCtx n -> LocalId n -> ResId n
localResId ctx v = (localIdentifier ctx (localIdName v), localIdType v)

-- | The 'HWType' of a type of a component. TODO(Phase 5, Util): translate
-- scoped types.
hwTypeOf :: CompCtx n -> Type n -> NetlistMonad HWType
hwTypeOf ctx = unsafeCoreTypeToHWTypeM' $(curLoc) . toLegacyType ctx

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
    resTys <- mapM (hwTypeOf ctx . snd) resIds
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
              -- TODO(Phase 5, Primitives): use the scoped
              -- 'Clash.Primitives.Scoped.Util.getFunctionPlurality' of the
              -- Primitives port
              P.getFunctionPlurality p (toLegacyArgs ctx args)
                (map (toLegacyType ctx) resTys) i
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
    hwTyM <- fmap stripFiltered <$> coreTypeToHWTypeM (toLegacyType ctx ty)
    let eTyMsg = "(" ++ showPpr (toLegacy ctx e) ++ " :: " ++ showPpr (toLegacyType ctx ty) ++ ")"
    ((e',t,l),d) <- case hwTyM of
      Nothing
        | (Prim p,_) <- collectArgs e
        , C.primName p == showt 'removedArg
        -> return ((Identifier (Id.unsafeMake (showt 'removedArg)) Nothing, Void Nothing, False), [])
        | otherwise
        -> return ((error ($(curLoc) ++ "Forced to evaluate untranslatable type: " ++ eTyMsg), Void Nothing, False), [])
      Just hwTy -> case collectArgsTicks e of
        (Var _ _ v,[],_) -> do
          return ((Identifier (localIdentifier ctx v) Nothing,hwTy,False),[])
        (GlobalVar g,[],_) -> do
          return ((Identifier (Id.unsafeFromCoreId (globalIdVar g)) Nothing,hwTy,False),[])
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

-- | Turn a primitive application into an expression and declarations: see
-- 'Clash.Netlist.BlackBox.mkPrimitive'
mkPrimitive
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> Bool
  -- ^ Put BlackBox expression in parenthesis
  -> Bool
  -- ^ Treat BlackBox expression as declaration
  -> DeclarationType
  -- ^ Are the declarations concurrent or sequential?
  -> NetlistId n
  -- ^ Id to assign the result to
  -> C.PrimInfo
  -- ^ Primitive info
  -> [Either (Term n) (Type n)]
  -- ^ Arguments
  -> [Declaration]
  -- ^ Tick declarations
  -> NetlistMonad (Expr, [Declaration])
mkPrimitive ctx bbEParen bbEasD declType dst pInfo args tickDecls =
  Legacy.mkPrimitive bbEParen bbEasD declType (toLegacyNetlistId ctx dst) pInfo
    (toLegacyArgs ctx args) tickDecls

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
                       (primWarnContext [] (unpack (Id.toText (fst resId)))
                          (toLegacyArgs ctx args)
                          <> ", as an argument of: " <> unpack parentName)
                       (C.primName p)
              case bb of
                P.BlackBox {..} ->
                  pure (Left (kind,outputUsage,libraries,imports,includes,C.primName p,template))
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

              resHTyM0 <- coreTypeToHWTypeM (toLegacyType ctx resTy)
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

                _ -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showPpr (toLegacy ctx e)
            GlobalVar fun -> do
              topAnns <- Lens.use topEntityAnns
              case lookupGlobalEnv fun topAnns of
                Just _ ->
                  error $ $(curLoc) ++ "Cannot make function input for partially applied Synthesize-annotated: " ++ showPpr (toLegacy ctx e)
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

                              Argument: #{showPpr (globalIdVar fun)}

                              Outputs: #{show (map (\(_,x,_) -> x) outps)}

                              Please report this as an issue.
                            |]
                      instLabel <- Id.next compName
                      let
                        portMap = NamedPortMap (outpAssigns ++ inpAssigns)
                        instDecl = InstDecl Entity Nothing [] compName instLabel [] portMap
                      return (Right ((Id.unsafeMake "",tickDecls ++ [instDecl]), Cont))
                    Nothing -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showPpr (toLegacy ctx e)
            Lam {} -> do
              Right . first (second (tickDecls ++)) <$> go ctx 0 appE
            _ -> error $ $(curLoc) ++ "Cannot make function input for: " ++ showPpr (toLegacy ctx e)
  let pNm = case appE of
              Prim p -> C.primName p
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
      resTy <- hwTypeOf c (inferCoreTypeOf (compTypeEnv c) (compScope c) app)
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
          throw (ClashException sp ($(curLoc) ++ "Not in normal form: Var-application with Type arguments:\n\n" ++ showPpr (toLegacy c app)) Nothing)
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
                   (Identifier (Id.unsafeFromCoreId (globalIdVar g)) Nothing)
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
      resTy <- hwTypeOf c ty
      let resId'  = NetlistId resNm ty
      selectionDecls <- mkSelection c declType resId' scrut ty (alt :| alts) []
      let assn = [ NetDecl' Nothing resNm resTy Nothing
                 , Assignment (Id.unsafeMake "~RESULT") assignTy (Identifier resNm Nothing) ]
      nm <- Id.makeBasic "selection"
      return ((nm,assn++selectionDecls), assignTy)

    go c _ e'@(Let {}) =
      fromMaybe (error ($(curLoc) ++ "Not in normal form: " ++ showPpr (toLegacy c e'))) $
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

    go c _ e'@(Case _ _ []) =
      error $ $(curLoc) ++ "Cannot make function input for case without alternatives: " ++ showPpr (toLegacy c e')

    go c _ e'@(TyLam {}) =
      error $ $(curLoc) ++ "Cannot make function input for TyLam: " ++ showPpr (toLegacy c e')

-- * Let-expressions

-- | Open a let-expression into an extension of the scope of a context: its
-- let-bindings and its result, a variable. Nested let-expressions in its body
-- are opened too. The ticks around the let-expression go on every right-hand
-- side. 'Nothing' if the term is not a let-expression, or if its result is not
-- a variable.
--
-- The counterpart of Clash Core's 'Clash.Netlist.Util.splitNormalized' on a
-- term without lambdas. The variables of the bindings don't have HDL
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
-- The counterpart of Clash Core's 'Clash.Netlist.Util.mkUniqueNormalized' on
-- a let-expression, see 'Clash.Netlist.Scoped.Util.mkUniqueNormalized' for
-- components. It generates the same identifiers in the same order. Like it, it
-- adds a let-binding when the result has to be redirected, so the continuation
-- gets the let-bindings and the result in a further extended scope.
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
  _ <- unsafeCoreTypeToHWTypeM $(curLoc) (toLegacyType ctx (localIdType res))
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
