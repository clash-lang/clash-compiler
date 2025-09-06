{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2021-2023, QBayLogic B.V.
                    2022     , LUMI GUIDE FIETSDETECTIE B.V.
                    2022     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utilties to verify blackbox contexts against templates and rendering filled
  in templates
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Netlist.BlackBox.Util
    ( renderTemplate
    , walkElement
    , verifyBlackBoxContext
    , onBlackBox
    , setSym
    , extractLiterals
    , renderBlackBox
    , getUsedArguments
    , renderFilePath
    , exprToString
    , renderElem
    , getDomainConf
    , prettyBlackBox
    ) where

import           Control.Exception               (throw)
import           Control.Lens
  (use, (%=), _1, _2, element, (^?))
import           Control.Monad                   (forM, (<=<))
import           Control.Monad.State             (State, StateT (..), lift, gets)
import           Data.Bitraversable              (bitraverse)
import           Data.Bool                       (bool)
import           Data.Coerce                     (coerce)
import           Data.Foldable                   (foldrM)
import           Data.Hashable                   (Hashable (..))
import qualified Data.HashMap.Strict             as HashMap
import qualified Data.IntMap                     as IntMap
import           Data.List                       (nub)
import           Data.List.Extra                 (indexMaybe)
import           Data.Maybe                      (mapMaybe, maybeToList, fromJust)
import           Data.Monoid                     (Ap(getAp))
import qualified Data.Text
import           Data.Text.Lazy                  (Text)
import qualified Data.Text.Lazy                  as Text

#if MIN_VERSION_prettyprinter(1,7,0)
import qualified Prettyprinter                   as PP
#else
import qualified Data.Text.Prettyprint.Doc       as PP
#endif

import           Data.Text.Prettyprint.Doc.Extra
import           GHC.Stack                       (HasCallStack)
import           System.FilePath                 (replaceBaseName, takeBaseName,
                                                  takeFileName, (<.>))
import           Text.Printf
import           Text.Read                       (readEither)
import           Text.Trifecta.Result            hiding (Err)

import           Clash.Backend
  (Backend (..), DomainMap, Usage (..), AggressiveXOptBB(..), RenderEnums(..))
import           Clash.Netlist.BlackBox.Parser
import           Clash.Netlist.BlackBox.Types
import           Clash.Netlist.Types
  (BlackBoxContext (..), Expr (..), HWType (..), Literal (..), Modifier (..),
   Declaration(BlackBoxD))
import qualified Clash.Netlist.Id                as Id
import qualified Clash.Netlist.Types             as N
import           Clash.Netlist.Util              (typeSize, isVoid, stripAttributes, stripVoid)
import           Clash.Signal.Internal
  (ResetKind(..), ResetPolarity(..), InitBehavior(..), VDomainConfiguration (..))
import           Clash.Util
import qualified Clash.Util.Interpolate          as I

import           Clash.Annotations.Primitive     (HDL(VHDL))

inputHole :: Element -> Maybe Int
inputHole = \case
  Text _           -> Nothing
  Component _      -> Nothing
  Result           -> Nothing
  Arg n            -> pure n
  ArgGen _ n       -> pure n
  Const n          -> pure n
  Lit n            -> pure n
  Name n           -> pure n
  ToVar _ n        -> pure n
  Sym _ _          -> Nothing
  Typ nM           -> nM
  TypM nM          -> nM
  Err nM           -> nM
  TypElem _        -> Nothing
  CompName         -> Nothing
  IncludeName _    -> Nothing
  IndexType _      -> Nothing
  Size _           -> Nothing
  Length _         -> Nothing
  Depth _          -> Nothing
  MaxIndex _       -> Nothing
  FilePath _       -> Nothing
  Template _ _     -> Nothing
  Gen _            -> Nothing
  IF _ _ _         -> Nothing
  And _            -> Nothing
  IW64             -> Nothing
  CmpLE _ _        -> Nothing
  HdlSyn _         -> Nothing
  BV _ _ _         -> Nothing
  Sel _ _          -> Nothing
  IsLit n          -> pure n
  IsVar n          -> pure n
  IsScalar n       -> pure n
  IsActiveHigh n   -> pure n
  Tag n            -> pure n
  Period n         -> pure n
  LongestPeriod    -> Nothing
  ActiveEdge _ n   -> pure n
  IsSync n         -> pure n
  IsInitDefined n  -> pure n
  IsActiveEnable n -> pure n
  IsUndefined n    -> pure n
  StrCmp _ n       -> pure n
  OutputUsage n    -> pure n
  Vars n           -> pure n
  GenSym _ _       -> Nothing
  Repeat _ _       -> Nothing
  DevNull _        -> Nothing
  SigD _ nM        -> nM
  CtxName          -> Nothing

-- | Determine if the number of normal\/literal\/function inputs of a blackbox
-- context at least matches the number of argument that is expected by the
-- template.
verifyBlackBoxContext
  :: BlackBoxContext
  -- ^ Blackbox to verify
  -> N.BlackBox
  -- ^ Template to check against
  -> Maybe String
verifyBlackBoxContext bbCtx (N.BBFunction _ _ (N.TemplateFunction _ f _)) =
  if f bbCtx then
    Nothing
  else
    -- TODO: Make TemplateFunction return a string
    Just ("Template function for returned False")
verifyBlackBoxContext bbCtx (N.BBTemplate t) =
  orElses (concatMap (walkElement verify') t)
  where
    concatTups = concatMap (\(x, y) -> [x, y])

    verify' e =
      Just $
      case e of
        Lit n ->
          case indexMaybe (bbInputs bbCtx) n of
            Just (inp, isVoid -> False, False) ->
              Just ( "Argument " ++ show n ++ " should be literal, as blackbox "
                  ++ "used ~LIT[" ++ show n ++ "], but was:\n\n" ++ show inp)
            _ -> Nothing
        Const n ->
          case indexMaybe (bbInputs bbCtx) n of
            Just (inp, isVoid -> False, False) ->
              Just ( "Argument " ++ show n ++ " should be literal, as blackbox "
                  ++ "used ~CONST[" ++ show n ++ "], but was:\n\n" ++ show inp)
            _ -> Nothing
        Component (Decl n subn l') ->
          case IntMap.lookup n (bbFunctions bbCtx) of
            Just funcs ->
              case indexMaybe funcs subn of
                Nothing ->
                  Just ( "Blackbox requested at least " ++ show (subn+1)
                      ++ " renders of function at argument " ++ show n ++ " but "
                      ++ "found only " ++ show (length funcs) )
                Just _ ->
                  orElses $
                    map
                      (verifyBlackBoxContext bbCtx . N.BBTemplate)
                      (concatTups l')
            Nothing ->
              Just ( "Blackbox requested instantiation of function at argument "
                  ++ show n ++ ", but BlackBoxContext did not contain one.")
        _ ->
          case inputHole e of
            Nothing ->
              Nothing
            Just n ->
              case indexMaybe (bbInputs bbCtx) n of
                Just _ -> Nothing
                Nothing -> do
                  let str = fromJust (fmap Text.unpack (getAp $ prettyElem e))
                  Just ( "Blackbox used \"" ++ str ++ "\""
                      ++ ", but only " ++ show (length (bbInputs bbCtx))
                      ++ " arguments were passed." )

extractLiterals :: BlackBoxContext
                -> [Expr]
extractLiterals = map (\case (e,_,_) -> e)
                . filter (\case (_,_,b) -> b)
                . bbInputs

-- | Update all the symbol references in a template, and increment the symbol
-- counter for every newly encountered symbol.
setSym
  :: forall m
   . Id.IdentifierSetMonad m
  => BlackBoxContext
  -> BlackBoxTemplate
  -> m (BlackBoxTemplate,[N.Declaration])
setSym bbCtx l = do
    (a,(_,decls)) <- runStateT (mapM setSym' l) (IntMap.empty,IntMap.empty)
    return (a,concatMap snd (IntMap.elems decls))
  where
    bbnm = Data.Text.unpack (bbName bbCtx)

    setSym'
      :: Element
      -> StateT ( IntMap.IntMap N.IdentifierText
                , IntMap.IntMap (N.IdentifierText, [N.Declaration]))
                m
                Element
    setSym' e = case e of
      ToVar nm i | i < length (bbInputs bbCtx) -> case bbInputs bbCtx !! i of
        (Identifier nm0 Nothing,_,_) ->
          return (ToVar [Text (Id.toLazyText nm0)] i)

        (e',hwTy,_) -> do
          varM <- IntMap.lookup i <$> use _2
          case varM of
            Nothing -> do
              nm' <- lift (Id.make (Text.toStrict (concatT (Text "c$":nm))))
              let decls = case typeSize hwTy of
                    0 -> []
                    _ -> [N.NetDecl Nothing nm' hwTy
                         ,N.Assignment nm' N.Cont e' -- TODO De-hardcode Cont
                         ]
              _2 %= (IntMap.insert i (Id.toText nm',decls))
              return (ToVar [Text (Id.toLazyText nm')] i)
            Just (nm',_) ->
              return (ToVar [Text (Text.fromStrict nm')] i)
      Sym _ i -> do
        symM <- IntMap.lookup i <$> use _1
        case symM of
          Nothing -> do
            t <- Id.toText <$> lift (Id.make "c$n")
            _1 %= (IntMap.insert i t)
            return (Sym (Text.fromStrict t) i)
          Just t -> return (Sym (Text.fromStrict t) i)
      GenSym t i -> do
        symM <- IntMap.lookup i <$> use _1
        case symM of
          Nothing -> do
            t' <- Id.toText <$> lift (Id.makeBasic (Text.toStrict (concatT t)))
            _1 %= (IntMap.insert i t')
            return (GenSym [Text (Text.fromStrict t')] i)
          Just _ ->
            error ("Symbol #" ++ show (t,i)
                ++ " is already defined in BlackBox for: "
                ++ bbnm)
      Component (Decl n subN l') ->
        Component <$> (Decl n subN <$> mapM (bitraverse (mapM setSym') (mapM setSym')) l')
      IF c t f      -> IF <$> pure c <*> mapM setSym' t <*> mapM setSym' f
      SigD e' m     -> SigD <$> (mapM setSym' e') <*> pure m
      BV t e' m     -> BV <$> pure t <*> mapM setSym' e' <*> pure m
      _             -> pure e

    concatT :: [Element] -> Text
    concatT = Text.concat . map (
      \case
        Text t -> t
        Name i ->
          case elementToText bbCtx (Name i) of
            Right t -> t
            Left msg ->
              error $ $(curLoc) ++  "Could not convert ~NAME[" ++ show i ++ "]"
                   ++ " to string:" ++ msg ++ "\n\nError occured while "
                   ++ "processing blackbox for " ++ bbnm
        Lit i ->
          case elementToText bbCtx (Lit i) of
            Right t -> t
            Left msg ->
              error $ $(curLoc) ++  "Could not convert ~LIT[" ++ show i ++ "]"
                   ++ " to string:" ++ msg ++ "\n\nError occured while "
                   ++ "processing blackbox for " ++ bbnm
        Result | [(Identifier t _, _)] <- bbResults bbCtx -> Id.toLazyText t
        CompName -> Id.toLazyText (bbCompName bbCtx)
        CtxName ->
          case bbCtxName bbCtx of
            Just nm -> Text.fromStrict nm
            _ | [(Identifier t _, _)] <- bbResults bbCtx -> Id.toLazyText t
            _ -> error $ $(curLoc) ++ "Internal error when processing blackbox "
                      ++ "for " ++ bbnm
        _ -> error $ $(curLoc) ++ "Unexpected element in GENSYM when processing "
                  ++ "blackbox for " ++ bbnm
        )

selectNewName
    :: Foldable t
    => t String
    -- ^ Set of existing names
    -> FilePath
    -- ^ Name for new file (
    -> String
selectNewName as a
  | elem a as = selectNewName as (replaceBaseName a (takeBaseName a ++ "_"))
  | otherwise = a

renderFilePath :: [(String,FilePath)] -> String -> ([(String,FilePath)],String)
renderFilePath fs f = ((f'',f):fs, f'')
  where
    f'  = takeFileName f
    f'' = selectNewName (map fst fs) f'

-- | Render a blackbox given a certain context. Returns a filled out template
-- and a list of 'hidden' inputs that must be added to the encompassing component.
renderTemplate
  :: Backend backend
  => BlackBoxContext -- ^ Context used to fill in the hole
  -> BlackBoxTemplate -- ^ Blackbox template
  -> State backend (Int -> Text)
renderTemplate bbCtx l = do
  l' <- mapM (renderElem bbCtx) l
  return (\col -> Text.concat (map ($ col) l'))

renderBlackBox
  :: Backend backend
  => [BlackBoxTemplate]
  -> [BlackBoxTemplate]
  -> [((Data.Text.Text,Data.Text.Text), N.BlackBox)]
  -> N.BlackBox
  -> BlackBoxContext
  -> State backend (Int -> Doc)
renderBlackBox libs imps includes bb bbCtx = do
  let nms' = zipWith (\_ i -> "~INCLUDENAME[" <> Text.pack (show i) <> "]")
                     includes
                     [(0 :: Int)..]
      layout = LayoutOptions (AvailablePerLine 120 0.4)
  nms <-
    forM includes $ \((nm,_),inc) -> do
      case verifyBlackBoxContext bbCtx inc of
        Nothing -> return ()
        Just err0 -> do
          sp <- getSrcSpan
          let err1 = concat [ "Couldn't instantiate blackbox for "
                            , Data.Text.unpack (bbName bbCtx), ". Verification "
                            , "procedure reported:\n\n" ++ err0 ]
          throw (ClashException sp ($(curLoc) ++ err1) Nothing)
      let bbCtx' = bbCtx {bbQsysIncName = map Text.toStrict nms'}
      incForHash <- onBlackBox (renderTemplate bbCtx')
                               (\_name _hash (N.TemplateFunction _ _ f) -> do
                                  t <- f bbCtx'
                                  let t' = renderLazy (layoutPretty layout t)
                                  return (const t'))
                               inc
      iw <- iwWidth
      topNm <- getTopName
      let incHash = hash (incForHash 0)
          nm'     = Text.concat
                      [ Text.fromStrict (Id.toText topNm)
                      , "_"
                      , Text.fromStrict nm
                      , "_"
                      , Text.pack (printf ("%0" ++ show (iw `div` 4) ++ "X") incHash)
                      ]
      pure nm'

  let bbNamedCtx = bbCtx {bbQsysIncName = map Text.toStrict nms}
      incs = snd <$> includes
  bb' <- case bb of
        N.BBTemplate bt   -> do
          t <- renderTemplate bbNamedCtx bt
          return (\col -> let t1 = t (col + 2)
                          in  if Text.null t1
                              then PP.emptyDoc
                              else PP.nest (col-2) (PP.pretty t1))
        N.BBFunction _ _ (N.TemplateFunction _ _ bf)  -> do
          t <- bf bbNamedCtx
          return (\_ -> t)

  incs' <- mapM (onBlackBox (fmap (PP.pretty . ($ 0)) . renderTemplate bbNamedCtx)
                            (\_name _hash (N.TemplateFunction _ _ f) -> f bbNamedCtx))
                incs
  libs' <- mapM (fmap ($ 0) . renderTemplate bbNamedCtx) libs
  imps' <- mapM (fmap ($ 0) . renderTemplate bbNamedCtx) imps
  addIncludes $ zipWith3 (\nm' ((_, ext), _) inc -> (Text.unpack nm' <.> Data.Text.unpack ext, inc)) nms includes incs'
  addLibraries libs'
  addImports imps'
  return bb'

-- | Render a single template element
renderElem
  :: HasCallStack
  => Backend backend
  => BlackBoxContext
  -> Element
  -> State backend (Int -> Text)
renderElem b (Component (Decl n subN (l:ls))) = do
  (o,oTy,_) <- idToExpr <$> bitraverse (lineToIdentifier b) (return . lineToType b) l
  is <- mapM (fmap idToExpr . bitraverse (lineToIdentifier b) (return . lineToType b)) ls
  sp <- getSrcSpan
  let func0 = IntMap.lookup n (bbFunctions b)
      errr = concat [ "renderElem: not enough functions rendered? Needed "
                    , show (subN +1 ), " got only ", show (length (fromJust func0)) ]
  case indexNote' errr subN <$> func0 of
    Just (templ0,_,libs,imps,inc,pCtx) -> do
      let b' = pCtx { bbResults = [(o,oTy)], bbInputs = bbInputs pCtx ++ is }
          layoutOptions = LayoutOptions (AvailablePerLine 120 0.4)
          render = N.BBTemplate . parseFail b' . renderLazy . layoutPretty layoutOptions

      templ1 <-
        case templ0 of
          Left t ->
            return t
          Right (nm0,ds) -> do
            nm1 <- Id.next nm0
            block <- getAp (blockDecl nm1 ds)
            return (render block)

      templ4 <-
        case templ1 of
          N.BBFunction {} ->
            return templ1
          N.BBTemplate templ2 -> do
            (templ3, templDecls) <- setSym b' templ2
            case templDecls of
              [] ->
                return (N.BBTemplate templ3)
              _ -> do
                nm1 <- Id.toText <$> Id.makeBasic "bb"
                nm2 <- Id.makeBasic "bb"
                let bbD = BlackBoxD nm1 libs imps inc (N.BBTemplate templ3) b'
                block <- getAp (blockDecl nm2 (templDecls ++ [bbD]))
                return (render block)

      case verifyBlackBoxContext b' templ4 of
        Nothing -> do
          bb <- renderBlackBox libs imps inc templ4 b'
          return (renderLazy . layoutPretty layoutOptions . bb)
        Just err0 -> do
          let err1 = concat [ "Couldn't instantiate blackbox for "
                            , Data.Text.unpack (bbName b), ". Verification procedure "
                            , "reported:\n\n" ++ err0 ]
          throw (ClashException sp ($(curLoc) ++ err1) Nothing)
    Nothing ->
      let err1 = concat [show n
                        , "'th argument isn't a function, only "
                        , show (IntMap.keys (bbFunctions b))
                        , "are."]
       in throw (ClashException sp ($(curLoc) ++ err1) Nothing)

renderElem b (SigD e m) = do
  e' <- Text.concat <$> mapM (fmap ($ 0) . renderElem b) e
  let ty = case m of
             Nothing -> snd $ bbResult "~SIGD" b
             Just n  -> let (_,ty',_) = bbInputs b !! n
                        in  ty'
  t  <- getAp (hdlSig e' ty)
  return (const (renderOneLine t))

renderElem b (Period n) = do
  let (_, ty, _) = bbInputs b !! n
  case stripVoid ty of
    KnownDomain _ period _ _ _ _ ->
      return $ const $ Text.pack $ show period
    _ ->
      error $ $(curLoc) ++ "Period: Expected `KnownDomain` or `KnownConfiguration`, not: " ++ show ty

renderElem _ LongestPeriod = do
  doms <- domainConfigurations
  -- Longest period with a minimum of 100 ns, see:
  -- https://github.com/clash-lang/clash-compiler/issues/2455
  let longestPeriod = maximum (100_000 : [vPeriod v | v <- HashMap.elems doms])
  return (const (Text.pack (show longestPeriod)))

renderElem b (Tag n) = do
  let (_, ty, _) = bbInputs b !! n
  case stripVoid ty of
    KnownDomain dom _ _ _ _ _ ->
      return (const (Text.pack (Data.Text.unpack dom)))
    Clock dom ->
      return (const (Text.pack (Data.Text.unpack dom)))
    ClockN dom ->
      return (const (Text.pack (Data.Text.unpack dom)))
    Reset dom ->
      return (const (Text.pack (Data.Text.unpack dom)))
    Enable dom ->
      return (const (Text.pack (Data.Text.unpack dom)))
    _ ->
      error $ $(curLoc) ++ "Tag: Expected `KnownDomain` or `KnownConfiguration`, not: " ++ show ty


renderElem b (IF c t f) = do
  iw <- iwWidth
  hdl <- gets hdlKind
  syn <- hdlSyn
  enums <- renderEnums
  xOpt <- aggressiveXOptBB
  c' <- check (coerce xOpt) iw hdl syn enums c
  if c' > 0 then renderTemplate b t else renderTemplate b f
  where
    check :: Backend backend => Bool -> Int -> HDL -> HdlSyn -> RenderEnums -> Element -> State backend Int
    check xOpt iw hdl syn enums c' = case c' of
      (Size e)   -> pure $ typeSize (lineToType b [e])
      (Length e) -> pure $ case lineToType b [e] of
                       (Vector n _)              -> n
                       Void (Just (Vector n _))  -> n
                       (MemBlob n _)             -> n
                       Void (Just (MemBlob n _)) -> n
                       _                         -> 0 -- HACK: So we can test in splitAt if one of the
                              -- vectors in the tuple had a zero length
      (Lit n) -> pure $ case bbInputs b !! n of
        (l,_,_)
          | Literal _ l' <- l ->
            case l' of
              -- Integer, Int#, KnownNat, Natural, Word#
              NumLit i -> fromInteger i
              -- Bit
              BitLit bl -> case bl of
                N.H -> 1
                N.L -> 0
                _   -> error $ $(curLoc) ++ "IF: LIT bit literal must be high or low"
              -- Bool
              BoolLit bl -> bool 0 1 bl
              _ -> error $ $(curLoc) ++ "IF: LIT must be a numeric lit"
          -- Int
          | DataCon (Signed _) _ [Literal _ (NumLit i)] <- l
            -> fromInteger i
          -- Word, SNat
          | DataCon (Unsigned _) _ [Literal _ (NumLit i)] <- l
            -> fromInteger i
          | BlackBoxE pNm _lib _use _incl _templ bbCtx _paren <- l
          , pNm `elem` ["GHC.Int.I8#", "GHC.Int.I16#", "GHC.Int.I32#", "GHC.Int.I64#"
                       ,"GHC.Word.W8#","GHC.Word.W16#","GHC.Word.W32#","GHC.Word.W64#"
                       ,"GHC.Types.I#","GHC.Types.W#"
                       ]
          , [Literal _ (NumLit j)] <- extractLiterals bbCtx
          -> fromInteger j
        k -> error $ $(curLoc) ++ ("IF: LIT must be a numeric lit:" ++ show k)
      (Depth e)  -> pure $ case lineToType b [e] of
                      (RTree n _) -> n
                      _ -> error $ $(curLoc) ++ "IF: treedepth of non-tree type"
      IW64       -> pure $ if iw == 64 then 1 else 0
      (HdlSyn s) -> pure $ if s == syn then 1 else 0
      (IsVar n)  -> pure $ let (e,_,_) = bbInputs b !! n
                    in case e of
                      Identifier _ Nothing -> 1
                      _                    -> 0
      (IsLit n)  -> pure $ let (e,_,_) = bbInputs b !! n
                    in case e of
                      DataCon {}   -> 1
                      Literal {}   -> 1
                      BlackBoxE {} -> 1
                      _            -> 0
      (IsScalar n) -> let (_,ty,_) = bbInputs b !! n
                          isScalar _ Bit          = 1
                          isScalar _ Bool         = 1
                          isScalar VHDL Integer   = 1
                          isScalar VHDL (Sum _ _) = case enums of
                                                       RenderEnums True  -> 1
                                                       RenderEnums False -> 0
                          isScalar _ _            = 0
                        in pure $ isScalar hdl ty

      (IsUndefined n) -> pure $
        let (e, _, _) = bbInputs b !! n
        in if xOpt && checkUndefined e then 1 else 0

      (IsActiveEnable n) -> pure $
        let (e, ty, _) = bbInputs b !! n in
        case ty of
          Enable _ ->
            case e of
              DataCon _ _ [Literal Nothing (BoolLit True)]  -> 0
              -- TODO: Emit warning? If enable signal is inferred as always
              -- TODO: False, the component will never be enabled. This is
              -- TODO: probably not the user's intention.
              DataCon _ _ [Literal Nothing (BoolLit False)] -> 1
              _                                             -> 1
          Bool ->
            case e of
              Literal Nothing (BoolLit True)  -> 0
              -- TODO: Emit warning? If enable signal is inferred as always
              -- TODO: False, the component will never be enabled. This is
              -- TODO: probably not the user's intention.
              Literal Nothing (BoolLit False) -> 1
              _                               -> 1
          _ ->
            error $ $(curLoc) ++ "IsActiveEnable: Expected Bool or Enable, not: " ++ show ty

      (ActiveEdge edgeRequested n) -> do
        let (_, ty, _) = bbInputs b !! n
        domConf <- getDomainConf ty
        case domConf of
          VDomainConfiguration _ _ edgeActual _ _ _ -> pure $
            if edgeRequested == edgeActual then 1 else 0

      (IsSync n) -> do
        let (_, ty, _) = bbInputs b !! n
        domConf <- getDomainConf ty
        case domConf of
          VDomainConfiguration _ _ _ Synchronous _ _ -> pure 1
          VDomainConfiguration _ _ _ Asynchronous _ _ -> pure 0

      (IsInitDefined n) -> do
        let (_, ty, _) = bbInputs b !! n
        domConf <- getDomainConf ty
        case domConf of
          VDomainConfiguration _ _ _ _ Defined _ -> pure 1
          VDomainConfiguration _ _ _ _ Unknown _ -> pure 0

      (IsActiveHigh n) -> do
        let (_, ty, _) = bbInputs b !! n
        domConf <- getDomainConf ty
        case domConf of
          VDomainConfiguration _ _ _ _ _ ActiveHigh -> pure 1
          VDomainConfiguration _ _ _ _ _ ActiveLow  -> pure 0

      (StrCmp [Text t1] n) -> pure $
        let (e,_,_) = bbInputs b !! n
        in  case exprToString e of
              Just t2
                | t1 == Text.pack t2 -> 1
                | otherwise -> 0
              Nothing -> error $ $(curLoc) ++ "Expected a string literal: " ++ show e
      (And es)   -> do
        es' <- mapM (check xOpt iw hdl syn enums) es
        pure $ if all (/=0) es'
                       then 1
                       else 0
      CmpLE e1 e2 -> do
        v1 <- check xOpt iw hdl syn enums e1
        v2 <- check xOpt iw hdl syn enums e2
        if v1 <= v2
          then pure 1
          else pure 0
      _ -> error $ $(curLoc) ++ "IF: condition must be: SIZE, LENGTH, LIT, DEPTH, IW64, VIVADO, OTHERSYN, ISVAR, ISLIT, ISUNDEFINED, ISACTIVEENABLE, ACTIVEEDGE, ISSYNC, ISINITDEFINED, ISACTIVEHIGH, STRCMP, AND, ISSCALAR or CMPLE."
                             ++ "\nGot: " ++ show c'
renderElem b e = fmap const (renderTag b e)

getDomainConf :: (Backend backend, HasCallStack) => HWType -> State backend VDomainConfiguration
getDomainConf = generalGetDomainConf domainConfigurations

generalGetDomainConf
  :: forall m. (Monad m, HasCallStack)
  => (m DomainMap) -- ^ a way to get the `DomainMap`
  -> HWType -> m VDomainConfiguration
generalGetDomainConf getDomainMap ty = case (snd . stripAttributes . stripVoid) ty of
  KnownDomain dom period activeEdge resetKind initBehavior resetPolarity ->
    pure $ VDomainConfiguration (Data.Text.unpack dom) (fromIntegral period) activeEdge resetKind initBehavior resetPolarity

  Clock dom -> go dom
  ClockN dom -> go dom
  Reset dom  -> go dom
  Enable dom -> go dom
  Product _DiffClock _ [Clock dom,_clkN] -> go dom
  t -> error $ "Don't know how to get a Domain out of HWType: " <> show t
 where
  go :: HasCallStack => N.DomainName -> m VDomainConfiguration
  go dom = do
    doms <- getDomainMap
    case HashMap.lookup dom doms of
      Nothing -> error $ "Can't find domain " <> show dom <> ". Please report an issue at https://github.com/clash-lang/clash-compiler/issues."
      Just conf -> pure conf

parseFail :: BlackBoxContext -> Text -> BlackBoxTemplate
parseFail b t = case runParse t of
  Failure errInfo ->
    error $ unlines
        [ "error while parsing blackbox: " <> Data.Text.unpack (bbName b)
        , "in component " <> Data.Text.unpack (Id.toText $ bbCompName b)
        , "error:"
        , show (_errDoc errInfo)
        ]
  Success templ -> templ

idToExpr
  :: (Text, HWType)
  -> (Expr, HWType, Bool)
idToExpr (t, ty) =
  (Identifier (Id.unsafeMake (Text.toStrict t)) Nothing, ty, False)

bbResult :: HasCallStack => String -> BlackBoxContext -> (Expr, HWType)
bbResult _s (bbResults -> [r]) = r
bbResult s ctx = error [I.i|
  Multi result primitives not supported when using template tag #{s}. Tag used
  in blackbox implementation of #{bbName ctx} |]

-- | Fill out the template corresponding to an output/input assignment of a
-- component instantiation, and turn it into a single identifier so it can
-- be used for a new blackbox context.
lineToIdentifier :: Backend backend
                 => BlackBoxContext
                 -> BlackBoxTemplate
                 -> State backend Text
lineToIdentifier b = foldrM (\e a -> do
                              e' <- renderTag b e
                              return (e' `Text.append` a)
                   ) Text.empty

lineToType :: BlackBoxContext
           -> BlackBoxTemplate
           -> HWType
lineToType b [(Typ Nothing)]  = snd $ bbResult "~TYPO" b
lineToType b [(Typ (Just n))] = let (_,ty,_) = bbInputs b !! n
                                in  ty
lineToType b [(TypElem t)]    = case lineToType b [t] of
                                  Vector _ elTy -> elTy
                                  MemBlob _ m -> BitVector m
                                  _ -> error $ $(curLoc) ++ "Element type selection of a non-vector-like type"
lineToType b [(IndexType (Lit n))] =
  case bbInputs b !! n of
    (Literal _ (NumLit n'),_,_) -> Index (fromInteger n')
    x -> error $ $(curLoc) ++ "Index type not given a literal: " ++ show x

lineToType _ _ = error $ $(curLoc) ++ "Unexpected type manipulation"

-- | Give a context and a tagged hole (of a template), returns part of the
-- context that matches the tag of the hole.
renderTag :: Backend backend
          => BlackBoxContext
          -> Element
          -> State backend Text
renderTag _ (Text t)        = return t
renderTag b (Result)    = do
  fmap renderOneLine . getAp . expr False . fst $ bbResult "~RESULT" b
renderTag b (Arg n)  = do
  let (e,_,_) = bbInputs b !! n
  renderOneLine <$> getAp (expr True e)

renderTag b (Const n)  = do
  let (e,_,_) = bbInputs b !! n
  renderOneLine <$> getAp (expr False e)

renderTag b t@(ArgGen k n)
  | k == bbLevel b
  , let (e,_,_) = bbInputs b !! n
  = renderOneLine <$> getAp (expr False e)
  | otherwise
  = getAp (prettyElem t)

renderTag b (Lit n) =
  renderOneLine <$> getAp (expr False (mkLit e))
 where
  (e,_,_) = bbInputs b !! n

  mkLit (Literal (Just (Signed _,_)) i)                                 = Literal Nothing i  -- Integer, Int#
  mkLit (Literal (Just (Unsigned _,_)) i)                               = Literal Nothing i  -- KnownNat, Natural, Word#

  mkLit (DataCon _ (DC (Void {}, _)) [Literal (Just (Signed _,_)) i])   = Literal Nothing i  -- Int
  mkLit (DataCon _ (DC (Void {}, _)) [Literal (Just (Unsigned _,_)) i]) = Literal Nothing i  -- SNat, Word

  mkLit (BlackBoxE pNm _ _ _ _ bbCtx _) | pNm `elem` ["GHC.Int.I8#", "GHC.Int.I16#", "GHC.Int.I32#", "GHC.Int.I64#"
                                                     ,"GHC.Word.W8#","GHC.Word.W16#","GHC.Word.W32#","GHC.Word.W64#"
                                                     ,"GHC.Types.I#","GHC.Types.W#"
                                                     ]
                                        , [Literal _ i] <- extractLiterals bbCtx
                                        = Literal Nothing i
  mkLit (BlackBoxE pNm _ _ _ _ bbCtx _) | pNm `elem` ["Clash.Sized.Internal.Signed.fromInteger#"
                                                     ,"Clash.Sized.Internal.Unsigned.fromInteger#"
                                                     ,"Clash.Sized.Internal.Index.fromInteger#"]
                                        , [Literal {}, Literal _ i] <- extractLiterals bbCtx
                                         = Literal Nothing i
  mkLit i                                                               = i

renderTag b e@(Name _i) =
  case elementToText b e of
      Right s  -> return s
      Left msg -> error $ $(curLoc) ++ unwords [ "Error when reducing to string"
                                               , "in ~NAME construct:", msg ]

renderTag _ (ToVar [Text t] _) = return t
renderTag _ (Sym t _) = return t

renderTag b (BV True es e) = do
  e' <- Text.concat <$> mapM (fmap ($ 0) . renderElem b) es
  let ty = lineToType b [e]
  renderOneLine <$> getAp (toBV ty e')
renderTag b (BV False es e) = do
  e' <- Text.concat <$> (mapM (fmap ($ 0) . renderElem b) es)
  let ty = lineToType b [e]
  renderOneLine <$> getAp (fromBV ty e')

renderTag b (Sel e n) =
  let ty = lineToType b [e]
  in  renderOneLine <$> getAp (hdlRecSel ty n)

renderTag b (Typ Nothing)   = fmap renderOneLine . getAp . hdlType Internal . snd $ bbResult "~TYPO" b
renderTag b (Typ (Just n))  = let (_,ty,_) = bbInputs b !! n
                              in  renderOneLine <$> getAp (hdlType Internal ty)
renderTag b (TypM Nothing)  = fmap renderOneLine . getAp . hdlTypeMark . snd $ bbResult "~TYPMO" b
renderTag b (TypM (Just n)) = let (_,ty,_) = bbInputs b !! n
                              in  renderOneLine <$> getAp (hdlTypeMark ty)
renderTag b (Err Nothing)   = fmap renderOneLine . getAp . hdlTypeErrValue . snd $ bbResult "~ERRORO" b
renderTag b (Err (Just n))  = let (_,ty,_) = bbInputs b !! n
                              in  renderOneLine <$> getAp (hdlTypeErrValue ty)
renderTag b (Size e)        = return . Text.pack . show . typeSize $ lineToType b [e]

renderTag b (Length e) = return . Text.pack . show . vecLen $ lineToType b [e]
  where
    vecLen (Vector n _)                = n
    vecLen (Void (Just (Vector n _)))  = n
    vecLen (MemBlob n _)               = n
    vecLen (Void (Just (MemBlob n _))) = n
    vecLen thing =
      error $ $(curLoc) ++ "vecLen of a non-vector-like type: " ++ show thing

renderTag b (Depth e) = return . Text.pack . show . treeDepth $ lineToType b [e]
  where
    treeDepth (RTree n _)               = n
    treeDepth (Void (Just (RTree n _))) = n
    treeDepth thing =
      error $ $(curLoc) ++ "treeDepth of a non-tree type: " ++ show thing

renderTag b (MaxIndex e) = return . Text.pack . show . vecLen $ lineToType b [e]
  where
    vecLen (Vector n _)  = n-1
    vecLen (MemBlob n _) = n-1
    vecLen thing =
      error $ $(curLoc) ++ "vecLen of a non-vector-like type: " ++ show thing

renderTag b e@(TypElem _)   = let ty = lineToType b [e]
                              in  renderOneLine <$> getAp (hdlType Internal ty)
renderTag _ (Gen b)         = renderOneLine <$> genStmt b
renderTag _ (GenSym [Text t] _) = return t

-- Determine variables used in argument /n/.
renderTag b (Vars n) = return $ vars'
  where
    (e, _, _) = bbInputs b !! n
    vars      = map Text.fromStrict (usedVariables e)
    vars'     = Text.concat (map (Text.cons ',') vars)

renderTag b (IndexType (Lit n)) =
  case bbInputs b !! n of
    (Literal _ (NumLit n'),_,_) ->
      let hty = Index (fromInteger n')
      in  fmap renderOneLine (getAp (hdlType Internal hty))
    x -> error $ $(curLoc) ++ "Index type not given a literal: " ++ show x
renderTag b (FilePath e)    = case e of
  Lit n -> do
    let (e',_,_) = bbInputs b !! n
    case exprToString e' of
      Just s -> do
        s' <- addAndSetData s
        return (Text.pack (show s'))
      _ -> do
        e2  <- getAp (prettyElem e)
        error $ $(curLoc) ++ "argument of ~FILE:" ++ show e2 ++  "does not reduce to a string"
  _ -> do e' <- getAp (prettyElem e)
          error $ $(curLoc) ++ "~FILE expects a ~LIT[N] argument, but got: " ++ show e'
renderTag b (IncludeName n) = case indexMaybe (bbQsysIncName b) n of
  Just nm -> return (Text.fromStrict nm)
  _ -> error $ $(curLoc) ++ "~INCLUDENAME[" ++ show n ++ "] does not correspond to any index of the 'includes' field that is specified in the primitive definition"
renderTag b (OutputUsage n) = do
  hdl <- gets hdlKind

  let u = case IntMap.lookup n (bbFunctions b) of
            Just ((_,u',_,_,_,_):_) -> u'
            _ -> error $ $(curLoc) ++ "~OUTPUTUSAGE[" ++ show n ++ "] used where argument " ++ show n ++ " is not a function"

  pure $ case (hdl, u) of
    (VHDL, N.Proc N.Blocking) -> "variable"
    (VHDL, _) -> "signal"

    (_, N.Cont) -> "wire"
    (_, _) -> "reg"
renderTag b (Repeat [es] [i]) = do
  i'  <- Text.unpack <$> renderTag b i
  es' <- renderTag b es
  let i'' = case (readEither i' :: Either String Int) of
              Left msg -> error $ $(curLoc) ++ "Could not parse " ++ show i' ++ ". read reported: " ++ msg ++ "."
              Right n  -> n
  return $ Text.concat $ take i'' $ repeat es'

renderTag b (DevNull es) = do
  _ <- mapM (renderElem b) es
  return $ Text.empty

renderTag b (Template filenameL sourceL) = case file of
  Left msg ->
      error $ $(curLoc) ++ unwords [ "Name or source in ~TEMPLATE construct"
                                   , "did not reduce to a string."
                                   , "'elementToText' reported:"
                                   , msg ]
  Right fstup@(filename, _source) -> do
    fs <- getMemoryDataFiles
    if elem filename (map fst fs)
      then if not (elem fstup fs)
        then error $ $(curLoc) ++ unwords [ "Multiple ~TEMPLATE constructs"
                                           , "specifiy the same filename"
                                           , "but different contents. Make"
                                           , "sure these names are unique." ]
      -- We replace the Template element with an empty constant, so nothing
      -- ends up in the generated HDL.
        else return (Text.pack "")
      else do
        addMemoryDataFile fstup
        return (Text.pack "")

  where
      file = do
          filename <- elementsToText b filenameL
          source   <- elementsToText b sourceL
          return (Text.unpack filename, Text.unpack source)

renderTag b CompName = pure (Id.toLazyText (bbCompName b))

renderTag b CtxName = case bbCtxName b of
  Just nm -> return (Text.fromStrict nm)
  _ | Identifier t _ <- fst (bbResult "~CTXNAME" b)
    -> return (Id.toLazyText t)
  _ -> error "internal error"


renderTag _ e = do e' <- getAp (prettyElem e)
                   error $ $(curLoc) ++ "Unable to evaluate: " ++ show e'

-- | Compute string from a list of elements. Can interpret ~NAME string literals
-- on template level (constants).
elementsToText
    :: BlackBoxContext
    -> [Element]
    -> Either String Text
elementsToText bbCtx elements =
    foldl (\txt el -> case txt of
                          -- Append new string (if no error) to string so far
                          Right s -> (Text.append s) <$> elementToText bbCtx el
                          -- If previous iteration resulted in an error: stop.
                          msg -> msg) (Right $ Text.pack "") elements

elementToText
    :: BlackBoxContext
    -> Element
    -> Either String Text
elementToText bbCtx  (Name n) = elementToText bbCtx (Lit n)
elementToText _bbCtx (Text t) = return $ t
elementToText bbCtx  (Lit n) =
    case bbInputs bbCtx ^? element n of
        Just (e,_,_) ->
            case exprToString e of
                Just t ->
                    Right $ Text.pack t
                Nothing ->
                    Left $ $(curLoc) ++ unwords [ "Could not extract string from"
                                                , show e, "referred to by"
                                                , show (Lit n) ]
        Nothing ->
            Left $ $(curLoc) ++ unwords [ "Invalid literal", show (Lit n)
                                        , "used in blackbox with context:"
                                        , show bbCtx, "." ]

elementToText _bbCtx e = error $ "Unexpected string like: " ++ show e

-- | Extracts string from SSymbol or string literals
exprToString
  :: Expr
  -> Maybe String
exprToString (Literal _ (NumLit i)) = Just (show i)
exprToString (Literal _ (StringLit l)) = Just l
exprToString (BlackBoxE "Clash.Promoted.Symbol.SSymbol" _ _ _ _ ctx _) =
  case bbInputs ctx of
    (e0,_,_):_ -> exprToString e0
    _ -> error "internal error: insufficient bbInputs"
exprToString (BlackBoxE "GHC.CString.unpackCString#" _ _ _ _ ctx _) =
  case bbInputs ctx of
    (e0,_,_):_ -> exprToString e0
    _ -> error "internal error: insufficient bbInputs"
exprToString _ = Nothing

prettyBlackBox :: Monad m
               => BlackBoxTemplate
               -> Ap m Text
prettyBlackBox bbT = Text.concat <$> mapM prettyElem bbT

prettyElem
  :: (HasCallStack, Monad m)
  => Element
  -> Ap m Text
prettyElem (Text t) = return t
prettyElem (Component (Decl i 0 args)) = do
  args' <- mapM (\(a,b) -> (,) <$> prettyBlackBox a <*> prettyBlackBox b) args
  case args' of
    (arg:rest) ->
      renderOneLine <$>
        (nest 2 (string "~INST" <+> int i <> line <>
            string "~OUTPUT" <+> string "=>" <+> string (fst arg) <+> string (snd arg) <+> string "~" <> line <>
            vcat (mapM (\(a,b) -> string "~INPUT" <+> string "=>" <+> string a <+> string b <+> string "~") rest))
          <> line <> string "~INST")
    _ -> error "internal error: insufficient args"
prettyElem (Component (Decl {})) =
  error $ $(curLoc) ++ "prettyElem can't (yet) render ~INST when subfuncion /= 0!"
prettyElem Result = return "~RESULT"
prettyElem (Arg i) = renderOneLine <$> ("~ARG" <> brackets (int i))
prettyElem (Lit i) = renderOneLine <$> (string "~LIT" <> brackets (int i))
prettyElem (Const i) = renderOneLine <$> (string "~CONST" <> brackets (int i))
prettyElem (Name i) = renderOneLine <$> (string "~NAME" <> brackets (int i))
prettyElem (ToVar es i) = do
  es' <- prettyBlackBox es
  renderOneLine <$> (string "~VAR" <> brackets (string es') <> brackets (int i))
prettyElem (Sym _ i) = renderOneLine <$> (string "~SYM" <> brackets (int i))
prettyElem (Typ Nothing) = return "~TYPO"
prettyElem (Typ (Just i)) = renderOneLine <$> (string "~TYP" <> brackets (int i))
prettyElem (TypM Nothing) = return "~TYPMO"
prettyElem (TypM (Just i)) = renderOneLine <$> (string "~TYPM" <> brackets (int i))
prettyElem (Err Nothing) = return "~ERRORO"
prettyElem (Err (Just i)) = renderOneLine <$> (string "~ERROR" <> brackets (int i))
prettyElem (TypElem e) = do
  e' <- prettyElem e
  renderOneLine <$> (string "~TYPEL" <> brackets (string e'))
prettyElem CompName = return "~COMPNAME"
prettyElem (IncludeName i) = renderOneLine <$> ("~INCLUDENAME" <> brackets (int i))
prettyElem (IndexType e) = do
  e' <- prettyElem e
  renderOneLine <$> (string "~INDEXTYPE" <> brackets (string e'))
prettyElem (Size e) = do
  e' <- prettyElem e
  renderOneLine <$> (string "~SIZE" <> brackets (string e'))
prettyElem (Length e) = do
  e' <- prettyElem e
  renderOneLine <$> (string "~LENGTH" <> brackets (string e'))
prettyElem (Depth e) = do
  e' <- prettyElem e
  renderOneLine <$> (string "~DEPTH" <> brackets (string e'))
prettyElem (MaxIndex e) = do
  e' <- prettyElem e
  renderOneLine <$> (string "~MAXINDEX" <> brackets (string e'))
prettyElem (FilePath e) = do
  e' <- prettyElem e
  renderOneLine <$> (string "~FILE" <> brackets (string e'))
prettyElem (Gen b) = if b then return "~GENERATE" else return "~ENDGENERATE"
prettyElem (IF b esT esF) = do
  b' <- prettyElem b
  esT' <- prettyBlackBox esT
  esF' <- prettyBlackBox esF
  (renderLazy . layoutCompact) <$>
    (string "~IF" <+> string b' <+> string "~THEN" <>
     string esT' <>
     string "~ELSE" <>
     string esF' <>
     string "~FI")
prettyElem (And es) = renderOneLine <$>
  (string "~AND" <>
  (brackets (hcat (punctuate comma (mapM (string <=< prettyElem) es)))))
prettyElem (CmpLE e1 e2) = do
  e1' <- prettyElem e1
  e2' <- prettyElem e2
  renderOneLine <$> (string "~CMPLE" <> brackets (string e1')
                                     <> brackets (string e2'))
prettyElem IW64 = return "~IW64"
prettyElem (HdlSyn s) = case s of
  Vivado -> return "~VIVADO"
  _      -> return "~OTHERSYN"
prettyElem (BV b es e) = do
  es' <- prettyBlackBox es
  e'  <- prettyBlackBox [e]
  renderOneLine <$>
    if b
       then string "~TOBV" <> brackets (string es') <> brackets (string e')
       else string "~FROMBV" <> brackets (string es') <> brackets (string e')
prettyElem (Sel e i) = do
  e' <- prettyElem e
  renderOneLine <$> (string "~SEL" <> brackets (string e') <> brackets (int i))
prettyElem (IsLit i) = renderOneLine <$> (string "~ISLIT" <> brackets (int i))
prettyElem (IsVar i) = renderOneLine <$> (string "~ISVAR" <> brackets (int i))
prettyElem (IsScalar i) = renderOneLine <$> (string "~ISSCALAR" <> brackets (int i))
prettyElem (IsActiveHigh i) = renderOneLine <$> (string "~ISACTIVEHIGH" <> brackets (int i))
prettyElem (IsActiveEnable i) = renderOneLine <$> (string "~ISACTIVEENABLE" <> brackets (int i))
prettyElem (IsUndefined i) = renderOneLine <$> (string "~ISUNDEFINED" <> brackets (int i))

-- Domain attributes:
prettyElem (Tag i) = renderOneLine <$> (string "~TAG" <> brackets (int i))
prettyElem (Period i) = renderOneLine <$> (string "~PERIOD" <> brackets (int i))
prettyElem LongestPeriod = return "~LONGESTPERIOD"
prettyElem (ActiveEdge e i) = renderOneLine <$> (string "~ACTIVEEDGE" <> brackets (string (Text.pack (show e))) <> brackets (int i))
prettyElem (IsSync i) = renderOneLine <$> (string "~ISSYNC" <> brackets (int i))
prettyElem (IsInitDefined i) = renderOneLine <$> (string "~ISINITDEFINED" <> brackets (int i))

prettyElem (StrCmp es i) = do
  es' <- prettyBlackBox es
  renderOneLine <$> (string "~STRCMP" <> brackets (string es') <> brackets (int i))
prettyElem (GenSym es i) = do
  es' <- prettyBlackBox es
  renderOneLine <$> (string "~GENSYM" <> brackets (string es') <> brackets (int i))
prettyElem (Repeat [es] [i]) = do
  es' <- prettyElem es
  i'  <- prettyElem i
  renderOneLine
    <$> string "~REPEAT"
    <>  brackets (string es')
    <>  brackets (string i')
prettyElem (Repeat es i) = error $ $(curLoc)
                                ++ "Unexpected number of arguments in either "
                                ++ show es
                                ++ " or "
                                ++ show i
                                ++ ". Both lists are expected to have a single element."
prettyElem (DevNull es) = do
  es' <- mapM prettyElem es
  renderOneLine <$> (string "~DEVNULL" <> brackets (string $ Text.concat es'))

prettyElem (SigD es mI) = do
  es' <- prettyBlackBox es
  renderOneLine <$>
    (maybe (string "~SIGDO" <> brackets (string es'))
           (((string "~SIGD" <> brackets (string es')) <>) . int)
           mI)
prettyElem (Vars i) = renderOneLine <$> (string "~VARS" <> brackets (int i))
prettyElem (OutputUsage n) = renderOneLine <$> (string "~OUTPUTUSAGE" <> brackets (int n))
prettyElem (ArgGen n x) =
  renderOneLine <$> (string "~ARGN" <> brackets (int n) <> brackets (int x))
prettyElem (Template bbname source) = do
  bbname' <- mapM prettyElem bbname
  source' <- mapM prettyElem source
  renderOneLine <$> (string "~TEMPLATE"
                                  <> brackets (string $ Text.concat bbname')
                                  <> brackets (string $ Text.concat source'))
prettyElem CtxName = return "~CTXNAME"

-- | Recursively walk @Element@, applying @f@ to each element in the tree.
walkElement
  :: (Element -> Maybe a)
  -> Element
  -> [a]
walkElement f el = maybeToList (f el) ++ walked
  where
    go     = walkElement f
    walked =
      -- TODO: alternatives are purposely explicitly listed in case @Element@
      -- TODO: gets extended. This way, GHC will complain about missing
      -- TODO: alternatives. It would probably be better to replace it by Lens
      -- TODO: logic?
      case el of
        Component (Decl _ _ args) ->
          concatMap (\(a,b) -> concatMap go a ++ concatMap go b) args
        IndexType e -> go e
        FilePath e -> go e
        Template bbname source ->
          concatMap go bbname ++ concatMap go source
        IF b esT esF ->
          go b ++ concatMap go esT ++ concatMap go esF
        SigD es _ -> concatMap go es
        BV _ es _ -> concatMap go es
        GenSym es _ -> concatMap go es
        DevNull es -> concatMap go es
        Text _ -> []
        Result -> []
        Arg _ -> []
        ArgGen _ _ -> []
        Const _ -> []
        Lit _ -> []
        Name _ -> []
        ToVar es _ -> concatMap go es
        Sym _ _ -> []
        Typ _ -> []
        TypM _ -> []
        Err _ -> []
        TypElem e -> go e
        CompName -> []
        IncludeName _ -> []
        Size e -> go e
        Length e -> go e
        Depth e -> go e
        MaxIndex e -> go e
        Gen _ -> []
        And es -> concatMap go es
        CmpLE e1 e2 -> go e1 ++ go e2
        IW64 -> []
        HdlSyn _ -> []
        Sel e _ -> go e
        IsLit _ -> []
        IsVar _ -> []
        IsScalar _ -> []
        Tag _ -> []
        Period _ -> []
        LongestPeriod -> []
        ActiveEdge _ _ -> []
        IsSync _ -> []
        IsInitDefined _ -> []
        IsActiveHigh _ -> []
        IsActiveEnable _ -> []
        IsUndefined _ -> []
        StrCmp es _ -> concatMap go es
        OutputUsage _ -> []
        Vars _ -> []
        Repeat es1 es2 ->
          concatMap go es1 ++ concatMap go es2
        CtxName -> []

-- | Determine variables used in an expression. Used for VHDL sensitivity list.
-- Also see: https://github.com/clash-lang/clash-compiler/issues/365
usedVariables :: Expr -> [N.IdentifierText]
usedVariables Noop              = []
usedVariables (Identifier i _)  = [Id.toText i]
usedVariables (DataCon _ _ es)  = concatMap usedVariables es
usedVariables (DataTag _ e')    = [Id.toText (either id id e')]
usedVariables (Literal {})      = []
usedVariables (ToBv _ _ e') = usedVariables e'
usedVariables (FromBv _ _ e') = usedVariables e'
usedVariables (IfThenElse e1 e2 e3) = concatMap usedVariables [e1,e2,e3]
usedVariables (BlackBoxE _ _ _ _ t bb _) = nub (sList ++ sList')
  where
    matchArg (Arg i) = Just i
    matchArg _         = Nothing

    matchVar (ToVar [Text v] _) = Just (Text.toStrict v)
    matchVar _                  = Nothing

    t'     = onBlackBox id (\_ _ _ -> []) t
    usedIs = mapMaybe (indexMaybe (bbInputs bb)) (concatMap (walkElement matchArg) t')
    sList  = concatMap (\(e,_,_) -> usedVariables e) usedIs
    sList' = concatMap (walkElement matchVar) t'

-- | Collect arguments (e.g., ~ARG, ~LIT) used in this blackbox
getUsedArguments :: N.BlackBox -> [Int]
getUsedArguments (N.BBFunction _nm _hsh (N.TemplateFunction k _ _)) = k
getUsedArguments (N.BBTemplate t) = nub (concatMap (walkElement matchArg) t)
  where
    matchArg =
      \case
        Arg i -> Just i
        Component (Decl i _ _) -> Just i
        Const i -> Just i
        IsLit i -> Just i
        IsActiveEnable i -> Just i
        IsUndefined i -> Just i
        Lit i -> Just i
        Name i -> Just i
        ToVar _ i -> Just i

        -- Domain properties (only need type):
        IsInitDefined _ -> Nothing
        ActiveEdge _ _ -> Nothing
        IsSync _ -> Nothing
        Period _ -> Nothing
        LongestPeriod -> Nothing
        Tag _ -> Nothing

        -- Others. Template tags only using types of arguments can be considered
        -- "not used".
        And _ -> Nothing
        ArgGen _ _ -> Nothing
        BV _ _ _ -> Nothing
        CmpLE _ _ -> Nothing
        CompName -> Nothing
        Depth _ -> Nothing
        DevNull _ -> Nothing
        Err _ -> Nothing
        FilePath _ -> Nothing
        Gen _ -> Nothing
        GenSym _ _ -> Nothing
        HdlSyn _ -> Nothing
        IF _ _ _ -> Nothing
        IncludeName _ -> Nothing
        IndexType _ -> Nothing
        IsActiveHigh _ -> Nothing
        IsVar _ -> Nothing
        IsScalar _ -> Nothing
        IW64 -> Nothing
        Length _ -> Nothing
        MaxIndex _ -> Nothing
        OutputUsage _ -> Nothing
        Repeat _ _ -> Nothing
        Result -> Nothing
        Sel _ _ -> Nothing
        SigD _ _ -> Nothing
        Size _ -> Nothing
        StrCmp _ _ -> Nothing
        Sym _ _ -> Nothing
        Template _ _ -> Nothing
        Text _ -> Nothing
        Typ _ -> Nothing
        TypElem _ -> Nothing
        TypM _ -> Nothing
        Vars _ -> Nothing
        CtxName -> Nothing

onBlackBox
  :: (BlackBoxTemplate -> r)
  -> (N.BBName -> N.BBHash -> N.TemplateFunction -> r)
  -> N.BlackBox
  -> r
onBlackBox f _ (N.BBTemplate t) = f t
onBlackBox _ g (N.BBFunction n h t) = g n h t

-- | Is the value of the 'Expr' fully undefined?
checkUndefined :: Expr -> Bool
checkUndefined = \case
  BlackBoxE _ _ _ _ (N.BBTemplate [Err _]) _ _ -> True
  BlackBoxE "Clash.Sized.Internal.BitVector.fromInteger#" [] [] [] _ bbCtx _
    | [sz, mask, _val] <- bbInputs bbCtx
    , (Literal _ (NumLit sz0), _, True) <- sz
    , (Literal _ (NumLit mask0), _, True) <- mask
    , mask0 == 2^sz0 - 1
    -> True
  DataCon (Product _ _ _) _ es -> and (map checkUndefined es)
  _ -> False
