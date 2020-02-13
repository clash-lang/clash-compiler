{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utilties to verify blackbox contexts against templates and rendering filled
  in templates
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Netlist.BlackBox.Util where

import           Control.Exception               (throw)
import           Control.Lens
  (use, (%=), _1, _2, element, (^?))
import           Control.Monad                   (forM)
import           Control.Monad.State             (State, StateT (..), lift)
import           Data.Bool                       (bool)
import           Data.Foldable                   (foldrM)
import           Data.Hashable                   (Hashable (..))
import qualified Data.IntMap                     as IntMap
import           Data.List                       (nub)
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid
#endif
import           Data.Maybe                      (mapMaybe, maybeToList, fromJust)
import           Data.Semigroup.Monad
import qualified Data.Text
import           Data.Text.Lazy                  (Text)
import qualified Data.Text.Lazy                  as Text
import qualified Data.Text.Prettyprint.Doc       as PP
import           Data.Text.Prettyprint.Doc.Extra
import           System.FilePath                 (replaceBaseName, takeBaseName,
                                                  takeFileName, (<.>))
import           Text.Printf
import           Text.Read                       (readEither)
import           Text.Trifecta.Result            hiding (Err)

import           Clash.Backend                   (Backend (..), Usage (..), mkUniqueIdentifier)
import qualified Clash.Backend                   as Backend
import           Clash.Netlist.BlackBox.Parser
import           Clash.Netlist.BlackBox.Types
import           Clash.Netlist.Id                (IdType (..))
import           Clash.Netlist.Types             (BlackBoxContext (..),
                                                  Expr (..), HWType (..),
                                                  Identifier, Literal (..),
                                                  Modifier (..),
                                                  Declaration(BlackBoxD))
import qualified Clash.Netlist.Types             as N
import           Clash.Netlist.Util              (typeSize, isVoid)
import           Clash.Signal.Internal
  (ResetKind(..), ResetPolarity(..), InitBehavior(..))
import           Clash.Util
import           GHC.FastString.Extra

-- | Strip as many "Void" layers as possible. Might still return a Void if the
-- void doesn't contain a hwtype.
stripVoid :: HWType -> HWType
stripVoid (Void (Just e)) = stripVoid e
stripVoid e = e

inputHole :: Element -> Maybe Int
inputHole = \case
  Arg _ n       -> pure n
  Lit n         -> pure n
  Const n       -> pure n
  Name n        -> pure n
  Typ (Just n)  -> pure n
  TypM (Just n) -> pure n
  Err (Just n)  -> pure n
  _             -> Nothing

-- | Determine if the number of normal/literal/function inputs of a blackbox
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
                Nothing ->
                  Just ( "Blackbox required at least " ++ show (n+1)
                      ++ " arguments, but only " ++ show (length (bbInputs bbCtx))
                      ++ " were passed." )

extractLiterals :: BlackBoxContext
                -> [Expr]
extractLiterals = map (\case (e,_,_) -> e)
                . filter (\case (_,_,b) -> b)
                . bbInputs

-- | Update all the symbol references in a template, and increment the symbol
-- counter for every newly encountered symbol.
setSym
  :: forall m
   . Monad m
  => (IdType -> Identifier -> m Identifier)
  -> BlackBoxContext
  -> BlackBoxTemplate
  -> m (BlackBoxTemplate,[N.Declaration])
setSym mkUniqueIdentifierM bbCtx l = do
    (a,(_,decls)) <- runStateT (mapM setSym' l) (IntMap.empty,IntMap.empty)
    return (a,concatMap snd (IntMap.elems decls))
  where
    bbnm = unpackFS (bbName bbCtx)

    setSym'
      :: Element
      -> StateT ( IntMap.IntMap Identifier
                , IntMap.IntMap (Identifier,[N.Declaration]))
                m
                Element
    setSym' e = case e of
      Var nm i | i < length (bbInputs bbCtx) -> case bbInputs bbCtx !! i of
        (Identifier nm' Nothing,_,_) ->
          return (Var [Text (Text.fromStrict nm')] i)

        (e',hwTy,_) -> do
          varM <- IntMap.lookup i <$> use _2
          case varM of
            Nothing -> do
              nm' <- lift (mkUniqueIdentifierM Extended (Text.toStrict (concatT (Text "c$":nm))))
              let decls = case typeSize hwTy of
                    0 -> []
                    _ -> [N.NetDecl Nothing nm' hwTy
                         ,N.Assignment nm' e'
                         ]
              _2 %= (IntMap.insert i (nm',decls))
              return (Var [Text (Text.fromStrict nm')] i)
            Just (nm',_) -> return (Var [Text (Text.fromStrict nm')] i)
      Sym _ i -> do
        symM <- IntMap.lookup i <$> use _1
        case symM of
          Nothing -> do
            t <- lift (mkUniqueIdentifierM Extended "c$n")
            _1 %= (IntMap.insert i t)
            return (Sym (Text.fromStrict t) i)
          Just t -> return (Sym (Text.fromStrict t) i)
      GenSym t i -> do
        symM <- IntMap.lookup i <$> use _1
        case symM of
          Nothing -> do
            t' <- lift (mkUniqueIdentifierM Basic (Text.toStrict (concatT t)))
            _1 %= (IntMap.insert i t')
            return (GenSym [Text (Text.fromStrict t')] i)
          Just _ ->
            error ("Symbol #" ++ show (t,i)
                ++ " is already defined in BlackBox for: "
                ++ bbnm)
      Component (Decl n subN l') ->
        Component <$> (Decl n subN <$> mapM (combineM (mapM setSym') (mapM setSym')) l')
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
        Result _ | Identifier t _ <- fst (bbResult bbCtx) -> Text.fromStrict t
        CompName -> Text.fromStrict (bbCompName bbCtx)
        CtxName ->
          case bbCtxName bbCtx of
            Just nm -> Text.fromStrict nm
            _ | Identifier t _ <- fst (bbResult bbCtx) -> Text.fromStrict t
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
      let bbCtx' = bbCtx {bbQsysIncName = map Text.toStrict nms'}
      incForHash <- onBlackBox (renderTemplate bbCtx')
                               (\_name _hash (N.TemplateFunction _ _ f) -> do
                                  t <- f bbCtx'
                                  let t' = renderLazy (layoutPretty layout t)
                                  return (const t'))
                               inc
      iw <- iwWidth
      let incHash = hash (incForHash 0)
          nm'     = Text.concat
                      [ Text.fromStrict nm
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
  (o,oTy,_) <- idToExpr <$> combineM (lineToIdentifier b) (return . lineToType b) l
  is <- mapM (fmap idToExpr . combineM (lineToIdentifier b) (return . lineToType b)) ls
  let func0 = IntMap.lookup n (bbFunctions b)
      errr = concat [ "renderElem: not enough functions rendered? Needed "
                    , show (subN +1 ), " got only ", show (length (fromJust func0)) ]
      func1 = indexNote' errr subN <$> func0
      Just (templ0,_,libs,imps,inc,pCtx) = func1
      b' = pCtx { bbResult = (o,oTy), bbInputs = bbInputs pCtx ++ is }
      layoutOptions = LayoutOptions (AvailablePerLine 120 0.4)
      render = N.BBTemplate . parseFail . renderLazy . layoutPretty layoutOptions

  templ1 <-
    case templ0 of
      Left t ->
        return t
      Right (nm0,ds) -> do
        nm1 <- mkUniqueIdentifier Basic nm0
        block <- getMon (blockDecl nm1 ds)
        return (render block)

  templ4 <-
    case templ1 of
      N.BBFunction {} ->
        return templ1
      N.BBTemplate templ2 -> do
        (templ3, templDecls) <- setSym Backend.mkUniqueIdentifier b' templ2
        case templDecls of
          [] ->
            return (N.BBTemplate templ3)
          _ -> do
            nm1 <- Backend.mkUniqueIdentifier Basic "bb"
            nm2 <- Backend.mkUniqueIdentifier Basic "bb"
            let bbD = BlackBoxD nm1 libs imps inc (N.BBTemplate templ3) b'
            block <- getMon (blockDecl nm2 (templDecls ++ [bbD]))
            return (render block)

  case verifyBlackBoxContext b' templ4 of
    Nothing -> do
      bb <- renderBlackBox libs imps inc templ4 b'
      return (renderLazy . layoutPretty layoutOptions . bb)
    Just err0 -> do
      sp <- getSrcSpan
      let err1 = concat [ "Couldn't instantiate blackbox for "
                        , unpackFS (bbName b), ". Verification procedure "
                        , "reported:\n\n" ++ err0 ]
      throw (ClashException sp ($(curLoc) ++ err1) Nothing)

renderElem b (SigD e m) = do
  e' <- Text.concat <$> mapM (fmap ($ 0) . renderElem b) e
  let ty = case m of
             Nothing -> snd $ bbResult b
             Just n  -> let (_,ty',_) = bbInputs b !! n
                        in  ty'
  t  <- getMon (hdlSig e' ty)
  return (const (renderOneLine t))

renderElem b (Period n) = do
  let (_, ty, _) = bbInputs b !! n
  case stripVoid ty of
    KnownDomain _ period _ _ _ _ ->
      return $ const $ Text.pack $ show period
    _ ->
      error $ $(curLoc) ++ "Period: Expected `KnownDomain` or `KnownConfiguration`, not: " ++ show ty

renderElem b (Tag n) = do
  let (_, ty, _) = bbInputs b !! n
  case stripVoid ty of
    KnownDomain dom _ _ _ _ _ ->
      return (const (Text.pack (Data.Text.unpack dom)))
    Reset dom ->
      return (const (Text.pack (Data.Text.unpack dom)))
    Clock dom ->
      return (const (Text.pack (Data.Text.unpack dom)))
    _ ->
      error $ $(curLoc) ++ "Tag: Expected `KnownDomain` or `KnownConfiguration`, not: " ++ show ty


renderElem b (IF c t f) = do
  iw <- iwWidth
  syn <- hdlSyn
  let c' = check iw syn c
  if c' > 0 then renderTemplate b t else renderTemplate b f
  where
    check iw syn c' = case c' of
      (Size e)   -> typeSize (lineToType b [e])
      (Length e) -> case lineToType b [e] of
                       (Vector n _)             -> n
                       Void (Just (Vector n _)) -> n
                       _                        -> 0 -- HACK: So we can test in splitAt if one of the
                              -- vectors in the tuple had a zero length
      (Lit n) -> case bbInputs b !! n of
        (l,_,_)
          | Literal _ l' <- l ->
            case l' of
              NumLit i -> fromInteger i
              BitLit bl -> case bl of
                N.H -> 1
                N.L -> 0
                _   -> error $ $(curLoc) ++ "IF: LIT bit literal must be high or low"
              BoolLit bl -> bool 0 1 bl
              _ -> error $ $(curLoc) ++ "IF: LIT must be a numeric lit"
          | DataCon (Signed _) _ [Literal _ (NumLit i)] <- l
            -> fromInteger i
          | DataCon (Unsigned _) _ [Literal _ (NumLit i)] <- l
            -> fromInteger i
        k -> error $ $(curLoc) ++ ("IF: LIT must be a numeric lit:" ++ show k)
      (Depth e)  -> case lineToType b [e] of
                      (RTree n _) -> n
                      _ -> error $ $(curLoc) ++ "IF: treedepth of non-tree type"
      IW64       -> if iw == 64 then 1 else 0
      (HdlSyn s) -> if s == syn then 1 else 0
      (IsVar n)  -> let (e,_,_) = bbInputs b !! n
                    in case e of
                      Identifier _ Nothing -> 1
                      _                    -> 0
      (IsLit n)  -> let (e,_,_) = bbInputs b !! n
                    in case e of
                      DataCon {}   -> 1
                      Literal {}   -> 1
                      BlackBoxE {} -> 1
                      _            -> 0

      (IsActiveEnable n) ->
        let (e, ty, _) = bbInputs b !! n in
        case (e, ty) of
          (Literal Nothing (BoolLit True), Bool)  -> 0
          -- TODO: Emit warning? If enable signal is inferred as always False,
          -- TODO: the component will never be enabled. This is probably not the
          -- TODO: user's intention.
          (Literal Nothing (BoolLit False), Bool) -> 1
          (_, Bool)                               -> 1
          _ ->
            error $ $(curLoc) ++ "IsActiveEnable: Expected Bool, not: " ++ show ty

      (ActiveEdge edgeRequested n) ->
        let (_, ty, _) = bbInputs b !! n in
        case stripVoid ty of
          KnownDomain _ _ edgeActual _ _ _ ->
            if edgeRequested == edgeActual then 1 else 0
          _ ->
            error $ $(curLoc) ++ "ActiveEdge: Expected `KnownDomain` or `KnownConfiguration`, not: " ++ show ty

      (IsSync n) ->
        let (_, ty, _) = bbInputs b !! n in
        case stripVoid ty of
          KnownDomain _ _ _ Synchronous _ _ -> 1
          KnownDomain _ _ _ Asynchronous _ _ -> 0
          _ -> error $ $(curLoc) ++ "IsSync: Expected `KnownDomain` or `KnownConfiguration`, not: " ++ show ty

      (IsInitDefined n) ->
        let (_, ty, _) = bbInputs b !! n in
        case stripVoid ty of
          KnownDomain _ _ _ _ Defined _ -> 1
          KnownDomain _ _ _ _ Unknown _ -> 0
          _ -> error $ $(curLoc) ++ "IsInitDefined: Expected `KnownDomain` or `KnownConfiguration`, not: " ++ show ty

      (IsActiveHigh n) ->
        let (_, ty, _) = bbInputs b !! n in
        case stripVoid ty of
          KnownDomain _ _ _ _ _ ActiveHigh -> 1
          KnownDomain _ _ _ _ _ ActiveLow -> 0
          _ -> error $ $(curLoc) ++ "IsActiveHigh: Expected `KnownDomain` or `KnownConfiguration`, not: " ++ show ty

      (StrCmp [Text t1] n) ->
        let (e,_,_) = bbInputs b !! n
        in  case exprToString e of
              Just t2
                | t1 == Text.pack t2 -> 1
                | otherwise -> 0
              Nothing -> error $ $(curLoc) ++ "Expected a string literal: " ++ show e
      (And es)   -> if all (/=0) (map (check iw syn) es)
                       then 1
                       else 0
      CmpLE e1 e2 -> if check iw syn e1 <= check iw syn e2
                        then 1
                        else 0
      _ -> error $ $(curLoc) ++ "IF: condition must be: SIZE, LENGTH, IW64, LIT, ISLIT, or ISARG"

renderElem b e = fmap const (renderTag b e)

parseFail :: Text -> BlackBoxTemplate
parseFail t = case runParse t of
  Failure errInfo ->
    error (show (_errDoc errInfo))
  Success templ -> templ

idToExpr
  :: (Text,HWType)
  -> (Expr,HWType,Bool)
idToExpr (t,ty) = (Identifier (Text.toStrict t) Nothing,ty,False)

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
lineToType b [(Typ Nothing)]  = snd $ bbResult b
lineToType b [(Typ (Just n))] = let (_,ty,_) = bbInputs b !! n
                                in  ty
lineToType b [(TypElem t)]    = case lineToType b [t] of
                                  Vector _ elTy -> elTy
                                  _ -> error $ $(curLoc) ++ "Element type selection of a non-vector type"
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
renderTag b (Result esc)    = do
  escape <- if esc then unextend else pure id
  fmap (Text.fromStrict . escape . Text.toStrict . renderOneLine) . getMon . expr False . fst $ bbResult b
renderTag b (Arg esc n)  = do
  let (e,_,_) = bbInputs b !! n
  escape <- if esc then unextend else pure id
  (Text.fromStrict . escape . Text.toStrict . renderOneLine) <$> getMon (expr False e)

renderTag b (Const n)  = do
  let (e,_,_) = bbInputs b !! n
  renderOneLine <$> getMon (expr False e)

renderTag b t@(ArgGen k n)
  | k == bbLevel b
  , let (e,_,_) = bbInputs b !! n
  = renderOneLine <$> getMon (expr False e)
  | otherwise
  = getMon (prettyElem t)

renderTag b (Lit n) =
  renderOneLine <$> getMon (expr False (mkLit e))
 where
  (e,_,_) = bbInputs b !! n

  mkLit (Literal (Just (Signed _,_)) i)                                 = Literal Nothing i
  mkLit (Literal (Just (Unsigned _,_)) i)                               = Literal Nothing i
  mkLit (DataCon _ (DC (Void {}, _)) [Literal (Just (Signed _,_)) i])   = Literal Nothing i
  mkLit (DataCon _ (DC (Void {}, _)) [Literal (Just (Unsigned _,_)) i]) = Literal Nothing i
  mkLit i                                                               = i

renderTag b e@(Name _i) =
  case elementToText b e of
      Right s  -> return s
      Left msg -> error $ $(curLoc) ++ unwords [ "Error when reducing to string"
                                               , "in ~NAME construct:", msg ]

renderTag _ (Var [Text t] _) = return t
renderTag _ (Sym t _) = return t

renderTag b (BV True es e) = do
  e' <- Text.concat <$> mapM (fmap ($ 0) . renderElem b) es
  let ty = lineToType b [e]
  renderOneLine <$> getMon (toBV ty e')
renderTag b (BV False es e) = do
  e' <- Text.concat <$> (mapM (fmap ($ 0) . renderElem b) es)
  let ty = lineToType b [e]
  renderOneLine <$> getMon (fromBV ty e')

renderTag b (Sel e n) =
  let ty = lineToType b [e]
  in  renderOneLine <$> getMon (hdlRecSel ty n)

renderTag b (Typ Nothing)   = fmap renderOneLine . getMon . hdlType Internal . snd $ bbResult b
renderTag b (Typ (Just n))  = let (_,ty,_) = bbInputs b !! n
                              in  renderOneLine <$> getMon (hdlType Internal ty)
renderTag b (TypM Nothing)  = fmap renderOneLine . getMon . hdlTypeMark . snd $ bbResult b
renderTag b (TypM (Just n)) = let (_,ty,_) = bbInputs b !! n
                              in  renderOneLine <$> getMon (hdlTypeMark ty)
renderTag b (Err Nothing)   = fmap renderOneLine . getMon . hdlTypeErrValue . snd $ bbResult b
renderTag b (Err (Just n))  = let (_,ty,_) = bbInputs b !! n
                              in  renderOneLine <$> getMon (hdlTypeErrValue ty)
renderTag b (Size e)        = return . Text.pack . show . typeSize $ lineToType b [e]

renderTag b (Length e) = return . Text.pack . show . vecLen $ lineToType b [e]
  where
    vecLen (Vector n _)               = n
    vecLen (Void (Just (Vector n _))) = n
    vecLen thing =
      error $ $(curLoc) ++ "vecLen of a non-vector type: " ++ show thing

renderTag b (Depth e) = return . Text.pack . show . treeDepth $ lineToType b [e]
  where
    treeDepth (RTree n _)               = n
    treeDepth (Void (Just (RTree n _))) = n
    treeDepth thing =
      error $ $(curLoc) ++ "treeDepth of a non-tree type: " ++ show thing

renderTag b (MaxIndex e) = return . Text.pack . show . vecLen $ lineToType b [e]
  where
    vecLen (Vector n _) = n-1
    vecLen thing =
      error $ $(curLoc) ++ "vecLen of a non-vector type: " ++ show thing

renderTag b e@(TypElem _)   = let ty = lineToType b [e]
                              in  renderOneLine <$> getMon (hdlType Internal ty)
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
      in  fmap renderOneLine (getMon (hdlType Internal hty))
    x -> error $ $(curLoc) ++ "Index type not given a literal: " ++ show x
renderTag b (FilePath e)    = case e of
  Lit n -> do
    let (e',_,_) = bbInputs b !! n
    case exprToString e' of
      Just s -> do
        s' <- addAndSetData s
        return (Text.pack (show s'))
      _ -> do
        e2  <- getMon (prettyElem e)
        error $ $(curLoc) ++ "argument of ~FILEPATH:" ++ show e2 ++  "does not reduce to a string"
  _ -> do e' <- getMon (prettyElem e)
          error $ $(curLoc) ++ "~FILEPATH expects a ~LIT[N] argument, but got: " ++ show e'
renderTag b (IncludeName n) = case indexMaybe (bbQsysIncName b) n of
  Just nm -> return (Text.fromStrict nm)
  _ -> error $ $(curLoc) ++ "~INCLUDENAME[" ++ show n ++ "] does not correspond to any index of the 'includes' field that is specified in the primitive definition"
renderTag b (OutputWireReg n) = case IntMap.lookup n (bbFunctions b) of
  Just ((_,rw,_,_,_,_):_) -> case rw of {N.Wire -> return "wire"; N.Reg -> return "reg"}
  _ -> error $ $(curLoc) ++ "~OUTPUTWIREREG[" ++ show n ++ "] used where argument " ++ show n ++ " is not a function"
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

renderTag b CompName = pure (Text.fromStrict (bbCompName b))

renderTag b CtxName = case bbCtxName b of
  Just nm -> return (Text.fromStrict nm)
  _ | Identifier t _ <- fst (bbResult b)
    -> return (Text.fromStrict t)
  _ -> error "internal error"


renderTag _ e = do e' <- getMon (prettyElem e)
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
  let (e',_,_) = head (bbInputs ctx)
  in  exprToString e'
exprToString (BlackBoxE "GHC.CString.unpackCString#" _ _ _ _ ctx _) =
  let (e',_,_) = head (bbInputs ctx)
  in  exprToString e'
exprToString _ = Nothing

prettyBlackBox :: Monad m
               => BlackBoxTemplate
               -> Mon m Text
prettyBlackBox bbT = Text.concat <$> mapM prettyElem bbT

prettyElem
  :: (HasCallStack, Monad m)
  => Element
  -> Mon m Text
prettyElem (Text t) = return t
prettyElem (Component (Decl i 0 args)) = do
  args' <- mapM (\(a,b) -> (,) <$> prettyBlackBox a <*> prettyBlackBox b) args
  renderOneLine <$>
    (nest 2 (string "~INST" <+> int i <> line <>
        string "~OUTPUT" <+> string "=>" <+> string (fst (head args')) <+> string (snd (head args')) <+> string "~" <> line <>
        vcat (mapM (\(a,b) -> string "~INPUT" <+> string "=>" <+> string a <+> string b <+> string "~") (tail args')))
      <> line <> string "~INST")
prettyElem (Component (Decl {})) =
  error $ $(curLoc) ++ "prettyElem can't (yet) render ~INST when subfuncion /= 0!"
prettyElem (Result b) = if b then return "~ERESULT" else return "~RESULT"
prettyElem (Arg b i) = renderOneLine <$> (if b then string "~EARG" else string "~ARG" <> brackets (int i))
prettyElem (Lit i) = renderOneLine <$> (string "~LIT" <> brackets (int i))
prettyElem (Const i) = renderOneLine <$> (string "~CONST" <> brackets (int i))
prettyElem (Name i) = renderOneLine <$> (string "~NAME" <> brackets (int i))
prettyElem (Var es i) = do
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
prettyElem (IsActiveHigh i) = renderOneLine <$> (string "~ISACTIVEHIGH" <> brackets (int i))
prettyElem (IsActiveEnable i) = renderOneLine <$> (string "~ISACTIVEENABLE" <> brackets (int i))

-- Domain attributes:
prettyElem (Tag i) = renderOneLine <$> (string "~TAG" <> brackets (int i))
prettyElem (Period i) = renderOneLine <$> (string "~PERIOD" <> brackets (int i))
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
prettyElem (OutputWireReg i) = renderOneLine <$> (string "~RESULTWIREREG" <> brackets (int i))
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
        Result _ -> []
        Arg _ _ -> []
        ArgGen _ _ -> []
        Const _ -> []
        Lit _ -> []
        Name _ -> []
        Var es _ -> concatMap go es
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
        Tag _ -> []
        Period _ -> []
        ActiveEdge _ _ -> []
        IsSync _ -> []
        IsInitDefined _ -> []
        IsActiveHigh _ -> []
        IsActiveEnable _ -> []
        StrCmp es _ -> concatMap go es
        OutputWireReg _ -> []
        Vars _ -> []
        Repeat es1 es2 ->
          concatMap go es1 ++ concatMap go es2
        CtxName -> []

-- | Determine variables used in an expression. Used for VHDL sensitivity list.
-- Also see: https://github.com/clash-lang/clash-compiler/issues/365
usedVariables :: Expr -> [Identifier]
usedVariables Noop              = []
usedVariables (Identifier i _)  = [i]
usedVariables (DataCon _ _ es)  = concatMap usedVariables es
usedVariables (DataTag _ e')    = [either id id e']
usedVariables (Literal {})      = []
usedVariables (ConvBV _ _ _ e') = usedVariables e'
usedVariables (IfThenElse e1 e2 e3) = concatMap usedVariables [e1,e2,e3]
usedVariables (BlackBoxE _ _ _ _ t bb _) = nub (sList ++ sList')
  where
    matchArg (Arg _ i) = Just i
    matchArg _         = Nothing

    matchVar (Var [Text v] _) = Just (Text.toStrict v)
    matchVar _                = Nothing

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
        Arg _ i -> Just i
        Component (Decl i _ _) -> Just i
        Const i -> Just i
        IsLit i -> Just i
        IsActiveEnable i -> Just i
        Lit i -> Just i
        Name i -> Just i
        Var _ i -> Just i

        -- Domain properties (only need type):
        IsInitDefined _ -> Nothing
        ActiveEdge _ _ -> Nothing
        IsSync _ -> Nothing
        Period _ -> Nothing
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
        IW64 -> Nothing
        Length _ -> Nothing
        MaxIndex _ -> Nothing
        OutputWireReg _ -> Nothing
        Repeat _ _ -> Nothing
        Result _ -> Nothing
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
