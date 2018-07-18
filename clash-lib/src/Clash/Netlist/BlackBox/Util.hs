{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utilties to verify blackbox contexts against templates and rendering filled
  in templates
-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

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
import           Data.Maybe                      (mapMaybe)
import           Data.Semigroup.Monad
import qualified Data.Text
import           Data.Text.Lazy                  (Text)
import qualified Data.Text.Lazy                  as Text
import qualified Data.Text.Prettyprint.Doc       as PP
import           Data.Text.Prettyprint.Doc.Extra
import           System.FilePath                 (replaceBaseName, takeBaseName,
                                                  takeFileName, (<.>))
import qualified Text.PrettyPrint.ANSI.Leijen    as ANSI
import           Text.Printf
import           Text.Read                       (readEither)
import           Text.Trifecta.Result            hiding (Err)

import           Clash.Backend                   (Backend (..), Usage (..))
import           Clash.Driver.Types              (ClashException (..))
import           Clash.Netlist.BlackBox.Parser
import           Clash.Netlist.BlackBox.Types
import           Clash.Netlist.Id                (IdType (..))
import           Clash.Netlist.Types             (BlackBoxContext (..),
                                                  Expr (..), HWType (..),
                                                  Identifier, Literal (..),
                                                  Modifier (..), NetlistMonad)
import qualified Clash.Netlist.Types             as N
import           Clash.Netlist.Util              (mkUniqueIdentifier, typeSize)
import           Clash.Signal.Internal           (ClockKind (Gated),
                                                  ResetKind (Synchronous))
import           Clash.Util

-- | Determine if the number of normal/literal/function inputs of a blackbox
-- context at least matches the number of argument that is expected by the
-- template.
verifyBlackBoxContext
  :: BlackBoxContext
  -- ^ Blackbox to verify
  -> N.BlackBox
  -- ^ Template to check against
  -> Bool
verifyBlackBoxContext bbCtx (N.BBFunction (N.TemplateFunction _ f _)) = f bbCtx
verifyBlackBoxContext bbCtx (N.BBTemplate t) = all verify' t
  where
    verify' (I _ n)         = n < length (bbInputs bbCtx)
    verify' (L n)           = case indexMaybe (bbInputs bbCtx) n of
                                Just (_,_,b) -> b
                                _            -> False
    verify' (Typ (Just n))  = n < length (bbInputs bbCtx)
    verify' (TypM (Just n)) = n < length (bbInputs bbCtx)
    verify' (Err (Just n))  = n < length (bbInputs bbCtx)
    verify' (D (Decl n l')) = case IntMap.lookup n (bbFunctions bbCtx) of
      Just _ -> all (\(x,y) -> verifyBlackBoxContext bbCtx (N.BBTemplate x) &&
                               verifyBlackBoxContext bbCtx (N.BBTemplate y)) l'
      _      -> False
    verify' _               = True

extractLiterals :: BlackBoxContext
                -> [Expr]
extractLiterals = map (\case (e,_,_) -> e)
                . filter (\case (_,_,b) -> b)
                . bbInputs

-- | Update all the symbol references in a template, and increment the symbol
-- counter for every newly encountered symbol.
setSym
  :: BlackBoxContext
  -> BlackBoxTemplate
  -> NetlistMonad (BlackBoxTemplate,[N.Declaration])
setSym bbCtx l = do
    (a,(_,decls)) <- runStateT (mapM setSym' l) (IntMap.empty,IntMap.empty)
    return (a,concatMap snd (IntMap.elems decls))
  where
    setSym' :: Element
            -> StateT ( IntMap.IntMap Identifier
                      , IntMap.IntMap (Identifier,[N.Declaration]))
                      NetlistMonad
                      Element
    setSym' e = case e of
      Var nm i | i < length (bbInputs bbCtx) -> case bbInputs bbCtx !! i of
        (Identifier nm' Nothing,_,_) -> return (Var [C nm'] i)
        (e',hwTy,_) -> do
          varM <- IntMap.lookup i <$> use _2
          case varM of
            Nothing -> do
              nm' <- lift (mkUniqueIdentifier Extended (concatT (C "#":nm)))
              let decls = case typeSize hwTy of
                    0 -> []
                    _ -> [N.NetDecl Nothing nm' hwTy
                         ,N.Assignment nm' e'
                         ]
              _2 %= (IntMap.insert i (nm',decls))
              return (Var [C nm'] i)
            Just (nm',_) -> return (Var [C nm'] i)
      Sym _ i -> do
        symM <- IntMap.lookup i <$> use _1
        case symM of
          Nothing -> do
            t <- lift (mkUniqueIdentifier Extended (Text.pack "#n"))
            _1 %= (IntMap.insert i t)
            return (Sym t i)
          Just t -> return (Sym t i)
      GenSym t i -> do
        symM <- IntMap.lookup i <$> use _1
        case symM of
          Nothing -> do
            t' <- lift (mkUniqueIdentifier Basic (concatT t))
            _1 %= (IntMap.insert i t')
            return (GenSym [C t'] i)
          Just _ -> error ("Symbol #" ++ show (t,i) ++ " is already defined")
      D (Decl n l') -> D <$> (Decl n <$> mapM (combineM (mapM setSym') (mapM setSym')) l')
      IF c t f      -> IF <$> pure c <*> mapM setSym' t <*> mapM setSym' f
      SigD e' m     -> SigD <$> (mapM setSym' e') <*> pure m
      BV t e' m     -> BV <$> pure t <*> mapM setSym' e' <*> pure m
      _             -> pure e

    concatT :: [Element] -> Text
    concatT = Text.concat
            . map (\case { C t -> t
                         ; N i -> case elementToText bbCtx (N i) of
                                         Right t ->
                                             t
                                         Left msg ->
                                             error $ $(curLoc) ++  "Could not convert "
                                                               ++ "~NAME[" ++ show i ++ "]"
                                                               ++ " to string:" ++ msg
                         ; O _ | Identifier t _ <- fst (bbResult bbCtx)
                               -> t
                         ; CompName -> bbCompName bbCtx
                         ; _   -> error "unexpected element in GENSYM"})

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
      let bbCtx' = bbCtx {bbQsysIncName = nms'}
      incForHash <- onBlackBox (renderTemplate bbCtx')
                               (\(N.TemplateFunction _ _ f) -> do
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

  let bbNamedCtx = bbCtx {bbQsysIncName = nms}
      incs = snd <$> includes
  bb' <- case bb of
        N.BBTemplate bt   -> do
          t <- renderTemplate bbNamedCtx bt
          return (\col -> PP.nest (col-2) (PP.pretty (t (col + 2))))
        N.BBFunction (N.TemplateFunction _ _ bf)  -> do
          t <- bf bbNamedCtx
          return (\_ -> t)

  incs' <- mapM (onBlackBox (fmap (PP.pretty . ($ 0)) . renderTemplate bbNamedCtx)
                            (\(N.TemplateFunction _ _ f) -> f bbNamedCtx))
                incs
  libs' <- mapM (fmap ($ 0) . renderTemplate bbNamedCtx) libs
  imps' <- mapM (fmap ($ 0) . renderTemplate bbNamedCtx) imps
  addIncludes $ zipWith3 (\nm' ((_, ext), _) inc -> (Text.unpack nm' <.> Data.Text.unpack ext, inc)) nms includes incs'
  addLibraries libs'
  addImports imps'
  return bb'

-- | Assign @Var@ holes in the context of a primitive HDL template that is
-- passed as an argument of a higher-order HDL template. For the general case,
-- use 'setSym'
--
-- This functions errors when the @Var@ hole cannot be filled with a variable,
-- as it is (currently) impossible to create unique names this late in the
-- pipeline.
setSimpleVar
  :: BlackBoxContext
  -> BlackBoxTemplate
  -> BlackBoxTemplate
setSimpleVar bbCtx = map go
  where
    go e = case e of
      Var _ i
        | i < length (bbInputs bbCtx)
        , (Identifier nm' Nothing,_,_) <- bbInputs bbCtx !! i
        -> Var [C nm'] i
        | otherwise
        -> error $ $(curLoc) ++ "You can only pass variables to function arguments in a higher-order primitive"
      D (Decl n l') -> D (Decl n (map (map go *** map go) l'))
      IF c t f      -> IF c (map go t) (map go f)
      SigD e' m     -> SigD (map go e') m
      BV t e' m     -> BV t (map go e') m
      _             -> e

-- | Render a single template element
renderElem :: Backend backend
           => BlackBoxContext
           -> Element
           -> State backend (Int -> Text)
renderElem b (D (Decl n (l:ls))) = do
  (o,oTy,_) <- idToExpr <$> combineM (lineToIdentifier b) (return . lineToType b) l
  is <- mapM (fmap idToExpr . combineM (lineToIdentifier b) (return . lineToType b)) ls
  let Just (templ,_,libs,imps,inc,pCtx)  = IntMap.lookup n (bbFunctions b)
      b' = pCtx { bbResult = (o,oTy), bbInputs = bbInputs pCtx ++ is }
  templ' <- case templ of
              Left t        -> return t
              Right (nm,ds) -> do block <- getMon $ blockDecl nm ds
                                  return . N.BBTemplate . parseFail . renderLazy $ (layoutPretty (LayoutOptions (AvailablePerLine 120 0.4)) block)
  let t2 = onBlackBox (N.BBTemplate . setSimpleVar b') N.BBFunction templ'
  if verifyBlackBoxContext b' t2
    then do
      bb <- renderBlackBox libs imps inc t2 b'
      return (renderLazy . layoutPretty (LayoutOptions (AvailablePerLine 120 0.4)) . bb)
    else do
      sp <- getSrcSpan
      throw (ClashException sp ($(curLoc) ++ "\nCan't match context:\n" ++ show b' ++ "\nwith template:\n" ++ show templ) Nothing)

renderElem b (SigD e m) = do
  e' <- Text.concat <$> mapM (fmap ($ 0) . renderElem b) e
  let ty = case m of
             Nothing -> snd $ bbResult b
             Just n  -> let (_,ty',_) = bbInputs b !! n
                        in  ty'
  t  <- getMon (hdlSig e' ty)
  return (const (renderOneLine t))

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
      (L n) -> case bbInputs b !! n of
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
      (IsGated n) -> let (_,ty,_) = bbInputs b !! n
                     in case ty of
                       Clock _ _ Gated -> 1
                       _               -> 0
      (IsSync n) -> let (_,ty,_) = bbInputs b !! n
                    in case ty of
                       Reset _ _ Synchronous -> 1
                       _                     -> 0
      (StrCmp [C t1] n) ->
        let (e,_,_) = bbInputs b !! n
        in  case exprToString e of
              Just t2
                | t1 == Text.pack t2 -> 1
                | otherwise -> 0
              Nothing -> error $ $(curLoc) ++ "Expected a string literal: " ++ show e
      (And es)   -> if all (==1) (map (check iw syn) es)
                       then 1
                       else 0
      _ -> error $ $(curLoc) ++ "IF: condition must be: SIZE, LENGTH, IW64, LIT, ISLIT, or ISARG"

renderElem b e = fmap const (renderTag b e)

parseFail :: Text -> BlackBoxTemplate
parseFail t = case runParse t of
  Failure errInfo ->
    error (ANSI.displayS (ANSI.renderCompact (_errDoc errInfo)) "")
  Success templ -> templ

idToExpr
  :: (Text,HWType)
  -> (Expr,HWType,Bool)
idToExpr (t,ty) = (Identifier t Nothing,ty,False)

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
lineToType b [(IndexType (L n))] =
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
renderTag _ (C t)           = return t
renderTag b (O esc)         = do
  escape <- if esc then unextend else pure id
  fmap (escape . renderOneLine) . getMon . expr False . fst $ bbResult b
renderTag b (I esc n)       = do
  let (e,_,_) = bbInputs b !! n
  escape <- if esc then unextend else pure id
  (escape . renderOneLine) <$> getMon (expr False e)

renderTag b t@(Arg k n)
  | k == bbLevel b
  , let (e,_,_) = bbInputs b !! n
  = renderOneLine <$> getMon (expr False e)
  | otherwise
  = getMon (prettyElem t)

renderTag b (L n)           = let (e,_,_) = bbInputs b !! n
                              in  renderOneLine <$> getMon (expr False (mkLit e))
  where
    mkLit (Literal (Just (Signed _,_)) i)   = Literal Nothing i
    mkLit (Literal (Just (Unsigned _,_)) i) = Literal Nothing i
    mkLit (DataCon _ (DC (Void {}, _)) [Literal (Just (Signed _,_)) i]) = Literal Nothing i
    mkLit (DataCon _ (DC (Void {}, _)) [Literal (Just (Unsigned _,_)) i]) = Literal Nothing i
    mkLit i                               = i

renderTag b e@(N _i) =
  case elementToText b e of
      Right s  -> return s
      Left msg -> error $ $(curLoc) ++ unwords [ "Error when reducing to string"
                                               , "in ~NAME construct:", msg ]

renderTag _ (Var [C t] _) = return t
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
renderTag b (Length e)      = return . Text.pack . show . vecLen $ lineToType b [e]
  where
    vecLen (Vector n _) = n
    vecLen _            = error $ $(curLoc) ++ "vecLen of a non-vector type"
renderTag b (Depth e)      = return . Text.pack . show . treeDepth $ lineToType b [e]
  where
    treeDepth (RTree n _) = n
    treeDepth _           = error $ $(curLoc) ++ "treeDepth of a non-tree type"
renderTag b e@(TypElem _)   = let ty = lineToType b [e]
                              in  renderOneLine <$> getMon (hdlType Internal ty)
renderTag _ (Gen b)         = renderOneLine <$> genStmt b
renderTag _ (GenSym [C t] _) = return t
renderTag b (Vars n)        =
  let (e,_,_) = bbInputs b !! n

      go (Identifier i _) = [i]
      go (DataCon _ _ es) = concatMap go es
      go (DataTag _ e')   = [either id id e']
      go (Literal {})     = []
      go (ConvBV _ _ _ e') = go e'
      go (BlackBoxE _ _ _ _ t b' _) =
        let usedArgs = mapMaybe (indexMaybe (bbInputs b')) (usedArguments t)
        in  concatMap (\(e',_,_) -> go e') usedArgs

      vars    = go e
  in  case vars of
        [] -> return Text.empty
        _  -> return (Text.concat $ map (Text.cons ',') vars)
renderTag b (IndexType (L n)) =
  case bbInputs b !! n of
    (Literal _ (NumLit n'),_,_) ->
      let hty = Index (fromInteger n')
      in  fmap renderOneLine (getMon (hdlType Internal hty))
    x -> error $ $(curLoc) ++ "Index type not given a literal: " ++ show x
renderTag b (FilePath e)    = case e of
  L n -> do
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
  Just nm -> return nm
  _ -> error $ $(curLoc) ++ "~INCLUDENAME[" ++ show n ++ "] does not correspond to any index of the 'includes' field that is specified in the primitive definition"
renderTag b (OutputWireReg n) = case IntMap.lookup n (bbFunctions b) of
  Just (_,rw,_,_,_,_) -> case rw of {N.Wire -> return "wire"; N.Reg -> return "reg"}
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

renderTag b CompName = pure (bbCompName b)

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
elementToText bbCtx  (N n) = elementToText bbCtx (L n)
elementToText _bbCtx (C t) = return $ t
elementToText bbCtx  (L n) =
    case bbInputs bbCtx ^? element n of
        Just (e,_,_) ->
            case exprToString e of
                Just t ->
                    Right $ Text.pack t
                Nothing ->
                    Left $ $(curLoc) ++ unwords [ "Could not extract string from"
                                                , show e, "referred to by"
                                                , show (L n) ]
        Nothing ->
            Left $ $(curLoc) ++ unwords [ "Invalid literal", show (L n)
                                        , "used in blackbox with context:"
                                        , show bbCtx, "." ]

elementToText _bbCtx e = error $ "Unexpected string like: " ++ show e

-- | Extracts string from SSymbol or string literals
exprToString
  :: Expr
  -> Maybe String
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

prettyElem :: Monad m
           => Element
           -> Mon m Text
prettyElem (C t) = return t
prettyElem (D (Decl i args)) = do
  args' <- mapM (\(a,b) -> (,) <$> prettyBlackBox a <*> prettyBlackBox b) args
  renderOneLine <$>
    (nest 2 (string "~INST" <+> int i <> line <>
        string "~OUTPUT" <+> string "=>" <+> string (fst (head args')) <+> string (snd (head args')) <+> string "~" <> line <>
        vcat (mapM (\(a,b) -> string "~INPUT" <+> string "=>" <+> string a <+> string b <+> string "~") (tail args')))
      <> line <> string "~INST")
prettyElem (O b) = if b then return "~ERESULT" else return "~RESULT"
prettyElem (I b i) = renderOneLine <$> (if b then string "~EARG" else string "~ARG" <> brackets (int i))
prettyElem (L i) = renderOneLine <$> (string "~LIT" <> brackets (int i))
prettyElem (N i) = renderOneLine <$> (string "~NAME" <> brackets (int i))
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
prettyElem (And es) =
  (renderLazy . layoutCompact) <$>
  (brackets (tupled $ mapM (string <=< prettyElem) es))
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
prettyElem (IsGated i) = renderOneLine <$> (string "~ISGATED" <> brackets (int i))
prettyElem (IsSync i) = renderOneLine <$> (string "~ISSYNC" <> brackets (int i))
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
prettyElem (Arg n x) =
  renderOneLine <$> (string "~ARGN" <> brackets (int n) <> brackets (int x))
prettyElem (Template bbname source) = do
  bbname' <- mapM prettyElem bbname
  source' <- mapM prettyElem source
  renderOneLine <$> (string "~TEMPLATE"
                                  <> brackets (string $ Text.concat bbname')
                                  <> brackets (string $ Text.concat source'))

usedArguments :: N.BlackBox
              -> [Int]
usedArguments (N.BBFunction (N.TemplateFunction k _ _)) = k
usedArguments (N.BBTemplate t) = nub (concatMap go t)
  where
    go x = case x of
      D (Decl i args) -> i : concatMap (\(a,b) -> concatMap go a ++ concatMap go b) args
      I _ i -> [i]
      L i -> [i]
      N i -> [i]
      Var _ i -> [i]
      IndexType e -> go e
      FilePath e -> go e
      Template bbname source -> concatMap go bbname ++ concatMap go source
      IF b esT esF -> go b ++ concatMap go esT ++ concatMap go esF
      SigD es _ -> concatMap go es
      BV _ es _ -> concatMap go es
      StrCmp _ i -> [i]
      GenSym es _ -> concatMap go es
      DevNull es -> concatMap go es
      _ -> []

onBlackBox
  :: (BlackBoxTemplate -> r)
  -> (N.TemplateFunction -> r)
  -> N.BlackBox
  -> r
onBlackBox f _ (N.BBTemplate t) = f t
onBlackBox _ g (N.BBFunction t) = g t
