{-|
  Copyright  :  (C) 2012-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utilties to verify blackbox contexts against templates and rendering filled
  in templates
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module CLaSH.Netlist.BlackBox.Util where

--import           Control.Lens                         (at, use, (%=), (+=), _1,
--                                                       _2)
import           Control.Exception                    (throw)
import           Control.Monad.State                  (State, StateT, evalStateT,
                                                       lift, modify, get)
import           Control.Monad.Writer.Strict          (MonadWriter, tell)
import           Data.Bool                            (bool)
import           Data.Foldable                        (foldrM)
import qualified Data.IntMap                          as IntMap
import           Data.List                            (mapAccumL, nub)
import           Data.Set                             (Set,singleton)
import           Data.Text.Lazy                       (Text)
import qualified Data.Text.Lazy                       as Text
import           System.FilePath                      (replaceBaseName,
                                                       takeBaseName,
                                                       takeFileName)
import           Text.PrettyPrint.Leijen.Text.Monadic (displayT, renderCompact,
                                                       renderOneLine, brackets,
                                                       int, (<>), text, (<+>),
                                                       vcat, (<$$>), nest)
import qualified Text.PrettyPrint.Leijen.Text.Monadic as PP

import           CLaSH.Backend                        (Backend (..))
import           CLaSH.Driver.Types                   (CLaSHException (..))
import           CLaSH.Netlist.BlackBox.Parser
import           CLaSH.Netlist.BlackBox.Types
import           CLaSH.Netlist.Types                  (HWType (..), Identifier,
                                                       BlackBoxContext (..),
                                                       SyncExpr, Expr (..),
                                                       Literal (..), NetlistMonad,
                                                       Modifier (..))
import qualified CLaSH.Netlist.Types                  as N
import           CLaSH.Netlist.Util                   (mkUniqueIdentifier,typeSize)
import           CLaSH.Util

-- | Determine if the number of normal/literal/function inputs of a blackbox
-- context at least matches the number of argument that is expected by the
-- template.
verifyBlackBoxContext :: BlackBoxContext -- ^ Blackbox to verify
                      -> BlackBoxTemplate -- ^ Template to check against
                      -> Bool
verifyBlackBoxContext bbCtx = all verify'
  where
    verify' (I n)           = n < length (bbInputs bbCtx)
    verify' (L n)           = case indexMaybe (bbInputs bbCtx) n of
                                Just (_,_,b) -> b
                                _            -> False
    verify' (Clk (Just n))  = n < length (bbInputs bbCtx)
    verify' (Rst (Just n))  = n < length (bbInputs bbCtx)
    verify' (Typ (Just n))  = n < length (bbInputs bbCtx)
    verify' (TypM (Just n)) = n < length (bbInputs bbCtx)
    verify' (Err (Just n))  = n < length (bbInputs bbCtx)
    verify' (D (Decl n l')) = case IntMap.lookup n (bbFunctions bbCtx) of
                                Just _ -> all (\(x,y) -> verifyBlackBoxContext bbCtx x &&
                                                         verifyBlackBoxContext bbCtx y) l'
                                _      -> False
    verify' _               = True

extractLiterals :: BlackBoxContext
                -> [Expr]
extractLiterals = map (\case (e,_,_) -> either id fst e)
                . filter (\case (_,_,b) -> b)
                . bbInputs

-- | Update all the symbol references in a template, and increment the symbol
-- counter for every newly encountered symbol.
setSym :: BlackBoxTemplate
       -> NetlistMonad BlackBoxTemplate
--setSym l = second fst
--  $ runState (setSym' l) ((ids,i),IntMap.empty)
setSym l = evalStateT (mapM setSym' l) IntMap.empty
  where
    setSym' :: Element
            -> StateT (IntMap.IntMap Identifier)
                      NetlistMonad
                      Element
    setSym' e = case e of
      Sym _ i -> do
        symM <- IntMap.lookup i <$> get
        case symM of
          Nothing -> do
            t <- lift (mkUniqueIdentifier (Text.pack "n"))
            modify (IntMap.insert i t)
            return (Sym t i)
          Just t -> return (Sym t i)
      GenSym t i -> do
        symM <- IntMap.lookup i <$> get
        case symM of
          Nothing -> do
            t' <- lift (mkUniqueIdentifier (concatT t))
            modify (IntMap.insert i t')
            return (GenSym [C t'] i)
          Just _ -> error ("Symbol #" ++ show (t,i) ++ " is already defined")
      D (Decl n l') -> D <$> (Decl n <$> mapM (combineM (mapM setSym') (mapM setSym')) l')
      IF c t f      -> IF <$> pure c <*> mapM setSym' t <*> mapM setSym' f
      SigD e' m     -> SigD <$> (mapM setSym' e') <*> pure m
      BV t e' m     -> BV <$> pure t <*> mapM setSym' e' <*> pure m
      _             -> pure e

    concatT :: [Element] -> Text
    concatT = Text.concat
            . map (\case {C t -> t; _ -> error "unexpected element in GENSYM"})

setCompName :: Identifier -> BlackBoxTemplate -> BlackBoxTemplate
setCompName nm = map setCompName'
  where
    setCompName' CompName       = C nm
    setCompName' (D (Decl n l)) = D (Decl n (map (setCompName nm *** setCompName nm) l))
    setCompName' (IF c t f)     = IF c (setCompName nm t) (setCompName nm f)
    setCompName' (GenSym es i)  = GenSym (setCompName nm es) i
    setCompName' (BV t e m)     = BV t (setCompName nm e) (setCompName' m)
    setCompName' e              = e

setClocks :: MonadWriter (Set (Identifier,HWType)) m
          => BlackBoxContext
          -> BlackBoxTemplate
          -> m BlackBoxTemplate
setClocks bc bt = mapM setClocks' bt
  where
    setClocks' (D (Decl n l)) = D <$> (Decl n <$> mapM (combineM (setClocks bc) (setClocks bc)) l)
    setClocks' (IF c t f)     = IF <$> pure c <*> setClocks bc t <*> setClocks bc f
    setClocks' (SigD e m)     = SigD <$> (setClocks bc e) <*> pure m
    setClocks' (BV t e m)     = BV <$> pure t <*> setClocks bc e <*> pure m

    setClocks' (Clk Nothing)  = let (clk,rate) = clkSyncId $ fst $ bbResult bc
                                    clkName    = Text.append clk (Text.pack (show rate))
                                in  tell (singleton (clkName,Clock clk rate)) >> return (C clkName)
    setClocks' (Clk (Just n)) = let (e,_,_)    = bbInputs bc !! n
                                    (clk,rate) = clkSyncId e
                                    clkName    = Text.append clk (Text.pack (show rate))
                                in  tell (singleton (clkName,Clock clk rate)) >> return (C clkName)

    setClocks' (Rst Nothing)  = let (rst,rate) = clkSyncId $ fst $ bbResult bc
                                    rstName    = Text.concat [rst,Text.pack (show rate),"_rstn"]
                                in  tell (singleton (rstName,Reset rst rate)) >> return (C rstName)
    setClocks' (Rst (Just n)) = let (e,_,_)    = bbInputs bc !! n
                                    (rst,rate) = clkSyncId e
                                    rstName    = Text.concat [rst,Text.pack (show rate),"_rstn"]
                                in  tell (singleton (rstName,Reset rst rate)) >> return (C rstName)

    setClocks' e = return e

findAndSetDataFiles :: BlackBoxContext -> [(String,FilePath)] -> BlackBoxTemplate -> ([(String,FilePath)],BlackBoxTemplate)
findAndSetDataFiles bbCtx fs = mapAccumL findAndSet fs
  where
    findAndSet fs' (FilePath e) = case e of
      (L n) ->
        let (s,_,_) = bbInputs bbCtx !! n
            e'      = either id fst s
        in case e' of
          BlackBoxE "GHC.CString.unpackCString#" _ _ _ _ bbCtx' _ -> case bbInputs bbCtx' of
            [(Left (Literal Nothing (StringLit s')),_,_)] -> renderFilePath fs s'
            _ -> (fs',FilePath e)
          Literal Nothing (StringLit s') -> renderFilePath fs s'
          _ -> (fs',FilePath e)
      _ -> (fs',FilePath e)
    findAndSet fs' l = (fs',l)

renderFilePath :: [(String,FilePath)] -> String -> ([(String,FilePath)],Element)
renderFilePath fs f = ((f'',f):fs,C (Text.pack $ show f''))
  where
    f'  = takeFileName f
    f'' = selectNewName (map fst fs) f'

    selectNewName as a
      | elem a as = selectNewName as (replaceBaseName a (takeBaseName a ++ "_"))
      | otherwise = a


-- | Get the name of the clock of an identifier
clkSyncId :: SyncExpr -> (Identifier,Integer)
clkSyncId (Right (_,clk)) = clk
clkSyncId (Left i) = error $ $(curLoc) ++ "No clock for: " ++ show i

-- | Render a blackbox given a certain context. Returns a filled out template
-- and a list of 'hidden' inputs that must be added to the encompassing component.
renderBlackBox :: Backend backend
               => BlackBoxTemplate -- ^ Blackbox template
               -> BlackBoxContext -- ^ Context used to fill in the hole
               -> State backend Text
renderBlackBox l bbCtx
  = fmap Text.concat
  $ mapM (renderElem bbCtx) l

-- | Render a single template element
renderElem :: Backend backend
           => BlackBoxContext
           -> Element
           -> State backend Text
renderElem b (D (Decl n (l:ls))) = do
    (o,oTy,_) <- syncIdToSyncExpr <$> combineM (lineToIdentifier b) (return . lineToType b) l
    is <- mapM (fmap syncIdToSyncExpr . combineM (lineToIdentifier b) (return . lineToType b)) ls
    let Just (templ,pCtx)    = IntMap.lookup n (bbFunctions b)
        b' = pCtx { bbResult = (o,oTy), bbInputs = bbInputs pCtx ++ is }
    templ' <- case templ of
                Left t  -> return t
                Right d -> do Just inst' <- inst d
                              return . parseFail . displayT $ renderCompact inst'
    if verifyBlackBoxContext b' templ'
      then Text.concat <$> mapM (renderElem b') templ'
      else do
        sp <- getSrcSpan
        throw (CLaSHException sp ($(curLoc) ++ "\nCan't match context:\n" ++ show b' ++ "\nwith template:\n" ++ show templ) Nothing)

renderElem b (SigD e m) = do
  e' <- Text.concat <$> mapM (renderElem b) e
  let ty = case m of
             Nothing -> snd $ bbResult b
             Just n  -> let (_,ty',_) = bbInputs b !! n
                        in  ty'
  t  <- hdlSig e' ty
  return (displayT $ renderOneLine t)

renderElem b (IF c t f) = do
  iw <- iwWidth
  syn <- hdlSyn
  let c' = check iw syn c
  if c' > 0 then renderBlackBox t b else renderBlackBox f b
  where
    check iw syn c' = case c' of
      (Size e)   -> typeSize (lineToType b [e])
      (Length e) -> case lineToType b [e] of
                       (Vector n _) -> n
                       _ -> error $ $(curLoc) ++ "IF: veclen of a non-vector type"
      (L n) -> case bbInputs b !! n of
        (l,_,_)
          | Literal _ l' <- either id fst l ->
            case l' of
              NumLit i -> fromInteger i
              BitLit bl -> case bl of
                N.H -> 1
                N.L -> 0
                _   -> error $ $(curLoc) ++ "IF: LIT bit literal must be high or low"
              BoolLit bl -> bool 0 1 bl
              _ -> error $ $(curLoc) ++ "IF: LIT must be a numeric lit"
          | DataCon (Signed _) _ [Literal _ (NumLit i)] <- either id fst l
            -> fromInteger i
        k -> error $ $(curLoc) ++ ("IF: LIT must be a numeric lit:" ++ show k)
      (Depth e)  -> case lineToType b [e] of
                      (RTree n _) -> n
                      _ -> error $ $(curLoc) ++ "IF: treedepth of non-tree type"
      IW64       -> if iw == 64 then 1 else 0
      (HdlSyn s) -> if s == syn then 1 else 0
      (IsVar n)  -> let (s,_,_) = bbInputs b !! n
                        e       = either id fst s
                    in case e of
                      Identifier _ Nothing -> 1
                      _ -> 0
      (IsLit n)  -> let (s,_,_) = bbInputs b !! n
                        e       = either id fst s
                    in case e of
                      DataCon {} -> 1
                      Literal {} -> 1
                      BlackBoxE {} -> 1
                      _ -> 0
      (And es)   -> if all (==1) (map (check iw syn) es)
                       then 1
                       else 0
      _ -> error $ $(curLoc) ++ "IF: condition must be: SIZE, LENGTH, IW64, LIT, ISLIT, or ISARG"

renderElem b e = renderTag b e

parseFail :: Text -> BlackBoxTemplate
parseFail t = case runParse t of
                    (templ,err) | null err  -> templ
                                | otherwise -> error $ $(curLoc) ++ "\nTemplate:\n" ++ show t ++ "\nHas errors:\n" ++ show err

syncIdToSyncExpr :: (Text,HWType)
                 -> (SyncExpr,HWType,Bool)
syncIdToSyncExpr (t,ty) = (Left (Identifier t Nothing),ty,False)

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
    (Left (Literal _ (NumLit n')),_,_) -> Index (fromInteger n')
    x -> error $ $(curLoc) ++ "Index type not given a literal: " ++ show x

lineToType _ _ = error $ $(curLoc) ++ "Unexpected type manipulation"

-- | Give a context and a tagged hole (of a template), returns part of the
-- context that matches the tag of the hole.
renderTag :: Backend backend
          => BlackBoxContext
          -> Element
          -> State backend Text
renderTag _ (C t)           = return t
renderTag b O               = fmap (displayT . renderOneLine) . expr False . either id fst . fst $ bbResult b
renderTag b (I n)           = let (s,_,_) = bbInputs b !! n
                                  e       = either id fst s
                              in  (displayT . renderOneLine) <$> expr False e
renderTag b (L n)           = let (s,_,_) = bbInputs b !! n
                                  e       = either id fst s
                              in  (displayT . renderOneLine) <$> expr False (mkLit e)
  where
    mkLit (Literal (Just (Signed _,_)) i)   = Literal Nothing i
    mkLit (Literal (Just (Unsigned _,_)) i) = Literal Nothing i
    mkLit (DataCon _ (DC (Void, _)) [Literal (Just (Signed _,_)) i]) = Literal Nothing i
    mkLit (DataCon _ (DC (Void, _)) [Literal (Just (Unsigned _,_)) i]) = Literal Nothing i
    mkLit i                               = i

renderTag _ (Sym t _) = return t

renderTag b (BV True es e) = do
  e' <- Text.concat <$> mapM (renderElem b) es
  let ty = lineToType b [e]
  (displayT . renderOneLine) <$> toBV ty e'
renderTag b (BV False es e) = do
  e' <- Text.concat <$> mapM (renderElem b) es
  let ty = lineToType b [e]
  (displayT . renderOneLine) <$> fromBV ty e'

renderTag b (Typ Nothing)   = fmap (displayT . renderOneLine) . hdlType . snd $ bbResult b
renderTag b (Typ (Just n))  = let (_,ty,_) = bbInputs b !! n
                              in  (displayT . renderOneLine) <$> hdlType ty
renderTag b (TypM Nothing)  = fmap (displayT . renderOneLine) . hdlTypeMark . snd $ bbResult b
renderTag b (TypM (Just n)) = let (_,ty,_) = bbInputs b !! n
                              in  (displayT . renderOneLine) <$> hdlTypeMark ty
renderTag b (Err Nothing)   = fmap (displayT . renderOneLine) . hdlTypeErrValue . snd $ bbResult b
renderTag b (Err (Just n))  = let (_,ty,_) = bbInputs b !! n
                              in  (displayT . renderOneLine) <$> hdlTypeErrValue ty
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
                              in  (displayT . renderOneLine) <$> hdlType ty
renderTag _ (Gen b)         = displayT . renderOneLine <$> genStmt b
renderTag _ (GenSym [C t] _) = return t
renderTag b (Vars n)        =
  let (s,_,_) = bbInputs b !! n
      e       = either id fst s

      go (Identifier i _) = [i]
      go (DataCon _ _ es) = concatMap go es
      go (DataTag _ e')   = [either id id e']
      go _                = []

      vars    = go e
  in  case vars of
        [] -> return Text.empty
        _  -> return (Text.concat $ map (Text.cons ',') vars)
renderTag b (IndexType (L n)) =
  case bbInputs b !! n of
    (Left (Literal _ (NumLit n')),_,_) ->
      let hty = Index (fromInteger n')
      in  fmap (displayT . renderOneLine) (hdlType hty)
    x -> error $ $(curLoc) ++ "Index type not given a literal: " ++ show x
renderTag b (FilePath e)    = case e of
  L n -> do
    let (s,_,_) = bbInputs b !! n
        e'      = either id fst s
    e2  <- prettyElem e
    case e' of
      BlackBoxE "GHC.CString.unpackCString#" _ _ _ _ bbCtx' _ -> case bbInputs bbCtx' of
        [(Left (Literal Nothing (StringLit _)),_,_)] -> error $ $(curLoc) ++ "argument of ~FILEPATH:" ++ show e2 ++  "does not reduce to a string"
        _ ->  error $ $(curLoc) ++ "argument of ~FILEPATH:" ++ show e2 ++  "does not reduce to a string"
      _ -> error $ $(curLoc) ++ "argument of ~FILEPATH:" ++ show e2 ++  "does not reduce to a string"
  _ -> do e' <- prettyElem e
          error $ $(curLoc) ++ "~FILEPATH expects a ~LIT[N] argument, but got: " ++ show e'
renderTag b QSysIncludeName = case bbQsysIncName b of
  Just nm -> return nm
  _ -> error $ $(curLoc) ++ "~QSYSINCLUDENAME used where no 'qysInclude' was specified in the primitive definition"
renderTag _ e = do e' <- prettyElem e
                   error $ $(curLoc) ++ "Unable to evaluate: " ++ show e'

prettyBlackBox :: Monad m
               => BlackBoxTemplate
               -> m Text
prettyBlackBox bbT = Text.concat <$> mapM prettyElem bbT

prettyElem :: Monad m
           => Element
           -> m Text
prettyElem (C t) = return t
prettyElem (D (Decl i args)) = do
  args' <- mapM (\(a,b) -> (,) <$> prettyBlackBox a <*> prettyBlackBox b) args
  (displayT . renderOneLine) <$>
    (nest 2 (text "~INST" <+> int i <$$>
        text "~OUTPUT" <+> text "=>" <+> text (fst (head args')) <+> text (snd (head args')) <+> text "~" <$$>
        vcat (mapM (\(a,b) -> text "~INPUT" <+> text "=>" <+> text a <+> text b <+> text "~") (tail args')))
      PP.<$$> text "~INST")
prettyElem O = return "~RESULT"
prettyElem (I i) = (displayT . renderOneLine) <$> (text "~ARG" <> brackets (int i))
prettyElem (L i) = (displayT . renderOneLine) <$> (text "~LIT" <> brackets (int i))
prettyElem (Sym _ i) = (displayT . renderOneLine) <$> (text "~SYM" <> brackets (int i))
prettyElem (Clk Nothing) = return "~CLKO"
prettyElem (Clk (Just i)) = (displayT . renderOneLine) <$> (text "~CLK" <> brackets (int i))
prettyElem (Rst Nothing) = return "~RSTO"
prettyElem (Rst (Just i)) = (displayT . renderOneLine) <$> (text "~RSTO" <> brackets (int i))
prettyElem (Typ Nothing) = return "~TYPO"
prettyElem (Typ (Just i)) = (displayT . renderOneLine) <$> (text "~TYP" <> brackets (int i))
prettyElem (TypM Nothing) = return "~TYPMO"
prettyElem (TypM (Just i)) = (displayT . renderOneLine) <$> (text "~TYPM" <> brackets (int i))
prettyElem (Err Nothing) = return "~ERRORO"
prettyElem (Err (Just i)) = (displayT . renderOneLine) <$> (text "~ERROR" <> brackets (int i))
prettyElem (TypElem e) = do
  e' <- prettyElem e
  (displayT . renderOneLine) <$> (text "~TYPEL" <> brackets (text e'))
prettyElem CompName = return "~COMPNAME"
prettyElem QSysIncludeName = return "~QSYSINCLUDENAME"
prettyElem (IndexType e) = do
  e' <- prettyElem e
  (displayT . renderOneLine) <$> (text "~INDEXTYPE" <> brackets (text e'))
prettyElem (Size e) = do
  e' <- prettyElem e
  (displayT . renderOneLine) <$> (text "~SIZE" <> brackets (text e'))
prettyElem (Length e) = do
  e' <- prettyElem e
  (displayT . renderOneLine) <$> (text "~LENGTH" <> brackets (text e'))
prettyElem (Depth e) = do
  e' <- prettyElem e
  (displayT . renderOneLine) <$> (text "~DEPTH" <> brackets (text e'))
prettyElem (FilePath e) = do
  e' <- prettyElem e
  (displayT . renderOneLine) <$> (text "~FILE" <> brackets (text e'))
prettyElem (Gen b) = if b then return "~GENERATE" else return "~ENDGENERATE"
prettyElem (IF b esT esF) = do
  b' <- prettyElem b
  esT' <- prettyBlackBox esT
  esF' <- prettyBlackBox esF
  (displayT . renderCompact) <$>
    (text "~IF" <+> text b' <+> text "~THEN" <>
     text esT' <>
     text "~ELSE" <>
     text esF' <>
     text "~FI")
prettyElem (And es) =
  (displayT . renderCompact) <$>
  (PP.brackets (PP.tupled $ mapM (text <=< prettyElem) es))
prettyElem IW64 = return "~IW64"
prettyElem (HdlSyn s) = case s of
  Vivado -> return "~VIVADO"
  _ -> return "~OTHERSYN"
prettyElem (BV b es e) = do
  es' <- prettyBlackBox es
  e'  <- prettyBlackBox [e]
  (displayT . renderOneLine) <$>
    if b
       then text "~TOBV" <> brackets (text es') <> brackets (text e')
       else text "~FROMBV" <> brackets (text es') <> brackets (text e')
prettyElem (IsLit i) = (displayT . renderOneLine) <$> (text "~ISLIT" <> brackets (int i))
prettyElem (IsVar i) = (displayT . renderOneLine) <$> (text "~ISVAR" <> brackets (int i))
prettyElem (GenSym es i) = do
  es' <- prettyBlackBox es
  (displayT . renderOneLine) <$> (text "~GENSYM" <> brackets (text es') <> brackets (int i))
prettyElem (SigD es mI) = do
  es' <- prettyBlackBox es
  (displayT . renderOneLine) <$>
    (maybe (text "~SIGDO" <> brackets (text es'))
           (((text "~SIGD" <> brackets (text es')) <>) . int)
           mI)
prettyElem (Vars i) = (displayT . renderOneLine) <$> (text "~VARS" <> brackets (int i))

usedArguments :: BlackBoxTemplate
              -> [Int]
usedArguments = nub . concatMap go
  where
    go x = case x of
      D (Decl i args) -> i : concatMap (\(a,b) -> usedArguments a ++ usedArguments b) args
      I i -> [i]
      L i -> [i]
      IndexType e -> go e
      FilePath e -> go e
      IF b esT esF -> go b ++ usedArguments esT ++ usedArguments esF
      SigD es _ -> usedArguments es
      BV _ es _ -> usedArguments es
      _ -> []
