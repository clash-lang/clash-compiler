{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}

-- | Utilties to verify blackbox contexts against templates and rendering
-- filled in templates
module CLaSH.Netlist.BlackBox.Util where

import           Control.Lens                         (at, use, (%=), (+=), _1,
                                                       _2)
import           Control.Monad.State                  (State, runState)
import           Control.Monad.Writer                 (MonadWriter, tell)
import           Data.Foldable                        (foldrM)
import qualified Data.IntMap                          as IntMap
import           Data.Set                             (Set,singleton)
import           Data.Text.Lazy                       (Text)
import qualified Data.Text.Lazy                       as Text
import           Text.PrettyPrint.Leijen.Text.Monadic (displayT, renderCompact,
                                                       renderOneLine)

import           CLaSH.Backend                        (Backend (..))
import           CLaSH.Netlist.BlackBox.Parser
import           CLaSH.Netlist.BlackBox.Types
import           CLaSH.Netlist.Types                  (HWType (..), Identifier,
                                                       BlackBoxContext (..),
                                                       SyncExpr, Expr(Identifier))
import           CLaSH.Netlist.Util                   (typeSize)
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
setSym :: Int -> BlackBoxTemplate -> (BlackBoxTemplate,Int)
setSym i l
  = second fst
  $ runState (setSym' l) (i,IntMap.empty)
  where
    setSym' :: BlackBoxTemplate -> State (Int,IntMap.IntMap Int) BlackBoxTemplate
    setSym' = mapM (\e -> case e of
                      Sym i'        -> do symM <- use (_2 . at i')
                                          case symM of
                                            Nothing -> do k <- use _1
                                                          _1 += 1
                                                          _2 %= IntMap.insert i' k
                                                          return (Sym k)
                                            Just k  -> return (Sym k)
                      D (Decl n l') -> D <$> (Decl n <$> mapM (combineM setSym' setSym') l')
                      SigD e' m     -> SigD <$> (head <$> setSym' [e']) <*> pure m
                      _             -> pure e
              )

setCompName :: Identifier -> BlackBoxTemplate -> BlackBoxTemplate
setCompName nm = map setCompName'
  where
    setCompName' CompName = C nm
    setCompName' e        = e

setClocks :: ( MonadWriter (Set (Identifier,HWType)) m
             , Applicative m
             )
          => BlackBoxContext
          -> BlackBoxTemplate
          -> m BlackBoxTemplate
setClocks bc bt = mapM setClocks' bt
  where
    setClocks' (D (Decl n l)) = D <$> (Decl n <$> mapM (combineM (setClocks bc) (setClocks bc)) l)
    setClocks' (SigD e m)     = SigD <$> (head <$> setClocks bc [e]) <*> pure m

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

-- | Get the name of the clock of an identifier
clkSyncId :: SyncExpr -> (Identifier,Int)
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
      else error $ $(curLoc) ++ "\nCan't match context:\n" ++ show b' ++ "\nwith template:\n" ++ show templ

renderElem b (SigD e m) = do
  e' <- renderElem b e
  let ty = case m of
             Nothing -> snd $ bbResult b
             Just n  -> let (_,ty',_) = bbInputs b !! n
                        in  ty'
  t  <- hdlSig e' ty
  return (displayT $ renderOneLine t)

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
                              in  (displayT . renderOneLine) <$> expr False e
renderTag _ (Sym n)         = return $ Text.pack ("n_" ++ show n)
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
renderTag b e@(TypElem _)   = let ty = lineToType b [e]
                              in  (displayT . renderOneLine) <$> hdlType ty

renderTag _ (D _)           = error $ $(curLoc) ++ "Unexpected component declaration"
renderTag _ (SigD _ _)      = error $ $(curLoc) ++ "Unexpected signal declaration"
renderTag _ (Clk _)         = error $ $(curLoc) ++ "Unexpected clock"
renderTag _ (Rst _)         = error $ $(curLoc) ++ "Unexpected reset"
renderTag _ CompName        = error $ $(curLoc) ++ "Unexpected component name"
