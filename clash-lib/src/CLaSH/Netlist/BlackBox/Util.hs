{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Utilties to verify blackbox contexts against templates and rendering
-- filled in templates
module CLaSH.Netlist.BlackBox.Util where

import           Control.Lens                         (at, use, (%=), (+=), _1,
                                                       _2)
import           Control.Monad.State                  (State, runState)
import           Control.Monad.Writer                 (MonadWriter, tell)
import           Data.Foldable                        (foldrM)
import qualified Data.IntMap                          as IntMap
import           Data.Maybe                           (fromJust)
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
import           CLaSH.Util

-- | Determine if the number of normal/literal/function inputs of a blackbox
-- context at least matches the number of argument that is expected by the
-- template.
verifyBlackBoxContext :: BlackBoxTemplate -- ^ Template to check against
                      -> BlackBoxContext -- ^ Blackbox to verify
                      -> Bool
verifyBlackBoxContext tmpl bbCtx =
  ((length (bbInputs bbCtx) - 1)    >= countArgs tmpl) &&
  ((length (bbLitInputs bbCtx) - 1) >= countLits tmpl) &&
  ((length (bbFunInputs bbCtx) - 1) >= countFuns tmpl)

-- | Count the number of argument tags/holes in a blackbox template
countArgs :: BlackBoxTemplate -> Int
countArgs [] = -1
countArgs l  = maximum
             $ map (\e -> case e of
                            I n -> n
                            D (Decl _ l') -> maximum $ map (countArgs . fst) l'
                            _ -> -1
                   ) l

-- | Counter the number of literal tags/holes in a blackbox template
countLits :: BlackBoxTemplate -> Int
countLits [] = -1
countLits l  = maximum
             $ map (\e -> case e of
                            L n -> n
                            D (Decl _ l') -> maximum $ map (countLits . fst) l'
                            _ -> -1
                   ) l

-- | Count the number of function instantiations in a blackbox template
countFuns :: BlackBoxTemplate -> Int
countFuns [] = -1
countFuns l  = maximum $ map (\e -> case e of { D (Decl n _) -> n; _ -> -1 }) l

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
                      SigD e m      -> SigD <$> (head <$> setSym' [e]) <*> pure m
                      _             -> pure e
              )

setClocks :: ( MonadWriter [(Identifier,HWType)] m
             , Applicative m
             )
          => BlackBoxContext
          -> BlackBoxTemplate
          -> m BlackBoxTemplate
setClocks bc bt = mapM setClocks' bt
  where
    setClocks' (D (Decl n l))  = D <$> (Decl n <$> mapM (combineM (setClocks bc) (setClocks bc)) l)
    setClocks' (SigD e m)      = SigD <$> (head <$> setClocks bc [e]) <*> pure m
    setClocks' (Clk Nothing)   = let (clk,rate) = clkSyncId $ fst $ bbResult bc
                                 in  tell [(clk,Clock rate)] >> return (C clk)
    setClocks' (Clk (Just n))  = let (clk,rate) = clkSyncId $ fst $ bbInputs bc !! n
                                 in  tell [(clk,Clock rate)] >> return (C clk)
    setClocks' (Rst Nothing)   = let (rst,rate) = (first (`Text.append` Text.pack "_rst")) . clkSyncId $ fst $ bbResult bc
                                 in  tell [(rst,Reset rate)] >> return (C rst)
    setClocks'  (Rst (Just n)) = let (rst,rate) = (first (`Text.append` Text.pack "_rst")) . clkSyncId $ fst $ bbInputs bc !! n
                                 in  tell [(rst,Reset rate)] >> return (C rst)
    setClocks' e               = return e

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
               -- -> ((Text, [(Identifier,HWType)]),backend)
renderBlackBox l bbCtx
  = fmap Text.concat
  -- $ flip runState s
  -- $ runWriterT
  -- $ runBlackBoxM
  $ mapM (renderElem bbCtx) l

-- | Render a single template element
renderElem :: Backend backend
           => BlackBoxContext
           -> Element
           -> State backend Text
renderElem b (D (Decl n (l:ls))) = do
    o  <- syncIdToSyncExpr <$> combineM (lineToIdentifier b) (lineToType b) l
    is <- mapM (fmap syncIdToSyncExpr . combineM (lineToIdentifier b) (lineToType b)) ls
    let (templ,pCtx) = indexNote ($(curLoc) ++ "No function argument " ++ show n) (bbFunInputs b) n
        b' = pCtx { bbResult = o, bbInputs = bbInputs pCtx ++ is }
    templ' <- either return (fmap (parseFail . displayT . renderCompact . fromJust) . inst) templ
    if verifyBlackBoxContext templ' b'
      then Text.concat <$> mapM (renderElem b') templ'
      else error $ $(curLoc) ++ "\nCan't match context:\n" ++ show b' ++ "\nwith template:\n" ++ show templ

renderElem b (SigD e m) = do
  e' <- renderElem b e
  t  <- hdlSig e' (maybe (snd $ bbResult b) (snd . (bbInputs b !!)) m)
  return (displayT $ renderOneLine t)

renderElem b e = renderTag b e

parseFail :: Text -> BlackBoxTemplate
parseFail t = case runParse t of
                    (templ,err) | null err  -> templ
                                | otherwise -> error $ $(curLoc) ++ "\nTemplate:\n" ++ show t ++ "\nHas errors:\n" ++ show err

syncIdToSyncExpr :: (Text,HWType)
                 -> (SyncExpr,HWType)
syncIdToSyncExpr (t,ty) = (Left (Identifier t Nothing),ty)

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
           -> State backend HWType
lineToType b [(Typ Nothing)]  = return (snd $ bbResult b)
lineToType b [(Typ (Just n))] = return (snd $ bbInputs b !! n)
lineToType b [(TypElem t)]    = do hwty' <- lineToType b [t]
                                   case hwty' of
                                     Vector _ elTy -> return elTy
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
renderTag b (I n)           = fmap (displayT . renderOneLine) . expr False . either id fst . fst $ bbInputs b !! n
renderTag b (L n)           = fmap (displayT . renderOneLine) . expr False $ bbLitInputs b !! n
renderTag _ (Sym n)         = return $ Text.pack ("n_" ++ show n)
renderTag b (Typ Nothing)   = fmap (displayT . renderOneLine) . hdlType . snd $ bbResult b
renderTag b (Typ (Just n))  = fmap (displayT . renderOneLine) . hdlType . snd $ bbInputs b !! n
renderTag b (TypM Nothing)  = fmap (displayT . renderOneLine) . hdlTypeMark . snd $ bbResult b
renderTag b (TypM (Just n)) = fmap (displayT . renderOneLine) . hdlTypeMark . snd $ bbInputs b !! n
renderTag b (Err Nothing)   = fmap (displayT . renderOneLine) . hdlTypeErrValue . snd $ bbResult b
renderTag b (Err (Just n))  = fmap (displayT . renderOneLine) . hdlTypeErrValue . snd $ bbInputs b !! n
renderTag _ (D _)           = error $ $(curLoc) ++ "Unexpected component declaration"
renderTag _ (TypElem _)     = error $ $(curLoc) ++ "Unexpected type element selector"
renderTag _ (Clk _)         = error $ $(curLoc) ++ "Unexpected clock"
renderTag _ (Rst _)         = error $ $(curLoc) ++ "Unexpected reset"
