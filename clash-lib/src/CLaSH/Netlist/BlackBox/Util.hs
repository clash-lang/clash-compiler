{-# LANGUAGE TemplateHaskell #-}

-- | Utilties to verify blackbox contexts against templates and rendering
-- filled in templates
module CLaSH.Netlist.BlackBox.Util where

import           Control.Lens                         (at, use, (%=), (+=), _1,
                                                       _2)
import           Control.Monad.State                  (State, lift, runState)
import           Control.Monad.Writer                 (runWriterT, tell)
import           Data.Foldable                        (foldrM)
import qualified Data.IntMap                          as IntMap
import qualified Data.List                            as List
import           Data.Text.Lazy                       (Text)
import qualified Data.Text.Lazy                       as Text
import           Text.PrettyPrint.Leijen.Text.Monadic (displayT, renderOneLine)

import           CLaSH.Netlist.BlackBox.Types
import           CLaSH.Netlist.Types                  (HWType (..), Identifier)
import           CLaSH.Netlist.VHDL                   (VHDLState,
                                                       vhdlType,
                                                       vhdlTypeErrValue,
                                                       vhdlTypeMark)
import           CLaSH.Util

-- | Determine if the number of normal/literal/function inputs of a blackbox
-- context at least matches the number of argument that is expected by the
-- template.
verifyBlackBoxContext :: BlackBoxTemplate -- ^ Template to check against
                      -> BlackBoxContext -- ^ Blackbox to verify
                      -> Bool
verifyBlackBoxContext tmpl bbCtx =
  ((length (inputs bbCtx) - 1)    >= countArgs tmpl) &&
  ((length (litInputs bbCtx) - 1) >= countLits tmpl) &&
  ((length (funInputs bbCtx) - 1) >= countFuns tmpl)

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
                      _             -> pure e
              )

-- | Get the name of the clock of an identifier
clkSyncId :: SyncIdentifier -> (Identifier,Int)
clkSyncId (Right (_,clk)) = clk
clkSyncId (Left i) = error $ $(curLoc) ++ "No clock for: " ++ show i

-- | Render a blackbox given a certain context. Returns a filled out template
-- and a list of 'hidden' inputs that must be added to the encompassing component.
renderBlackBox :: BlackBoxTemplate -- ^ Blackbox template
               -> BlackBoxContext -- ^ Context used to fill in the hole
               -> VHDLState
               -> ((Text, [(Identifier,HWType)]),VHDLState)
renderBlackBox l bbCtx s
  = first (Text.concat *** List.nub)
  $ flip runState s
  $ runWriterT
  $ runBlackBoxM
  $ mapM (renderElem bbCtx) l

-- | Render a single template element
renderElem :: BlackBoxContext
           -> Element
           -> BlackBoxMonad Text
renderElem b (D (Decl n (l:ls))) = do
  o  <- combineM (lineToIdentifier b) (lineToType b) l
  is <- mapM (combineM (lineToIdentifier b) (lineToType b)) ls
  let (templ,pCtx) = indexNote ($(curLoc) ++ "No function argument " ++ show n) (funInputs b) n
  let b' = pCtx { result = o, inputs = inputs pCtx ++ is }
  if verifyBlackBoxContext templ b'
    then Text.concat <$> mapM (renderElem b') templ
    else error $ $(curLoc) ++ "\nCan't match context:\n" ++ show b' ++ "\nwith template:\n" ++ show templ

renderElem b e = either id fst <$> mkSyncIdentifier b e

-- | Fill out the template corresponding to an output/input assignment of a
-- component instantiation, and turn it into a single identifier so it can
-- be used for a new blackbox context.
lineToIdentifier :: BlackBoxContext
                 -> BlackBoxTemplate
                 -> BlackBoxMonad SyncIdentifier
lineToIdentifier b = foldrM (\e a -> do
                              e' <- mkSyncIdentifier  b e
                              case (e', a) of
                                (Left t, Left t')             -> return (Left  (t `Text.append` t'))
                                (Left t, Right (t',clk))      -> return (Right (t `Text.append` t',clk))
                                (Right (t,clk), Left t')      -> return (Right (t `Text.append` t',clk))
                                (Right (t,clk), Right (t',_)) -> return (Right (t `Text.append` t',clk))
                   ) (Left Text.empty)

lineToType :: BlackBoxContext
           -> BlackBoxTemplate
           -> BlackBoxMonad HWType
lineToType b [(Typ Nothing)]  = return (snd $ result b)
lineToType b [(Typ (Just n))] = return (snd $ inputs b !! n)
lineToType b [(TypElem t)]    = do hwty' <- lineToType b [t]
                                   case hwty' of
                                     Vector _ elTy -> return elTy
                                     _ -> error $ $(curLoc) ++ "Element type selection of a non-vector type"
lineToType _ _ = error $ $(curLoc) ++ "Unexpected type manipulation"

-- | Give a context and a tagged hole (of a template), returns part of the
-- context that matches the tag of the hole.
mkSyncIdentifier :: BlackBoxContext
                 -> Element
                 -> BlackBoxMonad SyncIdentifier
mkSyncIdentifier _ (C t)           = return $ Left t
mkSyncIdentifier b O               = return $ fst $ result b
mkSyncIdentifier b (I n)           = return $ fst $ inputs b !! n
mkSyncIdentifier b (L n)           = return $ Left $ litInputs b !! n
mkSyncIdentifier _ (Sym n)         = return $ Left $ Text.pack ("n_" ++ show n)
mkSyncIdentifier b (Clk Nothing)   = let (clk,rate) = clkSyncId $ fst $ result b
                                     in tell [(clk,Clock rate)] >> return (Left clk)
mkSyncIdentifier b (Clk (Just n))  = let (clk,rate) = clkSyncId $ fst $ inputs b !! n
                                     in tell [(clk,Clock rate)] >> return (Left clk)
mkSyncIdentifier b (Rst Nothing)   = let (rst,rate) = (first (`Text.append` Text.pack "_rst")) . clkSyncId $ fst $ result b
                                     in tell [(rst,Reset rate)] >> return (Left rst)
mkSyncIdentifier b (Rst (Just n))  = let (rst,rate) = (first (`Text.append` Text.pack "_rst")) . clkSyncId $ fst $ inputs b !! n
                                     in tell [(rst,Reset rate)] >> return (Left rst)
mkSyncIdentifier b (Typ Nothing)   = fmap (Left . displayT . renderOneLine) . B . lift . vhdlType . snd $ result b
mkSyncIdentifier b (Typ (Just n))  = fmap (Left . displayT . renderOneLine) . B . lift . vhdlType . snd $ inputs b !! n
mkSyncIdentifier b (TypM Nothing)  = fmap (Left . displayT . renderOneLine) . B . lift . vhdlTypeMark . snd $ result b
mkSyncIdentifier b (TypM (Just n)) = fmap (Left . displayT . renderOneLine) . B . lift . vhdlTypeMark . snd $ inputs b !! n
mkSyncIdentifier b (Err Nothing)   = fmap (Left . displayT . renderOneLine) . B . lift . vhdlTypeErrValue . snd $ result b
mkSyncIdentifier b (Err (Just n))  = fmap (Left . displayT . renderOneLine) . B . lift . vhdlTypeErrValue . snd $ inputs b !! n
mkSyncIdentifier _ (D _)           = error $ $(curLoc) ++ "Unexpected component declaration"
mkSyncIdentifier _ (TypElem _)     = error $ $(curLoc) ++ "Unexpected type element selector"
