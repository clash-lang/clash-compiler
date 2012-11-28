module CLaSH.Netlist.BlackBox.Util where

import Control.Monad.Writer (tell,runWriter)
import Data.Foldable (foldrM)
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text

import CLaSH.Netlist.BlackBox.Types
import CLaSH.Netlist.Types (Identifier,HWType(..))
import CLaSH.Util

verifyBlackBoxContext ::
  Line
  -> BlackBoxContext
  -> Bool
verifyBlackBoxContext tmpl bbCtx =
  ((length (inputs bbCtx) - 1)    == countArgs tmpl) &&
  ((length (litInputs bbCtx) - 1) >= countLits tmpl) &&
  ((length (funInputs bbCtx) - 1) >= countFuns tmpl)

countArgs :: Line -> Int
countArgs [] = -1
countArgs l  = maximum
             $ map (\e -> case e of
                            I n -> n
                            D (Decl _ l') -> maximum $ map countArgs l'
                            _ -> (-1)
                   ) l

countLits :: Line -> Int
countLits [] = -1
countLits l  = maximum
             $ map (\e -> case e of
                            L n -> n
                            D (Decl _ l') -> maximum $ map countLits l'
                            _ -> (-1)
                   ) l

countFuns :: Line -> Int
countFuns [] = -1
countFuns l  = maximum $ map (\e -> case e of { D (Decl n _) -> n; _ -> (-1) }) l

setSym :: Int -> Line -> Line
setSym i = map (\e -> case e of
                      Sym _ -> Sym i
                      D (Decl n l) -> D (Decl n $ map (setSym i) l)
                      _ -> e
               )

clkSyncId :: SyncIdentifier -> Identifier
clkSyncId (Right (_,clk)) = clk
clkSyncId (Left i) = error $ $(curLoc) ++ "No clock for: " ++ show i

renderBlackBox ::
  Line
  -> BlackBoxContext
  -> (Text, [(Identifier,HWType)])
renderBlackBox l bbCtx
  = (Text.concat >< List.nub)
  $ runWriter
  $ runB
  $ mapM (renderElem bbCtx) l

renderElem :: BlackBoxContext -> Element -> BlackBoxMonad Text
renderElem b (D (Decl n (l:ls))) = do
  o  <- lineToIdentifier b l
  is <- mapM (lineToIdentifier b) ls
  let (templ,pCtx) = funInputs b !! n
  let b' = pCtx { result = o, inputs = (inputs pCtx) ++ is }
  if (verifyBlackBoxContext templ b')
    then Text.concat <$> mapM (renderElem b') templ
    else error $ $(curLoc) ++ "\nCan't match context:\n" ++ show b' ++ "\nwith template:\n" ++ show templ

renderElem b e = fmap (either id fst) $ mkSyncIdentifier b e

lineToIdentifier :: BlackBoxContext -> Line -> BlackBoxMonad SyncIdentifier
lineToIdentifier b = foldrM (\e a -> do
                              e' <- mkSyncIdentifier  b e
                              case (e', a) of
                                (Left t, Left t')             -> return $ Left  (t `Text.append` t')
                                (Left t, Right (t',clk))      -> return $ Right (t `Text.append` t',clk)
                                (Right (t,clk), Left t')      -> return $ Right (t `Text.append` t',clk)
                                (Right (t,clk), Right (t',_)) -> return $ Right (t `Text.append` t',clk)
                   ) (Left Text.empty)

mkSyncIdentifier :: BlackBoxContext -> Element -> BlackBoxMonad SyncIdentifier
mkSyncIdentifier _ (C t)          = return $ Left t
mkSyncIdentifier b O              = return $ result b
mkSyncIdentifier b (I n)          = return $ (inputs b)!!n
mkSyncIdentifier b (L n)          = return $ Left $ (litInputs b)!!n
mkSyncIdentifier b (Sym n)        = return $ Left $ Text.pack ("n_" ++ show n)
mkSyncIdentifier b (Clk Nothing)  = let t = clkSyncId $ result b
                                    in tell [(t,Bit)] >> return (Left t)
mkSyncIdentifier b (Clk (Just n)) = let t = clkSyncId $ (inputs b)!!n
                                    in tell [(t,Bit)] >> return (Left t)
mkSyncIdentifier b (Rst Nothing)  = let t = (`Text.append` (Text.pack "_rst")) . clkSyncId $ result b
                                    in tell [(t,Bit)] >> return (Left t)
mkSyncIdentifier b (Rst (Just n)) = let t = (`Text.append` (Text.pack "_rst")) . clkSyncId $ (inputs b)!!n
                                    in tell [(t,Bit)] >> return (Left t)

