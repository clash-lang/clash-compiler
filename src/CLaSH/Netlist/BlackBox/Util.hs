module CLaSH.Netlist.BlackBox.Util where

import Control.Monad.Writer (Writer,tell,runWriter)
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text

import CLaSH.Netlist.BlackBox.Types
import CLaSH.Netlist.Types (Identifier,HWType(..))
import CLaSH.Util

countArgs :: Line -> Int
countArgs [] = -1
countArgs l  = maximum $ map (\e -> case e of { I n -> n; _ -> (-1) }) l

countLits :: Line -> Int
countLits [] = -1
countLits l  = maximum $ map (\e -> case e of { L n -> n; _ -> (-1) }) l

countFuns :: Line -> Int
countFuns [] = -1
countFuns l  = maximum $ map (\e -> case e of { D (Decl n _) -> n; _ -> (-1) }) l

idSyncId :: SyncIdentifier -> Identifier
idSyncId (Left i) = i
idSyncId (Right (i,_)) = i

clkSyncId :: SyncIdentifier -> Identifier
clkSyncId (Right (_,clk)) = clk
clkSyncId (Left i) = error $ $(curLoc) ++ "No clock for: " ++ show i

renderBlackBox ::
  Line
  -> BlackBoxContext
  -> (Text, [(Identifier,HWType)])
renderBlackBox l bbCtx = (Text.concat >< List.nub) $ runWriter $ mapM (renderElem bbCtx) l

renderElem :: BlackBoxContext -> Element -> Writer [(Identifier,HWType)] Text
renderElem _ (C t)          = return t
renderElem b O              = return $! idSyncId $ result b
renderElem b (I n)          = return $! idSyncId $ (inputs b)!!n
renderElem b (L n)          = return $! (litInputs b)!!n
renderElem b (Clk Nothing)  = let t = clkSyncId $ result b
                              in tell [(t,Bit)] >> return t
renderElem b (Clk (Just n)) = let t = clkSyncId $ (inputs b)!!n
                              in tell [(t,Bit)] >> return t
renderElem b (Rst Nothing)  = let t = (`Text.append` (Text.pack "_rst")) . clkSyncId $ result b
                              in tell [(t,Bit)] >> return t
renderElem b (Rst (Just n)) = let t = (`Text.append` (Text.pack "_rst")) . clkSyncId $ (inputs b)!!n
                              in tell [(t,Bit)] >> return t
