{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Generate a HDL testbench for a component given a set of stimuli and a
-- set of matching expected outputs
module CLaSH.Driver.TestbenchGen
  ( genTestBench )
where

import           Control.Concurrent.Supply        (Supply)
import           Control.Lens                     ((.=))
import           Data.HashMap.Lazy                (HashMap)
import qualified Data.HashMap.Lazy                as HashMap
import           Data.IntMap.Strict               (IntMap)
import           Data.List                        (find,nub)
import           Data.Maybe                       (catMaybes,mapMaybe)
import           Data.Text.Lazy                   (append,pack,splitOn)
import           Unbound.Generics.LocallyNameless (name2String)

import           CLaSH.Core.Term
import           CLaSH.Core.TyCon
import           CLaSH.Core.Type

import           CLaSH.Driver.Types

import           CLaSH.Netlist
import           CLaSH.Netlist.BlackBox           (prepareBlackBox)
import           CLaSH.Netlist.BlackBox.Types     (Element (Err))
import           CLaSH.Netlist.Types              as N
import           CLaSH.Normalize                  (cleanupGraph, normalize,
                                                   runNormalization)
import           CLaSH.Primitives.Types
import           CLaSH.Rewrite.Types

import           CLaSH.Util

-- | Generate a HDL testbench for a component given a set of stimuli and a
-- set of matching expected outputs
genTestBench :: CLaSHOpts
             -> Supply
             -> PrimMap                      -- ^ Primitives
             -> (HashMap TyConName TyCon -> Type -> Maybe (Either String HWType))
             -> HashMap TyConName TyCon
             -> IntMap TyConName
             -> (HashMap TyConName TyCon -> Bool -> Term -> Term)
             -> Int
             -> HashMap TmName (Type,Term)   -- ^ Global binders
             -> Maybe TmName                 -- ^ Stimuli
             -> Maybe TmName                 -- ^ Expected output
             -> String                       -- ^ Name of the module containing the @topEntity@
             -> [(String,FilePath)]          -- ^ Set of collected data-files
             -> Component                    -- ^ Component to generate TB for
             -> IO ([Component],[(String,FilePath)])
genTestBench opts supply primMap typeTrans tcm tupTcm eval cmpCnt globals stimuliNmM expectedNmM modName dfiles
  (Component cName hidden [inp] [outp] _) = do
  let iw      = opt_intWidth opts
      ioDecl  = [ uncurry NetDecl inp
                , uncurry NetDecl outp
                ]
      inpExpr = Assignment (fst inp) (BlackBoxE "" [Err Nothing] (emptyBBContext {bbResult = (undefined,snd inp)}) False)
  (inpInst,inpComps,cmpCnt',hidden',dfiles') <- maybe (return (inpExpr,[],cmpCnt,hidden,dfiles))
      (genStimuli cmpCnt primMap globals typeTrans tcm normalizeSignal hidden inp modName dfiles iw)
      stimuliNmM

  ((finDecl,finExpr),s) <- runNetlistMonad (Just cmpCnt') globals primMap tcm typeTrans modName dfiles' iw $ do
      done    <- genDone primMap
      let finDecl' = [ NetDecl "finished" Bool
                     , done
                     ]
      finExpr' <- genFinish primMap
      return (finDecl',finExpr')

  (expInst,expComps,cmpCnt'',hidden'',dfiles'') <- maybe (return (finExpr,[],cmpCnt',hidden',dfiles'))
      (genVerifier cmpCnt' primMap globals typeTrans tcm normalizeSignal hidden' outp modName dfiles' iw)
      expectedNmM

  let clkNms = mapMaybe (\hd -> case hd of (clkNm,Clock _ _) -> Just clkNm ; _ -> Nothing) hidden
      rstNms = mapMaybe (\hd -> case hd of (clkNm,Reset _ _) -> Just clkNm ; _ -> Nothing) hidden

  ((clks,rsts),_) <- runNetlistMonad (Just cmpCnt'') globals primMap tcm typeTrans modName dfiles'' iw $ do
      varCount .= (_varCount s)
      clks' <- catMaybes <$> mapM (genClock primMap) hidden''
      rsts' <- catMaybes <$> mapM (genReset primMap) hidden''
      return (clks',rsts')

  let instDecl = InstDecl cName "totest"
                   (map (\i -> (i,Identifier i Nothing))
                        (concat [ clkNms, rstNms, [fst inp], [fst outp] ])
                   )

      tbComp = Component (pack modName `append` "_testbench") [] [] [("done",Bool)]
                  (concat [ finDecl
                          , concat clks
                          , concat rsts
                          , ioDecl
                          , [instDecl,inpInst,expInst]
                          ])

  return (tbComp:(inpComps++expComps),dfiles'')
  where
    normalizeSignal :: HashMap TmName (Type,Term)
                    -> TmName
                    -> HashMap TmName (Type,Term)
    normalizeSignal glbls bndr =
      runNormalization opts supply glbls typeTrans tcm tupTcm eval (normalize [bndr] >>= cleanupGraph bndr)

genTestBench opts _ _ _ _ _ _ _ _ _ _ _ dfiles c = traceIf (opt_dbgLevel opts > DebugNone) ("Can't make testbench for: " ++ show c) $ return ([],dfiles)

genClock :: PrimMap
         -> (Identifier,HWType)
         -> NetlistMonad (Maybe [Declaration])
genClock primMap (clkName,Clock clkSym rate) =
  case HashMap.lookup "CLaSH.Driver.TestbenchGen.clockGen" primMap of
    Just (BlackBox _ (Left templ)) -> do
      let (rising,rest) = divMod (toInteger rate) 2
          falling       = rising + rest
          ctx = emptyBBContext
                  { bbResult = (Left (Identifier clkName Nothing), Clock clkSym rate)
                  , bbInputs = [ (Left (N.Literal Nothing (NumLit 3)),Signed 32,True)
                               , (Left (N.Literal Nothing (NumLit rising)),Signed 32,True)
                               , (Left (N.Literal Nothing (NumLit falling)),Signed 32,True)
                               ]
                  }
      templ' <- prepareBlackBox "CLaSH.Driver.TestbenchGen.clockGen" templ ctx
      let clkGenDecl = BlackBoxD "CLaSH.Driver.TestbenchGen.clockGen" templ' ctx
          clkDecls   = [ NetDecl clkName (Clock clkSym rate)
                       , clkGenDecl
                       ]
      return (Just clkDecls)
    pM -> error $ $(curLoc) ++ ("Can't make clock declaration for: " ++ show pM)

genClock _ _ = return Nothing

genReset :: PrimMap
         -> (Identifier,HWType)
         -> NetlistMonad (Maybe [Declaration])
genReset primMap (rstName,Reset clkSym rate) =
  case HashMap.lookup "CLaSH.Driver.TestbenchGen.resetGen" primMap of
    Just (BlackBox _ (Left templ)) -> do
      let ctx = emptyBBContext
                  { bbResult = (Left (Identifier rstName Nothing), Reset clkSym rate)
                  , bbInputs = [(Left (N.Literal Nothing (NumLit 2)),Signed 32,True)]
                  }
      templ' <- prepareBlackBox "CLaSH.Driver.TestbenchGen.resetGen" templ ctx
      let resetGenDecl =  BlackBoxD "CLaSH.Driver.TestbenchGen.resetGen" templ' ctx
          rstDecls     = [ NetDecl rstName (Reset clkSym rate)
                       , resetGenDecl
                       ]
      return (Just rstDecls)

    pM -> error $ $(curLoc) ++ ("Can't make reset declaration for: " ++ show pM)

genReset _ _ =  return Nothing

genFinish :: PrimMap
          -> NetlistMonad Declaration
genFinish primMap = case HashMap.lookup "CLaSH.Driver.TestbenchGen.finishedGen" primMap of
  Just (BlackBox _ (Left templ)) -> do
    let ctx = emptyBBContext
                { bbResult = (Left (Identifier "finished" Nothing), Bool)
                , bbInputs = [ (Left (N.Literal Nothing (NumLit 100)),Signed 32,True) ]
                }
    templ' <- prepareBlackBox "CLaSH.Driver.TestbenchGen.finishGen" templ ctx
    return $ BlackBoxD "CLaSH.Driver.TestbenchGen.finishGen" templ' ctx
  pM -> error $ $(curLoc) ++ ("Can't make finish declaration for: " ++ show pM)

genDone :: PrimMap
        -> NetlistMonad Declaration
genDone primMap = case HashMap.lookup "CLaSH.Driver.TestbenchGen.doneGen" primMap of
  Just (BlackBox _ (Left templ)) -> do
    let ctx = emptyBBContext
                { bbResult    = (Left (Identifier "done" Nothing), Bool)
                , bbInputs    = [(Left (Identifier "finished" Nothing),Bool,False)]
                }
    templ' <- prepareBlackBox "CLaSH.Driver.TestbenchGen.doneGen" templ ctx
    return $ BlackBoxD "CLaSH.Driver.TestbenchGen.doneGen" templ' ctx
  pM -> error $ $(curLoc) ++ ("Can't make done declaration for: " ++ show pM)

genStimuli :: Int
           -> PrimMap
           -> HashMap TmName (Type,Term)
           -> (HashMap TyConName TyCon -> Type -> Maybe (Either String HWType))
           -> HashMap TyConName TyCon
           -> ( HashMap TmName (Type,Term)
                -> TmName
                -> HashMap TmName (Type,Term) )
           -> [(Identifier,HWType)]
           -> (Identifier,HWType)
           -> String
           -> [(String,FilePath)]
           -> Int
           -> TmName
           -> IO (Declaration,[Component],Int,[(Identifier,HWType)],[(String,FilePath)])
genStimuli cmpCnt primMap globals typeTrans tcm normalizeSignal hidden inp modName dfiles iw signalNm = do
  let stimNormal = normalizeSignal globals signalNm
  (comps,dfiles',cmpCnt') <- genNetlist (Just cmpCnt) stimNormal primMap tcm typeTrans Nothing modName dfiles iw signalNm
  let sigNm   = pack (modName ++ "_") `append` last (splitOn (pack ".") (pack (name2String signalNm))) `append` pack "_" `append` (pack (show cmpCnt))
      sigComp = case find ((== sigNm) . componentName) comps of
                  Just c -> c
                  Nothing -> error $ $(curLoc) ++ "Can't locate component for stimuli gen: " ++ (show $ pack $ name2String signalNm) ++ show (map (componentName) comps)

      (cName,hidden',outp) = case sigComp of
                               (Component a b [] [(c,_)] _) -> (a,b,c)
                               (Component a _ is _ _)       -> error $ $(curLoc) ++ "Stimuli gen " ++ show a ++ " has unexpected inputs: " ++ show is
      hidden'' = nub (hidden ++ hidden')
      clkNms   = mapMaybe (\hd -> case hd of (clkNm,Clock _ _) -> Just clkNm ; _ -> Nothing) hidden'
      rstNms   = mapMaybe (\hd -> case hd of (clkNm,Reset _ _) -> Just clkNm ; _ -> Nothing) hidden'
      decl     = InstDecl cName "stimuli"
                   (map (\i -> (i,Identifier i Nothing))
                        (concat [ clkNms, rstNms ]) ++
                        [(outp,Identifier (fst inp) Nothing)]
                   )
  return (decl,comps,cmpCnt',hidden'',dfiles')

genVerifier :: Int
            -> PrimMap
            -> HashMap TmName (Type,Term)
            -> (HashMap TyConName TyCon -> Type -> Maybe (Either String HWType))
            -> HashMap TyConName TyCon
            -> ( HashMap TmName (Type,Term)
                 -> TmName
                 -> HashMap TmName (Type,Term) )
            -> [(Identifier,HWType)]
            -> (Identifier,HWType)
            -> String
            -> [(String,FilePath)]
            -> Int
            -> TmName
            -> IO (Declaration,[Component],Int,[(Identifier,HWType)],[(String,FilePath)])
genVerifier cmpCnt primMap globals typeTrans tcm normalizeSignal hidden outp modName dfiles iw signalNm = do
  let stimNormal = normalizeSignal globals signalNm
  (comps,dfiles',cmpCnt') <- genNetlist (Just cmpCnt) stimNormal primMap tcm typeTrans Nothing modName dfiles iw signalNm
  let sigNm   = pack (modName ++ "_") `append` last (splitOn (pack ".") (pack (name2String signalNm))) `append` "_" `append` (pack (show cmpCnt))
      sigComp = case find ((== sigNm) . componentName) comps of
                  Just c -> c
                  Nothing -> error $ $(curLoc) ++ "Can't locate component for Verifier: " ++ (show $ pack $ name2String signalNm) ++ show (map (componentName) comps)
      (cName,hidden',inp,fin) = case sigComp of
        (Component a b [(c,_)] [(d,_)] _) -> (a,b,c,d)
        (Component a _ is _ _)            -> error $ $(curLoc) ++ "Verifier " ++ show a ++ " has unexpected inputs: " ++ show is
      hidden'' = nub (hidden ++ hidden')
      clkNms   = mapMaybe (\hd -> case hd of (clkNm,Clock _ _) -> Just clkNm ; _ -> Nothing) hidden'
      rstNms   = mapMaybe (\hd -> case hd of (clkNm,Reset _ _) -> Just clkNm ; _ -> Nothing) hidden'
      decl     = InstDecl cName "verify"
                   (map (\i -> (i,Identifier i Nothing))
                        (concat [ clkNms, rstNms ]) ++
                        [(inp,Identifier (fst outp) Nothing),(fin,Identifier "finished" Nothing)]
                   )
  return (decl,comps,cmpCnt',hidden'',dfiles')
