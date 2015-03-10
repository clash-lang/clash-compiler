{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Generate a HDL testbench for a component given a set of stimuli and a
-- set of matching expected outputs
module CLaSH.Driver.TestbenchGen
  ( genTestBench )
where

import           Control.Concurrent.Supply        (Supply)
import           Data.HashMap.Lazy                (HashMap)
import qualified Data.HashMap.Lazy                as HashMap
import           Data.List                        (find,nub)
import           Data.Maybe                       (mapMaybe)
import           Data.Text.Lazy                   (isPrefixOf,pack,splitOn)
import           Unbound.Generics.LocallyNameless          (name2String)

import           CLaSH.Core.Term
import           CLaSH.Core.TyCon
import           CLaSH.Core.Type

import           CLaSH.Netlist
import           CLaSH.Netlist.BlackBox.Types     (Element (Err))
import           CLaSH.Netlist.BlackBox.Util      (parseFail)
import           CLaSH.Netlist.Types              as N
import           CLaSH.Normalize                  (cleanupGraph, normalize,
                                                   runNormalization)
import           CLaSH.Primitives.Types
import           CLaSH.Rewrite.Types

import           CLaSH.Util

-- | Generate a HDL testbench for a component given a set of stimuli and a
-- set of matching expected outputs
genTestBench :: DebugLevel
             -> Supply
             -> PrimMap                      -- ^ Primitives
             -> (HashMap TyConName TyCon -> Type -> Maybe (Either String HWType))
             -> HashMap TyConName TyCon
             -> (HashMap TyConName TyCon -> Term -> Term)
             -> Int
             -> HashMap TmName (Type,Term)   -- ^ Global binders
             -> Maybe TmName                 -- ^ Stimuli
             -> Maybe TmName                 -- ^ Expected output
             -> Component                    -- ^ Component to generate TB for
             -> IO ([Component])
genTestBench dbgLvl supply primMap typeTrans tcm eval cmpCnt globals stimuliNmM expectedNmM
  (Component cName hidden [inp] outp _) = do
  let ioDecl  = [ uncurry NetDecl inp
                , uncurry NetDecl outp
                ]
      inpExpr = Assignment (fst inp) (BlackBoxE "" [Err Nothing] (emptyBBContext {bbResult = (undefined,snd inp)}) False)
  (inpInst,inpComps,cmpCnt',hidden') <- maybe (return (inpExpr,[],cmpCnt,hidden))
                                                 (genStimuli cmpCnt primMap globals typeTrans tcm normalizeSignal hidden inp)
                                                 stimuliNmM

  let finDecl = [ NetDecl "finished" Bool
                , genDone primMap
                ]
      finExpr = genFinish primMap
  (expInst,expComps,hidden'') <- maybe (return (finExpr,[],hidden'))
                                                 (genVerifier cmpCnt' primMap globals typeTrans tcm normalizeSignal hidden' outp)
                                                 expectedNmM
  let clkNms = mapMaybe (\hd -> case hd of (clkNm,Clock _) -> Just clkNm ; _ -> Nothing) hidden
      rstNms = mapMaybe (\hd -> case hd of (clkNm,Reset _) -> Just clkNm ; _ -> Nothing) hidden
      clks   = mapMaybe (genClock primMap) hidden''
      rsts   = mapMaybe (genReset primMap) hidden''

  let instDecl = InstDecl cName "totest"
                   (map (\i -> (i,Identifier i Nothing))
                        (concat [ clkNms, rstNms, [fst inp], [fst outp] ])
                   )

      tbComp = Component "testbench" [] [] ("done",Bool)
                  (concat [ finDecl
                          , concat clks
                          , concat rsts
                          , ioDecl
                          , [instDecl,inpInst,expInst]
                          ])

  return (tbComp:(inpComps++expComps))
  where
    normalizeSignal :: HashMap TmName (Type,Term)
                    -> TmName
                    -> HashMap TmName (Type,Term)
    normalizeSignal glbls bndr =
      runNormalization dbgLvl supply glbls typeTrans tcm eval (normalize [bndr] >>= cleanupGraph bndr)

genTestBench dbgLvl _ _ _ _ _ _ _ _ _ c = traceIf (dbgLvl > DebugNone) ("Can't make testbench for: " ++ show c) $ return []

genClock :: PrimMap
         -> (Identifier,HWType)
         -> Maybe [Declaration]
genClock primMap (clkName,Clock rate) = Just clkDecls
  where
    clkGenDecl = case HashMap.lookup "CLaSH.Driver.TestbenchGen.clockGen" primMap of
      Just (BlackBox _ (Left templ)) -> let (rising,rest) = divMod (toInteger rate) 2
                                            falling       = rising + rest
                                            ctx = emptyBBContext
                                                    { bbResult = (Left (Identifier clkName Nothing), Clock rate)
                                                    , bbInputs = [ (Left (N.Literal Nothing (NumLit 2)),Integer,True)
                                                                 , (Left (N.Literal Nothing (NumLit rising)),Integer,True)
                                                                 , (Left (N.Literal Nothing (NumLit falling)),Integer,True)
                                                                 ]
                                                    }
                                        in  BlackBoxD "CLaSH.Driver.TestbenchGen.clockGen" (parseFail templ) ctx
      pM -> error $ $(curLoc) ++ ("Can't make clock declaration for: " ++ show pM)

    clkDecls   = [ NetDecl clkName (Clock rate)
                 , clkGenDecl
                 ]

genClock _ _ = Nothing

genReset :: PrimMap
         -> (Identifier,HWType)
         -> Maybe [Declaration]
genReset primMap (rstName,Reset clk) = Just rstDecls
  where
    resetGenDecl = case HashMap.lookup "CLaSH.Driver.TestbenchGen.resetGen" primMap of
      Just (BlackBox _ (Left templ)) -> let ctx = emptyBBContext
                                                    { bbResult = (Left (Identifier rstName Nothing), Reset clk)
                                                    , bbInputs = [(Left (N.Literal Nothing (NumLit 1)),Integer,True)]
                                                    }
                                        in  BlackBoxD "CLaSH.Driver.TestbenchGen.resetGen" (parseFail templ) ctx
      pM -> error $ $(curLoc) ++ ("Can't make reset declaration for: " ++ show pM)

    rstDecls = [ NetDecl rstName (Reset clk)
               , resetGenDecl
               ]

genReset _ _ = Nothing

genFinish :: PrimMap
          -> Declaration
genFinish primMap = case HashMap.lookup "CLaSH.Driver.TestbenchGen.finishedGen" primMap of
  Just (BlackBox _ (Left templ)) -> let ctx = emptyBBContext
                                                { bbResult = (Left (Identifier "finished" Nothing), Bool)
                                                , bbInputs = [ (Left (N.Literal Nothing (NumLit 100)),Integer,True) ]
                                                }
                                    in  BlackBoxD "CLaSH.Driver.TestbenchGen.finishGen" (parseFail templ) ctx
  pM -> error $ $(curLoc) ++ ("Can't make finish declaration for: " ++ show pM)

genDone :: PrimMap
        -> Declaration
genDone primMap = case HashMap.lookup "CLaSH.Driver.TestbenchGen.doneGen" primMap of
  Just (BlackBox _ (Left templ)) -> let ctx = emptyBBContext
                                                { bbResult    = (Left (Identifier "done" Nothing), Bool)
                                                , bbInputs    = [(Left (Identifier "finished" Nothing),Bool,False)]
                                                }
                                    in  BlackBoxD "CLaSH.Driver.TestbenchGen.doneGen" (parseFail templ) ctx
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
           -> TmName
           -> IO (Declaration,[Component],Int,[(Identifier,HWType)])
genStimuli cmpCnt primMap globals typeTrans tcm normalizeSignal hidden inp signalNm = do
  let stimNormal = normalizeSignal globals signalNm
  (comps,cmpCnt') <- genNetlist (Just cmpCnt) stimNormal primMap tcm typeTrans Nothing signalNm
  let sigNm   = last (splitOn (pack ".") (pack (name2String signalNm)))
      sigComp = case find ((isPrefixOf sigNm) . componentName) comps of
                  Just c -> c
                  Nothing -> error $ $(curLoc) ++ "Can't locate component for stimuli gen: " ++ (show $ pack $ name2String signalNm) ++ show (map (componentName) comps)

      (cName,hidden',outp) = case sigComp of
                               (Component a b [] (c,_) _) -> (a,b,c)
                               (Component a _ is _ _)     -> error $ $(curLoc) ++ "Stimuli gen " ++ show a ++ " has unexpected inputs: " ++ show is
      hidden'' = nub (hidden ++ hidden')
      clkNms   = mapMaybe (\hd -> case hd of (clkNm,Clock _) -> Just clkNm ; _ -> Nothing) hidden'
      rstNms   = mapMaybe (\hd -> case hd of (clkNm,Reset _) -> Just clkNm ; _ -> Nothing) hidden'
      decl     = InstDecl cName "stimuli"
                   (map (\i -> (i,Identifier i Nothing))
                        (concat [ clkNms, rstNms ]) ++
                        [(outp,Identifier (fst inp) Nothing)]
                   )
  return (decl,comps,cmpCnt',hidden'')

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
            -> TmName
            -> IO (Declaration,[Component],[(Identifier,HWType)])
genVerifier cmpCnt primMap globals typeTrans tcm normalizeSignal hidden outp signalNm = do
  let stimNormal = normalizeSignal globals signalNm
  (comps,_) <- genNetlist (Just cmpCnt) stimNormal primMap tcm typeTrans Nothing signalNm
  let sigNm   = last (splitOn (pack ".") (pack (name2String signalNm)))
      sigComp = case find ((isPrefixOf sigNm) . componentName) comps of
                  Just c -> c
                  Nothing -> error $ $(curLoc) ++ "Can't locate component for Verifier: " ++ (show $ pack $ name2String signalNm) ++ show (map (componentName) comps)
      (cName,hidden',inp,fin) = case sigComp of
        (Component a b [(c,_)] (d,_) _) -> (a,b,c,d)
        (Component a _ is _ _)     -> error $ $(curLoc) ++ "Verifier " ++ show a ++ " has unexpected inputs: " ++ show is
      hidden'' = nub (hidden ++ hidden')
      clkNms   = mapMaybe (\hd -> case hd of (clkNm,Clock _) -> Just clkNm ; _ -> Nothing) hidden'
      rstNms   = mapMaybe (\hd -> case hd of (clkNm,Reset _) -> Just clkNm ; _ -> Nothing) hidden'
      decl     = InstDecl cName "verify"
                   (map (\i -> (i,Identifier i Nothing))
                        (concat [ clkNms, rstNms ]) ++
                        [(inp,Identifier (fst outp) Nothing),(fin,Identifier "finished" Nothing)]
                   )
  return (decl,comps,hidden'')
