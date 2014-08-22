{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fcontext-stack=21 #-}

-- | Generate a VHDL testbench for a component given a set of stimuli and a
-- set of matching expected outputs
module CLaSH.Driver.TestbenchGen
  ( genTestBench )
where

import           Control.Concurrent.Supply        (Supply)
import           Control.Monad.State              (evalState)
import           Data.HashMap.Lazy                (HashMap)
import           Data.List                        (find,nub)
import           Data.Maybe                       (mapMaybe)
import           Data.Text.Lazy                   (isPrefixOf,pack,splitOn)
import qualified Data.Text.Lazy.Builder           as Builder
import qualified Data.Text.Lazy.Builder.RealFloat as Builder
import           Text.PrettyPrint.Leijen.Text     ((<+>), (<>))
import qualified Text.PrettyPrint.Leijen.Text     as PP
import           Unbound.LocallyNameless          (name2String)

import           CLaSH.Core.Term
import           CLaSH.Core.TyCon
import           CLaSH.Core.Type

import           CLaSH.Netlist
import           CLaSH.Netlist.Types              as N
import           CLaSH.Netlist.VHDL               (vhdlTypeErrValue)
import           CLaSH.Normalize                  (cleanupGraph, normalize,
                                                   runNormalization)
import           CLaSH.Primitives.Types
import           CLaSH.Rewrite.Types

import           CLaSH.Util


-- | Generate a VHDL testbench for a component given a set of stimuli and a
-- set of matching expected outputs
genTestBench :: DebugLevel
             -> Supply
             -> PrimMap                      -- ^ Primitives
             -> (HashMap TyConName TyCon -> Type -> Maybe (Either String HWType))
             -> HashMap TyConName TyCon
             -> (HashMap TyConName TyCon -> Term -> Term)
             -> VHDLState
             -> Int
             -> HashMap TmName (Type,Term)   -- ^ Global binders
             -> Maybe TmName                 -- ^ Stimuli
             -> Maybe TmName                 -- ^ Expected output
             -> Component                    -- ^ Component to generate TB for
             -> IO ([Component],VHDLState)
genTestBench dbgLvl supply primMap typeTrans tcm eval vhdlState cmpCnt globals stimuliNmM expectedNmM
  (Component cName hidden [inp] outp _) = do
  let ioDecl  = [ uncurry NetDecl inp  Nothing
                , uncurry NetDecl outp Nothing
                ]
      inpAssg = evalState (vhdlTypeErrValue (snd inp)) vhdlState
      inpExpr = Assignment (fst inp) (BlackBoxE (PP.displayT $ PP.renderCompact inpAssg) Nothing)
  (inpInst,inpComps,vhdlState',cmpCnt',hidden') <- maybe (return (inpExpr,[],vhdlState,cmpCnt,hidden))
                                                 (genStimuli vhdlState cmpCnt primMap globals typeTrans tcm normalizeSignal hidden inp)
                                                 stimuliNmM

  let finDecl = [ NetDecl "finished" Bool (Just (N.Literal Nothing (BoolLit False)))
                , Assignment "done" (Identifier "finished" Nothing)
                ]
      finAssg = "true after 100 ns"
      finExpr = Assignment "finished" (BlackBoxE (PP.displayT $ PP.renderCompact finAssg) Nothing)
  (expInst,expComps,vhdlState'',hidden'') <- maybe (return (finExpr,[],vhdlState',hidden'))
                                                 (genVerifier vhdlState' cmpCnt' primMap globals typeTrans tcm normalizeSignal hidden' outp)
                                                 expectedNmM
  let clkNms = mapMaybe (\hd -> case hd of (clkNm,Clock _) -> Just clkNm ; _ -> Nothing) hidden
      rstNms = mapMaybe (\hd -> case hd of (clkNm,Reset _) -> Just clkNm ; _ -> Nothing) hidden
      clks   = mapMaybe genClock hidden''
      rsts   = mapMaybe genReset hidden''

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

  return (tbComp:(inpComps++expComps),vhdlState'')
  where
    normalizeSignal :: HashMap TmName (Type,Term)
                    -> TmName
                    -> HashMap TmName (Type,Term)
    normalizeSignal glbls bndr =
      runNormalization dbgLvl supply glbls typeTrans tcm eval (normalize [bndr] >>= cleanupGraph bndr)

genTestBench _ _ _ _ _ _ v _ _ _ _ c = traceIf True ("Can't make testbench for: " ++ show c) $ return ([],v)

genClock :: (Identifier,HWType)
         -> Maybe [Declaration]
genClock (clkName,Clock rate) = Just clkDecls
  where
    clkGenDecl = PP.vsep
                  [ "-- pragma translate_off"
                  , "process is"
                  , "begin"
                  , PP.indent 2
                      (PP.vsep [ "wait for 2 ns;"
                               , "while (not finished) loop"
                               , PP.indent 2
                                  (PP.vsep [PP.text clkName <+> "<= not" <+> PP.text clkName <> PP.semi
                                           ,"wait for" <+> renderFloat2Dec (fromIntegral rate * 0.5) <+> "ns;"
                                           ,PP.text clkName <+> "<= not" <+> PP.text clkName <> PP.semi
                                           ,"wait for" <+> renderFloat2Dec (fromIntegral rate * 0.5) <+> "ns;"
                                           ])
                               , "end loop;"
                               , "wait;"
                               ])
                  , "end process;"
                  , "-- pragma translate_on"
                  ]

    clkDecls = [ NetDecl clkName (Clock rate) (Just (N.Literal Nothing (BitLit L)))
               , BlackBoxD (PP.displayT $ PP.renderPretty 0.4 80 clkGenDecl)
               ]

genClock _ = Nothing

genReset :: (Identifier,HWType)
         -> Maybe [Declaration]
genReset (rstName,Reset _) = Just rstDecls
  where
    rstExpr = PP.vsep
                [ "-- pragma translate_off"
                , PP.text rstName <+> "<=" <+> PP.align (PP.vsep (PP.punctuate PP.comma
                                                  [ "'0' after 0 ns"
                                                  , "'1' after 1 ns;"
                                                  ]))
                , "-- pragma translate_on"
                ]

    rstDecls = [ NetDecl rstName (BitVector 1) Nothing
               , BlackBoxD (PP.displayT $ PP.renderCompact rstExpr)
               ]

genReset _ = Nothing

renderFloat2Dec :: Float -> PP.Doc
renderFloat2Dec = PP.text . Builder.toLazyText . Builder.formatRealFloat Builder.Fixed (Just 2)

genStimuli :: VHDLState
           -> Int
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
           -> IO (Declaration,[Component],VHDLState,Int,[(Identifier,HWType)])
genStimuli vhdlState cmpCnt primMap globals typeTrans tcm normalizeSignal hidden inp signalNm = do
  let stimNormal = normalizeSignal globals signalNm
  (comps,vhdlState',cmpCnt') <- genNetlist (Just vhdlState) (Just cmpCnt) stimNormal primMap tcm typeTrans Nothing signalNm
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
  return (decl,comps,vhdlState',cmpCnt',hidden'')

genVerifier :: VHDLState
            -> Int
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
            -> IO (Declaration,[Component],VHDLState,[(Identifier,HWType)])
genVerifier vhdlState cmpCnt primMap globals typeTrans tcm normalizeSignal hidden outp signalNm = do
  let stimNormal = normalizeSignal globals signalNm
  (comps,vhdlState',_) <- genNetlist (Just vhdlState) (Just cmpCnt) stimNormal primMap tcm typeTrans Nothing signalNm
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
  return (decl,comps,vhdlState',hidden'')
