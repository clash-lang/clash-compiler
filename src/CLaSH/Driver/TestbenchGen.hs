{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module CLaSH.Driver.TestbenchGen where

import           Control.Concurrent.Supply   (Supply)
import           Control.Error               (EitherT,eitherT,hoistEither,left,note,right)
import           Control.Monad.Trans.Class   (lift)
import           Data.Either                 (lefts)
import           Data.List                   (intersperse)
import           Data.HashMap.Lazy           (HashMap)
import qualified Data.HashMap.Lazy           as HashMap
import           Data.Maybe                  (catMaybes)
import qualified Data.Text.Lazy.Builder.RealFloat as Builder
import qualified Data.Text.Lazy.Builder           as Builder
import qualified Text.PrettyPrint.Leijen.Text     as PP
import           Text.PrettyPrint.Leijen.Text     ((<+>),(<>))
import           Unbound.LocallyNameless     (bind,makeName,name2String,name2Integer,rec,unrec)
import           Unbound.LocallyNameless.Ops (unsafeUnbind)

import CLaSH.Core.DataCon
import CLaSH.Core.Pretty
import CLaSH.Core.Term
import CLaSH.Core.TyCon
import CLaSH.Core.Type
import CLaSH.Core.Util

import CLaSH.Driver.Types
import CLaSH.Netlist
import CLaSH.Netlist.Types as N
import CLaSH.Normalize (runNormalization,normalize,cleanupGraph)
import CLaSH.Primitives.Types
import CLaSH.Rewrite.Types

import CLaSH.Util

genTestBench :: DebugLevel
             -> Supply
             -> DFunMap                      -- ^ Dictionary Functions
             -> ClassOpMap                   -- ^ Class operators
             -> PrimMap                      -- ^ Primitives
             -> VHDLState
             -> HashMap TmName (Type,Term)   -- ^ Global binders
             -> Maybe TmName                 -- ^ Stimuli
             -> Maybe TmName                 -- ^ Expected output
             -> Component                    -- ^ Component to generate TB for
             -> IO ([Component],VHDLState)
genTestBench dbgLvl supply dfunMap clsOpMap primMap vhdlState globals stimuliNmM expectedNmM
  (Component cName [(clkName,Clock rate),(rstName,Reset reset)] [inp] outp _) = eitherT error return $ do
  let rateF  = fromIntegral rate :: Float
      resetF = fromIntegral reset :: Float
  (inpDecls,inpComps,vhdlState',inpCnt) <- maybe' (right ([],[],vhdlState,0)) stimuliNmM $ \stimuliNm -> do
    (decls,sigVs,comps,vhdlState') <- prepareSignals vhdlState primMap globals normalizeSignal Nothing stimuliNm
    let sigAs     = zipWith (\s t -> PP.hsep
                                     -- [ (PP.text . Text.pack . name2String . varName) s
                                     [ PP.text s
                                     , "after"
                                     , (PP.text . Builder.toLazyText . Builder.formatRealFloat Builder.Fixed (Just 2)) t
                                     , "ns"
                                     ]
                            )
                      sigVs
                      (0.0:iterate (+rateF) (0.6 * rateF))
        sigAs'    = BlackBoxE (PP.displayT . PP.renderPretty 0.4 80 . PP.vsep $ PP.punctuate PP.comma sigAs) Nothing
        inpAssign = Assignment (fst inp) sigAs'
    return (inpAssign:decls,comps,vhdlState',length sigVs)

  (expDecls,expComps,vhdlState'',expCnt) <- maybe' (right ([],[],vhdlState',0)) expectedNmM $ \expectedNm -> do
    (decls,sigVs,comps,vhdlState'') <- prepareSignals vhdlState' primMap globals normalizeSignal (Just inpCnt) expectedNm
    let asserts  = map (genAssert (fst outp)) sigVs
        procDecl = PP.vsep
                   [ "process is"
                   , "begin"
                   , PP.indent 2 ( PP.vsep $
                                   map (<> PP.semi) $
                                   concat [ ["wait for" <+> (renderFloat2Dec $ rateF * 0.4) <+> "ns" ]
                                            , intersperse ("wait for" <+> (renderFloat2Dec rateF) <+> "ns") asserts
                                            , ["wait"]
                                            ]
                                 )
                   , "end process" <> PP.semi
                   ]
        procDecl' = BlackBoxD (PP.displayT $ PP.renderPretty 0.4 80 procDecl)
    return (procDecl':decls,comps,vhdlState'',length sigVs)

  let finExpr = "'1' after" <+> (renderFloat2Dec (rateF * ((fromIntegral $ max inpCnt expCnt) - 0.5))) <+> "ns"
      finDecl = [ NetDecl "finished" Bit (Just (N.Literal Nothing (BitLit L)))
                , Assignment "finished" (BlackBoxE (PP.displayT $ PP.renderCompact finExpr) Nothing)
                , Assignment "done" (Identifier "finished" Nothing)
                ]

      clkExpr = "not" <+> PP.text clkName <+> "after" <+> (renderFloat2Dec $ rateF * 0.5) <+> "ns when finished = '0'"
      clkDecl = [ NetDecl clkName (Clock rate) (Just (N.Literal Nothing (BitLit L)))
                , Assignment clkName (BlackBoxE (PP.displayT $ PP.renderCompact clkExpr) Nothing)
                ]

      retExpr = PP.vcat $ PP.punctuate PP.comma
                  [ "'0' after 0 ns"
                  , "'1' after" <+> renderFloat2Dec (0.24 * resetF) <+> "ns"
                  ]
      retDecl = [ NetDecl rstName Bit Nothing
                , Assignment rstName (BlackBoxE (PP.displayT $ PP.renderCompact retExpr) Nothing)
                ]
      ioDecl  = [ NetDecl (fst inp) (snd inp) Nothing
                , NetDecl (fst outp) (snd outp) Nothing
                ]

      instDecl = InstDecl cName "totest"
                  (map (\i -> (i,Identifier i Nothing))
                       [ clkName, rstName, fst inp, fst outp ]
                  )

      tbComp = Component "testbench" [] [] ("done",Bit)
                  (concat [ finDecl
                          , clkDecl
                          , retDecl
                          , ioDecl
                          , [instDecl]
                          , inpDecls
                          , expDecls
                          ])

  return (tbComp:inpComps ++ expComps,vhdlState'')

  where
    normalizeSignal :: (HashMap TmName (Type,Term) -> TmName -> [(TmName,(Type,Term))])
    normalizeSignal glbls bndr =
      runNormalization dbgLvl supply glbls dfunMap clsOpMap (normalize [bndr] >>= cleanupGraph [bndr])

genTestBench _ _ _ _ _ _ _ _ _ c = error $ "Can't make testbench for: " ++ show c

renderFloat2Dec :: Float -> PP.Doc
renderFloat2Dec = PP.text . Builder.toLazyText . (Builder.formatRealFloat Builder.Fixed (Just 2))

genAssert :: Identifier -> Identifier -> PP.Doc
genAssert compO expV = PP.hsep
  [ PP.text "assert"
  , PP.parens $ PP.hsep [ PP.text compO
                        , PP.equals
                        , PP.text expV
                        ]
  , PP.text "report"
  , PP.parens (PP.hsep [ "\"expected: \" &"
                       , "to_string" <+> PP.parens (PP.text expV)
                       , "& \", actual: \" &"
                       , "to_string" <+> PP.parens (PP.text compO)
                       ])
  , PP.text "severity error"
  ]

prepareSignals ::
  VHDLState
  -> PrimMap
  -> HashMap TmName (Type,Term)
  -> (HashMap TmName (Type,Term) -> TmName -> [(TmName,(Type,Term))])
  -> Maybe Int
  -> TmName
  -> EitherT String IO ([Declaration],[Identifier],[Component],VHDLState)
prepareSignals vhdlState primMap globals normalizeSignal mStart signalNm = do
  let signalS = name2String signalNm
  (signalTy,signalTm) <- hoistEither $ note ($(curLoc) ++ "Unable to find: " ++ signalS)
                                            (HashMap.lookup signalNm globals)
  signalList          <- termToList signalTm
  elemTy              <- stimuliElemTy signalTy

  let signalK  = name2Integer signalNm
      elemNms  = map (\i -> makeName (signalS ++ show i) signalK) [(0::Int)..]
      elemBnds = zipWith (\nm e -> (nm,(elemTy,e))) elemNms signalList
      signalList_normalized = map (normalizeSignal (HashMap.fromList elemBnds `HashMap.union` globals))
                                  (map fst elemBnds)

  lift $ createSignal vhdlState primMap mStart signalList_normalized

termToList :: Monad m => Term -> EitherT String m [Term]
termToList e = case (second lefts $ collectArgs e) of
  (Data _ dc,[])
    | name2String (dcName dc) == "[]" -> pure []
    | otherwise                                 -> errNoConstruct $(curLoc)
  (Data _ dc,[hdArg,tlArg])
    | name2String (dcName dc) == ":"  -> (hdArg:) <$> termToList tlArg
    | otherwise                                 -> errNoConstruct $(curLoc)
  _ -> errNoConstruct $(curLoc)
  where
    errNoConstruct l = left $ l ++ "Can't deconstruct list literal: " ++ show (second lefts $ collectArgs e)

stimuliElemTy ::Monad m => Type -> EitherT String m Type
stimuliElemTy ty = case splitTyConAppM ty of
  (Just (tc,[arg]))
    | name2String (tyConName tc) == "GHC.Types.[]" -> return arg
    | otherwise -> left $ $(curLoc) ++ "Not a List TyCon: " ++ showDoc ty
  _ -> left $ $(curLoc) ++ "Not a List TyCon: " ++ showDoc ty

createSignal ::
  VHDLState
  -> PrimMap
  -> Maybe Int
  -> [[(TmName,(Type,Term))]]
  -> IO ([Declaration],[Identifier],[Component],VHDLState)
createSignal vhdlState primMap mStart normalizedSignals = do
  let (signalHds,signalTls) = unzip $ map (\(l:ls) -> (l,ls)) normalizedSignals
      sigEs                 = map (\(_,(_,Letrec b)) -> unrec . fst $ unsafeUnbind b
                                  ) signalHds
      newExpr               = Letrec $ bind (rec $ concat sigEs)
                                            (Var (fst . snd $ head signalHds)
                                                 (fst $ head signalHds))
      newBndr               = ((fst $ head signalHds), (fst . snd $ head signalHds, newExpr))

  ((Component _ _ _ _ decls):comps,vhdlState') <- genNetlist (Just vhdlState)
                                                             (HashMap.fromList $ newBndr : concat signalTls)
                                                             primMap
                                                             mStart
                                                             (fst $ head signalHds)

  let sigVs = catMaybes $ map (\d -> case d of
                                NetDecl i _ _ -> Just i
                                _             -> Nothing
                              )
                              decls

  return (decls,sigVs,comps,vhdlState')
