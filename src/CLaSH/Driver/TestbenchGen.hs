{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fcontext-stack=21 #-}

-- | Generate a VHDL testbench for a component given a set of stimuli and a
-- set of matching expected outputs
module CLaSH.Driver.TestbenchGen
  ( genTestBench )
where

import           Control.Concurrent.Supply        (Supply)
import           Control.Error                    (EitherT, eitherT,
                                                   hoistEither, left, note,
                                                   right)
import           Control.Monad.Trans.Class        (lift)
import           Data.Either                      (lefts)
import           Data.HashMap.Lazy                (HashMap)
import qualified Data.HashMap.Lazy                as HashMap
import           Data.List                        (intersperse)
import           Data.Maybe                       (mapMaybe)
import           Data.Text.Lazy                   (Text)
import qualified Data.Text.Lazy.Builder           as Builder
import qualified Data.Text.Lazy.Builder.RealFloat as Builder
import           Text.PrettyPrint.Leijen.Text     ((<+>), (<>))
import qualified Text.PrettyPrint.Leijen.Text     as PP
import           Unbound.LocallyNameless          (bind, makeName, name2Integer,
                                                   name2String, rec, unrec)
import           Unbound.LocallyNameless.Ops      (unsafeUnbind)

import           CLaSH.Core.DataCon
import           CLaSH.Core.Pretty
import           CLaSH.Core.Term
import           CLaSH.Core.TyCon
import           CLaSH.Core.Type
import           CLaSH.Core.Util

import           CLaSH.Netlist
import           CLaSH.Netlist.Types              as N
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
             -> (Type -> Maybe (Either String HWType))
             -> VHDLState
             -> HashMap TmName (Type,Term)   -- ^ Global binders
             -> Maybe TmName                 -- ^ Stimuli
             -> Maybe TmName                 -- ^ Expected output
             -> Component                    -- ^ Component to generate TB for
             -> IO ([Component],VHDLState)
genTestBench dbgLvl supply primMap typeTrans vhdlState globals stimuliNmM expectedNmM
  (Component cName [(clkName,Clock rate),(rstName,Reset reset)] [inp] outp _)
  = eitherT error return $ do
  let rateF  = fromIntegral rate :: Float
      resetF = fromIntegral reset :: Float
      emptyStimuli = right ([],[],vhdlState,0)
  (inpDecls,inpComps,vhdlState',inpCnt) <- flip (maybe emptyStimuli) stimuliNmM $ \stimuliNm -> do
    (decls,sigVs,comps,vhdlState') <- prepareSignals vhdlState primMap globals
                                        typeTrans normalizeSignal Nothing
                                        stimuliNm

    let sigAs     = zipWith delayedSignal sigVs
                      (0.0:iterate (+rateF) (0.6 * rateF))
        sigAs'    = BlackBoxE ( PP.displayT . PP.renderPretty 0.4 80 . PP.vsep
                              $ PP.punctuate PP.comma sigAs ) Nothing
        inpAssign = Assignment (fst inp) sigAs'

    return (inpAssign:decls,comps,vhdlState',length sigVs)

  let emptyExpected = right ([],[],vhdlState',0)
  (expDecls,expComps,vhdlState'',expCnt) <- flip (maybe emptyExpected) expectedNmM $ \expectedNm -> do
    (decls,sigVs,comps,vhdlState'') <- prepareSignals vhdlState' primMap globals typeTrans normalizeSignal (Just inpCnt) expectedNm
    let asserts  = map (genAssert (fst outp)) sigVs
        procDecl = PP.vsep
                   [ "process is"
                   , "begin"
                   , PP.indent 2 ( PP.vsep $
                                   map (<> PP.semi) $
                                   concat [ ["wait for" <+> renderFloat2Dec (rateF * 0.4) <+> "ns" ]
                                            , intersperse ("wait for" <+> renderFloat2Dec rateF <+> "ns") asserts
                                            , ["wait"]
                                            ]
                                 )
                   , "end process" <> PP.semi
                   ]
        procDecl' = BlackBoxD (PP.displayT $ PP.renderPretty 0.4 80 procDecl)
    return (procDecl':decls,comps,vhdlState'',length sigVs)

  let finExpr = "'1' after" <+> renderFloat2Dec (rateF * (fromIntegral (max inpCnt expCnt) - 0.5)) <+> "ns"
      finDecl = [ NetDecl "finished" Bit (Just (N.Literal Nothing (BitLit L)))
                , Assignment "finished" (BlackBoxE (PP.displayT $ PP.renderCompact finExpr) Nothing)
                , Assignment "done" (Identifier "finished" Nothing)
                ]

      clkExpr = "not" <+> PP.text clkName <+> "after" <+> renderFloat2Dec (rateF * 0.5) <+> "ns when finished = '0'"
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
      ioDecl  = [ uncurry NetDecl inp  Nothing
                , uncurry NetDecl outp Nothing
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
    normalizeSignal :: (HashMap TmName (Type,Term)
                    -> TmName
                    -> [(TmName,(Type,Term))])
    normalizeSignal glbls bndr =
      runNormalization dbgLvl supply glbls typeTrans (normalize [bndr] >>= cleanupGraph [bndr])

genTestBench _ _ _ _ v _ _ _ c = traceIf True ("Can't make testbench for: " ++ show c) $ return ([],v)

delayedSignal :: Text
              -> Float
              -> PP.Doc
delayedSignal s t =
  PP.hsep
    [ PP.text s
    , "after"
    , renderFloat2Dec t
    , "ns"
    ]

renderFloat2Dec :: Float -> PP.Doc
renderFloat2Dec = PP.text . Builder.toLazyText . Builder.formatRealFloat Builder.Fixed (Just 2)

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

prepareSignals :: VHDLState
               -> PrimMap
               -> HashMap TmName (Type,Term)
               -> (Type -> Maybe (Either String HWType))
               -> ( HashMap TmName (Type,Term)
                    -> TmName
                    -> [(TmName,(Type,Term))])
               -> Maybe Int
               -> TmName
               -> EitherT String IO
                    ([Declaration],[Identifier],[Component],VHDLState)
prepareSignals vhdlState primMap globals typeTrans normalizeSignal mStart signalNm = do
  let signalS = name2String signalNm
  (signalTy,signalTm) <- hoistEither $ note ($(curLoc) ++ "Unable to find: " ++ signalS)
                                            (HashMap.lookup signalNm globals)
  signalList          <- termToList signalTm
  elemTy              <- stimuliElemTy signalTy

  let signalK  = name2Integer signalNm
      elemNms  = map (\i -> makeName (signalS ++ show i) signalK) [(0::Int)..]
      elemBnds = zipWith (\nm e -> (nm,(elemTy,e))) elemNms signalList
      signalList_normalized = map (normalizeSignal (HashMap.fromList elemBnds `HashMap.union` globals)
                                  . fst
                                  ) elemBnds

  lift $ createSignal vhdlState primMap typeTrans mStart signalList_normalized

termToList :: Monad m => Term -> EitherT String m [Term]
termToList e = case second lefts $ collectArgs e of
  (Data dc,[])
    | name2String (dcName dc) == "[]" -> pure []
    | name2String (dcName dc) == "Prelude.List.Nil" -> pure []
    | otherwise                                 -> errNoConstruct $(curLoc)
  (Data dc,[hdArg,tlArg])
    | name2String (dcName dc) == ":"  -> (hdArg:) <$> termToList tlArg
    | name2String (dcName dc) == "Prelude.List.::"  -> (hdArg:) <$> termToList tlArg
    | otherwise                                 -> errNoConstruct $(curLoc)
  _ -> errNoConstruct $(curLoc)
  where
    errNoConstruct l = left $ l ++ "Can't deconstruct list literal: " ++ show (second lefts $ collectArgs e)

stimuliElemTy :: Monad m => Type -> EitherT String m Type
stimuliElemTy ty = case splitTyConAppM ty of
  (Just (tc,[arg]))
    | name2String (tyConName tc) == "GHC.Types.[]" -> return arg
    | name2String (tyConName tc) == "Prelude.List.List" -> return arg
    | otherwise -> left $ $(curLoc) ++ "Not a List TyCon: " ++ showDoc ty
  _ -> left $ $(curLoc) ++ "Not a List TyCon: " ++ showDoc ty

createSignal :: VHDLState
             -> PrimMap
             -> (Type -> Maybe (Either String HWType))
             -> Maybe Int
             -> [[(TmName,(Type,Term))]]
             -> IO ([Declaration],[Identifier],[Component],VHDLState)
createSignal vhdlState primMap typeTrans mStart normalizedSignals = do
  let (signalHds,signalTls) = unzip $ map (\(l:ls) -> (l,ls)) normalizedSignals
      sigEs                 = map (\(_,(_,Letrec b)) -> unrec . fst $ unsafeUnbind b
                                  ) signalHds
      newExpr               = Letrec $ bind (rec $ concat sigEs)
                                            (Var (fst . snd $ head signalHds)
                                                 (fst $ head signalHds))
      newBndr               = (fst $ head signalHds, (fst . snd $ head signalHds, newExpr))

  (Component _ _ _ _ decls:comps,vhdlState') <- genNetlist (Just vhdlState)
                                                             (HashMap.fromList $ newBndr : concat signalTls)
                                                             primMap
                                                             typeTrans
                                                             mStart
                                                             (fst $ head signalHds)

  let sigVs = mapMaybe (\d -> case d of
                                NetDecl i _ _ -> Just i
                                _             -> Nothing
                       )
                       decls

  return (decls,sigVs,comps,vhdlState')
