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
import           Control.Monad                    (forM)
import           Control.Monad.State              (State,runState)
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
import           Text.PrettyPrint.Leijen.Text.Monadic ()
import qualified Text.PrettyPrint.Leijen.Text.Monadic as PPM
import           Unbound.LocallyNameless          (bind, makeName, name2Integer,
                                                   name2String, rec, runFreshM,
                                                   unbind, unrec)

import           CLaSH.Core.DataCon
import           CLaSH.Core.Pretty
import           CLaSH.Core.Term
import           CLaSH.Core.TyCon
import           CLaSH.Core.Type
import           CLaSH.Core.Util

import           CLaSH.Netlist
import           CLaSH.Netlist.Types              as N
import           CLaSH.Netlist.Util               (typeSize)
import           CLaSH.Netlist.VHDL               (vhdlType,vhdlTypeMark)
import           CLaSH.Normalize                  (cleanupGraph, normalize,
                                                   runNormalization)
import           CLaSH.Primitives.Types
import           CLaSH.Rewrite.Types
import           CLaSH.Rewrite.Util               (substituteBinders)

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
             -> HashMap TmName (Type,Term)   -- ^ Global binders
             -> Maybe TmName                 -- ^ Stimuli
             -> Maybe TmName                 -- ^ Expected output
             -> Component                    -- ^ Component to generate TB for
             -> IO ([Component],VHDLState)
genTestBench dbgLvl supply primMap typeTrans tcm eval vhdlState globals stimuliNmM expectedNmM
  (Component cName [(clkName,Clock rate),(rstName,Reset reset)] [inp] outp _)
  = eitherT error return $ do
  let rateF  = fromIntegral rate :: Float
      resetF = fromIntegral reset :: Float
      emptyStimuli = right ([],[],vhdlState,0)
  (inpDecls,inpComps,vhdlState',inpCnt) <- flip (maybe emptyStimuli) stimuliNmM $ \stimuliNm -> do
    (decls,sigVs,comps,vhdlState') <- prepareSignals vhdlState primMap globals
                                        typeTrans tcm normalizeSignal Nothing
                                        stimuliNm

    let sigAs     = zipWith delayedSignal sigVs
                      (0.0:iterate (+rateF) (0.6 * rateF))
        sigAs'    = BlackBoxE ( PP.displayT . PP.renderPretty 0.4 80 . PP.vsep
                              $ PP.punctuate PP.comma sigAs ) Nothing
        inpAssign = Assignment (fst inp) sigAs'

    return (inpAssign:decls,comps,vhdlState',length sigVs)

  let emptyExpected = right ([],[],vhdlState',0)
  (expDecls,expComps,vhdlState'',expCnt) <- flip (maybe emptyExpected) expectedNmM $ \expectedNm -> do
    (decls,sigVs,comps,vhdlState'') <- prepareSignals vhdlState' primMap globals typeTrans tcm normalizeSignal (Just inpCnt) expectedNm
    let asserts  = map (genAssert (fst outp)) sigVs
        (toStrDecls,vhdlState3) = runState (mkToStringDecls (snd outp)) vhdlState''
        procDecl = PP.vsep
                   [ "process is"
                   , PP.indent 2 toStrDecls
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
    return (procDecl':decls,comps,vhdlState3,length sigVs)

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
    normalizeSignal :: HashMap TmName (Type,Term)
                    -> TmName
                    -> HashMap TmName (Type,Term)
    normalizeSignal glbls bndr =
      runNormalization dbgLvl supply glbls typeTrans tcm eval (normalize [bndr] >>= cleanupGraph bndr)

genTestBench _ _ _ _ _ _ v _ _ _ c = traceIf True ("Can't make testbench for: " ++ show c) $ return ([],v)

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

mkToStringDecls :: HWType -> State VHDLState PP.Doc
mkToStringDecls t@(Product _ elTys) =
    PPM.vcat (mapM mkToStringDecls elTys) PPM.<$>
    "function to_string" PPM.<+> PPM.parens ("value :" PPM.<+> vhdlType t) PPM.<+> "return STRING is" PPM.<$>
    "begin" PPM.<$>
    PPM.indent 2 ("return" PPM.<+> PPM.parens (PPM.hcat (PPM.punctuate " & " elTyPrint)) PPM.<> PPM.semi) PPM.<$>
    "end function to_string;"
  where
    elTyPrint = forM [0..(length elTys - 1)]
                     (\i -> "to_string" PPM.<>
                            PPM.parens ("value." PPM.<> vhdlType t PPM.<> "_sel" PPM.<> PPM.int i))
mkToStringDecls (Vector _ Bit)  = PPM.empty
mkToStringDecls t@(Vector _ elTy) =
  mkToStringDecls elTy PPM.<$>
  "function to_string" PPM.<+> PPM.parens ("value : " PPM.<+> vhdlTypeMark t) PPM.<+> "return STRING is" PPM.<$>
    PPM.indent 2
      ( "alias ivalue    : " PPM.<+> vhdlTypeMark t PPM.<> "(1 to value'length) is value;" PPM.<$>
        "variable result : STRING" PPM.<> PPM.parens ("1 to value'length * " PPM.<> PPM.int (typeSize elTy)) PPM.<> PPM.semi
      ) PPM.<$>
  "begin" PPM.<$>
    PPM.indent 2
      ("for i in ivalue'range loop" PPM.<$>
          PPM.indent 2
            (  "result" PPM.<> PPM.parens (PPM.parens ("(i - 1) * " PPM.<> PPM.int (typeSize elTy)) PPM.<+> "+ 1" PPM.<+>
                                           "to i*" PPM.<> PPM.int (typeSize elTy)) PPM.<+>
                        ":= to_string" PPM.<> PPM.parens (if elTy == Bool then "toSLV(ivalue(i))" else "ivalue(i)") PPM.<> PPM.semi
            ) PPM.<$>
       "end loop;" PPM.<$>
       "return result;"
      ) PPM.<$>
  "end function to_string;"
mkToStringDecls _ = PPM.empty

prepareSignals :: VHDLState
               -> PrimMap
               -> HashMap TmName (Type,Term)
               -> (HashMap TyConName TyCon -> Type -> Maybe (Either String HWType))
               -> HashMap TyConName TyCon
               -> ( HashMap TmName (Type,Term)
                    -> TmName
                    -> HashMap TmName (Type,Term) )
               -> Maybe Int
               -> TmName
               -> EitherT String IO
                    ([Declaration],[Identifier],[Component],VHDLState)
prepareSignals vhdlState primMap globals typeTrans tcm normalizeSignal mStart signalNm = do
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

  lift $ createSignal vhdlState primMap typeTrans tcm mStart signalList_normalized

termToList :: Monad m => Term -> EitherT String m [Term]
termToList e = case second lefts $ collectArgs e of
  (Data dc,[])
    | name2String (dcName dc) == "GHC.Types.[]"     -> pure []
    | name2String (dcName dc) == "Prelude.List.Nil" -> pure []
    | otherwise                                     -> errNoConstruct $(curLoc)
  (Data dc,[hdArg,tlArg])
    | name2String (dcName dc) == "GHC.Types.:"     -> (hdArg:) <$> termToList tlArg
    | name2String (dcName dc) == "Prelude.List.::" -> (hdArg:) <$> termToList tlArg
    | otherwise                                    -> errNoConstruct $(curLoc)
  (Letrec b,[]) -> case (runFreshM $ unbind b) of
    (bndrs,body) -> case substituteBinders (unrec bndrs) [] body of
                      ([],bodyS) -> termToList bodyS
                      _          -> errNoConstruct $(curLoc)
  _ -> errNoConstruct $(curLoc)
  where
    errNoConstruct l = left $ l ++ "Can't deconstruct list literal: " ++ show (second lefts $ collectArgs e)

stimuliElemTy :: Monad m => Type -> EitherT String m Type
stimuliElemTy ty = case splitTyConAppM ty of
  (Just (tc,[arg]))
    | name2String tc == "GHC.Types.[]" -> return arg
    | name2String tc == "Prelude.List.List" -> return arg
    | otherwise -> left $ $(curLoc) ++ "Not a List TyCon: " ++ showDoc ty
  _ -> left $ $(curLoc) ++ "Not a List TyCon: " ++ showDoc ty

createSignal :: VHDLState
             -> PrimMap
             -> (HashMap TyConName TyCon -> Type -> Maybe (Either String HWType))
             -> HashMap TyConName TyCon
             -> Maybe Int
             -> [HashMap TmName (Type,Term)]
             -> IO ([Declaration],[Identifier],[Component],VHDLState)
createSignal vhdlState primMap typeTrans tcm mStart normalizedSignals = do
  let (signalHds,signalTls) = unzip $ map ((\(l:ls) -> (l,ls)) . HashMap.toList) normalizedSignals
      sigEs                 = runFreshM $ mapM (\(_,(_,Letrec b)) -> (unrec . fst) <$> unbind b) signalHds
      newExpr               = Letrec $ bind (rec $ concat sigEs)
                                            (Var (fst . snd $ head signalHds)
                                                 (fst $ head signalHds))
      newBndr               = (fst $ head signalHds, (fst . snd $ head signalHds, newExpr))

  (Component _ _ _ _ decls:comps,vhdlState') <- genNetlist (Just vhdlState)
                                                             (HashMap.fromList $ newBndr : concat signalTls)
                                                             primMap
                                                             tcm
                                                             typeTrans
                                                             mStart
                                                             (fst $ head signalHds)

  let sigVs = mapMaybe (\d -> case d of
                                NetDecl i _ _ -> Just i
                                _             -> Nothing
                       )
                       decls

  return (decls,sigVs,comps,vhdlState')
