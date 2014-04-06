{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fcontext-stack=21 #-}

-- | Generate a VHDL testbench for a component given a set of stimuli and a
-- set of matching expected outputs
module CLaSH.Driver.TestbenchGen
  ( genTestBench )
where

import           Control.Concurrent.Supply        (Supply)
import           Data.HashMap.Lazy                (HashMap)
import           Data.List                        (nub)
import           Data.Maybe                       (mapMaybe)
import qualified Data.Text.Lazy.Builder           as Builder
import qualified Data.Text.Lazy.Builder.RealFloat as Builder
import           Text.PrettyPrint.Leijen.Text     ((<+>), (<>))
import qualified Text.PrettyPrint.Leijen.Text     as PP

import           CLaSH.Core.Term
import           CLaSH.Core.TyCon
import           CLaSH.Core.Type

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
  (Component cName hidden [inp] outp _) = do
  (inpInst,inpComps,vhdlState',hidden') <- maybe (return (BlackBoxD "",[],vhdlState,hidden))
                                                 (genStimuli vhdlState primMap globals typeTrans tcm normalizeSignal hidden inp)
                                                 stimuliNmM
  (expInst,expComps,vhdlState'',hidden'') <- maybe (return (BlackBoxD "",[],vhdlState',hidden'))
                                                 (genVerifier vhdlState' primMap globals typeTrans tcm normalizeSignal hidden' outp)
                                                 expectedNmM
  let clkNms = mapMaybe (\hd -> case hd of (clkNm,Clock _) -> Just clkNm ; _ -> Nothing) hidden
      rstNms = mapMaybe (\hd -> case hd of (clkNm,Reset _) -> Just clkNm ; _ -> Nothing) hidden
      clks   = mapMaybe genClock hidden''
      rsts   = mapMaybe genReset hidden''

  let finDecl = NetDecl "finished" Bool (Just (N.Literal Nothing (BoolLit False)))


      ioDecl  = [ uncurry NetDecl inp  Nothing
                , uncurry NetDecl outp Nothing
                ]

      instDecl = InstDecl cName "totest"
                   (map (\i -> (i,Identifier i Nothing))
                        (concat [ clkNms, rstNms, [fst inp], [fst outp] ])
                   )

      tbComp = Component "testbench" [] [] ("done",Bool)
                  (concat [ [finDecl]
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

genTestBench _ _ _ _ _ _ v _ _ _ c = traceIf True ("Can't make testbench for: " ++ show c) $ return ([],v)

genClock :: (Identifier,HWType)
         -> Maybe [Declaration]
genClock (clkName,Clock rate) = Just clkDecls
  where
    clkGenDecl = PP.vsep
                  [ "process is"
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
                  ]

    clkDecls = [ NetDecl clkName (Clock rate) (Just (N.Literal Nothing (BitLit L)))
               , BlackBoxD (PP.displayT $ PP.renderPretty 0.4 80 clkGenDecl)
               ]

genClock _ = Nothing

genReset :: (Identifier,HWType)
         -> Maybe [Declaration]
genReset (rstName,Reset _) = Just rstDecls
  where
    rstExpr = PP.vcat $ PP.punctuate PP.comma
                [ "'0' after 0 ns"
                , "'1' after 1 ns"
                ]

    rstDecls = [ NetDecl rstName Bit Nothing
               , Assignment rstName (BlackBoxE (PP.displayT $ PP.renderCompact rstExpr) Nothing)
               ]

genReset _ = Nothing

renderFloat2Dec :: Float -> PP.Doc
renderFloat2Dec = PP.text . Builder.toLazyText . Builder.formatRealFloat Builder.Fixed (Just 2)

genStimuli :: VHDLState
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
genStimuli vhdlState primMap globals typeTrans tcm normalizeSignal hidden inp signalNm = do
  let stimNormal = normalizeSignal globals signalNm
  (comps,vhdlState') <- genNetlist (Just vhdlState) stimNormal primMap tcm typeTrans Nothing signalNm
  let (Component cName hidden' [] (outp,_) _) = head comps
      hidden'' = nub (hidden ++ hidden')
      clkNms   = mapMaybe (\hd -> case hd of (clkNm,Clock _) -> Just clkNm ; _ -> Nothing) hidden'
      rstNms   = mapMaybe (\hd -> case hd of (clkNm,Reset _) -> Just clkNm ; _ -> Nothing) hidden'
      decl     = InstDecl cName "stimuli"
                   (map (\i -> (i,Identifier i Nothing))
                        (concat [ clkNms, rstNms ]) ++
                        [(outp,Identifier (fst inp) Nothing)]
                   )
  return (decl,comps,vhdlState',hidden'')

genVerifier :: VHDLState
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
genVerifier vhdlState primMap globals typeTrans tcm normalizeSignal hidden outp signalNm = do
  let stimNormal = normalizeSignal globals signalNm
  (comps,vhdlState') <- genNetlist (Just vhdlState) stimNormal primMap tcm typeTrans Nothing signalNm
  let (Component cName hidden' [(inp,_)] (fin,_) _) = head comps
      hidden'' = nub (hidden ++ hidden')
      clkNms   = mapMaybe (\hd -> case hd of (clkNm,Clock _) -> Just clkNm ; _ -> Nothing) hidden'
      rstNms   = mapMaybe (\hd -> case hd of (clkNm,Reset _) -> Just clkNm ; _ -> Nothing) hidden'
      decl     = InstDecl cName "verify"
                   (map (\i -> (i,Identifier i Nothing))
                        (concat [ clkNms, rstNms ]) ++
                        [(inp,Identifier (fst outp) Nothing),(fin,Identifier "finished" Nothing)]
                   )
  return (decl,comps,vhdlState',hidden'')

-- mkToStringDecls :: HWType -> State VHDLState PP.Doc
-- mkToStringDecls t@(Product _ elTys) =
--     PPM.vcat (mapM mkToStringDecls elTys) PPM.<$>
--     "function to_string" PPM.<+> PPM.parens ("value :" PPM.<+> vhdlType t) PPM.<+> "return STRING is" PPM.<$>
--     "begin" PPM.<$>
--     PPM.indent 2 ("return" PPM.<+> PPM.parens (PPM.hcat (PPM.punctuate " & " elTyPrint)) PPM.<> PPM.semi) PPM.<$>
--     "end function to_string;"
--   where
--     elTyPrint = forM [0..(length elTys - 1)]
--                      (\i -> "to_string" PPM.<>
--                             PPM.parens ("value." PPM.<> vhdlType t PPM.<> "_sel" PPM.<> PPM.int i))
-- mkToStringDecls (Vector _ Bit)  = PPM.empty
-- mkToStringDecls t@(Vector _ elTy) =
--   mkToStringDecls elTy PPM.<$>
--   "function to_string" PPM.<+> PPM.parens ("value : " PPM.<+> vhdlTypeMark t) PPM.<+> "return STRING is" PPM.<$>
--     PPM.indent 2
--       ( "alias ivalue    : " PPM.<+> vhdlTypeMark t PPM.<> "(1 to value'length) is value;" PPM.<$>
--         "variable result : STRING" PPM.<> PPM.parens ("1 to value'length * " PPM.<> PPM.int (typeSize elTy)) PPM.<> PPM.semi
--       ) PPM.<$>
--   "begin" PPM.<$>
--     PPM.indent 2
--       ("for i in ivalue'range loop" PPM.<$>
--           PPM.indent 2
--             (  "result" PPM.<> PPM.parens (PPM.parens ("(i - 1) * " PPM.<> PPM.int (typeSize elTy)) PPM.<+> "+ 1" PPM.<+>
--                                            "to i*" PPM.<> PPM.int (typeSize elTy)) PPM.<+>
--                         ":= to_string" PPM.<> PPM.parens (if elTy == Bool then "toSLV(ivalue(i))" else "ivalue(i)") PPM.<> PPM.semi
--             ) PPM.<$>
--        "end loop;" PPM.<$>
--        "return result;"
--       ) PPM.<$>
--   "end function to_string;"
-- mkToStringDecls _ = PPM.empty
