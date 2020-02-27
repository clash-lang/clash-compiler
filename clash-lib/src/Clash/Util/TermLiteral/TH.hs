{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns          #-}

module Clash.Util.TermLiteral.TH
  (  deriveTermToData
  ) where

import           Data.Either
import qualified Data.Text                       as Text
import           Language.Haskell.TH.Syntax

import           Clash.Core.DataCon
import           Clash.Core.Term                 (collectArgs, Term(Data))
import           Clash.Core.Name                 (nameOcc)

-- Workaround for a strange GHC bug, where it complains about Subst only
-- existing as a boot file:
--
-- module Clash.Core.Subst cannot be linked; it is only available as a boot module
import Clash.Core.Subst ()

dcName' :: DataCon -> String
dcName' = Text.unpack . nameOcc . dcName

termToDataName :: Name
termToDataName = mkName "Clash.Util.TermLiteral.termToData"

deriveTermToData :: Name -> Q Exp
deriveTermToData typName = do
  TyConI (DataD _ _ _ _ constrs _) <- reify typName
  pure (deriveTermToData1 (map toConstr' constrs))
 where
  toConstr' (NormalC cName fields) = (cName, length fields)
  toConstr' c = error $ "Unexpected constructor: " ++ show c

deriveTermToData1 :: [(Name, Int)] -> Exp
deriveTermToData1 constrs =
  LamE
    [pat]
    (if null args then theCase else LetE args theCase)
 where
  nArgs = maximum (map snd constrs)

  args :: [Dec]
  args = zipWith (\n nm -> ValD (VarP nm) (NormalB (arg n)) []) [0..] argNames
  arg n = UInfixE (VarE argsName) (VarE '(!!)) (LitE (IntegerL n))

  -- case nm of {"ConstrOne" -> ConstOne <$> termToData arg0; "ConstrTwo" -> ...}
  theCase :: Exp
  theCase =
    CaseE
      (VarE nameName)
      (map match constrs ++ [emptyMatch])

  emptyMatch = Match WildP (NormalB (ConE 'Left `AppE` VarE termName)) []

  match :: (Name, Int) -> Match
  match (cName, nFields) =
    Match (LitP (StringL (show cName))) (NormalB (mkCall cName nFields)) []

  mkCall :: Name -> Int -> Exp
  mkCall cName 0  = ConE 'Right `AppE` ConE cName
  mkCall cName 1 =
    UInfixE
      (ConE cName)
      (VarE '(<$>))
      (VarE termToDataName `AppE` VarE (head argNames))
  mkCall cName nFields =
    foldl
      (\e aName ->
        UInfixE
          e
          (VarE '(<*>))
          (VarE termToDataName `AppE` VarE aName))
      (mkCall cName 1)
      (take (nFields-1) (tail argNames))

  -- term@(collectArgs -> (Data (dcName' -> nm), args))
  pat :: Pat
  pat =
    AsP
      termName
      (ViewP
        (VarE 'collectArgs)
        (TupP [ ConP 'Data [ViewP (VarE 'dcName') (VarP nameName)]
              , ViewP
                 (VarE 'lefts)
                 (if nArgs == 0 then WildP else VarP argsName)]))

  termName = mkName "term"
  argsName = mkName "args"
  argNames = [mkName ("arg" ++ show n) | n <- [0..nArgs-1]]
  nameName = mkName "nm"

