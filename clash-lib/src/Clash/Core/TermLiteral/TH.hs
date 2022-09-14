{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.TermLiteral.TH
  ( deriveTermToData
  , deriveshowsTypePrec
  , deriveTermLiteral
     -- Stop exporting @dcName'@  once `ghcide` stops type-checking expanded
     -- TH splices
  ,  dcName'
  ) where

import           Data.Either
import qualified Data.Text                       as Text
import           Data.List                       (intersperse)
import           Data.Proxy
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Lib         hiding (match)

import           Clash.Core.DataCon
import           Clash.Core.Term                 (collectArgs, Term(Data))
import           Clash.Core.Name                 (nameOcc)

-- Workaround for a strange GHC bug, where it complains about Subst only
-- existing as a boot file:
--
-- module Clash.Core.Subst cannot be linked; it is only available as a boot module
import Clash.Core.Subst ()

#if __GLASGOW_HASKELL__ >= 900
type CompatTyVarBndr = TyVarBndr ()
#else
type CompatTyVarBndr = TyVarBndr
#endif

dcName' :: DataCon -> String
dcName' = Text.unpack . nameOcc . dcName

termToDataName :: Name
termToDataName =
  -- Note that we can't use a fully qualified name here: GHC disallows fully
  -- qualified names in instance function declarations.
  mkName "termToData"

showsTypePrecName :: Name
showsTypePrecName =
  -- Note that we can't use a fully qualified name here: GHC disallows fully
  -- qualified names in instance function declarations.
  mkName "showsTypePrec"

termLiteralName :: Name
termLiteralName = mkName "Clash.Core.TermLiteral.TermLiteral"

-- | Extracts variable names from a 'TyVarBndr', errors if it is not a simply
-- typed variable name.
typeVarName :: CompatTyVarBndr -> Q Name
typeVarName = \case
#if __GLASGOW_HASKELL__ >= 900
  PlainTV typVarName () -> pure typVarName
  KindedTV typVarName () StarT -> pure typVarName
#else
  PlainTV typVarName -> pure typVarName
  KindedTV typVarName StarT -> pure typVarName
#endif
  k@(KindedTV {}) -> fail $ "Not supported: KindedTV: " <> show k

-- | Derive a t'Clash.Core.TermLiteral.TermLiteral' instance for given type
deriveTermLiteral :: Name -> Q [Dec]
deriveTermLiteral typName = do
  TyConI (DataD _ _ typeVars _ _ _) <- reify typName
  typeVarNames <- mapM typeVarName typeVars
  showsTypePrec <- deriveshowsTypePrec typName typeVarNames
  termToDataBody <- deriveTermToData typName
  let
    termToData = FunD termToDataName [Clause [] (NormalB termToDataBody) []]
    innerInstanceType = foldl AppT (ConT typName) (map VarT typeVarNames)
    instanceType = ConT termLiteralName `AppT` innerInstanceType
    constraint typVarName = [t| $(conT termLiteralName) $(varT typVarName) |]
  constraints <- mapM constraint typeVarNames
  pure $ [InstanceD Nothing constraints instanceType [showsTypePrec, termToData]]

-- | For 'Maybe', constructs:
--
-- > showsTypePrec n _
-- >   = let
-- >       showSpace = showChar ' '
-- >       precCalls = [showsTypePrec 11 (Proxy @a)]
-- >       interspersedPrecCalls = intersperse showSpace precCalls
-- >       showType = foldl (.) (showString "Maybe") (showSpace : interspersedPrecCalls)
-- >     in
-- >       showParen (n > 10) showType
--
deriveshowsTypePrec :: Name -> [Name] -> Q Dec
deriveshowsTypePrec typName typeVarNames = do
  TyConI (DataD _ _ typeVars _ _ _) <- reify typName
  showTypeBody <- mkShowTypeBody typeVars
  pure (FunD showsTypePrecName [Clause [VarP nName, WildP] (NormalB showTypeBody) []])
 where
  showTypeName = [| showString $(litE (StringL (nameBase typName))) |]

  -- Constructs:
  --
  -- > showsTypePrec 11 (Proxy @a)
  --
  -- where the 'a' is given as an argument. The surrounding operator precedence
  -- is set to indicate "function" application. I.e., it instructs the call to
  -- wrap the type string in parentheses.
  --
  mkTypePrecCall typVarName =
    [| $(varE showsTypePrecName) 11 (Proxy @($(varT typVarName))) |]

  -- Constructs:
  --
  -- > showString "Maybe" . showChar ' ' . showsTypePrec 11 (Proxy @a)
  --
  -- This is wrapped in an if-statement wrapping the result in parentheses if the
  -- incoming prec is more than 10 (function application).
  --
  mkShowTypeBody :: [CompatTyVarBndr] -> Q Exp
  mkShowTypeBody typeVars =
    case typeVars of
      [] ->
        -- We seq on `n` here to prevent _unused variable_ warnings. This is a
        -- bit of a hack (the real solution would be to selectively pattern
        -- match).
        [| $(varE nName) `seq` $(showTypeName) |]
      _  -> [|
        let
          showSpace = showChar ' '
          precCalls = $(listE (map mkTypePrecCall typeVarNames))
          interspersedPrecCalls = intersperse showSpace precCalls
          showType = foldl (.) $(showTypeName) (showSpace : interspersedPrecCalls)
        in
          showParen ($(varE nName) > 10) showType
       |]

  nName = mkName "n"

deriveTermToData :: Name -> Q Exp
deriveTermToData typName = do
  TyConI (DataD _ _ _ _ constrs _) <- reify typName
  pure (deriveTermToData1 (map toConstr' constrs))
 where
  toConstr' (NormalC cName fields) = (cName, length fields)
  toConstr' c = error $ "Unexpected constructor: " ++ show c

deriveTermToData1 :: [(Name, Int)] -> Exp
deriveTermToData1 constrs =
  LamCaseE
    [ Match pat (NormalB (if null args then theCase else LetE args theCase)) []
    , Match (VarP termName) (NormalB ((ConE 'Left `AppE` VarE termName))) []

    ]
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
