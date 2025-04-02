{-|
Copyright   :  (C) 2019, Myrtle Software Ltd,
                   2021, QBayLogic B.V.
                   2022, Google Inc
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Template Haskell utilities for "Clash.Core.TermLiteral".
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.TermLiteral.TH
  ( deriveTermToData
  , deriveShowsTypePrec
  , deriveTermLiteral
     -- Stop exporting @dcName'@  once `ghcide` stops type-checking expanded
     -- TH splices
  ,  dcName'
  ) where

import           Data.Either
import qualified Data.Text                       as Text
import           Data.List                       (intersperse)
import qualified Data.List.NonEmpty              as NE
import           Data.Proxy
import           Data.Maybe                      (isNothing)
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
  mkName "termToData#"

showsTypePrecName :: Name
showsTypePrecName =
  -- Note that we can't use a fully qualified name here: GHC disallows fully
  -- qualified names in instance function declarations.
  mkName "showsTypePrec"

termLiteralName :: Name
termLiteralName = mkName "Clash.Core.TermLiteral.TermLiteral"

-- | Extracts variable names from a 'TyVarBndr'.
typeVarName :: CompatTyVarBndr -> Q (Name, Maybe Type)
typeVarName = \case
#if __GLASGOW_HASKELL__ >= 900
  PlainTV typVarName ()        -> pure (typVarName, Nothing)
  KindedTV typVarName () StarT -> pure (typVarName, Nothing)
  KindedTV typVarName () kind  -> pure (typVarName, Just kind)
#else
  PlainTV typVarName        -> pure (typVarName, Nothing)
  KindedTV typVarName StarT -> pure (typVarName, Nothing)
  KindedTV typVarName kind  -> pure (typVarName, Just kind)
#endif

-- | Derive a t'Clash.Core.TermLiteral.TermLiteral' instance for given type
deriveTermLiteral :: Name -> Q [Dec]
deriveTermLiteral typName = do
  TyConI (DataD _ _ typeVars _ _ _) <- reify typName
#if MIN_VERSION_template_haskell(2,21,0)
  typeVarNames <- mapM (typeVarName . fmap (const ())) typeVars
#else
  typeVarNames <- mapM typeVarName typeVars
#endif
  showsTypePrec <- deriveShowsTypePrec typName
  termToDataBody <- deriveTermToData typName
  let
    termToData = FunD termToDataName [Clause [] (NormalB termToDataBody) []]
    innerInstanceType = foldl AppT (ConT typName) (map (VarT . fst) typeVarNames)
    instanceType = ConT termLiteralName `AppT` innerInstanceType
    constraint typVarName = [t| $(conT termLiteralName) $(varT typVarName) |]
  constraints <- mapM (constraint . fst) (filter (isNothing . snd) typeVarNames)
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
deriveShowsTypePrec :: Name -> Q Dec
deriveShowsTypePrec typName = do
  TyConI (DataD _ _ typeVars _ _ _) <- reify typName
#if MIN_VERSION_template_haskell(2,21,0)
  typeVarNames <- mapM (typeVarName . fmap (const ())) typeVars
#else
  typeVarNames <- mapM typeVarName typeVars
#endif
  showTypeBody <- mkShowTypeBody typeVarNames
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
  mkTypePrecCall = \case
    (typVarName, Nothing) ->
      [| $(varE showsTypePrecName) 11 (Proxy @($(varT typVarName))) |]
    (_, Just _) ->
      -- XXX: Not sure how to deal with non-Type type variables so we do the dumb
      --      thing and insert an underscore.
      [| showString "_" |]

  -- Constructs:
  --
  -- > showString "Maybe" . showChar ' ' . showsTypePrec 11 (Proxy @a)
  --
  -- This is wrapped in an if-statement wrapping the result in parentheses if the
  -- incoming prec is more than 10 (function application).
  --
  mkShowTypeBody :: [(Name, Maybe Type)] -> Q Exp
  mkShowTypeBody typeVarNames =
    case typeVarNames of
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
  toConstr' (RecC cName fields) = (cName, length fields)
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
  args = zipWith (\n nm -> ValD (VarP nm) (NormalB (arg (toInteger n))) []) [0..nArgs-1] (NE.toList argNames)
  arg n = UInfixE (VarE argsName) (VarE '(!!)) (LitE (IntegerL n))

  -- case nm of {"ConstrOne" -> ConstOne <$> termToData# arg0; "ConstrTwo" -> ...}
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
      (VarE termToDataName `AppE` VarE (NE.head argNames))
  mkCall cName nFields =
    foldl
      (\e aName ->
        UInfixE
          e
          (VarE '(<*>))
          (VarE termToDataName `AppE` VarE aName))
      (mkCall cName 1)
      (take (nFields-1) (NE.tail argNames))

  -- term@(collectArgs -> (Data (dcName' -> nm), args))
  pat :: Pat
  pat =
    AsP
      termName
      (ViewP
        (VarE 'collectArgs)
#if MIN_VERSION_template_haskell(2,18,0)
        (TupP [ ConP 'Data [] [ViewP (VarE 'dcName') (VarP nameName)]
#else
        (TupP [ ConP 'Data [ViewP (VarE 'dcName') (VarP nameName)]
#endif
              , ViewP
                 (VarE 'lefts)
                 (if nArgs == 0 then WildP else VarP argsName)]))

  termName = mkName "term"
  argsName = mkName "args"
  argNames = fmap (mkName . ("arg" <>) . show) (NE.iterate (+1) (0 :: Word))
  nameName = mkName "nm"
