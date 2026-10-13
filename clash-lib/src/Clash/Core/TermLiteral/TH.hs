{-|
Copyright   :  (C) 2019, Myrtle Software Ltd,
                   2021, QBayLogic B.V.
                   2022, Google Inc
                   2026, QBayLogic B.V.
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
  , deriveLiftTermToData
  , deriveTermLiteral1
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
import           Clash.Core.Name                 (nameOcc)
import           Clash.Core.Syntax        (Term(Data))
import           Clash.Core.Term          (collectArgs)

type CompatTyVarBndr = TyVarBndr ()

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
  PlainTV typVarName ()        -> pure (typVarName, Nothing)
  KindedTV typVarName () StarT -> pure (typVarName, Nothing)
  KindedTV typVarName () kind  -> pure (typVarName, Just kind)

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
  pure (deriveTermToData1 (map (fmap (map (const (VarE termToDataName))) . conFields) constrs))

-- | Derive the 'Clash.Core.TermLiteral.liftTermToData#' of a type
-- constructor @T@ whose last type parameter @a@ has kind @Type@: an expression
-- of type
--
-- > (Term n -> Either (Term n) a) -> Term n -> Either (Term n) (T .. a)
--
-- It decodes fields of type @a@ with the given decoder, fields of type @F a@,
-- for some @F@ not mentioning @a@, with @liftTermToData# decoder@ (so @F@,
-- which may be @T ..@ itself, must have a
-- t'Clash.Core.TermLiteral.TermLiteral1' instance), and fields not
-- mentioning @a@ with @termToData#@.
deriveLiftTermToData :: Name -> Q Exp
deriveLiftTermToData typName = do
  TyConI (DataD _ _ typeVars _ constrs _) <- reify typName
  a <- case reverse typeVars of
    tv : _ -> fst <$> typeVarName (() <$ tv)
    [] -> fail ("deriveLiftTermToData: " <> show typName <> " has no type parameters")
  leafName <- newName "leaf"
  let
    fieldDecoder ty
      | ty == VarT a = pure (VarE leafName)
      | not (a `occursIn` ty) = pure (VarE termToDataName)
      | AppT f (VarT a') <- ty, a' == a, not (a `occursIn` f) =
          pure (VarE liftTermToDataName `AppE` VarE leafName)
      | otherwise =
          fail ("deriveLiftTermToData: unsupported field type " <> show ty)
  constrs1 <- mapM (traverse (mapM fieldDecoder) . conFields) constrs
  pure (LamE [VarP leafName] (deriveTermToData1 constrs1))
 where
  occursIn nm = \case
    VarT nm' -> nm == nm'
    AppT t1 t2 -> occursIn nm t1 || occursIn nm t2
    SigT t _ -> occursIn nm t
    ParensT t -> occursIn nm t
    _ -> False

-- | Derive a t'Clash.Core.TermLiteral.TermLiteral1' instance for a type
-- constructor whose last type parameter has kind @Type@, see
-- 'deriveLiftTermToData'. The other type parameters of kind @Type@ need
-- t'Clash.Core.TermLiteral.TermLiteral' instances.
deriveTermLiteral1 :: Name -> Q [Dec]
deriveTermLiteral1 typName = do
  TyConI (DataD _ _ typeVars _ _ _) <- reify typName
  body <- deriveLiftTermToData typName
  typeVarNames <- mapM (typeVarName . (() <$)) (init typeVars)
  let
    liftTermToData = FunD liftTermToDataName [Clause [] (NormalB body) []]
    innerInstanceType = foldl AppT (ConT typName) (map (VarT . fst) typeVarNames)
    instanceType = ConT termLiteral1Name `AppT` innerInstanceType
    constraint typVarName = [t| $(conT termLiteralName) $(varT typVarName) |]
  constraints <- mapM (constraint . fst) (filter (isNothing . snd) typeVarNames)
  pure [InstanceD Nothing constraints instanceType [liftTermToData]]

liftTermToDataName :: Name
liftTermToDataName = mkName "liftTermToData#"

termLiteral1Name :: Name
termLiteral1Name = mkName "Clash.Core.TermLiteral.TermLiteral1"

-- | The name and field types of a constructor
conFields :: Con -> (Name, [Type])
conFields = \case
  NormalC cName fields -> (cName, map snd fields)
  RecC cName fields -> (cName, map (\(_, _, ty) -> ty) fields)
  c -> error $ "Unexpected constructor: " ++ show c

-- | Given the constructors of a type, with the decoders of their fields,
-- constructs:
--
-- > \case
-- >   term@(collectArgs -> (Data (dcName' -> nm), lefts -> args)) ->
-- >     let arg0 = args !! 0; ... in
-- >     case nm of
-- >       "ConstrOne" -> ConstrOne <$> decoder0 arg0
-- >       "ConstrTwo" -> ...
-- >       _ -> Left term
-- >   term -> Left term
deriveTermToData1 :: [(Name, [Exp])] -> Exp
deriveTermToData1 constrs =
  LamCaseE
    [ Match pat (NormalB (if null args then theCase else LetE args theCase)) []
    , Match (VarP termName) (NormalB ((ConE 'Left `AppE` VarE termName))) []

    ]
 where
  nArgs = maximum (map (length . snd) constrs)

  args :: [Dec]
  args = zipWith (\n nm -> ValD (VarP nm) (NormalB (arg (toInteger n))) []) [0..nArgs-1] (NE.toList argNames)
  arg n = UInfixE (VarE argsName) (VarE '(!!)) (LitE (IntegerL n))

  theCase :: Exp
  theCase =
    CaseE
      (VarE nameName)
      (map match constrs ++ [emptyMatch])

  emptyMatch = Match WildP (NormalB (ConE 'Left `AppE` VarE termName)) []

  match :: (Name, [Exp]) -> Match
  match (cName, decoders) =
    Match (LitP (StringL (show cName))) (NormalB (mkCall cName decoders)) []

  mkCall :: Name -> [Exp] -> Exp
  mkCall cName [] = ConE 'Right `AppE` ConE cName
  mkCall cName (decoder0 : decoders) =
    foldl
      (\e (decoder, aName) ->
        UInfixE
          e
          (VarE '(<*>))
          (decoder `AppE` VarE aName))
      (UInfixE
        (ConE cName)
        (VarE '(<$>))
        (decoder0 `AppE` VarE (NE.head argNames)))
      (zip decoders (NE.tail argNames))

  -- term@(collectArgs -> (Data (dcName' -> nm), args))
  pat :: Pat
  pat =
    AsP
      termName
      (ViewP
        (VarE 'collectArgs)
        (TupP [ ConP 'Data [] [ViewP (VarE 'dcName') (VarP nameName)]
              , ViewP
                 (VarE 'lefts)
                 (if nArgs == 0 then WildP else VarP argsName)]))

  termName = mkName "term"
  argsName = mkName "args"
  argNames = fmap (mkName . ("arg" <>) . show) (NE.iterate (+1) (0 :: Word))
  nameName = mkName "nm"
