{-|
Copyright   :  (C) 2026, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Resolve signal portal markers in a hierarchical netlist by adding hidden ports
to components and instances.
-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Clash.Netlist.SignalPortal
  ( resolveSignalPortals
  ) where

import Control.Monad (foldM, forM, unless)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State.Strict (State, runState)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Class (lift)
import qualified Data.Map.Ordered as OMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT

import Clash.Netlist.BlackBox.Types as BB
  (Element(Text))
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types
import Clash.Unique (Unique)

type PortalName = Text
type PortalSources = Map PortalName PortalSource
type PortalSinks = Map PortalName [PortalSink]
type PortalPorts = Map PortalName PortalPort
type PortalTypes = Map PortalName HWType
type PortalDemands = Map PortalName PortalDemand
type ChildOutputWires = Map Int (Map PortalName ChildOutput)
type ProcessM = ExceptT String (State ProcessState)
type ScopeM = ExceptT String (State IdentifierSet)

data PortalKind = PortalSource | PortalSink
  deriving Eq

data PortalValue a = PortalValue
  { pvType :: HWType
  , pvValue :: a
  }

data PortalDemand = PortalDemand
  { pdType :: HWType
  , pdCount :: Int
  }

type PortalSource = PortalValue Expr
type PortalSink = PortalValue Identifier
type PortalPort = PortalValue Identifier

data PortalMarker = PortalMarker
  { pmKind :: PortalKind
  , pmName :: PortalName
  , pmType :: HWType
  , pmResult :: Identifier
  , pmSource :: Expr
  }

data LocalPortals = LocalPortals
  { lpDeclarations :: [Declaration]
  , lpSources :: PortalSources
  , lpSinks :: PortalSinks
  }

data ChildInstance = ChildInstance
  { ciIndex :: Int
  , ciComponent :: PortalComponent
  }

data ChildOutput = ChildOutput
  { coPort :: PortalPort
  , coWire :: Identifier
  }

data PortalComponent = PortalComponent
  { pcMeta :: ComponentMeta
  , pcComponent :: Component
  , pcProvides :: PortalPorts
  , pcRequires :: PortalPorts
  , pcDemands :: PortalDemands
  }

data ProcessState = ProcessState
  { psDone :: Map Text PortalComponent
  , psComponents :: Map Text (Unique, ComponentMeta, Component)
  }

portalSourcePrefix :: LT.Text
portalSourcePrefix = "__CLASH_SIGNAL_PORTAL_SOURCE__:"

portalSinkPrefix :: LT.Text
portalSinkPrefix = "__CLASH_SIGNAL_PORTAL_SINK__:"

resolveSignalPortals
  :: Component
  -> ComponentMap
  -> Either String (Component, ComponentMap, [String])
resolveSignalPortals topComponent componentMap = do
  let
    componentsByName =
      Map.fromList
        [ (Id.toText (componentName component), (uniq, meta, component))
        | (uniq, (meta, component)) <- OMap.assocs componentMap
        ]
    topName = Id.toText (componentName topComponent)
    initialState = ProcessState Map.empty componentsByName
    allLocalPortals =
      mapM
        (\(_uniq, _meta, component) ->
          collectLocalPortals (declarations component))
        componentsByName

  (globalDemands, unusedWarnings) <-
    case allLocalPortals of
      Left err -> Left err
      Right localPortals -> do
        globalDemands0 <-
          collectGlobalDemands componentsByName localPortals topName
        Right
          ( globalDemands0
          , unusedSourceWarnings globalDemands0 localPortals
          )

  psDone <-
    case runState (runExceptT (mapM_ (processByName globalDemands topName) (Map.keys componentsByName))) initialState of
      (Left err, _) -> Left err
      (Right (), ProcessState{psDone}) -> Right psDone

  topComponent1 <-
    case Map.lookup topName psDone of
      Just PortalComponent{pcComponent} -> Right pcComponent
      Nothing -> Right topComponent

  let
    componentMap1 =
      OMap.fromList
        [ (uniq, (pcMeta portalComponent, pcComponent portalComponent))
        | (uniq, (_meta, component)) <- OMap.assocs componentMap
        , let name = Id.toText (componentName component)
        , Just portalComponent <- [Map.lookup name psDone]
        ]

  pure (topComponent1, componentMap1, unusedWarnings)

processByName :: PortalDemands -> Text -> Text -> ProcessM PortalComponent
processByName globalDemands topName name = do
  done <- State.gets psDone
  case Map.lookup name done of
    Just component -> pure component
    Nothing -> do
      componentEntry <- State.gets (Map.lookup name . psComponents)
      case componentEntry of
        Nothing ->
          throwError ("Internal error: no component named " <> Text.unpack name)
        Just (_uniq, meta, component) -> do
          allComponents <- State.gets psComponents
          let childNames =
                filter (`Map.member` allComponents) $
                  mapMaybe instComponentName (declarations component)
          childInfos <-
            Map.fromList <$> forM childNames \childName ->
              (childName,) <$> processByName globalDemands topName childName

          portalComponent <-
            liftE $
              processComponent globalDemands (name == topName) childInfos meta component

          State.modify' \s ->
            s{psDone = Map.insert name portalComponent (psDone s)}

          pure portalComponent

processComponent
  :: PortalDemands
  -> Bool
  -> Map Text PortalComponent
  -> ComponentMeta
  -> Component
  -> Either String PortalComponent
processComponent globalDemands isTop childInfos meta0 component0 =
  runIdentifierM meta0 do
    local@LocalPortals{lpDeclarations} <-
      liftE $
        collectLocalPortals (declarations component0)

    childOutputs <-
      allocateChildOutputWires childInfos lpDeclarations

    sourcesHere <-
      liftE $
        collectSourcesHere local childOutputs

    requiredTypes <-
      liftE $
        collectRequiredTypes childInfos lpDeclarations local

    demandsHere <-
      liftE $
        collectDemandsHere childInfos lpDeclarations local

    liftE $
      checkRequiredTypes requiredTypes sourcesHere

    downwardPorts <-
      allocateDownwardPorts isTop requiredTypes sourcesHere

    sourcesAvailable <-
      liftE $
        mergeSources sourcesHere (portSources downwardPorts)

    upwardPorts <-
      allocateUpwardPorts
        isTop
        (demandsOutside globalDemands demandsHere)
        sourcesHere

    declsWithPortalPorts <-
      rewriteInstantiations childInfos childOutputs sourcesAvailable lpDeclarations

    let
      component1 =
        component0
          { inputs = inputs component0 ++ inputPorts downwardPorts
          , outputs = outputs component0 ++ outputPorts upwardPorts
          , declarations =
              childWireDecls childOutputs
                ++ declsWithPortalPorts
                ++ sinkAssignments sourcesAvailable (lpSinks local)
                ++ upwardAssignments sourcesHere upwardPorts
          }

    pure PortalComponent
      { pcMeta = meta0
      , pcComponent = component1
      , pcProvides = upwardPorts
      , pcRequires = downwardPorts
      , pcDemands = demandsHere
      }

runIdentifierM
  :: ComponentMeta
  -> ScopeM PortalComponent
  -> Either String PortalComponent
runIdentifierM meta0 action =
  case runState (runExceptT action) (cmScope meta0) of
    (Left err, _) -> Left err
    (Right component, scope1) ->
      Right component{pcMeta=(pcMeta component){cmScope=scope1}}

liftE :: Monad m => Either String a -> ExceptT String m a
liftE =
  either throwError pure

traverseWithKeyM
  :: (Monad m, Ord k)
  => (k -> a -> m b)
  -> Map k a
  -> m (Map k b)
traverseWithKeyM f =
  fmap Map.fromAscList . mapM (\(k, a) -> (k,) <$> f k a) . Map.toAscList

instComponentName :: Declaration -> Maybe Text
instComponentName = \case
  InstDecl _ _ _ componentName _ _ _ -> Just (Id.toText componentName)
  _ -> Nothing

collectGlobalDemands
  :: Map Text (Unique, ComponentMeta, Component)
  -> Map Text LocalPortals
  -> Text
  -> Either String PortalDemands
collectGlobalDemands componentsByName localPortals topName =
  go topName
 where
  go name =
    case Map.lookup name componentsByName of
      Nothing ->
        Left ("Internal error: no component named " <> Text.unpack name)
      Just (_uniq, _meta, component) -> do
        local <-
          case Map.lookup name localPortals of
            Just local0 -> Right local0
            Nothing ->
              Left ("Internal error: no portal summary for component " <> Text.unpack name)
        let
          childNames =
            filter (`Map.member` componentsByName) $
              mapMaybe instComponentName (declarations component)
        childDemands <- mapM go childNames
        foldM mergeDemands (localDemands local) childDemands

childInstances
  :: Map Text PortalComponent
  -> [Declaration]
  -> [ChildInstance]
childInstances childInfos =
  mapMaybe go . zip [(0 :: Int)..]
 where
  go (ix, InstDecl _ _ _ componentName _ _ _) =
    ChildInstance ix <$> Map.lookup (Id.toText componentName) childInfos
  go _ = Nothing

collectLocalPortals :: [Declaration] -> Either String LocalPortals
collectLocalPortals =
  fmap finish . foldM collect emptyLocalPortals
 where
  emptyLocalPortals =
    LocalPortals [] Map.empty Map.empty

  finish lp@LocalPortals{lpDeclarations} =
    lp{lpDeclarations = reverse lpDeclarations}

  collect lp@LocalPortals{..} decl =
    case portalDeclaration decl of
      Just PortalMarker{pmKind=PortalSource, pmName, pmType, pmResult, pmSource} -> do
        sources1 <- insertSource pmName (PortalValue pmType pmSource) lpSources
        Right lp
          { lpDeclarations = Assignment pmResult Cont pmSource : lpDeclarations
          , lpSources = sources1
          }

      Just PortalMarker{pmKind=PortalSink, pmName, pmType, pmResult} ->
        Right lp
          { lpSinks =
              Map.insertWith
                (++)
                pmName
                [PortalValue pmType pmResult]
                lpSinks
          }

      Nothing ->
        Right lp{lpDeclarations = decl : lpDeclarations}

portalDeclaration :: Declaration -> Maybe PortalMarker
portalDeclaration (BlackBoxD _ _ _ _ (BBTemplate [BB.Text marker]) bbCtx) = do
  (pmKind, pmName) <- portalMarker marker
  (resultExpr, pmType) <- singleResult bbCtx
  pmResult <- exprIdentifier resultExpr
  pmSource <-
    case pmKind of
      PortalSource -> findSourceInput pmType bbCtx
      PortalSink -> Just Noop
  pure PortalMarker{..}
portalDeclaration _ =
  Nothing

portalMarker :: LT.Text -> Maybe (PortalKind, PortalName)
portalMarker marker
  | Just label <- LT.stripPrefix portalSourcePrefix marker =
      Just (PortalSource, LT.toStrict label)
  | Just label <- LT.stripPrefix portalSinkPrefix marker =
      Just (PortalSink, LT.toStrict label)
  | otherwise =
      Nothing

singleResult :: BlackBoxContext -> Maybe (Expr, HWType)
singleResult Context{bbResults=[result]} = Just result
singleResult _ = Nothing

exprIdentifier :: Expr -> Maybe Identifier
exprIdentifier (Identifier identifier Nothing) = Just identifier
exprIdentifier _ = Nothing

findSourceInput :: HWType -> BlackBoxContext -> Maybe Expr
findSourceInput resultTy Context{bbInputs} =
  case [expr | (expr, ty, _isLit) <- reverse bbInputs, ty == resultTy] of
    expr : _ -> Just expr
    [] -> Nothing

allocateChildOutputWires
  :: Map Text PortalComponent
  -> [Declaration]
  -> ScopeM ChildOutputWires
allocateChildOutputWires childInfos decls =
  Map.fromList <$> mapM allocate (childInstances childInfos decls)
 where
  allocate ChildInstance{ciIndex, ciComponent} = do
    outputs <-
      traverseWithKeyM
        (\label port -> do
          wire <- freshPortalId label ("wire_" <> Text.pack (show ciIndex))
          pure ChildOutput{coPort=port, coWire=wire})
        (pcProvides ciComponent)
    pure (ciIndex, outputs)

collectSourcesHere
  :: LocalPortals
  -> ChildOutputWires
  -> Either String PortalSources
collectSourcesHere LocalPortals{lpSources} childOutputs =
  foldM mergeSources lpSources (childOutputSources <$> Map.elems childOutputs)

childOutputSources :: Map PortalName ChildOutput -> PortalSources
childOutputSources =
  Map.map \ChildOutput{coPort=PortalValue{pvType}, coWire} ->
    PortalValue pvType (Identifier coWire Nothing)

collectRequiredTypes
  :: Map Text PortalComponent
  -> [Declaration]
  -> LocalPortals
  -> Either String PortalTypes
collectRequiredTypes childInfos decls LocalPortals{lpSinks} = do
  childRequiredTypes <-
    foldM
      (\acc ChildInstance{ciComponent} ->
        mergePortTypes "sink" acc (pcRequires ciComponent))
      Map.empty
      (childInstances childInfos decls)

  localRequiredTypes <- sinkTypes lpSinks
  mergeTypeMaps "sink" childRequiredTypes localRequiredTypes

collectDemandsHere
  :: Map Text PortalComponent
  -> [Declaration]
  -> LocalPortals
  -> Either String PortalDemands
collectDemandsHere childInfos decls local =
  foldM
    mergeDemands
    (localDemands local)
    [ pcDemands ciComponent
    | ChildInstance{ciComponent} <- childInstances childInfos decls
    ]

localDemands :: LocalPortals -> PortalDemands
localDemands LocalPortals{lpSinks} =
  Map.map demandFromSinks lpSinks
 where
  demandFromSinks sinks@(PortalValue{pvType}:_) =
    PortalDemand pvType (length sinks)
  demandFromSinks [] =
    error "localDemands: empty sink list"

sinkTypes :: PortalSinks -> Either String PortalTypes
sinkTypes =
  foldM insertSinkTypes Map.empty . Map.toList
 where
  insertSinkTypes acc (label, sinks) =
    foldM
      (\acc0 PortalValue{pvType} -> insertType "sink" label pvType acc0)
      acc
      sinks

checkRequiredTypes
  :: PortalTypes
  -> PortalSources
  -> Either String ()
checkRequiredTypes required sources =
  mapM_ check (Map.toList required)
 where
  check (label, requiredTy) =
    case Map.lookup label sources of
      Nothing -> Right ()
      Just PortalValue{pvType}
        | requiredTy == pvType -> Right ()
        | otherwise ->
            Left ("Signal portal source/sink type mismatch for: " <> Text.unpack label)

allocateDownwardPorts
  :: Bool
  -> PortalTypes
  -> PortalSources
  -> ScopeM PortalPorts
allocateDownwardPorts isTop requiredTypes sourcesHere = do
  let
    missingSources =
      Map.difference requiredTypes (Map.map pvType sourcesHere)

  unless (not isTop || Map.null missingSources) $
    throwError $
      "No signal portal source found for: " <>
      Text.unpack (Text.intercalate ", " (Map.keys missingSources))

  traverseWithKeyM
    (\label ty -> PortalValue ty <$> freshPortalId label "in")
    missingSources

allocateUpwardPorts
  :: Bool
  -> PortalDemands
  -> PortalSources
  -> ScopeM PortalPorts
allocateUpwardPorts isTop outsideDemands sourcesHere
  | isTop = pure Map.empty
  | otherwise =
      traverseWithKeyM
        (\label PortalValue{pvType} -> PortalValue pvType <$> freshPortalId label "out")
        (Map.intersection sourcesHere outsideDemands)

portSources :: PortalPorts -> PortalSources
portSources =
  Map.map \PortalValue{pvType, pvValue=portId} ->
    PortalValue pvType (Identifier portId Nothing)

inputPorts :: PortalPorts -> [(Identifier, HWType)]
inputPorts ports =
  [ (portId, portTy)
  | PortalValue{pvType=portTy, pvValue=portId} <- Map.elems ports
  ]

outputPorts :: PortalPorts -> [(Usage, (Identifier, HWType), Maybe Expr)]
outputPorts ports =
  [ (Cont, (portId, portTy), Nothing)
  | PortalValue{pvType=portTy, pvValue=portId} <- Map.elems ports
  ]

childWireDecls :: ChildOutputWires -> [Declaration]
childWireDecls childOutputs =
  [ NetDecl Nothing coWire portTy
  | outputs <- Map.elems childOutputs
  , ChildOutput{coPort=PortalValue{pvType=portTy}, coWire} <- Map.elems outputs
  ]

sinkAssignments :: PortalSources -> PortalSinks -> [Declaration]
sinkAssignments sourcesAvailable sinksByName =
  [ Assignment sinkId Cont sourceExpr
  | (label, sinks) <- Map.toList sinksByName
  , PortalValue{pvValue=sourceExpr} <- maybeToList (Map.lookup label sourcesAvailable)
  , PortalValue{pvValue=sinkId} <- sinks
  ]

upwardAssignments :: PortalSources -> PortalPorts -> [Declaration]
upwardAssignments sourcesHere upwardPorts =
  [ Assignment portId Cont sourceExpr
  | (label, PortalValue{pvValue=portId}) <- Map.toList upwardPorts
  , PortalValue{pvValue=sourceExpr} <- maybeToList (Map.lookup label sourcesHere)
  ]

rewriteInstantiations
  :: Map Text PortalComponent
  -> ChildOutputWires
  -> PortalSources
  -> [Declaration]
  -> ScopeM [Declaration]
rewriteInstantiations childInfos childOutputs sourcesAvailable =
  mapM rewrite . zip [(0 :: Int)..]
 where
  rewrite :: (Int, Declaration) -> ScopeM Declaration
  rewrite (_ix, decl@InstDecl{}) | Nothing <- instComponentName decl =
    pure decl

  rewrite (ix, InstDecl ent lib attrs componentName instName params portMap) =
    case Map.lookup (Id.toText componentName) childInfos of
      Nothing ->
        pure (InstDecl ent lib attrs componentName instName params portMap)
      Just childInfo -> do
        outputPortMap <- outputMappings ix
        inputPortMap <- inputMappings childInfo

        case portMap of
          NamedPortMap ports ->
            pure $
              InstDecl ent lib attrs componentName instName params $
                NamedPortMap (ports ++ outputPortMap ++ inputPortMap)
          IndexedPortMap{} ->
            if null outputPortMap && null inputPortMap then
              pure (InstDecl ent lib attrs componentName instName params portMap)
            else
              throwError $
                "Cannot add signal portal ports to indexed instantiation: " <>
                Text.unpack (Id.toText instName)

  rewrite (_ix, decl) =
    pure decl

  outputMappings
    :: Int
    -> ScopeM [(Expr, PortDirection, HWType, Expr)]
  outputMappings ix =
    case Map.lookup ix childOutputs of
      Nothing -> pure []
      Just outputs ->
        pure
          [ ( Identifier childPortId Nothing
            , Out
            , portTy
            , Identifier coWire Nothing )
          | ChildOutput{coPort=PortalValue{pvType=portTy, pvValue=childPortId}, coWire}
              <- Map.elems outputs
          ]

  inputMappings
    :: PortalComponent
    -> ScopeM [(Expr, PortDirection, HWType, Expr)]
  inputMappings childInfo =
    forM (Map.toList (pcRequires childInfo)) \(label, PortalValue{pvType=portTy, pvValue=childPortId}) ->
      case Map.lookup label sourcesAvailable of
        Just PortalValue{pvValue=sourceExpr} ->
          pure (Identifier childPortId Nothing, In, portTy, sourceExpr)
        Nothing ->
          throwError $
            "No signal portal source found for: " <> Text.unpack label

freshPortalId :: Text -> Text -> ScopeM Identifier
freshPortalId label suffix =
  lift (Id.makeBasic ("portal_" <> sanitize label <> "_" <> suffix))

sanitize :: Text -> Text
sanitize =
  Text.map \c ->
    if c == '_' || c == '$' || c == '.' || c == '-' || c == ':' || c == '/' ||
       ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9')
    then c
    else '_'

mergeSources
  :: PortalSources
  -> PortalSources
  -> Either String PortalSources
mergeSources left right =
  foldM
    (\acc (label, source) -> insertSource label source acc)
    left
    (Map.toList right)

insertSource
  :: PortalName
  -> PortalSource
  -> PortalSources
  -> Either String PortalSources
insertSource label source acc =
  case Map.lookup label acc of
    Nothing -> Right (Map.insert label source acc)
    Just old
      | pvType old == pvType source ->
          Left ("Multiple signal portal sources found for: " <> Text.unpack label)
      | otherwise ->
          Left ("Signal portal source type mismatch for: " <> Text.unpack label)

mergePortTypes
  :: String
  -> PortalTypes
  -> PortalPorts
  -> Either String PortalTypes
mergePortTypes context acc ports =
  mergeTypeMaps context acc (Map.map pvType ports)

mergeTypeMaps
  :: String
  -> PortalTypes
  -> PortalTypes
  -> Either String PortalTypes
mergeTypeMaps context acc types =
  foldM
    (\acc0 (label, ty) -> insertType context label ty acc0)
    acc
    (Map.toList types)

mergeDemands
  :: PortalDemands
  -> PortalDemands
  -> Either String PortalDemands
mergeDemands left right =
  foldM
    (\acc (label, demand) -> insertDemand label demand acc)
    left
    (Map.toList right)

insertDemand
  :: PortalName
  -> PortalDemand
  -> PortalDemands
  -> Either String PortalDemands
insertDemand label demand acc =
  case Map.lookup label acc of
    Nothing ->
      Right (Map.insert label demand acc)
    Just old
      | pdType old == pdType demand ->
          Right $
            Map.insert
              label
              demand{pdCount = pdCount old + pdCount demand}
              acc
      | otherwise ->
          Left ("Signal portal sink type mismatch for: " <> Text.unpack label)

demandsOutside
  :: PortalDemands
  -> PortalDemands
  -> PortalDemands
demandsOutside global subtree =
  Map.mapMaybeWithKey outside global
 where
  outside label demand =
    let outsideCount = pdCount demand - maybe 0 pdCount (Map.lookup label subtree) in
    if outsideCount > 0 then
      Just demand{pdCount = outsideCount}
    else
      Nothing

unusedSourceWarnings
  :: PortalDemands
  -> Map Text LocalPortals
  -> [String]
unusedSourceWarnings globalDemands localPortals =
  [ "Signal portal source '" <> Text.unpack label <>
    "' in component '" <> Text.unpack componentName <>
    "' has no matching portalSink"
  | (componentName, LocalPortals{lpSources}) <- Map.toList localPortals
  , label <- Map.keys lpSources
  , Map.notMember label globalDemands
  ]

insertType
  :: String
  -> PortalName
  -> HWType
  -> PortalTypes
  -> Either String PortalTypes
insertType context label ty acc =
  case Map.lookup label acc of
    Nothing -> Right (Map.insert label ty acc)
    Just old
      | old == ty -> Right acc
      | otherwise ->
          Left ("Signal portal " <> context <> " type mismatch for: " <> Text.unpack label)
