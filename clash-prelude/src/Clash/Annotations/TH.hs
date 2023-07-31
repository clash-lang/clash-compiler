{-|

This module can automatically generate TopEntity definitions from 'Clash.NamedTypes'
annotations. Annotations involving data\/type families must be inspected for correctness.
Not all cases can be handled with automatic generation due to the difficulty of type manipulation
in template Haskell. In particular annotations __inside__ the following is unlikely to work:

- Data\/type family referencing other data\/type families.
- Annotations inside recursive data types
- Clock constraints other than a single HiddenClockResetEnable. (You can still
  use arbitrary explicit clock\/reset\/enables!)

See <https://github.com/clash-lang/clash-compiler/blob/master/clash-prelude/tests/Clash/Tests/TopEntityGeneration.hs Clash.Tests.TopEntityGeneration>
for more examples.

@
import Clash.Annotations.TH

data Named
  = Named
  { name1 :: "named1" ::: BitVector 3
  , name2 :: "named2" ::: BitVector 5
  }

topEntity :: "tup1" ::: Signal System (Int, Bool)
          -> "tup2" ::: (Signal System Int, Signal System Bool)
          -> "tup3" ::: Signal System ("int":::Int, "bool":::Bool)
          -> "tup4" ::: ("int":::Signal System Int, "bool":::Signal System Bool)
          -> "custom" ::: Signal System Named
          -> "outTup" ::: Signal System ("outint":::Int, "outbool":::Bool)
topEntity = undefined
makeTopEntity 'topEntity
-- ===>
--  {-# ANN topEntity Synthesize "topEntity3"
--     [ PortName "tup1"
--     , PortName "tup2"
--     , PortProduct "tup3" [PortName "int",PortName "bool"]
--     , PortProduct "tup4" [PortName "int",PortName "bool"]
--     , PortProduct "custom" [PortName "named1",PortName "named2"]
--     ]
--     (PortProduct "outTup" [PortName "outint",PortName "outbool"])
--     #-}
@

-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- Required to 'makeBaseFunctor' of 'Language.Haskell.TH.Syntax.Type'

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Annotations.TH
  ( -- * To create a Synthesize annotation pragma
    makeTopEntity
  , makeTopEntityWithName
  , makeTopEntityWithName'
    -- * To create a TopEntity value
  , buildTopEntity
  , maybeBuildTopEntity
  , getNameBinding
  )
where

import           Data.Foldable                  ( fold)
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes )
import           Language.Haskell.TH

import           Data.Functor.Foldable          ( para )
import           Data.Functor.Foldable.TH
import           Control.Lens                   ( (%~), (&), (.~)
                                                , _1, _2, _3, view
                                                )
import           Control.Monad                  (mfilter, liftM2, forM, zipWithM)
import           Control.Monad.Trans.Reader     (ReaderT(..), asks, local)
import           Control.Monad.Trans.Class      (lift)
import           Language.Haskell.TH.Instances  ( )
import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Syntax     (qRecover)
import           Data.Generics.Uniplate.Data    (rewrite)

import           Clash.Annotations.TopEntity    ( PortName(..)
                                                , TopEntity(..)
                                                )
import           Clash.NamedTypes               ((:::))
import           Clash.Signal                   ( HiddenClockResetEnable
                                                , HiddenClock, HiddenReset, HiddenEnable
                                                , Signal)
import           Clash.Signal.Delayed           (DSignal)

$(makeBaseFunctor ''Type)

-- | A datatype to track failing naming in a subtree.
data Naming a = Complete a | HasFail String | BackTrack (Set.Set Name)
  deriving Functor

instance Semigroup a => Semigroup (Naming a) where
  Complete a <> Complete b     = Complete $ a <> b
  BackTrack n1 <> BackTrack n2 = BackTrack $ n1 <> n2
  BackTrack n <> _             = BackTrack n
  _ <> BackTrack n             = BackTrack n
  HasFail e1 <> HasFail e2     = HasFail $ e1 ++ "\n" ++ e2
  _ <> HasFail e               = HasFail e
  HasFail e <> _               = HasFail e

instance Monoid a => Monoid (Naming a) where
  mempty = Complete mempty

-- | Track seen 'Name's, and track current 'Info' for error reporting.
type ErrorContext = String
type TrackData = (Set.Set Name, ErrorContext)
type Tracked m a = ReaderT TrackData m a

-- * Utility functions

-- | Matches a type `a -> b`
pattern ArrowTy :: Type -> Type -> Type
pattern ArrowTy a b = AppT (AppT ArrowT a) b

-- | Greedily split on top level 'AppT' to recover basic type
-- application as a list of 'Type'.
unapp :: Type -> [Type]
unapp (AppT l r) = unapp l ++ [r]
unapp t = [t]

-- | Greedily split on top level outer arrows, splitting a function 'Type' into
-- it's arguments. (Result type discarded)
unarrow :: Type -> [Type]
unarrow (ArrowTy x y) = x : unarrow y
unarrow _ = []

-- | Collapse a list of 'PortName's into a single 'PortName'
collapseNames :: [PortName] -> [PortName]
collapseNames [] = []
collapseNames [x] = [x]
collapseNames xs = [PortProduct "" xs]

-- | Failure message with a prefix to add some context for end users.
failMsg :: String -> String
failMsg s = "TopEntity generation error: " ++ s

-- | Retrieve current error context
errorContext :: Tracked Q String
errorContext = asks snd

-- Failure message with prefix in a 'Tracked' context
failMsgWithContext :: String -> Tracked Q String
failMsgWithContext s = (++) (failMsg s) <$> errorContext

-- | Track a new seen 'Name' and update 'Info' for error handling
visit :: (Show b) => Name -> b -> Tracked m a -> Tracked m a
visit name a = local (\t -> t & _1 %~ Set.insert name
                              & _2 .~ show a)

-- | Grab the 'Name's of type variables in a datatype
datatypeVars' :: DatatypeInfo -> [Name]
#if MIN_VERSION_th_abstraction(0,3,0)
datatypeVars' d = tvName <$> datatypeVars d
#else
datatypeVars' d = name <$> datatypeVars d
 where
  name (VarT n) = n
  name (SigT n _) = name n
  name e = error $ "Unexpected datatype variable name of type " ++ show e
#endif

-- | Run a 'Name' through the template haskell machinery, getting a
-- 'DatatypeInfo' if the 'Name' specified a datatype. The result is processed by
-- a given function or a default @a@ is returned in the style of 'maybe'.
tryReifyDatatype :: a -> (DatatypeInfo -> a) -> Name -> Tracked Q a
tryReifyDatatype a f name = lift (recover (pure a) $ f <$> reifyDatatype name)

-- * Type tree folding \/ unfolding

-- | Flag constructors with partially named fields as failing.
portsFromTypes
  :: [Type]
  -> Tracked Q (Naming [PortName])
portsFromTypes xs = do
  (mconcat <$> mapM f xs)
  >>= \case
    Complete names | length names > 0 && length names /= length xs ->
      HasFail <$> failMsgWithContext "Partially named constructor arguments!\n"
    x -> return x
 where
  f = fmap (fmap collapseNames) . gatherNames

-- | Flag sum types as failing if they have any constructors with names.
handleNamesInSum
  :: [ConstructorInfo]
  -> Tracked Q (Naming [PortName])
handleNamesInSum xs =
  (fold <$> mapM portsFromTypes (constructorFields <$> xs)) >>= \case
    Complete [] -> return $ Complete []
    x ->
      mappend x . HasFail <$> failMsgWithContext "Annotated sum types not supported!\n"

-- | Build a list of 'PortName's from a Template Haskell 'Con' and a free
-- variable mapping
constructorToPorts :: Con -> Map.Map Name Type -> Tracked Q (Naming [PortName])
constructorToPorts c m = do
  let xs = applySubstitution m (ctys c)
  portsFromTypes xs
 where
  ctys (NormalC _ (fmap snd -> tys)) = tys
  ctys (RecC _ (fmap (view _3) -> tys)) = tys
  ctys (InfixC _ _ (snd -> ty)) = [ty]
  ctys (ForallC _ _ c') = ctys c'
  ctys (GadtC _ (fmap snd -> tys) _) = tys
  ctys (RecGadtC _ (fmap (view _3) -> tys) _) = tys

-- | Build a list of 'PortName's from a Template Haskell 'Name'
datatypeNameToPorts
  :: Name
  -> Tracked Q (Naming [PortName])
datatypeNameToPorts name = do
  constructors <- tryReifyDatatype [] datatypeCons name

  names <- case constructors of
    []  -> return $ Complete []
    [x] -> portsFromTypes (constructorFields x)
    xs  -> handleNamesInSum xs

  case names of
    BackTrack ns | Set.member name ns -> do
      lift $ reportWarning $ "Make sure HDL port names are correct:\n"
                           ++ "Backtracked when constructing " ++ pprint name
                           ++ "\n(Type appears recursive)"
      return $ case (Set.delete name ns) of
        e | e == Set.empty -> Complete []
        xs -> BackTrack xs
    _ -> return names

-- This shouldn't reduce
type family PortLabel where

-- Replace (:::) annotations with a stuck type family, to inhibit unifyTypes to reduce it
guardPorts :: Type -> Type
guardPorts = rewrite $ \case
    AppT (ConT split) name@(LitT (StrTyLit _)) | split == ''(:::) -> Just $ AppT (ConT ''PortLabel) name
    _ -> Nothing

-- | Recursively walking a 'Type' tree and building a list of 'PortName's.
typeTreeToPorts
  :: TypeF (Type, Tracked Q (Naming [PortName]))
  -- ^ Case under scrutiny, paramorphism style
  -> Tracked Q (Naming [PortName])
typeTreeToPorts (AppTF (AppT (ConT split) (LitT (StrTyLit name)), _) (_,c))
  -- Is there a '<String> ::: <something>' annotation?
  | split == ''PortLabel
  -- We found our split. If:
  -- - We only have no names from children: use split name as PortName
  -- - We have children reporting names: use split name as name to PortProduct
  = c >>= \case
    Complete []  -> return $ Complete [PortName name]
    Complete [PortName n2] -> return $ Complete [PortName (name ++ "_" ++ n2)]
    Complete xs  -> return $ Complete [PortProduct name xs]
    x            -> return x

typeTreeToPorts (ConTF name) = do
  -- Only attempt to resolve a subtree for names we haven't seen before
  seen <- asks fst
  if Set.member name seen
  then return $ BackTrack $ Set.singleton name
  else visit name name $ do
    info <- lift $ reify name
    case info of
      -- Either `name` is an unannotated primitive
      PrimTyConI _ _ _ -> return $ Complete []
      -- ... or a type synonym
      TyConI (TySynD _ _ t) -> gatherNames t
      -- ... or something "datatype" like
      _ -> datatypeNameToPorts name

typeTreeToPorts f@(AppTF (a,a') (b,b')) = do
  -- Gather types applied to a head type
  case unapp (AppT a b) of
    -- Return the inner type for signals
    (ConT x : _ : _ : []) | x == ''Clash.Signal.Signal -> b'
    (ConT x : _ : _ : _ : []) | x == ''Clash.Signal.Delayed.DSignal -> b'

    -- Other handled type applications are
    -- 1. Type synonyms
    -- 2. Closed type families
    -- 3. Open type and data families
    -- 4. Regular data types
    (ConT x : xs) -> do
      info <- lift $ reify x
      case info of
        -- 1. Type synonym case is just inserting the relevant port tree
        (TyConI (TySynD _ synvars def)) -> do
          gatherNames $ applyContext xs (tvName <$> synvars) def

        -- 2. Match argument lengths, substitute types, and then insert the port
        -- tree
        FamilyI (ClosedTypeFamilyD (TypeFamilyHead _ bds _ _) eqs) _
          | length bds == length xs -> do
              matches <- lift $ forM eqs $ \eq -> qRecover (return Nothing) . fmap Just $ do
                  sub <- mconcat <$> zipWithM (\l r -> unifyTypes [l, r]) xs (tySynArgs eq)
                  return $ applySubstitution sub $ tySynRHS eq
              case catMaybes matches of
                  (r:_) -> gatherNames r
                  -- We didn't find any matching instances (i.e. the
                  -- type family application is stuck) so give up.
                  [] -> return $ Complete []

        -- 3. Match argument lengths then:
        --   - Substitute port tree for type family
        --   - Try to get a unique constructor for data families and build
        --     port tree from the constructor
        _ | familyArity info == Just (length xs) -> do
          (lift $ reifyInstances x xs) >>= \case
#if MIN_VERSION_template_haskell(2,15,0)
            [TySynInstD (TySynEqn _ _ r)] ->
#else
            [TySynInstD _ (TySynEqn _ r)] ->
#endif
                gatherNames (applyFamilyBindings xs info r)

            [NewtypeInstD _ _ _ _ c _] -> constructorToPorts c (familyTyMap xs info)
            [DataInstD    _ _ _ _ cs _] -> do
              case cs of
                [c] -> constructorToPorts c (familyTyMap xs info)
                _ -> return $ Complete []
            y -> fail $ failMsg "Encountered unexpected type during family application!"
                      ++ pprint y

        -- 4. Check if head really is a datatype, apply free variables,
        --    and attempt to get a unique constructor
        _ -> do
          dataTy <- tryReifyDatatype Nothing Just x

          let -- Apply tail types to head datatype free type variables
              hasAllArgs   = \vs -> length xs == length (datatypeVars vs)
              constructors = applyDatatypeContext xs <$> mfilter hasAllArgs dataTy

              -- Attempt to get a unique constructor
              getSingleConstructor cs = do [c] <- cs; return c
              constructor = getSingleConstructor constructors

          -- If any steps failed, return the PortNames according to the head type.
          maybe a' (visit x (ppr x) . portsFromTypes . constructorFields) constructor

    -- If head is a tuple or list then we take all the names
    (ListT:_)    -> fold <$> mapM snd f
    (TupleT _:_) -> fold <$> mapM snd f

    -- We're not applying to a head 'ConT' so lets try best effort of getting names
    -- from all applied types
    _ -> do
      lift $ reportWarning $ "Make sure HDL port names are correct:\n"
                           ++ "Type application with non ConT head:\n:("
                           ++ pprint (AppT a b)
      f' <- mapM snd f
      return $ fold f'
 where
  tyMap ctx holes = Map.fromList $ zip holes ctx
  familyTyMap ctx (familyBindings -> Just holes) = tyMap ctx (tvName <$> holes)
  familyTyMap _ _  = error "familyTyMap called with non family argument!"
  applyContext ctx holes = applySubstitution (tyMap ctx holes)
  applyDatatypeContext ctx d = applyContext ctx (datatypeVars' d) <$> datatypeCons d
  applyFamilyBindings ctx (familyBindings -> Just holes) t
    = applyContext ctx (tvName <$> holes) t
  applyFamilyBindings _ _ _ = error "familyTyMap called with non family argument!"

#if MIN_VERSION_template_haskell(2,15,0)
  tySynArgs (TySynEqn _ args _) = drop 1 (unapp args)
#else
  tySynArgs (TySynEqn args _) = args
#endif

#if MIN_VERSION_template_haskell(2,15,0)
  tySynRHS (TySynEqn _ _ r) = r
#else
  tySynRHS (TySynEqn _ r) = r
#endif

  familyBindings (FamilyI (ClosedTypeFamilyD (TypeFamilyHead _ xs _ _) _) _) = Just xs
  familyBindings (FamilyI (OpenTypeFamilyD (TypeFamilyHead _ xs _ _)) _) = Just xs
  familyBindings (FamilyI (DataFamilyD _ xs _) _) = Just xs
  familyBindings _ = Nothing
  familyArity = fmap length . familyBindings

typeTreeToPorts f = do
  -- Just collect names
  f' <- mapM snd f
  return $ fold f'

-- | Gather naming tree attached to a 'Type' and its inner 'Type's
gatherNames
  :: Type
  -- ^ Type to investigate
  -> Tracked Q (Naming [PortName])
gatherNames =
  para typeTreeToPorts . guardPorts

-- Build a possible failing 'PortName' tree and unwrap the 'Naming' result.
buildPorts
  :: Type
  -- ^ Type to investigate
  -> Q [PortName]
buildPorts x = do
  flip runReaderT (Set.empty, "") $ gatherNames x
    >>= \case
      Complete xs -> return xs
      HasFail err -> fail err
      BackTrack n -> fail $ failMsg "Encountered recursive type at entry! " ++ show n

-- | Get the result 'PortName' from a function type
toReturnName :: Type -> Q PortName
toReturnName (ArrowTy _ b) = toReturnName b
toReturnName b             =
  buildPorts b
  >>= \case
     [] -> fail $ failMsg "No return name specified!"
     [x] -> return x
     xs -> return $ PortProduct "" xs

-- | Get the argument 'PortName's from a function type
toArgNames :: Type -> Q [PortName]
toArgNames ty = traverse build (unarrow ty)
 where
  build x = buildPorts x >>= check x
  check x []  = fail $ failMsg "Unnamed argument " ++ pprint x
  check _ [a] = return a
  check _ xs  = return $ PortProduct "" xs

data ClockType = None | SingleClockResetEnable | Other
  deriving Eq

-- | Strip constraints from a type.
--
-- Fail if:
-- - There are free type variables.
-- - There are multiple hidden clocks
handleConstraints :: Type -> ClockType -> Q (Type, ClockType)
handleConstraints (ForallT [] [] x) clk = handleConstraints x clk
handleConstraints (ForallT xs@(_:_) _ _) _ =
  fail $ failMsg "Free type variables!\n"
       ++ pprint xs
handleConstraints (ForallT _ c x) clk = handleConstraints x hiddenClocks
 where
  hiddenClocks = foldl findHiddenClocks clk c
  findHiddenClocks a (AppT (ConT b) _)
    | b == ''Clash.Signal.HiddenClockResetEnable && a == None
      = SingleClockResetEnable
    | b == ''Clash.Signal.HiddenClockResetEnable && a /= None
      = Other
    | b == ''Clash.Signal.HiddenClock
      || b == ''Clash.Signal.HiddenReset
      || b == ''Clash.Signal.HiddenEnable
      = Other
  findHiddenClocks a _ = a
handleConstraints x clk = return (x, clk)

clockToPorts :: ClockType -> Q [PortName]
clockToPorts None = return []
clockToPorts (SingleClockResetEnable) =
  return [PortProduct "" [ PortName "clk" , PortName "rst" , PortName "en" ]]
clockToPorts Other =
  fail $ failMsg "TH generation for multiple hidden clocks and"
       ++ " HiddenClock/HiddenReset/HiddenEnable currently unsupported!"

-- *

-- | Return a typed expression for a 'TopEntity' of a given @('Name', 'Type')@.
buildTopEntity :: Maybe String -> (Name, Type) -> TExpQ TopEntity
buildTopEntity topName (name, ty) = do
    (ty', clock) <- handleConstraints ty None

    ins   <- liftM2 (<>) (clockToPorts clock) (toArgNames ty')
    out   <- toReturnName ty'

    let outName = case topName of
          Just name' -> name'          -- user specified name
          Nothing    -> nameBase name  -- auto-generated from Haskell name

#if MIN_VERSION_template_haskell(2,17,0)
    (examineCode
#else
    (
#endif
                  [|| Synthesize
                     { t_name   = outName
                     , t_inputs = ins
                     , t_output = out
                     } ||])

-- | Return a typed 'Maybe TopEntity' expression given a 'Name'.
-- This will return an 'TExp' of 'Nothing' if 'TopEntity' generation failed.
maybeBuildTopEntity :: Maybe String -> Name -> Q (TExp (Maybe TopEntity))
maybeBuildTopEntity topName name = do
#if MIN_VERSION_template_haskell(2,17,0)
  recover (examineCode [|| Nothing ||]) $ do
    let expr = liftCode (getNameBinding name >>= buildTopEntity topName)
    examineCode [|| Just ($$expr) ||]
#else
  recover ([|| Nothing ||]) $ do
    let expr = getNameBinding name >>= buildTopEntity topName
    [|| Just ($$expr) ||]
#endif

-- | Turn the 'Name' of a value to a @('Name', 'Type')@
getNameBinding :: Name -> Q (Name, Type)
getNameBinding n = reify n >>= \case
  VarI name ty _ -> return (name, ty)
  _ -> fail "getNameBinding: Invalid Name, must be a top-level binding!"

-- | Wrap a 'TopEntity' expression in an annotation pragma
makeTopEntityWithName' :: Name -> Maybe String -> DecQ
makeTopEntityWithName' n topName = do
  (name,ty) <- getNameBinding n
  topEntity <- buildTopEntity topName (name,ty)
  let prag t = PragmaD (AnnP (valueAnnotation name) t)
  return $ prag $ unType topEntity

-- | Automatically create a @'TopEntity'@ for a given @'Name'@, using the given
-- @'String'@ to specify the name of the generated RTL entity.
--
-- The function arguments and return values of the function specified by the
-- given @'Name'@ must be annotated with @'(:::)'@. This annotation provides the
-- given name of the port.
makeTopEntityWithName :: Name -> String -> DecsQ
makeTopEntityWithName nam top = pure <$> makeTopEntityWithName' nam (Just top)

-- | Automatically create a @'TopEntity'@ for a given @'Name'@. The name of the
-- generated RTL entity will be the name of the function that has been
-- specified; e.g. @'makeTopEntity' 'foobar@ will generate a @foobar@ module.
--
-- The function arguments and return values of the function specified by the
-- given @'Name'@ must be annotated with @'(:::)'@. This annotation provides the
-- given name of the port.
makeTopEntity :: Name -> DecsQ
makeTopEntity nam = pure <$> makeTopEntityWithName' nam Nothing
