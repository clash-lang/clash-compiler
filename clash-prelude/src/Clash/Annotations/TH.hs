{-|

This module can automatically generate TopEntity definitions from 'Clash.NamedTypes'
annotations. Annotations try to be as accurate to the shape of the Clash compiler output.
Not all cases can be handled with automatic generation due to the difficulty of type manipulation
in template Haskell. In particular annotations _inside_ the following is unlikely to work:

- Data/type family constructors referencing other data/type families.
- Recursive data types
- Clock constraints other than a single HiddenClockResetEnable. (You can still
  use arbitrary explicit clock/reset/enables!)

See "Clash.Tests.TopEntityGeneration" for more examples.

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

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

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

import           Data.Foldable                  ( fold
                                                , find
                                                )
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup                as Semigroup
#endif
import           Language.Haskell.TH

import           Data.Functor.Foldable          ( cata, para, embed )
import           Data.Functor.Foldable.TH
import           Control.Lens                   ( view, (%~), (?~), (&)
                                                , _1, _2, _3
                                                )
import           Control.Monad                  ((>=>))
import           Control.Monad.Trans.Reader     (ReaderT(..), asks, local)
import           Control.Monad.Trans.Class      (lift)
import           Language.Haskell.TH.Instances  ( )
import           Language.Haskell.TH.Datatype

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
data Naming a = Complete a | HasFail String | BackTrack [Name]
  deriving Functor

instance Semigroup a => Semigroup (Naming a) where
  Complete a <> Complete b     = Complete $ a <> b
  BackTrack n1 <> BackTrack n2 = BackTrack $ n1 ++ n2
  BackTrack n <> _             = BackTrack n
  _ <> BackTrack n             = BackTrack n
  HasFail e1 <> HasFail e2     = HasFail $ e1 ++ "\n" ++ e2
  _ <> HasFail e               = HasFail e
  HasFail e <> _               = HasFail e

instance (Semigroup a, Monoid a) => Monoid (Naming a) where
  mempty = Complete mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = (Semigroup.<>)
#endif

-- | Track seen 'Name's, and track current 'Info' for error reporting.
type TrackData = (Set.Set Name, Maybe Info)
type Tracked m a = ReaderT TrackData m a

-- * Utility functions

-- | A Template Haskell helper function. Get the 'Type's from a 'Con'.
getTypes :: Con -> [Type]
getTypes (NormalC _ xs)    = map snd xs
getTypes (RecC _ xs)       = map (view _3) xs
getTypes (InfixC x _ y)    = [snd x, snd y]
getTypes (ForallC _ _ c)   = getTypes c
getTypes (GadtC _ xs _)    = map snd xs
getTypes (RecGadtC _ xs _) = map (view _3) xs

-- | A Template Haskell helper function. Get the 'Name' of a 'Con'. These names
-- are only used in type/data family resolution which is best effort,
-- so for Gadts we just take the first name.
getName :: Con -> Name
getName (NormalC n _)          = n
getName (RecC    n _)          = n
getName (InfixC  _ n _)        = n
getName (ForallC _ _ c)        = getName c
getName (GadtC    (n : _) _ _) = n
getName (RecGadtC (n : _) _ _) = n
getName (GadtC    [] _ _) = error "Encountered GADT with no constructors?"
getName (RecGadtC [] _ _) = error "Encountered GADT with no constructors?"

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

-- | Remove constraints from a type
stripConstraints :: Type -> Type
stripConstraints (ForallT _ _ x) = stripConstraints x
stripConstraints x = x

-- | Collapse a list of 'PortNames' into a single 'PortName'
collapseNames :: [PortName] -> [PortName]
collapseNames [] = []
collapseNames [x] = [x]
collapseNames xs = [PortProduct "" xs]

-- | Failure message with a prefix to add some context for end users.
failMsg :: String -> String
failMsg s = "TopEntity generation error: " ++ s

-- | Track a new seen 'Name' and update 'Info' for error handling
visit :: Name -> Info -> Tracked m a -> Tracked m a
visit name info = local (\t -> t & _1 %~ Set.insert name
                                 & _2 ?~ info)

-- | Retrieve current 'Info'
getInfo :: Tracked Q (Maybe Info)
getInfo = asks snd

-- * Type tree folding / unfolding

-- | Flag constructors with partially named fields as failing.
portsFromTypes
  :: [Type]
  -> Tracked Q (Naming [PortName])
portsFromTypes xs = do
  (mconcat <$> mapM f xs)
  >>= \case
    Complete names | length names > 0 && length names /= length xs -> do
      info <- getInfo
      return $ HasFail
             $ failMsg "Partially named constructor arguments!\n"
             ++ maybe "" pprint info
    x -> return x
 where
  f = fmap (fmap collapseNames) . expandFamiliesAndGatherNames

-- | Flag sum types as failing if they have any constructors with names.
handleNamesInSum
  :: [Con]
  -> Tracked Q (Naming [PortName])
handleNamesInSum xs =
  (fold <$> mapM portsFromTypes (getTypes <$> xs)) >>= \case
    Complete [] -> return $ Complete []
    x -> do
      info <- getInfo
      return $ mappend x
             $ HasFail
             $ failMsg "Annotated sum types not supported!\n"
             ++ maybe "" pprint info

-- | Build a list of 'PortName's from a Template Haskell 'Name'
nameToPorts
  :: Name
  -- ^ Name to investigate
  -> Tracked Q (Naming [PortName])
nameToPorts name = do
  seen <- asks fst
  if Set.member name seen
  then return $ Complete []
  else do
    -- Recur on types by reifying the name.
    -- This allows us to find splits defined in data types.
    info <- lift $ reify name

    names <- case info of
      TyConI (getConstructors -> [x]) ->
        visit name info $ portsFromTypes (getTypes x)
      TyConI (getConstructors -> xs)  ->
        -- This case can't have any valid name annotations as it is a sum type,
        -- but we decend to check for existence annotations to flag an error.
        visit name info $ handleNamesInSum xs
      DataConI _ ty _ -> do
        let xs = unarrow (stripConstraints ty)
        visit name info $ portsFromTypes xs
      PrimTyConI _ _ _ -> return $ Complete []
      _ -> return $ HasFail
                  $ failMsg "Unhandled type during building PortName tree: "
                  ++ pprint info

    return $ case names of
      BackTrack ns ->
        case filter (firstUsage seen) ns of
          [] -> Complete []
          x  -> BackTrack x
      x -> x
 where
  getConstructors (DataD _ _ _ _ x _)        = x
  getConstructors (NewtypeD _ _ _ _ x _)     = [x]
  getConstructors (DataInstD _ _ _ _ x _)    = x
  getConstructors (NewtypeInstD _ _ _ _ x _) = [x]
  getConstructors _                          = []

  firstUsage seen x = name == x && Set.notMember x seen

-- | A helper function for recursively walking a 'Type' tree and
-- building a list of 'PortName's.
gatherNames
  :: TypeF (Type, Tracked Q (Naming [PortName]))
  -- ^ Case under scrutiny, paramorphism style
  -> Tracked Q (Naming [PortName])
gatherNames (AppTF (AppT (ConT split) (LitT (StrTyLit name)), _) (_,c))
  -- Is there a '<String> ::: <something>' annotation?
  | split == ''(:::)
  -- We found our split. If:
  -- - We only have no names from children: use split name as PortName
  -- - We have children reporting names: use split name as name to PortProduct
  = c >>= \case
    Complete [] -> return $ Complete [PortName name]
    Complete xs -> return $ Complete [PortProduct name xs]
    x           -> return x

gatherNames (ConTF name) = nameToPorts name
gatherNames f@(AppTF (a,a') (b,_)) = do
  -- 1. Gather types applied to a head type
  -- 2. Check if head is a datatype
  -- 3. Apply tail types to head datatype free type variables
  -- 4. Attempt to get a unique constructor
  case unapp (AppT a b) of
    (ConT n : xs) -> do
      lift (recover (pure Nothing) (Just <$> reifyDatatype n)) >>= \case
        Just d | length xs == length (datatypeVars d) -> do
          case applyContext xs d of
            -- Good news- we've found a unique constructor
            [c] -> do
              let name = constructorName c
              info <- lift $ reify name
              visit name info $ portsFromTypes (constructorFields c)

            -- We failed to apply even though we have the correct number of args
            -- so just attempt to continue by returning the ports inside of
            -- the head type.
            _ -> a'

        -- It looks like we haven't got enough type arguments here so we can
        -- return the default of ports inside the head type. (If enough types
        -- are applied further down the tree, we will try again to correctly
        -- apply the types.)
        _ -> a'

    -- We're not applying to a 'ConT' so lets try best effort of getting names
    -- from all applied types
    _ -> do
      f' <- mapM snd f
      return $ fold f'
 where
  -- Substitute types into datatype type arguments
  applyContext ctx d = applySubstitution (Map.fromList $ zip vars ctx) <$> datatypeCons d
#if MIN_VERSION_th_abstraction(0,3,0)
   where vars = tvName <$> datatypeVars d
#else
   where vars = name <$> datatypeVars d
         name (VarT n) = n
         name (SigT n _) = name n
         name e = error $ "Unexpected datatype variable name of type " ++ show e
#endif

gatherNames f = do
  -- Just collect names
  f' <- mapM snd f
  return $ fold f'

-- | Helper algebra to expand first level (non-recursive) type/data family
-- instances in a best effort manner. Data family instances with sum type
-- constructors are ignored.
expandFamilies :: TypeF (Tracked Q Type) -> Tracked Q Type
expandFamilies (AppTF a b) = do
  a' <- a
  b' <- b
  case unapp (AppT a' b') of
    -- Simplify by replacing Signals/DSignals with the type they contain
    (ConT x : _ : y : []) | x == ''Clash.Signal.Signal -> return y
    (ConT x : _ : _ : y : []) | x == ''Clash.Signal.Delayed.DSignal -> return y
    (ConT x : xs) -> do
      info <- lift $ reify x
      case info of

        -- Closed type families have to be handled separately
        FamilyI (ClosedTypeFamilyD (TypeFamilyHead _ bds _ _) eqs) _ ->
          if length bds == length xs then
            case find ((==) xs . tySynArgs) eqs of
#if MIN_VERSION_template_haskell(2,15,0)
              Just (TySynEqn _ _ r) -> return r
#else
              Just (TySynEqn _ r) -> return r
#endif
              _ -> return (AppT a' b')
                -- We didn't find a matching instance so give up.
          else return (AppT a' b')
                  -- We don't yet have all the arguments.

        -- Check for open type families and data families
        _ | familyArity info == Just (length xs) -> do
          (lift $ reifyInstances x xs) >>= \case
#if MIN_VERSION_template_haskell(2,15,0)
            [TySynInstD (TySynEqn _ _ t)] -> return t
#else
            [TySynInstD _ (TySynEqn _ t)] -> return t
#endif
            [NewtypeInstD _ _ _ _ c _] -> return $ ConT (getName c)
            [DataInstD    _ _ _ _ cs _] -> do
              case cs of
                [c] -> return $ ConT (getName c)
                _ -> return $ PromotedTupleT 0
                  -- Ignore sum type in a data family by replacing with
                  -- empty tuple. We don't want to fail because this subtree
                  -- might not be relevant to naming.
            y -> fail $ failMsg "Encountered unexpected type during family application!"
                      ++ " Perhaps a missing instance?\n" ++ pprint y

        _ -> return (AppT a' b')
    _ -> return (AppT a' b')
 where
#if MIN_VERSION_template_haskell(2,15,0)
  tySynArgs (TySynEqn _ args _) = tail (unapp args)
#else
  tySynArgs (TySynEqn args _) = args
#endif

  familyArity (FamilyI (OpenTypeFamilyD (TypeFamilyHead _ xs _ _)) _) =
    Just (length xs)
  familyArity (FamilyI (DataFamilyD _ xs _) _) = Just (length xs)
  familyArity _ = Nothing
expandFamilies t = embed <$> sequence t

-- | Runs 'expandFamilies' and then 'gatherNames'
expandFamiliesAndGatherNames
  :: Type
  -- ^ Type to investigate
  -> Tracked Q (Naming [PortName])
expandFamiliesAndGatherNames =
  cata expandFamilies >=> para gatherNames

-- | Build a possible failing 'PortName' tree and unwrap the 'Naming' result.
buildPorts
  :: Type
  -- ^ Type to investigate
  -> Q [PortName]
buildPorts x = do
  flip runReaderT (Set.empty, Nothing) $ expandFamiliesAndGatherNames x
    >>= \case
      Complete xs -> return xs
      HasFail err -> fail err
      BackTrack n -> fail $ failMsg "Encountered recursive type at entry! " ++ pprint n

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
handleConstraints (ForallT _ c x) clk
  | hiddenClocks == Other
    = fail $ failMsg "TH generation for multiple hidden clocks and"
           ++ " HiddenClock/HiddenReset/HiddenEnable currently unsupported!"
  | otherwise = handleConstraints x hiddenClocks
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

-- *

-- | Return a typed expression for a 'TopEntity' of a given @('Name', 'Type')@.
buildTopEntity :: Maybe String -> (Name, Type) -> TExpQ TopEntity
buildTopEntity topName (name, ty) = do
    (ty', clockConstraints) <- handleConstraints ty None

    let clock = if clockConstraints == SingleClockResetEnable
                then [ PortProduct ""
                       [ PortName "clk"
                       , PortName "rst"
                       , PortName "en"
                       ]
                     ]
                else []

    ins   <- mappend clock <$> toArgNames ty'
    out   <- toReturnName ty'

    let outName = case topName of
          Just name' -> name'          -- user specified name
          Nothing    -> nameBase name  -- auto-generated from Haskell name

    [|| Synthesize
        { t_name   = outName
        , t_inputs = ins
        , t_output = out
        } ||]

-- | Return a typed 'Maybe TopEntity' expression given a 'Name'.
-- This will return an 'TExp' of 'Nothing' if 'TopEntity' generation failed.
maybeBuildTopEntity :: Maybe String -> Name -> Q (TExp (Maybe TopEntity))
maybeBuildTopEntity topName name = do
  recover ([|| Nothing ||]) $ do
    let expr = getNameBinding name >>= buildTopEntity topName
    [|| Just ($$expr) ||]

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
