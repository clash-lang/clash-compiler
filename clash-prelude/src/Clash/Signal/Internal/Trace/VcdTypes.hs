
module Clash.Signal.Internal.Trace.VcdTypes (
  DeclarationCommand(..),
  SimulationCommand(..),
  Var(..),
  ValueChange(..),
  Scope,IDCode,VcdTime,TimeUnit,
  Width, Value, Period,
  zipTimed, treeFold, findChangesSimple,combineChangesPerDom,
) where

import GHC.Exts (groupWith)
import GHC.Natural (Natural)
import Data.List
  (zip4, unzip5, transpose, uncons)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)

import Clash.Time (Time, timeInFS)

-- More generic than VCD, but they need to be placed in the common ancestor.
type Width = Int
type Period = Time
type Value = (Natural, Natural) -- (mask, value)

-- | A VCD declaration command.
data DeclarationCommand
  = Version String                   -- ^ The (Clash) version used to generate the VCD.
  | Date UTCTime                     -- ^ The timestamp the VCD was created.
  | TimeScale Time                   -- ^ The unit of the VCD time stamps.
                                     -- Must be 1, 10 or 100 and an appropriate unit.
  | VarDec Var                       -- ^ Variable declaration.
  | Scope Scope [DeclarationCommand] -- ^ A closed scope declaration.
  | Comment String                   -- ^ A comment.
  deriving (Show)

-- | A VCD simulation command.
data SimulationCommand
  = SimulationTime VcdTime     -- ^ Timestamp.
  | DumpVars [ValueChange]     -- ^ The initial values of variables.
  | ValueChanges [ValueChange] -- ^ Value changes.
 deriving (Show)

-- | A VCD Variable.
data Var
  = Var
  { varSize      :: Width  -- ^ Bit width.
  , varIDCode    :: IDCode -- ^ Variable ID (typically one character).
  , varReference :: String -- ^ Variable name.
  }
  deriving (Show)

-- | A change to a variable's value.
data ValueChange
  = ValueChange
  { changeSize   :: Width  -- ^ Bit width of the value.
  , changeIDCode :: IDCode -- ^ ID of the variable that changed.
  , changeValue  :: Value  -- ^ The new value.
  }
  deriving (Show)

type Scope = String
type IDCode = String
type VcdTime = Integer --TODO: check if Int is faster
type TimeUnit = String


-- | Combine a list of pairs of initial values and timestamped changes.
combineChangesPerDom ::
  [([ValueChange],[(VcdTime,[ValueChange])])] ->
  ([ValueChange],[SimulationCommand])
combineChangesPerDom changesPerDomain = (initialValues, changes)
 where
  initialValues :: [ValueChange]
  initialValues = concatMap fst changesPerDomain

  -- essentially merge sort with nested merging on equal values
  -- TODO: small to large merging might be better?
  blocks :: [(VcdTime, [ValueChange])]
  blocks =
    treeFold zipTimed $
      map snd changesPerDomain

  changes :: [SimulationCommand]
  changes = concatMap (\(t,cs) -> [SimulationTime t, ValueChanges cs]) blocks


-- | Find changes in a list of signals with equal period
findChangesSimple
  :: VcdTime
  -> (VcdTime, VcdTime, VcdTime)
  -> (Period,([Width],[String],[[Value]]))
  -> ([ValueChange],[(VcdTime,[ValueChange])])
findChangesSimple
  vcdTimeScaleFS
  (vStart, vClockStart, vStop)
  (period,(widths0,labels0,valuess0))
  = (initial0, samples0)
 where
  clkEdges :: [VcdTime]
  clkEdges = [vClockStart, vClockStart + vPeriod ..]
    where
    vPeriod = timeInFS period `div` vcdTimeScaleFS

  -- group by clock cycle and add timestamps (the initial value starts at -inf)
  valuessFrom :: [(VcdTime,[Value])]
  valuessFrom = zip (-1000000000000:clkEdges) $ transpose valuess0
    --TODO: minBound, but we're using integers, which are unbounded :/

  -- drop values until we reach the start
  skipStart :: [(VcdTime,[Value])]
  skipStart = map fst $ dropWhile ((<= vStart) . snd) $ zip valuessFrom clkEdges

  ((_,initial), rest) = fromMaybe (error "(unreachable) finite signal") $ uncons skipStart

  samples :: [(VcdTime,[Value])]
  samples = takeWhile ((< vStop) . fst) rest

  filterChanges (_ta,va) (tb,vb) =
    (tb
    , [ValueChange w l b | (a,b,w,l) <- zip4 va vb widths0 labels0, a /= b]
    )

  initial0 :: [ValueChange]
  initial0 = zipWith3 ValueChange widths0 labels0 initial

  samples0 :: [(VcdTime,[ValueChange])]
  samples0 = filter (not . null . snd) $ zipWith filterChanges skipStart samples

-- | Merge items with the given function using a binary tree.
-- A linear fold would incur quadratic runtime.
treeFold :: (a -> a -> a) -> [a] -> a
treeFold f0 xs0 = treeFold0 (length xs0) f0 xs0
 where
  treeFold0 _ _ []  = error "treeFold used on empty sequence" -- unreachable for us
  treeFold0 _ _ [x] = x
  treeFold0 l f xs  = treeFold0 l0 f as `f` treeFold0 (l - l0) f bs
   where
    l0 = l `div` 2
    (as, bs) = splitAt l0 xs

-- | Zip two lists of timestamped lists of changes.
-- A single merge in the merge sort
zipTimed :: [(VcdTime,[a])] -> [(VcdTime,[a])] -> [(VcdTime,[a])]
zipTimed aa [] = aa
zipTimed [] bb = bb
zipTimed aa@((ta,va):as) bb@((tb,vb):bs) =
  case compare ta tb of
    LT -> (ta,    va) : zipTimed as bb
    EQ -> (ta,va<>vb) : zipTimed as bs
    GT -> (tb,    vb) : zipTimed aa bs
