{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Self-synchronising circuits based on data-flow principles.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module CLaSH.Prelude.DataFlow
  ( -- * Data types
    DataFlow
  , DataFlow' (..)
    -- * Creating DataFlow circuits
  , liftDF
  , pureDF
  , mealyDF
  , mooreDF
  , fifoDF
    -- * Composition combinators
  , idDF
  , seqDF
  , firstDF
  , swapDF
  , secondDF
  , parDF
  , parNDF
  , loopDF
  , loopDF_nobuf
    -- * Lock-Step operation
  , LockStep (..)
  )
where

import GHC.TypeLits           (KnownNat, KnownSymbol, type (+), type (^))
import Prelude                hiding ((++), (!!), length, map, repeat, tail, unzip3, zip3
                              , zipWith)

import CLaSH.Class.BitPack    (boolToBV)
import CLaSH.Class.Resize     (truncateB)
import CLaSH.Prelude.BitIndex (msb)
import CLaSH.Prelude.Mealy    (mealyB')
import CLaSH.Promoted.Nat     (SNat (..), addSNat, pow2SNat)
import CLaSH.Signal           ((.&&.), not1, regEn, unbundle)
import CLaSH.Signal.Bundle    (Bundle (..))
import CLaSH.Signal.Explicit  (Clock (..), Signal', SystemClock, sclock)
import CLaSH.Sized.BitVector  (BitVector)
import CLaSH.Sized.Vector

{- | Dataflow circuit with bidirectional synchronisation channels.

In the /forward/ direction we assert /validity/ of the data. In the /backward/
direction we assert that the circuit is /ready/ to receive new data. A circuit
adhering to the 'DataFlow' type should:

 * Not consume data when validity is deasserted.
 * Only update its output when readiness is asserted.

The 'DataFlow'' type is defined as:

@
newtype DataFlow' clk iEn oEn i o
  = DF
  { df :: 'Signal'' clk i     -- Incoming data
       -> 'Signal'' clk iEn   -- Flagged with /valid/ bits @iEn@.
       -> 'Signal'' clk oEn   -- Incoming back-pressure, /ready/ edge.
       -> ( 'Signal'' clk o   -- Outgoing data.
          , 'Signal'' clk oEn -- Flagged with /valid/ bits @oEn@.
          , 'Signal'' clk iEn -- Outgoing back-pressure, /ready/ edge.
          )
  }
@

where:

 * @clk@ is the clock to which the circuit is synchronised.
 * @iEn@ is the type of the bidirectional incoming synchronisation channel.
 * @oEn@ is the type of the bidirectional outgoing synchronisation channel.
 * @i@ is the incoming data type.
 * @o@ is the outgoing data type.

We define several composition operators for our 'DataFlow' circuits:

 * 'seqDF' sequential composition.
 * 'parDF' parallel composition.
 * 'loopDF' add a feedback arc.
 * 'lockStep' proceed in lock-step.

When you look at the types of the above operators it becomes clear why we
parametrise in the types of the synchronisation channels.
-}
newtype DataFlow' clk iEn oEn i o
  = DF
  { -- | Create an ordinary circuit from a 'DataFlow' circuit
    df :: Signal' clk i     -- Incoming data
       -> Signal' clk iEn   -- Flagged with /valid/ bits @iEn@.
       -> Signal' clk oEn   -- Incoming back-pressure, /ready/ edge.
       -> ( Signal' clk o   -- Outgoing data.
          , Signal' clk oEn -- Flagged with /valid/ bits @oEn@.
          , Signal' clk iEn -- Outgoing back-pressure, /ready/ edge.
          )
  }

-- | Dataflow circuit synchronised to the 'SystemClock'.
type DataFlow iEn oEn i o = DataFlow' SystemClock iEn oEn i o

-- | Create a 'DataFlow' circuit from a circuit description with the appropriate
-- type:
--
-- @
-- 'Signal'' clk i        -- Incoming data.
-- -> 'Signal'' clk Bool  -- Flagged with a single /valid/ bit.
-- -> 'Signal'' clk Bool  -- Incoming back-pressure, /ready/ bit.
-- -> ( 'Signal'' clk o   -- Outgoing data.
--    , 'Signal'' clk oEn -- Flagged with a single /valid/ bit.
--    , 'Signal'' clk iEn -- Outgoing back-pressure, /ready/ bit.
--    )
-- @
--
-- A circuit adhering to the 'DataFlow' type should:
--
--  * Not consume data when validity is deasserted.
--  * Only update its output when readiness is asserted.
liftDF :: (Signal' clk i -> Signal' clk Bool -> Signal' clk Bool
                         -> (Signal' clk o, Signal' clk Bool, Signal' clk Bool))
       -> DataFlow' clk Bool Bool i o
liftDF = DF

-- | Create a 'DataFlow' circuit where the given function @f@ operates on the
-- data, and the synchronisation channels are passed unaltered.
pureDF :: (i -> o)
       -> DataFlow' clk Bool Bool i o
pureDF f = DF (\i iV oR -> (fmap f i,iV,oR))

-- | Create a 'DataFlow' circuit from a Mealy machine description as those of
-- "CLaSH.Prelude.Mealy"
mealyDF :: (s -> i -> (s,o))
        -> s
        -> DataFlow Bool Bool i o
mealyDF f iS = DF (\i iV oR -> let en     = iV .&&. oR
                                   (s',o) = unbundle (f <$> s <*> i)
                                   s      = regEn iS en s'
                               in  (o,iV,oR))

-- | Create a 'DataFlow' circuit from a Moore machine description as those of
-- "CLaSH.Prelude.Moore"
mooreDF :: (s -> i -> s)
        -> (s -> o)
        -> s
        -> DataFlow Bool Bool i o
mooreDF ft fo iS = DF (\i iV oR -> let en  = iV .&&. oR
                                       s'  = ft <$> s <*> i
                                       s   = regEn iS en s'
                                       o   = fo <$> s
                                   in  (o,iV,oR))

fifoDF_mealy :: forall addrSize a . KnownNat addrSize
  => (Vec (2^addrSize) a, BitVector (addrSize + 1), BitVector (addrSize + 1))
  -> (a, Bool, Bool)
  -> ((Vec (2^addrSize) a, BitVector (addrSize + 1), BitVector (addrSize + 1))
     ,(a, Bool, Bool))
fifoDF_mealy (mem,rptr,wptr) (wdata,winc,rinc) =
  case pow2SNat (SNat @ addrSize) of
    SNat -> case addSNat (SNat @ addrSize) (SNat @ 1) of
      SNat ->
        let raddr = truncateB rptr :: BitVector addrSize
            waddr = truncateB wptr :: BitVector addrSize

            mem' | winc && not full = replace waddr wdata mem
                 | otherwise        = mem

            rdata = mem !! raddr

            rptr' = rptr + boolToBV (rinc && not empty)
            wptr' = wptr + boolToBV (winc && not full)
            empty = rptr == wptr
            full  = msb rptr /= msb wptr && raddr == waddr
        in  ((mem',rptr',wptr'), (rdata,empty,full))

-- | Create a FIFO buffer adhering to the 'DataFlow' protocol. Can be filled
-- with initial content.
--
-- To create a FIFO of size 4, with two initial values 2 and 3 you would write:
--
-- @
-- fifo4 = 'fifoDF' d4 (2 :> 3 :> Nil)
-- @
fifoDF :: forall addrSize m n a nm rate .
     (KnownNat addrSize, KnownNat n, KnownNat m, (m + n) ~ (2 ^ addrSize),
     KnownSymbol nm, KnownNat rate)
  => SNat (m + n) -- ^ Depth of the FIFO buffer. Must be a power of two.
  -> Vec m a      -- ^ Initial content. Can be smaller than the size of the
                  -- FIFO. Empty spaces are initialised with 'undefined'.
  -> DataFlow' ('Clk nm rate) Bool Bool a a
fifoDF _ iS = DF $ \i iV oR -> case addSNat (SNat @ addrSize) (SNat @ 1) of
  SNat ->
    let clk            = sclock
        initRdPtr      = 0
        initWrPtr      = fromIntegral (length iS)
        initMem        = iS ++ repeat undefined :: Vec (m + n) a
        initS          = (initMem,initRdPtr,initWrPtr)
        (o,empty,full) = mealyB' clk fifoDF_mealy initS (i,iV,oR)
    in  (o,not1 empty, not1 full)

-- | Identity circuit
--
-- <<doc/idDF.svg>>
idDF :: DataFlow' clk en en a a
idDF = DF (\a val rdy -> (a,val,rdy))

-- | Sequential composition of two 'DataFlow' circuits.
--
-- <<doc/seqDF.svg>>
seqDF :: DataFlow' clk aEn bEn a b
      -> DataFlow' clk bEn cEn b c
      -> DataFlow' clk aEn cEn a c
(DF f) `seqDF` (DF g) = DF (\a aVal cRdy -> let (b,bVal,aRdy) = f a aVal bRdy
                                                (c,cVal,bRdy) = g b bVal cRdy
                                            in  (c,cVal,aRdy))

-- | Apply the circuit to the first halve of the communication channels, leave
-- the second halve unchanged.
--
-- <<doc/firstDF.svg>>
firstDF :: DataFlow' clk aEn bEn a b
        -> DataFlow' clk (aEn,cEn) (bEn,cEn) (a,c) (b,c)
firstDF (DF f) = DF (\ac acV bcR -> let (a,c)     = unbundle ac
                                        (aV,cV)   = unbundle acV
                                        (bR,cR)   = unbundle bcR
                                        (b,bV,aR) = f a aV bR
                                        bc        = bundle (b,c)
                                        bcV       = bundle (bV,cV)
                                        acR       = bundle (aR,cR)
                                    in  (bc,bcV,acR)
                    )

-- | Swap the two communication channels.
--
-- <<doc/swapDF.svg>>
swapDF :: DataFlow' clk (aEn,bEn) (bEn,aEn) (a,b) (b,a)
swapDF = DF (\ab abV baR -> (swap <$> ab, swap <$> abV, swap <$> baR))
  where
    swap ~(a,b) = (b,a)

-- | Apply the circuit to the second halve of the communication channels, leave
-- the first halve unchanged.
--
-- <<doc/secondDF.svg>>
secondDF :: DataFlow' clk aEn bEn a b
         -> DataFlow' clk (cEn,aEn) (cEn,bEn) (c,a) (c,b)
secondDF f = swapDF `seqDF` firstDF f `seqDF` swapDF

-- | Compose two 'DataFlow' circuits in parallel.
--
-- <<doc/parDF.svg>>
parDF :: DataFlow' clk aEn bEn a b
      -> DataFlow' clk cEn dEn c d
      -> DataFlow' clk (aEn,cEn) (bEn,dEn) (a,c) (b,d)
f `parDF` g = firstDF f `seqDF` secondDF g

-- | Compose /n/ 'DataFlow' circuits in parallel.
parNDF :: KnownNat n
       => Vec n (DataFlow' clk aEn bEn a b)
       -> DataFlow' clk
                    (Vec n aEn)
                    (Vec n bEn)
                    (Vec n a)
                    (Vec n b)
parNDF fs =
  DF (\as aVs bRs ->
        let as'  = unbundle as
            aVs' = unbundle aVs
            bRs' = unbundle bRs
            (bs,bVs,aRs) = unzip3 (zipWith (\k (a,b,r) -> df k a b r) fs
                                  (zip3 (lazyV as') (lazyV aVs') bRs'))
        in  (bundle bs,bundle bVs, bundle aRs)
     )

-- | Feed back the second halve of the communication channel. The feedback loop
-- is buffered by a 'fifoDF' circuit.
--
-- So given a circuit /h/ with two synchronisation channels:
--
-- @
-- __h__ :: 'DataFlow' (Bool,Bool) (Bool,Bool) (a,d) (b,d)
-- @
--
-- Feeding back the /d/ part (including its synchronisation channels) results
-- in:
--
-- @
-- 'loopDF' d4 Nil h
-- @
--
-- <<doc/loopDF.svg>>
--
-- When you have a circuit @h'@, with only a single synchronisation channel:
--
-- @
-- __h'__ :: 'DataFlow' Bool Bool (a,d) (b,d)
-- @
--
-- and you want to compose /h'/ in a feedback loop, the following will not work:
--
-- @
-- f \`@'seqDF'@\` ('loopDF' d4 Nil h') \`@'seqDF'@\` g
-- @
--
-- The circuits @f@, @h@, and @g@, must operate in /lock-step/ because the /h'/
-- circuit only has a single synchronisation channel. Consequently, there
-- should only be progress when all three circuits are producing /valid/ data
-- and all three circuits are /ready/ to receive new data. We need to compose
-- /h'/ with the 'lockStep' and 'stepLock' functions to achieve the /lock-step/
-- operation.
--
-- @
-- f \`@'seqDF'@\` ('lockStep' \`@'seqDF'@\` 'loopDF' d4 Nil h' \`@'seqDF'@\` 'stepLock') \`@'seqDF'@\` g
-- @
--
-- <<doc/loopDF_sync.svg>>
loopDF :: (KnownNat m, KnownNat n, KnownNat addrSize, KnownNat rate
          ,KnownSymbol nm,(m+n) ~ (2^addrSize))
       => SNat (m + n) -- ^ Depth of the FIFO buffer. Must be a power of two
       -> Vec m d -- ^ Initial content of the FIFO buffer. Can be smaller than
                  -- the size of the FIFO. Empty spaces are initialised with
                  -- 'undefined'.
       -> DataFlow' ('Clk nm rate) (Bool,Bool) (Bool,Bool) (a,d) (b,d)
       -> DataFlow' ('Clk nm rate) Bool Bool   a           b
loopDF sz is (DF f) =
  DF (\a aV bR -> let (bd,bdV,adR) = f ad adV bdR
                      (b,d)        = unbundle bd
                      (bV,dV)      = unbundle bdV
                      (aR,dR)      = unbundle adR
                      (d_buf,dV_buf,dR_buf) = df (fifoDF sz is) d dV dR

                      ad  = bundle (a,d_buf)
                      adV = bundle (aV,dV_buf)
                      bdR = bundle (bR,dR_buf)
                  in  (b,bV,aR)
     )

-- | Feed back the second halve of the communication channel. Unlike 'loopDF',
-- the feedback loop is /not/ buffered.
loopDF_nobuf :: DataFlow' clk (Bool,Bool) (Bool,Bool) (a,d) (b,d)
             -> DataFlow' clk Bool Bool   a           b
loopDF_nobuf (DF f) = DF (\a aV bR -> let (bd,bdV,adR) = f ad adV bdR
                                          (b,d)        = unbundle bd
                                          (bV,dV)      = unbundle bdV
                                          (aR,dR)      = unbundle adR
                                          ad           = bundle (a,d)
                                          adV          = bundle (aV,dV)
                                          bdR          = bundle (bR,dR)
                                      in  (b,bV,aR)
                         )

-- | Reduce or extend the synchronisation granularity of parallel compositions.
class LockStep a b where
  -- | Reduce the synchronisation granularity to a single 'Bool'ean value.
  --
  -- Given:
  --
  -- @
  -- __f__ :: 'DataFlow' Bool Bool a b
  -- __g__ :: 'DataFlow' Bool Bool c d
  -- __h__ :: 'DataFlow' Bool Bool (b,d) (p,q)
  -- @
  --
  -- We /cannot/ simply write:
  --
  -- @
  -- (f \`@'parDF'@\` g) \`@'seqDF'@\` h
  -- @
  --
  -- because, @f \`parDF\` g@, has type, @'DataFlow' (Bool,Bool) (Bool,Bool) (a,c) (b,d)@,
  -- which does not match the expected synchronisation granularity of @h@. We
  -- need a circuit in between that has the type:
  --
  -- @
  -- 'DataFlow' (Bool,Bool) Bool (b,d) (b,d)
  -- @
  --
  -- Simply '&&'-ing the /valid/ signals in the forward direction, and
  -- duplicating the /ready/ signal in the backward direction is however not
  -- enough. We also need to make sure that @f@ does not update its output when
  -- @g@'s output is invalid and visa versa, as @h@ can only consume its input
  -- when both @f@ and @g@ are producing valid data. @g@'s /ready/ port is hence
  -- only asserted when @h@ is ready and @f@ is producing /valid/ data. And @f@'s
  -- ready port is only asserted when @h@ is ready and @g@ is producing valid
  -- data. @f@ and @g@ will hence be proceeding in /lock-step/.
  --
  -- The 'lockStep' function ensures that all synchronisation signals are
  -- properly connected:
  --
  -- @
  -- (f \`@'parDF'@\` g) \`@'seqDF'@\` 'lockStep' \`@'seqDF'@\` h
  -- @
  --
  -- <<doc/lockStep.svg>>
  --
  -- __Note 1__: ensure that the components that you are synchronising have
  -- buffered/delayed @ready@ and @valid@ signals, or 'lockStep' has the
  -- potential to introduce combinational loops. You can do this by placing
  -- 'fifoDF's on the parallel channels. Extending the above example, you would
  -- write:
  --
  -- @
  -- ((f \`@'seqDF'@\` 'fifoDF' d4 Nil) \`@'parDF'@\` (g \`@'seqDF'@\` 'fifoDF' d4 Nil)) \`@'seqDF'@\` 'lockStep' \`@'seqDF'@\` h
  -- @
  --
  -- __Note 2__: 'lockStep' works for arbitrarily nested tuples. That is:
  --
  -- @
  -- p :: 'DataFlow' Bool Bool ((b,d),d) z
  --
  -- q :: 'DataFlow' ((Bool,Bool),Bool) ((Bool,Bool),Bool) ((a,c),c) ((b,d),d)
  -- q = f \`@'parDF'@\` g \`@'parDF'@\` g
  --
  -- r = q \`@'seqDF'@\` 'lockStep' \`@'seqDF'@\` p
  -- @
  --
  -- Does the right thing.
  lockStep :: DataFlow' clk a Bool b b

  -- | Extend the synchronisation granularity from a single 'Bool'ean value.
  --
  -- Given:
  --
  -- @
  -- __f__ :: 'DataFlow' Bool Bool a b
  -- __g__ :: 'DataFlow' Bool Bool c d
  -- __h__ :: 'DataFlow' Bool Bool (p,q) (a,c)
  -- @
  --
  -- We /cannot/ simply write:
  --
  -- @
  -- h \`@'seqDF'@\` (f \`@'parDF'@\` g)
  -- @
  --
  -- because, @f \`parDF\` g@, has type, @'DataFlow' (Bool,Bool) (Bool,Bool) (a,c) (b,d)@,
  -- which does not match the expected synchronisation granularity of @h@. We
  -- need a circuit in between that has the type:
  --
  -- @
  -- 'DataFlow' Bool (Bool,Bool) (a,c) (a,c)
  -- @
  --
  -- Simply '&&'-ing the /ready/ signals in the backward direction, and
  -- duplicating the /valid/ signal in the forward direction is however not
  -- enough. We need to make sure that @f@ does not consume values when @g@ is
  -- not /ready/ and visa versa, because @h@ cannot update the values of its
  -- output tuple independently. @f@'s /valid/ port is hence only asserted when
  -- @h@ is valid and @g@ is ready to receive new values. @g@'s /valid/ port is
  -- only asserted when @h@ is valid and @f@ is ready to receive new values.
  -- @f@ and @g@ will hence be proceeding in /lock-step/.
  --
  -- The 'stepLock' function ensures that all synchronisation signals are
  -- properly connected:
  --
  -- @
  -- h \`@'seqDF'@\` 'stepLock' \`@'seqDF'@\` (f \`@'parDF'@\` g)
  -- @
  --
  -- <<doc/stepLock.svg>>
  --
  -- __Note 1__: ensure that the components that you are synchronising have
  -- buffered/delayed @ready@ and @valid@ signals, or 'stepLock' has the
  -- potential to introduce combinational loops. You can do this by placing
  -- 'fifoDF's on the parallel channels. Extending the above example, you would
  -- write:
  --
  -- @
  -- h \`@'seqDF'@\` 'stepLock' \`@'seqDF'@\` ((`fifoDF` d4 Nil \`@'seqDF'@\` f) \`@'parDF'@\` (`fifoDF` d4 Nil \`@'seqDF'@\` g))
  -- @
  --
  -- __Note 2__: 'stepLock' works for arbitrarily nested tuples. That is:
  --
  -- @
  -- p :: 'DataFlow' Bool Bool z ((a,c),c)
  --
  -- q :: 'DataFlow' ((Bool,Bool),Bool) ((Bool,Bool),Bool) ((a,c),c) ((b,d),d)
  -- q = f \`@'parDF'@\` g \`@'parDF'@\` g
  --
  -- r = p \`@'seqDF'@\` 'stepLock' \`@'seqDF'@\` q
  -- @
  --
  -- Does the right thing.
  stepLock :: DataFlow' clk Bool a b b

instance LockStep Bool c where
  lockStep = idDF
  stepLock = idDF

instance (LockStep a x, LockStep b y) => LockStep (a,b) (x,y) where
  lockStep = (lockStep `parDF` lockStep) `seqDF`
                (DF (\xy xyV rdy -> let (xV,yV)   = unbundle xyV
                                        val       = xV .&&. yV
                                        xR        = yV .&&. rdy
                                        yR        = xV .&&. rdy
                                        xyR       = bundle (xR,yR)
                                    in  (xy,val,xyR)))

  stepLock = (DF (\xy val xyR -> let (xR,yR) = unbundle xyR
                                     rdy     = xR  .&&. yR
                                     xV      = val .&&. yR
                                     yV      = val .&&. xR
                                     xyV     = bundle (xV,yV)
                                 in  (xy,xyV,rdy))) `seqDF` (stepLock `parDF` stepLock)

instance (LockStep en a, m ~ (n + 1), KnownNat m, KnownNat n) =>
  LockStep (Vec m en) (Vec m a) where
  lockStep = parNDF (repeat lockStep) `seqDF`
    DF (\xs vals rdy ->
          let val  = and <$> vals
              rdys = allReady <$> rdy <*> (repeat <$> vals)
          in  (xs,val,rdys)
       )
  stepLock =
    DF (\xs val rdys ->
          let rdy  = and <$> rdys
              vals = allReady <$> val <*> (repeat <$> rdys)
          in  (xs,vals,rdy)
       ) `seqDF` parNDF (repeat stepLock)

allReady :: forall n . KnownNat n
         => Bool -> Vec (n+1) (Vec (n+1) Bool)
         -> Vec (n+1) Bool
allReady b vs = case addSNat (SNat @ n) (SNat @ 1) of
  SNat -> map (and . (b :>) . tail) (smap (flip rotateLeftS) vs)
