module T2628 where

import Clash.Prelude

-- idx cacheline entries are Just(tag,Just addr) to translate idx++tag->addr
--                       and Just(tag,Nothing) for invalidated idx++tag entry
--                       and Nothing for no entry there
type CacheLine m tag addr                    -- 2^m tags per line, 2^n lines
               = Vec (2^m) (Maybe(tag,Maybe addr))

{-# ANN tacache_server_step32
  (Synthesize { t_name   = "TACacheServerStep"
              , t_inputs = [ PortName "dx"        -- user B
                           , PortName "d_x"       -- tlb C
                           , PortName "dw"        -- tlb D
                           , PortName "out2"     -- cache B
                           , PortName "out3"     -- cache C
                           ]
              , t_output = PortProduct ""
                             [ PortName "win1"   -- cache A1
                             , PortName "win2"   -- cache A2
                             ]
              }) #-}

{-# NOINLINE tacache_server_step32 #-}
tacache_server_step32 = tacache_server_step'
  where
  tacache_server_step'
   :: forall (m::Nat) (n::Nat) (p::Nat) (q::Nat)
            cxdr addr idx tag cacheline
   . ( KnownNat q, KnownNat n, KnownNat m, KnownNat p
     , n <= p
     , cxdr ~ Signed p
     , addr ~ Signed q
     , idx  ~ Signed n
     , tag  ~ Signed (p-n)
     , cacheline ~ CacheLine m tag addr
     , p ~ 132
     , q ~ 32
     , n ~ 6
     , m ~ 0
     )
   -- SNat n                         -- 2^n lines
   -- SNat m                         -- of 2^m entries each
   => ( Maybe cxdr        -- input frnt invalidate addr req to server
      , Maybe cxdr        -- input back/weak invalidate req to server
      , Maybe (cxdr,addr) -- input back/weak write req to server
      , Maybe (idx,cacheline)
      , Maybe (idx,cacheline)
      )
   -> ( Maybe(idx,cacheline)
      , Maybe(idx,cacheline)
      )
  tacache_server_step' = tacache_server_step (SNat::SNat n) (SNat::SNat m)

tacache_server_step
   :: forall (m::Nat) (n::Nat) (p::Nat) (q::Nat)
            cxdr addr idx tag cacheline
   . ( KnownNat q, KnownNat n, KnownNat m, KnownNat p
     , n <= p
     , cxdr ~ Signed p
     , addr ~ Signed q
     , idx  ~ Signed n
     , tag  ~ Signed (p-n)
     , cacheline ~ CacheLine m tag addr
--   , p ~ 132
--   , q ~ 32
     )
   => SNat n                         -- 2^n lines
   -> SNat m                         -- of 2^m entries each
   -> ( Maybe cxdr        -- input frnt invalidate addr req to server
      , Maybe cxdr        -- input back/weak invalidate req to server
      , Maybe (cxdr,addr) -- input back/weak write req to server
      , Maybe (idx,cacheline)
      , Maybe (idx,cacheline)
      )
   -> ( Maybe(idx,cacheline)
      , Maybe(idx,cacheline)
      )
tacache_server_step n m (dx,d_x,dw,out1,out2) = (win1,win2)

  where
      -- outs1 and outs2 are prev state
      -- (may need to write two lines in one cycle)
      win1,win2 :: Maybe(idx,CacheLine m tag addr)
      (win1,win2) =
             case (dx, d_x, dw, out1, out2) of

  -- !!! FIX for HDL from here on, replace (v,_) = with v = fst $  !!! --

               (Just x1,Just x2,Nothing,Just (idx1,v1),Just (idx2,v2)) ->
                 let (idx2',tag2) = tacache_split_cxdr x2
                 in
                 if 1 /= idx2' then
                      ( Just(1,v1)
                      , Just(idx2',v2)
                      )
                 else
                   let (v1',_) = tazcache_line_inval_step v1 2          -- HERE
                       (v2',_) = tazcache_line_weak_inval_step v1' tag2 -- HERE
                   in ( Just(idx2',v2')
                      , Nothing
                      )

  -- !!! FIX for HDL from here, as above, and make cases top level fns !!! ---

               (Nothing,Just x,Nothing,_,Just (idx,v)) ->
                 let (v',_) = tazcache_line_weak_inval_step v 4       -- HERE
                 in ( Nothing
                    , Just(3,v')
                    )

               _ -> (Nothing,Nothing)

   --------------------  DUMMY NOINLINE support -----------------------

-- split incoming addr for translation into a cacheline index and tag
{-# NOINLINE tacache_split_cxdr #-}
tacache_split_cxdr
  :: forall (n::Nat) (p::Nat) tag cxdr idx f
   . ( KnownNat n, KnownNat p
     , Resize f  -- might as well be just Signed
     , n <= p, (n + (p-n)) ~ p, ((p-n) + n) ~ p
     , BitPack cxdr, p ~    BitSize cxdr, cxdr ~ f p
     , BitPack idx,  n ~    BitSize idx,  idx  ~ f n
     , BitPack tag, (p-n) ~ BitSize tag,  tag  ~ f (p-n)
     )
  => cxdr
  -> (idx,tag)
tacache_split_cxdr x = (unpack 5, unpack 6)

   ------------------  DUMMY NOINLINE cacheline ops ---------------------

-- remove element with matching tag from cacheline, report position
{-# NOINLINE tazcache_line_inval_step #-}
tazcache_line_inval_step ::
                   ( KnownNat m, KnownNat p_n, KnownNat q
                   , BitPack tag,  p_n ~ BitSize tag,  Eq tag
                   , BitPack addr, q ~   BitSize addr
                   )
                => CacheLine m tag addr
                -> tag
                -> (CacheLine m tag addr, Maybe(Index(2^m)))
tazcache_line_inval_step v tag = (v,Nothing)

-- add placeholder invalidated entry to cacheline, replace entry if was there
{-# NOINLINE tazcache_line_weak_inval_step #-}
tazcache_line_weak_inval_step ::
                   ( KnownNat m, KnownNat p_n, KnownNat q
                   , BitPack tag,  p_n ~ BitSize tag,  Eq tag
                   , BitPack addr, q ~   BitSize addr
                   )
                => CacheLine m tag addr
                -> tag
                -> (CacheLine m tag addr, Maybe(Index(2^m)))
tazcache_line_weak_inval_step v tag = (v,Nothing)
