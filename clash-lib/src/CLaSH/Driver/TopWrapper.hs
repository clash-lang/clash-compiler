{-|
  Copyright  :  (C) 2015-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module CLaSH.Driver.TopWrapper where

import           Data.Char            (isDigit)
import qualified Data.HashMap.Lazy    as HashMap
import           Data.List            (mapAccumL)
import           Data.Maybe           (mapMaybe)
import           Data.Text.Lazy       (Text, append, pack, unpack)
import           System.IO.Unsafe     (unsafePerformIO)

import CLaSH.Annotations.TopEntity    (TopEntity (..), ClockSource (..))

import CLaSH.Netlist                  (runNetlistMonad)
import CLaSH.Netlist.BlackBox         (prepareBlackBox)
import CLaSH.Netlist.BlackBox.Types   (BlackBoxTemplate)
import CLaSH.Netlist.Types            (BlackBoxContext (..), Component (..),
                                       Declaration (..), Expr (..), Identifier,
                                       HWType (..), Modifier (..), NetlistMonad,
                                       emptyBBContext)
import CLaSH.Primitives.Types         (PrimMap, Primitive (..))
import CLaSH.Util

-- | Create a wrapper around a component, potentially initiating clock sources
mkTopWrapper :: PrimMap BlackBoxTemplate
             -> Maybe TopEntity -- ^ TopEntity specifications
             -> String          -- ^ Name of the module containing the @topEntity@
             -> Int             -- ^ Int/Word/Integer bit-width
             -> Component       -- ^ Entity to wrap
             -> Component
mkTopWrapper primMap teM modName iw topComponent
  = Component
  { componentName = maybe (pack modName `append` "_topEntity") (pack . t_name) teM
  , inputs        = inputs'' ++ extraIn teM
  , outputs       = outputs'' ++ extraOut teM
  , hiddenPorts   = case maybe [] t_clocks teM of
                      [] -> originalHidden
                      _  -> filter (`notElem` (mapMaybe isNetDecl clkDecls))
                                   originalHidden
  , declarations  = concat [ clkDecls
                           , wrappers
                           , instDecl:unwrappers
                           ]
  }
  where
    iNameSupply                = maybe [] (map pack . t_inputs) teM
    originalHidden             = hiddenPorts topComponent

    clkDecls                   = mkClocks primMap originalHidden iw teM

    inputs'                    = map (first (const "input"))
                                     (inputs topComponent)
    (inputs'',(wrappers,idsI)) = (concat *** (first concat . unzip))
                               . unzip
                               . snd
                               $ mapAccumL (\nm (i,c) -> mkInput nm i c)
                                            iNameSupply
                                            (zip inputs' [0..])

    oNameSupply                   = maybe [] (map pack . t_outputs) teM
    outputs'                      = map (first (const "output"))
                                        (outputs topComponent)
    (outputs'',(unwrappers,idsO)) = (concat *** (first concat . unzip))
                                  . unzip
                                  . snd
                                  $ mapAccumL (\nm (o,c) -> mkOutput nm o c)
                                              oNameSupply
                                              (zip outputs' [0..])

    instDecl = InstDecl (componentName topComponent)
                        (append (componentName topComponent) (pack "_inst"))
                        (zipWith (\(p,_) i -> (p,Identifier i Nothing))
                                 (inputs topComponent)
                                 idsI
                         ++
                         map (\(p,_) -> (p,Identifier p Nothing))
                             (hiddenPorts topComponent)
                         ++
                         zipWith (\(p,_) i -> (p,Identifier i Nothing))
                                 (outputs topComponent)
                                 idsO)

    isNetDecl (NetDecl nm ty) = Just (nm,ty)
    isNetDecl _               = Nothing

-- | Create extra input ports for the wrapper
extraIn :: Maybe TopEntity -> [(Identifier,HWType)]
extraIn = maybe [] ((map (pack *** BitVector)) . t_extraIn)

-- | Create extra output ports for the wrapper
extraOut :: Maybe TopEntity -> [(Identifier,HWType)]
extraOut = maybe [] ((map (pack *** BitVector)) . t_extraOut)

-- | Generate input port mappings
mkInput :: [Identifier]
        -> (Identifier,HWType)
        -> Int
        -> ( [Identifier]
           , ( [(Identifier,HWType)]
             , ( [Declaration]
               , Identifier
               )
             )
           )
mkInput nms (i,hwty) cnt = case hwty of
  Vector sz hwty' ->
    let (nms',(ports',(decls',ids)))
                 = second ( (concat *** (first concat . unzip))
                          . unzip
                          )
                 $ mapAccumL
                    (\nm c -> mkInput nm (iName,hwty') c)
                    nms [0..(sz-1)]
        netdecl  = NetDecl iName hwty
        netassgn = Assignment iName (mkVectorChain sz hwty' ids)
    in  (nms',(ports',(netdecl:decls' ++ [netassgn],iName)))
  Product _ hwtys ->
    let (nms',(ports',(decls',ids)))
                 = second ( (concat *** (first concat . unzip))
                          . unzip
                          )
                 $ mapAccumL
                    (\nm (inp,c) -> mkInput nm inp c)
                    nms (zip (map (iName,) hwtys) [0..])
        netdecl  = NetDecl iName hwty
        ids'     = map (`Identifier` Nothing) ids
        netassgn = Assignment iName (DataCon hwty (DC (hwty,0)) ids')
    in  (nms',(ports',(netdecl:decls' ++ [netassgn],iName)))
  _ -> case nms of
         []       -> (nms,([(iName,hwty)],([],iName)))
         (n:nms') -> (nms',([(n,hwty)],([],n)))
  where

    iName = append i (pack ("_" ++ show cnt))

-- | Create a Vector chain for a list of 'Identifier's
mkVectorChain :: Int
              -> HWType
              -> [Identifier]
              -> Expr
mkVectorChain _ elTy []      = DataCon (Vector 0 elTy) VecAppend []
mkVectorChain _ elTy [i]     = DataCon (Vector 1 elTy) VecAppend
                                [Identifier i Nothing]
mkVectorChain sz elTy (i:is) = DataCon (Vector sz elTy) VecAppend
                                [ Identifier i Nothing
                                , mkVectorChain (sz-1) elTy is
                                ]

-- | Generate output port mappings
mkOutput :: [Identifier]
         -> (Identifier,HWType)
         -> Int
         -> ( [Identifier]
            , ( [(Identifier,HWType)]
              , ( [Declaration]
                , Identifier
                )
              )
            )
mkOutput nms (i,hwty) cnt = case hwty of
  Vector sz hwty' ->
    let (nms',(ports',(decls',ids)))
                = second ( (concat *** (first concat . unzip))
                         . unzip
                         )
                $ mapAccumL
                   (\nm c -> mkOutput nm (iName,hwty') c)
                   nms [0..(sz-1)]
        netdecl = NetDecl iName hwty
        assigns = zipWith
                    (\id_ n -> Assignment id_
                                 (Identifier iName (Just (Indexed (hwty,10,n)))))
                    ids
                    [0..]
    in  (nms',(ports',(netdecl:assigns ++ decls',iName)))
  Product _ hwtys ->
    let (nms',(ports',(decls',ids)))
                = second ( (concat *** (first concat . unzip))
                         . unzip
                         )
                $ mapAccumL
                   (\nm (inp,c) -> mkOutput nm inp c)
                   nms (zip (map (iName,) hwtys) [0..])
        netdecl = NetDecl iName hwty
        assigns = zipWith
                    (\id_ n -> Assignment id_
                                (Identifier iName (Just (Indexed (hwty,0,n)))))
                    ids
                    [0..]
    in  (nms',(ports',(netdecl:assigns ++ decls',iName)))
  _ -> case nms of
         []       -> (nms,([(iName,hwty)],([],iName)))
         (n:nms') -> (nms',([(n,hwty)],([],n)))
  where
    iName = append i (pack ("_" ++ show cnt))

-- | Create clock generators
mkClocks :: PrimMap BlackBoxTemplate -> [(Identifier,HWType)] -> Int -> Maybe TopEntity -> [Declaration]
mkClocks primMap hidden iw teM = concat
    [ clockGens
    , resets
    ]
  where
    (clockGens,clkLocks) = maybe ([],[])
                                 (first concat . unzip . map mkClock . t_clocks)
                                 teM
    resets               = mkResets primMap hidden iw clkLocks

stringToVar :: String -> Expr
stringToVar = (`Identifier` Nothing) . pack

-- | Create a single clock generator
mkClock :: ClockSource -> ([Declaration],(Identifier,[String],Bool))
mkClock (ClockSource {..}) = (clkDecls ++ [lockedDecl,instDecl],(lockedName,clks,c_sync))
  where
    c_nameT      = pack c_name
    lockedName   = append c_nameT "_locked"
    lockedDecl   = NetDecl lockedName (Reset lockedName 0)
    (ports,clks) = clockPorts c_inp c_outp
    clkDecls     = map mkClockDecl clks
    instDecl     = InstDecl c_nameT (append c_nameT "_inst")
                 $ concat [ ports
                          , maybe [] ((:[]) . (pack *** stringToVar))
                                  c_reset
                          , [(pack c_lock,Identifier lockedName Nothing)]
                          ]

mkClockDecl :: String -> Declaration
mkClockDecl s = NetDecl (pack s) (Clock (pack name) (read rate))
  where
    (name,rate) = span (not . isDigit) s


-- | Create a single clock path
clockPorts :: [(String,String)] -> [(String,String)]
           -> ([(Identifier,Expr)],[String])
clockPorts inp outp = (ports,clks)
  where
    ports = map (pack *** stringToVar) (inp ++ outp)
    clks  = map snd outp

-- | Generate resets
mkResets :: PrimMap BlackBoxTemplate
         -> [(Identifier,HWType)]
         -> Int
         -> [(Identifier,[String],Bool)]
         -> [Declaration]
mkResets primMap hidden iw = unsafeRunNetlist iw . fmap concat . mapM assingReset
  where
    assingReset (lock,clks,doSync) = concat <$> mapM connectReset matched
      where
        matched = filter match hidden
        match (_,(Reset nm r)) = elem (unpack nm ++ show r) clks
        match _                = False

        connectReset (rst,(Reset nm r)) = if doSync
            then return [NetDecl rst (Reset nm r), Assignment rst (Identifier lock Nothing)]
            else genSyncReset primMap lock rst nm r
        connectReset _ = return []

-- | Generate a reset synchroniser that synchronously de-asserts an
-- asynchronous reset signal
genSyncReset :: PrimMap BlackBoxTemplate
             -> Identifier
             -> Identifier
             -> Text
             -> Int
             -> NetlistMonad [Declaration]
genSyncReset primMap lock rst nm r = do
  let resetType = Reset rst 0
      ctx = emptyBBContext
              { bbResult = (Right ((Identifier rst Nothing),(nm,r)), resetType)
              , bbInputs = [(Left (Identifier lock Nothing),resetType,False)]
              }
      bbName = "CLaSH.TopWrapper.syncReset"
  resetGenDecl <- case HashMap.lookup bbName primMap of
        Just (BlackBox _ (Left templ)) -> do
          templ' <- prepareBlackBox bbName templ ctx
          return (BlackBoxD bbName templ' ctx)
        pM -> error $ $(curLoc) ++ ("Can't make reset sync for: " ++ show pM)

  return [NetDecl rst (Reset nm r),resetGenDecl]

-- | The 'NetListMonad' is a transformer stack with 'IO' at the bottom.
-- So we must use 'unsafePerformIO'.
unsafeRunNetlist :: Int
                 -> NetlistMonad a
                 -> a
unsafeRunNetlist iw
  = unsafePerformIO
  . fmap fst
  . runNetlistMonad Nothing HashMap.empty HashMap.empty
      HashMap.empty (\_ _ -> Nothing) "" [] iw
