{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module CLaSH.Driver.TopWrapper where

import           Data.Aeson           (FromJSON (..), Value (..), (.:), (.:?),
                                       (.!=))
import           Data.Aeson.Extra     (decodeAndReport)
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict  as H
import qualified Data.HashMap.Lazy    as HashMap
import           Data.List            (mapAccumL)
import           Data.Text.Lazy       (Text, append, pack)
import           System.Directory     (doesFileExist)
import           System.IO.Unsafe     (unsafePerformIO)

import CLaSH.Netlist                  (runNetlistMonad)
import CLaSH.Netlist.BlackBox         (prepareBlackBox)
import CLaSH.Netlist.Types            (BlackBoxContext (..), Component (..),
                                       Declaration (..), Expr (..), Identifier,
                                       HWType (..), Modifier (..), NetlistMonad,
                                       emptyBBContext)
import CLaSH.Primitives.Types         (PrimMap, Primitive (..))
import CLaSH.Util

-- | TopEntity specifications, fields are self-explanatory
data TopEntity
  = TopEntity
  { t_name     :: Text
  , t_inputs   :: [Text]        -- optional
  , t_outputs  :: [Text]        -- optional
  , t_extraIn  :: [(Text,Int)]  -- optional
  , t_extraOut :: [(Text,Int)]  -- optional
  , t_clocks   :: [ClockSource] -- optional
  }
  deriving Show

-- | A clock source
data ClockSource
  = ClockSource
  { c_name  :: Text              -- ^ Component name
  , c_paths :: [ClockPath]       -- ^ Number of clock paths
  , c_reset :: Maybe (Text,Text) -- ^ optional: Asynchronous reset input
  , c_lock  :: Text              -- ^ Port name that indicates clock is stable
  , c_sync  :: Bool              -- ^ optional: devices connected this clock
                                 -- source should be pulled out of reset in-sync
  }
  deriving Show

-- | A clock path
data ClockPath
  = ClockPath
  { cp_inp   :: Maybe (Text,Text) -- optional: (Input port, clock pin)
  , cp_outp  :: [(Text,Clock)]    -- [(output port,clock signal)]
  }
  deriving Show

-- | A clock
data Clock
  = Clk { clk_name :: Text, clk_rate :: Int }
  deriving (Eq,Show)

instance FromJSON TopEntity where
  parseJSON (Object v) = case H.toList v of
    [(conKey,Object conVal)] -> case conKey of
      "TopEntity"  -> TopEntity <$> conVal .: "name"
                                <*> (conVal .:? "inputs" .!= [])
                                <*> (conVal .:? "outputs" .!= [])
                                <*> (conVal .:? "extra_in" .!= [])
                                <*> (conVal .:? "extra_out" .!= [])
                                <*> (conVal .:? "clocks" .!= [])
      _ -> error "Expected: TopEntity"
    _ -> error "Expected: TopEntity object"
  parseJSON _ = error "Expected: TopEntity object"

instance FromJSON ClockSource where
  parseJSON (Object v) = case H.toList v of
    [(conKey,Object conVal)] -> case conKey of
      "Source" -> ClockSource <$> conVal .: "name" <*> conVal .: "paths"
                              <*> conVal .:? "reset" <*> conVal .: "lock"
                              <*> (conVal .:? "sync" .!= False)
      _ -> error "Expected: Source"
    _ -> error "Expected: Source object"
  parseJSON _ = error "Expected: Source object"

instance FromJSON ClockPath where
  parseJSON (Object v) = case H.toList v of
    [(conKey,Object conVal)] -> case conKey of
      "Path" -> ClockPath <$> conVal .:? "inp" <*> conVal .: "outp"
      _ -> error "Expected: Path"
    _ -> error "Expected: Path object"
  parseJSON _ = error "Expected: Path object"

instance FromJSON Clock where
  parseJSON (Object v) = case H.toList v of
    [(conKey,Object conVal)] -> case conKey of
      "Clk" -> Clk <$> conVal .: "name" <*> conVal .: "rate"
      _ -> error "Expected: Clk"
    _ -> error "Expected: Clk object"
  parseJSON (String "System") = pure (Clk "system" 1000)
  parseJSON _ = error "Expected: System, or, Clk object"

-- | Create a 'TopEntity' data type from the JSON encoded @.topentity@ file.
generateTopEnt :: String
               -> IO (Maybe TopEntity)
generateTopEnt modName = do
  let topEntityFile = modName ++ ".topentity"
  exists <- doesFileExist topEntityFile
  if exists
    then return . decodeAndReport <=< B.readFile $ topEntityFile
    else return Nothing

-- | Create a wrapper around a component, potentially initiating clock sources
mkTopWrapper :: PrimMap
             -> Maybe TopEntity -- ^ TopEntity specifications
             -> Component       -- ^ Entity to wrap
             -> Component
mkTopWrapper primMap teM topComponent
  = Component
  { componentName = maybe "topEntity" t_name teM
  , inputs        = inputs'' ++ extraIn teM
  , outputs       = outputs'' ++ extraOut teM
  , hiddenPorts   = case maybe [] t_clocks teM of
                      [] -> originalHidden
                      _  -> []
  , declarations  = concat [ mkClocks primMap originalHidden teM
                           , wrappers
                           , instDecl:unwrappers
                           ]
  }
  where
    iNameSupply                = maybe [] t_inputs teM
    originalHidden             = hiddenPorts topComponent

    inputs'                    = map (first (const "input"))
                                     (inputs topComponent)
    (inputs'',(wrappers,idsI)) = (concat *** (first concat . unzip))
                               . unzip
                               . snd
                               $ mapAccumL (\nm (i,c) -> mkInput nm i c)
                                            iNameSupply
                                            (zip inputs' [0..])

    oNameSupply                   = maybe [] t_outputs teM
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

-- | Create extra input ports for the wrapper
extraIn :: Maybe TopEntity -> [(Identifier,HWType)]
extraIn = maybe [] ((map (second BitVector)) . t_extraIn)

-- | Create extra output ports for the wrapper
extraOut :: Maybe TopEntity -> [(Identifier,HWType)]
extraOut = maybe [] ((map (second BitVector)) . t_extraOut)

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
                                 (Identifier iName (Just (Indexed (hwty,1,n)))))
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
mkClocks :: PrimMap -> [(Identifier,HWType)] -> Maybe TopEntity -> [Declaration]
mkClocks primMap hidden teM = concat
    [ hiddenSigDecs
    , clockGens
    , resets
    ]
  where
    hiddenSigDecs        = map (uncurry NetDecl) hidden
    (clockGens,clkLocks) = maybe ([],[])
                                 (first concat . unzip . map mkClock . t_clocks)
                                 teM
    resets               = mkResets primMap hidden clkLocks

-- | Create a single clock generator
mkClock :: ClockSource -> ([Declaration],(Identifier,[Clock],Bool))
mkClock (ClockSource {..}) = ([lockedDecl,instDecl],(lockedName,clks,c_sync))
  where
    lockedName   = append c_name "_locked"
    lockedDecl   = NetDecl lockedName (Reset lockedName 0)
    (ports,clks) = (concat *** concat) . unzip $ map clockPorts c_paths
    instDecl     = InstDecl c_name (append c_name "_inst")
                 $ concat [ ports
                          , maybe [] ((:[]) . second (`Identifier` Nothing))
                                  c_reset
                          , [(c_lock,Identifier lockedName Nothing)]
                          ]

-- | Create a single clock path
clockPorts :: ClockPath -> ([(Identifier,Expr)],[Clock])
clockPorts (ClockPath {..}) = (inp ++ outp,clks)
  where
    inp  = maybe [] ((:[]) . second (`Identifier` Nothing)) cp_inp
    outp = map (second ((`Identifier` Nothing) . clkToId)) cp_outp
    clks = map snd cp_outp

    clkToId (Clk nm r) = append nm (pack (show r))

-- | Generate resets
mkResets :: PrimMap
         -> [(Identifier,HWType)]
         -> [(Identifier,[Clock],Bool)]
         -> [Declaration]
mkResets primMap hidden = unsafeRunNetlist . fmap concat . mapM assingReset
  where
    assingReset (lock,clks,doSync) = concat <$> mapM connectReset matched
      where
        matched = filter match hidden
        match (_,(Reset nm r)) = elem (Clk nm r) clks
        match _                = False

        connectReset (rst,(Reset nm r)) = if doSync
            then return [Assignment rst (Identifier lock Nothing)]
            else genSyncReset primMap lock rst (Clk nm r)
        connectReset _ = return []

-- | Generate a reset synchroniser that synchronously de-asserts an
-- asynchronous reset signal
genSyncReset :: PrimMap
             -> Identifier
             -> Identifier
             -> Clock
             -> NetlistMonad [Declaration]
genSyncReset primMap lock rst (Clk nm r) = do
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

  return [resetGenDecl]

-- | The 'NetListMonad' is an transformer stack with 'IO' at the bottom.
-- So we must use 'unsafePerformIO'.
unsafeRunNetlist :: NetlistMonad a
                 -> a
unsafeRunNetlist = unsafePerformIO
                 . fmap fst
                 . runNetlistMonad Nothing HashMap.empty HashMap.empty
                     HashMap.empty (\_ _ -> Nothing)
