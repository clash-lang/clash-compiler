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

import           Data.List            (mapAccumL)
import           Data.Text.Lazy       (append, pack)

import CLaSH.Annotations.TopEntity    (TopEntity (..))

import CLaSH.Netlist.Types
  (Component (..), Declaration (..), Expr (..), Identifier, HWType (..),
   Modifier (..), PortDirection(..))
import CLaSH.Util

-- | Create a wrapper around a component, potentially initiating clock sources
mkTopWrapper :: (Identifier -> Identifier)
             -> Maybe TopEntity -- ^ TopEntity specifications
             -> String          -- ^ Name of the module containing the @topEntity@
             -> Component       -- ^ Entity to wrap
             -> Component
mkTopWrapper mkId teM modName topComponent
  = Component
  { componentName = maybe (mkId (pack modName `append` "_topEntity")) (pack . t_name) teM
  , inputs        = inputs'' ++ extraIn teM
  , outputs       = outputs'' ++ extraOut teM
  , hiddenPorts   = originalHidden
  , declarations  = concat [ wrappers
                           , instDecl:unwrappers
                           ]
  }
  where
    iNameSupply                = maybe [] (map pack . t_inputs) teM
    originalHidden             = hiddenPorts topComponent

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
                        (zipWith (\(p,t) i -> (p,In,t,Identifier i Nothing))
                                 (inputs topComponent)
                                 idsI
                         ++
                         map (\(p,t) -> (p,In,t,Identifier p Nothing))
                             (hiddenPorts topComponent)
                         ++
                         zipWith (\(p,t) i -> (p,Out,t,Identifier i Nothing))
                                 (outputs topComponent)
                                 idsO)

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
  RTree d hwty' ->
    let (nms',(ports',(decls',ids)))
                 = second ( (concat *** (first concat . unzip))
                          . unzip
                          )
                 $ mapAccumL
                    (\nm c -> mkInput nm (iName,hwty') c)
                    nms [0..((2^d)-1)]
        netdecl  = NetDecl iName hwty
        netassgn = Assignment iName (mkRTreeChain d hwty' ids)
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

-- | Create a RTree chain for a list of 'Identifier's
mkRTreeChain :: Int
             -> HWType
             -> [Identifier]
             -> Expr
mkRTreeChain _ elTy [i] = DataCon (RTree 0 elTy) RTreeAppend
                                  [Identifier i Nothing]
mkRTreeChain d elTy is =
  let (isL,isR) = splitAt (length is `div` 2) is
  in  DataCon (RTree d elTy) RTreeAppend
        [ mkRTreeChain (d-1) elTy isL
        , mkRTreeChain (d-1) elTy isR
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
  RTree d hwty' ->
    let (nms',(ports',(decls',ids)))
                 = second ( (concat *** (first concat . unzip))
                          . unzip
                          )
                 $ mapAccumL
                    (\nm c -> mkOutput nm (iName,hwty') c)
                    nms [0..((2^d)-1)]
        netdecl  = NetDecl iName hwty
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

stringToVar :: String -> Expr
stringToVar = (`Identifier` Nothing) . pack

