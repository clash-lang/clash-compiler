{-|
  Copyright  :  (C) 2015-2016, University of Twente,
                         2017, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module CLaSH.Driver.TopWrapper where

import Data.Text.Lazy              (append, pack)

import CLaSH.Annotations.TopEntity (TopEntity (..), PortName (..))
import CLaSH.Netlist.Id            (IdType (..))
import CLaSH.Netlist.Types
  (Component (..), Declaration (..), Expr (..), Identifier, HWType (..),
   Modifier (..), PortDirection(..), WireOrReg (..))
import CLaSH.Util

-- | Create a wrapper around a component, potentially initiating clock sources
mkTopWrapper :: (IdType -> Identifier -> Identifier)
             -> Maybe TopEntity -- ^ TopEntity specifications
             -> String          -- ^ Name of the module containing the @topEntity@
             -> Component       -- ^ Entity to wrap
             -> Component
mkTopWrapper mkId teM modName topComponent
  = Component
  { componentName = maybe (mkId Basic (pack modName `append` "_topEntity")) (pack . t_name) teM
  , inputs        = inputs3
  , outputs       = zip (repeat Wire) outputs3
  , declarations  = concat [wrappers,topCompDecl:unwrappers]
  }
  where
    -- input ports
    iPortSupply    = maybe (repeat Nothing)
                           (extendPorts . t_inputs)
                           teM
    inputs1        = map (first (const "input"))
                         (inputs topComponent)
    inputs2        = zipWith mkInput iPortSupply
                             (zipWith appendNumber inputs1 [0..])
    (inputs3,wrappers,idsI) = concatPortDecls inputs2

    -- output ports
    oTop           = map snd (outputs topComponent)
    oPortSupply    = maybe (repeat Nothing)
                           ((++ repeat Nothing) . map Just . t_outputs)
                           teM
    outputs1       = map (first (const "output")) oTop
    outputs2       = zipWith mkOutput oPortSupply
                             (zipWith appendNumber outputs1 [0..])
    (outputs3,unwrappers,idsO) = concatPortDecls outputs2

    -- instantiate the top-level component
    topCompDecl =
      InstDecl (componentName topComponent)
               (componentName topComponent `append` "_inst")
               (zipWith (\(p,t) i -> (Identifier p Nothing,In,t,Identifier i Nothing))
                        (inputs topComponent)
                        idsI
                ++
                zipWith (\(p,t) o -> (Identifier p Nothing,Out,t,Identifier o Nothing))
                        oTop
                        idsO)

extendPorts :: [PortName] -> [Maybe PortName]
extendPorts ps = map Just ps ++ repeat Nothing

concatPortDecls
  :: [([(Identifier,HWType)],[Declaration],Identifier)]
  -> ([(Identifier,HWType)],[Declaration],[Identifier])
concatPortDecls portDecls = case unzip3 portDecls of
  (ps,decls,ids) -> (concat ps, concat decls, ids)

appendNumber
  :: (Identifier,HWType)
  -> Int
  -> (Identifier,HWType)
appendNumber (nm,hwty) i =
  (nm `append` "_" `append` pack (show i),hwty)

portName
  :: String
  -> Identifier
  -> Identifier
portName [] i = i
portName x  _ = pack x

mkInput
  :: Maybe PortName
  -> (Identifier,HWType)
  -> ([(Identifier,HWType)],[Declaration],Identifier)
mkInput pM = case pM of
  Nothing -> go
  Just p  -> go' p
  where
    go (i,hwty) = case hwty of
      Vector sz hwty' -> (ports,netdecl:netassgn:decls,i)
        where
          inputs1  = map (appendNumber (i,hwty')) [0..(sz-1)]
          inputs2  = map (mkInput Nothing) inputs1
          (ports,decls,ids) = concatPortDecls inputs2
          netdecl  = NetDecl Nothing i hwty
          ids'     = map (`Identifier` Nothing) ids
          netassgn = Assignment i (mkVectorChain sz hwty' ids')

      RTree d hwty' -> (ports,netdecl:netassgn:decls,i)
        where
          inputs1  = map (appendNumber (i,hwty')) [0..((2^d)-1)]
          inputs2  = map (mkInput Nothing) inputs1
          (ports,decls,ids) = concatPortDecls inputs2
          ids'     = map (`Identifier` Nothing) ids
          netdecl  = NetDecl Nothing i hwty
          netassgn = Assignment i (mkRTreeChain d hwty' ids')

      Product _ hwtys -> (ports,netdecl:netassgn:decls,i)
        where
          inputs1  = zipWith appendNumber (map (i,) hwtys) [0..]
          inputs2  = map (mkInput Nothing) inputs1
          (ports,decls,ids) = concatPortDecls inputs2
          ids'     = map (`Identifier` Nothing) ids
          netdecl  = NetDecl Nothing i hwty
          netassgn = Assignment i (DataCon hwty (DC (hwty,0)) ids')

      _ -> ([(i,hwty)],[],i)

    go' (PortName p)     (i,hwty) = let pN = portName p i in ([(pN,hwty)],[],pN)
    go' (PortField p ps) (i,hwty) = let pN = portName p i in case hwty of
      Vector sz hwty' -> (ports,netdecl:netassgn:decls,pN)
        where
          inputs1  = map (appendNumber (pN,hwty')) [0..(sz-1)]
          inputs2  = zipWith mkInput (extendPorts ps) inputs1
          (ports,decls,ids) = concatPortDecls inputs2
          netdecl  = NetDecl Nothing pN hwty
          ids'     = map (`Identifier` Nothing) ids
          netassgn = Assignment pN (mkVectorChain sz hwty' ids')

      RTree d hwty' -> (ports,netdecl:netassgn:decls,pN)
        where
          inputs1  = map (appendNumber (pN,hwty')) [0..((2^d)-1)]
          inputs2  = zipWith mkInput (extendPorts ps) inputs1
          (ports,decls,ids) = concatPortDecls inputs2
          netdecl  = NetDecl Nothing pN hwty
          ids'     = map (`Identifier` Nothing) ids
          netassgn = Assignment pN (mkRTreeChain d hwty' ids')

      Product _ hwtys -> (ports,netdecl:netassgn:decls,pN)
        where
          inputs1  = zipWith appendNumber (map (pN,) hwtys) [0..]
          inputs2  = zipWith mkInput (extendPorts ps) inputs1
          (ports,decls,ids) = concatPortDecls inputs2
          ids'     = map (`Identifier` Nothing) ids
          netdecl  = NetDecl Nothing pN hwty
          netassgn = Assignment pN (DataCon hwty (DC (hwty,0)) ids')

      _ -> ([(pN,hwty)],[],pN)


-- | Create a Vector chain for a list of 'Identifier's
mkVectorChain :: Int
              -> HWType
              -> [Expr]
              -> Expr
mkVectorChain _ elTy []      = DataCon (Vector 0 elTy) VecAppend []
mkVectorChain _ elTy [e]     = DataCon (Vector 1 elTy) VecAppend
                                [e]
mkVectorChain sz elTy (e:es) = DataCon (Vector sz elTy) VecAppend
                                [ e
                                , mkVectorChain (sz-1) elTy es
                                ]

-- | Create a RTree chain for a list of 'Identifier's
mkRTreeChain :: Int
             -> HWType
             -> [Expr]
             -> Expr
mkRTreeChain _ elTy [e] = DataCon (RTree 0 elTy) RTreeAppend
                                  [e]
mkRTreeChain d elTy es =
  let (esL,esR) = splitAt (length es `div` 2) es
  in  DataCon (RTree d elTy) RTreeAppend
        [ mkRTreeChain (d-1) elTy esL
        , mkRTreeChain (d-1) elTy esR
        ]

-- | Generate output port mappings
mkOutput
  :: Maybe PortName
  -> (Identifier,HWType)
  -> ([(Identifier,HWType)],[Declaration],Identifier)
mkOutput pM = case pM of
  Nothing -> go
  Just p  -> go' p
  where
    go (o,hwty) = case hwty of
      Vector sz hwty' -> (ports,netdecl:assigns ++ decls,o)
        where
          outputs1 = map (appendNumber (o,hwty')) [0..(sz-1)]
          outputs2 = map (mkOutput Nothing) outputs1
          (ports,decls,ids) = concatPortDecls outputs2
          netdecl  = NetDecl Nothing o hwty
          assigns  = zipWith (assingId o hwty 10) ids [0..]

      RTree d hwty' -> (ports,netdecl:assigns ++ decls,o)
        where
          outputs1 = map (appendNumber (o,hwty')) [0..((2^d)-1)]
          outputs2 = map (mkOutput Nothing) outputs1
          (ports,decls,ids) = concatPortDecls outputs2
          netdecl  = NetDecl Nothing o hwty
          assigns  = zipWith (assingId o hwty 10) ids [0..]

      Product _ hwtys -> (ports,netdecl:assigns ++ decls,o)
        where
          outputs1 = zipWith appendNumber (map (o,) hwtys) [0..]
          outputs2 = map (mkOutput Nothing) outputs1
          (ports,decls,ids) = concatPortDecls outputs2
          netdecl  = NetDecl Nothing o hwty
          assigns  = zipWith (assingId o hwty 0) ids [0..]

      _ -> ([(o,hwty)],[],o)

    go' (PortName p)     (i,hwty) = let pN = portName p i in ([(pN,hwty)],[],pN)
    go' (PortField p ps) (i,hwty) = let pN = portName p i in case hwty of
      Vector sz hwty' -> (ports,netdecl:assigns ++ decls,pN)
        where
          outputs1 = map (appendNumber (pN,hwty')) [0..(sz-1)]
          outputs2 = zipWith mkOutput (extendPorts ps) outputs1
          (ports,decls,ids) = concatPortDecls outputs2
          netdecl  = NetDecl Nothing pN hwty
          assigns  = zipWith (assingId pN hwty 10) ids [0..]

      RTree d hwty' -> (ports,netdecl:assigns ++ decls,pN)
        where
          outputs1 = map (appendNumber (pN,hwty')) [0..((2^d)-1)]
          outputs2 = zipWith mkOutput (extendPorts ps) outputs1
          (ports,decls,ids) = concatPortDecls outputs2
          netdecl  = NetDecl Nothing pN hwty
          assigns  = zipWith (assingId pN hwty 10) ids [0..]

      Product _ hwtys -> (ports,netdecl:assigns ++ decls,pN)
        where
          outputs1 = zipWith appendNumber (map (pN,) hwtys) [0..]
          outputs2 = zipWith mkOutput (extendPorts ps) outputs1
          (ports,decls,ids) = concatPortDecls outputs2
          netdecl  = NetDecl Nothing pN hwty
          assigns  = zipWith (assingId pN hwty 0) ids [0..]

      _ -> ([(pN,hwty)],[],pN)

    assingId p hwty con i n =
      Assignment i (Identifier p (Just (Indexed (hwty,con,n))))
