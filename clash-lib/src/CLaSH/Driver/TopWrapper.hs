{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module CLaSH.Driver.TopWrapper where

import Data.Text.Lazy      (append, pack)

import CLaSH.Netlist.Types (Component (..), Declaration (..), Expr (..), Identifier, HWType (..), Modifier (..))
import CLaSH.Util

mkTopWrapper :: Component -> Component
mkTopWrapper topComponent
  = topComponent
  { componentName = "topEntity"
  , inputs        = inputs''
  , outputs       = outputs''
  , declarations  = wrappers ++ instDecl:unwrappers
  }
  where
    inputs'                    = map (first (const "input"))
                                     (inputs topComponent)
    (inputs'',(wrappers,idsI)) = (concat *** (first concat . unzip))
                                . unzip
                                $ zipWith mkInput inputs' [0..]

    outputs'                       = map (first (const "output"))
                                         (outputs topComponent)
    (outputs'',(unwrappers,idsO)) = (concat *** (first concat . unzip))
                                   . unzip
                                   $ zipWith mkOutput outputs' [0..]

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

mkInput :: (Identifier,HWType)
        -> Int
        -> ( [(Identifier,HWType)]
           , ( [Declaration]
             , Identifier
             )
           )
mkInput (i,hwty) cnt = case hwty of
  Vector sz hwty' ->
    let (ports',(decls',ids)) = (concat *** (first concat . unzip))
                              . unzip
                              $ map (mkInput (iName,hwty')) [0..(sz-1)]
        netdecl  = NetDecl iName hwty
        netassgn = Assignment iName (mkVectorChain sz hwty' ids)
    in  (ports',(netdecl:decls' ++ [netassgn],iName))
  Product _ hwtys ->
    let (ports',(decls',ids)) = (concat *** (first concat . unzip))
                              . unzip
                              $ zipWith mkInput (map (iName,) hwtys)
                                                [0..]
        netdecl  = NetDecl iName hwty
        ids'     = map (`Identifier` Nothing) ids
        netassgn = Assignment iName (DataCon hwty (DC (hwty,0)) ids')
    in  (ports',(netdecl:decls' ++ [netassgn],iName))
  _               -> ([(iName,hwty)],([],iName))
  where
    iName = append i (pack ("_" ++ show cnt))

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

mkOutput :: (Identifier,HWType)
         -> Int
         -> ( [(Identifier,HWType)]
            , ( [Declaration]
              , Identifier
              )
            )
mkOutput (i,hwty) cnt = case hwty of
  Vector sz hwty' ->
    let (ports',(decls',ids)) = (concat *** (first concat . unzip))
                              . unzip
                              $ map (mkInput (iName,hwty')) [0..(sz-1)]
        netdecl  = NetDecl iName hwty
        assigns  = zipWith
                     (\id_ n -> Assignment id_
                                  (Identifier iName (Just (Indexed (hwty,1,n)))))
                     ids
                     [0..]
    in  (ports',(netdecl:assigns ++ decls',iName))
  Product _ hwtys ->
    let (ports',(decls',ids)) = (concat *** (first concat . unzip))
                              . unzip
                              $ zipWith mkInput (map (iName,) hwtys)
                                                [0..]
        netdecl  = NetDecl iName hwty
        assigns  = zipWith
                     (\id_ n -> Assignment id_
                                  (Identifier iName (Just (Indexed (hwty,0,n)))))
                     ids
                     [0..]
    in  (ports',(netdecl:assigns ++ decls',iName))
  _               -> ([(iName,hwty)],([],iName))
  where
    iName = append i (pack ("_" ++ show cnt))
