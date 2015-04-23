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
  , outputs       = [("output",Integer)]
  , declarations  = wrappers
  }
  where
    inputs'                    = map (first (const "input")) (inputs topComponent)
    (inputs'',(wrappers,_ids)) = (concat *** (first concat . unzip))
                               . unzip
                               $ zipWith mkInput inputs' [0..]

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
      in  (ports',(netdecl:netassgn:decls',iName))
    Product _ hwtys ->
      let (ports',(decls',ids)) = (concat *** (first concat . unzip))
                                . unzip
                                $ zipWith mkInput (map (iName,) hwtys)
                                                  [0..]
          netdecl  = NetDecl iName hwty
          ids'     = map (`Identifier` Nothing) ids
          netassgn = Assignment iName (DataCon hwty (DC (hwty,0)) ids')
      in  (ports',(netdecl:netassgn:decls',iName))
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
