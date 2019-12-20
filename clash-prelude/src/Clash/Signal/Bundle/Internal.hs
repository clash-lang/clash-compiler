{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Signal.Bundle.Internal where

import           Clash.CPP             (maxTupleSize)
import           Clash.Signal.Internal (Signal)
import           Control.Monad         (replicateM)
import           Data.List             (foldl')
import           Language.Haskell.TH

-- | Contruct all the tuple instances for Bundle.
deriveBundleTuples
  :: Name
  -- ^ Bundle
  -> Name
  -- ^ Unbundled
  -> Name
  -- ^ bundle
  -> Name
  -- ^ unbundle
  -> DecsQ
deriveBundleTuples bundleTyName unbundledTyName bundleName unbundleName = do
  let bundleTy = ConT bundleTyName
      signal   = ConT ''Signal

  allNames <- replicateM maxTupleSize (newName "a")
  tempNames <- replicateM maxTupleSize (newName "b")
  t <- newName "t"
  x <- newName "x"
  tup <- newName "tup"

  pure $ flip map [2..maxTupleSize] $ \tupleNum ->
    let names = take tupleNum allNames
        temps = take tupleNum tempNames
        vars  = fmap VarT names
        tuple = foldl' AppT (TupleT tupleNum)

        -- Instance declaration
        instTy = AppT bundleTy $ tuple vars

        -- Associated type Unbundled
#if MIN_VERSION_template_haskell(2,15,0)
        unbundledTypeEq =
          TySynEqn Nothing
            ((ConT unbundledTyName `AppT`
                VarT t ) `AppT` tuple vars )
            $ tuple $ map (AppT (signal `AppT` VarT t)) vars
        unbundledType = TySynInstD unbundledTypeEq
#else
        unbundledTypeEq =
          TySynEqn
            [ VarT t, tuple vars ]
            $ tuple $ map (AppT (signal `AppT` VarT t)) vars
        unbundledType = TySynInstD unbundledTyName unbundledTypeEq
#endif

        bundleLambda = LamE (map VarP temps) (TupE $ map VarE temps)
        applicatives = VarE '(<$>) : repeat (VarE '(<*>))
        bundle =
          FunD
            bundleName
            [ Clause
                [ TupP $ map VarP names ]
                ( NormalB
                $ foldl'
                    (\f (a, b) -> a `AppE` f `AppE` b)
                    bundleLambda
                    (zip applicatives $ map VarE names)
                )
                []
            ]

        unbundleLambda n =
          LamE
            [ TupP [ if i == n then VarP x else WildP | i <- [0..tupleNum-1] ] ]
            (VarE x)

        unbundle =
          FunD
            unbundleName
            [ Clause
                [ VarP tup ]
                ( NormalB . TupE $
                    map
                      (\n -> VarE 'fmap `AppE` unbundleLambda n `AppE` VarE tup)
                      [0..tupleNum-1]
                )
                []
            ]

    in InstanceD Nothing [] instTy [unbundledType, bundle, unbundle]
