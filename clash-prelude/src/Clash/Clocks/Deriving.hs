{-|
Copyright  :  (C) 2018-2022, Google Inc
                  2019,      Myrtle Software Ltd
                  2023,      QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Clocks.Deriving (deriveClocksInstances) where

import Control.Monad               (foldM)
import Clash.Explicit.Signal       (unsafeSynchronizer)
import Clash.Signal.Internal
import Language.Haskell.TH.Compat
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Unsafe.Coerce               (unsafeCoerce)

conPatternNoTypes :: Name -> [Pat] -> Pat
#if MIN_VERSION_template_haskell(2,18,0)
conPatternNoTypes nm pats = ConP nm [] pats
#else
conPatternNoTypes nm pats = ConP nm pats
#endif

-- Derive instance for /n/ clocks
derive' :: Int -> Q Dec
derive' n = do
  -- (Clock d0, Clock d1, )
  instType0 <- foldM (\a n' -> AppT a <$> clkType n') (TupleT $ n + 1) [1..n]
  instType1 <- AppT instType0 <$> lockType
  let instHead = AppT (ConT $ mkName "Clocks") instType1

  cxtRHS0 <-
    foldM (\a n' -> AppT a <$> knownDomainCxt n') (TupleT $ n + 1) [1..n]
  cxtRHS1 <- AppT cxtRHS0 <$> lockKnownDomainCxt
#if MIN_VERSION_template_haskell(2,15,0)
  let cxtLHS = AppT (ConT $ mkName "ClocksCxt") instType1
  let cxtTy  = TySynInstD (TySynEqn Nothing cxtLHS cxtRHS1)
#else
  let cxtTy  = TySynInstD (mkName "ClocksCxt") (TySynEqn [instType1] cxtRHS1)
#endif

  -- Function definition of 'clocks'
  let clk = mkName "clk"
  let rst = mkName "rst"

  -- Implementation of 'clocks'
  lockImpl <- [| unsafeSynchronizer clockGen clockGen
                   (unsafeToLowPolarity $(varE rst)) |]
  let
    noInline  = PragmaD $ InlineP (mkName "clocks") NoInline FunLike AllPhases
    clkImpls  = replicate n (clkImpl clk)
    instTuple = mkTupE $ clkImpls ++ [lockImpl]
    funcBody  = NormalB instTuple
    errMsg    = "clocks: dynamic clocks unsupported"
    errBody   = NormalB ((VarE 'error) `AppE` (LitE (StringL errMsg)))
    instFunc  = FunD (mkName "clocks")
      [ Clause
          [ AsP
              clk
              (conPatternNoTypes 'Clock [WildP, conPatternNoTypes 'Nothing []])
          , VarP rst]
          funcBody
          []
      , Clause [WildP, WildP] errBody []
      ]

  return $ InstanceD Nothing [] instHead [cxtTy, instFunc, noInline]

  where
    -- | Generate type @Clock dom@ with fresh @dom@ variable
    clkType n' =
      let c = varT $ mkName ("c" ++ show n') in
      [t| Clock $c |]

    knownDomainCxt n' =
      let c = varT $ mkName ("c" ++ show n') in
      [t| KnownDomain $c |]

    -- | Generate type @Signal dom 'Bool@ with fresh @dom@ variable
    lockType =
      let c = varT $ mkName "pllLock" in
      [t| Signal $c Bool |]

    lockKnownDomainCxt =
      let p = varT $ mkName "pllLock" in
      [t| KnownDomain $p |]


    clkImpl clk = AppE (VarE 'unsafeCoerce) (VarE clk)

-- Derive instances for up to and including to /n/ clocks
deriveClocksInstances :: Int -> Q [Dec]
deriveClocksInstances n = mapM derive' [1..n]
