{-|
Copyright  :  (C) 2018, Google Inc
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Clocks.Deriving (deriveClocksInstances) where

import Control.Monad               (foldM)
import Clash.Signal.Internal
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Unsafe.Coerce               (unsafeCoerce)

-- Derive instance for /n/ clocks
derive' :: Int -> Q Dec
derive' n = do
  -- (Clock d0 'Source, Clock d1 Source', )
  instType  <- foldM (\a n' -> AppT a <$> clkType n') (TupleT $ n + 1) [1..n]
  instType' <- (AppT (ConT $ mkName "Clocks") . AppT instType) <$> lockType

  -- Function definition of 'clocks'
  let clk = mkName "clk"
  let rst = mkName "rst"

  -- Implementation of 'clocks'
  let noInline  = PragmaD $ InlineP (mkName "clocks") NoInline FunLike AllPhases
  let clkImpls  = replicate n (clkImpl clk rst)
  let instTuple = TupE $ clkImpls ++ [AppE (VarE 'unsafeCoerce) (VarE rst)]
  let funcBody  = NormalB instTuple
  let rstPat    = ConP 'Async [VarP rst]
  let instFunc  = FunD (mkName "clocks") [Clause [VarP clk, rstPat] funcBody []]

  return $ InstanceD Nothing [] instType' [instFunc, noInline]

  where
    -- | Generate type @Clock dom 'Source@ with fresh @dom@ variable
    clkType n' =
      let c = varT $ mkName ("c" ++ show n') in
      [t| Clock $c 'Source |]

    -- | Generate type @Signal dom 'Bool@ with fresh @dom@ variable
    lockType =
      let c = varT $ mkName "pllLock" in
      [t| Signal $c Bool |]

    clkImpl (VarE -> clk) (VarE -> rst) =
      AppE (VarE 'unsafeCoerce) (AppE (AppE (VarE 'clockGate) clk) rst)

-- Derive instances for up to and including to /n/ clocks
deriveClocksInstances :: Int -> Q [Dec]
deriveClocksInstances n = mapM derive' [1..n]
