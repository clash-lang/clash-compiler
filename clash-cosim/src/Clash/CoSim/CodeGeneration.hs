{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}

module Clash.CoSim.CodeGeneration
    ( arrow
    , arrowsR
    , arrowsL
    , applyE
    , applyT
    , coSimGen
    ) where

import Paths_clash_cosim

import Clash.Annotations.Primitive (Primitive(..), HDL(..))
import Clash.CoSim.Types
import Clash.Prelude (Signal)
import System.Environment (getEnv)
import Language.Haskell.TH
import Control.Monad (replicateM)

--------------------------------------
---- TEMPLATE HASKELL TOOLS ----------
--------------------------------------
-- | Put arrow in between types. That is, conceptually, evaluating
-- `arrow a b` ~ `a -> (b)`. Note you can use infix notation.
arrow :: Type -> Type -> Type
arrow t1 t2 = AppT (AppT ArrowT t1) t2

-- | Apply arrows repeatedly from [left] to right. That is, conceptually,
-- evaluating `arrowsR [a, b] c` ~ `a -> (b -> c)`.
arrowsR :: [Type] -> Type -> Type
arrowsR [] t = t
arrowsR ts t = arrow (head ts) (arrowsR (tail ts) t)

-- | Apply arrows repeatedly from left to [right]. That is, conceptually,
-- evaluating `arrowsR a [b, c]` ~ `a -> (b -> c)`.
arrowsL :: Type -> [Type] -> Type
arrowsL t [] = t
arrowsL t ts = arrow t (arrowsL (head ts) (tail ts))

-- | Convenience function to apply a list of arguments to a function
applyE :: Foldable t => ExpQ -> t Name -> ExpQ
applyE = foldl (\f x -> [| $f $(varE x) |])

applyT :: Foldable t => Type -> t (Name) -> Type
applyT = foldl (\t x -> AppT t (VarT x))

--------------------------------------
---- CODE GENERATION -----------------
--------------------------------------
coSimGen :: Q [Dec]

coSimGen = do
#ifdef CABAL
    -- We're running in a CABAL environment, so we know this environment
    -- variable is set:
    n <- read <$> runIO (getEnv "COSIM_MAX_NUMBER_OF_ARGUMENTS")
#else
    let n = 16
#endif


    concat <$> (sequence $ map coSimGen' [1..n])

coSimGen' :: Int -> Q [Dec]
coSimGen' n = do
    let coSimName = mkName $ "coSim" ++ (show n)

    -- Type signature
    coSimType <- SigD coSimName <$> coSimTypeGen n

    -- Function declaration and body
    let coSim = FunD coSimName [Clause [] (NormalB $ VarE $ mkName "coSimN") []]

    -- NOINLINE pragma
    let inline = PragmaD $ InlineP coSimName NoInline FunLike AllPhases

    -- Clash blackbox pragma
    primDir        <- runIO $ getDataFileName "src/prims/verilog"
    primitiveAnn   <- [| Primitive Verilog primDir |]
    let blackboxAnn = PragmaD $ AnnP (ValueAnnotation coSimName) primitiveAnn

    return [coSimType, coSim, inline, blackboxAnn]


-- | Generate type signature of coSim function
coSimTypeGen
    :: Int
    -- ^ Type signature for coSimN
    -> Q Type
coSimTypeGen n = do
    -- Generate "random" names for type variables
    let ids = concat $ drop 1 $ (`replicateM` ['a'..'z']) <$> [0..]
    let argNames     = map mkName (take n ids)
    let argTypeNames = map (return . VarT) argNames

    let resultName = mkName "result"
    let result     = return $ VarT resultName

    let clkName = mkName "clk"
    let clk     = return $ VarT clkName

    -- Generate contraints:
    argConstraints <- sequence $ map (\name -> [t| ClashType $name |]) argTypeNames
    resConstraint  <- [t| ClashType $result |]
    let constraints = resConstraint : argConstraints

    -- Generate type:
    fixedArgs      <- sequence [[t| String |], [t| String |], [t| CoSimSettings |]]
    argSignalTypes <- sequence $ map (\name -> [t| Signal $clk $name |]) argTypeNames
    resSignalType  <- [t| Signal $clk $result |]

    let ctx = (fixedArgs ++ argSignalTypes) `arrowsR` resSignalType
    let varNames = resultName : clkName : argNames
    return $ ForallT (map PlainTV varNames) constraints ctx



