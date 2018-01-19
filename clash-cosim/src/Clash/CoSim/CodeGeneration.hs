{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.CoSim.CodeGeneration
    ( arrow
    , arrowsR
    , arrowsL
    , applyE
    , applyT
    , coSimGen
    , blackboxJson
    , blackboxJsonString
    ) where


import Clash.CoSim.Types
import Clash.Prelude (Signal)
import System.Environment (lookupEnv)

import qualified Data.Text as Text
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T


import Control.Monad (replicateM)

-- Blackbox generation
import GHC.Exts (fromList)
import Language.Haskell.TH
import Data.Aeson (Value (Array, String, Object))
import Data.Aeson.Encode.Pretty (encodePretty)
import Text.Printf (printf)

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
    n <- (maybe 8 read) <$> runIO (lookupEnv "COSIM_MAX_NUMBER_OF_ARGUMENTS")
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

    return [coSimType, coSim, inline]


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


--------------------------------------
---- BLACKBOX GENERATION -------------
--------------------------------------
-- | Create a blackbox object of the following structure:
--
--        { 'name': name,
--          'type': type_,
--          'templateD': templateD }
--
bbObject
    :: String
    -- ^ name
    -> String
    -- ^ type
    -> String
    -- ^ templateD
    -> Value
bbObject bbname type_ templateD =
  Object (fromList [("BlackBox", Object (fromList [
      ("name", String $ Text.pack bbname)
    , ("type", String $ Text.pack type_)
    , ("templateD", String $ Text.pack templateD)
    ]))])

-- | Create blackbox for a given number of arguments
blackboxJson'
    :: Int
    -- ^ Number of arguments of coSimN
    -> Value
    -- ^ Blackbox object
blackboxJson' n = bbObject bbname "" templateD
    where
      -- Offset where 'real' arguments start, instead of constraints
      argsOffset = 1 -- result constraint
                 + n -- argument constraints

      -- Offset where signal arguments start
      signalOffset = argsOffset -- constraints
                   + 3          -- source, module name, simulation settings

      sourceOffset = argsOffset
      moduleOffset = argsOffset + 1

      bbname    = "Clash.CoSim.CoSimInstances.coSim" ++ show n
      args      = concat [printf "~ARG[%d], " i | i <- [signalOffset..signalOffset+n-1]] :: String
      template  = printf "~TEMPLATE[~LIT[%d].v][~LIT[%d]]" moduleOffset sourceOffset
      compname  = printf "~STRLIT[%d]" moduleOffset
      instanc_  = printf "~GENSYM[~STRLIT[%d]_inst][0] (%s~RESULT)" moduleOffset args
      templateD = unwords [template, compname, instanc_, ";"]

-- | Create blackbox for all coSim functions up to n
blackboxJson
    :: Int
    -- ^ Number of blackboxes to generate
    -> Value
    -- ^ Array of blackbox objects
blackboxJson n = Array $ fromList $ map blackboxJson' [1..n]

-- | Create blackbox for all coSim functions up to n. This function will encode
-- the json structure as a string, using a pretty printer.
blackboxJsonString
    :: Int
    -- ^ Number of blackboxes to generate
    -> String
    -- ^ Encoded json file
blackboxJsonString = T.unpack . T.decodeUtf8 . encodePretty . blackboxJson
