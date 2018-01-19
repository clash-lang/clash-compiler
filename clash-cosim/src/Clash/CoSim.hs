{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.CoSim
    ( CoSimulator(..)
    , CoSimSettings(..)
    , ClashType
    , coSimN
    , defaultSettings
    , verilog
    , verilogWithSettings
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Clash.CoSim.Simulator
import Clash.CoSim.CodeGeneration
import Clash.CoSim.CoSimInstances
import System.Random (randomRs, newStdGen)
import System.IO.Unsafe

import Data.Maybe (fromMaybe)

import qualified Clash.CoSim.DSLParser as DSL

notHandled :: [Char] -> a
notHandled ts = error (ts ++ " are not handled by the verilog quasiquoter.")

verilog :: QuasiQuoter
verilog = customVerilog False

verilogWithSettings :: QuasiQuoter
verilogWithSettings = customVerilog True

customVerilog :: Bool -> QuasiQuoter
customVerilog explicitSettings =
    QuasiQuoter { quoteExp  = compileVerilog explicitSettings
                , quotePat  = notHandled "patterns"
                , quoteType = notHandled "types"
                , quoteDec  = notHandled "declarations"
                }

compileVerilog :: Bool -> String -> Q Exp
compileVerilog explicitSettings source = do
  -- coSimWrapper: we generate a function with $(length userArgs) arguments
  -- which passes those arguments to coSim. The function adheres to the
  -- following type signature:
  --
  --   coSimWrapper :: a1 -> a2 -> .. -> CoSimSettings -> aa1 -> aa2 -> r
  --
  -- where a1, a2, .. are explicitely named arguments and aa1, aa2, .. are
  -- anonymous arugments (thus allowing dot-free notation).
  settingsArg   <- newName "settings"
  namedArgs     <- sequence $ map newName varNames
  anonymousArgs <- sequence $ map newName anonymousNames

  let wrapperArgs = concat [ map varP namedArgs
                           , [varP settingsArg]
                           , map varP anonymousArgs ]

  -- TODO: Find a better way to do this..
  let coSimFunc = qCoSim $ length $ namedArgs ++ anonymousArgs

  -- Fully apply actual coSim call to verilog source, verilog module name,
  -- settings argument from wrapper and argument variables from wrapper
  let coSimFunc'  = [| $coSimFunc verilogSource verilogModName $(varE settingsArg) |]
  let coSimFunc'' = applyE coSimFunc' (namedArgs ++ anonymousArgs)

  -- Create wrapper which calls actual coSim function
  let coSimWrapper   = lamE wrapperArgs coSimFunc''
  let appliedWrapper = applyE coSimWrapper (map mkName varNames)

  -- Add default settings to wrapper call if user does not supply their
  -- own settings record
  if explicitSettings
      then
          appliedWrapper
      else
          [| $appliedWrapper $(liftData defaultSettings) |]

    where
        -- HACK: Generate random name for module. We should replace this with
        -- some intelligent procedure taking into account the context in which
        -- the user quasiquoted. Alternatively, we should run 'prepareBlackbox'
        -- allow blackbox syntax to be used in the verilog templates. This would
        -- enable us to generate truly unique names (and we could just use static
        -- once during simulation.)
        randName = "cosim_" ++ (take 10 $ randomRs ('a','z') $ unsafePerformIO newStdGen)

        -- Parse DSL, throw runtime error if invalid
        (name, dsl) = case DSL.parseDSL source of
                          Left err -> error $ show err
                          Right d  -> d

        -- Get variables from DSL, create names we can use in quasiquoters
        (varNames, anonymousNames) = DSL.vars dsl

        -- XXXXX
        verilogModName = fromMaybe randName name
        verilogSource  = DSL.toVerilog dsl verilogModName

