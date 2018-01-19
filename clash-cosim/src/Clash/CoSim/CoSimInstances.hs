{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.CoSim.CoSimInstances where

import NeatInterpolation

import qualified Data.Text as Text

import Clash.CoSim.CodeGeneration (coSimGen)
import Clash.CoSim.Simulator      (coSimN)

import Language.Haskell.TH        ( Q )
import Language.Haskell.TH.Syntax ( Exp(VarE), Name(..), OccName(..) )

-- Generate coSim1, coSim2, ... The default number of functions it generates is
-- eight, but this can be increased by settings the environment variables
-- COSIM_MAX_NUMBER_OF_ARGUMENTS while building.
$(coSimGen)

-- |
qCoSim :: Int -> Q Exp
qCoSim n = do
    coSim1Ref <- [| qCoSim |]

    return $
        case coSim1Ref of
            -- Get 'magic' values from statically deduced referal to qCoSim. Then,
            -- replace the name of the function, with one of our generated ones.
            VarE (Name (OccName _) nf) ->
                VarE (Name (OccName $ "coSim" ++ show n) nf)
            (Text.pack . show -> e) -> error $ Text.unpack [text|
              An error occured in Clash.CoSim.CoSimInstances.qCoSim. This is an
              error in the cosim pacakge itself. We expected a datastructure:

                  VarE (Name (OccName _) _)

              but got:

                  $e
            |]

