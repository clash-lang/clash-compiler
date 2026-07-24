{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.Evaluator.Primitives.GHC.Base
  ( primitives
  ) where

import           Data.Text           (Text)

#if MIN_VERSION_ghc(9,10,0)
import           Clash.Core.DataCon  (DataCon (..))
#endif
import           Clash.Core.Evaluator.Types
import           Clash.Core.Literal  (Literal (..))
import Clash.Core.Term (Term (..))
import Clash.Util (textNameLit)

import qualified GHC.Base

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  [ ( $(textNameLit 'GHC.Base.eqString)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [PrimVal _ _ [Lit (StringLiteral s1)]
              ,PrimVal _ _ [Lit (StringLiteral s2)]
              ] <- args
            -> reduce (boolToBoolLiteral tcm ty (s1 == s2))
            | otherwise -> error (show args)
    )

  , ( $(textNameLit 'GHC.Base.quotInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ DC intDc [Left (Literal (IntLiteral i))]
              , DC _     [Left (Literal (IntLiteral j))]
              ] <- args
            -> reduce (catchDivByZero (App (Data intDc) (Literal (IntLiteral (i `quot` j)))))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Base.remInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ DC intDc [Left (Literal (IntLiteral i))]
              , DC _     [Left (Literal (IntLiteral j))]
              ] <- args
            -> reduce (catchDivByZero (App (Data intDc) (Literal (IntLiteral (i `rem` j)))))
          _ -> Nothing
    )

  , ( $(textNameLit 'GHC.Base.divInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ DC intDc [Left (Literal (IntLiteral i))]
              , DC _     [Left (Literal (IntLiteral j))]
              ] <- args
            -> reduce (catchDivByZero (App (Data intDc) (Literal (IntLiteral (i `div` j)))))
          _ -> Nothing
    )


  , ( $(textNameLit 'GHC.Base.modInt)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [ DC intDc [Left (Literal (IntLiteral i))]
              , DC _     [Left (Literal (IntLiteral j))]
              ] <- args
            -> reduce (catchDivByZero (App (Data intDc) (Literal (IntLiteral (i `mod` j)))))
          _ -> Nothing
    )
#if MIN_VERSION_ghc(9,10,0)
  , ( $(textNameLit 'GHC.Base.dataToTag#)
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..}
            | [DC dc _] <- args
            -> reduce (Literal (IntLiteral (toInteger (dcTag dc - 1))))
          _ -> Nothing
    )
#endif
  ]
