{-|
  Copyright   :  (C) 2019, QBayLogic
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Orestis Melkonian <melkon.or@gmail.com>

  Entry point for the @clash-term@ executable.
-}
{-# LANGUAGE DataKinds, FlexibleInstances, ScopedTypeVariables, TypeApplications, TypeFamilies, InstanceSigs, ExplicitForAll #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import Data.List   (find)
import Data.Maybe  (fromJust)
import qualified Data.ByteString.Lazy as BL

import Clash.Core.Scoped.Syntax  (S (VoidS))
import qualified Clash.Core.Scoped.Syntax as S
import Clash.Core.Scoped.Pretty ()
import Clash.Rewrite.Scoped.History (decodeRewriteHistory)
import Clash.Rewrite.Types (RewriteStep (..))
import Clash.Core.Pretty   ( ClashDoc, ClashAnnotation (..), SyntaxElement (..)
                           , PrettyOptions (..), SubtermPos )
import qualified Clash.Core.Pretty as Pr

import RewriteInspector (Diff (..), History, HStep (..), Syntax)
import qualified RewriteInspector as RW

-- | The terms of the rewrite history: closed terms of scoped Clash Core, see
-- "Clash.Rewrite.Scoped.History"
type Term = S.Term 'VoidS

main :: IO ()
main = RW.runTerminal @Term "clash-term/theme.ini"

-------------------------------
-- Clash instance for Diff.
-- | The steps record no context: every step shows the closed term before and
-- after the transformation, abstracted over its free variables. The printer
-- of scoped Clash Core marks subterms by their position ('SubtermPos').
instance Diff Term where
  type Ann     Term = ClashAnnotation
  type Options Term = PrettyOptions
  type Ctx     Term = SubtermPos

  readHistory :: FilePath -> IO (History Term SubtermPos)
  readHistory fname = map go . decodeRewriteHistory <$> BL.readFile fname
    where
      go :: RewriteStep -> HStep Term SubtermPos
      go st@(RewriteStep {}) = HStep { _ctx    = []
                                     , _bndrS  = t_bndrS st
                                     , _name   = t_name st
                                     , _before = t_before st
                                     , _after  = t_after st }

  initialExpr :: History Term SubtermPos -> Term
  initialExpr = _before . fromJust . find ((== "normalization") . _name)

  topEntity :: String
  topEntity = "topEntity"

  handleAnn :: ClashAnnotation -> Either Syntax SubtermPos
  handleAnn (AnnSubterm p) = Right p
  handleAnn (AnnContext _) =
    error "clash-term: the printer of scoped Clash Core marks no CoreContext"
  handleAnn (AnnSyntax s)  = Left $ case s of
    Keyword   -> RW.Keyword
    LitS      -> RW.Literal
    Type      -> RW.Type
    Unique    -> RW.Unique
    Qualifier -> RW.Qualifier
    Ticky     -> RW.Custom "Tick"

  ppr' :: PrettyOptions -> Term -> ClashDoc
  ppr' = Pr.ppr'

  initOptions :: PrettyOptions
  initOptions = PrettyOptions True True True True

  flagFields :: [(PrettyOptions -> Bool, PrettyOptions -> Bool -> PrettyOptions, String)]
  flagFields =
    [ (displayUniques, \opt b -> opt {displayUniques = b}, "display uniques")
    , (displayTypes, \opt b -> opt {displayTypes = b}, "display types")
    , (displayQualifiers, \opt b -> opt {displayQualifiers = b}, "display qualifiers")
    , (displayTicks, \opt b -> opt {displayTicks = b}, "display ticks")
    ]

  -- The steps record no context, so a step replaces the whole term
  patch :: Term -> [SubtermPos] -> Term -> Term
  patch _ _ t = t
