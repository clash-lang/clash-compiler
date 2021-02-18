{-|
  Copyright   :  (C) 2019, QBayLogic
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Orestis Melkonian <melkon.or@gmail.com>

  Entry point for the @clash-term@ executable.
-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeFamilies, InstanceSigs, ExplicitForAll #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import Data.Binary (Binary, decodeOrFail)
import Data.List   (find)
import Data.Maybe  (fromJust)
import qualified Data.ByteString.Lazy as BL

import Clash.Core.Term     ( Term (..), LetBinding, Pat (..), Alt
                           , Context, CoreContext (..) )
import Clash.Core.Var      (Id)
import Clash.Rewrite.Types (RewriteStep (..))
import Clash.Core.Pretty   ( ClashDoc, ClashAnnotation (..), SyntaxElement (..)
                           , PrettyOptions (..) )
import qualified Clash.Core.Pretty as Pr

import RewriteInspector (Diff (..), History, HStep (..), Syntax)
import qualified RewriteInspector as RW

main :: IO ()
main = RW.runTerminal @Term "clash-term/theme.ini"

-------------------------------
-- Clash instance for Diff.
decodeList :: forall a. Binary a => FilePath -> IO [a]
decodeList fn = do
  bytes <- BL.readFile fn
  return (go bytes)
  where
    go :: BL.ByteString -> [a]
    go s
      | BL.null s
      = []
      | otherwise
      = case decodeOrFail @a s of
          Right (s', _, x) -> x : go s'
          Left  _          -> error "malformed history"

instance Diff Term where
  type Ann     Term = ClashAnnotation
  type Options Term = PrettyOptions
  type Ctx     Term = CoreContext

  readHistory :: FilePath -> IO (History Term CoreContext)
  readHistory fname = map go <$> decodeList @RewriteStep fname
    where
      go :: RewriteStep -> HStep Term CoreContext
      go st@(RewriteStep {}) = HStep { _ctx    = reverse (t_ctx st)
                                     , _bndrS  = t_bndrS st
                                     , _name   = t_name st
                                     , _before = t_before st
                                     , _after  = t_after st }

  initialExpr :: History Term CoreContext -> Term
  initialExpr = _before . fromJust . find ((== "normalization") . _name)

  topEntity :: String
  topEntity = "topEntity"

  handleAnn :: ClashAnnotation -> Either Syntax CoreContext
  handleAnn (AnnContext c) = Right c
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

  patch :: Term -> Context -> Term -> Term
  patch _ [] t = t
  patch curE (c:cs) t' =
    case (curE, c) of
        (App l r, AppFun) ->
          App (go l) r
        (App l r, AppArg _) ->
          App l (go r)
        (TyApp e ty, TyAppC) ->
          TyApp (go e) ty
        (Letrec bnds body, LetBinding i' _) ->
          Letrec (mapBindings i' bnds) body
        (Letrec bnds t, LetBody is) ->
          if (fst <$> bnds) == is
            then Letrec bnds (go t)
            else error "Ctx.LetBody: different bindings"
        (Lam i t, LamBody i') ->
          if i == i'
            then Lam i (go t)
            else error $ "Ctx.Lam: different identifiers " ++ show (i, i')
        (TyLam tyVar t, TyLamBody tyVar') ->
          if tyVar == tyVar'
            then TyLam tyVar (go t)
            else error "Ctx.TyLam: different type variables"
        (Case scrut ty alts, CaseAlt pat) ->
          Case scrut ty (mapAlternatives pat alts)
        (Case t ty alts, CaseScrut) ->
          Case (go t) ty alts
        (Cast t ty ty', CastBody) ->
          Cast (go t) ty ty'
        (Tick ti x, TickC ti') ->
          if ti == ti'
            then Tick ti (go x)
            else error $ "Ctx.Tick: different ticks " ++ show (ti, ti')
        _ -> error "patch: context does not agree with term"
    where
      go :: Term -> Term
      go = \t -> patch t cs t'

      mapBindings :: Id -> [LetBinding] -> [LetBinding]
      mapBindings i ((i', t) : bs)
        | i == i'   = (i', go t) : bs
        | otherwise = (i', t)    : mapBindings i bs
      mapBindings _ [] = error "Ctx.LetBinding: no such binding"

      mapAlternatives :: Pat -> [Alt] -> [Alt]
      mapAlternatives pat ((pat', t) : alts)
        | pat == pat' = (pat', go t) : alts
        | otherwise   = (pat', t)    : mapAlternatives pat alts
      mapAlternatives _ [] = error "Ctx.Case: no such alternative"
