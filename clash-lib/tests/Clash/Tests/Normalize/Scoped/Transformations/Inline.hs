{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.Inline": the ports
  agree with Clash Core, through 'Clash.Rewrite.Scoped.ViaFoil.viaFoil'.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.Inline (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User), mkUnsafeName, noSrcSpan)
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (mkVarEnv)
import Clash.Driver.Types (Binding (..))
import qualified Clash.Normalize.Scoped.Transformations.Inline as S
import Clash.Normalize.Transformations.Inline
  (bindConstantVarWorker, inlineCastWorker, inlineSimIOWorker)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL (onLet, toTransformation)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteState (..))

import Test.Clash.Rewrite
  ( assertAlphaEq, globalId, inScopeOf, intFunTy, intId, intLit, intTy, localId, mkBinding
  , runSingleTransformation, testTyConMap, withTyConMap )

-- * The function being normalized

-- | The function being normalized, and a global with a binding
top, g :: Id
top = globalId User "top" 10 intTy
g = globalId User "g" 11 intTy

-- | A state in which 'top' is being normalized, with the given bindings
topState :: [(Id, Binding C.Term)] -> RewriteState extra -> RewriteState extra
topState bs st = st
  { _curFun = (top, noSrcSpan)
  , _bindings = mkVarEnv ((g, mkBinding g (intLit 1)) : bs)
  }

-- | Run a rewrite on a term in which @x@ is free, in a state set up with
-- 'topState'
runInTop :: [(Id, Binding C.Term)] -> NormRewrite -> C.Term -> IO C.Term
runInTop bs rw =
  runSingleTransformation (withTyConMap testTyConMap def) (topState bs def) (inScopeOf [x]) rw

x, y :: Id
x = intId User "x" 20
y = intId User "y" 21

-- | Both implementations of a transformation of let-bindings, by the name of
-- the test, with the given implementation of Clash Core
bothImpls :: NormRewrite -> NormRewrite -> [(String, NormRewrite)]
bothImpls cur scoped = [("Clash Core", cur), ("scoped", scoped)]

tests :: TestTree
tests =
  testGroup "Clash.Tests.Normalize.Scoped.Transformations.Inline"
    [ testGroup "bindConstantVar" $ concat
        [ [ testCase ("doesn't inline the function being normalized (#2839), " <> nm) $
              runInTop [] rw (C.Let (C.NonRec y (C.Var top)) (C.Var y))
                >>= assertAlphaEq (C.Let (C.NonRec y (C.Var top)) (C.Var y))
          , testCase ("doesn't inline another global, which isn't work-free-ish, " <> nm) $
              runInTop [] rw (C.Let (C.NonRec y (C.Var g)) (C.Var y))
                >>= assertAlphaEq (C.Let (C.NonRec y (C.Var g)) (C.Var y))
          , testCase ("inlines a constant, " <> nm) $
              runInTop [] rw (C.Let (C.NonRec y (intLit 3)) (C.Var y))
                >>= assertAlphaEq (intLit 3)
          , testCase ("inlines a local variable, " <> nm) $
              runInTop [] rw (C.Let (C.NonRec y (C.Var x)) (C.Var y))
                >>= assertAlphaEq (C.Var x)
          ]
        | (nm, rw) <- bothImpls
            $(asRewriteQ (toTransformation "bindConstantVar" (onLet 'bindConstantVarWorker)))
            $(asRewriteQ S.bindConstantVar)
        ]
    , testGroup "inlineCast" $ concat
        [ [ testCase ("inlines a cast of a variable, " <> nm) $ do
              let e = C.Cast (C.Var x) intTy intTy
              runInTop [] rw (C.Let (C.NonRec y e) (C.App (C.Var fun) (C.Var y)))
                >>= assertAlphaEq (C.App (C.Var fun) e)
          , testCase ("inlines a cast of a global, " <> nm) $ do
              let e = C.Cast (C.Var g) intTy intTy
              runInTop [] rw (C.Let (C.NonRec y e) (C.App (C.Var fun) (C.Var y)))
                >>= assertAlphaEq (C.App (C.Var fun) e)
          , testCase ("doesn't inline a cast of an application, " <> nm) $ do
              let e = C.Let (C.NonRec y (C.Cast (C.App (C.Var fun) (C.Var x)) intTy intTy))
                        (C.Var y)
              runInTop [] rw e >>= assertAlphaEq e
          ]
        | (nm, rw) <- bothImpls
            $(asRewriteQ (toTransformation "inlineCast" (onLet 'inlineCastWorker)))
            $(asRewriteQ S.inlineCast)
        ]
    , testGroup "inlineSimIO" $ concat
        [ [ testCase ("doesn't inline a binding of another type, " <> nm) $ do
              let e = C.Let (C.NonRec y (C.App (C.Var fun) (C.Var x))) (C.Var y)
              runInTop [] rw e >>= assertAlphaEq e
          , testCase ("inlines a SimIO action at every occurrence, " <> nm) $ do
              let act = C.App (C.Var simIOFun) (C.Var x)
              runInTop [] rw
                (C.Let (C.NonRec io act) (C.mkApps (C.Var bindIO) [Left (C.Var io), Left (C.Var io)]))
                >>= assertAlphaEq (C.mkApps (C.Var bindIO) [Left act, Left act])
          ]
        | (nm, rw) <- bothImpls
            $(asRewriteQ (toTransformation "bindSimIO" (onLet 'inlineSimIOWorker)))
            $(asRewriteQ S.inlineSimIO)
        ]
    ]
 where
  fun = globalId User "fun" 12 (intFunTy 1)
  -- @SimIO Int@, and functions producing and combining actions
  simIOTy = C.AppTy (C.ConstTy (C.TyCon (mkUnsafeName User "Clash.Explicit.SimIO.SimIO" 300))) intTy
  simIOFun = globalId User "act" 13 (C.mkFunTy intTy simIOTy)
  bindIO = globalId User "both" 14 (C.mkFunTy simIOTy (C.mkFunTy simIOTy simIOTy))
  io = localId User "io" 22 simIOTy
