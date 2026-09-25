module THCorePlugin (plugin) where

import GHC.Plugins

-- Register this plugin through TH in a local module, so Clash must run it
-- when compiling the object code used by subsequent splices.
plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = \_ passes ->
      pure (CoreDoPluginPass "rewrite pluginValue" rewriteValue : passes)
  , pluginRecompile = purePlugin
  }

rewriteValue :: ModGuts -> CoreM ModGuts
rewriteValue guts = do
  dflags <- getDynFlags
  let
    rewrite (b, rhs)
      | getOccString b == "pluginValue" = (b, mkIntExpr (targetPlatform dflags) 42)
      | otherwise = (b, rhs)
    rewriteBind (NonRec b rhs) = uncurry NonRec (rewrite (b, rhs))
    rewriteBind (Rec bindings) = Rec (map rewrite bindings)
  pure guts { mg_binds = map rewriteBind (mg_binds guts) }
