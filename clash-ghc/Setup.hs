-- cabal-install runs the hooks in 'SetupHooks' itself, but other tools such as
-- Stack build packages with @build-type: Hooks@ through this file.
import Distribution.Simple (defaultMainWithSetupHooks)
import SetupHooks (setupHooks)

main :: IO ()
main = defaultMainWithSetupHooks setupHooks
