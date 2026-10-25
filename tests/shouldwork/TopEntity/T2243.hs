module T2243 where

import qualified Prelude as P
import Data.List (isInfixOf, isPrefixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Clash.Explicit.Prelude
import qualified Clash.Prelude as Hidden

-- Companion to tests/shouldfail/TopEntity/T2243.hs: the annotations below are
-- valid and must /not/ trigger the superfluous-port-names check. All top
-- entities are compiled in a single Clash invocation, and 'mainVHDL' checks
-- that the generated entities have exactly the annotated ports.

-- Exactly matching names on a split clock/reset product.
{-# ANN exactTop
  (Synthesize
    { t_name     = "exactTop"
    , t_inputs   =
      [ PortProduct ""
        [ PortName "clk"
        , PortName "rst"
        ]
      , PortName "x"
      ]
    , t_output   = PortName "y"
}) #-}
exactTop
  :: (Clock System, Reset System)
  -> Signal System Int
  -> Signal System Int
exactTop (clk, rst) = register clk rst enableGen 0
{-# OPAQUE exactTop #-}

-- Nested split with names following the syntactic nesting: products are split
-- one level at a time.
{-# ANN nestedExactTop
  (Synthesize
    { t_name     = "nestedExactTop"
    , t_inputs   =
      [ PortProduct ""
        [ PortName "x"
        , PortProduct "cr"
          [ PortName "clk"
          , PortName "rst"
          ]
        ]
      ]
    , t_output   = PortName "y"
}) #-}
nestedExactTop
  :: (Signal System Int, (Clock System, Reset System))
  -> Signal System Int
nestedExactTop (x, (clk, rst)) = register clk rst enableGen 0 x
{-# OPAQUE nestedExactTop #-}

-- Hidden clock/reset/enable named through a 'PortProduct', the supported
-- style (see also T1171).
{-# ANN hiddenExactTop
  (Synthesize
    { t_name     = "hiddenExactTop"
    , t_inputs   =
      [ PortProduct ""
        [ PortName "clk"
        , PortName "rst"
        , PortName "en"
        ]
      , PortName "x"
      ]
    , t_output   = PortName "y"
}) #-}
hiddenExactTop
  :: Hidden.HiddenClockResetEnable System
  => Signal System Int
  -> Signal System Int
hiddenExactTop = Hidden.register 0
{-# OPAQUE hiddenExactTop #-}

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise = P.error $ P.concat [ "Expected:\n\n  ", needle
                                   , "\n\nIn:\n\n", haystack ]

-- | The port declarations of the (first) entity in a generated VHDL file.
entityPorts :: String -> [String]
entityPorts content =
  P.filter (\l -> ": in " `isInfixOf` l P.|| ": out " `isInfixOf` l)
    $ P.takeWhile (P.not . isPrefixOf "end")
    $ P.dropWhile (P.not . isPrefixOf "entity")
    $ P.lines content

-- | Check that an entity has exactly the given ports, in order.
assertPorts :: FilePath -> String -> [String] -> IO ()
assertPorts topDir qualifiedName ports = do
  let entity = P.drop 1 (P.dropWhile (P./= '.') qualifiedName)
  content <- readFile (topDir </> qualifiedName </> entity P.<> ".vhdl")
  mapM_ (`assertIn` content) ports
  let n = P.length (entityPorts content)
  if n P.== P.length ports
    then return ()
    else P.error $ P.concat
      [ entity, " has ", P.show n, " ports, expected "
      , P.show (P.length ports), " in:\n\n", content ]

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs

  assertPorts topDir (P.show 'exactTop)
    [ "clk : in exactTop_types.clk_System;"
    , "rst : in exactTop_types.rst_System;"
    , "x   : in signed(63 downto 0);"
    , "y   : out signed(63 downto 0));"
    ]

  assertPorts topDir (P.show 'nestedExactTop)
    [ "x      : in signed(63 downto 0);"
    , "cr_clk : in nestedExactTop_types.clk_System;"
    , "cr_rst : in nestedExactTop_types.rst_System;"
    , "y      : out signed(63 downto 0));"
    ]

  assertPorts topDir (P.show 'hiddenExactTop)
    [ "clk : in hiddenExactTop_types.clk_System;"
    , "rst : in hiddenExactTop_types.rst_System;"
    , "en  : in hiddenExactTop_types.en_System;"
    , "x   : in signed(63 downto 0);"
    , "y   : out signed(63 downto 0));"
    ]
