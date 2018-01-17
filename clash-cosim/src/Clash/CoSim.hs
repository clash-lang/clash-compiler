{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

module Clash.CoSim
    ( CoSimulator(..)
    , coSim
    , coSimCleanUp
    , CoSimSettings(..)
    , customVerilog
    , defaultSettings
    , mapAccumLM
    , verilog
    , verilogWithSettings
    , wordPack
    , wordUnpack
    ) where

---------------------------
---- IMPORTS --------------
---------------------------
import Paths_clash_cosim

-- Haskell
import Data.Data
import Data.List
import Data.Maybe
import Data.Typeable
import Prelude
import qualified Data.Text as T
import Text.Printf (printf)

-- Clash
import qualified Clash.Prelude as CP
import Clash.Prelude (BitPack, BitSize, Index, Signal, KnownNat)

-- FFI
import Foreign
import Foreign.C

-- GC / IO / Monad
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Monad
import System.IO
import System.IO.Unsafe
import System.Mem
import System.Random (randomRs, newStdGen)

-- Inline
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import NeatInterpolation

-- Cosim
import qualified Clash.CoSim.DSL as DSL

--------------------------------------
---- FFI Imports ---------------------
--------------------------------------

foreign import ccall "simStart"         c_simStart      :: Ptr CInt -> CString -> CString -> Ptr CString -> IO (Ptr a)
foreign import ccall "&simEnd"          c_simEnd        :: FunPtr (Ptr a -> IO ())
foreign import ccall "simStep"          c_simStep       :: Ptr a -> IO CInt

foreign import ccall "getInputLength"   c_inputLength   :: Ptr a -> IO CInt
foreign import ccall "getOutputLength"  c_outputLength  :: Ptr a -> IO CInt
foreign import ccall "getInputSizes"    c_inputSizes    :: Ptr a -> IO (Ptr CInt)
foreign import ccall "getOutputSizes"   c_outputSizes   :: Ptr a -> IO (Ptr CInt)
foreign import ccall "getInputPtr"      c_inputPtr      :: Ptr a -> IO (Ptr (Ptr CInt))
foreign import ccall "getOutputPtr"     c_outputPtr     :: Ptr a -> IO (Ptr (Ptr CInt))

foreign import ccall "writeToFile"      c_writeToFile   :: CString -> IO CString

--------------------------------------
---- Types ---------------------------
--------------------------------------

data CoSimSettings = CoSimSettings
    { simulator    :: CoSimulator
    , period       :: Int
    , resetFase    :: Bool
    , files        :: [String]
    , enableStdout :: Bool
    } deriving (Show, Typeable, Data)


defaultSettings = CoSimSettings
    { simulator    = Icarus
    , period       = 20
    , resetFase    = False
    , files        = []
    , enableStdout = False
    }

type CoSimRun = ( String
                -- ^ Source code
                , String
                -- ^ Module name
                , CoSimSettings
                )

type ClashType a = ( BitPack a
                   , KnownNat (BitSize a)
                   --, KnownNat (BitSize a + 1)
                   --, KnownNat (BitSize a + 2)
                   -- But gwhy (TODO).
                   )

type SignalStream = [[Int32]]

data CoSimulator = Icarus
                -- ^ https://github.com/steveicarus/iverilog
                 | ModelSim
                -- ^ https://www.mentor.com/products/fv/modelsim/
                   deriving (Show, Eq, Typeable, Data)

data HDL = Verilog
         | SystemVerilog
         | VHDL

--------------------------------------
---- INLINE --------------------------
--------------------------------------
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

  let runTuple       = [| (verilogModule, modName', $(varE settingsArg)) |]
  let coSimCall      = apply [| coSim $(runTuple) |] (namedArgs ++ anonymousArgs)
  let coSimWrapper   = lamE wrapperArgs coSimCall
  let appliedWrapper = apply coSimWrapper (map mkName varNames)

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

        -- Convenience function to apply a list of arguments to a function
        apply = foldl (\f x -> [| $f $(varE x) |])

        -- Create CoSimRun by recompiling to verilog source
        modName' = fromMaybe randName name
        verilogModule = DSL.toVerilog dsl modName'


--------------------------------------
---- Help-Functions ------------------
--------------------------------------
replaceStr
    :: String
    -- ^ needle to search for. If this string is empty, an error will occur.
    -> String
    -- ^ replacement to replace needle with.
    -> String
    -- ^ haystack in which to search.
    -> String
replaceStr needle replacement haystack =
    T.unpack $ T.replace (T.pack needle) (T.pack replacement) (T.pack haystack)

source :: CoSimRun -> String
source (s, _, _) = s

modname :: CoSimRun -> String
modname (_, m, _) = m

bool2Num False   = fromIntegral 0
bool2Num True    = fromIntegral 1

sim2Num Icarus   = fromIntegral 1
sim2Num ModelSim = fromIntegral 2

hdl2Num Verilog       = fromIntegral 1
hdl2Num SystemVerilog = fromIntegral 2
hdl2Num VHDL          = fromIntegral 3

transposeList ::(Eq a, Num b, Eq b) => b -> [[a]] -> [[a]]
transposeList 0 _   = []
transposeList n xss = ys : transposeList (n-1) yss
    where
        ys  = map head xss
        yss = map tail xss

mapAccumLM :: (acc -> x -> IO (acc, y)) -> acc -> [x] -> IO (acc, [y])
mapAccumLM f s xs = return $ mapAccumL (\a xs -> unsafePerformIO $ f a xs) s xs

--------------------------------------
---- Array Marshalling ---------------
--------------------------------------

peekArray' :: (Storable a, Integral b) => b -> Ptr a -> IO [a]
peekArray' (-1) _    = error "null-pointer"
peekArray' size ptr
    | ptr == nullPtr = error "null-pointer"
    | otherwise      = peekArray (fromIntegral size) ptr

pokeArray' :: Storable a => Ptr a -> [a] -> IO ()
pokeArray' ptr []    = return ()
pokeArray' ptr xs
    | ptr == nullPtr = error "null-pointer"
    | otherwise      = pokeArray ptr xs

--------------------------------------
---- Co-Simulation -------------------
--------------------------------------

coSimCleanUp :: IO ()
coSimCleanUp = performGC

coSim
    :: (CoSim r)
    => CoSimRun
    -- ^ Simulation settings
    -> r
    -- ^ Polyvariadic number of arguments
coSim settings = coSim' settings []
{-# INLINE coSim #-}

--------------------------------------
---- Co-Simulation MARSHALLING -------
--------------------------------------

coSimMarshall
    :: CoSimRun
    -- ^ Simulation settings
    -> [SignalStream]
    -- ^ SignalStreams
    -> IO ( Bool
          -- ^ ResetFase
          , Ptr CInt
          -- ^ Settings
          , CString
          -- ^ Module directory
          , CString
          -- ^ Top entity name
          , Ptr CString
          -- ^ Files
          )
coSimMarshall (source, name, CoSimSettings{..}) streams = do
        -- files
        c_f         <- (newCString source) >>= c_writeToFile
        c_fs        <- mapM newCString files
        let c_files = c_f : c_fs
        c_filePtrs  <- newArray c_files

        -- topEntity & settings
        moduleDir     <- getDataFileName "src/cbits/"
        c_topEntity   <- newCString name
        c_moduleDir   <- newCString moduleDir
        c_settingsPtr <- newArray $ map fromIntegral $ c_settingsf c_files

        -- return
        return (resetFase, c_settingsPtr, c_moduleDir, c_topEntity, c_filePtrs)

    where
        c_settingsf fs    = [ sim2Num simulator
                            , hdl2Num Verilog
                            , period
                            , bool2Num resetFase
                            , bool2Num enableStdout
                            , length fs
                            , length streams
                            ]

--------------------------------------
---- Co-Simulation START -------------
--------------------------------------

coSimStart
  :: CoSimRun
  -> [SignalStream]
  -- ^ Input streams
  -> SignalStream
  -- ^ Output stream
coSimStart settings streams = unsafePerformIO $ do
        -- clean up
        coSimCleanUp

        -- marshall c-types
        (resetFase, c_settingsPtr, c_moduleDir, c_topEntity, c_filePtrs) <-
            coSimMarshall settings streams

        -- start simulation
        c_coSimState' <- c_simStart c_settingsPtr c_moduleDir c_topEntity c_filePtrs
        when (c_coSimState' == nullPtr) $ error "Start co-simulation failed"

        -- add finilizer
        c_coSimState <- newForeignPtr c_simEnd c_coSimState'

        -- perform simulation steps
        c_oLength <- withForeignPtr c_coSimState c_outputLength
        (_, ys)   <- mapAccumLM coSimStep c_coSimState $ f resetFase streams

        return $ case transposeList c_oLength ys of
                     []  -> error "Simulator expects no output ports"
                     [s] -> s
                     _   -> error "Simulator expects more than one output port"
    where
        f r | r         = ([]:) . transpose
            | otherwise = transpose

--------------------------------------
---- Co-Simulation STEP --------------
--------------------------------------

coSimStep
    :: ForeignPtr a
    -> [[Int32]]
    -> IO ( ForeignPtr a
          , [[Int32]]
          )
coSimStep state xs = do

        -- write input
        coSimInput state xs

        -- perform simulation step
        rv <- withForeignPtr state c_simStep

        -- HACK: Fetch Ubuntu 18.04 version of iverilog in case of error. Although
        -- this is not guaranteed to install the latest patched version, it is a
        -- pretty safe bet as this package has been stable for almost two years
        -- now. We use a Danish mirror (one.com) as the *.archive.ubuntu.com do
        -- not support HTTPS (this is usually no problem due to APT handling auth
        -- logic).
        when (rv /= 0) $ error $ T.unpack [text|
            Error in co-simulation step. This can be caused by a bug in iverilog,
            which occurs in some builds. Use the latest version from the website,
            or on Ubuntu <= 16.04 systems, run:

                cd /tmp
                wget -q https://mirror.one.com/ubuntu/pool/main/r/readline/libreadline7_7.0-0ubuntu2_amd64.deb
                wget -q https://mirror.one.com/ubuntu/pool/universe/i/iverilog/iverilog_10.1-0.1build1_amd64.deb
                sha256sum libreadline7_7.0-0ubuntu2_amd64.deb iverilog_10.1-0.1build1_amd64.deb
                sudo dpkg -i libreadline7_7.0-0ubuntu2_amd64.deb iverilog_10.1-0.1build1_amd64.deb
                rm libreadline7_7.0-0ubuntu2_amd64.deb iverilog_10.1-0.1build1_amd64.deb
                cd -

            Make sure the checksums correspond with:

                647f958429e17496bc96f188befd8229d30b2c1719255a5e8d15b5cd7be8593b  libreadline7_7.0-0ubuntu2_amd64.deb
                5aab60f8f7cbae29205c47684c5fce41a60e6d8e1b8fea31013747407e95bf0b  iverilog_10.1-0.1build1_amd64.deb

            |]

        -- read output
        ys <- coSimOutput state

        -- touch state, to keep state alive
        touchForeignPtr state

        -- return output
        return (state, ys)

--------------------------------------
---- Co-Simulation INPUT OUTPUT ------
--------------------------------------

coSimInput
    :: ForeignPtr a
    -> [[Int32]]
    -> IO ()
coSimInput state xs = do

    -- check sizes
    c_iLength <- withForeignPtr state c_inputLength
    c_iSizes  <- withForeignPtr state c_inputSizes >>= peekArray' c_iLength
    when (f c_iSizes) $ error $ errStr c_iSizes

    -- write input
    c_inputPtrs <- withForeignPtr state c_inputPtr >>= peekArray' c_iLength
    zipWithM_ pokeArray' c_inputPtrs xs'

    where
        xs'       = map (map fromIntegral) xs
        f         = not . and . zipWith (\x y -> ( x == 0 ) || ( x == y )) iSizes
        iLength   = fromIntegral $ length xs
        iSizes    = map (fromIntegral . length) xs
        errStr ls = unwords ["Simulator expects"
                              , show ls
                              , "input words, but"
                              , show iSizes
                              , "given"
                              ]


coSimOutput :: ForeignPtr a -> IO [[Int32]]
coSimOutput state = do

    -- read output
    c_oLength    <- withForeignPtr state c_outputLength
    c_oSizes     <- withForeignPtr state c_outputSizes >>= peekArray' c_oLength
    c_outputPtrs <- withForeignPtr state c_outputPtr   >>= peekArray' c_oLength
    ys           <- zipWithM peekArray' c_oSizes c_outputPtrs

    -- convert and return
    return $ map (map fromIntegral) ys

--------------------------------------
---- CONVERSION ----------------------
--------------------------------------

-- | Return the number of bits in the type of the argument. The actual value of
-- the argument is ignored. Errors for types that do not have a fixed bitsize,
-- like Integer.
bitSize' :: Bits a => a -> Int
bitSize' (bitSizeMaybe -> Just v) = v
bitSize' _                        = error "Value does not have a fixed bitsize"

wordPack    :: (Integral a, Bits a) => a -> [Int32]
wordPack x = snd $ mapAccumR wordPack' x [1 .. wordSize]
    where
        size     = bitSize' x
        wordSize = 1 + shiftR (size - 1) 5

wordUnpack  :: (Integral a, Bits a) => [Int32] -> a
wordUnpack = foldl wordUnpack' 0

wordPack'   :: (Integral a, Bits a) => a -> b -> (a, Int32)
wordPack' x _ = (shiftR x 32, fromIntegral x)

wordUnpack' :: (Integral a, Bits a) => a -> Int32 -> a
wordUnpack' x y = (shiftL x 32) .|. (4294967295 .&. (fromIntegral y))

--------------------------------------
---- PARSING -------------------------
--------------------------------------

parseInput
    :: CoSimType t
    => [SignalStream]
    -> t
    -> [SignalStream]
parseInput streams x = toSignalStream x : streams

parseOutput
    :: CoSimType t
    => ([SignalStream] -> SignalStream)
    -> [SignalStream]
    -> t
parseOutput f streams = res
        where
            qs  = f (reverse streams)
            res = fromSignalStream $ qs

--------------------------------------
---- POLYVARIDIC ---------------------
--------------------------------------

class CoSim r where
    coSim'
      :: CoSimRun
      -> [SignalStream]
      -> r

--------------------------------------
---- Instances Definitions -----------
--------------------------------------
-- CoSim instances. Implemented to support up to 3 arguments.
--
-- This was originally implemented similar to /printf/, to support an infinite
-- number of arguments. In order to generate blackboxes however, we need to know
-- the number of arguments at statically, hence the verbose implementation.
--
-- These instances were generated by utils/coSimTemplate.hs. If we ever need to
-- support more arguments, just rerun that script.
--
-- TODO: Replace by Haskell code generation
instance {-# OVERLAPPABLE #-} CoSimType r => CoSim r where
    coSim' s streams = parseOutput (coSimStart s) streams

instance {-# OVERLAPPABLE #-} CoSimType r => CoSim (CoSimSettings -> r) where
    coSim' (source, name, _) streams settings = coSim' (source, name, settings) streams

instance {-# OVERLAPPING #-} (CoSimType t1, CoSimType r) => CoSim (t1 -> r) where
    coSim' s streams a1 = coSimBB1 (source s) (modname s) a1 $ (coSim' s . parseInput streams) a1
    {-# INLINE coSim' #-}

instance {-# OVERLAPPING #-} (CoSimType t1, CoSimType t2, CoSimType r) => CoSim (t1 -> t2 -> r) where
    coSim' s streams a1 a2 = coSimBB2 (source s) (modname s) a1 a2 $ (coSim' s . parseInput streams) a1 a2
    {-# INLINE coSim' #-}

instance {-# OVERLAPPING #-} (CoSimType t1, CoSimType t2, CoSimType t3, CoSimType r) => CoSim (t1 -> t2 -> t3 -> r) where
    coSim' s streams a1 a2 a3 = coSimBB3 (source s) (modname s) a1 a2 a3 $ (coSim' s . parseInput streams) a1 a2 a3
    {-# INLINE coSim' #-}


-- Cosim functions with an associated blackbox
coSimBB0 :: (CoSimType r) => String -> String -> r -> r
coSimBB0 s m  = id
{-# NOINLINE coSimBB0 #-}

coSimBB1 :: (CoSimType a1, CoSimType r) => String -> String -> a1 -> r -> r
coSimBB1 s m a1 = id
{-# NOINLINE coSimBB1 #-}

coSimBB2 :: (CoSimType a1, CoSimType a2, CoSimType r) => String -> String -> a1 -> a2 -> r -> r
coSimBB2 s m a1 a2 = id
{-# NOINLINE coSimBB2 #-}

coSimBB3 :: (CoSimType a1, CoSimType a2, CoSimType a3, CoSimType r) => String -> String -> a1 -> a2 -> a3 -> r -> r
coSimBB3 s m a1 a2 a3 = id
{-# NOINLINE coSimBB3 #-}



--------------------------------------
---- SUPPORTED TYPES -----------------
--------------------------------------

class CoSimType t where

--------------------------------------
---- Func Definitions ----------------
--------------------------------------
    -- | Return the number of bits in the type of the argument. The actual value of
    -- the argument is ignored. Errors for types that do not have a fixed bitsize,
    -- like Integer. This function will not evaluate its argument.
    toSignalStream   :: t -> SignalStream
    fromSignalStream :: SignalStream -> t

--------------------------------------
---- Instances Definitions -----------
--------------------------------------

instance {-# OVERLAPPING #-} (ClashType a, NFData a) => CoSimType (Signal clk a) where
    toSignalStream   = map (wordPack . CP.pack) . CP.sample
    fromSignalStream = CP.fromList . map (CP.unpack . wordUnpack)
