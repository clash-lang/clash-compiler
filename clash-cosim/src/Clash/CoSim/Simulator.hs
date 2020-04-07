{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Clash.CoSim.Simulator
Description : Simulator communication logic

This module has been developed as a part of John Verheij's MSc project. Please
see: <https://essay.utwente.nl/70777/>.
-}
module Clash.CoSim.Simulator
    ( coSimN
    , defaultSettings
    ) where

---------------------------
---- IMPORTS --------------
---------------------------
#ifdef CABAL
import Paths_clash_cosim
#else
import Clash.CoSim.Paths_clash_cosim
#endif

-- Haskell
import Data.List (mapAccumL, mapAccumR, transpose)
import Data.Maybe
import Prelude
import qualified Data.Text as T

-- Clash
import qualified Clash.Prelude as CP
import qualified Clash.Signal.Internal as CS

-- FFI
import Foreign
import Foreign.C

-- GC / IO / Monad
import Control.Monad
import System.IO.Unsafe
import System.Mem

-- Inline
import NeatInterpolation

-- Types shared with Clash.CoSim.CodeGeneration
import Clash.CoSim.Types


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
type CoSimRun = ( String
                -- Source code
                , String
                -- Module name
                , CoSimSettings
                )

type SignalStream = [[Int32]]


data HDL = Verilog
         | SystemVerilog
         | VHDL

--------------------------------------
---- Help-Functions ------------------
--------------------------------------
bool2Num :: Num p => Bool -> p
bool2Num False   = 0
bool2Num True    = 1

sim2Num :: Num p => CoSimulator -> p
sim2Num Icarus   = 1
sim2Num ModelSim = 2

hdl2Num :: Num p => HDL -> p
hdl2Num Verilog       = 1
hdl2Num SystemVerilog = 2
hdl2Num VHDL          = 3

transposeList ::(Eq a, Num b, Eq b) => b -> [[a]] -> [[a]]
transposeList 0 _   = []
transposeList n xss = ys : transposeList (n-1) yss
    where
        ys  = map head xss
        yss = map tail xss

mapAccumLM :: (acc -> x -> IO (acc, y)) -> acc -> [x] -> IO (acc, [y])
mapAccumLM f s xs = return $ mapAccumL (\a xs' -> unsafePerformIO $ f a xs') s xs

--------------------------------------
---- Array Marshalling ---------------
--------------------------------------

peekArray' :: (Storable a, Integral b) => b -> Ptr a -> IO [a]
peekArray' (-1) _    = error "null-pointer"
peekArray' size ptr
    | ptr == nullPtr = error "null-pointer"
    | otherwise      = peekArray (fromIntegral size) ptr

pokeArray' :: Storable a => Ptr a -> [a] -> IO ()
pokeArray' _ptr []   = return ()
pokeArray' ptr xs
    | ptr == nullPtr = error "null-pointer"
    | otherwise      = pokeArray ptr xs

--------------------------------------
---- Co-Simulation -------------------
--------------------------------------

coSimCleanUp :: IO ()
coSimCleanUp = performGC

--------------------------------------
---- Co-Simulation MARSHALLING -------
--------------------------------------

coSimMarshall
    :: CoSimRun
    -- ^ Simulation settings
    -> [SignalStream]
    -- ^ SignalStreams
    -> IO ( Bool
          -- ResetFase
          , Ptr CInt
          -- Settings
          , CString
          -- Module directory
          , CString
          -- Top entity name
          , Ptr CString
          -- Files
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
    => Bool
    -> [SignalStream]
    -> t
    -> [SignalStream]
parseInput doDup streams t = dup (toSignalStream t) : streams
  where
    dup | doDup     = go
        | otherwise = id
    go (x:xs) = (x:x:go xs)
    go _      = error "Unexpected empty SignalStream"

parseClock
  :: [SignalStream]
  -> CS.Clock dom
  -> [SignalStream]
parseClock streams _clk = (cycle [[0],[1]]) : streams

parseOutput
    :: CoSimType t
    => Bool
    -> ([SignalStream] -> SignalStream)
    -> [SignalStream]
    -> t
parseOutput doUndup f streams = res
        where
            qs  = undup (f (reverse streams))
            res = fromSignalStream $ qs
            undup
              | doUndup   = go
              | otherwise = id
            go (x:_:xs) = x:go xs
            go _        = error "Unexpected empty SignalStream"

--------------------------------------
---- POLYVARIDIC ---------------------
--------------------------------------
-- | Polyvariadic simulation function.
coSimN
    :: CoSim r
    => String
    -- ^ HDL source
    -> String
    -- ^ Module name to simulate
    -> CoSimSettings
    -- ^ Simulation settings
    -> r
coSimN source' modName' settings =
    coSim False (source', modName', settings) []
{-# INLINE coSimN #-}

class CoSim r where
    coSim
      :: Bool
      -> CoSimRun
      -> [SignalStream]
      -> r

instance {-# OVERLAPPABLE #-} CoSimType r => CoSim r where
    coSim b s = parseOutput b (coSimStart s)

instance {-# OVERLAPPING #-} (CoSim r) => CoSim (CS.Clock dom -> r) where
    coSim _ s streams = coSim True s . parseClock streams

instance {-# OVERLAPPING #-} (CoSimType t, CoSim r) => CoSim (t -> r) where
    coSim b s streams = coSim b s . parseInput b streams

class CoSimType t where
    toSignalStream   :: t -> SignalStream
    fromSignalStream :: SignalStream -> t

instance (ClashType a, CS.KnownDomain dom) => CoSimType (CP.Signal dom a) where
    toSignalStream   = map (wordPack . CP.pack) . CP.sample
    fromSignalStream = CP.fromList . map (CP.unpack . wordUnpack)
