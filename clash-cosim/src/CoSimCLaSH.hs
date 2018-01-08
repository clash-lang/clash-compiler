{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module CoSimCLaSH
    ( CoSimulator(..)
      ,coSim
      ,coSimCleanUp
      ,coSimDisableStdOut
      ,coSimEnableStdOut
      ,coSimSeq
      ,coSimWithFiles
      ,verilog
      ,wordPack
      ,wordUnpack
      ,mapAccumLM
    ) where

---------------------------
---- IMPORTS --------------
---------------------------

-- Haskell  
import qualified Prelude as P
import qualified Data.List as L
import Data.Maybe


-- CLaSH
import CLaSH.Prelude
import CLaSH.Signal.Explicit

-- FFI
import Foreign 
import Foreign.C

-- GC / IO / Monad
import System.Mem
import System.IO
import System.IO.Unsafe
import Control.Monad

-- Inline
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

--------------------------------------
---- FFI Imports ---------------------
--------------------------------------

foreign import ccall "simStart"         c_simStart      :: Ptr CInt -> CString -> Ptr CString -> IO (Ptr a)
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

-- (HDL, Period, ResetFase, Data, Files, Enable StdOut)
type CoSimSettings          = (Int, Int, Bool, String, [String], Bool)
type CoSimSettings'         = (CoSimSettings, CoSimulator, String)
type CLaSHType a            = (BitPack a, KnownNat (BitSize a), KnownNat (BitSize a + 1), KnownNat (BitSize a + 2))

type SignalStream           = [[Int32]]
data CoSimulator            = Icarus | ModelSim deriving (Show, Eq)

--------------------------------------
---- INLINE --------------------------
--------------------------------------

verilog                     :: QuasiQuoter
verilog                     = createQuasiQuoter' $ inlineCoSim' Icarus

inlineCoSim'                :: CoSimulator -> String -> Q Exp
inlineCoSim' hdl s          = liftM TupE $ sequence [q_hdl, q_period, q_reset, q_data, q_list, q_stdOut]
    where
        q_hdl               = lift (sim2Num hdl :: Int)
        q_period            = lift (20 :: Int)
        q_reset             = lift False
        q_data              = lift s
        q_list              = lift ([] :: [String])
        q_stdOut            = lift True

createQuasiQuoter'          :: (String -> Q Exp) -> QuasiQuoter        
createQuasiQuoter' f        = QuasiQuoter
                                {quoteExp  = f
                                ,quotePat  = undefined
                                ,quoteType = undefined
                                ,quoteDec  = undefined}

--------------------------------------
---- Help-Functions ------------------
--------------------------------------

bool2Num False              = fromIntegral 0
bool2Num True               = fromIntegral 1

sim2Num Icarus              = fromIntegral 1
sim2Num ModelSim            = fromIntegral 2

transposeList ::(Eq a, Num b, Eq b) => b -> [[a]] -> [[a]]
transposeList 0 _           = []
transposeList n xss         = ys : transposeList (n-1) yss
    where 
        ys                  = P.map P.head xss
        yss                 = P.map P.tail xss

mapAccumLM :: (acc -> x -> IO (acc, y)) -> acc -> [x] -> IO (acc, [y])        
mapAccumLM f s xs           = return $ L.mapAccumL (\a xs -> unsafePerformIO $ f a xs) s xs
  
--------------------------------------
---- Array Marshalling ---------------
--------------------------------------  
            
peekArray' :: (Storable a, Integral b) => b -> Ptr a -> IO [a]
peekArray' (-1) _           = error "null-pointer"
peekArray' size ptr         
    | ptr == nullPtr        = error "null-pointer"
    | otherwise             = peekArray (fromIntegral size) ptr
    
pokeArray' :: Storable a => Ptr a -> [a] -> IO ()
pokeArray' ptr []           = return ()
pokeArray' ptr xs 
    | ptr == nullPtr        = error "null-pointer"
    | otherwise             = pokeArray ptr xs

--------------------------------------
---- Co-Simulation -------------------
--------------------------------------

coSimCleanUp :: IO ()
coSimCleanUp                = performGC

coSimEnableStdOut :: CoSimSettings -> CoSimSettings
coSimEnableStdOut settings  = (a,b,c,d,e,True)
    where (a,b,c,d,e,_)     = settings

coSimDisableStdOut :: CoSimSettings -> CoSimSettings
coSimDisableStdOut settings = (a,b,c,d,e,False)
    where (a,b,c,d,e,_)     = settings

coSimWithFiles :: CoSimSettings -> [String] -> CoSimSettings
coSimWithFiles settings fs  = (a,b,c,d,(P.++) e fs,f)
    where (a,b,c,d,e,f)     = settings

coSimSeq :: CoSimSettings -> (Int,Bool) -> [String] -> CoSimSettings
coSimSeq set (p,rst) fs'    = (hdl, fromIntegral p, rst, m, (P.++) fs fs', stdOut)
        where 
            (hdl, _, _, m, fs, stdOut) = set 

coSim :: (CoSim r) => CoSimSettings -> CoSimulator -> String -> r
coSim settings sim top      = coSim' (settings, sim, top) False []

--------------------------------------
---- Co-Simulation MARSHALLING -------
--------------------------------------

coSimMarshall :: CoSimSettings' -> [SignalStream] -> IO (Bool, Ptr CInt, CString, Ptr CString)
coSimMarshall settings xs = do
        
        -- files
        c_f                 <- (newCString m) >>= c_writeToFile
        c_fs                <- mapM newCString d
        let c_files         = c_f : c_fs
        c_filePtrs          <- newArray c_files
        
        -- topEntity & settings
        c_topEntity         <- newCString top
        c_settingsPtr       <- newArray $ P.map fromIntegral $ c_settingsf c_files
        
        -- return
        return (c, c_settingsPtr, c_topEntity, c_filePtrs)
        
    where
        (set,sim,top)       = settings
        (a, b, c, m, d, e)  = set --(HDL, Period, ResetFase, Data, Files, Enable StdOut)
        c_settingsf fs      = [sim2Num sim, a, b, rst, stdOut, lenf fs, lenf xs]
        lenf                = P.length
        rst                 = bool2Num c
        stdOut              = bool2Num e

--------------------------------------
---- Co-Simulation START -------------
--------------------------------------

coSimStart :: CoSimSettings' -> [SignalStream] -> [SignalStream]
coSimStart settings xs  = unsafePerformIO $ do
    
        -- clean up
        coSimCleanUp   
        
        -- marshall c-types
        (rst, c_sPtr, c_topE, c_fPtrs) <- coSimMarshall settings xs
        
        -- start simulation
        c_coSimState'       <- c_simStart c_sPtr c_topE c_fPtrs
        when (c_coSimState' == nullPtr) $ error "Start co-simulation failed"

        -- add finilizer
        c_coSimState        <- newForeignPtr c_simEnd c_coSimState'
        
        -- perform simulation steps
        c_oLength           <- withForeignPtr c_coSimState c_outputLength 
        (_, ys)             <- mapAccumLM coSimStep c_coSimState $ f rst xs
        
        -- transpose and return
        return $ transposeList c_oLength ys
    where
        f r | r             = ([]:) . L.transpose
            | otherwise     = L.transpose

--------------------------------------
---- Co-Simulation STEP --------------
--------------------------------------

coSimStep :: ForeignPtr a -> [[Int32]] -> IO (ForeignPtr a, [[Int32]])
coSimStep state xs          = do
        
        -- write input
        coSimInput state xs
        
        -- perform simulation step
        rv                  <- withForeignPtr state c_simStep
        when (rv /= 0) $ error "Error in co-simulation step"
                
        -- read output 
        ys                  <- coSimOutput state
        
        -- touch state, to keep state alive
        touchForeignPtr state
            
        -- return output    
        return (state, ys)
        
--------------------------------------
---- Co-Simulation INPUT OUTPUT ------
--------------------------------------
        
coSimInput :: ForeignPtr a -> [[Int32]] -> IO ()
coSimInput state xs         = do

    -- check sizes
    c_iLength               <- withForeignPtr state c_inputLength 
    c_iSizes                <- withForeignPtr state c_inputSizes >>= peekArray' c_iLength
    when (f c_iSizes) $ error $ errStr c_iSizes
    
    -- write input
    c_inputPtrs             <- withForeignPtr state c_inputPtr >>= peekArray' c_iLength
    zipWithM_ pokeArray' c_inputPtrs xs'
    
    where
        xs'                 = P.map (P.map fromIntegral) xs
        f                   = not . and . P.zipWith (\x y -> ( x == 0 ) || ( x == y )) iSizes
        iLength             = fromIntegral $ P.length xs
        iSizes              = P.map (fromIntegral . P.length) xs
        errStr ls           = P.concat ["Simulator expects ", show ls, " input words, but ", show iSizes, " given"]
        
        
coSimOutput :: ForeignPtr a -> IO [[Int32]]
coSimOutput state          = do

    -- read output   
    c_oLength               <- withForeignPtr state c_outputLength      
    c_oSizes                <- withForeignPtr state c_outputSizes   >>= peekArray' c_oLength
    c_outputPtrs            <- withForeignPtr state c_outputPtr     >>= peekArray' c_oLength
    ys                      <- zipWithM peekArray' c_oSizes c_outputPtrs
    
    -- convert and return
    return $ P.map (P.map fromIntegral) ys

--------------------------------------
---- CONVERSION ----------------------
--------------------------------------

wordPack    :: (Integral a, Bits a) => a -> [Int32]
wordPack x 
    | isJust size           = snd $ L.mapAccumR wordPack' x [1 .. wordSize]
    | otherwise             = error "Value does not have a fixed bitsize"
    where   
        size                = bitSizeMaybe x
        wordSize            = 1 + shiftR (fromJust size - 1) 5

wordUnpack  :: (Integral a, Bits a) => [Int32] -> a
wordUnpack                  = P.foldl wordUnpack' 0

wordPack'   :: (Integral a, Bits a) => a -> b -> (a, Int32)
wordPack' x _               = (shiftR x 32, fromIntegral x)

wordUnpack' :: (Integral a, Bits a) => a -> Int32 -> a
wordUnpack' x y             = (shiftL x 32) .|. (4294967295 .&. (fromIntegral y))

--------------------------------------
---- PARSING -------------------------
--------------------------------------

parseInput :: CoSimType t => [SignalStream] -> t -> [SignalStream]
parseInput xs x                 = toSignalStream x : xs

parseOutput :: CoSimType t => ([SignalStream] -> [SignalStream]) -> Bool -> [SignalStream] -> ([SignalStream], t)
parseOutput f u xs           
        | qs == []              = error "Simulator expects less output ports"
        | otherwise             = (ys, fromSignalStream y)
        where 
            (y:ys)              = qs
            qs      | u         = xs
                    | otherwise = f $ P.reverse xs
                                
--------------------------------------
---- POLYVARIDIC ---------------------
--------------------------------------        

class CoSim r where

--------------------------------------
---- Func Definitions ----------------
--------------------------------------

    coSim' :: CoSimSettings' -> Bool -> [SignalStream] -> r

--------------------------------------
---- Instances Definitions -----------
--------------------------------------

instance {-# OVERLAPPABLE #-} CoSimType t => CoSim t where
    coSim' s u xs           
        | ys == []              = y'
        | otherwise             = error "Simulator expects more output ports"
        where (ys, y')          = parseOutput (coSimStart s) u xs
             
instance {-# OVERLAPPING #-} (CoSimType t, CoSim r) => CoSim (t, r) where
    coSim' s u xs               = (y', y'')
        where 
            (ys, y')            = parseOutput (coSimStart s) u xs
            y''                 = coSim' s True ys
                    
instance {-# OVERLAPPING #-} (CoSimType t, CoSim r) => CoSim (t -> r) where
    coSim' s u xs               = coSim' s u . parseInput xs
           
--------------------------------------
---- Tupple Definitions --------------
--------------------------------------
            
instance {-# OVERLAPPING #-} CoSim (a,(b,r)) => CoSim (a,b,r) where
    coSim' s u                  = (\(a,(b,r)) -> (a,b,r)) . coSim' s u
        
instance {-# OVERLAPPING #-} CoSim (a,(b,c,r)) => CoSim (a,b,c,r) where
    coSim' s u                  = (\(a,(b,c,r)) -> (a,b,c,r)) . coSim' s u
            
instance {-# OVERLAPPING #-} CoSim (a,(b,c,d,r)) => CoSim (a,b,c,d,r) where
    coSim' s u                  = (\(a,(b,c,d,r)) -> (a,b,c,d,r)) . coSim' s u

--------------------------------------
---- SUPPORTED TYPES -----------------
--------------------------------------      
        
class CoSimType t where

--------------------------------------
---- Func Definitions ----------------
--------------------------------------

    toSignalStream   :: t -> SignalStream
    fromSignalStream :: SignalStream -> t

--------------------------------------
---- Instances Definitions -----------
--------------------------------------

instance {-# OVERLAPPABLE #-} CLaSHType a => CoSimType a  where
    toSignalStream          = (:[]) . wordPack . pack
    fromSignalStream        = unpack . wordUnpack . P.head

instance {-# OVERLAPPING #-} CLaSHType a => CoSimType (Signal' clk a) where
    toSignalStream          = P.map (wordPack . pack) . sample
    fromSignalStream        = fromList . P.map (unpack . wordUnpack)
    
instance {-# OVERLAPPING #-} (Integral a, Bits a) => CoSimType [a] where
    toSignalStream          = P.map wordPack
    fromSignalStream        = P.map wordUnpack












