module I2C.Types where

import CLaSH.Prelude
import CLaSH.Signal.Explicit
import CLaSH.Signal.Bundle
import Control.Lens

data I2Ccommand = I2C_NOP | I2C_Start | I2C_Stop | I2C_Read | I2C_Write
  deriving Eq

data I2CIPair
  = I2CIPair
  { _scli, _sdai :: Bit }

makeLenses ''I2CIPair

instance Bundle I2CIPair where
  type Unbundled' clk I2CIPair = (Signal' clk Bit, Signal' clk Bit)
  unbundle' _ i2ci  = (_scli <$> i2ci, _sdai <$> i2ci)
  bundle'   _ (a,b) = I2CIPair <$> a <*> b

data I2COPair
  = I2COPair
  { _sclo   :: Bit
  , _scloEn :: Bool
  , _sdao   :: Bit
  , _sdaoEn :: Bool
  }

makeLenses ''I2COPair

instance Bundle I2COPair where
  type Unbundled' clk I2COPair = (Signal' clk Bit, Signal' clk Bool, Signal' clk Bit, Signal' clk Bool)
  unbundle' _ i2co      = (_sclo <$> i2co, _scloEn <$> i2co, _sdao <$> i2co, _sdaoEn <$> i2co)
  bundle'   _ (a,b,c,d) = I2COPair <$> a <*> b <*> c <*> d
