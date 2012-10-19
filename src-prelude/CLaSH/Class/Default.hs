module CLaSH.Class.Default where

class Default a where
  def :: a
  def = error "No default value"

instance Default Integer where
  def = 0
