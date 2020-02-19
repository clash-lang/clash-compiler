module Clash.Normalize.Strategy (constantPropagation, normalization, transPropagateLocal) where

import Clash.Normalize.Types (NormRewrite)

normalization :: NormRewrite
constantPropagation :: NormRewrite
transPropagateLocal :: NormRewrite
