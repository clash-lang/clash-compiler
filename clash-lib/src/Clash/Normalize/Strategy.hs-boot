module Clash.Normalize.Strategy (constantPropagation, normalization, partialEvaluatorBefore) where

import Clash.Normalize.Types (NormRewrite)

normalization :: NormRewrite
constantPropagation :: NormRewrite
partialEvaluatorBefore :: NormRewrite -> NormRewrite
