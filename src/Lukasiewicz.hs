module Lukasiewicz
  ( negation,
    tCoNorm,
    tNorm,
    residuum,
  )
where

import Fuzzy (Value, clamp, top)

negation :: Value -> Value
negation = clamp . (-) top

tCoNorm :: Foldable t => t Value -> Value
tCoNorm = clamp . sum

tNorm :: [Value] -> Value
tNorm = clamp . negation . sum . map negation

residuum :: Value -> Value -> Value
residuum x y = clamp $ negation x + y
