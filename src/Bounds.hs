module Bounds
  ( bounds,
    updateBounds,
    roundBounds,
    invertBounds,
    sumBounds,
    clampBounds,
  )
where

import Fuzzy (Value, clamp)
import Lukasiewicz (negation)
import Neuron (Neuron (A, I, N, O, V))
import Utils (round')

-- | returns bounds of neuron
bounds :: Neuron -> (Value, Value)
bounds (V _ l u) = (l, u)
bounds (N _ _ l u) = (l, u)
bounds (A _ _ l u) = (l, u)
bounds (O _ _ l u) = (l, u)
bounds (I _ _ _ l u) = (l, u)

-- | updates bounds of neuron
updateBounds :: (Value, Value) -> Neuron -> Neuron
updateBounds (l, u) (V s _ _) = V s l u
updateBounds (l, u) (N s x _ _) = N s x l u
updateBounds (l, u) (A s xs _ _) = A s xs l u
updateBounds (l, u) (O s xs _ _) = O s xs l u
updateBounds (l, u) (I s x y _ _) = I s x y l u

-- | rounds bounds of neuron
roundBounds :: Neuron -> Neuron
roundBounds n = updateBounds (round' l, round' u) n
  where
    (l, u) = bounds n

-- | negates bounds of neuron
invertBounds :: (Value, Value) -> (Value, Value)
invertBounds (a, b) = (negation a, negation b)

-- | sums up bounds of two neurons
sumBounds :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
sumBounds (a, b) (c, d) = (a + c, b + d)

-- | clmaps bounds of neuron
clampBounds :: (Value, Value) -> (Value, Value)
clampBounds (a, b) = (clamp a, clamp b)
