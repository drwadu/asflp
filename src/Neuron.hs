module Neuron
  ( Neuron
      ( V,
        N,
        A,
        O,
        I
      ),
    repr,
  )
where

import Fuzzy (Value)

-- | neuron
data Neuron
  = V String Value Value
  | N String Int Value Value
  | A String [Int] Value Value
  | O String [Int] Value Value
  | I String Int Int Value Value
  deriving (Ord, Eq, Show)

-- | helper function to display neuron's description
repr :: Neuron -> String
repr (V s _ _) = s
repr (N s _ _ _) = s
repr (A s _ _ _) = s
repr (O s _ _ _) = s
repr (I s _ _ _ _) = s
