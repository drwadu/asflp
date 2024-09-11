{-# LANGUAGE DeriveDataTypeable #-}

module Visual (dot) where

import Data.Data
import Lnn (Neuron (A, I, N, O, V))
import Utils (round')

data Style = Variable | Not | And | Or | Implication deriving (Eq, Show, Data, Typeable)

dotStyle :: Style -> String
dotStyle enum = case enum of
  Variable -> "\" shape=box width=0.5]\n"
  Not -> "\" shape=triangle style=filled fillcolor=gray width=0.5]\n"
  And -> "\" shape=rectangle style=filled fillcolor=cornflowerblue width=0.5]\n"
  Or -> "\" shape=circle style=filled fillcolor=aquamarine4 width=0.5]\n"
  Implication -> "\" shape=circle style=filled fillcolor=gold4 width=0.5]\n"

edge :: (Show a1, Show a2) => a1 -> a2 -> [Char]
edge i j = "N" ++ show i ++ " -> " ++ "N" ++ show j ++ "\n"

dot :: Show a => (a, Neuron) -> [Char]
dot (i, V a l u) = "N" ++ show i ++ " [label=\"" ++ a ++ show (round' l, round' u) ++ dotStyle Variable
dot (i, N _ j l u) = "N" ++ show i ++ " [label=\"" ++ show (round' l, round' u) ++ dotStyle Not ++ edge i j
dot (i, A _ xs l u) = "N" ++ show i ++ " [label=\"" ++ show (round' l, round' u) ++ dotStyle And ++ concatMap (edge i) xs
dot (i, O _ xs l u) = "N" ++ show i ++ " [label=\"" ++ show (round' l, round' u) ++ dotStyle Or ++ concatMap (edge i) xs
dot (i, I _ x y l u) = "N" ++ show i ++ " [label=\"" ++ show (round' l, round' u) ++ dotStyle Implication ++ concatMap (edge i) [x, y]
