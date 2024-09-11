module Visual (dot) where

import Lnn (Neuron (A, I, N, O, V))
import Utils (round')

edge i j = "N" ++ show i ++ " -> " ++ "N" ++ show j ++ "\n"

dot (i, (V a l u)) = "N" ++ show i ++ " [label=\"" ++ a ++ show (round' l, round' u) ++ "\" shape=box width=0.5]\n"
dot (i, (N _ j l u)) = "N" ++ show i ++ " [label=\"" ++ show (round' l, round' u) ++ "\" shape=triangle style=filled fillcolor=gray width=0.5]\n" ++ edge i j
dot (i, (A _ xs l u)) = "N" ++ show i ++ " [label=\"" ++ show (round' l, round' u) ++ "\" shape=rectangle style=filled fillcolor=cornflowerblue width=0.5]\n" ++ (concat $ map (edge i) xs)
dot (i, (O _ xs l u)) = "N" ++ show i ++ " [label=\"" ++ show (round' l, round' u) ++ "\" shape=circle style=filled fillcolor=aquamarine4 width=0.5]\n" ++ (concat $ map (edge i) xs)
dot (i, (I _ x y l u)) = "N" ++ show i ++ " [label=\"" ++ show (round' l, round' u) ++ "\" shape=circle style=filled fillcolor=gold4 width=0.5]\n" ++ (concat $ map (edge i) [x, y])
