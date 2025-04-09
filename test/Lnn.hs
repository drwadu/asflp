module Lnn (
    tiny,
    compile,
    ast,
    upwardPass,
    downwardPass,
    tinyLnn,
    tinyInput,
    tinyLnn',
    Pass (..),
    Neuron (..),
    Lnn (..),
    t0,
    t0Lnn,
    t0Input,
    approximate,
    infer,
    solve,
)
where

import Lnn.Compiler (compile)
import Lnn.Logic (Flp)
import Lnn.Neuro (Lnn (..), Neuron (..))
import Lnn.Parser (parse, parseBounds)
import Lnn.Solver (Pass (..), approximate, downwardPass, infer, solve, upwardPass)

import Test.HUnit

import qualified Data.Map as Map
import qualified Data.Sequence as Seq

tiny = "a :- -b\nb :- -a\nc :- b, -d\nd :- b, -c"
tinyInput = Map.fromList [("a", (0.4, 0.4)), ("b", (0.6, 0.6))]
tinyLnn = compile tiny tinyInput
tinyRoot = And [10, 13, 17, 21] 1.0 1.0 0.0
tinyLnn' = Lnn (Seq.take 22 (ast . upwardPass $ tinyLnn) Seq.|> tinyRoot) 23 (delta tinyLnn)

t0 = "a :- b,-d\nc :- -b,d\ne :- b,h\nf :- d,h\ne :- -d,h\nf :- -b,h"

-- t0Input = Map.fromList [("b",(0.6,0.9)),("c",(0.0,0.0)),("h",(0.8,1.0))]
t0Input = Map.fromList [("a", (0.6, 0.9))]
t0Lnn = compile t0 t0Input
