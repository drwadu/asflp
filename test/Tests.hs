import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Lib
  ( Eval (..),
    Expression (..),
    LnnOperator (..),
    Logic (..),
    LogicalNeuron (..),
    Pass (..),
    Value,
    awp,
    con,
    dis,
    display,
    downwardPass,
    evaluate,
    fpAwp,
    fpFpi,
    fpTp',
    fpi,
    imp,
    infer,
    lnnCmp,
    neg,
    proof,
    solve,
    solveDebug,
    tp,
    tp',
    upwardPass,
    var,
  )
import qualified System.Exit as Exit
import Test.HUnit

tinyLnn =
  Seq.fromList
    [ var "a" (Just (0.2 :: Double)) (Just (0.7 :: Double)), -- 0
      var "b" (Just (0.3 :: Double)) (Just (0.6 :: Double)), -- 1
      -- var "a" (Just (0.4 :: Double)) (Just (1.0 :: Double)), -- 0
      -- var "b" (Just (0.6 :: Double)) (Just (1.0 :: Double)), -- 1
      -- var "c" (Just (0.4 :: Double)) (Just (0.4 :: Double)), -- 0
      -- var "d" (Just (0.2 :: Double)) (Just (0.2 :: Double)), -- 1
      var "c" Nothing Nothing, -- 2
      var "d" Nothing Nothing, -- 3
      neg "-a" 0 Nothing Nothing, -- 4
      neg "-b" 1 Nothing Nothing, -- 5
      neg "-c" 2 Nothing Nothing, -- 6
      neg "-d" 3 Nothing Nothing, -- 7
      imp "lhs a" 5 0 Nothing Nothing, -- 8
      imp "rhs a" 0 5 Nothing Nothing, -- 9
      con "proof a" [8, 9] Nothing Nothing, -- 10
      imp "lhs b" 4 1 Nothing Nothing, -- 11
      imp "rhs b" 1 4 Nothing Nothing, -- 12
      con "proof b" [11, 12] Nothing Nothing, -- 13
      con "(AND -d b)" [7, 1] Nothing Nothing, -- 14
      imp "lhs c" 14 2 Nothing Nothing, -- 15
      imp "rhs c" 2 14 Nothing Nothing, -- 16
      con "proof c" [15, 16] Nothing Nothing, -- 17
      con "(AND -c b)" [6, 1] Nothing Nothing, -- 18
      imp "lhs d" 18 3 Nothing Nothing, -- 19
      imp "rhs d" 3 18 Nothing Nothing, -- 20
      con "proof d" [19, 20] Nothing Nothing, -- 21
      con "completion" [10, 13, 17, 21] Nothing Nothing -- 22
    ]

-- nocaption = unlines . map ((++) "\n" . show)

testTinyLnn :: Test
testTinyLnn = TestCase (assertEqual description expected True)
  where
    description = "" -- nocaption $ zip [0 ..] $ result
    expected = False
    result = tinyLnn

tests :: Test
tests = TestList [testTinyLnn]

-- experiments
--
{-- positive
  p :- q
  q :- r
  r[0.6;0.6]
--}
positive =
  [ var "p" Nothing Nothing, -- 0
    var "q" Nothing Nothing, -- 1
    var "r" (Just (0.6 :: Double)) (Just (0.6 :: Double)), -- 2
    imp "p -> q" 0 1 Nothing Nothing, -- 3
    imp "q -> p" 1 0 Nothing Nothing, -- 4
    con "proof p" [3, 4] Nothing Nothing, -- 5
    imp "q -> r" 1 2 Nothing Nothing, -- 6
    imp "r -> q" 2 1 Nothing Nothing, -- 7
    con "proof q" [6, 7] Nothing Nothing, -- 8
    con "completion" [5, 8] Nothing Nothing
  ]

flp0 =
  Map.fromList
    [ ("p", [[Var "q" 0.0 1.0]]),
      ("q", [[Var "r" 0.0 1.0]]),
      ("r", [[Const 0.6 0.6]])
    ]

flp0I = Map.fromList [("p", (0.0, 1.0)), ("q", (0.0, 1.0)), ("r", (0.0, 1.0))]

flp0T = Map.fromList [("p", (0.0, 0.0)), ("q", (0.0, 0.0)), ("r", (0.0, 0.0))]

{-- normal
  p :- -q
  p :- r
  q :- -p
  q :- s
  r :- s, aux
  r[0.3;0.3]
  aux[0.6;0.6]
--}

normal =
  [ var "p" Nothing Nothing, -- 0
    var "q" Nothing Nothing, -- 1
    var "r" (Just (0.3 :: Double)) (Just (0.3 :: Double)), -- 2
    var "s" Nothing Nothing, -- 3
    var "aux" (Just (0.6 :: Double)) (Just (0.6 :: Double)), -- 4
    neg "-q" 1 Nothing Nothing, -- 5
    dis "-q | r" [5, 2] Nothing Nothing, -- 6
    imp "p -> (-q | r)" 0 6 Nothing Nothing, -- 7
    imp "(-q | r) -> p" 6 0 Nothing Nothing, -- 8
    con "proof p" [7, 8] Nothing Nothing, -- 9
    neg "-p" 0 Nothing Nothing, -- 10
    dis "-p | s" [10, 3] Nothing Nothing, -- 11
    imp "q -> (-p | s)" 1 11 Nothing Nothing, -- 12
    imp "(-p | s) -> q" 11 1 Nothing Nothing, -- 13
    con "proof q" [12, 13] Nothing Nothing, -- 14
    imp "q -> r" 1 2 Nothing Nothing, -- 15
    imp "r -> q" 2 1 Nothing Nothing, -- 16
    con "proof q" [6, 7] Nothing Nothing, -- 17
    con "s & aux" [3, 4] Nothing Nothing, -- 18
    imp "r -> (s & aux)" 2 18 Nothing Nothing, -- 19
    imp "(s & aux) -> r" 18 2 Nothing Nothing, -- 20
    con "proof r" [19, 20] Nothing Nothing, -- 21
    con "completion" [9, 14, 17, 21] Nothing Nothing
  ]

flp1 =
  Map.fromList
    [ ("p", [[Neg $ Var "q" 0.0 1.0], [Var "r" 0.0 1.0]]),
      ("q", [[Neg $ Var "p" 0.0 1.0], [Var "s" 0.0 1.0]]),
      ("r", [[Var "s" 0.0 1.0, Var "aux" 0.0 1.0], [Const 0.3 0.3]]),
      ("aux", [[Const 0.6 0.6]]),
      ("s", [])
      -- ("s", [[Const 0.0 1.0]])
      -- ("s", [[Var "s" 0.0 1.0]])
    ]

flp1I = Map.fromList [("p", (0.0, 1.0)), ("q", (0.0, 1.0)), ("r", (0.0, 1.0)), ("s", (0.0, 1.0)), ("aux", (0.0, 1.0))]

flp1T = Map.fromList [("p", (0.0, 0.0)), ("q", (0.0, 0.0)), ("r", (0.0, 0.0)), ("s", (0.0, 0.0)), ("aux", (0.0, 0.0))]

flp2 =
  Map.fromList
    [ ( "a",
        [ [Neg $ Var "b" 0.0 1.0] -- ,
        -- [Const 0.4 0.4]
        ]
      ),
      ( "b",
        [ [Neg $ Var "a" 0.0 1.0] -- ,
        -- [Const 0.6 0.6]
        ]
      ),
      ("c", [[Var "b" 0.0 1.0, Neg $ Var "d" 0.0 1.0]]),
      ("d", [[Var "b" 0.0 1.0, Neg $ Var "c" 0.0 1.0]])
    ]

-- flp2I = Map.fromList [("a", (0.2, 0.7)), ("b", (0.3, 0.6)), ("c", (0.0, 1.0)), ("d", (0.0, 1.0))]
flp2I = Map.fromList [("a", (0.0, 0.4)), ("b", (0.6, 1.0)), ("c", (0.0, 1.0)), ("d", (0.0, 1.0))]

flp2T = Map.fromList [("a", (0.0, 0.0)), ("b", (0.0, 0.0)), ("c", (0.0, 0.0)), ("d", (0.0, 0.0))]

-- a :- -b
-- b :- -a
-- c :- b, -d
-- d :- b, -c
flp =
  Map.fromList
    [ ( "a",
        [ [Neg $ Var "b" 0.0 1.0],
          [Const 0.4 0.4]
        ]
      ),
      ( "b",
        [ [Neg $ Var "a" 0.0 1.0],
          [Const 0.5 0.5]
        ]
      ),
      ("c", [[Var "b" 0.0 1.0, Neg $ Var "d" 0.0 1.0]]),
      ("d", [[Var "b" 0.0 1.0, Neg $ Var "c" 0.0 1.0]])
    ]

flpI = Map.fromList [("a", (0.0, 1.0)), ("b", (0.0, 1.0)), ("c", (0.0, 1.0)), ("d", (0.0, 1.0))]

flpT = Map.fromList [("a", (0.0, 0.0)), ("b", (0.0, 0.0)), ("c", (0.0, 0.0)), ("d", (0.0, 0.0))]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

{-- A --}
tinyOneDirectionA =
  Seq.fromList
    [ var "a" (Just (0.4 :: Double)) (Just (0.4 :: Double)), -- 0
      var "b" (Just (0.6 :: Double)) (Just (0.6 :: Double)), -- 1
      var "c" Nothing Nothing, -- 2
      var "d" Nothing Nothing, -- 3
      neg "-a" 0 Nothing Nothing, -- 4
      neg "-b" 1 Nothing Nothing, -- 5
      neg "-c" 2 Nothing Nothing, -- 6
      neg "-d" 3 Nothing Nothing, -- 7
      imp "a :- -b" 5 0 Nothing Nothing, -- 8
      imp "b :- -a" 4 1 Nothing Nothing, -- 9
      con "(AND -d b)" [7, 1] Nothing Nothing, -- 10
      imp "c :- -d, b" 10 2 Nothing Nothing, -- 11
      con "(AND -c b)" [6, 1] Nothing Nothing, -- 12
      imp "d :- -c, b" 12 3 Nothing Nothing, -- 13
      con "flp" [8, 9, 11, 13] Nothing Nothing -- 14
    ]

tinyOneDirectionA' =
  Seq.fromList
    [ var "a" (Just (0.4 :: Double)) (Just (0.4 :: Double)), -- 0
      var "b" (Just (0.6 :: Double)) (Just (0.6 :: Double)), -- 1
      var "c" Nothing Nothing, -- 2
      var "d" Nothing Nothing, -- 3
      neg "-a" 0 (Just (0.6 :: Double)) (Just (0.6 :: Double)), -- 4
      neg "-b" 1 (Just (0.4 :: Double)) (Just (0.4 :: Double)), -- 5
      neg "-c" 2 Nothing Nothing, -- 6
      neg "-d" 3 Nothing Nothing, -- 7
      imp "a :- -b" 5 0 (Just (1.0 :: Double)) (Just (1.0 :: Double)), -- 8
      imp "b :- -a" 4 1 (Just (1.0 :: Double)) (Just (1.0 :: Double)), -- 9
      con "(AND -d b)" [7, 1] (Just (0.0 :: Double)) (Just (0.6 :: Double)), -- 10
      imp "c :- -d, b" 10 2 (Just (0.4 :: Double)) (Just (1.0 :: Double)), -- 11
      con "(AND -c b)" [6, 1] (Just (0.0 :: Double)) (Just (0.6 :: Double)), -- 12
      imp "d :- -c, b" 12 3 (Just (0.4 :: Double)) (Just (1.0 :: Double)), -- 13
      con "flp" [8, 9, 11, 13] (Just (1.0 :: Double)) (Just (1.0 :: Double)) -- 14
    ]

-- bla =
--  Seq.fromList
--    [ var "a" (Just (1.0 :: Double)) (Just (1.0 :: Double)), -- 0
--      var "b" (Just (1.0 :: Double)) (Just (1.0 :: Double)), -- 1
--      var "c" (Just (0.4 :: Double)) (Just (1.0 :: Double)), -- 2
--      var "d" (Just (0.4 :: Double)) (Just (1.0 :: Double)), -- 3
--      con "a & b & c & d" [0, 1, 2, 3] Nothing Nothing -- 4
--    ]

aA = upwardPass tinyOneDirectionA

aA' = downwardPass 14 tinyOneDirectionA'

{-- B --}
tinyOneDirectionB =
  Seq.fromList
    [ var "a" (Just (0.4 :: Double)) (Just (0.4 :: Double)), -- 0
      var "b" (Just (0.6 :: Double)) (Just (0.6 :: Double)), -- 1
      var "c" Nothing Nothing, -- 2
      var "d" Nothing Nothing, -- 3
      neg "-a" 0 Nothing Nothing, -- 4
      neg "-b" 1 Nothing Nothing, -- 5
      neg "-c" 2 Nothing Nothing, -- 6
      neg "-d" 3 Nothing Nothing, -- 7
      imp "a :- -b" 5 0 Nothing Nothing, -- 8
      imp "b :- -a" 4 1 Nothing Nothing, -- 9
      con "(AND -d b)" [7, 1] Nothing Nothing, -- 10
      imp "c :- -d, b" 10 2 Nothing Nothing, -- 11
      con "(AND -c b)" [6, 1] Nothing Nothing, -- 12
      imp "d :- -c, b" 12 3 Nothing Nothing -- 13
    ]

tinyOneDirectionB' =
  Seq.fromList
    [ var "a" (Just (0.4 :: Double)) (Just (0.4 :: Double)), -- 0
      var "b" (Just (0.6 :: Double)) (Just (0.6 :: Double)), -- 1
      var "c" Nothing Nothing, -- 2
      var "d" Nothing Nothing, -- 3
      neg "-a" 0 (Just (0.6 :: Double)) (Just (0.6 :: Double)), -- 4
      neg "-b" 1 (Just (0.4 :: Double)) (Just (0.4 :: Double)), -- 5
      neg "-c" 2 Nothing Nothing, -- 6
      neg "-d" 3 Nothing Nothing, -- 7
      imp "a :- -b" 5 0 (Just (1.0 :: Double)) (Just (1.0 :: Double)), -- 8
      imp "b :- -a" 4 1 (Just (1.0 :: Double)) (Just (1.0 :: Double)), -- 9
      con "(AND -d b)" [7, 1] (Just (0.0 :: Double)) (Just (0.6 :: Double)), -- 10
      -- imp "c :- -d, b" 10 2 (Just 0.4, Just 1.0), -- 11
      imp "c :- -d, b" 10 2 (Just (1.0 :: Double)) (Just (1.0 :: Double)), -- 11
      con "(AND -c b)" [6, 1] (Just (0.0 :: Double)) (Just (0.6 :: Double)), -- 12
      -- imp "d :- -c, b" 12 3 (Just 0.4, Just 1.0) -- 13
      imp "d :- -c, b" 12 3 (Just (1.0 :: Double)) (Just (1.0 :: Double)) -- 13
    ]

aB = upwardPass tinyOneDirectionB

aB' = downwardPass 13 tinyOneDirectionB'

bB = fpTp' tinyTpI tinyTp

{-- C --}
tinyTp =
  Map.fromList
    [ ( "a",
        [ [Neg $ Var "b" 0.0 1.0]
        ]
      ),
      ( "b",
        [ [Neg $ Var "a" 0.0 1.0]
        ]
      ),
      ("c", [[Var "b" 0.0 1.0, Neg $ Var "d" 0.0 1.0]]),
      ("d", [[Var "b" 0.0 1.0, Neg $ Var "c" 0.0 1.0]])
    ]

tinyTpI =
  Map.fromList
    [ ("a", (0.4, 0.4)),
      ("b", (0.6, 0.6)),
      ("c", (0.0, 1.0)),
      ("d", (0.0, 1.0))
    ]

aC = upwardPass tinyOneDirectionA

aC' = downwardPass 14 $ upwardPass tinyOneDirectionA

bC = fpTp' tinyTpI tinyTp

tinyLn =
  Conjunction
    [ proof (Variable "a") (Negation . Variable $ "b"),
      proof (Variable "b") (Negation . Variable $ "a"),
      proof (Variable "c") (Conjunction [Variable "b", Negation . Variable $ "d"]),
      proof (Variable "d") (Conjunction [Variable "b", Negation . Variable $ "c"])
    ]

bla0 = Implication (Variable "a") (Negation . Variable $ "b")

bla1 = Implication (Negation . Variable $ "b") (Variable "a")

tinyI =
  Map.fromList
    [ ("a", (0.2, 0.7)),
      ("b", (0.3, 0.6)),
      ("c", (0.0, 1.0)),
      ("d", (0.0, 1.0))
    ]

tinyJ =
  Map.fromList
    [ ("a", (0.4, 0.4)),
      ("b", (0.6, 0.6)),
      ("c", (0.0, 1.0)),
      ("d", (0.0, 1.0))
    ]

tiny0 =
  Map.fromList
    [ ("a", (0.0, 1.0)),
      ("b", (0.0, 1.0)),
      ("c", (0.0, 1.0)),
      ("d", (0.0, 1.0))
    ]
