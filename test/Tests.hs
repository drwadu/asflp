import Lib (Neuron (A, I, N, O, V), display, infer, lnnify, solve, unlnnify)
import qualified System.Exit as Exit
import Test.HUnit

{--
  a[0.2;0.7]
  b[0.3;0.6]
  a :- -b
  b :- -a
  c :- -d, b
  d :- -c, b
  e :-[0.2;0.4] a, -c
  e :-[0.0;0.0] d, f
  f :- a
  g :-[0.5;0.5] d, f
  h :- -c, a
--}
bn0 =
  [ V "a" 0.2 0.7,
    V "b" 0.3 0.6,
    V "c" 0.0 1.0,
    V "d" 0.0 1.0,
    V "e" 0.0 1.0,
    V "f" 0.0 1.0,
    V "g" 0.0 1.0,
    V "h" 0.0 1.0,
    N "-a" 0 0.0 1.0,
    N "-b" 1 0.0 1.0,
    N "-c" 2 0.0 1.0,
    N "-d" 3 0.0 1.0,
    I "rhs a" 9 0 0.0 1.0,
    I "lhs a" 0 9 0.0 1.0,
    A "proof a" [12, 13] 0.0 1.0,
    I "rhs b" 8 1 0.0 1.0,
    I "lhs b" 1 8 0.0 1.0,
    A "proof b" [15, 16] 0.0 1.0,
    A "(AND -d b)" [11, 1] 0.0 1.0,
    I "rhs c" 18 2 0.0 1.0,
    I "lhs c" 2 18 0.0 1.0,
    A "proof c" [19, 20] 0.0 1.0,
    A "(AND -c b)" [10, 1] 0.0 1.0,
    I "rhs d" 22 3 0.0 1.0,
    I "lhs d" 3 22 0.0 1.0,
    A "proof d" [23, 24] 0.0 1.0,
    A "(AND -c a)" [10, 0] 0.0 1.0,
    A "[e] BRANCH (AND -c a)" [26] 0.2 0.4,
    A "(AND d f)" [3, 5] 0.0 1.0,
    A "[e] BRANCH (AND d f)" [28] 0.0 0.0,
    O "[e] OR (AND -c a) (AND d f)" [27, 29] 0.0 1.0,
    I "rhs e" 30 4 0.0 1.0,
    I "lhs e" 4 30 0.0 1.0,
    A "proof e" [31, 32] 0.0 1.0,
    I "rhs f" 0 5 0.0 1.0,
    I "lhs f" 5 0 0.0 1.0,
    A "proof f" [34, 35] 0.0 1.0,
    A "[g] BRANCH (AND d f)" [28] 0.5 0.5,
    I "rhs g" 37 6 0.0 1.0,
    I "lhs g" 6 37 0.0 1.0,
    A "proof g" [38, 39] 0.0 1.0,
    A "[h] BRANCH (AND -c a)" [26] 0.0 1.0,
    I "rhs h" 39 7 0.0 1.0,
    I "lhs h" 7 39 0.0 1.0,
    A "proof h" [41, 42] 0.0 1.0,
    A "root" [14, 17, 21, 25, 33, 36, 40, 42] 1.0 1.0
  ]

bn1 =
  [ V "a" 0.2 0.7, -- 0
    V "b" 0.3 0.6, -- 1
    V "c" 0.0 1.0, -- 2
    V "d" 0.0 1.0, -- 3
    V "e" 0.0 1.0, -- 4
    V "f" 0.0 1.0, -- 5
    V "g" 0.0 1.0, -- 6
    V "h" 0.0 1.0, -- 7
    N "-a" 0 0.0 1.0, -- 8
    N "-b" 1 0.0 1.0, -- 9
    N "-c" 2 0.0 1.0, -- 10
    N "-d" 3 0.0 1.0, -- 11
    I "rhs a" 9 0 0.0 1.0, -- 12
    I "lhs a" 0 9 0.0 1.0, -- 13
    A "proof a" [12, 13] 0.0 1.0, -- 14
    I "rhs b" 8 1 0.0 1.0, -- 15
    I "lhs b" 1 8 0.0 1.0, -- 16
    A "proof b" [15, 16] 0.0 1.0, -- 17
    A "(AND -d b)" [11, 1] 0.0 1.0, -- 18
    I "rhs c" 18 2 0.0 1.0, -- 19
    I "lhs c" 2 18 0.0 1.0, -- 20
    A "proof c" [19, 20] 0.0 1.0, -- 21
    A "(AND -c b)" [10, 1] 0.0 1.0, -- 22
    I "rhs d" 22 3 0.0 1.0, -- 23
    I "lhs d" 3 22 0.0 1.0, -- 24
    A "proof d" [23, 24] 0.0 1.0, -- 25
    A "{e} (AND -c a)" [10, 0] 0.2 0.4, -- 26
    A "{e} (AND d f)" [3, 5] 0.0 0.0, -- 27
    O "[e] OR (AND -c a) (AND d f)" [26, 27] 0.0 1.0, -- 28
    I "rhs e" 28 4 0.0 1.0, -- 29
    I "lhs e" 4 28 0.0 1.0, -- 30
    A "proof e" [29, 30] 0.0 1.0, -- 31
    I "rhs f" 0 5 0.0 1.0, -- 32
    I "lhs f" 5 0 0.0 1.0, -- 33
    A "proof f" [32, 33] 0.0 1.0, -- 34
    A "{g} (AND d f)" [3, 5] 0.0 1.0, -- 35
    I "rhs g" 35 6 0.0 1.0, -- 36
    I "lhs g" 6 35 0.0 1.0, -- 37
    A "proof g" [36, 37] 0.0 1.0, -- 38
    A "{h} (AND -c a)" [10, 0] 0.0 1.0, -- 39
    I "rhs h" 39 7 0.0 1.0, -- 40
    I "lhs h" 7 39 0.0 1.0, -- 41
    A "proof h" [40, 41] 0.0 1.0, -- 42
    A "root" [14, 17, 21, 25, 31, 34, 38, 42] 1.0 1.0
  ]

nocaption = unlines . map ((++) "\n" . show)

testbn :: Test
testbn = TestCase (assertEqual description expected True)
  where
    description = nocaption $ zip [0 ..] $ unlnnify result
    expected = False
    result = infer (length bn0 - 1) $ lnnify bn0

-- result = infer (length bn1 - 1) $ lnnify bn1

-- result = solve (length bn1 - 1) bn1

tests :: Test
tests = TestList [testbn]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
