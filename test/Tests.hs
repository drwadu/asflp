import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Data.Foldable (toList)
import Data.List (find, deleteBy, intercalate)
import Data.Maybe (maybe)

import Lib
  ( Pass (..),
    Value,
    Neuron (..),
    con,
    dis,
    display,
    downwardPass,
    imp,
    infer,
    lnnCmp,
    neg,
    solve,
    solveDebug,
    upwardPass,
    var,
    bounds
  )
import qualified System.Exit as Exit
import Test.HUnit

import System.Random
import Control.Monad

import ImmediateConsequenceOperator ((>*), (>*<), eval, eval')
import qualified H1
import qualified H2

tinyLnn =
  Seq.fromList
    [ --var "a" (Just (0.2 :: Double)) (Just (0.7 :: Double)), -- 0
      --var "b" (Just (0.3 :: Double)) (Just (0.6 :: Double)), -- 1
      var "a" (Just (0.4 :: Double)) (Just (0.4 :: Double)), -- 0
      var "b" (Just (0.6 :: Double)) (Just (0.6 :: Double)), -- 1
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

-- disjunction over rule bodies
-- only do upwardpass and then check disjunction per atom
--
-- a :- -b
-- b :- -a
-- c :- b, -d
-- d :- b, -c
-- c :- e
dorb =
  Seq.fromList
    [ var "a" (Just (0.4 :: Double)) (Just (0.4 :: Double)), -- 0
      var "b" (Just (0.6 :: Double)) (Just (0.6 :: Double)), -- 1
      var "c" Nothing Nothing, -- 2
      var "d" Nothing Nothing, -- 3
      var "e" Nothing Nothing, -- 4
      neg "-a" 0 Nothing Nothing, -- 5
      neg "-b" 1 Nothing Nothing, -- 6
      neg "-c" 2 Nothing Nothing, -- 7
      neg "-d" 3 Nothing Nothing, -- 8
      con "(AND -d b)" [8, 1] Nothing Nothing, -- 9
      con "(AND -c b)" [7, 1] Nothing Nothing, -- 10
      dis "OR (AND -d b) e" [9, 4] Nothing Nothing -- 11
    ]


dorbI =
  Map.fromList
    [ ("a", (0.4, 0.4)),
      ("b", (0.6, 0.6)),
      ("c", (0.0, 1.0)),
      ("d", (0.0, 1.0)),
      ("e", (0.0, 1.0))
    ]

-- conjunction over rules 
-- set this conjunction to true in the downwardpass


-- experiment-getrieben mit einer random interpretation und einem random program (mit fixer atommenge)
-- (signifikanter anteil fuer kleinen anteil (<= 5, start bei 3) an atom) 
-- mit gesampleter interpretation aus einer von 3 klassen
--   * all uncertain
--   * exact pairs 
--   * proper intervals function per hypothesis that compares (fuzzy testing?)
--  * guess per atom a number of rules
--  * guess body per rule
--  * guess literal sign

-- beweis-getrieben

randomBool :: IO Bool
randomBool = head <$> replicateM 1 randomIO

randomBounds :: IO (Double,Double)
randomBounds = do
    l <- head <$> replicateM 1 (getStdRandom (randomR (0.0,1.0)) :: IO Double)
    u <- head <$> replicateM 1 (getStdRandom (randomR (l,1.0)) :: IO Double)
    return (l,u)

randomExactPair :: IO (Double,Double)
randomExactPair = do
    v <- head <$> replicateM 1 (getStdRandom (randomR (0.0,1.0)) :: IO Double)
    return (v,v)

randomInterpretationAllUncertain n = replicateM n (0.0,1.0)

randomInterpretationExactPairs n = replicateM n randomExactPair 

randomInterpretation n = replicateM n randomBounds

randomLiteralFrom atom = do 
    sign <- randomBool
    let lit = if not sign then negate atom else atom
    return lit

randomLit [] = error "empty src"
randomLit [x] = return x
randomLit xs = do 
  i <- getStdRandom (randomR (0,length xs - 1)) :: IO Int
  randomLiteralFrom $ xs !! i
  
incSib b l = negate l `elem` b
--inconsistencies b = findIndices (incSib b) b 
inconsistency b = find (incSib b) b 

deleteOne :: Eq a => a -> [a] -> [a]
deleteOne _ [] = [] -- Nothing to delete
deleteOne x (y:ys) | x == y = ys -- Drop exactly one matching item
deleteOne x (y:ys) = y : deleteOne x ys -- Drop one, but not this one (doesn't match).

deleteMany :: Eq a => [a] -> [a] -> [a]
deleteMany [] = id 
deleteMany (x:xs) = deleteMany xs . deleteOne x 

randomBody src ub = do
  n <- getStdRandom (randomR (1,ub)) :: IO Int
  xs <- replicateM n (randomLit src) 
  -- NOTE: only works if we restrict body to 3 literals
  let xs' = maybe xs (`deleteOne` xs)  $ inconsistency xs
  return $ toList $ Set.fromList xs'
      
tightBody src maxBodySize maxNRules lp atom = do  
    n <- randomRIO (0,maxNRules)
    let body = []
    return (atom, body)

randomFlp src n m = do 
  k <- getStdRandom (randomR (0,n)) :: IO Int
  replicateM (length src) $ replicateM k (randomBody src m)

prettyPrintRandomFlp p = mapM_ putStr $ map (\(i,x) -> toRule (toAtom i) x) $ zip [1..] p
  where
   toRule a xs = concatMap (\x -> a ++ " :- " ++ intercalate ", " (map (\x' -> if x' > 0 then toAtom x' else "-" ++ toAtom (abs x')) x) ++ "\n") xs
   toAtom i = "aux_" ++ show i
  
prettyPrintRandomInterpreatation i = mapM_ putStr $ map (\(j,x) -> toAtom j ++ "[" ++ show (fst x) ++ ";" ++ show (snd x) ++ "]\n") $ zip [1..] i
  where
   toAtom k = "aux_" ++ show k


validateH1 src n m =  do
  p <- randomFlp src n m
  i <- randomInterpretationExactPairs (length src)
  let l = H1.lhs i p
  let (r,nn) = H1.rhs i p
  let v = l == r
  unless v $ do
    putStrLn "----------------------------------------------------------------------"
    putStrLn "FAILED"
    putStrLn ""
    putStrLn "1 IMMEDIATE CONSEQUENCE OPERATOR APPLICATION GIVES:"
    mapM_ print l
    putStrLn "WHEREAS 1 UPWARD PASS ON DISJUNCTIVELY CONJOINED RULE BODIES GIVES:"
    mapM_ print r
    putStrLn ""
    putStrLn "WITH"
    putStrLn ""
    putStrLn "interpretation + program:"
    --mapM_ print $ zip src i
    prettyPrintRandomInterpreatation i
    prettyPrintRandomFlp p
    putStrLn ""
    putStrLn "lnn:"
    mapM_ print $ zip [0..] (toList nn)
    putStrLn "----------------------------------------------------------------------"

validateH2 src n m =  do
  p <- randomFlp src n m
  i <- randomInterpretation (length src)
  let l = H2.lhs i p
  let (r,nn) = H2.rhs i p
  let v = l == r
  unless v $ do
    putStrLn "----------------------------------------------------------------------"
    putStrLn "FAILED"
    putStrLn ""
    putStrLn "1 IMMEDIATE CONSEQUENCE OPERATOR APPLICATION GIVES:"
    mapM_ print l
    putStrLn "WHEREAS 1 UPWARD PASS AND 1 DOWNWARD PASS ON CONJUNCTION OVER RULES GIVES:"
    mapM_ print r
    putStrLn ""
    putStrLn "WITH"
    putStrLn ""
    putStrLn "interpretation:"
    mapM_ print $ zip src i
    putStrLn ""
    putStrLn "program:"
    mapM_ print $ zip src p
    putStrLn ""
    putStrLn "lnn:"
    mapM_ print $ zip [0..] (toList nn)
    putStrLn "----------------------------------------------------------------------"

kValidate k f src n m = replicateM_ k $ f src n m


ceI = [(0.5851016622729768,0.5851016622729768), 
  (0.9193706083381828,0.9193706083381828),
  (0.8848069986113406,0.8848069986113406),
  (0.5053037560549419,0.5053037560549419),
  (0.13565261726760702,0.13565261726760702),
  (0.26745557672062303,0.26745557672062303),
  (0.6313567472460009,0.6313567472460009),
  (0.29527449871282896,0.29527449871282896),
  (0.18388582146117638,0.18388582146117638),
  (0.5362989530719682,0.5362989530719682)
  ]
ceFlp = [[[-2,5]], [[-7]], [[-6,-1]], [[1]], [[-6]], [[-8]], [[-10,-9]], [[-7,-6]], [[-6,-1]], [[-2]]]


-- does order matter?
