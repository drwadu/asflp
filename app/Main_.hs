module Main where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Lib
  ( Neuron (..)
  , complete
  , con
  , find'
  , lnnFromCnf
  , neg
  , parse
  , parseBounds'
  , solveH1
  , solveWithAssumptions
  , var
  )
import System.Environment
import System.Exit


main :: IO ()
main = do
  xs <- getArgs
  if length xs < 2
    then putStrLn "error: provide flag and file path" >> exit
    else do
      f <- readFile $ xs !! 1
      run (head xs) f


run :: String -> String -> IO ()
run "-h" _ = usage >> exit
run "-v" _ = version >> exit
run "-lnn" x = solveLnn x
run "-aw" x = solveAw x
run "-sd" x = undefined
run "-lnncnf" x = solveCnf x
run a _ = putStrLn ("error: invalid flag " ++ a) >> exit


usage = putStrLn "usage: asflp [-vhcs] [file_path]"


version = putStrLn "asflp 0.0.1"


exit = exitSuccess


solveLnn flp = do
  version
  putStrLn ""
  putStrLn "ASP -> completion -> LNN inference"
  putStrLn ""
  res <- solveWithAssumptions (length lnn - 1) lnn assumptions
  print res
  return ()
 where
  -- lnn = tinyws
  assumptions = Map.fromList . map parseBounds' $ filter (\x -> '[' `elem` x) $ filter (\x -> length x > 1) $ lines flp
  lnn = rootify lnn3 lnn0
  lnn3 = complete m lnn2 lnn0
  lnn2 = lnn1 Seq.>< Seq.fromList (natoms m lnn1)
  lnn1 = inputs Map.empty lnn0
  lnn0 = atoms m
  m = parse Map.empty flp


compile_ flp inputs = lnn
 where
  lnn = (map (conditionTo assumptions) . toList) $ rootify lnn3 lnn0
  lnn3 = complete m lnn2 lnn0
  lnn2 = lnn1 Seq.>< Seq.fromList (natoms m lnn1)
  lnn1 = inputs Map.empty lnn0
  lnn0 = atoms m
  m = parse Map.empty flp
  conditionTo m (V s l u) = maybe (V s l u) (uncurry (update (V s l u))) $ Map.lookup s m
  conditionTo _ n = n


solveAw flp = do
  version
  putStrLn ""
  putStrLn "aw"
  putStrLn ""
  -- _ <- solveWithAssumptions (length lnn - 1) lnn assumptions
  _ <- solveH1 lnn assumptions
  return ()
 where
  assumptions = Map.fromList . map parseBounds' $ filter (\x -> '[' `elem` x) $ filter (\x -> length x > 1) $ lines flp
  lnn = rootify lnn3 lnn0
  lnn3 = complete m lnn2 lnn0
  lnn2 = lnn1 Seq.>< Seq.fromList (natoms m lnn1)
  lnn1 = inputs Map.empty lnn0
  lnn0 = atoms m
  m = parse Map.empty flp


inputs as vs = Seq.fromList $ map (atomify as) vs


-- atomify :: (Eval a) => Map.Map String (a, a) -> String -> Neuron a
-- atomify :: Map.Map String (Double, Double) -> String -> Neuron Double
atomify :: Map.Map String (Double, Double) -> String -> Neuron
atomify as a = uncurry (var a) ret
 where
  ret = case Map.lookup a as of
    Just (x, y) -> (Just x, Just y)
    _ -> (Nothing, Nothing)


atoms m = vs
 where
  vs = Set.toList . Set.fromList $ hs ++ map (\x -> if head x == '-' then tail x else x) bs
  hs = Map.keys m
  bs = concat . concat . Map.foldr (:) [] $ m


natoms m ns = map (\x -> neg x (find' ns (tail x)) Nothing Nothing) vs
 where
  vs = Set.toList . Set.fromList $ filter (\x -> head x == '-') bs
  bs = concat . concat . Map.foldr (:) [] $ m


rootify a b = a Seq.>< (Seq.fromList [root a b])
 where
  root a b = con "root" (filter (> 0) $ map (\x -> find' a ("proof " ++ x)) b) Nothing Nothing


solveCnf flp = do
  version
  putStrLn ""
  putStrLn "ASP -> CNF -> completion -> LNN inference"
  putStrLn ""
  putStrLn $ "#neurons: " ++ (show $ Seq.length lnn)
  putStrLn ""
  -- _ <- mapM_ (putStrLn . show) lnn
  solveWithAssumptions (length lnn - 1) lnn (Map.fromList [("a", (0.4 :: Double, 0.4 :: Double)), ("b", (0.6 :: Double, 0.6 :: Double))])
 where
  -- solveWithAssumptions (length lnn - 1) lnn (Map.fromList [("digitL(3)",(1.0 :: Double, 1.0 :: Double)),("digitR(6)",(1.0 :: Double, 1.0 :: Double))])
  -- solveWithAssumptions (length lnn - 1) lnn (Map.fromList [("predicted_sum(10)",(1.0 :: Double, 1.0 :: Double))])
  -- solveWithAssumptions (length lnn - 1) lnn (Map.fromList [("digitL(3)",(1.0 :: Double, 1.0 :: Double))])
  -- solveWithAssumptions (length lnn - 1) lnn (Map.fromList [("predicted_sum(0)",(0.5 :: Double, 0.5 :: Double)),("predicted_sum(18)",(0.5 :: Double, 0.5 :: Double))])
  -- solveWithAssumptions (length lnn - 1) lnn (Map.fromList [("action(up,0)",(1.0 :: Double, 1.0 :: Double)),("monster(1,2,1)",(1.0 :: Double, 1.0 :: Double)),("agent(1,2,1)",(1.0 :: Double, 1.0 :: Double))])
  -- solveWithAssumptions (length lnn - 1) lnn (Map.fromList [("fell_off",(1.0 :: Double, 1.0 :: Double))])

  lnn = lnnFromCnf flp


tinyws =
  Seq.fromList
    [ V{_s = "a", _l = 0.0, _u = 1.0}
    , V{_s = "b", _l = 0.0, _u = 1.0}
    , V{_s = "c", _l = 0.0, _u = 1.0}
    , V{_s = "d", _l = 0.0, _u = 1.0}
    , N{_s = "-a", _x = 0, _l = 0.0, _u = 1.0}
    , N{_s = "-b", _x = 1, _l = 0.0, _u = 1.0}
    , N{_s = "-c", _x = 2, _l = 0.0, _u = 1.0}
    , N{_s = "-d", _x = 3, _l = 0.0, _u = 1.0}
    , I{_s = "rhs a", _x = 5, _y = 0, _l = 0.0, _u = 1.0, _ws = [1.0, 1.0]}
    , I{_s = "lhs a", _x = 0, _y = 5, _l = 0.0, _u = 1.0, _ws = [1.0, 1.0]}
    , A{_s = "proof a", _xs = [8, 9], _l = 0.0, _u = 1.0, _ws = [1.0, 1.0]}
    , I{_s = "rhs b", _x = 4, _y = 1, _l = 0.0, _u = 1.0, _ws = [1.0, 1.0]}
    , I{_s = "lhs b", _x = 1, _y = 4, _l = 0.0, _u = 1.0, _ws = [1.0, 1.0]}
    , A{_s = "proof b", _xs = [11, 12], _l = 0.0, _u = 1.0, _ws = [1.0, 1.0]}
    , A{_s = "( _ :- -d b)", _xs = [7, 1], _l = 0.0, _u = 1.0, _ws = [1.0, 0.9]}
    , I{_s = "rhs c", _x = 14, _y = 2, _l = 0.0, _u = 1.0, _ws = [1.0, 1.0]}
    , I{_s = "lhs c", _x = 2, _y = 14, _l = 0.0, _u = 1.0, _ws = [1.0, 1.0]}
    , A{_s = "proof c", _xs = [15, 16], _l = 0.0, _u = 1.0, _ws = [1.0, 1.0]}
    , A{_s = "( _ :- -c b)", _xs = [6, 1], _l = 0.0, _u = 1.0, _ws = [1.0, 1.0]}
    , I{_s = "rhs d", _x = 18, _y = 3, _l = 0.0, _u = 1.0, _ws = [1.0, 1.0]}
    , I{_s = "lhs d", _x = 3, _y = 18, _l = 0.0, _u = 1.0, _ws = [1.0, 1.0]}
    , A{_s = "proof d", _xs = [19, 20], _l = 0.0, _u = 1.0, _ws = [1.0, 1.0]}
    , A{_s = "root", _xs = [10, 13, 17, 21], _l = 0.0, _u = 1.0, _ws = [1.0, 1.0, 1.0, 1.0]}
    ]
