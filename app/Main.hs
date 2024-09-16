module Main where

import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Lib
  ( Neuron,
    Value,
    complete,
    con,
    find',
    neg,
    parse,
    parseBounds',
    solveWithAssumptions,
    var,
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
run "-s" x = solveLnn x
run "-sd" x = undefined
run a _ = putStrLn ("error: invalid flag " ++ a) >> exit

usage = putStrLn "usage: asflp [-vhcs] [file_path]"

version = putStrLn "asflp 0.0.1"

exit = exitSuccess

solveLnn flp = do
  version
  putStrLn ""
  putStrLn "ASP -> completion -> LNN inference"
  putStrLn ""
  _ <- solveWithAssumptions (length lnn - 1) lnn assumptions
  return ()
  where
    assumptions = Map.fromList . map parseBounds' $ filter (\x -> elem '[' x) $ filter (\x -> length x > 1) $ lines $ flp
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
    hs = Map.keys m
    bs = concat . concat . Map.foldr (:) [] $ m

rootify a b = a Seq.>< (Seq.fromList [root a b])
  where
    root a b = con "root" (filter (> 0) $ map (\x -> find' a ("proof " ++ x)) b) Nothing Nothing
