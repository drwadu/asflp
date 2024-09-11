module Main where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Lib (Neuron (A, V), approximate, atoms, clarkComplete, complete, dbg, display, displayRanked, down, epsilon, infer, inputs, lnnCmp, lnnify, natoms, parse, parseBodyBounds', parseBounds', pascal, rootify, solveWithAssumptions, solveWithAssumptionsBb, solveWithAssumptionsDebug, solveWithAssumptionsExp, solveWithAssumptionsVis, unlnnify, up, updateBounds)
import System.Environment
import System.Exit

main :: IO ()
main = do
  xs <- getArgs
  if length xs < 2
    then putStrLn "error: provide flag and file path" >> exit
    else do
      f <- readFile $ xs !! 1
      handle (head xs) f

handle :: String -> String -> IO ()
handle "-h" _ = usage >> exit
handle "-v" _ = version >> exit
handle "-c" x = compile x
handle "-ce" x = compileExp x
handle "-s" x = solve x
handle "-se" x = solveExp x
handle "-sb" x = solve' x
handle "-sr" x = solveRanked x
handle "-sv" x = solveVisual x
-- handle "-sp" x = pascal x 100
-- handle "-sve" x = solveVisualExp x
handle a _ = putStrLn ("error: invalid flag " ++ a) >> exit

compile :: String -> IO ()
compile x = dbg ns'
  where
    ns' = lnnify (init uns ++ [root])
    root = updateBounds (1.0, 1.0) $ last uns
    uns = map (g' as') $ unlnnify vns
    vns = map (g as) $ unlnnify ns
    g m' (V a l u) = maybe (V a l u) (\b -> updateBounds b (V a l u)) $ Map.lookup a m'
    g _ n = n
    g' m' (A a xs l u) = maybe (A a xs l u) (\b -> updateBounds b (A a xs l u)) $ Map.lookup a m'
    g' _ n = n
    as' = Map.fromList . map parseBodyBounds' $ filter (\l -> '[' `elem` l) $ filter (\l -> ':' `elem` l) $ filter (\l -> length l > 1) $ lines x
    as = Map.fromList . map parseBounds' $ filter (\l -> '[' `elem` l) $ filter (\l -> ':' `notElem` l) $ filter (\l -> length l > 1) $ lines x
    ns = rootify ns3 ns0
    ns3 = complete m ns2 ns0
    ns2 = ns1 Seq.>< (lnnify $ natoms m ns1)
    ns1 = inputs Map.empty ns0
    ns0 = atoms m
    m = parse Map.empty x

compileExp :: String -> IO ()
compileExp x = dbg ns'
  where
    ns' = lnnify (init uns ++ [root])
    root = updateBounds (1.0, 1.0) $ last uns
    uns = map (g' as') $ unlnnify vns
    vns = map (g as) $ unlnnify ns
    g m' (V a l u) = maybe (V a l u) (\b -> updateBounds b (V a l u)) $ Map.lookup a m'
    g _ n = n
    g' m' (A a xs l u) = maybe (A a xs l u) (\b -> updateBounds b (A a xs l u)) $ Map.lookup a m'
    g' _ n = n
    as' = Map.fromList . map parseBodyBounds' $ filter (\l -> '[' `elem` l) $ filter (\l -> ':' `elem` l) $ filter (\l -> length l > 1) $ lines x
    as = Map.fromList . map parseBounds' $ filter (\l -> '[' `elem` l) $ filter (\l -> ':' `notElem` l) $ filter (\l -> length l > 1) $ lines x
    ns = rootify ns3 ns0
    ns3 = clarkComplete m ns2 ns0
    ns2 = ns1 Seq.>< (lnnify $ natoms m ns1)
    ns1 = inputs Map.empty ns0
    ns0 = atoms m
    m = parse Map.empty x

solveVisual x = mapM_ putStr $ solveWithAssumptionsVis (length ns - 1) ns as
  where
    as = Map.fromList . map parseBounds' $ filter (\l -> '[' `elem` l) $ filter (\l -> length l > 1) $ lines x
    ns = rootify ns3 ns0
    ns3 = complete m ns2 ns0
    ns2 = ns1 Seq.>< (lnnify $ natoms m ns1)
    ns1 = inputs Map.empty ns0
    ns0 = atoms m
    m = parse Map.empty x

solve x = mapM_ putStr $ solveWithAssumptions (length ns - 1) ns as display
  where
    as = Map.fromList . map parseBounds' $ filter (\l -> '[' `elem` l) $ filter (\l -> length l > 1) $ lines x
    ns = rootify ns3 ns0
    ns3 = complete m ns2 ns0
    ns2 = ns1 Seq.>< (lnnify $ natoms m ns1)
    ns1 = inputs Map.empty ns0
    ns0 = atoms m
    m = parse Map.empty x

solveDebug x = solveWithAssumptionsDebug (length ns - 1) ns as
  where
    as = Map.fromList . map parseBounds' $ filter (\l -> '[' `elem` l) $ filter (\l -> length l > 1) $ lines x
    ns = rootify ns3 ns0
    ns3 = complete m ns2 ns0
    ns2 = ns1 Seq.>< (lnnify $ natoms m ns1)
    ns1 = inputs Map.empty ns0
    ns0 = atoms m
    m = parse Map.empty x

solveExp x = mapM_ putStr $ solveWithAssumptionsExp (length ns - 1) ns as as' display
  where
    as' = Map.fromList . map parseBodyBounds' $ filter (\l -> '[' `elem` l) $ filter (\l -> ':' `elem` l) $ filter (\l -> length l > 1) $ lines x
    as = Map.fromList . map parseBounds' $ filter (\l -> '[' `elem` l) $ filter (\l -> not $ ':' `elem` l) $ filter (\l -> length l > 1) $ lines x
    ns = rootify ns3 ns0
    ns3 = clarkComplete m ns2 ns0
    ns2 = ns1 Seq.>< (lnnify $ natoms m ns1)
    ns1 = inputs Map.empty ns0
    ns0 = atoms m
    m = parse Map.empty x

solve' x = mapM_ putStr $ solveWithAssumptionsBb (length ns - 1) ns as as' display
  where
    as' = Map.fromList . map parseBodyBounds' $ filter (\l -> '[' `elem` l) $ filter (\l -> ':' `elem` l) $ filter (\l -> length l > 1) $ lines x
    as = Map.fromList . map parseBounds' $ filter (\l -> '[' `elem` l) $ filter (\l -> not $ ':' `elem` l) $ filter (\l -> length l > 1) $ lines x
    ns = rootify ns3 ns0
    ns3 = complete m ns2 ns0
    ns2 = ns1 Seq.>< (lnnify $ natoms m ns1)
    ns1 = inputs Map.empty ns0
    ns0 = atoms m
    m = parse Map.empty x

solveRanked x = mapM_ putStr $ solveWithAssumptions (length ns - 1) ns as displayRanked
  where
    as = Map.fromList . map parseBounds' $ filter (\l -> '[' `elem` l) $ filter (\l -> length l > 1) $ lines x
    ns = rootify ns3 ns0
    ns3 = complete m ns2 ns0
    ns2 = ns1 Seq.>< (lnnify $ natoms m ns1)
    ns1 = inputs Map.empty ns0
    ns0 = atoms m
    m = parse Map.empty x

usage = putStrLn "usage: asflp [-vhcs] [file_path]"

version = putStrLn "asflp 0.0.1"

exit = exitWith ExitSuccess

initLpP x = ns''
  where
    ns'' = down (length ns' - 1) $ lnnify ns'
    ns' = init uns ++ [root]
    root = updateBounds (1.0, 1.0) $ last uns
    uns = unlnnify $ up vns Nothing
    vns = map (g as) $ unlnnify ns
    f V {} = True
    f _ = False
    g m (V a l u) = maybe (V a l u) (\b -> updateBounds b (V a l u)) $ Map.lookup a as
    g _ n = n
    as' = Map.fromList . map parseBodyBounds' $ filter (\l -> '[' `elem` l) $ filter (\l -> ':' `elem` l) $ filter (\l -> length l > 1) $ lines x
    as = Map.fromList . map parseBounds' $ filter (\l -> '[' `elem` l) $ filter (\l -> ':' `notElem` l) $ filter (\l -> length l > 1) $ lines x
    ns = rootify ns3 ns0
    ns3 = clarkComplete m ns2 ns0
    ns2 = ns1 Seq.>< (lnnify $ natoms m ns1)
    ns1 = inputs Map.empty ns0
    ns0 = atoms m
    m = parse Map.empty x

inferLpP i ns = if lnnCmp ns ns' <= epsilon then Nothing else Just $ infer i ns'
  where
    ns' = approximate i ns
