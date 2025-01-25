module Main where

import qualified Data.Map as Map
import Lnn.Parser
  ( parseBounds
  )
import Lnn.Solver
  ( solve
  , solveDbg
  )
import System.Environment
import System.Exit


main :: IO ()
main = do
  xs <- getArgs
  case length xs of
    0 -> putStrLn "error: provide at least file path" >> exit
    1 -> do  
        f <- readFile . head $ xs
        run (head xs) f
    2 -> do  
        f <- readFile $ xs !! 1
        run (head xs) f
    _ -> putStrLn "error: provide at least file path and at most one known flag" >> exit


run :: String -> String -> IO ()
run "-h" _ = usage >> exit
run "-v" _ = version >> exit
run "-dbg" x = solveLnnDbg x
run _ x = solveLnn x


usage = putStrLn "usage: asflp [-dbg,-ico] file_path"


version = putStrLn "asflp 0.0.1"


exit = exitSuccess


solveLnn flp = do
  version
  putStrLn ""
  putStrLn "ASP -> completion -> LNN inference"
  putStrLn ""
  _ <- solve  flp (readInputs flp) 
  return ()


solveLnnDbg flp = do
  version
  putStrLn ""
  putStrLn "ASP -> completion -> LNN inference"
  putStrLn ""
  _ <- solveDbg flp (readInputs flp)
  return ()


readInputs flp = Map.fromList . map parseBounds $ filter (\x -> '[' `elem` x) $ filter (\x -> length x > 1) $ lines flp

