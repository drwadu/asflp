module H1 where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import System.Random
import Control.Monad


import Lib
  ( Expression (..),
    Neuron,
    Value,
    awp,
    complete,
    con,
    evaluate,
    find',
    fpAwp,
    fpFpi,
    fpi,
    neg,
    parse,
    parseBounds',
    solveWithAssumptions,
    tp,
    tp',
    var,
  )



randomLiteralFrom atom = (sign:lit)
  where
    sign = 
      case replicateM 1 randomIO of
        [False] -> '-'
        _ -> '\0'
      

tightBody src maxBodySize lp atom = undefined

    
  

--solveLnn flp = do
--  putStrLn ""
--  putStrLn "ASP -> completion -> LNN inference"
--  putStrLn ""
--  _ <- solveWithAssumptions (length lnn - 1) lnn assumptions
--  return ()
--  where
--    assumptions = Map.fromList . map parseBounds' $ filter (\x -> elem '[' x) $ filter (\x -> length x > 1) $ lines $ flp
--    lnn = rootify lnn3 lnn0
--    lnn3 = complete m lnn2 lnn0
--    lnn2 = lnn1 Seq.>< Seq.fromList (natoms m lnn1)
--    lnn1 = inputs Map.empty lnn0
--    lnn0 = atoms m
--    m = parse Map.empty flp
--
