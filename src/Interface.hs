{-# LANGUAGE ForeignFunctionInterface #-}

module Interface where

import Lib (
    Neuron,
    complete,
    con,
    find',
    lnnFromCnf,
    neg,
    parse,
    parseBounds',
    solveH1,
    solveWithAssumptionsStr,
    var,
 )

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Foreign.C (CString, newCString, peekCString)

forward :: CString -> IO CString
forward flp = do
    s <- peekCString flp
    let m = parse Map.empty s
    let lnn0 = atoms m
    let lnn1 = inputs Map.empty lnn0
    let lnn2 = lnn1 Seq.>< Seq.fromList (natoms m lnn1)
    let lnn3 = complete m lnn2 lnn0
    let lnn = rootify lnn3 lnn0
    let assumptions = Map.fromList . map parseBounds' $ filter (\x -> '[' `elem` x) $ filter (\x -> length x > 1) $ lines s

    -- _ <- solveWithAssumptions (length lnn - 1) lnn assumptions
    -- return ()
    -- let res = solveWithAssumptionsStr (length lnn - 1) lnn assumptions
    -- newCString res
    -- print assumptions
    newCString $ solveWithAssumptionsStr (length lnn - 1) lnn assumptions

foreign export ccall forward :: CString -> IO CString

atoms m = vs
  where
    vs = Set.toList . Set.fromList $ hs ++ map (\x -> if head x == '-' then tail x else x) bs
    hs = Map.keys m
    bs = concat . concat . Map.foldr (:) [] $ m

inputs as vs = Seq.fromList $ map (atomify as) vs

atomify :: Map.Map String (Double, Double) -> String -> Neuron
atomify as a = uncurry (var a) ret
  where
    ret = case Map.lookup a as of
        Just (x, y) -> (Just x, Just y)
        _ -> (Nothing, Nothing)

natoms m ns = map (\x -> neg x (find' ns (tail x)) Nothing Nothing) vs
  where
    vs = Set.toList . Set.fromList $ filter (\x -> head x == '-') bs
    bs = concat . concat . Map.foldr (:) [] $ m

rootify a b = a Seq.>< Seq.fromList [root a b]
  where
    root a b = con "root" (filter (> 0) $ map (\x -> find' a ("proof " ++ x)) b) Nothing Nothing
