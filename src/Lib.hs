module Lib
  ( con,
    neg,
    dis,
    imp,
    var,
    Value,
    Pass (..),
    upwardPass,
    downwardPass,
    solve,
    solveDebug,
    parseBounds',
    parse,
    find',
    Neuron (A, V, N, O, I, _s, _xs, _l, _u, _x, _y),
    complete,
    solveWithAssumptions,
    solveWithAssumptionsStr,
    infer,
    display,
    lnnCmp,
    inferDebug,
    bounds,
    solveH1,
    update,
    lnnFromCnf
  )
where

import Data.Either
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Fuzzy (Eval (..), Logic (..), Value)
import Inference
  ( Pass (..),
    display,
    displayRaw,
    downwardPass,
    infer,
    inferDebug,
    lnnCmp,
    upwardPass,
  )
import Neuron
  ( Neuron (A, V, N, O, I, _s, _xs, _l, _u, _x, _y),
    con,
    dis,
    imp,
    neg,
    update,
    var,
    bounds
  )
import Parser
  ( parse,
    parseBounds',
  )
import Data.List (findIndex)


--solveWithAssumptionsStr :: (Monad m, Foldable t) => Int -> t Neuron -> Map.Map String (Double, Double) -> m String
solveWithAssumptionsStr i lnn assumptions = do
  x <- inferDebug i ns'
  --l <- show $ length $ concatMap displayRaw $ toList x
  --return l
  l <-  concatMap displayRaw $ toList x
  return l
  where
    ns' = Seq.take i ns Seq.|> root
    root = update (fromMaybe (error "") $ Seq.lookup i ns) (1.0 :: Double) (1.0 :: Double)
    ns = upwardPass $ (map (conditionTo assumptions) . toList) lnn
    isVar V {} = True
    isVar _ = False
    conditionTo m (V s l u) = maybe (V s l u) (uncurry (update (V s l u))) $ Map.lookup s m
    conditionTo _ n = n

solveWithAssumptions i lnn assumptions = do
  --mapM_ (putStr . display) ns
  mapM_ print $ toList lnn
  mapM_ (putStr . display) $ filter (\n -> (_u n - _l n) /= 1.0 && (not $ List.isPrefixOf "aux_" (_s n))) $ toList ns
  putStrLn ""
  x <- inferDebug i ns'
  --mapM_ (putStr . display) x
  --mapM_ (putStr . display) $ filter (\n -> (_u n - _l n) /= 1.0 && (not $ List.isPrefixOf "aux_" (_s n))) $ toList x
  mapM_ (putStr . display) $ filter (\n -> (not ((_l n == 0.0) && (_u n == 1.0))) && (not $ List.isPrefixOf "aux_" (_s n))) $ toList x
  where
    ns' = Seq.take i ns Seq.|> root
    root = update (fromMaybe (error "") $ Seq.lookup i ns) (1.0 :: Double) (1.0 :: Double)
    ns = upwardPass $ (map (conditionTo assumptions) . toList) lnn
    isVar V {} = True
    isVar _ = False
    conditionTo m (V s l u) = maybe (V s l u) (uncurry (update (V s l u))) $ Map.lookup s m
    conditionTo _ n = n

solveH1 lnn assumptions = do
  mapM_ (putStr . display) ns
  putStrLn ""
  mapM_ print ns
  where
    ns = upwardPass $ (map (conditionTo assumptions) . toList) lnn
    conditionTo m (V s l u) = maybe (V s l u) (uncurry (update (V s l u))) $ Map.lookup s m
    conditionTo _ n = n

solve i lnn = infer i lnn'
  where
    lnn' = downwardPass i ns'
    ns' = Seq.take i ns Seq.|> root
    root = update (fromMaybe (error "") $ Seq.lookup i ns) (1.0 :: Double) (1.0 :: Double)
    ns = upwardPass lnn

solveDebug i lnn = do
  mapM_ print $ toList lnn
  putStrLn ""
  mapM_ print $ toList lnn'
  inferDebug i lnn'
  where
    lnn' = downwardPass i ns'
    ns' = Seq.take i ns Seq.|> root
    root = update (fromMaybe (error "") $ Seq.lookup i ns) (1.0 :: Double) (1.0 :: Double)
    ns = upwardPass lnn
    isVar (V {}) = True
    isVar _ = False

find' ns x = fromMaybe (-1) $ Seq.findIndexL (\n -> _s n == x) ns

findNeuron ns x = Seq.index ns <$> Seq.findIndexL (\n -> _s n == x) ns

findNeuronByIndex = Seq.index

andify ns xs =
  if length xs > 1
    then case findNeuron ns s of
      Just n -> Left . fromMaybe (-1) . Seq.findIndexL (\n' -> _s n' == _s n) $ ns
      _ -> Right (con s (map (find' ns) xs') Nothing Nothing)
    else Left . find' ns . head $ xs
  where
    s = "(AND " ++ unwords xs' ++ ")"
    xs' = List.sort xs

justification assumptions lnn atom = if rhs /= [] then res else []
  where
    v = find' lnn atom
    j = cl + length ands + 1
    res
      | lchi > 1 =
          ands
            ++ [ dis (atom ++ " OR " ++ unwords (map (_s . findNeuronByIndex lnn) idxs ++ map _s ands)) chi Nothing Nothing,
                 imp ("rhs " ++ atom) j v Nothing Nothing,
                 imp ("lhs " ++ atom) v j Nothing Nothing,
                 con ("proof " ++ atom) [j + 1, j + 2] Nothing Nothing
               ]
      | not (null ands) =
          ands
            ++ [ imp ("rhs " ++ atom) (cl + 1) v Nothing Nothing,
                 imp ("lhs " ++ atom) v (cl + 1) Nothing Nothing,
                 con ("proof " ++ atom) [cl + 2, cl + 3] Nothing Nothing
               ]
      | otherwise =
          [ imp ("rhs " ++ atom) (head chi) v Nothing Nothing,
            imp ("lhs " ++ atom) v (head chi) Nothing Nothing,
            con ("proof " ++ atom) [cl + 1, cl + 2] Nothing Nothing
          ]
    lchi = length chi
    chi = idxs ++ [i + cl | i <- [1 .. (length ands)]]
    (idxs, ands) = partitionEithers $ map (andify lnn) rhs
    rhs = fromMaybe [] $ Map.lookup atom assumptions
    cl = Seq.length lnn - 1



complete _ ns [] = ns
complete m ns (a : as) = complete m ns' as
  where
    ns' = ns Seq.>< Seq.fromList (justification m ns a)

lnnFromCnf cnf = lnn'''
    where
      xs = tail . lines $ cnf
      mapping = Map.fromList . map (\ys -> (words ys !! 1,last . words $ ys)) . filter ((==) 'c' . head) $ xs
      clauses = filter (\c -> length c > 1) . map (map (read :: String -> Int) . init . words) . filter ((/=) 'c' . head) $ xs
      --(vars,lits) = extractVarsLits clauses
      --atoms = map (unwrapAtom mapping . show) vars
      vars = map show $ extractVars [] clauses
      lnn = Seq.fromList $ map (\x -> var (unwrapAtom mapping x) Nothing Nothing) vars
      negs = toList $ Set.fromList $ extractNots mapping [] clauses vars
      lits = map (unwrapAtom mapping) vars ++ map _s negs
      lnn' = lnn Seq.>< Seq.fromList negs
      lnn'' = Seq.fromList $ extractClauses mapping (toList lnn') clauses lits
      lnn''' = lnn'' Seq.|> con "root" [length lits .. Seq.length lnn''-1] Nothing Nothing
     


extractVarsLits cnf = List.partition (> 0) . map (read :: String -> Int) . toList . Set.fromList . concat $ cnf

unwrapAtom m i = 
  case Map.lookup i m of
    Just a -> a
    _      -> "aux_" ++ i

extractVars lnn []  = lnn
extractVars lnn [clause]  = lnn ++ (filter (\l -> not $ l `elem` lnn) $ map abs clause)
extractVars lnn (clause:clauses)  = extractVars lnn' clauses
  where
    lnn' = extractVars lnn [clause]
    
extractNots _ lnn [] _  = lnn
extractNots m lnn [clause] xs  = lnn ++ (map (\l -> neg ("-" ++ (unwrapAtom m $ tail . show $ l)) (fromMaybe 0 $ List.elemIndex (tail . show $ l) xs) Nothing Nothing) $ filter (< 0) clause)
extractNots m lnn (clause:clauses) xs  = extractNots m lnn' clauses xs
  where
    lnn' = extractNots m lnn [clause] xs

extractClauses _ lnn [] _  = lnn
extractClauses m lnn [clause] xs  = lnn ++ [dis "clause" (indexifyClause m xs clause) Nothing Nothing]
extractClauses m lnn (clause:clauses) xs  = extractClauses m lnn' clauses xs
  where
    lnn' = extractClauses m lnn [clause] xs

indexifyClause m xs clause = map f clause
  where 
    f l = if l < 0 then fromMaybe 0 $ List.elemIndex ("-" ++ (unwrapAtom m $ tail . show $ l)) xs else fromMaybe 0 $ List.elemIndex (unwrapAtom m $ show l) xs
