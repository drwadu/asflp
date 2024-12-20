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
    infer,
    display,
    lnnCmp,
    inferDebug,
    bounds,
    solveH1,
    update
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

solveWithAssumptions i lnn assumptions = do
  mapM_ (putStr . display) ns
  putStrLn ""
  x <- inferDebug i ns'
  mapM_ (putStr . display) x
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
            ++ [ dis ("[" ++ atom ++ "] OR " ++ unwords (map (_s . findNeuronByIndex lnn) idxs ++ map _s ands)) chi Nothing Nothing,
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
