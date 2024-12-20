module H2 where

import ImmediateConsequenceOperator 
  ((>*), 
  (>*<), 
  eval, 
  eval'
  )
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Either ( partitionEithers ) 
import Data.Bifunctor (bimap)
import qualified Data.List as List
import Data.Foldable (toList)

import Lib
  ( con,
    dis,
    downwardPass,
    neg, upwardPass,
    var,
    imp,
    Neuron (..),
    bounds,
    update
  )


lhs = eval'

rhs i flp = (res,lnn)
  where
    res = map bounds . take k . toList $ lnn''
    lnn = downwardPass (j-1) lnn'' 
    k = length i
    lnn'' = Seq.take j lnn' Seq.|> root
    root = update (fromMaybe (error "") $ Seq.lookup (j-1) lnn') (1.0 :: Double) (1.0 :: Double)
    j = Seq.length lnn'
    lnn' = upwardPass $ constructLnn i flp
    cmp n n'
     | (read (head $ words $ _s n) :: Int) < (read (head $ words $ _s n') :: Int)  = LT  
     | (read (head $ words $ _s n) :: Int) == (read (head $ words $ _s n') :: Int) = EQ
     | otherwise = GT

constructLnn i flp = lnn
  where
    lnn = lnn' ++ [con "root" (map (\(x,_) -> x) $ filter (\(_,y) -> "RULE" == _s y) $ zip [0..] lnn') Nothing Nothing]
    lnn' = toList $ complete hbs lnnLits atoms
    lnnLits = lnnVars Seq.>< (Seq.fromList . map (\x -> neg x (find' lnnVars (tail x)) Nothing Nothing) . Set.toList . Set.fromList . map show . concatMap (filter (< 0)) . concat $ flp)
    lnnVars = Seq.fromList $ zipWith (uncurry . var) (map (\x -> if Just [] == Map.lookup x hbs then x ++ " PROOF" else x) atoms) (fmap (bimap Just Just) i)
    hbs = Map.fromList $ zip atoms [map (\x' -> [show y | y <- x']) x | x <- flp]
    atoms = map show [1..length i]

complete _ ns [] = ns
complete m ns (a : as) = complete m ns' as
  where
    ns' = ns Seq.>< (Seq.fromList $ justification m ns a)

justification assumptions lnn atom = res 
  where
    res =
      if lchi > 1 then ands ++ [imp "RULE" c ((read atom) -1)  Nothing Nothing | c <- chi]
        else 
            if lchi < 1 then [] 
          else 
            ands ++ [imp "RULE" (head chi) ((read atom) - 1) Nothing Nothing]
    lchi = length chi
    chi = idxs ++ [i + cl | i <- [1 .. (length ands)]]
    (idxs, ands) = partitionEithers $ map (andify lnn) rhs
    rhs = fromMaybe [] $ Map.lookup atom assumptions
    cl = Seq.length lnn - 1

find' ns x = maybe (-1) id $ Seq.findIndexL (\n -> (head $ words $ _s n) == x) ns

findNeuron ns x = fmap (Seq.index ns) $ Seq.findIndexL (\n -> (_s n) == x) ns

andify ns xs =
  if length xs > 1
    then case findNeuron ns s of
      Just n -> Left . fromMaybe (-1) . Seq.findIndexL (\n' -> (head $ words $ _s n') == _s n) $ ns
      _ -> Right (con s (map (find' ns) xs') Nothing Nothing)
    else Left . find' ns . head $ xs
  where
    s = "(AND " ++ unwords xs' ++ ")"
    xs' = List.sort xs



-- sanity check
-- a :- -b
-- b :- -a
-- c :- -d, b
-- c :- e
-- d :- -c, b
scI = [(0.4,0.4),(0.6,0.6),(0.0,1.0),(0.0,1.0),(0.0,1.0)]
scFlp = [
  [[-2 :: Int]],
  [[-1]],
  [[-4,2],[5]],
  [[-3,2]],
  []
  ]

