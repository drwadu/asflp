module WellFounded (Expression (..), evaluate, tp, fpi, tp', awp, fpFpi, fpAwp, fpTp') where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data Expression
  = Var String Double Double
  | Const Double Double
  | Neg Expression
  | And Expression Expression
  deriving (Show)

evaluate _ (Const l u) = (l, u)
evaluate i (Var s _ _) =
  case Map.lookup s i of
    Just (l, u) -> (l, u)
    _ -> undefined
evaluate i (Neg (Var s _ _)) = (1.0 - u, 1.0 - l)
  where
    (l, u) = case Map.lookup s i of
      Just (l', u') -> (l', u')
      _ -> undefined
-- evaluate i (And (Var s1 _ _) (Var s2 _ _)) = (tnorm l1 l2, tnorm u1 u2)
--  where
--    tnorm a b = max (1.0 - ((1.0 - a) + (1.0 - b))) 0.0
--    (l1, u1) = case Map.lookup s1 i of
--      Just (l', u') -> (l', u')
--      _ -> undefined
--    (l2, u2) = case Map.lookup s2 i of
--      Just (l', u') -> (l', u')
--      _ -> undefined

evaluate i (And e e') = (tnorm l1 l2, tnorm u1 u2)
  where
    tnorm a b = max (1.0 - ((1.0 - a) + (1.0 - b))) 0.0
    -- tnorm a b = min a b
    (l1, u1) = evaluate i e
    (l2, u2) = evaluate i e'

-- tp i flp a = if null bs then fromMaybe (error "") $ Map.lookup a i else (maximum ls, maximum us)
tp i flp a = if null bs then fromMaybe (error "") $ Map.lookup a i else (maximum ls, maximum us)
  where
    us = fmap snd bs
    ls = fmap fst bs
    bs = map evaluate' $ fromMaybe (error "") $ Map.lookup a flp
    evaluate' [x] = evaluate i x
    evaluate' [x, y] = evaluate i $ And x y

tp' i flp = zip atoms (map (tp i flp) atoms)
  where
    atoms = Map.keys i

meet i' j' = Map.unionWith (\(lj, uj) (lj', uj') -> (min lj lj', max uj uj')) i' j'

join i' j' = Map.unionWith (\(lj, uj) (lj', uj') -> (max lj lj', min uj uj')) i' j'

fpi i j flp = meet iF $ Map.fromList $ tp' iT flp
  where
    iT = join i j
    iF = Map.map (const (0.0, 0.0)) i

iCmp a b = sum $ zipWith (curry f) (g a) (g b)
  where
    g = Map.elems
    f ((l, u), (l', u')) = abs (l - l') + abs (u - u')

fpFpi i j flp = if iCmp j j' <= epsilon then j' else fpFpi i j' flp
  where
    j' = fpi i j flp
    epsilon = 0.0001

awp i flp = join a b
  where
    a = Map.fromList $ tp' i flp
    iF = Map.map (const (0.0, 0.0)) i
    b = fpFpi i iF flp

fpAwp i flp = if iCmp i j' <= epsilon then j' else fpAwp j' flp
  where
    j' = awp i flp
    epsilon = 0.0001

fpTp' i flp = do
  print j'
  if iCmp i j' <= epsilon
    then do print "done"; return j'
    else fpTp' j' flp
  where
    j' = Map.fromList $ tp' i flp
    epsilon = 0.0001

-- fpTp' i flp = if iCmp i j' <= epsilon then j' else fpTp' j' flp
--  where
--    j' = Map.fromList $ tp' i flp
--    epsilon = 0.0001
