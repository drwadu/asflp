{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Solver
  (
  )
where

import Data.Data (Data, Typeable)
import Data.List (intercalate, sort)
--import Utils (remove)
--import qualified Data.Map as Map
import Fuzzy (Logic (Lukasiewicz), bot, negation, negation', tCoNorm', top, tNorm, residuum)


type Bounds = (Double, Double)
type Info = (Bounds, Double, Double)
type Interpretation = [(Int, Info)]

-- | neurons corresponding to propositional logical formulas
data Neuron
  = Variable {_id :: String, _l :: Double, _u :: Double, _d :: Double}
  | Negation {_x :: Neuron, _l :: Double, _u :: Double, _d :: Double}
  | Conjunction {_xs :: [Neuron], _l :: Double, _u :: Double, _d :: Double}
  | Disjunction {_xs :: [Neuron], _l :: Double, _u :: Double, _d :: Double}
  | Implication {_lhs :: Neuron, _rhs :: Neuron, _l :: Double, _u :: Double, _d :: Double}
  deriving
    ( Ord
    , Eq
    , Data
    , Typeable
    )

--type Interpretation = 


instance Show Neuron where
  show (Variable s _ _ _) = s
  show (Negation x _ _ _) = "!(" ++ show x ++ ")"
  show (Conjunction xs _ _ _) = "(" ++ intercalate " & " (map show xs) ++ ")"
  show (Disjunction xs _ _ _) = "(" ++ intercalate " | " (map show xs) ++ ")"
  show (Implication lhs rhs _ _ _) = "(" ++ show lhs ++ " => " ++ show rhs ++ ")"


showBounds l u = show l ++ " " ++ show u

bounds n = (_l n, _u n)
successors (Negation x _ _ _) = [x] ++ successors x
successors (Conjunction xs _ _ _) = xs ++ concatMap successors xs
successors (Disjunction xs _ _ _) = xs ++ concatMap successors xs
successors (Implication lhs rhs _ _ _) = [lhs,rhs] ++ successors lhs ++ successors rhs
successors _ = []


type Bounds = (Double, Double)
--type Interpretation = Map.Map Neuron Bounds
type Interpretation =  [(Neuron,Bounds)]


aggregate :: (Ord a, Ord b) => (a, b) -> (a, b) -> (a, b)
aggregate (l, u) (l', u') = (max l l', min u u')


diff (l, u) (l', u') = abs (l - l') + abs (u - u')

upward (Negation x l u _) = Negation x' l' u' (diff (l,u) (l',u'))
 where
  (l',u') = aggregate (l, u) (negation Lukasiewicz xu, negation Lukasiewicz xl)
  (xl,xu) = bounds x'
  x' = upward x
upward (Conjunction xs l u _) = Conjunction xs' l' u' (diff (l,u) (l',u'))
 where
  (l',u') = aggregate (l, u) (xl, xu)
  (xl, xu) = negation' Lukasiewicz $ foldr ((tCoNorm' Lukasiewicz . negation' Lukasiewicz) . bounds) (bot, bot) xs'
  xs' = map upward xs
upward (Disjunction xs l u _) = Disjunction xs' l' u' (diff (l,u) (l',u'))
 where
  (l',u') = aggregate (l, u) (xl, xu)
  (xl, xu) = foldr (tCoNorm' Lukasiewicz . bounds) (bot, bot) xs'
  xs' = map upward xs
upward (Implication lhs rhs l u _) = Implication lhs' rhs' l' u' (diff (l,u) (l',u'))
 where
  (l',u') = aggregate (l, u) (xl, xu)
  (xl, xu) = tCoNorm' Lukasiewicz (uLhs, lLhs) (lRhs, uRhs)
  ((lLhs, uLhs), (lRhs, uRhs)) = (negation' Lukasiewicz . bounds $ lhs', bounds rhs')
  (lhs', rhs') = (upward lhs, upward rhs)
upward v = v

updateBounds (Variable s l u _) l' u' =  Variable s l' u' (diff (l,u) (l',u'))
updateBounds (Negation x l u _) l' u' =  Negation x l' u' (diff (l,u) (l',u'))
updateBounds (Conjunction xs l u _) l' u' =  Conjunction xs l' u' (diff (l,u) (l',u'))
updateBounds (Disjunction xs l u _) l' u' =  Disjunction xs l' u' (diff (l,u) (l',u'))
updateBounds (Implication lhs rhs l u _) l' u' =  Implication lhs rhs l' u' (diff (l,u) (l',u'))

remove :: Int -> [a] -> [a]
remove _ [] = []
remove 0 (_ : xs) = xs
remove n (x : xs) = x : remove (n - 1) xs

downward (Negation x l u d) = Negation (downward (updateBounds x l' u')) l u d
 where
  (l',u') = aggregate (xl, xu) (negation Lukasiewicz u, negation Lukasiewicz l)
  (xl,xu) = bounds x
downward (Conjunction xs l u d) = Conjunction (map downward xs') l u d
 where
  xs' = zipWith (curry (\(i, x) -> uncurry (updateBounds x) (aggregate (bounds x) (tl i, tu i)))) [0 ..] xs
  tl j =
    if l > bot
      then residuum Lukasiewicz (tNorm Lukasiewicz . map _u . remove j $ xs) l
      else bot
  tu j =
    if u < top
      then residuum Lukasiewicz (tNorm Lukasiewicz . map _l . remove j $ xs) u
      else top
downward (Disjunction xs l u d) = Disjunction (map downward xs') l u d
 where
  xs' = zipWith (curry (\(i, x) -> uncurry (updateBounds x) (aggregate (bounds x) (tl i, tu i)))) [0 ..] xs
  tl j =
    if l > bot
      then tNorm Lukasiewicz [tNorm Lukasiewicz . map (negation Lukasiewicz . _u) . remove j $ xs, l]
      else bot
  tu j =
    if u < top
      then tNorm Lukasiewicz [tNorm Lukasiewicz . map (negation Lukasiewicz . _l) . remove j $ xs, u]
      else top
downward (Implication lhs rhs l u d) = Implication (downward lhs') (downward rhs') l u d
 where
  lhs' = uncurry (updateBounds lhs) (aggregate (bounds lhs) (lLhs',uLhs'))
  rhs' = uncurry (updateBounds rhs) (aggregate (bounds rhs) (lRhs',uRhs'))
  lLhs' = if u < top then residuum Lukasiewicz u (_l rhs) else bot
  uLhs' = if l > bot then residuum Lukasiewicz l (_u rhs) else bot
  lRhs' = if l > bot then tNorm Lukasiewicz [_l lhs, l] else bot
  uRhs' = if u < top then tNorm Lukasiewicz [_u lhs, u] else top
downward n = n

tightenMultiNeuron src n = sort $ successors n

approximate n = downward n'
  where 
    n' = upward n

epsilon = 0.0001


inferStandard n = if _d n + (sum . map _d $ _xs n) <= epsilon then n else inferStandard n'
  where
    n' = approximate n

infer n = if _d n + (sum . map _d $ _xs n) <= epsilon then n else inferStandard n'
  where
    n' = downward $ updateBounds (upward n) top top
    

expose (_,xs) = mapM_ print xs

dbg (Variable s l u d) = show l ++ " " ++ show u ++ " " ++ show d ++ " :: " ++ s 
dbg n@(Negation x l u d) = show l ++ " " ++ show u ++ " " ++ show d ++ " :: " ++ show n ++ "\n" ++ dbg x 
dbg n@(Implication lhs rhs l u d) = show l ++ " " ++ show u ++ " " ++ show d ++ " :: " ++ show n ++ "\n" ++ intercalate "\n" (map dbg [lhs,rhs])
dbg n = show (_l n) ++ " " ++ show (_u n) ++ " " ++ show (_d n) ++ " :: " ++ show n  ++ "\n" ++ intercalate "\n" (map dbg (_xs n)) 


-- 
a = Variable "a" 0.4 0.4 0.0
b = Variable "b" 0.6 0.6 0.0
c = Variable "c" bot top 0.0
d' = Variable "d" bot top 0.0
nc = Disjunction [Negation (Conjunction [a,b] bot top 0.0) bot top 0.0, Conjunction [b, Negation c bot top 0.0] bot top 0.0] bot top 1.0
foo = Conjunction [Negation a 0.0 1.0 0.0, Implication b (Negation c 0.0 1.0 0.0) 0.0 1.0 0.0, Negation b 0.0 1.0 0.0] 0.0 1.0 1.0
bla = Conjunction [Disjunction [a,b] bot top 0.0, Disjunction [b,c] bot top 0.0, Disjunction [Conjunction [Negation a bot top 0.0, b] bot top 0.0,c] bot top 0.0] 0.0 1.0 1.0
tiny = Conjunction [
  Conjunction [
      Implication (Negation b bot top 0.0) a bot top 0.0,
      Implication a (Negation b bot top 0.0) bot top 0.0
    ] bot top 0.0,
  Conjunction [
      Implication (Negation a bot top 0.0) b bot top 0.0,
      Implication b (Negation a bot top 0.0) bot top 0.0
    ] bot top 0.0,
  Conjunction [
      Implication (Conjunction [b, Negation d' bot top 0.0] bot top 0.0) c bot top 0.0,
      Implication c (Conjunction [b, Negation d' bot top 0.0] bot top 0.0) bot top 0.0
    ] bot top 0.0,
  Conjunction [
      Implication (Conjunction [b, Negation c bot top 0.0] bot top 0.0) d' bot top 0.0,
      Implication d' (Conjunction [b, Negation c bot top 0.0] bot top 0.0) bot top 0.0
    ] bot top 0.0
  ] 0.0 1.0 1.0
