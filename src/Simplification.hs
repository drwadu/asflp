module Simplification
  ( LogicalNeuron (..),
    LnnOperator (..),
    proof,
  )
where

import Control.Monad (foldM)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import Fuzzy
  ( Eval (..),
    Logic (..),
    bot,
    negation',
    tCoNorm',
    tNorm',
    top,
  )
import Utils (remove)

type Bounds = (Double, Double)

type Interpretation = Map.Map String Bounds

data LogicalNeuron
  = Variable String
  | Negation LogicalNeuron
  | Conjunction [LogicalNeuron]
  | Disjunction [LogicalNeuron]
  | Implication LogicalNeuron LogicalNeuron
  deriving (Ord, Eq, Show)

proof a b = Conjunction [Implication a b, Implication b a]

logic = Lukasiewicz

aggregate (l, u) (l', u') = (max l l', min u u')

aggregateM (Just (l, u)) (Just (l', u')) = (max l l', min u u')
aggregateM _ _ = undefined

class LnnOperator input where
  _infer :: Interpretation -> input -> Maybe Bounds
  tighten :: Interpretation -> input -> [Bounds]

-- infer :: input -> Interpretation -> input -> Maybe Bounds

-- infer :: input -> Interpretation -> input -> Maybe Bounds

-- aggregate :: Interpretation -> Interpretation -> input -> Maybe Bounds
-- evaluate :: Interpretation -> input -> Maybe Bouns

instance LnnOperator LogicalNeuron where
  -- aggregate i j x =
  --  case evaluate i x of
  --    Just (l, u) ->
  --      case evaluate j x of
  --        Just (l', u') -> Just (max l l', min u u')
  --        _ -> Nothing
  --    _ -> Nothing

  --
  _infer i (Variable s) =
    case Map.lookup s i of
      Nothing -> error ("no bounds found for variable " ++ s)
      b -> b
  _infer i (Negation x) = negationM <$> _infer i x
    where
      negationM (l', u') = (negation logic u', negation logic l')
  _infer i (Conjunction xs) = Just $ tNormM (map (_infer i) xs)
    where
      tNormM xs' = foldr (tNorm' logic) (top, top) $ catMaybes xs'
  _infer i (Disjunction xs) = Just $ tCoNormM (map (_infer i) xs)
    where
      tCoNormM xs' = foldr (tCoNorm' logic) (bot, bot) $ catMaybes xs'
  _infer i (Implication x y') =
    case _infer i x of
      Just (l, u) -> case _infer i y' of
        Just (l', u') -> Just $ tCoNorm' logic (negation logic u, negation logic l) (l', u')
        _ -> Nothing
      _ -> Nothing

  --
  tighten i (Conjunction xs) = zipWith aggregate (map (\j -> (tl j, tu j)) [0 .. length xs]) inner
    where
      tl j =
        if l > bot
          then residuum logic (tNorm logic . map snd . remove j $ inner) l
          else bot
      tu j =
        if u < top
          then residuum logic (tNorm logic . map fst . remove j $ inner) u
          else top
      (l, u) = foldr (tNorm' logic) (top, top) inner
      inner = mapMaybe (_infer i) xs
  tighten i (Disjunction xs) = zipWith aggregate (map (\j -> (tl j, tu j)) [0 .. length xs]) inner
    where
      tl j =
        if l > bot
          then tNorm logic [tNorm logic . map (negation logic . snd) . remove j $ inner, l]
          else bot
      tu j =
        if u < top
          then tNorm logic [tNorm logic . map (negation logic . fst) . remove j $ inner, u]
          else top
      (l, u) = foldr (tCoNorm' logic) (bot, bot) inner
      inner = mapMaybe (_infer i) xs

-- tighten i (Implication x y) = zipWith aggregate (map (\j -> (tl j, tu j)) [0 .. length xs]) inner
--  where
--    tl j =
--      if l > bot
--        then tNorm logic [tNorm logic . map (negation logic . snd) . remove j $ inner, l]
--        else bot
--    tu j =
--      if u < top
--        then tNorm logic [tNorm logic . map (negation logic . fst) . remove j $ inner, u]
--        else top
--    (l, u) = foldr (tCoNorm' logic) (bot, bot) inner
--    inner = mapMaybe (infer i) xs

-- tighten p i (Variable s) = aggregateM (infer i s) (tighten (infer))

--
-- down i (Negation x) = negationM <$> evaluate i (Negation x)
--  where
--    v = case evaluate i x of
--      Just b -> b
--      _ -> error ("upward pass for " ++ show x ++ " failed")
--    negationM (l, u) = (negation logic u, negation logic l)
---- aggregateM (Just (l, u)) (l', u') = aggregate (l,u)
-- down i (Conjunction xs) = undefined
--  where
--    bs = zip [0 ..] $ map (evaluate i) xs
-- down i (Variable s) = Map.lookup s i
-- down i _ = undefined

-- evaluate i _ (Variable s) = Map.lookevaluate s i
-- evaluate i (Just b) (Negation x) = aggregate b . negationM <$> evaluate i (Just unc) x
--  where
--    negationM (l', u') = (negation logic u', negation logic l')
-- evaluate i (Just b) (Conjunction xs) = Just . aggregate b $ tNormM (map (evaluate i (Just unc)) xs)
--  where
--    tNormM xs = foldr (tNorm' logic) (top, top) $ catMaybes xs
-- evaluate i (Just b) (Disjunction xs) = Just . aggregate b $ tCoNormM (map (evaluate i (Just unc)) xs)
--  where
--    tCoNormM xs = foldr (tCoNorm' logic) (bot, bot) $ catMaybes xs
-- evaluate i (Just b) (Implication x x') =
--  case evaluate i (Just unc) x of
--    Just (l, u) -> case evaluate i (Just unc) x' of
--      Just (l', u') -> Just . aggregate b $ tCoNorm' logic (negation logic u, negation logic l) (l', u')
--      _ -> Nothing
--    _ -> Nothing
---- evaluate i (Just unc) n =
-- evaluate i Nothing n = undefined
