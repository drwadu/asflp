module Ico () where


data Expression 
  = Var Int
  | Neg Expression
  | And Expression Expression
  deriving (Show)

bot = 0.0
top = 1.0

neg a = top - a
tconorm a b = min (a + b) top
tnorm a b = max (neg (tconorm (neg a) (neg b))) bot

evaluate i (Var a) = i !! a

evaluate i (Neg e) = (1.0 - u, 1.0 - l)
  where
    (l, u) = evaluate i e

evaluate i (And e e') = (tnorm l1 l2, tnorm u1 u2)
  where
    (l1, u1) = evaluate i e
    (l2, u2) = evaluate i e'

run i p a = 
  if 
    null bs 
  then 
    fromMaybe (error "") $ Map.lookup a i 
  else 
    (maximum ls, maximum us)
  where
    us = fmap snd bs
    ls = fmap fst bs
    bs = map evaluate' $ fromMaybe (error "") $ p !! a
    evaluate' [x] = evaluate i x
    evaluate' [x, y] = evaluate i $ And x y

