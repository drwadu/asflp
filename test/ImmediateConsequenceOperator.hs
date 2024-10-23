module ImmediateConsequenceOperator ((>*), (>*<), eval, eval') where

data Expression 
  = Lit Int
  | And Expression Expression
  deriving (Show)

bot = 0.0
top = 1.0

neg a = top - a
tconorm a b = min (a + b) top
tnorm a b = max (neg (tconorm (neg a) (neg b))) bot

i >* lit = 
  if lit > 0 
  then i !! (lit - 1)
  else 
    (neg u, neg l)
      where
        (l, u) = i >* negate lit


i >*< [lit] = i >* lit
i >*< [lit,lit'] = (tnorm l l', tnorm u u')
  where
    (l, u) = i >* lit
    (l', u') = i >* lit' 
_ >*< _ = undefined

eval i p a = 
  if null bs -- atom occurs in no rule head
  then i !! (a-1)
  else (maximum ls, maximum us)
    where
      us = fmap snd bs
      ls = fmap fst bs
      bs = map evaluate $ p !! (a-1)
      evaluate [x] =  i >* x
      evaluate xs  = i >*< xs

eval' i p =  map (eval i p) [1..length p]
