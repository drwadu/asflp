{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fuzzy
  ( Value,
    Logic (..),
    Eval (..),
    negation',
    tCoNorm',
    tNorm',
  )
where

import Data.Data

newtype Value = Value Double deriving (Ord, Num, Eq, Fractional, Show, Read)

data Logic = Lukasiewicz | Goedel | Zadeh
  deriving (Ord, Eq, Show, Data, Typeable)

class Eval a where
  negation :: Logic -> a -> a
  tCoNorm :: Foldable t => Logic -> t a -> a
  tNorm :: Logic -> [a] -> a
  residuum :: Logic -> a -> a -> a
  bot :: a
  top :: a

-- instance Eval Value where
instance Eval Double where
  bot = 0.0
  top = 1.0
  negation l x = case l of
    Goedel -> if x == bot then top else bot
    _ -> top - x
  tCoNorm l xs = case l of
    Lukasiewicz -> min (sum xs) top
    _ -> maximum xs
  tNorm l xs = case l of
    Lukasiewicz -> max (negation l . sum . map (negation l) $ xs) bot
    _ -> minimum xs
  residuum l x y = case l of
    Lukasiewicz -> min (tCoNorm l [negation l x, y]) top
    Goedel -> if x <= y then top else y
    Zadeh -> max (negation l x) y

negation' :: (Eval a, Eval b) => Logic -> (a, b) -> (a, b)
negation' l (x, y) = (negation l x, negation l y)

tCoNorm' l (x, y) (x', y') = (tCoNorm l [x, x'], tCoNorm l [y, y'])

tNorm' l (x, y) (x', y') = (tNorm l [x, x'], tNorm l [y, y'])
