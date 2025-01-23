{-# LANGUAGE DeriveDataTypeable #-}

module Lnn.Logic
  ( Flp
  , Bounds
  , Interpretation
  , Symbol
  , Logic (..)
  , Eval (..)
  , negation'
  , tCoNorm'
  , tNorm'
  , residuum'
  ) where

import qualified Data.Map as Map

import Data.Data (Data, Typeable) 


type Bounds = (Double, Double)
type Symbol = String
type Flp = String
type Interpretation = Map.Map String Bounds


data Logic =  Goedel | Lukasiewicz | Zadeh
  deriving (Ord, Eq, Show, Data, Typeable)


class Eval a where
  negation :: Logic -> a -> a
  tCoNorm :: (Foldable t) => Logic -> t a -> a
  tNorm :: Logic -> [a] -> a
  residuum :: Logic -> a -> a -> a
  bot :: a
  top :: a


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


tCoNorm' :: (Eval a, Eval b) => Logic -> (a, b) -> (a, b) -> (a,b)
tCoNorm' l (x, y) (x', y') = (tCoNorm l [x, x'], tCoNorm l [y, y'])


tNorm' :: (Eval a, Eval b) => Logic -> (a, b) -> (a, b) -> (a,b)
tNorm' l (x, y) (x', y') = (tNorm l [x, x'], tNorm l [y, y'])


residuum' :: (Eval a, Eval b) => Logic -> (a, b) -> (a, b) -> (a,b)
residuum' l (x, y) (x', y') = (residuum l x x', residuum l y y')
