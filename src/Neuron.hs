{-# LANGUAGE DeriveDataTypeable #-}

module Neuron
  ( Neuron_ (A, V, N, O, I, _s, _xs, _l, _u, _x, _y, _ws),
    var,
    neg,
    con,
    dis,
    imp,
    update_,
    bounds
  )
where

import Data.Data
import Fuzzy (bot, top)

{- | Logical Neuron
 
 V .. variable

 N .. negation

 A .. conjunction

 O .. disjunction

 I .. implication

-}
data Neuron_
  = V
      { _s :: String,
        _l :: Double,
        _u :: Double
      }
  | N
      { _s :: String,
        _x :: Int,
        _l :: Double,
        _u :: Double
      }
  | A
      { _s :: String,
        _xs :: [Int],
        _l :: Double,
        _u :: Double,
        _ws :: [Double]
      }
  | O
      { _s :: String,
        _xs :: [Int],
        _l :: Double,
        _u :: Double,
        _ws :: [Double]
      }
  | I
      { _s :: String,
        _x :: Int,
        _y :: Int,
        _l :: Double,
        _u :: Double,
        _ws :: [Double]
      }
  deriving
    ( Ord,
      Eq,
      Show,
      Data,
      Typeable
    )

update_ :: Neuron_ -> Double -> Double -> Neuron_
update_ (V s  _ _) l u       = V s l u
update_ (N s x _ _) l u      = N s x l u
update_ (A s xs _ _ ws) l u  = A s xs l u ws
update_ (O s xs _ _ ws) l u  = O s xs l u ws
update_ (I s x y _ _ ws) l u = I s x y l u ws

var :: String -> Maybe Double -> Maybe Double -> Neuron_
var s (Just l) (Just u) = V s l u
var s _ _               = V s bot top

neg s x (Just l) (Just u) = N s x l u
neg s x _ _               = N s x bot top

con s xs (Just l) (Just u) = A s xs l u     [1.0 | _ <- xs] 
con s xs _ _               = A s xs bot top [1.0 | _ <- xs]

dis s xs (Just l) (Just u) = O s xs l u     [1.0 | _ <- xs] 
dis s xs _ _               = O s xs bot top [1.0 | _ <- xs]

imp s x y (Just l) (Just u) = I s x y l u     [1.0, 1.0] 
imp s x y _ _               = I s x y bot top [1.0, 1.0]

bounds n = (_l n,_u n)
