{-# LANGUAGE DeriveDataTypeable #-}

module Neuron
  ( Neuron (A, V, N, O, I, _s, _xs, _l, _u, _x, _y),
    var,
    neg,
    con,
    dis,
    imp,
    update,
    bounds
  )
where

import Data.Data
import Fuzzy (bot, top)

data Neuron
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
        _u :: Double
      }
  | O
      { _s :: String,
        _xs :: [Int],
        _l :: Double,
        _u :: Double
      }
  | I
      { _s :: String,
        _x :: Int,
        _y :: Int,
        _l :: Double,
        _u :: Double
      }
  deriving
    ( Ord,
      Eq,
      Show,
      Data,
      Typeable
    )

update (V s  _ _) l u    = V s l u
update (N s x _ _) l u   = N s x l u
update (A s xs _ _) l u  = A s xs l u
update (O s xs _ _) l u  = O s xs l u
update (I s x y _ _) l u = I s x y l u

var s (Just l) (Just u) = V s l u
var s _ _ = V s bot top

neg s x (Just l) (Just u) = N {_s = s, _x = x, _l = l, _u = u}
neg s x _ _ = N {_s = s, _x = x, _l = bot, _u = top}

con s xs (Just l) (Just u) = A {_s = s, _xs = xs, _l = l, _u = u}
con s xs _ _ = A {_s = s, _xs = xs, _l = bot, _u = top}

dis s xs (Just l) (Just u) = O {_s = s, _xs = xs, _l = l, _u = u}
dis s xs _ _ = O {_s = s, _xs = xs, _l = bot, _u = top}

imp s x y (Just l) (Just u) = I {_s = s, _x = x, _y = y, _l = l, _u = u}
imp s x y _ _ = I {_s = s, _x = x, _y = y, _l = bot, _u = top}

bounds n = (_l n,_u n)
