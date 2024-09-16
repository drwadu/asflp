{-# LANGUAGE DeriveDataTypeable #-}

module Neuron
  ( Neuron (A, V, N, O, I, _s, _xs, _l, _u, _x, _y),
    var,
    neg,
    con,
    dis,
    imp,
    update,
  )
where

import Data.Data
import Fuzzy (bot, top)

-- data Neuron a
--  = V
--      { _s :: String,
--        _l :: a,
--        _u :: a
--      }
--  | N
--      { _s :: String,
--        _x :: Int,
--        _l :: a,
--        _u :: a
--      }
--  | A
--      { _s :: String,
--        _xs :: [Int],
--        _l :: a,
--        _u :: a
--      }
--  | O
--      { _s :: String,
--        _xs :: [Int],
--        _l :: a,
--        _u :: a
--      }
--  | I
--      { _s :: String,
--        _x :: Int,
--        _y :: Int,
--        _l :: a,
--        _u :: a
--      }
--  deriving
--    ( Ord,
--      Eq,
--      Show,
--      Data,
--      Typeable
--    )

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

update (V {_s = s, _l = _, _u = _}) l u = (V {_s = s, _l = l, _u = u})
update (N {_s = s, _x = x, _l = _, _u = _}) l u = (N {_s = s, _x = x, _l = l, _u = u})
update (A {_s = s, _xs = xs, _l = _, _u = _}) l u = (A {_s = s, _xs = xs, _l = l, _u = u})
update (O {_s = s, _xs = xs, _l = _, _u = _}) l u = (O {_s = s, _xs = xs, _l = l, _u = u})
update (I {_s = s, _x = x, _y = y, _l = _, _u = _}) l u = (I {_s = s, _x = x, _y = y, _l = l, _u = u})

var s (Just l) (Just u) = V {_s = s, _l = l, _u = u}
var s _ _ = V {_s = s, _l = bot, _u = top}

neg s x (Just l) (Just u) = N {_s = s, _x = x, _l = l, _u = u}
neg s x _ _ = N {_s = s, _x = x, _l = bot, _u = top}

con s xs (Just l) (Just u) = A {_s = s, _xs = xs, _l = l, _u = u}
con s xs _ _ = A {_s = s, _xs = xs, _l = bot, _u = top}

dis s xs (Just l) (Just u) = O {_s = s, _xs = xs, _l = l, _u = u}
dis s xs _ _ = O {_s = s, _xs = xs, _l = bot, _u = top}

imp s x y (Just l) (Just u) = I {_s = s, _x = x, _y = y, _l = l, _u = u}
imp s x y _ _ = I {_s = s, _x = x, _y = y, _l = bot, _u = top}
