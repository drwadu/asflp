{-# LANGUAGE FlexibleInstances #-}

import H1 ()
--import Sampler (Expression (Var, Neg, And))
import Sampler (Literal, Interpretation3, Program3, isUnique, atoms, Body,randomBounds3)

import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad
import Lib (var, neg, con, dis)

import qualified Data.Map as Map

import qualified Ico as Tp

import qualified Data.Sequence as Seq




-- a b c d e
iT = [(0.4,0.4),(0.6,0.6),(0.0,1.0),(0.0,1.0),(0.0,1.0)]

-- a :- not b
-- b :- not a
-- c :- not d, b
-- d :- not c, b
--pT :: [[[Literal]]]
pT :: [[[Int]]]
pT = [
  [[-2]],
  [[-1]],
  [[-4,2]],
  [[-3,2]],
  []]

ruleBodiesLnn i p = Seq.fromList vars
  where
    n = length p 
    vars = map (\j -> uncurry (var (show j) . Just)  $ i !! j) [1..n]
    --vars = map (var . show) [1..n]

--prop_h1 i p = length p >= 3 ==> length p === 3
--prop_h1 i p = map abs $ concat $ concat p == [1..length i - 1] ==> length p === 3
--prop_h1 p = p === pT_
--  where 
--    types = p :: Program3 --[[[Literal]]]

--prop_h1 p = length p == 5 && all (not . null) p ==> Tp.eval iT p 1 === (0.6,0.6)
  --where 
  --  types = p :: [[[Int]]]

--prop_h1 p = length p == length iT ==> Tp.eval iT p 1 === (0.6,0.6)
prop_h1 p = length p == length iT ==> Tp.eval' iT p === map (Tp.eval iT p) [1..length p - 1]
