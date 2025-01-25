{-# LANGUAGE DeriveDataTypeable #-}

module Infer
  (
  )
where

import Data.Data (Data, Typeable)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Sequence as Seq
import Fuzzy
  ( Logic (..)
  , Value
  , bot
  , negation
  , tNorm
  , residuum
  , negation'
  , tCoNorm'
  , tNorm'
  , top
  )
import Text.Printf
import Utils (remove, round')


type Bounds = (Double, Double)


{- | neurons corresponding to propositional logical formulas equipped
  with a lower/upper bound (`_lb`/`_ub`) and a `_delta` value that
  ought to represent the latest change of bounds according to `diff`
-}
data Neuron
  = Atom {_symbol :: String, _lb :: Double, _ub :: Double, _delta :: Double}
  | Not {_x :: Int, _l :: Double, _ub :: Double, _d :: Double}
  | And {_xs :: [Int], _lb :: Double, _ub :: Double, _delta :: Double}
  | Or {_xs :: [Int], _lb :: Double, _ub :: Double, _delta :: Double}
  | IfThen {_lhs :: Int, _rhs :: Int, _lb :: Double, _ub :: Double, _delta :: Double}
  deriving
    ( Ord
    , Eq
    , Data
    , Typeable
    )


instance Show Neuron where
  show (Atom s _ _ _) = s
  show (Not x _ _ _) = "!(" ++ show x ++ ")"
  show (And xs _ _ _) = "(" ++ intercalate " & " (map show xs) ++ ")"
  show (Or xs _ _ _) = "(" ++ intercalate " | " (map show xs) ++ ")"
  show (IfThen lhs rhs _ _ _) = "(" ++ show lhs ++ " => " ++ show rhs ++ ")"


-- | returns bounds of a neuron
bounds :: Neuron -> Bounds
bounds n = (_lb n, _ub n)


-- | aggregates two input pairs of bounds
aggregate :: (Ord a, Ord b) => (a, b) -> (a, b) -> (a, b)
aggregate (l, u) (l', u') = (max l l', min u u')


-- | computes the difference of two input pairs of bounds
diff :: Num a => (a, a) -> (a, a) -> a 
diff (l, u) (l', u') = abs (l - l') + abs (u - u')

logic = Lukasiewicz

class Infer a where
  upward :: Seq.Seq a -> a -> a
  downward :: Seq.Seq a -> a -> [a]

instance Infer Neuron where
  upward lnn neuron = 
    case neuron of  
      Not x l u _ -> Not x l' u' d'
        where
          d' = diff (l,u) (l',u')
          (l',u') =  maybe (-1.0,-1.0) (aggregate (l,u) . activation . bounds) (access x)
          activation (a,b) = (negation logic b, negation logic a)
          access a = lnn Seq.!? a
      -- 
      And xs l u _ -> And xs l' u' d'
        where
          d' = diff (l,u) (l',u')
          --(l',u') =  activation (fmap (maybe (error "") bounds) (access xs)
          --activation = negation' logic $ foldr (tCoNorm' logic . negation' logic) (bot, bot)
          bla =  activation (fmap (maybe (error "") bounds) (access xs)
          access a = map (lnn Seq.!?) a
          

          --activationL j =
          --  if l > bot
          --  then residuum logic (tNorm logic . map snd . remove j $ bs) l
          --  else bot
          --activationU j =
          --  if u < top
          --  then residuum logic (tNorm logic . map fst . remove j $ bs) u
          --  else top
          --bs = map bounds ns
          --ns = mapMaybe (lnn Seq.!?) xs
