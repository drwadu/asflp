module Lnn.Neuro (Neuron (..), stringifyAnd, bounds, Lnn (..), Supports, showw, diff, update)
where

import Data.Foldable (toList)
import Data.List (intercalate)
import Lnn.Logic (Bounds, Symbol)

import Text.Printf (printf)

import qualified Data.Map as Map
import qualified Data.Sequence as Seq


-- | neuron corresponding to propositional logical formula
data Neuron
  = Atom {_symbol :: Symbol, _lb :: Double, _ub :: Double, _delta :: Double}
  | Not {_x :: Int, _lb :: Double, _ub :: Double, _delta :: Double}
  | And {_xs :: [Int], _lb :: Double, _ub :: Double, _delta :: Double}
  | Or {_xs :: [Int], _lb :: Double, _ub :: Double, _delta :: Double}
  | IfThen {_lhs :: Int, _rhs :: Int, _lb :: Double, _ub :: Double, _delta :: Double}
  | Proof {_lhs :: Int, _rhs :: Int, _lb :: Double, _ub :: Double, _delta :: Double}
  deriving
    ( Ord
    , Eq
    , Show
    )


stringifyNot s = "-" ++ s
stringifyAnd xs = "(" ++ intercalate " & " xs ++ ")"
stringifyOr xs = "(" ++ intercalate " | " xs ++ ")"
stringifyIfThen x y = "(" ++ x ++ " => " ++ y ++ ")"
stringifyProof x y = "[" ++ x ++ " <=> " ++ y ++ "]"


showw _ (Atom s _ _ _) = s
showw lnn@(Lnn nn _ _) (Not x _ _ _) = stringifyNot $ maybe (show x) (showw lnn) (Seq.lookup x nn)
showw lnn@(Lnn nn _ _) (And xs _ _ _) = stringifyAnd (map (\x -> maybe (show x) (showw lnn) (Seq.lookup x nn)) xs)
showw lnn@(Lnn nn _ _) (Or xs _ _ _) = stringifyOr (map (\x -> maybe (show x) (showw lnn) (Seq.lookup x nn)) xs)
showw lnn@(Lnn nn _ _) (IfThen lhs rhs _ _ _) = stringifyIfThen (maybe (show lhs) (showw lnn) (Seq.lookup lhs nn)) (maybe (show rhs) (showw lnn) (Seq.lookup rhs nn))
showw lnn@(Lnn nn _ _) (Proof lhs rhs _ _ _) = stringifyProof (maybe (show lhs) (showw lnn) (Seq.lookup lhs nn)) (maybe (show rhs) (showw lnn) (Seq.lookup rhs nn))


-- | returns bounds of a neuron
bounds :: Neuron -> Bounds
bounds n = (_lb n, _ub n)


-- | computes the difference of two input pairs of bounds
diff :: (Num a) => (a, a) -> (a, a) -> a
diff (l, u) (l', u') = abs (l - l') + abs (u - u')

update :: Neuron -> Double -> Double -> Neuron
update (Atom s l u _) l' u'   = Atom s l' u' (diff (l,u) (l',u'))
update (Not x l u _) l' u'    = Not x l' u' (diff (l,u) (l',u'))
update (And xs l u _) l' u' = And xs l' u' (diff (l,u) (l',u'))
update (Or xs l u _) l' u'  = Or xs l' u' (diff (l,u) (l',u'))
update (IfThen lhs rhs l u _) l' u' = IfThen lhs rhs l' u' (diff (l,u) (l',u'))
update (Proof lhs rhs l u _) l' u' = Proof lhs rhs l' u' (diff (l,u) (l',u')) 


type Supports = Map.Map Symbol [[Symbol]]
type Ast = Seq.Seq Neuron


-- | logical neural network (LNN)
data Lnn = Lnn {ast :: Ast, size :: Int, delta :: Double}


-- deriving (Show)

instance Show Lnn where
  show lnn@(Lnn nn k d) = show k ++ " " ++ show d ++ "\n" ++ intercalate "\n" (map (\n -> "(" ++ printf "%.2f" (_lb n) ++ "," ++ printf "%.2f" (_ub n) ++ ") " ++ printf "%.2f" d ++ " // " ++ showw lnn n) (toList nn))
