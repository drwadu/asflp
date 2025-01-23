module Lnn.Solver
  ( Pass (..), upwardPass, downwardPass, Lnn (..), approximate, infer, solve
  ) where

import Lnn.Neuro (bounds, Neuron (..), Lnn (..), update, diff)
import Lnn.Logic
  ( Eval (..),
    Logic (..),
    bot,
    negation',
    tCoNorm',
    tNorm',
    top,
  )
import Utils (remove)

import Data.Foldable (toList)
import Data.List (intercalate)

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Data.Maybe (fromMaybe, mapMaybe)
import Lnn.Compiler (compile)


-- | aggregates two input pairs of bounds
aggregate :: (Ord a, Ord b) => (a, b) -> (a, b) -> (a, b)
aggregate (l, u) (l', u') = (max l l', min u u')



logic :: Logic
logic = Lukasiewicz

class Pass a where
  upward :: Seq.Seq a -> a -> a
  downward :: Seq.Seq a -> a -> [a]

instance Pass Neuron where
  upward lnn neuron = case neuron of
    -- | upward pass for negation
    Not x l u _ -> Not x l' u' (diff (l,u) (l',u'))
      where
        (l', u') = maybe undefined (aggregate (l, u) . activation . bounds) $ lnn Seq.!? x 
        activation (a,b) = (negation logic b, negation logic a)
    -- | upward pass for conjunction
    And xs l u _ -> And xs l' u' (diff (l,u) (l',u'))
      where
        (l', u') = aggregate (l, u) (a, b)
        (a, b) = activation (mapMaybe (lnn Seq.!?) xs)
        activation = negation' logic . foldr (tCoNorm' logic . negation' logic . bounds) (bot, bot) 
    -- | upward pass for disjunction
    Or xs l u _ -> Or xs l' u' (diff (l,u) (l',u'))
      where
        (l', u') = aggregate (l, u) $ activation (mapMaybe (lnn Seq.!?) xs)
        activation = foldr (tCoNorm' logic . bounds) (bot, bot)
    -- | upward pass for implication
    IfThen lhs rhs l u _ -> IfThen lhs rhs l' u' (diff (l,u) (l',u')) 
      where
        (l', u') = aggregate (l, u) $ tCoNorm' logic (lhsU, lhsL) (rhsL, rhsU)
        (lhsL, lhsU) = negation' logic . head $ bs
        (rhsL, rhsU) = last bs
        bs = map bounds . mapMaybe (lnn Seq.!?) $ [lhs, rhs]
    -- | upward pass for equivalence
    Proof lhs rhs l u _ -> Proof lhs rhs l' u' (diff (l,u) (l',u')) 
      where 
        (l', u') = aggregate (l, u) (a, b)
        (a, b) = activation (mapMaybe (lnn Seq.!?) [lhs,rhs])
        activation = negation' logic . foldr (tCoNorm' logic . negation' logic . bounds) (bot, bot) 
    _ -> neuron

  downward lnn neuron = case neuron of
    -- | downward pass for negation
    Not x l u d -> [update x' l' u']
      where
        (l', u') = aggregate (negation logic u, negation logic l) . bounds $ x'
        x' = fromMaybe undefined (lnn Seq.!? x)
    -- | downward pass for conjunction
    And xs l u _ ->
      zipWith (curry (\(i, (b, n)) -> uncurry (update n) (aggregate b (tl i, tu i)))) [0 ..] (zip bs xs')
      where
        tl j =
          if l > bot
            then residuum logic (tNorm logic . map snd . remove j $ bs) l
            else bot
        tu j =
          if u < top
            then residuum logic (tNorm logic . map fst . remove j $ bs) u
            else top
        bs = map bounds xs'
        xs' = mapMaybe (lnn Seq.!?) xs
    -- | downward pass for disjunction
    Or xs l u _ ->
      zipWith (curry (\(i, (b, n)) -> uncurry (update n) (aggregate b (tl i, tu i)))) [0 ..] (zip bs xs')
      where
        tl j =
          if l > bot
            then tNorm logic [tNorm logic . map (negation logic . snd) . remove j $ bs, l]
            else bot
        tu j =
          if u < top
            then tNorm logic [tNorm logic . map (negation logic . fst) . remove j $ bs, u]
            else top
        bs = map bounds xs'
        xs' = mapMaybe (lnn Seq.!?) xs
    -- | downward pass for implication
    IfThen lhs rhs l u _ ->
      [ uncurry (update nx) (aggregate (lx, ux) (lx', ux')),
        uncurry (update ny) (aggregate (ly, uy) (ly', uy'))
      ]
      where
        lx' = if u < top then residuum logic u ly else bot
        ux' = if l > bot then residuum logic l uy else top
        ly' = if l > bot then tNorm logic [lx, l] else bot
        uy' = if u < top then tNorm logic [ux, u] else top
        (lx, ux) = bounds nx
        (ly, uy) = bounds ny
        nx = fromMaybe (error "") $ Seq.lookup lhs lnn
        ny = fromMaybe (error "") $ Seq.lookup rhs lnn
    -- | downward pass for equivalence
    Proof lhs rhs l u _ ->
      zipWith (curry (\(i, (b, n)) -> uncurry (update n) (aggregate b (tl i, tu i)))) [0 ..] (zip bs xs')
      where
        tl j =
          if l > bot
            then residuum logic (tNorm logic . map snd . remove j $ bs) l
            else bot
        tu j =
          if u < top
            then residuum logic (tNorm logic . map fst . remove j $ bs) u
            else top
        bs = map bounds xs'
        xs' = mapMaybe (lnn Seq.!?) [lhs,rhs]
    _ -> []

--upwardPass :: (Pass a, Foldable t) => t a -> Seq.Seq a
upwardPass (Lnn nn k _) = Lnn nn' k ((sum $ map _delta $ toList nn') / fromIntegral (2*k))
  where
    nn' = aux (toList nn) Nothing
    aux [] (Just ns) = ns
    aux [x] (Just ns) = ns Seq.|> upward ns x
    aux (x : xs) (Just ns) = aux xs $ Just (ns Seq.|> upward ns x)
    aux xs Nothing = aux xs $ Just Seq.Empty

downwardPass' 0 nn = nn
downwardPass' i nn = downwardPass' (i - 1) nn'
  where
    nn' = aux (Seq.index nn i) nn
    aux (Atom {}) ns = ns
    aux n@(Not x _ _ _) ns = upd ns [(x, head (downward ns n))]
    aux n@(IfThen lhs rhs _ _ _) ns = upd ns $ zip [lhs, rhs] (downward ns n)
    aux n@(Proof lhs rhs _ _ _) ns = upd ns $ zip [lhs, rhs] (downward ns n)
    aux n ns = upd ns $ zip (_xs n) (downward ns n)
    upd s [] = s
    --upd s ((j, x) : xs) = upd (Seq.update j x s) xs
    upd s ((j, x) : xs) = 
      upd (Seq.update j y s) xs
        where
          jn = fromMaybe undefined $ nn Seq.!? j
          jbs = bounds jn
          xbs = bounds x
          y = uncurry (update jn) (aggregate jbs xbs)

downwardPass (Lnn nn k _) = Lnn nn' k d'
  where
    d' = (sum . map _delta . toList $ nn') / fromIntegral (2*k)
    nn' = downwardPass' (k - 1) nn

approximate lnn = downwardPass lnn'
  where
    lnn' = upwardPass lnn

changes lnn@(Lnn nn k d) = show d ++ "\n" ++ show (Lnn (Seq.filter ((==) 0.0 . _delta) nn) k d)
--changes lnn@(Lnn nn k d) = show d ++ "\n" ++ show (Seq.fromList $ filter ((>) 0.0 . _delta) $ toList nn)

infer lnn = do 
  if delta lnn' <= epsilon 
  then 
    do
    putStrLn "+++++++"
    return lnn' 
  else 
    do
     putStrLn "-------"
     print lnn'
     infer lnn'
  where
    lnn' = approximate lnn
    epsilon = 0.0001

solve flp inputs = do 
    putStrLn "-------"
    print uLnn
    putStrLn "-------"
    print dLnn
    infer dLnn
      where 
        dLnn = downwardPass lnnT 
        lnnT = Lnn (Seq.take (k-1) (ast uLnn) Seq.|> (update (fromMaybe undefined (nn Seq.!? (k-1))) top top)) k (delta uLnn)
        uLnn = upwardPass lnn
        lnn@(Lnn nn k _) = compile flp inputs
