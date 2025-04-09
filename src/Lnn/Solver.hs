module Lnn.Solver
  ( Pass (..)
  , upwardPass
  , downwardPass
  , Lnn (..)
  , approximate
  , infer
  , solve
  , solveDbg
  , inferDbg
  )
where

import Data.Foldable (toList)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Lnn.Compiler (compile)
import Lnn.Logic
  ( Eval (..)
  , Logic (..)
  , bot
  , negation
  , negation'
  , tCoNorm'
  , tNorm'
  , top
  )
import Lnn.Neuro
  ( Lnn (..)
  , Neuron (..)
  , bounds
  , diff
  , showw
  , update
  )
import Text.Printf (printf)
import Utils (remove)


-- | aggregates two input pairs of bounds
aggregate :: (Ord a, Ord b) => (a, b) -> (a, b) -> (a, b)
aggregate (l, u) (l', u') = (max l l', min u u')


logic :: Logic
logic = Lukasiewicz


-- logic = Goedel
-- logic = Zadeh

class Pass a where
  upward :: Seq.Seq a -> a -> a
  downward :: Seq.Seq a -> a -> [a]


instance Pass Neuron where
  upward lnn neuron = case neuron of
    -- \| upward pass for negation
    Not x l u -> Not x l' u'
     where
      (l', u') = maybe undefined (aggregate (l, u) . activation . bounds) $ lnn Seq.!? x
      activation (a, b) = (negation logic b, negation logic a)
    -- \| upward pass for conjunction
    And xs l u -> And xs l' u'
     where
      (l', u') = aggregate (l, u) (a, b)
      (a, b) = activation (mapMaybe (lnn Seq.!?) xs)
      activation = negation' logic . foldr (tCoNorm' logic . negation' logic . bounds) (bot, bot)
    -- \| upward pass for disjunction
    Or xs l u -> Or xs l' u'
     where
      (l', u') = aggregate (l, u) $ activation (mapMaybe (lnn Seq.!?) xs)
      activation = foldr (tCoNorm' logic . bounds) (bot, bot)
    -- \| upward pass for implication
    IfThen lhs rhs l u -> IfThen lhs rhs l' u'
     where
      (l', u') = aggregate (l, u) $ tCoNorm' logic (lhsU, lhsL) (rhsL, rhsU)
      (lhsL, lhsU) = negation' logic . head $ bs
      (rhsL, rhsU) = last bs
      bs = map bounds . mapMaybe (lnn Seq.!?) $ [lhs, rhs]
    -- \| upward pass for equivalence
    Proof lhs rhs l u -> Proof lhs rhs l' u'
     where
      (l', u') = aggregate (l, u) (a, b)
      (a, b) = activation (mapMaybe (lnn Seq.!?) [lhs, rhs])
      activation = negation' logic . foldr (tCoNorm' logic . negation' logic . bounds) (bot, bot)
    _ -> neuron


  downward lnn neuron = case neuron of
    -- \| downward pass for negation
    Not x l u -> [update x' l' u']
     where
      (l', u') = aggregate (negation logic u, negation logic l) . bounds $ x'
      x' = fromMaybe undefined (lnn Seq.!? x)
    -- \| downward pass for conjunction
    And xs l u ->
      zipWith (curry (\(i, (b, n)) -> uncurry (update n) (aggregate b (tl i, tu i)))) [0 ..] (zip bs xs')
     where
      tl j =
        -- max (residuum logic (tNorm logic . map snd . remove j $ bs) l) bot
        if l > bot
          then residuum logic (tNorm logic . map snd . remove j $ bs) l
          else bot
      tu j =
        -- min (residuum logic (tNorm logic . map fst . remove j $ bs) u) top
        if u < top
          then residuum logic (tNorm logic . map fst . remove j $ bs) u
          else top
      bs = map bounds xs'
      xs' = mapMaybe (lnn Seq.!?) xs
    -- \| downward pass for disjunction
    Or xs l u ->
      zipWith (curry (\(i, (b, n)) -> uncurry (update n) (aggregate b (tl i, tu i)))) [0 ..] (zip bs xs')
     where
      tl j =
        -- max (tNorm logic [tNorm logic . map (negation logic . snd) . remove j $ bs, l]) bot
        if l > bot
          then tNorm logic [tNorm logic . map (negation logic . snd) . remove j $ bs, l]
          else bot
      tu j =
        -- min (tNorm logic [tNorm logic . map (negation logic . fst) . remove j $ bs, u]) top
        if u < top
          then tNorm logic [tNorm logic . map (negation logic . fst) . remove j $ bs, u]
          else top
      bs = map bounds xs'
      xs' = mapMaybe (lnn Seq.!?) xs
    -- \| downward pass for implication
    IfThen lhs rhs l u ->
      [ uncurry (update nLhs) (aggregate (lLhs, uLhs) (lx', ux'))
      , uncurry (update nRhs) (aggregate (lRhs, uRhs) (ly', uy'))
      ]
     where
      lx' = if u < top then residuum logic u lRhs else bot
      ux' = if l > bot then residuum logic l uRhs else top
      ly' = if l > bot then tNorm logic [lLhs, l] else bot
      uy' = if u < top then tNorm logic [uLhs, u] else top
      (lLhs, uLhs) = bounds nLhs
      (lRhs, uRhs) = bounds nRhs
      nLhs = fromMaybe (error "") $ Seq.lookup lhs lnn
      nRhs = fromMaybe (error "") $ Seq.lookup rhs lnn
    -- \| downward pass for equivalence
    Proof lhs rhs l u ->
      zipWith (curry (\(i, (b, n)) -> uncurry (update n) (aggregate b (tl i, tu i)))) [0 ..] (zip bs xs')
     where
      tl j =
        -- max (residuum logic (tNorm logic . map snd . remove j $ bs) l) bot
        if l > bot
          then residuum logic (tNorm logic . map snd . remove j $ bs) l
          else bot
      tu j =
        -- min (residuum logic (tNorm logic . map fst . remove j $ bs) u) top
        if u < top
          then residuum logic (tNorm logic . map fst . remove j $ bs) u
          else top
      bs = map bounds xs'
      xs' = mapMaybe (lnn Seq.!?) [lhs, rhs]
    _ -> []


upwardPass (Lnn nn k _) = Lnn nn' k (lnnCmp nn nn')
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
  aux (Atom{}) ns = ns
  aux n@(Not x _ _) ns = upd ns [(x, head (downward ns n))]
  aux n@(IfThen lhs rhs _ _) ns = upd ns $ zip [lhs, rhs] (downward ns n)
  aux n@(Proof lhs rhs _ _) ns = upd ns $ zip [lhs, rhs] (downward ns n)
  aux n ns = upd ns $ zip (_xs n) (downward ns n)
  upd s [] = s
  upd s ((j, x) : xs) = upd (Seq.update j y s) xs
   where
    jn = fromMaybe undefined $ nn Seq.!? j
    jbs = bounds jn
    xbs = bounds x
    y = uncurry (update jn) (aggregate jbs xbs)


downwardPass (Lnn nn k _) = Lnn nn' k d'
 where
  d' = lnnCmp nn nn'
  nn' = downwardPass' (k - 1) nn


approximate lnn = do
  return lnn''
 where
  lnn''@(Lnn nn'' _ d'') = downwardPass lnn'
  lnn'@(Lnn nn' _ d') = upwardPass lnn


approximateDbg lnn = do
  print "========="
  print lnn'
  print "/////////"
  print lnn''
  print "========="
  return lnn''
 where
  lnn''@(Lnn nn'' _ d'') = downwardPass lnn'
  lnn'@(Lnn nn' _ d') = upwardPass lnn


tc :: String
tc = "\x1b[0;30;42m[T]\x1b[0m"


fc :: String
fc = "\x1b[0;30;41m[F]\x1b[0m"


uc :: String
uc = "\x1b[0;30;44m[U]\x1b[0m"


display (Atom a l u)
  | (l <= negation logic 0.5) && (u >= 0.5) =
      uc ++ " [" ++ printf "%.2f" l ++ ";" ++ printf "%.2f" u ++ "]" ++ " " ++ a ++ "\n"
  | (l >= 0.5) && (u >= 0.5) =
      tc ++ " [" ++ printf "%.2f" l ++ ";" ++ printf "%.2f" u ++ "]" ++ " " ++ a ++ "\n"
  | otherwise =
      fc ++ " [" ++ printf "%.2f" l ++ ";" ++ printf "%.2f" u ++ "]" ++ " " ++ a ++ "\n"
display _ = ""


inferDbg lnn = do
  lnn'@(Lnn _ _ d) <- approximateDbg lnn
  if lnnCmp (ast lnn) (ast lnn') <= epsilon
    then do
      putStrLn $ "++++++ DONE " ++ printf "%.2f" d
      print lnn'
      putStrLn "++++++ DONE "
      mapM_ (putStr . display) $ ast lnn'
      return lnn'
    else
      inferDbg lnn'
 where
  epsilon = 0.0001


infer lnn = do
  lnn'@(Lnn _ _ d) <- approximate lnn
  if lnnCmp (ast lnn) (ast lnn') <= epsilon
    then do
      mapM_ (putStr . display) $ ast lnn'
      return lnn'
    else
      infer lnn'
 where
  epsilon = 0.0001


lnnCmp a b = sum $ zipWith (curry f) (g a) (g b)
 where
  g = map bounds . toList
  f ((l, u), (l', u')) = abs (l - l') + abs (u - u')
  bounds n = (_lb n, _ub n)


solve flp inputs = do
  infer dLnn
 where
  dLnn = downwardPass lnnT
  lnnT = Lnn (Seq.take (k - 1) (ast uLnn) Seq.|> update (fromMaybe undefined (nn Seq.!? (k - 1))) top top) k (delta uLnn)
  uLnn = upwardPass lnn
  lnn@(Lnn nn k _) = compile flp inputs


solveDbg flp inputs = do
  putStrLn "~~~~~~ IN"
  mapM_ print (Map.toList inputs)
  putStrLn $ "------ UP " ++ printf "%.2f" (delta uLnn)
  print uLnn
  putStrLn $ "------ DOWN T " ++ printf "%.2f" (delta dLnn)
  print dLnn
  inferDbg dLnn
 where
  dLnn = downwardPass lnnT
  lnnT = Lnn (Seq.take (k - 1) (ast uLnn) Seq.|> update (fromMaybe undefined (nn Seq.!? (k - 1))) top top) k (delta uLnn)
  uLnn = upwardPass lnn
  lnn@(Lnn nn k _) = compile flp inputs
