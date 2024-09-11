module Lib
  ( atoms,
    inputs,
    natoms,
    complete,
    unlnnify,
    lnnify,
    rootify,
    parse,
    solveWithAssumptions,
    solveWithAssumptionsBb,
    solveWithAssumptionsVis,
    solveWithAssumptionsExp,
    solveWithAssumptionsDebug,
    parseBounds',
    parseBodyBounds',
    dbg,
    display,
    displayRanked,
    updateBounds,
    Neuron (V, N, A, O, I),
    solve,
    lnnMap',
    clarkComplete,
    infer,
    lnnCmp,
    approximate,
    epsilon,
    down,
    up,
    pascal,
  )
where

import Data.Either
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe, maybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Lnn
  ( Net,
    Neuron
      ( A,
        I,
        N,
        O,
        V
      ),
    Value,
    approximate,
    bot,
    boundsPredicate,
    clamp,
    down,
    downwardPass,
    epsilon,
    infer,
    inferDbg,
    lnnCmp,
    lnnMap,
    lnnMap',
    lnnZip,
    lnnify,
    negation,
    repr,
    residuum,
    roundBounds,
    tCoNorm,
    tNorm,
    top,
    unlnnify,
    up,
    updateBounds,
    upwardPass,
  )
import Parser
  ( Sym,
    parse,
    parseBodyBounds',
    parseBounds',
  )
import System.Environment ()
import System.Exit ()
import System.IO ()
import Utils (round')
import Visual (dot)

solve :: Foldable t => Int -> t Neuron -> Seq.Seq Neuron
-- solve i ns = vs
solve i ns = ns'
  where
    -- vs = Seq.filter f $ infer i ns'
    -- f V {} = True
    -- f _ = False
    ns' = down i $ lnnify (init uns ++ [root])
    root = updateBounds (top, top) $ last uns
    uns = unlnnify $ up (unlnnify ns) Nothing

atomify :: Map.Map String (Value, Value) -> String -> Neuron
atomify as a = maybe (V a bot top) (uncurry (V a)) $ Map.lookup a as

inputs as vs = lnnify $ map (atomify as) vs

atoms m = vs
  where
    vs = Set.toList . Set.fromList $ hs ++ map (\x -> if head x == '-' then tail x else x) bs
    hs = Map.keys m
    bs = concat . concat . Map.foldr (:) [] $ m

natoms m ns = map (\x -> N x (find' ns (tail x)) bot top) vs
  where
    vs = Set.toList . Set.fromList $ filter (\x -> head x == '-') bs
    hs = Map.keys m
    bs = concat . concat . Map.foldr (:) [] $ m

justifications m ns a = if rhs /= [] then res else []
  where
    v = find' ns a
    j = cl + length ands + 1
    res =
      if lchi > 1
        then
          ands
            ++ [ O ("[" ++ a ++ "] OR " ++ (unwords $ oth ++ (map repr ands))) chi bot top,
                 I ("rhs " ++ a) j v bot top,
                 I ("lhs " ++ a) v j bot top,
                 A ("proof " ++ a) [j + 1, j + 2] bot top
               ]
        else
          if length ands > 0
            then
              ands
                ++ [ I ("rhs " ++ a) (cl + 1) v bot top,
                     I ("lhs " ++ a) v (cl + 1) bot top,
                     A ("proof " ++ a) [cl + 2, cl + 3] bot top
                   ]
            else
              [ I ("rhs " ++ a) (head vi) v bot top,
                I ("lhs " ++ a) v (head vi) bot top,
                A ("proof " ++ a) [cl + 1, cl + 2] bot top
              ]
    lchi = length chi
    chi = vi ++ ni
    lvi = length vi
    vi = map (find' ns) oth
    ni = [x + cl | x <- [1 .. ta]]
    ands = map (orify m ns) pcs
    oth = concat $ filter (\xs -> length xs == 1) $ rhs
    ta = length pcs
    pcs = filter (\xs -> length xs > 1) rhs
    rhs = maybe [] id $ Map.lookup a m
    cl = (Seq.length ns) - 1

justificationsFor m ns a = if rhs /= [] then res else []
  where
    v = find' ns a
    j = cl + length ands + 1
    res =
      if lchi > 1
        then
          ands
            ++ [ O ("[" ++ a ++ "] OR " ++ (unwords $ map (repr . findNeuronByIndex ns) idxs ++ (map repr ands))) chi bot top,
                 I ("rhs " ++ a) j v bot top,
                 I ("lhs " ++ a) v j bot top,
                 A ("proof " ++ a) [j + 1, j + 2] bot top
               ]
        else
          if not (null ands)
            then
              ands
                ++ [ I ("rhs " ++ a) (cl + 1) v bot top,
                     I ("lhs " ++ a) v (cl + 1) bot top,
                     A ("proof " ++ a) [cl + 2, cl + 3] bot top
                   ]
            else
              [ I ("rhs " ++ a) (head chi) v bot top,
                I ("lhs " ++ a) v (head chi) bot top,
                A ("proof " ++ a) [cl + 1, cl + 2] bot top
              ]
    lchi = length chi
    chi = idxs ++ [i + cl | i <- [1 .. (length ands)]]
    (idxs, ands) = partitionEithers $ map (andify ns) rhs
    rhs = fromMaybe [] $ Map.lookup a m
    cl = Seq.length ns - 1

clark m ns a = if rhs /= [] then res else []
  where
    v = find' ns a
    j = cl + length ands + 1
    res =
      if lchi > 1
        then
          ands
            ++ [ O ("[" ++ a ++ "] OR " ++ (unwords $ map (repr . findNeuronByIndex ns) idxs ++ (map repr ands))) chi bot top,
                 I ("rhs " ++ a) j v bot top,
                 I ("lhs " ++ a) v j bot top,
                 A ("proof " ++ a) [j + 1, j + 2] bot top
               ]
        else
          if not (null ands)
            then
              ands
                ++ [ I ("rhs " ++ a) (cl + 1) v bot top,
                     I ("lhs " ++ a) v (cl + 1) bot top,
                     A ("proof " ++ a) [cl + 2, cl + 3] bot top
                   ]
            else
              [ I ("rhs " ++ a) (head chi) v bot top,
                I ("lhs " ++ a) v (head chi) bot top,
                A ("proof " ++ a) [cl + 1, cl + 2] bot top
              ]
    lchi = length chi
    chi = idxs ++ [i + cl | i <- [1 .. (length ands)]]
    (idxs, ands) = partitionEithers $ map (andifyNaive ns a) rhs
    rhs = fromMaybe [] $ Map.lookup a m
    cl = Seq.length ns - 1

-- andifyNaive :: Seq.Seq Neuron -> [String] -> Either Int Neuron
andifyNaive ns a xs =
  if length xs > 1
    then Right $ A (a ++ " (AND " ++ unwords xs' ++ ")") (map (find' ns) xs') bot top
    else Left $ find' ns $ head xs'
  where
    xs' = List.sort xs

orify :: p -> Seq.Seq Neuron -> [String] -> Neuron
orify m ns xs =
  if length xs > 1
    then A ("(AND " ++ unwords xs ++ ")") (map (find' ns) xs) bot top
    else Seq.index ns $ find' ns $ head xs

andify :: Seq.Seq Neuron -> [String] -> Either Int Neuron
andify ns xs =
  if length xs > 1
    then case findNeuron ns s of
      Just n -> Left . fromMaybe (-1) . Seq.findIndexL (\n' -> repr n' == repr n) $ ns
      _ -> Right (A s (map (find' ns) xs') bot top)
    else -- maybe (Just (A s (map (find' ns) xs') bot top)) Nothing $ findNeuron ns s

      Left . find' ns . head $ xs
  where
    -- findNeuron ns $ head xs
    -- Nothing

    s = "(AND " ++ unwords xs' ++ ")"
    xs' = List.sort xs

findNeuron :: Seq.Seq Neuron -> String -> Maybe Neuron
findNeuron ns x = fmap (Seq.index ns) $ Seq.findIndexL (\n -> (repr n) == x) ns

findNeuronByIndex :: Seq.Seq Neuron -> Int -> Neuron
findNeuronByIndex ns x = Seq.index ns x

find' :: Seq.Seq Neuron -> String -> Int
find' ns x = maybe (-1) id $ Seq.findIndexL (\n -> (repr n) == x) ns

complete :: Map.Map String [[String]] -> Seq.Seq Neuron -> [String] -> Seq.Seq Neuron

complete_ m ns [] = ns
complete_ m ns (a : as) = complete_ m ns' as
  where
    ns' = ns Seq.>< (lnnify $ justifications m ns a)

complete _ ns [] = ns
complete m ns (a : as) = complete m ns' as
  where
    ns' = ns Seq.>< (lnnify $ justificationsFor m ns a)

clarkComplete _ ns [] = ns
clarkComplete m ns (a : as) = clarkComplete m ns' as
  where
    ns' = ns Seq.>< (lnnify $ clark m ns a)

root :: Seq.Seq Neuron -> [[Char]] -> Neuron
root a b = A "root" (filter (> 0) $ map (\x -> find' a ("proof " ++ x)) b) bot top

rootify :: Seq.Seq Neuron -> [[Char]] -> Seq.Seq Neuron
rootify a b = a Seq.>< (lnnify [root a b])

solveWithAssumptions ::
  (Fractional t1, Foldable t2) =>
  Int ->
  t2 Neuron ->
  Map.Map String (Value, Value) ->
  (t1 -> Neuron -> b) ->
  [b]
solveWithAssumptions i ns as p = map (p 0.5) . filter f . unlnnify $ infer i ns'
  where
    ns' = down i $ lnnify ((init uns) ++ [root])
    root = updateBounds (top, top) $ last uns
    uns = unlnnify $ up vns Nothing
    vns = map (g as) $ unlnnify ns
    f V {} = True
    f _ = False
    g m (V a l u) = maybe (V a l u) (\b -> updateBounds b (V a l u)) $ Map.lookup a as
    g _ n = n

solveWithAssumptionsBb i ns as as' p = map (p 0.5) . filter f . unlnnify $ infer i ns'
  where
    ns' = down i $ lnnify ((init uns') ++ [root])
    root = updateBounds (top, top) $ last uns'
    uns' = map (g' as') $ unlnnify uns
    uns = unlnnify $ up vns Nothing
    vns = map (g as) $ unlnnify ns
    f V {} = True
    f _ = False
    g m (V a l u) = maybe (V a l u) (\b -> updateBounds b (V a l u)) $ Map.lookup a m
    g _ n = n
    g' m (A a xs l u) = maybe (A a xs l u) (\b -> updateBounds b (A a xs l u)) $ Map.lookup a m
    g' _ n = n

solveWithAssumptionsExp i ns as as' p = map (p 0.5) . filter f . unlnnify $ infer i ns'
  where
    ns' = down i $ lnnify (init uns' ++ [root])
    root = updateBounds (top, top) $ last uns'
    uns' = unlnnify $ up uns Nothing
    uns = map (g' as') $ unlnnify vns
    vns = map (g as) $ unlnnify ns
    g m' (V a l u) = maybe (V a l u) (\b -> updateBounds b (V a l u)) $ Map.lookup a m'
    g _ n = n
    g' m' (A a xs l u) = maybe (A a xs l u) (\b -> updateBounds b (A a xs l u)) $ Map.lookup a m'
    g' _ n = n
    f V {} = True
    f _ = False

solveWithAssumptionsDbg ::
  Foldable t =>
  Int ->
  t Neuron ->
  Map.Map String (Value, Value) ->
  [String]
solveWithAssumptionsDbg i ns as = [show (map (display 0.5) . filter f . unlnnify $ fi, unlnnify fi)]
  where
    fi = infer i ns'
    ns' = down i $ lnnify ((init uns) ++ [root])
    root = updateBounds (top, top) $ last uns
    uns = unlnnify $ up vns Nothing
    vns = map (g as) $ unlnnify ns
    f V {} = True
    f _ = False
    g m (V a l u) = maybe (V a l u) (\b -> updateBounds b (V a l u)) $ Map.lookup a m
    g _ n = n

solveWithAssumptionsVis i ns as = ["digraph nnf2dot {\nedge[arrowhead=none];\n"] ++ (map dot $ zip [0 ..] (unlnnify $ infer i ns')) ++ ["}"]
  where
    ns' = down i $ lnnify ((init uns) ++ [root])
    root = updateBounds (top, top) $ last uns
    uns = unlnnify $ up vns Nothing
    vns = map (g as) $ unlnnify ns
    f V {} = True
    f _ = False
    g m (V a l u) = maybe (V a l u) (\b -> updateBounds b (V a l u)) $ Map.lookup a as
    g _ n = n

dbg :: (Show b, Foldable t) => t b -> IO ()
dbg ns = mapM_ print (zip [0 ..] (unlnnify ns))

tc :: String
tc = "\x1b[0;30;42m[T]\x1b[0m"

fc :: String
fc = "\x1b[0;30;41m[F]\x1b[0m"

uc :: String
uc = "\x1b[0;30;44m[U]\x1b[0m"

red :: [Char] -> [Char]
red x = "\x1b[0;30;41m" ++ x ++ "\x1b[0m"

blue :: [Char] -> [Char]
blue x = "\x1b[0;30;44m" ++ x ++ "\x1b[0m"

green :: [Char] -> [Char]
green x = "\x1b[0;30;42m" ++ x ++ "\x1b[0m"

display :: Value -> Neuron -> [Char]
display alpha (V a l u)
  | (l <= (negation alpha)) && (u >= alpha) =
      uc ++ " " ++ show (round' l, round' u) ++ " " ++ a ++ "\n"
  | (l >= alpha) && (u >= alpha) =
      tc ++ " " ++ show (round' l, round' u) ++ " " ++ a ++ "\n"
  | otherwise =
      fc ++ " " ++ show (round' l, round' u) ++ " " ++ a ++ "\n"

displayRanked :: Value -> Neuron -> [Char]
displayRanked alpha (V a l u)
  | (l <= (negation alpha)) && (u >= alpha) =
      blue (show . round' $ (l + u) / 2.0) ++ " " ++ show (round' l, round' u) ++ " " ++ a ++ "\n"
  | (l >= alpha) && (u >= alpha) =
      green (show . round' $ (l + u) / 2.0) ++ " " ++ show (round' l, round' u) ++ " " ++ a ++ "\n"
  | otherwise =
      red (show . round' $ (l + u) / 2.0) ++ " " ++ show (round' l, round' u) ++ " " ++ a ++ "\n"

solveWithAssumptionsDebug i ns as = do
  -- mapM_ putStrLn $ filter f $ unlnnify ns'
  mapM_ (putStr . boundsPredicate 0) . filter f . unlnnify $ ns'
  putStrLn "\n"
  -- mapM_ (putStr . map (boundsPredicate 0) . filter f . unlnnify) $ inferDbg i [ns']
  let x = inferDbg i [ns']
  mapM_ (putStrLn . show) $ zip [1 ..] x
  where
    -- mapM_ (putStrLn . show . (\(j, y) -> map (filter f . unlnnify) y)) $ zip [1 ..] x
    --
    -- mapM_ (putStrLn . unwords . (\(j, y) -> map (boundsPredicate j) . filter f $ unlnnify y)) $ zip [1 ..] x

    ns' = down i $ lnnify (init uns ++ [root])
    root = updateBounds (top, top) $ last uns
    uns = unlnnify $ up vns Nothing
    vns = map (g as) $ unlnnify ns
    f V {} = True
    f _ = False
    g m (V a l u) = maybe (V a l u) (\b -> updateBounds b (V a l u)) $ Map.lookup a as
    g _ n = n

initLpP x = ns''
  where
    ns'' = down (length ns' - 1) $ lnnify ns'
    ns' = init uns ++ [root]
    root = updateBounds (1.0, 1.0) $ last uns
    uns = unlnnify $ up vns Nothing
    vns = map (g as) $ unlnnify ns
    as = Map.fromList . map parseBounds' $ filter (\l -> '[' `elem` l) $ filter (\l -> ':' `notElem` l) $ filter (\l -> length l > 1) $ lines x
    g m (V a l u) = maybe (V a l u) (\b -> updateBounds b (V a l u)) $ Map.lookup a m
    g _ n = n
    ns = rootify ns3 ns0
    ns3 = clarkComplete m ns2 ns0
    ns2 = ns1 Seq.>< lnnify (natoms m ns1)
    ns1 = inputs Map.empty ns0
    ns0 = atoms m
    m = parse Map.empty x

inferLpP ns = if lnnCmp ns ns' <= epsilon then ns else infer i ns'
  where
    ns' = approximate i ns
    i = length ns - 1

inferUntil j n xs =
  if (lnnCmp ns ns' <= epsilon) || (j == n) then xs else inferUntil (j + 1) n (xs ++ [ns'])
  where
    ns = last xs
    ns' = approximate i ns
    i = length ns - 1

inferUntilDiff j n xs =
  if (lnnCmp ns ns' <= epsilon) || (j == n) then xs else inferUntil (j + 1) n (xs ++ [ns'])
  where
    ns = last xs
    ns' = approximate i ns
    i = length ns - 1

-- interpretationP ns = map (boundsPredicate 100) . filter f $ ns
interpretationP ns = filter f $ ns
  where
    f V {} = True
    f _ = False

pascal x n = do
  ns <- fmap initLpP (readFile x)
  print . map (boundsPredicate 0) . filter f . unlnnify $ ns
  -- mapM_ print $ map (g 100) $ inferUntil 0 n [ns]
  -- mapM_ print $ map (uncurry g) $ zip [1 ..] (inferUntil 0 n [ns])
  mapM_ print $ zipWith (curry (uncurry g)) [1 ..] (inferUntil 0 n [ns])
  where
    g i = map (boundsPredicate i) . filter f . unlnnify
    f V {} = True
    f _ = False

dag x n = do
  ns <- fmap initLpP (readFile x)
  mapM_ (putStrLn . show) $ zip [0 ..] (unlnnify $ ns)
