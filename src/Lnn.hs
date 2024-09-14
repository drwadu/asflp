module Lnn
  ( clamp,
    top,
    bot,
    negation,
    tNorm,
    tCoNorm,
    residuum,
    up,
    down,
    infer,
    inferDbg,
    boundsPredicate,
    lnnify,
    unlnnify,
    updateBounds,
    roundBounds,
    lnnMap,
    lnnMap',
    lnnCmp,
    lnnZip,
    epsilon,
    repr,
    upwardPass,
    downwardPass,
    approximate,
    Net,
    Value,
    Neuron
      ( V,
        N,
        A,
        O,
        I
      ),
  )
where

import Bounds
  ( bounds,
    clampBounds,
    invertBounds,
    roundBounds,
    sumBounds,
    updateBounds,
  )
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Sequence as Seq
import Fuzzy (Value, bot, clamp, top)
import Lukasiewicz
  ( negation,
    residuum,
    tCoNorm,
    tNorm,
  )
import Neuron
  ( Neuron
      ( A,
        I,
        N,
        O,
        V
      ),
    repr,
  )
import Utils (remove)

type Net = Seq.Seq Neuron

lnnify :: [a] -> Seq.Seq a
lnnify = Seq.fromList

unlnnify :: Foldable t => t a -> [a]
unlnnify = toList

lnnMap :: Foldable t => (a1 -> a2) -> t a1 -> Seq.Seq a2
lnnMap f = lnnify . map f . toList

lnnMap' :: Foldable t => (a -> b) -> t a -> [b]
lnnMap' f = map f . toList

lnnZip :: Seq.Seq a -> Seq.Seq b -> Seq.Seq (a, b)
lnnZip = Seq.zip

accessNeuron :: Int -> Seq.Seq a -> Maybe a
accessNeuron = Seq.lookup

accessNeurons :: Seq.Seq b -> [Int] -> [b]
accessNeurons m = mapMaybe (`accessNeuron` m)

tighten :: (Ord a, Ord b) => (a, b) -> (a, b) -> (a, b)
tighten (l, u) (l', u') = (max l l', min u u')

upward :: Net -> Neuron -> Neuron
upward _ (V s l u) = V s l u
upward ns (N s i l u) = N s i tl tu
  where
    (tl, tu) =
      case fmap bounds . accessNeuron i $ ns of
        Just (l', u') -> tighten (l, u) (negation u', negation l')
        _ -> error "unknown error."
upward ns (A s xs l u) = A s xs tl tu
  where
    (tl, tu) = tighten (l, u) (l', u')
    (l', u') = invertBounds $ foldr (sumBounds . invertBounds . bounds) (bot, bot) xs'
    xs' = accessNeurons ns xs
upward ns (O s xs l u) = O s xs tl tu
  where
    (tl, tu) = tighten (l, u) (l', u')
    (l', u') = clampBounds $ foldr (sumBounds . bounds) (bot, bot) xs'
    xs' = accessNeurons ns xs
upward ns (I s x y l u) = I s x y tl tu
  where
    (tl, tu) = tighten (l, u) (l', u')
    (l', u') = clampBounds (xu + yl, xl + yu)
    (xl, xu) = clampBounds . invertBounds . head $ bs
    (yl, yu) = last bs
    bs = map bounds . accessNeurons ns $ [x, y]

downward :: Net -> Neuron -> [Neuron]
downward _ (V {}) = []
downward ns (N _ x l u) = [updateBounds (tl, tu) n]
  where
    n = fromMaybe (error "") $ accessNeuron x ns
    (tl, tu) = tighten (negation u, negation l) . bounds $ n
downward ns (A _ xs l u) =
  map (\(i, (b, n)) -> updateBounds (tighten b (tl i, tu i)) n) xs'
  where
    xs' = zip [0 ..] $ zip bs ns'
    tl j =
      if l > bot
        then residuum (tNorm . map snd . remove j $ bs) l
        else bot
    tu j =
      if u < top
        then residuum (tNorm . map fst . remove j $ bs) u
        else top
    bs = map bounds ns'
    ns' = accessNeurons ns xs
downward ns (O _ xs l u) =
  map (\(i, (b, n)) -> updateBounds (tighten b (tl i, tu i)) n) xs'
  where
    xs' = zip [0 ..] $ zip bs ns'
    tl j =
      if l > bot
        then tNorm [tNorm . map (negation . snd) . remove j $ bs, l]
        else bot
    tu j =
      if u < top
        then tNorm [tNorm . map (negation . fst) . remove j $ bs, u]
        else top
    bs = map bounds ns'
    ns' = accessNeurons ns xs
downward ns (I _ x y l u) =
  [ updateBounds (tighten (lx, ux) (lx', ux')) nx,
    updateBounds (tighten (ly, uy) (ly', uy')) ny
  ]
  where
    lx' = if u < top then residuum u ly else bot
    ux' = if l > bot then residuum l uy else top
    ly' = if l > bot then tNorm [lx, l] else bot
    uy' = if u < top then tNorm [ux, u] else top
    (lx, ux) = bounds nx
    (ly, uy) = bounds ny
    nx = fromMaybe (error "") $ accessNeuron x ns
    ny = fromMaybe (error "") $ accessNeuron y ns

upd :: Seq.Seq a -> [(Int, a)] -> Seq.Seq a
upd s [] = s
upd s ((i, x) : xs) = upd (Seq.update i x s) xs

up :: [Neuron] -> Maybe (Seq.Seq Neuron) -> Seq.Seq Neuron
up [] (Just ns) = ns
up [x] (Just ns) = ns Seq.|> upward ns x
up (x : xs) (Just ns) = up xs $ Just (ns Seq.|> upward ns x)
up xs Nothing = up xs $ Just Seq.Empty

down :: Int -> Net -> Net
down 0 ns = ns
down i ns = down (i - 1) ns'
  where
    ns' = aux (Seq.index ns i) ns
    aux (V {}) lnn = lnn
    aux (N s x l u) lnn = upd lnn [(x, head (downward lnn (N s x l u)))]
    aux (A s xs l u) lnn = upd lnn $ zip xs (downward lnn (A s xs l u))
    aux (O s xs l u) lnn = upd lnn $ zip xs (downward lnn (O s xs l u))
    aux (I s x y l u) lnn = upd lnn $ zip [x, y] (downward lnn (I s x y l u))

upwardPass :: Foldable t => t Neuron -> Seq.Seq Neuron
upwardPass ns = up (unlnnify ns) Nothing

downwardPass :: Seq.Seq Neuron -> Net
downwardPass ns = down (length ns - 1) ns

epsilon :: Value
epsilon = 0.0001

lnnCmp :: Foldable t => t Neuron -> t Neuron -> Value
lnnCmp a b = sum $ zipWith (curry f) (g a) (g b)
  where
    g = map bounds . toList
    f ((l, u), (l', u')) = abs (l - l') + abs (u - u')

approximate :: Int -> Net -> Net
approximate i ns = down i ns'
  where
    ns' = up (unlnnify ns) Nothing

infer :: Int -> Net -> Net
infer i ns = if lnnCmp ns ns' <= epsilon then ns else infer i ns'
  where
    ns' = approximate i ns

inferDbg _ [] = []
inferDbg i [x] = if lnnCmp x x' <= epsilon then [x] else inferDbg i [x, x']
  where
    x' = approximate i x
inferDbg i xs = if lnnCmp x x' <= epsilon then xs else inferDbg i (xs ++ [x])
  where
    x = last xs
    x' = approximate i x

-- boundsPredicate i (V s l u) = "bounds(lnn" ++ show i ++ "," ++ "\"" ++ s ++ "\"," ++ show (10 * l) ++ "," ++ "\"" ++ show (10 * u) ++ "\") "
boundsPredicate i (V s l u) = "bounds(lnn" ++ show i ++ "," ++ s ++ "," ++ show (floor $ 10 * l) ++ "," ++ show (floor $ 10 * u) ++ ")"

bn0 =
  [ V "a" 0.2 0.7, -- 0
    V "b" 0.3 0.6, -- 1
    V "c" 0.0 1.0, -- 2
    V "d" 0.0 1.0, -- 3
    N "-a" 0 0.0 1.0, -- 4
    N "-b" 1 0.0 1.0, -- 5
    N "-c" 2 0.0 1.0, -- 6
    N "-d" 3 0.0 1.0, -- 7
    I "rhs a" 5 0 0.0 1.0, -- 8
    I "lhs a" 0 5 0.0 1.0, -- 9
    A "proof a" [8, 9] 0.0 1.0, -- 10
    I "rhs b" 4 1 0.0 1.0, -- 11
    I "lhs b" 1 4 0.0 1.0, -- 12
    A "proof b" [11, 12] 0.0 1.0, -- 13
    A "(AND -d b)" [7, 1] 0.0 1.0, -- 14
    I "rhs c" 14 2 0.0 1.0, -- 15
    I "lhs c" 2 14 0.0 1.0, -- 16
    A "proof c" [15, 16] 0.0 1.0, -- 17
    A "(AND -c b)" [6, 1] 0.0 1.0, -- 18
    I "rhs d" 18 3 0.0 1.0, -- 19
    I "lhs d" 3 18 0.0 1.0, -- 20
    A "proof d" [19, 20] 0.0 1.0, -- 21
    A "root" [10, 13, 17, 21] 0.0 1.0
  ]
