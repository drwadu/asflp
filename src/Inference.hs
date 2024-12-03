module Inference
  ( Pass (..),
    upwardPass,
    downwardPass,
    infer,
    inferDebug,
    display,
    lnnCmp,
  )
where

import Data.Foldable (toList)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Sequence as Seq
import Fuzzy
  ( Eval (..),
    Logic (..),
    Value,
    bot,
    negation',
    tCoNorm',
    tNorm',
    top,
  )
import Neuron
  ( Neuron (..),
    update,
  )
import Text.Printf
import Utils (remove, round')


logic :: Logic
logic = Lukasiewicz

class Pass node where
  upward :: Seq.Seq node -> node -> node
  downward :: Seq.Seq node -> node -> [node]

instance Pass Neuron where
  upward lnn neuron = case neuron of
    N {_s = s, _x = x, _l = l, _u = u} -> N {_s = s, _x = x, _l = tl, _u = tu}
      where
        (tl, tu) = case fmap bounds . Seq.lookup x $ lnn of
          Just (l', u') -> aggregate (l, u) (negation logic u', negation logic l')
          _ -> undefined
    A {_s = s, _xs = xs, _l = l, _u = u} -> A {_s = s, _xs = xs, _l = tl, _u = tu}
      where
        (tl, tu) = aggregate (l, u) (l', u')
        (l', u') = negation' logic $ foldr (tCoNorm' logic . negation' logic . bounds) (bot, bot) (access xs)
    O {_s = s, _xs = xs, _l = l, _u = u} -> O {_s = s, _xs = xs, _l = tl, _u = tu}
      where
        (tl, tu) = aggregate (l, u) (l', u')
        (l', u') = foldr (tCoNorm' logic . bounds) (bot, bot) (access xs)
    I {_s = s, _x = x, _y = y, _l = l, _u = u} -> I {_s = s, _x = x, _y = y, _l = tl, _u = tu}
      where
        (tl, tu) = aggregate (l, u) (l', u')
        (l', u') = tCoNorm' logic (xu, xl) (yl, yu)
        (xl, xu) = negation' logic . head $ bs
        (yl, yu) = last bs
        bs = map bounds . access $ [x, y]
    _ -> neuron
    where
      aggregate (l, u) (l', u') = (max l l', min u u')
      bounds n = (_l n, _u n)
      access = mapMaybe (`Seq.lookup` lnn)

  downward lnn neuron = case neuron of
    N {_s = _, _x = x, _l = l, _u = u} -> [update x' tl tu]
      where
        (tl, tu) = aggregate (negation logic u, negation logic l) . bounds $ x'
        x' = case Seq.lookup x lnn of
          Just n -> n
          _ -> undefined
    A {_s = _, _xs = xs, _l = l, _u = u} ->
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
        xs' = access xs
    O {_s = _, _xs = xs, _l = l, _u = u} ->
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
        xs' = access xs
    I {_s = _, _x = x, _y = y, _l = l, _u = u} ->
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
        nx = fromMaybe (error "") $ Seq.lookup x lnn
        ny = fromMaybe (error "") $ Seq.lookup y lnn
    _ -> []
    where
      aggregate (l, u) (l', u') = (max l l', min u u')
      bounds n = (_l n, _u n)
      access = mapMaybe (`Seq.lookup` lnn)

upwardPass :: (Pass a, Foldable t) => t a -> Seq.Seq a
upwardPass lnn = aux (toList lnn) Nothing
  where
    aux [] (Just ns) = ns
    aux [x] (Just ns) = ns Seq.|> upward ns x
    aux (x : xs) (Just ns) = aux xs $ Just (ns Seq.|> upward ns x)
    aux xs Nothing = aux xs $ Just Seq.Empty

downwardPass 0 lnn = lnn
downwardPass i lnn = downwardPass (i - 1) lnn'
  where
    lnn' = aux (Seq.index lnn i) lnn
    aux (V {}) ns = ns
    aux (N s x l u) ns = upd ns [(x, head (downward ns (N s x l u)))]
    aux (I s x y l u) ns = upd ns $ zip [x, y] (downward ns (I s x y l u))
    aux n ns = upd ns $ zip (_xs n) (downward ns n)
    upd s [] = s
    upd s ((j, x) : xs) = upd (Seq.update j x s) xs

approximate i lnn = downwardPass i lnn'
  where
    lnn' = upwardPass lnn

approximateDebug i lnn = do
  downwardPass i lnn'
  where
    lnn' = upwardPass lnn

lnnCmp a b = sum $ zipWith (curry f) (g a) (g b)
  where
    g = map bounds . toList
    f ((l, u), (l', u')) = abs (l - l') + abs (u - u')
    bounds n = (_l n, _u n)

infer i lnn = if lnnCmp lnn lnn' <= epsilon then lnn else infer i lnn'
  where
    lnn' = approximate i lnn
    epsilon = 0.0001

inferDebug i lnn = do
  -- mapM_ print $ filter isVar $ toList lnn'
  -- mapM_ print $ toList lnn'
  -- mapM_ (putStr . display) $ toList lnn'
  -- putStrLn ""
  if lnnCmp lnn lnn' <= epsilon
    then return lnn
    else inferDebug i lnn'
  where
    lnn' = approximate i lnn
    epsilon = 0.0001

tc :: String
tc = "\x1b[0;30;42m[T]\x1b[0m"

fc :: String
fc = "\x1b[0;30;41m[F]\x1b[0m"

uc :: String
uc = "\x1b[0;30;44m[U]\x1b[0m"

display (V a l u)
  | (l <= negation Lukasiewicz 0.5) && (u >= 0.5) =
      uc ++ " [" ++ printf "%.2f" l ++ ";" ++ printf "%.2f" u ++ "]" ++ " " ++ a ++ "\n"
  | (l >= 0.5) && (u >= 0.5) =
      tc ++ " [" ++ printf "%.2f" l ++ ";" ++ printf "%.2f" u ++ "]" ++ " " ++ a ++ "\n"
  | otherwise =
      fc ++ " [" ++ printf "%.2f" l ++ ";" ++ printf "%.2f" u ++ "]" ++ " " ++ a ++ "\n"
display _ = ""
