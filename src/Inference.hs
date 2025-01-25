module Inference
  ( Pass (..),
    upwardPass_,
    downwardPass_,
    infer,
    inferDebug,
    display,
    displayRaw,
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
  ( Neuron_ (..),
    update_,
  )
import Text.Printf
import Utils (remove, round')


logic :: Logic
logic = Lukasiewicz

class Pass a where
  upward :: Seq.Seq a -> a -> a
  downward :: Seq.Seq a -> a -> [a]

instance Pass Neuron_ where
  upward lnn neuron = case neuron of
    N {_s = s, _x = x, _l = l, _u = u} -> N {_s = s, _x = x, _l = tl, _u = tu}
      where
        (tl, tu) = case fmap bounds . Seq.lookup x $ lnn of
          Just (l', u') -> aggregate (l, u) (negation logic u', negation logic l')
          _ -> undefined
    A {_s = s, _xs = xs, _l = l, _u = u, _ws = ws} -> A {_s = s, _xs = xs, _l = tl, _u = tu, _ws = ws}
      where
        (tl, tu) = aggregate (l, u) (l', u')
        (l', u') = negation' logic $ foldr (tCoNorm' logic . (\(w,(l,u)) -> (w * negation logic l, w * negation logic u))) (bot, bot) (zip ws (map bounds . access $ xs))
        --(l', u') = negation' logic $ foldr (tCoNorm' logic . negation' logic . bounds) (bot, bot) (access xs)
    O {_s = s, _xs = xs, _l = l, _u = u, _ws = ws} -> O {_s = s, _xs = xs, _l = tl, _u = tu, _ws = ws}
      where
        (tl, tu) = aggregate (l, u) (l', u')
        (l', u') = foldr (tCoNorm' logic . (\(w,(l,u)) -> (w*l, w*u))) (bot, bot) (zip ws (map bounds . access $ xs))
        --(l', u') = foldr (tCoNorm' logic . bounds) (bot, bot) (access xs)
    I {_s = s, _x = x, _y = y, _l = l, _u = u, _ws = ws} -> I {_s = s, _x = x, _y = y, _l = tl, _u = tu, _ws = ws}
      where
        (tl, tu) = aggregate (l, u) (l', u')
        (l', u') = tCoNorm' logic (head ws * negation logic xu, head ws * negation logic xl) (last ws * yl, last ws * yu)
        (xl, xu) = head bs
        --(l', u') = tCoNorm' logic (xu, xl) (yl, yu)
        --(xl, xu) = negation' logic . head $ bs
        (yl, yu) = last bs
        bs = map bounds . access $ [x, y]
    _ -> neuron
    where
      aggregate (l, u) (l', u') = (max l l', min u u')
      bounds n = (_l n, _u n)
      boundsW n = (_l n, _u n)
      access = mapMaybe (`Seq.lookup` lnn)

  downward lnn neuron = case neuron of
    N {_s = _, _x = x, _l = l, _u = u} -> [update_ x' tl tu]
      where
        (tl, tu) = aggregate (negation logic u, negation logic l) . bounds $ x'
        x' = case Seq.lookup x lnn of
          Just n -> n
          _ -> undefined
    A {_s = _, _xs = xs, _l = l, _u = u, _ws = ws} ->
      zipWith (curry (\(i, (b, n)) -> uncurry (update_ n) (aggregate b (tl i, tu i)))) [0 ..] (zip bs xs')
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
    O {_s = _, _xs = xs, _l = l, _u = u, _ws = ws} ->
      zipWith (curry (\(i, (b, n)) -> uncurry (update_ n) (aggregate b (tl i, tu i)))) [0 ..] (zip bs xs')
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
    I {_s = _, _x = x, _y = y, _l = l, _u = u, _ws = ws} ->
      [ uncurry (update_ nx) (aggregate (lx, ux) (lx', ux')),
        uncurry (update_ ny) (aggregate (ly, uy) (ly', uy'))
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

upwardPass_ :: (Pass a, Foldable t) => t a -> Seq.Seq a
upwardPass_ lnn = aux (toList lnn) Nothing
  where
    aux [] (Just ns) = ns
    aux [x] (Just ns) = ns Seq.|> upward ns x
    aux (x : xs) (Just ns) = aux xs $ Just (ns Seq.|> upward ns x)
    aux xs Nothing = aux xs $ Just Seq.Empty

downwardPass_ 0 lnn = lnn
downwardPass_ i lnn = downwardPass_ (i - 1) lnn'
  where
    lnn' = aux (Seq.index lnn i) lnn
    aux (V {}) ns = ns
    aux (N s x l u) ns = upd ns [(x, head (downward ns (N s x l u)))]
    aux (I s x y l u ws) ns = upd ns $ zip [x, y] (downward ns (I s x y l u ws))
    aux n ns = upd ns $ zip (_xs n) (downward ns n)
    upd s [] = s
    upd s ((j, x) : xs) = upd (Seq.update j x s) xs

approximate i lnn = downwardPass_ i lnn'
  where
    lnn' = upwardPass_ lnn

approximateDebug i lnn = do
  downwardPass_ i lnn'
  where
    lnn' = upwardPass_ lnn

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
  -- mapM_ (putStr . display) $ toList lnn'
  -- putStrLn ""
  if lnnCmp lnn lnn' <= epsilon
    then return lnn
    else 
    do
    mapM_ (\n -> print $ show ((_l n, _u n), _s n)) $ toList lnn'
    putStrLn "///"
    inferDebug i lnn'
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

displayRaw (V a l u) = a ++ "[" ++ show l ++ ";" ++ show u ++ "]~" 
displayRaw (N a _ l u) = a ++ "[" ++ show l ++ ";" ++ show u ++ "]~"
displayRaw (A a _ l u ws) = a ++ "[" ++ show l ++ ";" ++ show u ++ "]~" 
displayRaw (O a _ l u ws) = a ++ "[" ++ show l ++ ";" ++ show u ++ "]~"
displayRaw _ = ""

