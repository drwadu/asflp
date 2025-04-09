module Ico.Solver (solveIco)
where

import Lnn.Logic (
    Eval (..),
    Logic (..),
    bot,
    negation,
    negation',
    tCoNorm',
    tNorm',
    top,
 )

import Data.Bifunctor (bimap, first)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Ico.Parser (parse, parseBounds)
import Text.Printf (printf)

data Expression
    = Atom String
    | Not String
    | And [Expression]
    deriving (Show, Eq)

logic :: Logic
logic = Lukasiewicz

-- | returns bounds of atom
i >* a@(Atom x) = fromMaybe (error (x ++ " bounds not found")) $ lookup a i
-- \| returns flipped negation of atom (approximator)
i >* (Not x) = (negation logic u, negation logic l)
  where
    (l, u) = i >* Atom x

-- \| aggregates rule body based on a given t-norm
i >* (And [x]) = i >* x
i >* (And xs) = foldr (tNorm' logic) (top, top) xs'
  where
    xs' = map (i >*) xs

approxIco i supports a =
    case lookup a supports of
        Just bs -> (maximum $ map (fst . (i >*)) bs, maximum $ map (snd . (i >*)) bs)
        _ -> i >* a

-- \^^ atom occurs in no rule head
-- do
--   case lookup a supports of
--     Just [] -> return $ i >* a -- ^^ atom occurs in no rule head
--     Just bs -> do
--       let (l,u) = (maximum $ map (fst . (i >*)) bs, maximum $ map (snd . (i >*)) bs)
--       print (a,bs,(l,u))
--       return (l,u)
--     _ -> error ""

readInputs flp = Map.fromList . map parseBounds $ filter (\x -> '[' `elem` x) $ filter (\x -> length x > 1) $ lines flp

iCmp a b = sum $ zipWith f a b
  where
    f (l, u) (l', u') = abs (l - l') + abs (u - u')

-- infer :: _
infer i supports atoms = do
    let curr = zip atoms (map (approxIco i supports) atoms)
    print i
    print curr
    putStrLn ""
    let (a, b) = (map snd curr, map snd i)
    -- let delta = sum $ zipWith (\((l,u),(l',u')) -> abs (l - l') + abs (u - u')) (map snd curr) (map snd i)
    let delta = iCmp a b
    if delta <= epsilon
        then do
            -- print (show delta ++ " " ++ show a ++ " --- " ++ show b)
            -- putStrLn "++++++ DONE "
            mapM_ (putStr . display) curr
            return curr
        else do
            -- print (show delta ++ " " ++ show a ++ " --- " ++ show b)
            -- mapM_ (putStr . display) curr
            -- print "/////////"
            -- print curr
            infer curr supports atoms
  where
    -- undefined

    epsilon = 0.0001

solveIco flp = do
    -- print supports
    -- print bodies
    print atoms
    -- print fullInputs
    print fullInputs
    putStrLn ""
    -- let x =  zip atoms (map (approxIco fullInputs supports) atoms)
    -- let x = infer fullInputs supports atoms
    -- mapM_ (putStr . display) x
    _ <- infer fullInputs supports atoms
    return ()
  where
    supports = map (bimap Atom (map (And . map litify))) $ sort $ Map.toList $ parse Map.empty flp
    fullInputs = fillUp fullInputs' atoms
    atoms = map Atom $ Set.toList $ Set.fromList $ sort $ map (\(Atom x) -> x) bodies
    bodies = map fst supports ++ (concatMap (\(And xs) -> map atomify xs) $ concatMap snd supports)
    atomify (Not x) = Atom x
    atomify (Atom x) = Atom x
    atomify _ = undefined
    fullInputs' = fillUp inputs atoms
    inputs = map (first Atom) $ Map.toList $ readInputs flp
    litify l = if head l == '-' then Not $ tail l else Atom l
    fillUp i [] = i
    fillUp i [x] =
        case lookup x i of
            Just _ -> i
            _ -> i <> [(x, (bot, top))]
    fillUp i (x : xs) = fillUp (fillUp i [x]) xs

display (Atom a, (l, u))
    | (l <= negation logic 0.5) && (u >= 0.5) =
        uc ++ " [" ++ printf "%.2f" l ++ ";" ++ printf "%.2f" u ++ "]" ++ " " ++ a ++ "\n"
    | (l >= 0.5) && (u >= 0.5) =
        tc ++ " [" ++ printf "%.2f" l ++ ";" ++ printf "%.2f" u ++ "]" ++ " " ++ a ++ "\n"
    | otherwise =
        fc ++ " [" ++ printf "%.2f" l ++ ";" ++ printf "%.2f" u ++ "]" ++ " " ++ a ++ "\n"
  where
    tc = "\x1b[0;30;42m[T]\x1b[0m"
    fc = "\x1b[0;30;41m[F]\x1b[0m"
    uc = "\x1b[0;30;44m[U]\x1b[0m"
display _ = "bla"
