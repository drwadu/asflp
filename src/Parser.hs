module Parser
  ( parse,
    Sym,
    parseBounds',
    parseBodyBounds',
  )
where

import Data.List (sort)
import qualified Data.Map as Map
import Utils (rm, rm', split)

type Sym = String

arrow :: String
arrow = ":-"

conjunction :: String
conjunction = ","

comment :: Char
comment = '%'

parseBounds' :: [Char] -> ([Char], (Double, Double))
parseBounds' x = (a, (read (last $ split "[" (head xs)) :: Double, read (init $ head (tail xs)) :: Double))
  where
    a = head $ split "[" x
    xs = split ";" x

parseBodyBounds' :: [Char] -> ([Char], (Double, Double))
parseBodyBounds' x = (b, (read (head xs) :: Double, read (last xs) :: Double))
  where
    xs = split ";" . last . split ":-[" . head . split "]" $ x
    b = (rm' . rm . head . split ":-" $ x) ++ " (AND " ++ (unwords . sort . map (rm' . rm) . split conjunction . rm' . rm . last $ split "]" x) ++ ")"

-- parseBodyBounds' x = (b, (read (head xs) :: Double, read (last xs) :: Double))
--  where
--    xs = split ";" . last . split ":-[" . head . split "]" $ x
--    b = "(AND " ++ (unwords . map (rm' . rm) . split conjunction . rm' . rm . last $ split "]" x) ++ ")"

headBody :: Map.Map [Char] [[[Char]]] -> String -> Map.Map [Char] [[[Char]]]
headBody m x = Map.insert h (b' ++ [b]) m
  where
    b = map (rm' . rm) . split conjunction . rm' . rm . last . split "]" . last $ s
    b' = maybe [] sort $ Map.lookup h m
    h = head s
    s = map (rm' . rm) . split arrow . rm' . rm $ x

parse :: Map.Map [Char] [[[Char]]] -> String -> Map.Map [Char] [[[Char]]]
parse m = Map.unionsWith (++) . map (headBody m) . filter (\l -> (head l /= comment) && (':' `elem` l)) . filter (\l -> length l > 1) . lines
