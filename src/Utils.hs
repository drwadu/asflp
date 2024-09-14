module Utils where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import qualified Data.Sequence as Seq
import Fuzzy (Eval)

remove :: Int -> [a] -> [a]
remove _ [] = []
remove 0 (_ : xs) = xs
remove n (x : xs) = x : remove (n - 1) xs

setr :: [Int] -> [a] -> Seq.Seq a -> Seq.Seq a
setr [] [] m = m
setr [j] [x] m = Seq.update j x m
setr (j : js) (x : xs) m = setr js xs m'
  where
    m' = setr [j] [x] m

split :: (Eq a) => [a] -> [a] -> [[a]]
split sub str = split' sub str [] []
  where
    split' _ [] subacc acc = reverse (reverse subacc : acc)
    split' sub str subacc acc
      | sub `isPrefixOf` str = split' sub (drop (length sub) str) [] (reverse subacc : acc)
      | otherwise = split' sub (tail str) (head str : subacc) acc

rm :: String -> String
rm = unlines . map (dropWhile isSpace) . lines

rm' :: [Char] -> [Char]
rm' = reverse . dropWhile isSpace . reverse

truncate' :: (Fractional a, RealFrac a) => a -> Int -> a
truncate' x n = fromIntegral (floor (x * t)) / t
  where
    t = 10 ^ n

round' :: (Fractional a, RealFrac a) => a -> a
round' x = truncate' x 3
