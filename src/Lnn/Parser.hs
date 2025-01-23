module Lnn.Parser
  ( parse
  , parseBounds
  )
where

import Data.List (sort)
import qualified Data.Map as Map
import Utils (rm, rm', split)


arrow :: String
arrow = ":-"


conjunction :: String
conjunction = ","


defaultNegation :: String
defaultNegation = "-"


comment :: Char
comment = '%'


boundsOpen :: String
boundsOpen = "["


boundsClose :: String
boundsClose = "]"


boundsDelimiter :: String
boundsDelimiter = ";"


parse :: Map.Map [Char] [[[Char]]] -> String -> Map.Map [Char] [[[Char]]]
parse support = Map.unionsWith (++) . map (headBody support) . filter normalRules . lines
 where
  normalRules line = (head line /= comment) && (':' `elem` line) && (length line > 1)


parseBounds :: [Char] -> ([Char], (Double, Double))
parseBounds rule = (headAtom, (lb, ub))
 where
  headAtom = head $ split boundsOpen rule
  ub = read . init . head . tail $ parts
  lb = read . last . split boundsOpen . head $ parts
  parts = split boundsDelimiter rule


headBody :: Map.Map [Char] [[[Char]]] -> String -> Map.Map [Char] [[[Char]]]
headBody supports rule = Map.insert headAtom (support ++ [additionalSupport]) supports
 where
  additionalSupport = map strip . split conjunction . strip . last . split boundsClose . last $ parts
  support = maybe [] sort $ Map.lookup headAtom supports
  headAtom = head parts
  parts = map strip . split arrow . strip $ rule
  strip = rm' . rm
