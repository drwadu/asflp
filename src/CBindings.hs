{-# LANGUAGE ForeignFunctionInterface #-}

module CBindings where

import Lib
  ( solve,
    parseBounds,
    Neuron (..),
    showw,
    Lnn (..)
  )

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Text.Printf (printf)

import Foreign.C (peekCString, CString, newCString)

forward :: CString -> IO CString
forward flp = do
  s <- peekCString flp
  lnn <- solve s (readInputs s)
  newCString . concatMap (displayRaw lnn) $ init . toList $ (ast lnn)

readInputs flp = Map.fromList . map parseBounds $ filter (\x -> '[' `elem` x) $ filter (\x -> length x > 1) $ lines flp

displayRaw lnn (Atom s l u) = s ++ "[" ++ printf "%.2f" l ++ ";" ++ printf "%.2f" u ++ "]~" 
displayRaw lnn n@(Not _ l u) = showw lnn n ++ "[" ++ printf "%.2f" l ++ ";" ++ printf "%.2f" u ++ "]~"
displayRaw lnn n@(And _ l u) = showw lnn n ++ "[" ++ printf "%.2f" l ++ ";" ++ printf "%.2f" u ++ "]~" 
displayRaw _ _ = ""


foreign export ccall forward :: CString -> IO CString

