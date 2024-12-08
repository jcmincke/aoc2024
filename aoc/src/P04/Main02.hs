{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module P04.Main02 (main) where

import Text.ParserCombinators.Parsec as P
import Text.Parsec.Char (char, string, anyChar)
--import Text.Parsec.Char (symbol)
--import qualified Text.ParserCombinators.Parsec.Token as P
--import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Token (symbol)
import Control.Monad (guard)
import Data.List as L
import Data.Maybe as M
--import Data.Set as S
--import Debug.Trace (trace)
--import Data.Sort (sortOn)
import System.IO
import Text.Regex.Posix
import qualified Data.Array as A



--read_data :: IO [[Int]]
read_data = do
  inh <- openFile "./src/P04/data1" ReadMode
  loop inh []
  where
  loop inh acc  = do
    ineof <- hIsEOF inh
    if ineof
      then return $ L.foldl' (++) "" (L.reverse acc)
      else do st::String <- hGetLine inh
              loop inh (st:acc)


n::Int = 140


gen_indices_at_in_dir (x, y) (dx, dy) = 
  --path'
  if length path' >= 4
  then Just(L.take 4 path')
  else Nothing
  where
  path = L.reverse (L.foldl' (\acc i -> (x+i*dx, y+i*dy):acc) [] [0..n+1])
  path' = L.takeWhile (\(x, y) -> (x>=1) && (x<=n) && (y>=1) && (y<=n)) path

gen_indices_at (x, y)  = 
  []
  where
  indices_1 = [(x-1, y-1), (x, y), (x+1, y+1)] 
  indices_2 = [(x-1, y+1), (x, y), (x+1, y-1)] 
  dxys = do 
    dx <- [-1, 0, 1]
    dy <- [-1, 0, 1]
    guard ((dx /= 0) || (dy /=0))
    return (dx, dy)

gen_indices = 
  M.catMaybes paths
  where
  paths = L.concat $ map gen_indices_at xys 
  xys = do 
    x <- [1..n]
    y <- [1..n]
    return (x, y)


check_xmas a (x,y) =
  ((word_1 == "MAS") || (word_1 == "SAM"))&& ((word_2 == "MAS") || (word_2 == "SAM"))
  where
  indices_1 = [(x-1, y-1), (x, y), (x+1, y+1)] 
  indices_2 = [(x-1, y+1), (x, y), (x+1, y-1)] 

  word_1 = map (\xy -> a A.! xy) indices_1
  word_2 = map (\xy -> a A.! xy) indices_2

check_xmas_all a = 
  r
  where
    r = map (check_xmas a) xys
    xys = do 
      x <- [2..n-1]
      y <- [2..n-1]
      return (x, y)

main :: IO()
main = do
  st <- read_data
  let a = A.listArray ((1, 1), (n, n)) st
  print st
  --print $ a A.! (11::Int, 11::Int)
  --print a
  print [0..n+1]
  --print (gen_indices_at (1, 1))

  let paths = gen_indices
  let checks = L.filter id (check_xmas_all a)
  print $ length checks
  return ()
  --mapM_ print l

