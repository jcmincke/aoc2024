{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module P01.Main02 (main) where


import Data.List as L
--import Data.Maybe as M
--import Data.Set as S
--import Debug.Trace (trace)
--import Data.Sort (sortOn)
import System.IO
import Text.Regex.Posix


pat = "([0-9]+)[ ]+([0-9]+)"



parse st =
  let (_, _, _, [n1, n2]) = st =~ pat :: (String, String, String, [String])
  in (read n1, read n2)


read_data :: IO [(Int, Int)]
read_data = do
  inh <- openFile "data" ReadMode
  loop inh []
  where
  loop inh acc  = do
    ineof <- hIsEOF inh
    if ineof
      then return $ L.reverse acc
      else do st::String <- hGetLine inh
              let ins = parse st
              loop inh (ins:acc)


sim l1 l2 = 

  do  e <- l1
      let fl2 = L.filter (\x -> x==e) l2
      let s = e * L.length fl2
      return s


main :: IO()
main = do
  l <- read_data
  let (l1, l2) = L.unzip l
  let r = sim l1 l2
  print (sum r)


