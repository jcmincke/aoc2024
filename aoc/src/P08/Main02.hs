{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module P08.Main02 (main) where

import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.HashSet as S
import qualified Data.Map.Strict as M
import System.IO
import qualified Data.Array as A
import Control.Monad (guard)
import Data.Hashable (Hashable)





read_data  = do
  inh <- openFile "./src/P08/data_1" ReadMode
  loop inh []
  where
  loop inh acc  = do
    ineof <- hIsEOF inh
    if ineof
      then return $ (L.reverse acc)
      else do st::String <- hGetLine inh
              loop inh (st:acc)


find_antennas a = 
  L.foldl' go M.empty $ A.assocs a
  where
  go acc (pos, '.') = acc
  go acc ((y, x), c) = M.insertWith (\ov nv -> nv++ov) c [(x,y)] acc


find_anti_nodes_pair :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
find_anti_nodes_pair (x0, y0) (x1, y1) =
  pts
  where
  dx = x1 - x0
  dy = y1 - y0
  pts = L.map (\i -> (x0+i*dx, y0+i*dy)) [-60 .. 60]

in_box max_size (x, y) = (1<=x) && (x<=max_size) && (1<=y) && (y<=max_size)


find_all_anti_nodes_for_freq :: [(Int, Int)] -> [[(Int, Int)]]
find_all_anti_nodes_for_freq poss =
  map (\(p1, p2) -> find_anti_nodes_pair p1 p2) pos_pairs
  where
  pos_pairs = do
    p1 <- poss
    p2 <- poss
    guard $ p1 /= p2
    return (p1, p2) 

find_all_anti_nodes :: M.Map k [(Int, Int)] -> S.HashSet (Int, Int)
find_all_anti_nodes antennas_map = 
  S.fromList $ L.concat $ M.foldl go [] antennas_map
  where 
  go acc poss = find_all_anti_nodes_for_freq poss ++ acc


main :: IO()
main = do
  sts@(h:_) <- read_data
  mapM_ putStrLn sts
  let max_size = L.length sts
  print (max_size, L.length h)
  let a = A.listArray ((1, 1), (max_size, max_size)) $ L.concat sts
  let antennas_map = find_antennas a
  print antennas_map
  let nodes = find_all_anti_nodes antennas_map
  let nodes' = S.filter (\p -> in_box max_size p) nodes
  print nodes'
  print $ S.size nodes'
  return ()











 
