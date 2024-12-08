{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module P06.Main01 (main) where

import qualified Text.ParserCombinators.Parsec as P
import qualified Data.List as L
import Data.Maybe as M
import qualified Data.HashSet as S
import qualified Data.Map.Strict as M
import System.IO
import qualified Data.Array as A






read_data  = do
  inh <- openFile "./src/P06/data_1" ReadMode
  loop 1 Nothing inh []

  where
  decode x y d '#' = ('#', d)
  decode x y d '.' = ('.', d)
  decode x y d 'v' = ('.', Just ((x, y),(0, 1)))
  decode x y d '^' = ('.', Just ((x, y),(0, -1)))
  decode x y d '>' = ('.', Just ((x, y),(1, 0)))
  decode x y d '<' = ('.', Just ((x, y),(-1, 0)))

  loop y d inh acc  = do
    ineof <- hIsEOF inh
    if ineof
      then return $ (L.reverse acc, y-1, d)
      else do st::String <- hGetLine inh
              let (nbs, nd) = L.foldl' (\(m, pd) (c, x) -> let (v, d) = decode x y pd c 
                                                  in (v:m, d)) 
                              ([], d) $ L.zip st [1..]
              loop (y+1) nd inh ((L.reverse nbs):acc)


show_map a (xc, yc) = 
  L.reverse $ L.foldl' (\acc y -> (line y):acc) [] [y0 .. y1]
  where
  ((x0, y0), (x1, y1)) = A.bounds a
  line y = map (\x -> if (x==xc) && (y==yc) then 'O' else (a A.! (y, x))) [x0 .. x1] 


walk :: A.Array (Int, Int) Char -> [(Int, Int)] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
walk a acc (xc, yc) (dx, dy) = 
  if is_inside (nx, ny)
  then  
    case a A.! (ny, nx) of
    --'.' -> walk a (S.insert (ny, nx) acc) (nx, ny) (dx, dy)
    '.' -> walk a ((nx, ny): acc) (nx, ny) (dx, dy)
    '#' -> walk a acc (xc, yc) (turn_right (dx, dy))
  else acc
  where

  (nx, ny) = (xc+dx, yc+dy)
  ((x0, y0), (x1, y1)) = A.bounds a
  is_inside (xc, yc) = (x0 <= xc) && (xc <= x1) && (y0 <= yc) && (yc <= y1)
  turn_right (0, 1) = (-1, 0)
  turn_right (0, -1) = (1, 0)
  turn_right (1, 0) = (0, 1)
  turn_right (-1, 0) = (0, -1)



walk_1 a count (xc, yc) (dx, dy) = 
  if is_inside (nx, ny)
  then  
    case a A.! (ny, nx) of
    '.' -> Just $ ((count+1), (nx, ny), (dx, dy))
    '#' -> Just $ (count, (xc, yc), (turn_right (dx, dy)))
  else Nothing
  where
  (nx, ny) = (xc+dx, yc+dy)
  ((x0, y0), (x1, y1)) = A.bounds a
  is_inside (xc, yc) = (x0 <= xc) && (xc <= x1) && (y0 <= yc) && (yc <= y1)
  turn_right (0, 1) = (-1, 0)
  turn_right (0, -1) = (1, 0)
  turn_right (1, 0) = (0, 1)
  turn_right (-1, 0) = (0, -1)


do_walk :: A.Array (Int, Int) Char -> Int-> (Int, Int) -> (Int, Int) -> IO ()
do_walk a count (xc, yc) (dx, dy) = do
  print count
  mapM_ putStrLn $ show_map a (xc, yc) 
  putStrLn ""
  case walk_1 a count (xc, yc) (dx, dy) of
    Just ((ncount), (nxc, nyc), (ndx, ndy)) -> do_walk a ncount (nxc, nyc) (ndx, ndy)
    Nothing -> print count


-- answser = 5152 too low
-- good answer = 5153
main :: IO()
main = do
  (m, n, Just ((xc, yc), (dx, dy))) <- read_data
  print n
  print ((xc, yc), (dx, dy))
  let a = A.listArray ((1, 1), (n, n)) $ L.concat m
  --print a
  let ((x0, y0), (x1, y1)) = A.bounds a
  print $ (a A.! (x0, y0))
  print $ (a A.! (x1, y1))
  print $ A.bounds a
  --mapM_ putStrLn $ show_map (xc, yc) a
  let r = walk a [(xc, yc)] (xc, yc) (dx, dy)
  --let r = walk a (S.singleton (xc, yc)) (xc, yc) (dx, dy)
  --let r = walk a (S.empty) (xc, yc) (dx, dy)
  print r
  print $ S.size $ S.fromList r
  return ()



 
