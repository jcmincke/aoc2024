{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}



module P06.Main02 (main) where

import qualified Text.ParserCombinators.Parsec as P
import qualified Data.List as L
import Data.Maybe as M
import qualified Data.HashSet as S
import qualified Data.Map.Strict as M
import System.IO
import qualified Data.Array as A
import Data.Hashable
import GHC.Generics

data Dir =
    DUp
  | DDown
  | DRight
  | DLeft
  deriving (Show, Eq, Generic, Hashable)

turn_right DUp = DRight
turn_right DRight = DDown
turn_right DDown = DLeft
turn_right DLeft = DUp


data Guard = Guard (Int, Int) Dir
  deriving (Show, Eq, Generic, Hashable)

move_guard (Guard (xg, yg) DUp) = Guard (xg, yg-1) DUp
move_guard (Guard (xg, yg) DRight) = Guard (xg+1, yg) DRight
move_guard (Guard (xg, yg) DDown) = Guard (xg, yg+1) DDown
move_guard (Guard (xg, yg) DLeft) = Guard (xg-1, yg) DLeft


read_data  = do
  inh <- openFile "./src/P06/data_1" ReadMode
  loop 1 Nothing inh []
  where
  decode x y d '#' = ('#', d)
  decode x y d '.' = ('.', d)
  decode x y d 'v' = ('.', Just $ Guard (x, y) DDown)
  decode x y d '^' = ('.', Just $ Guard (x, y) DUp)
  decode x y d '>' = ('.', Just $ Guard (x, y) DRight)
  decode x y d '<' = ('.', Just $ Guard (x, y) DLeft)

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



--is_cycle :: A.Array (Int, Int) Char -> S.HashSet Guard -> Guard -> Bool
is_cycle lab current_path current_path_set guard@(Guard (xg, yg) dir) = 
  if is_inside (nxg, nyg)
  then  
    case lab A.! (nyg, nxg) of
    '.' -> if S.member n_guard current_path_set
           then (current_path, True)
           else is_cycle lab (n_guard:current_path) (S.insert n_guard current_path_set) n_guard
    '#' -> is_cycle lab current_path current_path_set (Guard (xg, yg) (turn_right dir))
  else (current_path, False)
  where
  n_guard@(Guard (nxg, nyg) _) = move_guard guard
  is_inside (xc, yc) = (x0 <= xc) && (xc <= x1) && (y0 <= yc) && (yc <= y1)
  ((x0, y0), (x1, y1)) = A.bounds lab



find_path lab current_path (guard@(Guard (xg, yg) dir)) = 
  if is_inside (nxg, nyg)
  then  
    case lab A.! (nyg, nxg) of
    '.' -> find_path lab (n_guard:current_path)n_guard
    '#' -> find_path lab current_path (Guard (xg, yg) (turn_right dir))
  else L.reverse current_path
  where
  n_guard@(Guard (nxg, nyg) _) = move_guard guard
  is_inside (xc, yc) = (x0 <= xc) && (xc <= x1) && (y0 <= yc) && (yc <= y1)
  ((x0, y0), (x1, y1)) = A.bounds lab


find_changes lab path (guard@(Guard (xg, yg) dir)) = 
  L.filter (\(b, _) -> b) $ map check_one locs_to_block
  where
  path' = S.fromList path
  locs_to_block = S.toList $ S.delete (xg, yg) (S.map (\(Guard pos _) -> pos) path')
  block_pos (x, y) = lab A.// [((y, x), '#')] 
  blocked_labs = map block_pos locs_to_block
  check_one (x, y) = 
    case is_cycle (block_pos (x, y)) [guard] (S.singleton guard) guard of 
      (_, b) -> (b, (x, y))




main :: IO()
main = do
  (m, n, Just guard) <- read_data
  print n
  print guard
  let lab = A.listArray ((1, 1), (n, n)) $ L.concat m
  --print a
  --let r = is_cycle lab [guard] (S.singleton guard) guard
  let path = find_path lab [guard] guard
  print $ S.size (S.map (\(Guard pos _) -> pos) $ S.fromList path)
  --print path
  let blocks = find_changes lab path guard
  --let r = walk a (S.singleton (xc, yc)) (xc, yc) (dx, dy)
  --let r = walk a (S.empty) (xc, yc) (dx, dy)
  print blocks
  print $ L.length blocks
  --print $ S.size $ S.fromList r
  return ()



 
