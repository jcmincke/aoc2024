{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module P02.Main02 (main) where

import Text.ParserCombinators.Parsec as P
--import Text.Parsec.Char (symbol)
--import qualified Text.ParserCombinators.Parsec.Token as P
--import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Token (symbol)

import Data.List as L
--import Data.Maybe as M
--import Data.Set as S
--import Debug.Trace (trace)
--import Data.Sort (sortOn)
import System.IO
import Text.Regex.Posix



--parser :: Parser (String, String)
parser= do
    l <- (many1 digit) `sepBy` (P.string " ")
    return $ map read l



decode st =
  let (Right r) =  parse parser "source " st
  in r


read_data :: IO [[Int]]
read_data = do
  inh <- openFile "./src/P02/data" ReadMode
  loop inh []
  where
  loop inh acc  = do
    ineof <- hIsEOF inh
    if ineof
      then return $ L.reverse acc
      else do st::String <- hGetLine inh
              let nbs = decode st
              loop inh (nbs:acc)


run_through check l prefix e [] = [check prefix, check l]
run_through check l prefix e suffix@(h:t) = 
  (check (prefix++suffix)):(run_through check l (prefix++[e]) h t)


run_check l@(h:t) = run_through run_one_1 l [] h t



run_one_1 l@(_:t) = 
  let deltas = map (\(a, b) -> b-a) $ L.zip l t
      r_pos = L.all id $ map (\x -> x > 0) deltas
      r_neg = L.all id $ map (\x -> x < 0) deltas
      abs_deltas = map abs deltas
      r = L.all id $ map (\x -> (x >= 1) && (x <= 3)) abs_deltas

  in r && (r_pos || r_neg) 



run_one l@(a:b:t) = 
  case (a,b) of
    (a,b) | a == b -> False
    (a,b) | a > b -> run_one_dec l
    (a,b) | a < b -> run_one_asc l

run_one_dec (a:b:t) = 
  case (a,b) of
    (a,b) | a == b -> False
    (a,b) | a > b -> case (a-b) of
                        d | (d >= 1) && (d <= 3) -> run_one_dec (b:t)
                        _ -> False
    (a,b) | a < b -> False
run_one_dec [_] = True


run_one_asc (a:b:t) = 
  case (a,b) of
    (a,b) | a == b -> False
    (a,b) | a < b -> case (b-a) of
                        d | (d >= 1) && (d <= 3) -> run_one_asc (b:t)
                        _ -> False
    (a,b) | a > b -> False
run_one_asc [_] = True


-- 671 too high
-- 650 too low
-- 659 too low

main :: IO()
main = do
  l <- read_data
  let res::[[Bool]] = map run_check l
  let res'' = L.map (\r -> L.foldl' (||) False r) res
  let res' = L.filter id res''
  mapM_ print res''
  print $ L.all (\x -> length x == 5) l
  print $ length res'

  let xs::[Int] = [6, 7, 9, 11, 13, 14, 18]
  print xs
  let sol = run_check xs
  print sol







{-

run_one_dec_1 (a:b:t) = 
  case (a,b) of
    (a,b) | a == b -> False
    (a,b) | a > b -> case (a-b) of
                        d | (d >= 1) && (d <= 3) -> run_one_dec_1 (b:t)
                        _ -> False
    (a,b) | a < b -> False
run_one_dec_1 [_] = True
run_one_dec_1 [] = True


run_one_asc_1 (a:b:t) = 
  case (a,b) of
    (a,b) | a == b -> False
    (a,b) | a < b -> case (b-a) of
                        d | (d >= 1) && (d <= 3) -> run_one_asc_1 (b:t)
                        _ -> False
    (a,b) | a > b -> False
run_one_asc_1 [_] = True
run_one_asc_1 [] = True
-}

