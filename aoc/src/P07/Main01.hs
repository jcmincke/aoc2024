{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module P07.Main01 (main) where

import qualified Text.ParserCombinators.Parsec as P
import qualified Data.List as L
import Data.Maybe as M
import qualified Data.HashSet as S
import qualified Data.Map.Strict as M
import System.IO
import qualified Data.Array as A




import Text.ParserCombinators.Parsec as P
import Text.Parsec.Char (char, string, anyChar)
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


-- mul(2,4). `sepBy`
parser :: Parser (Int, [Int])
parser = do
    total <- nb_p
    _ <- char ':'  
    _ <- char ' '
    nbs <-  nb_p `sepBy` (char ' ')
    return (total, nbs)
    where 
    nb_p = fmap read $ (many1 digit)


decode st =
  case parse parser "source " st of
    (Right r) -> r


read_data :: IO [(Int, [Int])]
read_data = do
  inh <- openFile "./src/P07/data_1" ReadMode
  loop inh []
  where
  loop inh acc  = do
    ineof <- hIsEOF inh
    if ineof
      then return $ (L.reverse acc)
      else do st::String <- hGetLine inh
              let r = decode st
              loop inh (r:acc)


do_one :: Int -> [Int] -> Int
do_one total (nbs@(h:t)) = 
  if L.elem total res then total else 0
  where
  go :: Char -> Int -> [Int] -> [Int]
  go '+' current (nbs@(h:[])) = [current + h]
  go '+' current (nbs@(h:t)) = (go '+' (current + h) t) ++ (go '*' (current + h) t)
  go '*' current (nbs@(h:[])) = [current * h]
  go '*' current (nbs@(h:t)) = (go '+' (current * h) t) ++ (go '*' (current * h) t)
  res = (go '+' h t) ++ (go '*' h t)

main :: IO()
main = do
  inputs <- read_data
  let l = map (\(total, nbs) -> do_one total nbs) inputs
  --mapM_ print l
  print$ sum l
  return ()











 
