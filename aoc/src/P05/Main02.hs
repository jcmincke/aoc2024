{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module P05.Main02 (main) where

import Text.ParserCombinators.Parsec as P
import Text.Parsec.Char (char, string, anyChar)
import Text.Parsec.Token (symbol)
import Control.Monad (guard)
import Data.List as L
import Data.Maybe as M
import Data.Ord (comparing)
import System.IO
import Text.Regex.Posix
import qualified Data.Array as A
import qualified Data.Map.Strict as M




parser_1 :: Parser (Int, Int)
parser_1 = do
    (d1, d2)  <- mult  
    rest <- many anyChar
    return (d1, d2)
    where
      mult = do 
        d1 <- many1 digit
        _ <- char '|'  
        d2 <- many1 digit
        return (read d1, read d2) 



parser_2 :: Parser [Int]
parser_2 = do
    (fmap read (many1 digit)) `sepBy` (char ',' )

decode_1 st =
  let (Right r) =  parse parser_1 "source " st
  in r

decode_2 st =
  let (Right r) =  parse parser_2 "source " st
  in r

read_data_1 = do
  inh <- openFile "./src/P05/data_1" ReadMode
  loop inh []
  where
  loop inh acc  = do
    ineof <- hIsEOF inh
    if ineof
      then return $ L.reverse acc
      else do st::String <- hGetLine inh
              let nbs = decode_1 st
              loop inh (nbs:acc)


read_data_2 = do
  inh <- openFile "./src/P05/data_2" ReadMode
  loop inh []
  where
  loop inh acc  = do
    ineof <- hIsEOF inh
    if ineof
      then return $ L.reverse acc
      else do st::String <- hGetLine inh
              let nbs = decode_2 st
              loop inh (nbs:acc)

rule_map rules = M.fromList 


sort_all rules updates =
  map sort_one updates
  where
  is_good pages = if pages == sort_one pages then Just pages else Nothing
  sort_one pages = L.sortBy cmp pages
  rule_map::M.Map Int [Int] = L.foldl' (\acc (k, v) -> M.insertWithKey (\k pv nv -> pv ++ nv) k [v] acc) M.empty rules
  cmp a b = 
    case M.lookup a rule_map of
      Nothing -> GT
      Just others -> if elem b others then LT else GT

middle updates = updates L.!! ((div (length updates) 2 ))

-- answser = 5713

main :: IO()
main = do
  rules <- read_data_1
  updates <- read_data_2
  --print rules
  --print updates
  let sorted = sort_all rules updates
  print "----"
  mapM print sorted
  print ()
  print $ (map middle sorted)
  print $ sum (map middle sorted) - 5713
  return ()
 
