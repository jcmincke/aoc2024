{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module P05.Main01 (main) where

import Text.ParserCombinators.Parsec as P
import qualified Data.List as L
import Data.Maybe as M
import qualified Data.Map.Strict as M
import System.IO





parser_1 :: Parser (Int, Int)
parser_1 = do
    (d1, d2)  <- mult  
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


read_data fn parser = do
  inh <- openFile fn ReadMode
  loop inh []
  where
  decode st =
    let (Right r) =  parse parser "source " st
    in r

  loop inh acc  = do
    ineof <- hIsEOF inh
    if ineof
      then return $ L.reverse acc
      else do st::String <- hGetLine inh
              let nbs = decode st
              loop inh (nbs:acc)


sort_all rules updates =
  catMaybes (map (is_good) updates)
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
  rules <- read_data "./src/P05/data_1" parser_1
  updates <- read_data "./src/P05/data_2" parser_2
  let sorted = sort_all rules updates
  print "----"
  mapM_ print sorted
  print ()
  print $ (map middle sorted)
  print $ sum (map middle sorted)
  return ()
 
