{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module P03.Main01 (main) where

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
parser :: Parser ((Int, Int), String)
parser = do
    (d1, d2)  <- mult  
    rest <- many anyChar
    return ((d1, d2), rest)
    where
      mult = do 
        _ <- string "mul("
        d1 <- many1 digit
        _ <- char ','  
        d2 <- many1 digit
        _ <- char ')' 
        return (read d1, read d2) 

decode [] = []
decode st@(h:t) =
  case parse parser "source " st of
    (Right (p, rest)) ->  p:decode rest
    (Left _) -> decode t


--read_data :: IO [[Int]]
read_data = do
  inh <- openFile "./src/P03/data1" ReadMode
  loop inh []
  where
  loop inh acc  = do
    ineof <- hIsEOF inh
    if ineof
      then return $ L.foldl' (++) "" (L.reverse acc)
      else do st::String <- hGetLine inh
              loop inh (st:acc)



mult (a, b) | (a < 1000) && (b < 1000) = a*b
mult _ = 0

-- 763  & 652 too high

main :: IO()
main = do
  st <- read_data
  let r = decode st
  let r' = map mult r
  let r'' = L.sum r'
  print r''
  return ()
  --mapM_ print l

