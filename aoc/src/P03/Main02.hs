{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module P03.Main02 (main) where

import Text.ParserCombinators.Parsec as P
import Text.Parsec.Char (char, string, anyChar)
--import Text.Parsec.Char (symbol)
--import qualified Text.ParserCombinators.Parsec.Token as P
--import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Token (symbol)
import Debug.Trace (trace)
import Data.List as L
--import Data.Maybe as M
--import Data.Set as S
--import Debug.Trace (trace)
--import Data.Sort (sortOn)
import System.IO
import Text.Regex.Posix


data OpCode = 
  Mult Int Int
  |Do
  |Dont
  deriving Show

-- mul(2,4). `sepBy`
parser :: Parser (OpCode, String)
parser = do
    op_code  <- choice [p_mult, p_dont, p_do]  
    rest <- many anyChar
    return (op_code, rest)
    where
      p_mult = do 
        _ <- string "mul("
        d1 <- many1 digit
        _ <- char ','  
        d2 <- many1 digit
        _ <- char ')' 
        return $ Mult (read d1) (read d2) 
      p_do = fmap (\_ -> Do) $ string "do()" 
      p_dont = fmap (\_ -> Dont) $ string "xyz()" 

decode [] = []
decode st@(h:t) =
  case parse parser "source " st of
    (Right (p, rest)) ->  p:decode rest
    (Left _) ->  decode t


--read_data :: IO [[Int]]
read_data = do
  inh <- openFile "./src/P03/data2" ReadMode
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

filter_op _ [] = []
filter_op True ((Mult a b):t) = (a, b): filter_op True t
filter_op True (Do:t) = filter_op True t
filter_op True (Dont:t) = filter_op False t
filter_op False ((Mult a b):t) = filter_op False t
filter_op False (Do:t) = filter_op True t
filter_op False (Dont:t) = filter_op False t

-- 763  & 652 too high

main :: IO()
main = do
  st <- read_data
  let r = decode st
  let r' = filter_op True r
  let r'' = map mult r'
  --let r'' = L.sum r'
  print $ L.sum r''
  return ()
  --mapM_ print l

