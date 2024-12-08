{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module P03.MainTest (main) where

import Text.ParserCombinators.Parsec as P

data OpCode = 
  Mult Int Int
  |Do
  |Dont
  deriving Show

parser :: Parser (OpCode, String)
parser = do
    op_code  <- choice [(try p_dont), (try p_do), (try p_mult)  ]  
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
      p_dont = fmap (\_ -> Dont) $ string "don't()" 


decode :: String -> [OpCode]
decode [] = []
decode st@(_:t) =
  case parse parser "source " st of
    (Right (p, rest)) ->  p:decode rest
    (Left _) ->  decode t


main :: IO()
main = do
  let st1 = "xdon't() do() mul(1,2)"
  print $ decode st1
  let st2 = "xdon't() do() mul(1,2)"
  print $ decode st2
  let st3 = "do() don't() do() mul(1,2)"
  print $ decode st3
  let st4 = "don't() do() mul(1,2)"
  print $ decode st4
  let st5 = "don't() mul(1,2)"
  print $ decode st5
  let st6 = "don't()"
  print $ decode st6
