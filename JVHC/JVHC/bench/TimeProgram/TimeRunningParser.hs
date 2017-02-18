module TimeProgram.TimeRunningParser where

import Data.Either

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator

import Control.Applicative((*>),(<*))
import Control.Monad (void,replicateM)

import Parser.Helpers

-- parseTime :: Num a => String -> a
parseTime s = fromIntegral
            $ head
            $ rights
            $ map (parse parseTimeRunning "") (lines s)

-- parseLines s = either (const $ error $ "Cannot GCOutput parse: " ++ show s) id  (parse parseGCOutput "" s)

parseTimeRunning :: Parser Integer
parseTimeRunning =
  do void $ string "Time Running: "
     n <- num
     return n

parseMaxStack :: Parser Integer
parseMaxStack =
  do void $ string "Stack Max Height: "
     n <- num
     return n

parseMaxStackHeight :: String -> Int
parseMaxStackHeight s =
               fromIntegral
             $ head
             $ rights
             $ map (parse parseMaxStack "") (lines s)



fromFile f =
  do s <- readFile f
     -- putStrLn s
     let a =  parseTime s
     putStrLn $ show a
     return ()
