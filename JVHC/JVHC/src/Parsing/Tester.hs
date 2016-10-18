module Tester ()
  where

import Lexer
import System.Environment

main :: IO ()
main = do
  [f] <- getArgs
  x <- readFile f
  putStrLn x
  let s = alexScanTokens x
  putStrLn (show s)
  return ()
