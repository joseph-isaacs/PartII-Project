module Parser.Helpers where


import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator

import Control.Applicative((*>),(<*))
import Control.Monad (void,replicateM)

import MemoryUsage.MemUsageTypes

lexemeWS :: Parser a -> Parser a
lexemeWS = lexeme ws1

lexeme :: Parser b -> Parser a -> Parser a
lexeme pb p =
  do v <- p
     pb
     return v

line :: Parser ()
line = void (many $ noneOf "\n")  *> (void $ char '\n')

betweenPara :: Parser a -> Parser a
betweenPara p = between (symbol '(') (symbol ')') p

betweenSqBrk :: Parser a -> Parser a
betweenSqBrk p = between (symbol '[') (symbol ']') p

symbol :: Char -> Parser ()
symbol s =
  do void $ char s
     return ()

ws1 :: Parser ()
ws1 = symbol ' '

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

parseUntil :: String -> Parser ()
parseUntil s = void $ many $ noneOf s

comma :: Parser ()
comma = symbol ','

num :: Parser Integer
num =
  do n <- many1 digit
     return (read n)

numK :: Parser Integer
numK =
  do n <- num
     void $ char 'K'
     return n

dot :: Parser ()
dot = symbol '.'

double :: Parser Double
double =
  do n <- many1 digit
     void $ dot
     n' <- many1 digit
     return (read (n ++ "." ++ n'))
