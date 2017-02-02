module MemoryUsage.MemUsageParser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator

import Control.Applicative((*>),(<*))
import Control.Monad (void)

import MemoryUsage.MemUsageTypes

parseLines :: String -> [GCOutput]
parseLines = (either (const []) id) . (parse parseGCLines "")

parseGCLines :: Parser [GCOutput]
parseGCLines = whitespace *> sepEndBy eitherGC whitespace

eitherGC :: Parser GCOutput
eitherGC = choice [try gcAlloc, gcFull]

gcAlloc :: Parser GCOutput
gcAlloc =
  do symbol '['
     void $ lexemeWS $ string "GC (Allocation Failure)"
     yGC  <- lexemeWS $ betweenSqBrk (parPSGen "PSYoungGen")
     oGC  <- lexemeWS $ lexeme comma gcTriple
     rTime <- lexemeWS double <* secs
     lexemeWS $ symbol ']'
     realTime <- betweenSqBrk times
     return $ GCAlloc yGC oGC realTime rTime

gcFull :: Parser GCOutput
gcFull =
  do symbol '['
     void $ lexemeWS $ string "Full GC (Ergonomics)"
     yG  <- lexemeWS $ betweenSqBrk (parPSGen "PSYoungGen")
     poG <- lexemeWS $ betweenSqBrk (parPSGen "ParOldGen")
     oG  <- lexemeWS $ lexeme comma gcTriple
     ms  <- lexemeWS $ lexeme comma $ betweenSqBrk (parPSGen "Metaspace")
     rTime <- lexemeWS double <* secs
     lexemeWS $ symbol ']'
     realTime <- betweenSqBrk times
     return $ GCFull yG poG oG ms realTime rTime


times :: Parser Times
times =
  do void $ string "Times"
     lexemeWS $ symbol ':'
     user <- lexemeWS $ valueEqual "user"
     sys  <- lexemeWS $ lexeme comma (valueEqual "sys")
     real <- lexemeWS $ valueEqual "real"
     secs
     return $ Times user sys real

lexemeWS :: Parser a -> Parser a
lexemeWS = lexeme ws1

lexeme :: Parser b -> Parser a -> Parser a
lexeme pb p =
  do v <- p
     pb
     return v



secs :: Parser ()
secs = void $ string "secs"

valueEqual :: String -> Parser Double
valueEqual name =
  do void $ string name
     symbol '='
     double


parPSGen :: String ->    -- Word at start of [**: XK->YK(ZK)]
            Parser GCRun -- To X Y Z
parPSGen word =
  do void $ string word
     symbol ':'
     void $ spaces
     gcTriple

gcTriple :: Parser GCRun
gcTriple =
  do b <- numK
     void $ string "->"
     a <- numK
     t <- betweenPara numK
     return $ GCRun b a t

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
