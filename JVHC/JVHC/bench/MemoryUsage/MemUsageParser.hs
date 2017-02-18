module MemoryUsage.MemUsageParser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator

import Control.Applicative((*>),(<*))
import Control.Monad (void,replicateM)

import MemoryUsage.MemUsageTypes

import Parser.Helpers

parseLines :: String -> GCParserOut
parseLines s = either (const $ error $ "Cannot GCOutput parse: " ++ show s) id  (parse parseGCOutput "" s)

parseLines2 s = either (const $ error $ "Cannot GCOutput parse: " ++ show s) id  (parse parseLine "" s)


parseGCOutput :: Parser ([GCTimed],GCHeap)
parseGCOutput =
  do timed <- manyTill (try parseLine) (lookAhead gcHeap)
     gcH <- gcHeap
     return (concat timed,gcH)


parseLine :: Parser [GCTimed]
parseLine = choice [try parseGCLine, try parseAny]

parseGCLine :: Parser [GCTimed]
parseGCLine =
  do l <- lexeme whitespace timedGC
     return [l]

parseAny :: Parser [GCTimed]
parseAny =
  do line
     return []


gcHeap :: Parser GCHeap
gcHeap =
  do void $ lexeme line $ string "Heap"
     (yTotal,yUsed)     <- parseHeapLine "PSYoungGen"
     replicateM 3 line
     (parTotal,parUsed) <- parseHeapLine "ParOldGen"
     line
     (metaTotal,metaUsed) <- parseMetaspace
     return $ GCHeap { youngUsed      = yUsed
                     , youngTotal     = yTotal
                     , parOldTotal    = parTotal
                     , parOldUsed     = parUsed
                     , metaspaceTotal = metaTotal
                     , metaspaceUsed  = metaUsed  }


parseMetaspace :: Parser (Integer,Integer)
parseMetaspace =
  do ws1
     void $ lexeme whitespace $ string "Metaspace"
     void $ lexemeWS $ string "used"
     mUsed <- lexemeWS $ lexeme comma $ numK
     void $ lexemeWS $ string "capacity"
     mTotal <- numK
     replicateM 2 line
     return (mTotal,mUsed)



parseHeapLine :: String -> Parser (Integer,Integer)
parseHeapLine name =
  do ws1
     void $ lexeme whitespace $ string name
     void $ lexemeWS $ string "total"
     yTotal <- lexemeWS $ lexeme comma $ numK
     void $ lexemeWS $ string "used"
     yUsed <- numK
     line
     return (yTotal, yUsed)


timedGC :: Parser GCTimed
timedGC =
  do time <- double
     lexemeWS $ symbol ':'
     gcOut <- eitherGC
     return $ GCTimed time gcOut


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
