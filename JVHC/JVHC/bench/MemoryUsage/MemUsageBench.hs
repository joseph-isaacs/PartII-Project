module MemoryUsage.MemUsageBench where

import Control.Monad(liftM)

import SampleProg.InlineProg
import SampleProg.ProgMaker

import Pipeline.Compiler

import Printing.CSVPrint

import TimeProgram.RunProgram

import MemoryUsage.MemUsageParser
import MemoryUsage.MemUsageTypes
import MemoryUsage.GCCount

import TimeProgram.RunInlineBench(numb)
import TimeProgram.TimeRunningParser

memoryN = 50 : map ( (\x -> x -10000 ) . floor . (*) 230000 . log . log) [4,9..80]
--
-- mem2 = [255941,269245,279861,288640,296091,302541,308213,313263,317806,321929,325698,329166]

runBenchmark = varyProg memoryN

saveBenchmark :: FilePath -> IO ()
saveBenchmark path =
  do out <- (liftM (toCSV . map (\(a,b) -> (a,toStrList b))) $ runBenchmark)
     putStr out
     writeFile path out


type MaxHeaps = (Integer,Integer,Integer)

toStrList :: [(Double,Int,MaxHeaps)] -> [String]
toStrList ls =  map showAndFilt d ++ map showAndFilt i
             ++ map showAndFilt ys ++ map showAndFilt ps
             ++ map showAndFilt ms
  where (d,i,gcH) = unzip3 ls
        (ys,ps,ms) = unzip3 gcH

        showAndFilt :: Show a => a -> String
        showAndFilt = filter (/= '"') . show


varyProg :: [Int] -> IO [(IsOp String,[(Double,Int,MaxHeaps)])]
varyProg enum =
  do opt   <- mapM (runProg normalOpt) enum
     noopt <- mapM (runProg noOpt)     enum
     return $ opt ++ noopt

flags = "-Xss2000m -XX:+PrintGC -XX:+PrintGCDetails -XX:+PrintGCTimeStamps"
flags' = flags ++ " -XX:MaxJavaStackTraceDepth=1000000"

intAvg :: [Int] -> Int
intAvg l = quot (sum l) (length l)

runProg ::  OptimizeParams -> Int -> IO (IsOp String,[(Double,Int,MaxHeaps)])
runProg op n =
  do let src = buildProg n
     putStrLn $ show n
     res         <-  runN 10 src False op flags parseOut
     return ((inlineTimes op == 1, show n), res)
  where parseOut :: String -> (Double,Int,MaxHeaps)
        parseOut x = (totalGCTime (parseLines x), parseTime x, toMaxPartitions (parseLines x))

toMaxPartitions :: ([GCTimed],GCHeap) -> MaxHeaps
toMaxPartitions (gcR,h) = max3 $ unzip3 (map (handleGCOut.gcDetails) gcR)
  where (GCHeap {youngUsed = yu, parOldUsed = parU, metaspaceUsed = msU}) = h
        handleGCOut gc@(GCAlloc _ _ _ _) = (before $ youngGen gc,before $ otherGen gc,0)
        handleGCOut gc@(GCFull  _ _ _ _ _ _) = (before $ youngGen gc,before $ parOldGen gc,before $ metaSpace gc)
        max3 (a,b,c) = (maximum' yu a, maximum' parU b, maximum' msU c)
        maximum' x = foldr max x



buildProg :: Int -> String
buildProg times = functionsToProg [mkPutIntMainMethod times "testN", testProg ]
