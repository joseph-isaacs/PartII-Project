module CodeSize.CountCodeSize where


import Control.Monad(liftM,foldM,replicateM)
import Control.Concurrent

import System.Directory
import System.Process
import System.Exit
import System.FilePath
import System.Posix.Files(fileSize,getFileStatus)

import GHC.IO.Handle

import Data.Text

import Pipeline.Compiler

import Printing.CSVPrint

import TimeProgram.RunProgram

import BenchCompiler.BenchProgams

countCodeSize :: FilePath       -> -- | Test directory path
                 String         -> -- | Program source text
                 BenchDefaults  ->
                 OptimizeParams ->
                 IO Integer
countCodeSize testDir
  sourceText defaults op =
  do let (BD { manifestPath = mP, runtimeJarPath = rTarPath }) = defaults
     tmpTestDir <- liftM (++testDir) getTemporaryDirectory
     dirExists <- doesDirectoryExist tmpTestDir
     if dirExists then removeDirectoryRecursive tmpTestDir else return ()
     createDirectory tmpTestDir
     compiler False op (pack tmpTestDir) sourceText
     copyFile mP (appendExtension tmpTestDir mP)
     buildJar tmpTestDir
     size <- countSize (appendExtension tmpTestDir outJarName)
     removeDirectoryRecursive tmpTestDir
     return size

countSize :: FilePath -> IO Integer
countSize fp =
  do fs <- getFileStatus fp
     let st = fileSize fs
     putStrLn $ show st
     return $ toInteger st


saveBenchmark :: FilePath -> IO ()
saveBenchmark path =
  do out <- (liftM toCSV $ runBenchmark)
     putStr out
     writeFile path out


runBenchmark = countOpAndNOp allProgs

countOpAndNOp :: (Show a, Num a) => [(String,String)] -> IO [(IsOp String,[Integer])]
countOpAndNOp progs =
  do p   <- mapM (runProg noOpt)     progs
     pOp <- mapM (runProg normalOpt) progs
     return $ p ++ pOp

runProg :: OptimizeParams -> (String,String) -> IO (IsOp String,[Integer])
runProg op (name,src)  =
  do res <- countCodeSize "/testJVHCCodeSize/" src benchDefaults op
     putStrLn (name ++ ": " ++ show res)
     return ((inlineTimes op == 1, name),[res])
