module TimeProgram.RunProgram where

import System.Directory
import System.Process
import System.Exit
import System.FilePath

import Data.Text(pack)

import Control.Monad(liftM,foldM,replicateM)
import Control.Concurrent

import Numeric.Statistics

import GHC.IO.Handle
import Pipeline.Compiler

data BenchDefaults = BD
  { manifestPath   :: FilePath
  , runtimeJarPath :: FilePath
  }

outJarName :: String
outJarName = "bench.jar"

benchDefaults = BD { manifestPath   = "res/MANIFEST.MF"
                   , runtimeJarPath = "res/Runtime.jar"
                   }

compileAndRunProgram :: String         -> -- Command line options
                        FilePath       -> -- | Test directory path
                        String         -> -- | Program source text
                        BenchDefaults  ->
                        Bool           -> -- | Print current construct
                        OptimizeParams ->
                        IO String
compileAndRunProgram cmdLineOpts testDir
  sourceText defaults pC op =
  do let (BD { manifestPath = mP, runtimeJarPath = rTarPath }) = defaults
     tmpTestDir <- liftM (++testDir) getTemporaryDirectory
     dirExists <- doesDirectoryExist tmpTestDir
     if dirExists then removeDirectoryRecursive tmpTestDir else return ()
     createDirectory tmpTestDir
     compiler pC op (pack tmpTestDir) sourceText
     copyFile mP (appendExtension tmpTestDir mP)
     buildJar tmpTestDir
     copyFile rTarPath (appendExtension tmpTestDir rTarPath)
     threadDelay 100
     output <- runJar cmdLineOpts tmpTestDir
     removeDirectoryRecursive tmpTestDir
     return output

runN :: Int            -> -- | Number of repititions
        String         -> -- | Program source
        Bool           ->
        OptimizeParams ->
        String         -> -- | Command line options
        (String -> a)  ->
        IO [a]
runN repeats source pC op commandLineOpt outParser =
  do let toRun = compileAndRunProgram commandLineOpt "/testJVHC/" source  benchDefaults pC op
     outputs <- replicateM repeats ((liftM outParser) toRun)
     return outputs

appendExtension :: FilePath -> FilePath -> FilePath
appendExtension dir file =
  dir ++ (takeFileName file)

runJar :: String   -> -- Command line options
          FilePath -> -- Test Directory
          IO String
runJar cmdLOpts dir =
  do (_,Just o,_,h) <- createProcess (shell ("java -jar " ++ cmdLOpts ++ " " ++ outJarName))
                     { cwd = Just dir, std_out = CreatePipe }
     exitCode <- waitForProcess h
     if exitCode  == ExitSuccess
        then do hGetContents o
        else fail (show exitCode)

buildJar :: FilePath -> -- | Jar directory
            IO ()
buildJar dir =
  do let pr = (shell $ "jar cfm " ++ outJarName ++ " MANIFEST.MF *.class")
               { cwd = Just dir, std_out = CreatePipe }
     (_, _, er, ph) <- createProcess pr
     out <- waitForProcess ph
     if out  == ExitSuccess
        then return ()
        else fail (show out)



