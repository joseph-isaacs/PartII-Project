module BenchCompiler.BenchProgams where

import SampleProg.Programs


fibProgM = ("fibProg", functionsToProg [fib,"main = putInt (fib 30)"])

evenOddProgM = ("evenProg", functionsToProg [evenOdd,"main = putInt (odd 31)"])

testProgM = ("testNProg", functionsToProg [testProg, "main = putInt (testN 3000)"])


listProgM = ("listProgs", functionsToProg [listProgs, "main = putInt (sumList (take 100 (nat 0)))"])

lotsProgM = ("AllProgs", functionsToProg [fib,listProgs, testProg, "main = putInt (testN 3000)"])

allProgs = [evenOddProgM, fibProgM, testProgM, listProgM, lotsProgM]
