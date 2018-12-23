module Main where

import PE0001
import PE0002
import PE0003
import PE0004

runProblem :: Show a => Integer -> String -> a -> IO ()
runProblem problemID problemTitle solution = do
    putStr (show problemID)
    putStr ") "
    putStr problemTitle
    putStr ": "
    print solution

main :: IO ()
main = do
    runProblem PE0001.problemID PE0001.problemTitle PE0001.solution
    runProblem PE0002.problemID PE0002.problemTitle PE0002.solution
    runProblem PE0003.problemID PE0003.problemTitle PE0003.solution
    runProblem PE0004.problemID PE0004.problemTitle PE0004.solution
