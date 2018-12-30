{-# OPTIONS_GHC -F -pgmF ./project-euler-discover #-}
module Solutions (solutions) where

import Criterion.Measurement (initializeTime, getTime)
import Data.Text (pack)
import Text.Printf (printf)
import Control.Monad (foldM, liftM)

runProblem :: (Integer, String, String) -> IO Double
runProblem (problemID, problemTitle, solution) = do
    start <- getTime
    printf "%04d %-40s %24s" problemID problemTitle solution
    end <- getTime

    let duration = end - start
    printf " % 9.5f s\n" duration

    return duration

drawLine :: IO ()
drawLine = printf "%s %s %s %s\n" (replicate 4 '-') (replicate 40 '-')
                                  (replicate 24 '-') (replicate 11 '-')

solutions :: IO ()
solutions = do
    initializeTime

    printf "%-4s %-40s %24s %-11s\n" "Num." "Title" "Solution" "Time"
    drawLine

    total <- foldM (\dur pb -> (dur +) <$> runProblem pb) 0.0 problems

    drawLine
    printf "Total time:%60s% 9.5f s\n" "" total
