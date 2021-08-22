module Main where

import System.Environment(getArgs)
import CleanData
import StatReport
import CovidStats

main :: IO ()
main  = do
  let file = "covidStats.csv"
  stats <- readFile file

  let (minQ, maxQ, numDays) = computeMinMaxDays  (makeAllQuotes stats) Open
  putStrLn "-------------------------------------------------------------------------------"
  putStrLn "Category   | Minimum       | Maximum     | Average            | Days Difference"
  putStrLn "-------------------------------------------------------------------------------"
 

  putStrLn $  "Open       |" ++ show(open minQ ) ++ "     |"  ++  show (open maxQ)   ++
           "   |"  ++ show (avgOpen $ makeAllQuotes stats)  ++    "   |"  ++show numDays