module CovidStats where

import Data.Time (Day, parseTimeOrError,  defaultTimeLocale, diffDays)
import CleanData

data CovidStats = CovidStats {  -- using the order that the csv file uses
                   day :: Day,
                   volume :: Int,
                   open :: Double,
                   close :: Double,
                   high :: Double,
                   low :: Double
                 }
  deriving (Ord, Eq, Show)


data QField = Open | Close | High | Low | Volume
  deriving (Eq, Ord, Show, Enum, Bounded)

field2fun :: QField -> CovidStats -> Double
field2fun Open = open
field2fun Close = close
field2fun High = high
field2fun Low = low
field2fun Volume = fromIntegral . volume

makeDate :: String -> Day
makeDate  = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

--convert each of the strings to an appropriate type in form of tuple
strToTuple :: [String] -> (Day, Double, Int, Double, Double, Double)
strToTuple (day:close:vol:open:high:low:_)  = 
              (makeDate day, read close:: Double, read vol:: Int, 
              read open::Double,  read high :: Double, read low :: Double)

tupleToQuote :: (Day, Double, Int, Double, Double, Double)  -> CovidStats
tupleToQuote (day, close, vol, open, high, low )  = CovidStats day vol open  close high low


makeAllQuotes::String -> [CovidStats]
makeAllQuotes csvData = map (tupleToQuote . strToTuple ) (getImportedData csvData)