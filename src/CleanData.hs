module CleanData where

import Data.List.Split (splitOn)

-- Separate the flat string into an array of Strings taking out the heading
getImportedData :: String -> [[String]]
getImportedData csvRawData =  drop 7 $ filter (/=[""]) ( map (splitOn ",") $ splitOn "\n" csvRawData )

--dropColumns :: [[String]] -> [[String]]
--dropColumns xss = [x | xs <- xss, x <- xs]