module StatReport where

    import Data.List (sortOn)
    import Data.Time (diffDays)
    import CovidStats
    
    avgOpen :: [CovidStats] -> Double  -- average  opening values
    avgOpen all =  sum (map open all)  / fromIntegral (length all)
    
    computeMinMaxDays :: [CovidStats] -> QField  -> (CovidStats, CovidStats, Int)
    computeMinMaxDays   all onfield  = (minQ, maxQ, daysBetweenMinMax) where
                          get = field2fun onfield
                          sorted = sortOn get all
                          minQ = head sorted
                          maxQ = last sorted
                          daysBetweenMinMax = fromIntegral $ abs $ diffDays (day minQ) (day maxQ)
    