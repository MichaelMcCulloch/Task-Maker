{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( magic
    ) where

import qualified Data.ByteString.Lazy as BL
import Data.Time
import Data.Csv
import Data.List
import qualified Data.Vector as V

type Task = (String, Day, Double)

magic :: IO ()
magic = do
    putStrLn "Time commitment"
    comm <- getLine
    csvData <- BL.readFile "tasks.csv"
    curr <- getCurrentTime
    let 
        today = utctDay curr
        tasks = readTasks csvData
        c = read comm
        amount = map (\(s, d) -> (s,c * 60 * d)) $ makeTaskList tasks today
        sorted = sortBy (\(_, a) (_, b) -> compare b a) amount
        in mapM_ putStrLn $ map (\(s, t) -> s ++ ": " ++ show (floor t) ++ " minutes") sorted


readTasks :: BL.ByteString -> [Task]
readTasks dat = 
    case decode NoHeader dat of
        Left error -> []
        Right v -> V.toList (V.map makeTask v)


toInt ::[String] -> [Int]
toInt = map read

makeTask :: (String, String, Double) -> Task
makeTask (n, t, w) = let
    (y:m:d:_) = toInt $ splitBy '-' t 
    in (n, fromGregorian (toInteger y) m d, w)

date :: IO (Day) -- :: (year,month,day)
date = getCurrentTime >>= return . utctDay

fromGregorian':: [(Integer, Int, Int)] -> [Day]
fromGregorian' [] = []
fromGregorian' ((y, m, d):rest) = let 
    x = fromGregorian y m d
    xs = fromGregorian' rest
    in x:xs

diffDays' :: Day -> [Day] -> [Integer]
diffDays' today days =
     map (\x -> diffDays x today) days



--List of tasks and Today's date to number of minutes to spend on each task
makeTaskList:: [Task] -> Day-> [(String, Double)]
makeTaskList task today = let
    (names, days, weights) = unzip3 task
    daysRemaining = diffDays' today days
    daysRemainingF = map ( \x -> 1.0 /  fromIntegral x) daysRemaining
    dayWeights = makeWeights daysRemainingF
    valueWeights = makeWeights weights
    taskWeight = zipWith (*) valueWeights dayWeights
    theWeights = makeWeights taskWeight
    in zipWith (\s v -> (s, v)) names theWeights

makeWeights::[Double] -> [Double]
makeWeights weights = let
    s = sum weights
    in map (/s) weights

splitBy :: (Foldable t, Eq p) => p -> t p -> [[p]]
splitBy delimiter = foldr f [[]] where 
    f c l@(x:xs) 
        | c == delimiter = []:l
        | otherwise = (c:x):xs