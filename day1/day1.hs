module Day1 where

import Data.List (sort)


main :: IO ()
main = do
    contents <- readFile "data.txt"
    let listOne  =  sort $ map head $ extractNums contents
    let listTwo  =  sort $ map last $ extractNums contents
    
    let diff     = compareLists listOne listTwo
    let similar  = similarityScore listOne listTwo
    print $ show diff ++ ", " ++ show similar
    


compareLists :: [Int] -> [Int] -> Int
compareLists (x:xs) (y:ys) = abs(x - y) + compareLists xs ys
compareLists _ _ = 0
 
similarityScore :: [Int] -> [Int] -> Int
similarityScore (x:xs) rest = (x * count x rest)  + similarityScore xs rest
similarityScore _ _ = 0

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

extractNums :: String -> [[Int]]
extractNums string = map (map read . words) (lines string)
--One line is all you need
    