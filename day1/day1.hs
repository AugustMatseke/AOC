module Day1 where

import Data.List (sort)


main :: IO ()
main = do
    contents <- readFile "data.txt"
    let (listOne, listTwo) = extractNums contents
    
    let diff     = compareLists listOne listTwo
    let similar  = similarityScore listOne listTwo
    print $ show diff ++ ", " ++ show similar

compareLists :: [Int] -> [Int] -> Int
compareLists (x:xs) (y:ys) = abs (x - y) + compareLists xs ys

similarityScore :: [Int] -> [Int] -> Int
similarityScore (x:xs) rest = (x * count x rest)  + similarityScore xs rest
similarityScore _ _ = 0

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

extractNums :: String -> ([Int], [Int])
extractNums string =
    let (s1, s2) = unzip $ map ((\[x, y] -> (read x, read y)) . words) (lines string)
    in (s1, s2)
--One line is all you need
