module Day1 where

import Data.List (sort)


main :: IO ()
main = do
    contents <- readFile "data.txt"
<<<<<<< HEAD
    let (listOne, listTwo) = extractNums contents
=======
    let listOne  =  sort $ map head $ extractNums contents
    let listTwo  =  sort $ map last $ extractNums contents
>>>>>>> 9ae71f21638ab348eab2b6373e5c075703b7531c
    
    let diff     = compareLists listOne listTwo
    let similar  = similarityScore listOne listTwo
    print $ show diff ++ ", " ++ show similar
<<<<<<< HEAD
=======
    

>>>>>>> 9ae71f21638ab348eab2b6373e5c075703b7531c

compareLists :: [Int] -> [Int] -> Int
compareLists (x:xs) (y:ys) = abs (x - y) + compareLists xs ys

similarityScore :: [Int] -> [Int] -> Int
similarityScore (x:xs) rest = (x * count x rest)  + similarityScore xs rest
similarityScore _ _ = 0

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

<<<<<<< HEAD
extractNums :: String -> ([Int], [Int])
extractNums string =
    let (s1, s2) = unzip $ map ((\[x, y] -> (read x, read y)) . words) (lines string)
    in (s1, s2)
--One line is all you need
=======
extractNums :: String -> [[Int]]
extractNums string = map (map read . words) (lines string)
--One line is all you need
    
>>>>>>> 9ae71f21638ab348eab2b6373e5c075703b7531c
