module Day1 where
import Data.List (sort)

readi :: String -> Int
readi = read

main :: IO ()
main = do
    contents <- readFile "data.txt"
    let eachLine =  map words $ lines contents
    let listOne  =  sort $ map (readi . head) eachLine
    let listTwo  =  sort $ map (readi . last) eachLine 
    
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
    