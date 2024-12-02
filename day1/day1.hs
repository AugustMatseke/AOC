module Main where
import Data.List (sort)

main :: IO ()
main = do
    let x = compareLists [2,5,7] [1,2,3]
    print x




compareLists :: [Int] -> [Int] -> Int
compareLists (x:xs) (y:ys) 
    | length xs /= length ys = error "Lists are wrong Length"
    | otherwise = comparedLists (sort xs) (sort ys)
    where
        comparedLists [] [] = 0
        comparedLists (x:xs) (y:ys) = abs(x-y) + comparedLists xs ys
        comparedLists _ _ = 0
    
