module Day2 where
-- import Text.Read (Lexeme(String))

main :: IO ()
main = do
    content <- readFile "data.txt"
    let vals = parse content
    let test = [1, 3, 8, 10]

    print $ diffs test

    -- print $ head vals


isSafe :: [Int] -> Bool
isSafe x = monotone x && True
    -- | all $ 

monotone :: [Int] -> Bool
monotone xs  
    | and $ zipWith (<) xs (tail xs) = True
    | and $ zipWith (>) xs (tail xs) = True
    | otherwise = False

diffs ::  [Int] -> Bool
diffs x =  do
    let list = zipWith (-) (tail x) x
    return True

parse :: String -> [[Int]]
parse string = map (map read. words) (lines string)
