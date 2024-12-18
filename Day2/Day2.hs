module Day2 where

main :: IO ()
main = do
    content <- readFile "data.txt"
    let vals = parse content

    print $ part1 vals


part1 :: [[Int]] -> Int
part1 = length . filter isSafe

isSafe :: [Int] -> Bool
isSafe x = monotone x && diffs x

monotone :: [Int] -> Bool
monotone xs = all (uncurry (<)) pairs || all (uncurry (>)) pairs
    where pairs = zip xs (tail xs)

diffs :: [Int] -> Bool
diffs nums = all (\x -> x > 0 && x <= 3) $ zipWith (\a b -> abs (a - b)) nums (tail nums)

parse :: String -> [[Int]]
parse string = map (map read. words) (lines string)

