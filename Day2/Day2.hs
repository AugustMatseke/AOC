module Day2 where

main :: IO ()
main = do
    content <- readFile "data.txt"
    let vals = parse content

    print $ part1 vals
    print $ part2 vals


part1 :: [[Int]] -> Int
part1 vals = length $ filter isSafe vals

part2 :: [[Int]] -> Int
part2 [] = 0
part2 (x:xs) = case (monotone x, diffs x) of
    (0, 0) -> 1 + part2 xs
    _      -> if dampened x then 1 + part2 xs else part2 xs
    where
        dampened :: [Int] -> Bool
        dampened x = any isSafe (brute x)

brute :: [a] -> [[a]]
brute [] = []
brute (x:xs) = xs : map (x :) (brute xs)

isSafe :: [Int] -> Bool
isSafe x = monotone x == 0 && diffs x == 0

monotone :: [Int] -> Int
monotone xs = min (length (filter not lt)) (length (filter not gt))
    where pairs = zip xs (tail xs)
          lt = map (uncurry (<)) pairs
          gt = map (uncurry (>)) pairs

diffs :: [Int] -> Int
diffs nums = length $ filter (\(x, y) -> abs (x - y) > 3) pairs
    where pairs = zip nums (tail nums)

parse :: String -> [[Int]]
parse string = map (map read. words) (lines string)
