module Day3 where

main :: IO ()
main = do
    content <- readFile "data.txt"
    
    print (read ['4', '2', '5'] :: Int)

findMul :: String -> Int
findMul str = if substring str 0 4 == "mul(" then 1 else 0


substring :: String -> Int -> Int -> String
substring str start end = take (end - start) (drop start str)

mul :: Int -> Int -> Int
mul x y = x * y