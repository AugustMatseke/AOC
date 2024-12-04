module Day2 where
import Text.Read (Lexeme(String))

main :: IO ()
main = do
    content <- readFile "data.txt"

    let result = extractNums content

    print result


-- isSafe

extractNums :: String -> [[Int]]
extractNums string = map (map read. words) (lines string)
--One line is all you ever need

