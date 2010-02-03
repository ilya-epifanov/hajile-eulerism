import Data.Char

-- cheating
solve1 = sum $ map digitToInt $ show (2^1000)

main = putStrLn $ show solve1

