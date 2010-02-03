import Data.List

mods a b = a `mod` b == 0

good a = a `mods` 3 || a `mods` 5

solve1 = sum [ x | x <- [1..999], good x]
solve2 = sum $ filter good [1..999]
solve3 = foldl (\a x -> if good x then a + x else a) 0 [1..999]
-- solve4 = sum $ uniq 

main = do
    putStrLn $ show solve1
    putStrLn $ show solve2
    putStrLn $ show solve3
