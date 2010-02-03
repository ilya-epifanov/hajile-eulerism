
xs = [1..100]

solve1 = abs $ sum (map sqr xs) - sqr (sum xs)
    where sqr x = x * x

main = putStrLn $ show solve1
