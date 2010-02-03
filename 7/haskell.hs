
prime x = null $ filter (== 0) $ map (mod x) $ [2..xrt]
    where xrt = floor $ sqrt $ fromIntegral x

solve1 = filter prime [2..] !! 10000

main = putStrLn $ show solve1
