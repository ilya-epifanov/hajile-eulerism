

n = 600851475143
--n = 65536

isqrt x = ceiling $ sqrt $ fromIntegral x

divisors x = filter (\n -> x `mod` n == 0) $ [2..(isqrt x)]

isprime x = null $ divisors x

solve1 = head $ filter isprime $ reverse $ divisors n

main = do
  putStrLn $ show solve1