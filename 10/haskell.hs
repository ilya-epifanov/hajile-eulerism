{-# LANGUAGE BangPatterns #-}
import Data.List
import Debug.Trace

l = 2000000

trace_ a = traceShow a a

isqrt x = (floor $ sqrt (fromIntegral x))
cutoff l xs = takeWhile (\x -> x < l) xs

prime n = null [x | x <- [2..(isqrt n)], n `mod` x == 0]

primes1 = filter prime [2..]

prime2 tests n = null [x | x <- cutoff ((isqrt n)+1) tests, n `mod` x == 0]

-- nextPrime :: [Int] -> Int -> Int
nextPrime ps c
    | prime2 ps c = c
    | otherwise   = nextPrime ps (c+1)

primes2 = unfoldr unfolder ([2], 2)
    where unfolder (ps, p) = let p' = nextPrime ps (p+1)
                             in Just (p, (ps ++ [p'], p'))

solve1 = sum $ cutoff l primes1
solve2 = sum $ cutoff l primes2

main = putStrLn $ show solve2
