import Data.List
import Data.Array

dividents n o l = takeWhile (< l) $ iterate (+ n) first
    where first = (((o-1) `div` n) + 1) * n

incArray a ixs = accum (\v _ -> v+1) a (map (\ix -> (ix, ())) ixs)

incDividents a o l = incArray a $ concat [dividents n o l | n <- [2..l]]

isqrt x = floor $ sqrt $ fromIntegral x

divisors n = foldl red [] [d | d <- [1..(isqrt n)], n `mod` d == 0]
    where red xs x = if x == h then x:xs else x:h:xs where h = n `div` x

ndivisors n = foldl red 0 [(d, n `div` d) | d <- [1..(isqrt n)], n `mod` d == 0]
    where red n (x,x')  = if x == x' then n+1 else n+2

triangles1 :: [Integer]
triangles1 = [sum [1..n] | n <- [1..]]

triangles2 :: [Integer]
triangles2 = unfoldr gen (0,1)
             where gen (a, n) = Just (a+n, (a+n, n+1))

triangles3 :: [Integer]
triangles3 = [(n*(n+1)) `div` 2 | n <- [1..]]

steps :: [(Int, Int)]
steps = [ (1+n*100000,(n+1)*100000) | n <- [0..]]

--solve1 = find (\(n, d) -> d > 100) $
--         concat [ assocs $ incDividents (array (b,e) $ [(i,0) | i <- [b..e]]) b (e+1) | (b,e) <- steps ]
--         map (\n -> (n, divisors n)) $
--         triangles1

solve2 = find (\(n,d) -> d > 500) $
         map (\n -> (n, ndivisors n)) $
         triangles3

-- triangles1 = 65s
-- triangles2 and triangles3 = 42s

main = putStrLn $ show solve2

