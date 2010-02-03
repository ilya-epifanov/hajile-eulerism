import Data.List

fibs1 = [fibn x | x <- [0..]]
    where
      fibn 0 = 1
      fibn 1 = 2
      fibn n = fibn (n-1) + fibn (n-2)

fibs2 = [1,2] ++ 
        unfoldr (\(a,v) -> Just (a + v, (v, a + v))) (1,2)

solve1 f = sum $ filter even $ takeWhile (4000000 >) f
solve11 = solve1 fibs1
solve12 = solve1 fibs2

main = do
  putStrLn $ show solve11
  putStrLn $ show solve12
