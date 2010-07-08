import Data.List

palindrome x = s == (reverse s)
    where s = show x

solve1 = last $ sort $ filter palindrome $ [a*b | a <- [100..999], b <- [100..999]]

main = do
  putStrLn $ show solve1
