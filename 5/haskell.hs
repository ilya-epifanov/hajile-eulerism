
divs1 = filter (\n -> null [x | x <- [(n+1)..20], x `mod` n == 0]) [2..20]
divs2 = [2..20]

divides divs x = null [d | d <- divs, x `mod` d /= 0]
solve1 = head $ filter (divides divs1) [1..]


minify n [] = n
minify n ds'@(d:ds) = case n `divMod` d of 
                        (n',0) -> if divides divs2 n'
                                  then minify n' ds'
                                  else minify n ds
                        (_,_) -> minify n ds

solve2 = minify (product divs2) divs2

main = do 
--  putStrLn $ show solve1
  putStrLn $ show solve2
