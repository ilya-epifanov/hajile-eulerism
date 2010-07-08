import qualified Data.Map as M

data Pair a b = Pair !a !b

paths n = m where 
    m = M.fromList [((x,y),p x y) | x <- [0..n], y <- [0..n]]
    p 0 0 = 1
    p 0 _ = 1
    p _ 0 = 1
    p x y = (m M.! (x-1, y)) + (m M.! (x, y-1))


solve1 n = moves (Pair n n)

solve2 n = (paths n) M.! (n,n)

main = putStrLn $ show $ solve2 20
