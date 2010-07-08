{-# LANGUAGE BangPatterns #-}

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

{-
No loops by definition means we are *always* on the possible track while in bounds
Amount of options varies depending on whether we are on edge or not
-}

deadzone s = Set.fromList $ [(x,y) | x <- [0..(s-1)], y <- [-1,s]]
           ++ [(x,y) | y <- [0..(s-1)], x <- [-1,s]]

moves dz p f
    | p == f = 1
    | Set.member p dz = 0
    | otherwise = (moves dz' (movedown p) f) +
                  (moves dz' (moveright p) f)
--                  (moves dz' (moveup p) f) +
--                  (moves dz' (moveleft p) f) + 
                      where dz' = Set.insert p dz

movedown (x,y) = (x,y+1)
moveright (x,y) = (x+1,y)

--moveup (x,y) = (x,y-1)
--moveleft (x,y) = (x-1,y)

solve1 = moves (deadzone $ n+1) (0,0) (n,n)
    where n = 10

main = putStrLn $ show solve1
