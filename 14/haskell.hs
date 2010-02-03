{-# LANGUAGE BangPatterns #-}
import Data.List
import Data.Function

import Control.Monad.ST
import Control.Monad
import Data.STRef

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Debug.Trace

type IntIntMap = IntMap Int

solve1 = take 10 $ reverse $ sortBy (compare `on` snd) $ snd $ mapAccumL next (IntMap.fromList [(1,1)]) [1..1000000]
    where next m n = 
              let (m',l) = memoNextCollatz m n
              in (m', (n, l))
          

memoNextCollatz :: IntIntMap -> Int -> (IntIntMap, Int)
memoNextCollatz m n = (m', m' IntMap.! n)
    where m' = unwind m `uncurry` (dig m n 0 [])

unwind :: IntIntMap -> Int -> [Int] -> IntIntMap
unwind m _ [] = m
unwind m l (n:ns) = unwind (IntMap.insert n l m) (l+1) ns

dig :: IntIntMap -> Int -> Int -> [Int] -> (Int, [Int])
dig m n l ns =
    case IntMap.lookup n m of
      Just l' -> (l'+1, ns)
      Nothing -> dig m (nextCollatz n) (l+1) (n:ns)

nextCollatz n
    | even n    = n `div` 2
    | otherwise = 3*n + 1

-- chainlength1 n = length $ chain n

chainlength2 n = chainlength' n 1
    where chainlength' 1 l = l
          chainlength' n l = chainlength' (nextCollatz n) (l+1)

-- solve1 = take 10 $ reverse $ sortBy (compare `on` snd) $ [(n, chainlength2 n) | n <- [1..100000]] 
-- solve2 = take 10 $ reverse $ sortBy (compare `on` snd) $ chainlengths chainlengthCPS

main = putStrLn $ show solve1
