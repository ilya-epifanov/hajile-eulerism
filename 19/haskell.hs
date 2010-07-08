
data Youbi = Getsu | Ka | Sui | Moka | Kan | Do | Nichi deriving (Show, Eq)
data Nendo = Uruudoshi | Heinen deriving (Show, Eq)

youbi = repeat [Getsu, Ka, Sui, Moka, Kan, Do, Nichi]

nendo x
    | x `mod` 400 == 0 = Uruudoshi
    | x `mod` 100 == 0 = Heinen
    | x `mod` 4 == 0   = Uruudoshi
    | otherwise        = Heinen

toshi Uruudoshi = [1..31] ++ [1..29] ++ [1..31] ++
                  [1..30] ++ [1..31] ++ [1..30] ++
                  [1..31] ++ [1..31] ++ [1..30] ++
                  [1..31] ++ [1..30] ++ [1..31]
toshi Heinen    = [1..31] ++ [1..28] ++ [1..31] ++
                  [1..30] ++ [1..31] ++ [1..30] ++
                  [1..31] ++ [1..31] ++ [1..30] ++
                  [1..31] ++ [1..30] ++ [1..31]

f `withSnd` (p,x) = (p, f x)
f `withArg` x  = (x, f x)

nichi = map (uncurry zip) $ map (withSnd toshi) $ map (withArg nendo) [1900..2001]

