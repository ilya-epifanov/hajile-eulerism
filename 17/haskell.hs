
import Data.Monoid
import Data.List
import Data.Char

between n (min,max)
        | n <= min = False
        | n >= max = False
        | otherwise = True

ifNotZero :: (Monoid m, Num n) => n -> m -> m
ifNotZero 0 f = mempty
ifNotZero _ f = f

pluralize 0 s = undefined
pluralize 1 s = s
pluralize _ s = s ++ "s"

intToNumeral 1 = "one"
intToNumeral 2 = "two"
intToNumeral 3 = "three"
intToNumeral 4 = "four"
intToNumeral 5 = "five"
intToNumeral 6 = "six"
intToNumeral 7 = "seven"
intToNumeral 8 = "eight"
intToNumeral 9 = "nine"
intToNumeral 10 = "ten"
intToNumeral 11 = "eleven"
intToNumeral 12 = "twelve"
intToNumeral 13 = "thirteen"
intToNumeral 14 = "fourteen"
intToNumeral 15 = "fifteen"
intToNumeral 16 = "sixteen"
intToNumeral 17 = "seventeen"
intToNumeral 18 = "eighteen"
intToNumeral 19 = "nineteen"
intToNumeral 20 = "twenty"
intToNumeral 30 = "thirty"
intToNumeral 40 = "forty"
intToNumeral 50 = "fifty"
intToNumeral 60 = "sixty"
intToNumeral 70 = "seventy"
intToNumeral 80 = "eighty"
intToNumeral 90 = "ninety"
intToNumeral x
    | thousands > 0 = intToNumeral thousands ++ pluralize thousands " thousand"
                      ++ if (rem1000 `between` (0,100))
                         then (" and " ++ intToNumeral rem1000)
                         else if (rem1000 == 0) then "" else " " ++ intToNumeral rem1000
    | hundred > 0 = intToNumeral hundred ++ " hundred"
                     ++ ifNotZero rem100 (" and " ++ intToNumeral rem100)
    | tens > 0 = intToNumeral (tens * 10)
                 ++ ifNotZero rem10 ("-" ++ intToNumeral rem10)
    where (thousands, rem1000) = x `divMod` 1000
          (hundred, rem100) = x `divMod` 100
          (tens, rem10) = x `divMod` 10

allUpTo1000 = concat $ intersperse ", " $ map intToNumeral [1..1000]

solve1 = length $ filter isAlpha allUpTo1000

main = putStrLn $ show solve1

