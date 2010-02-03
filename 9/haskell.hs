
s = 1000

{-
  / a + b + c = s;
  \ a^2 + b^2 = c^2

  / a + b - s = c;
  \ sqrt(a^2 + b^2) = c;

  / a + b - s = sqrt(a^2 + b^2);
  \ a + b + c = s;

  / a^2 + ab - as + ab + b^2 - bs - as - bs + s^2 = a^2 + b^2;
  / 2ab - 2as - 2bs + s^2 = 0;
-}

solve = [(a*b*c, a, b, c)
         | a <- [1..s], b <- [(a+1)..s],
         (a+b-s)^2 == (a^2 + b^2), -- O(n^3) -> O(n^2)
         c <- [(b+1)..s],
         (a+b+c) == s, a^2 + b^2 == c^2]

main = putStrLn $ show solve

