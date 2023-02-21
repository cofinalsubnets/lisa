fib n | n < 3 = 1
      | otherwise = fib (n-1) + fib (n-2)
main = putStrLn . show $ fib 32
