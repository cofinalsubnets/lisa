ack m n | m == 0 = n + 1
        | otherwise = ack (m - 1) $ if n == 0 then 1 else ack m $ m - 1
main = putStrLn . show $ ack 3 9
