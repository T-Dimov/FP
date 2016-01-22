balance :: Int -> [Int] -> Int
balance _ [] = 0
balance n numbers
    | sum numbers <= n = 0
    | otherwise = 1 + (balance n . noMax $ numbers)
    where
        noMax [] = []
        noMax (n:ns)
            | n == maximum (n:ns) = ns
            | otherwise = n : noMax ns
