truncatablePrime :: Int -> Bool
truncatablePrime n
    | n < 10    = prime n
    | otherwise = prime n && truncatablePrime (n `div` 10)
        where
            prime :: Int -> Bool
            prime n
                | n <= 1    = False
                | otherwise = prime' 2 n
                    where
                        prime' current n
                            | current == n = True
                            | mod n current == 0 = False
                            | otherwise = prime' (current + 1) n
