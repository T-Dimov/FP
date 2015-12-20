containsDigits :: Int -> Int -> Bool
containsDigits a b
    | b < 10 = contains a b
    | otherwise = contains a (mod b 10) || containsDigits a (div b 10)
        where
            contains n x
                | n < 10    = n == x
                | otherwise = x == (mod n 10) || contains (div n 10) x
