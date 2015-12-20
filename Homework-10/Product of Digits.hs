productOfDigits :: Int -> Int
productOfDigits n
    | n < 10    = n
    | otherwise = (mod n 10) * productOfDigits (div n 10)
