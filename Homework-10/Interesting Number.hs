interestingNumber :: Int -> Bool
interestingNumber n
    | n < 1     = False
    | otherwise = n == sumDivsiors (sumDivsiors n 1) 1
        where
            sumDivsiors n curr
                | curr == n         = 0
                | (mod n curr) == 0 = curr + sumDivsiors n (curr + 1)
                | otherwise         = sumDivsiors n (curr + 1)
