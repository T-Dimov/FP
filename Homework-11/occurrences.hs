occurrences :: [Int] -> [Int] -> [Int]
occurrences [] _ = []
occurrences (x:xs) ys = occur x ys : occurrences xs ys
    where
        occur _ []     = 0
        occur x (y:ys)
            | x == y    = 1 + occur x ys
            | otherwise = occur x ys
