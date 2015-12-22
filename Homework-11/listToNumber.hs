listToNumber :: [Int] -> Int
listToNumber []     = error "Empty list"
listToNumber (x:[]) = x
listToNumber xs     = result 0 xs
    where
        result res (x:[]) = res * 10 + x
        result res (x:xs) = result (res * 10 + x) xs
