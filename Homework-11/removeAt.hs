removeAt :: Int -> [a] -> [a]
removeAt x (y:ys)
    | x < 0         = error "Index out of bounds!"
    | x > length ys = error "Index out of bounds!"
    | x == 0        = ys
    | otherwise     = y : removeAt (x - 1) ys
