quadrant :: Double -> Double -> Int
quadrant 0.0 0.0 = 0
quadrant x y
    | x > 0 && y > 0 = 1
    | x < 0 && y > 0 = 2
    | x < 0 && y < 0 = 3
    | otherwise      = 4
