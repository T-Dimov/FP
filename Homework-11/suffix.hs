suffix :: (Eq a) => [a] -> [a] -> Bool
suffix [] _ = True
suffix _ [] = False
suffix xs ys = (last xs) == (last ys) && suffix (init xs) (init ys)
