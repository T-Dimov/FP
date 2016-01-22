data Tree a = Empty | Node a (Tree a) (Tree a)

levelSum :: Tree Int -> Int -> Int
levelSum Empty _               = 0
levelSum (Node a _ _) 0        = a
levelSum (Node a left right) x = (levelSum left (x-1)) + (levelSum right (x-1))

cone :: Tree Int -> Bool
cone Empty = True
cone t = ascending . sumList t $ (height t - 1)

height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

sumList :: Tree Int -> Int -> [Int]
sumList _ (-1) = []
sumList t n = sumList t (n-1) ++ [levelSum t n]

ascending :: Ord a => [a] -> Bool
ascending [] = True
ascending (x:[]) = True
ascending (x:y:zs) = x < y && ascending (y:zs)
