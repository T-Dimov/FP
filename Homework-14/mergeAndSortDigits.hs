import Data.List (sort, group)

mergeAndSortDigits :: Int -> Int -> Int
mergeAndSortDigits x y
    | sum  (digits x) <= sum (digits y) = fromList . rmdups $ digits x ++ digits y
    | otherwise = fromList . reverse . rmdups $ digits x ++ digits y

digits :: Int -> [Int]
digits x
    | x > 0 && (x `mod` 10) > 0 = digits (x `div` 10) ++ [x `mod` 10]
    | x > 0 = digits (x `div` 10)
    | otherwise = []

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

fromList :: [Int] -> Int
fromList [] = 0
fromList x  = (fromList $ init x) * 10 + last x
