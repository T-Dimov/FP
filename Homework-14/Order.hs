data Order = Online Float Int Int | Offline Float
isOnline :: Order -> Bool
isOnline (Online _ _ _) = True
isOnline (Offline _) = False

timeUntilReceiving :: Order -> Int
timeUntilReceiving (Online _ _ x) = x

totalPrice :: [Order] -> Float
totalPrice [] = 0
totalPrice ((Online x _ _):ys) = x + totalPrice ys
totalPrice ((Offline x):ys) = x + totalPrice ys

onlineOrders :: [Order] -> Int
onlineOrders = length . filter isOnline

isExpensive :: Order -> Bool
isExpensive x = 100 < totalPrice [x]

instance Show Order where
    show (Online price num time) = show price ++ " " ++ show num ++ " " ++ show time
    show (Offline price) = show price

instance Eq Order where
    (==) (Online price num time) (Online price1 num1 time1) = price == price1 && num == num1 && time == time1
    (==) (Offline price) (Offline price1) = price == price1
    (==) _ _ = False
