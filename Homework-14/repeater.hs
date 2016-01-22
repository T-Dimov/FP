repeater :: String -> (Int -> String -> String)
repeater str count glue = str ++ (rep (count - 1) glue str)

rep :: Int -> String -> String -> String
rep 0 _ _ = []
rep c g s = (rep (c - 1) g s) ++ g ++ s
