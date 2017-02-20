remChamp :: [Int] -> [Int]
remChamp [] = []
remChamp [_] = []
remChamp (x:xs)
    | length ([ c | c <- (xs), c > x ]) == 0 = xs
    | otherwise = (x:(remChamp xs))

