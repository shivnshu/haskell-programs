remDup :: [Int] -> [Int]
remDup = helperDup []
    where
    helperDup seen [] = seen
    helperDup seen (x:xs)
        | x `elem` seen = helperDup seen xs
        | otherwise = helperDup (seen ++ [x]) xs
