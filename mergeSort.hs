merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs

merge (x:xs) (y:ys)
    | x <= y = x:(merge xs (y:ys))
    | otherwise = y:(merge (x:xs) ys)


mergeSort [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = merge (mergeSort(front l) (mergeSort(back l)))
    where
    front l = take ((length l) `div` 2) l
    back l = drop ((length l) `div` 2) l

