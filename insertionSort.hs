insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
    | (x<=y) = x:y:ys
    | otherwise y:(insert x ys)

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
