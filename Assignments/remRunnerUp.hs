remRunnerUp :: [Int] -> [Int]
remRunnerUp [] = []
remRunnerUp [x] = [x]
remRunnerUp (x:xs)
    | length ([ c | c <- xs, c >= x  ]) == 1 = xs
    | otherwise = (x:remRunnerUp xs)
